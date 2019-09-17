(import (only chicken.foreign foreign-declare)
        srfi-4
        (only chicken.syntax begin-for-syntax)
        bind)

(foreign-declare "
#include <gsl/gsl_complex.h>
#include <gsl/gsl_mode.h>
")

(foreign-declare "
#ifndef CSL_COMPLEX_HELPERS
#define CSL_COMPLEX_HELPERS
gsl_complex f64_to_complex(double *arg) {
    gsl_complex z;
    GSL_SET_COMPLEX(&z,arg[0],arg[1]);
    return z;
}

gsl_complex_float f32_to_complex(float *arg) {
    gsl_complex_float z;
    GSL_SET_COMPLEX(&z,arg[0],arg[1]);
    return z;
}
#endif
" )

(define prec-double (foreign-value "GSL_PREC_DOUBLE" int))
(define prec-single (foreign-value "GSL_PREC_SINGLE" int))
(define prec-approx (foreign-value "GSL_PREC_APPROX" int))

(import-for-syntax bind-translator
                   (only chicken.pretty-print pp)
                   (only chicken.string conc)
                   (only chicken.irregex
                         irregex-search
                         irregex-match
                         irregex-match-substring)
                   (only chicken.format format)
                   (only srfi-1 any filter)
                   (only srfi-13 string-prefix?)
                   (only matchable match))

(define (f64vector->values v)
  (apply values (f64vector->list v)))

(begin-for-syntax
  ;; convert any foreign-lambda with a gsl-complex struct return-type, and make
  ;; it return a 2-element f64vector/f32vector instead (depending on complex
  ;; base type), which is then converted into a complex number
  (define (gsl-ret-transformer* x rename)

    (define (make-complex-ret-lambda fl args body typestr vec make-vec vec-ref)
      (let* ((argnames (map cadr args))
             ;; return-type -> void, add f64vector/f32vector destination
             ;; argument, and cast to gsl-complex.
             (lambda-with-destination
              (bind-foreign-lambda*
               `(,fl
                 void ;; new return type
                 ,(cons `(,vec dest) args) ;; add destination arg
                 (stmt
                  ;; Allocate & assign
                  ;;,body here is essentially the 'original' function call in C
                  (= ,(format "~a _z" typestr) ,body)
                  (= "dest[0]" "GSL_REAL(_z)")
                  (= "dest[1]" "GSL_IMAG(_z)")
                  ))
               rename))
             ;; allocate a f64vector and use it as desination
             (destination-wrapper
              `(lambda ,argnames
                 (,(rename 'let) ((destination (,make-vec 2)))
                  (,lambda-with-destination destination ,@argnames)
                  (make-rectangular (,vec-ref destination 0)
                                    (,vec-ref destination 1))))))
        destination-wrapper))

    (match x
      ;; return-type is a gsl-complex, need to convert
      ((foreign-lambda*
           ('struct (?
                     (cut irregex-search "^gsl_complex" <>)
                     type))
           args
         body)
       (cond ((string=? type "gsl_complex")
              (make-complex-ret-lambda foreign-lambda* args body type
                                       'f64vector 'make-f64vector 'f64vector-ref))
             ((string=? type "gsl_complex_float")
              (make-complex-ret-lambda foreign-lambda* args body type
                                       'f32vector 'make-f32vector 'f32vector-ref))
             (else (error "Unknown complex type" type))))
      ;; Handle gsl_vector_view by copying into gsl_vector and returning that
      ;; instead
      ((foreign-lambda*
           ('struct (?
                     (cut irregex-match "gsl_vector(_\\w+)?_view" <>)
                     type))
           args
         body)
       (let* ((argnames (map cadr args))
              (pref (irregex-match-substring
                     (irregex-match "(gsl_vector(_\\w+)?)_view" type)
                     1))
              (lambda-with-destination
               (bind-foreign-lambda*
                `(,foreign-lambda*
                     csl_vector ;; new return type
                     ,args
                   (stmt
                    ;; allocate, copy, return
                    (= ,(format "~a view" type) ,body)
                    ,(format "~a *vec = ~a_alloc(view.vector.size);" pref pref)
                    ,(format "~a_memcpy(vec,&view.vector);" pref pref)
                    (return vec)))
                rename)))
         lambda-with-destination))
      ((foreign-lambda*
           ('struct (?
                     (cut irregex-match "gsl_matrix(_\\w+)?_view" <>)
                     type))
           args
         body)
       (let* ((argnames (map cadr args))
              (pref (irregex-match-substring
                     (irregex-match "(gsl_matrix(_\\w+)?)_view" type)
                     1))
              (lambda-with-destination
               (bind-foreign-lambda*
                `(,foreign-lambda*
                     csl_matrix ;; new return type
                     ,args
                   (stmt
                    ;; allocate, copy, return
                    (= ,(format "~a view" type) ,body)
                    ,(format "~a *vec = ~a_alloc(view.matrix.size1, view.matrix.size2);" pref pref)
                    ,(format "~a_memcpy(vec,&view.matrix);" pref pref)
                    (return vec)))
                rename)))
         lambda-with-destination))
      ;; ignore other return-types
      (else (bind-foreign-lambda* x rename))))

  (define (foo#gsl-arg-transformer* x rename)
    (define (complex-type? str)
      (lambda (type)
        (and (pair? type)
             (eq? (car type) 'struct)
             (string=? (cadr type) str))))
    (define (make-complex-arg-lambda fl rtype args body typestr convertstr vec)
      (let ((argnames (map cadr args)))
        (define (type varname)
          (any (lambda (spec)
                 (and (eq? (cadr spec) varname)
                      (car spec))) args))
        (define (gsl-complex? type)
          (and (equal? type `(struct ,typestr))))
        (define (gsl-complex->f64vector as)
          (if (gsl-complex? (car as))
              (list vec (cadr as))
              as))
        ;; recursively look for variables which reference arguments of
        ;; type struct and cast from f64vector to struct gsl-complex.
        (define (dereference body)
          (if (list? body)
              (map dereference body)
              (if (and (symbol? body) (gsl-complex? (type body)))
                  (conc convertstr "(" body ")")
                  body)))
        (let ((final-lambda
               `(lambda ,argnames
                  (,(gsl-ret-transformer*
                     `(,fl ,rtype
                           ,(map gsl-complex->f64vector args)
                           ,(dereference body))
                     rename)
                   ,@(map (lambda (x)
                            (if (gsl-complex? (type x))
                                `(,vec (exact->inexact (real-part ,x))
                                       (exact->inexact (imag-part ,x)))
                                x))
                          argnames)))))
          final-lambda)))
    (match x
      ;; arg type is a gsl-complex, need to convert
      ((foreign-lambda* rtype
           (? (lambda (x)
                (any (complex-type? "gsl_complex") (map car x)))
              args)
         body)
       (make-complex-arg-lambda foreign-lambda* rtype args body
                                "gsl_complex" "f64_to_complex" 'f64vector))
      ((foreign-lambda* rtype
           (? (lambda (x)
                (any (complex-type? "gsl_complex_float") (map car x)))
              args)
         body)
       (make-complex-arg-lambda foreign-lambda* rtype args body
                                "gsl_complex_float" "f32_to_complex" 'f32vector))
      ;; Boy is the below a kludge:

      ;;I wasn't sure how to decouple gsl_sf_result args from gsl_mode_t args
      ;;in the transformer, so I process them with the same pattern and
      ;;conditionally do things based on the arg type
      ((foreign-lambda* rtype
           (? (lambda (x)
                (any
                 (lambda (x)
                   (or (equal? "gsl_mode_t" x)
                       (equal? '(c-pointer "gsl_sf_result") x)))
                 (map car x)))
              args)
         body)
       (let ((argnames (map cadr args)))
         (define (type varname)
           (any (lambda (spec)
                  (and (eq? (cadr spec) varname)
                       (car spec))) args))

         (define (sf-result? type)
           (and (equal? type '(c-pointer "gsl_sf_result"))))

         (let ((final-lambda
                `(lambda  ,(filter (lambda (x) (not (sf-result? (type x))))
                                   argnames)
                   (let* (,@(if (any sf-result? (map type argnames))
                                '((fvec (make-f64vector 2)))
                                '())
                          (fnres
                           (,(gsl-ret-transformer*
                              `(,foreign-lambda* ,rtype
                                   ,(map (lambda (as)
                                           (if (equal? "gsl_mode_t" (car as))
                                               (list 'unsigned-int (cadr as))
                                               as)
                                           )
                                         args)
                                 ,body)
                              rename)
                            ,@(map (lambda (x)
                                     (cond ((equal? "gsl_mode_t" (type x))
                                            `(case ,x
                                               ((double) prec-double)
                                               ((single) prec-single)
                                               ((approx) prec-approx)
                                               (else (error "Invalid precision specifier" ,x))))
                                           ((sf-result? (type x))
                                            '(location fvec))
                                           (else x)))
                                   argnames))))
                     ,(if (any sf-result? (map type argnames))
                          '(f64vector->values fvec)
                          'fnres)))))
           final-lambda)))
      (else
       (gsl-ret-transformer*
        x
        rename)))))

(bind-options foreign-transformer: foo#gsl-arg-transformer*)
