(import (only chicken.foreign foreign-declare)
        (only srfi-4
              f64vector make-f64vector f64vector->list
              f32vector make-f32vector f32vector->list)
        bind)

(foreign-declare "#include <gsl/gsl_complex_math.h>")

(foreign-declare "
gsl_complex f64_to_complex(double *arg) {
    return gsl_complex_rect(arg[0],arg[1]);
}
" )

(import-for-syntax bind-translator
                   (only chicken.pretty-print pp)
                   (only chicken.string conc)
                   (only chicken.irregex
                         irregex-search
                         irregex-match
                         irregex-match-substring)
                   (only chicken.format format)
                   (only srfi-1 any)
                   (only srfi-13 string-prefix?)
                   (only matchable match))

(begin-for-syntax
  (define debug (make-parameter #f))
  (debug #t)
  (when (debug)
    (print "===== compile-time"))
  ;; convert any foreign-lambda with a gsl-complex struct return-type,
  ;; and make it return a 2-element f64vector instead.
  (define (gsl-ret-transformer* x rename)

    (define (make-complex-ret-lambda fl args body vec make-vec vec->list )
      (let* ((argnames (map cadr args))
             ;; return-type -> void, add f64vector destination
             ;; argument, and cast to gsl-complex.
             (lambda-with-destination
              (bind-foreign-lambda*
               `(,fl
                 void                           ;; new return type
                 ,(cons `(,vec dest) args) ;; add destination arg
                 (stmt
                  (= "gsl_complex _z" ,body) ;; allocate, cast & assign
                  (= "dest[0]" "GSL_REAL(_z)")
                  (= "dest[1]" "GSL_IMAG(_z)")
                  ))
               rename))
             ;; allocate a f64vector and use it as desination
             (destination-wrapper
              `(lambda ,argnames
                 (,(rename 'let) ((destination (,make-vec 2)))
                  (,lambda-with-destination destination ,@argnames)
                  (apply make-rectangular (,vec->list destination))))))
        destination-wrapper))

    (match x
      ;; return-type is a gsl-complex, need to convert
      ((foreign-lambda* ('struct (? (cut irregex-search "^gsl_complex" <>) type)) args body)
       (cond ((string=? type "gsl_complex")
              (make-complex-ret-lambda foreign-lambda* args body
                                       'f64vector 'make-f64vector 'f64vector->list))
             ((string=? type "gsl_complex_float")
              (make-complex-ret-lambda foreign-lambda* args body
                                       'f32vector 'make-f32vector 'f32vector->list))
             (else (error "Unknown complex type" type))))
      ((foreign-lambda* ('struct (? (cut irregex-match "gsl_vector_\\w+_view" <>) type)) args body)
       (let* ((argnames (map cadr args))
              (pref (irregex-match-substring
                     (irregex-match "(gsl_vector(_\\w+)?)_view" type)
                     1))
              (lambda-with-destination
               (bind-foreign-lambda*
                `(,foreign-lambda*
                     gsl_vector ;; new return type
                     ,args
                   (stmt
                    (= ,(format "~a view" type) ,body) ;; allocate, cast & assign
                    ,(format "~a *vec = ~a_alloc(view.vector.size);" pref pref)
                    ,(format "memcpy(vec,&view.vector,sizeof(~a));" pref)
                    (return vec)))
                rename)))
         lambda-with-destination))
      ;; ignore other return-types
      (else (bind-foreign-lambda* x rename))))

  (define (gsl-arg-transformer* x rename)
    (define (complex-type? type)
      (and (pair? type)
           (eq? (car type) 'struct)
           (irregex-search "^gsl_complex" (cadr type))
           ;; (equal? type '(struct "gsl_complex"))
           ))
    (match x
      ;; return-type is a gsl-complex, need to convert
      ((foreign-lambda* rtype (? (lambda (x) (any complex-type? (map car x))) args) body)
       (when (debug)
         (print "----LAMBDA:")
         (pp x)
         (print "=>"))
       (let ((argnames (map cadr args)))
         (define (type varname)
           (any (lambda (spec)
                  (and (eq? (cadr spec) varname)
                       (car spec))) args))
         (define (gsl-complex? type)
           (and (equal? type '(struct "gsl_complex"))))
         (define (gsl-complex->f64vector as)
           (if (gsl-complex? (car as))
               (list 'f64vector (cadr as))
               as))
         ;; recursively look for variables which reference arguments of
         ;; type struct and cast from f64vector to struct gsl-complex*.
         (define (dereference body)
           (if (list? body)
               (map dereference body)
               (if (and (symbol? body) (gsl-complex? (type body)))
                   (conc "f64_to_complex(" body ")")
                   body)))
         (let ((final-lambda
                `(lambda ,argnames
                   (,(gsl-ret-transformer*
                      `(,foreign-lambda* ,rtype
                           ,(map gsl-complex->f64vector args)
                         ,(dereference body))
                      rename)
                    ,@(map (lambda (x)
                             (if (gsl-complex? (type x))
                                 `(f64vector (exact->inexact (real-part ,x))
                                             (exact->inexact (imag-part ,x)))
                                 x))
                           argnames)))))
           (when (debug)
             (pp final-lambda))
           final-lambda)))
      (else
       (gsl-ret-transformer*
        x
        rename)
       )
      ))
  )

;; convert any arguments of type (struct "gsl-complex") to f64vectors,
;; and cast & dereference from C.
