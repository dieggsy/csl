(module csl.zmath (make-rectangular
                   make-polar
                   angle
                   magnitude
                   magnitude-squared
                   log-magnitude
                   conjugate
                   inverse
                   negative
                   +
                   -
                   *
                   /
                   sqrt
                   expt
                   exp
                   log10
                   log
                   sin
                   asin
                   cos
                   acos
                   tan
                   atan
                   sec
                   asec
                   csc
                   acsc
                   cot
                   acot
                   sinh
                   asinh
                   cosh
                   acosh
                   tanh
                   atanh
                   sech
                   asech
                   csch
                   acsch
                   coth
                   acoth)

  (import (except (rename scheme
                          (make-rectangular %make-rectangular))
                  + - * / sqrt expt exp log sin asin cos acos tan atan)
          (chicken module)
          (chicken foreign))

  (import-for-syntax (srfi 1)
                     (srfi 13)
                     (chicken format)
                     (chicken foreign)
                     (chicken string))

  (foreign-declare "#include <gsl/gsl_complex.h>")
  (foreign-declare "#include <gsl/gsl_complex_math.h>")

  (define-syntax complex-foreign-lambda
    (ir-macro-transformer
     (lambda (e i c)
       (define (make-gensym sym)
         (case (strip-syntax sym)
           ((double) (gensym "x"))
           ((complex) (gensym "z"))))
       (define (make-letvars args)
         (let* ((only-complex (filter (lambda (x)
                                        (eq? (car x) 'complex))
                                      args))
                (only-names (map cadr only-complex)))
           (fold (lambda (x y)
                   (cons
                    `(,(string->symbol (conc "r" x)) (real-part ,x))
                    (cons
                     `(,(string->symbol (conc "i" x)) (imag-part ,x))
                     y)))
                 '()
                 only-names)))
       (define (make-foreign-args* foreign-args)
         (fold (lambda (x y)
                 (if (eq? (car x) 'complex)
                     (cons `(double ,(string->symbol (conc "r" (cadr x))))
                           (cons `(double ,(string->symbol (conc "i" (cadr x))))
                                 y))
                     (cons x y)))
               '()
               (reverse foreign-args)))
       (define (make-inits args)
         (let* ((only-complex (filter (lambda (x)
                                        (eq? (car x) 'complex))
                                      args)))
           (fold (lambda (x y)
                   (cons
                    (let ((name (cadr x)))
                      (format "gsl_complex ~a = gsl_complex_rect(~a,~a);"
                              (symbol->string name)
                              (conc "r" name)
                              (conc "i" name)))
                    y))
                 '()
                 only-complex)))
       (define (make-return ret-type fn names)
         (let ((strargs (string-join (map symbol->string names) ",")))
           (if (eq? ret-type 'scheme-object)
               `(,(format "gsl_complex out = ~a(~a);" fn strargs)
                 ,(format "C_return(numbers_make_rect(GSL_REAL(out),GSL_IMAG(out)));"))
               `(,(format "C_return(~a(~a));" fn strargs)))))
       (let* ((name (caddr e))
              (arg-types (map strip-syntax (cdddr e)))
              (arg-names (map make-gensym (cdddr e)))
              (foreign-args (zip arg-types arg-names))
              (foreign-args* (make-foreign-args* foreign-args))
              (letvars (make-letvars foreign-args))
              (ret-type (if (eq? (strip-syntax (cadr e)) 'complex)
                            'scheme-object
                            (strip-syntax (cadr e)))))
         (if (null? letvars)
             `(foreign-safe-lambda* ,ret-type ,foreign-args*
                ,@(make-inits foreign-args)
                ,@(make-return ret-type name arg-names))
             `(lambda ,arg-names
                (let (,@letvars)
                  ((foreign-safe-lambda* ,ret-type ,foreign-args*
                     ,@(make-inits foreign-args)
                     ,@(make-return ret-type name arg-names))
                   ,@(map car letvars)))))))))

  ;; TODO concerns about memory allocation with complex numbers?
  (define-external (numbers_make_rect (double r) (double i)) scheme-object
    (%make-rectangular r i))

  ;; Representation/conversion
  (define make-rectangular (complex-foreign-lambda complex "gsl_complex_rect" double double))
  (define make-polar (complex-foreign-lambda complex "gsl_complex_polar" double double))

  ;; Properties
  (define angle (complex-foreign-lambda double "gsl_complex_arg" complex))
  (define magnitude (complex-foreign-lambda double "gsl_complex_abs" complex))

  (define magnitude-squared (complex-foreign-lambda double "gsl_complex_abs2" complex))
  (define log-magnitude (complex-foreign-lambda double "gsl_complex_logabs" complex))

  ;; Arithmetic operators
  (define conjugate (complex-foreign-lambda complex "gsl_complex_conjugate" complex))
  (define inverse (complex-foreign-lambda complex "gsl_complex_inverse" complex))
  (define negative (complex-foreign-lambda complex "gsl_complex_negative" complex))
  (define %+ (complex-foreign-lambda complex "gsl_complex_add" complex complex))
  (define (+ . args)
    (let loop ((r args)
               (sum 0))
      (if (null? r)
          sum
          (loop (cdr r) (%+ (car r) sum)))))

  (define %- (complex-foreign-lambda complex "gsl_complex_sub" complex complex))
  (define (- . args)
    (if (= (length args) 1)
        (negative (car args))
        (let loop ((r args)
                   (sum 0))
          (if (null? r)
              sum
              (loop (cdr r) (%- (car r) sum))))))

  (define %* (complex-foreign-lambda complex "gsl_complex_mul" complex complex))
  (define (* . args)
    (if (= (length args) 1)
        (negative (car args))
        (let loop ((r args)
                   (prod 1))
          (if (null? r)
              prod
              (loop (cdr r) (%* (car r) prod))))))

  (define %/ (complex-foreign-lambda complex "gsl_complex_div" complex complex))
  (define (/ . args)
    (if (= (length args) 1)
        (inverse (car args))
        (if (= (length args) 1)
            (negative (car args))
            (let loop ((r (cdr args))
                       (div (car args)))
              (if (null? r)
                  div
                  (loop (cdr r) (%/ div (car r))))))))
  ;; (defcomplex (complex "add_real" complex double))
  ;; (defcomplex (complex "sub_real" complex double))
  ;; (defcomplex (complex "mul_real" complex double))
  ;; (defcomplex (complex "div_real" complex double))
  ;; (defcomplex (complex "add_imag" complex double))
  ;; (defcomplex (complex "sub_imag" complex double))
  ;; (defcomplex (complex "mul_imag" complex double))
  ;; (defcomplex (complex "div_imag" complex double))

  ;; Elementary functions
  (define sqrt (complex-foreign-lambda complex "gsl_complex_sqrt" complex))
  ;; (defcomplex (complex "sqrt_real" double))
  (define expt (complex-foreign-lambda complex "gsl_complex_pow" complex complex))
  ;; (defcomplex (complex "pow_real" complex double) expt-real)
  (define exp (complex-foreign-lambda complex "gsl_complex_exp" complex))
  (define log10 (complex-foreign-lambda complex "gsl_complex_log10" complex))
  (define log (complex-foreign-lambda complex "gsl_complex_log" complex))

  ;; Trigonometric functions
  (define sin (complex-foreign-lambda complex "gsl_complex_sin" complex))
  (define cos (complex-foreign-lambda complex "gsl_complex_cos" complex))
  (define tan (complex-foreign-lambda complex "gsl_complex_tan" complex))
  (define sec (complex-foreign-lambda complex "gsl_complex_sec" complex))
  (define csc (complex-foreign-lambda complex "gsl_complex_csc" complex))
  (define cot (complex-foreign-lambda complex "gsl_complex_cot" complex))

  (define asin (complex-foreign-lambda complex "gsl_complex_arcsin" complex))
  ;; (defcomplex (complex "arcsin_real" double) asin-real)
  (define acos (complex-foreign-lambda complex "gsl_complex_arccos" complex))
  ;; (defcomplex (complex "arccos_real" double) acos-real)
  (define atan (complex-foreign-lambda complex "gsl_complex_arctan" complex))

  (define asec (complex-foreign-lambda complex "gsl_complex_arcsec" complex))
  ;; (defcomplex (complex "arcsec_real" double) asec-real)
  (define acsc (complex-foreign-lambda complex "gsl_complex_arccsc" complex))
  ;; (defcomplex (complex "arccsc_real" double) acsc-real)
  (define acot (complex-foreign-lambda complex "gsl_complex_arccot" complex))

  (define sinh (complex-foreign-lambda complex "gsl_complex_sinh" complex))
  (define cosh (complex-foreign-lambda complex "gsl_complex_cosh" complex))
  (define tanh (complex-foreign-lambda complex "gsl_complex_tanh" complex))
  (define sech (complex-foreign-lambda complex "gsl_complex_sech" complex))
  (define csch (complex-foreign-lambda complex "gsl_complex_csch" complex))
  (define coth (complex-foreign-lambda complex "gsl_complex_coth" complex))

  (define asinh (complex-foreign-lambda complex "gsl_complex_arcsinh" complex))
  (define acosh (complex-foreign-lambda complex "gsl_complex_arccosh" complex))
  ;; (defcomplex (complex "arccosh_real" double) acosh-real)
  (define atanh (complex-foreign-lambda complex "gsl_complex_arctanh" complex))
  ;; (defcomplex (complex "arctanh_real" double) atanh-real)
  (define asech (complex-foreign-lambda complex "gsl_complex_arcsech" complex))
  (define acsch (complex-foreign-lambda complex "gsl_complex_arccsch" complex))
  (define acoth (complex-foreign-lambda complex "gsl_complex_arccoth" complex)))
