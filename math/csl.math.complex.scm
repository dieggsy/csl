(module csl.math.complex (make-rectangular
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

  (import (except (rename scheme (make-rectangular %make-rectangular))
                  magnitude
                  make-polar
                  angle
                  +
                  -
                  *
                  /
                  sqrt
                  expt
                  exp
                  log
                  sin
                  asin
                  cos
                  acos
                  tan
                  atan)
          (chicken base)
          (chicken module)
          (chicken foreign))

  (foreign-declare "#include <gsl/gsl_complex.h>")
  (foreign-declare "#include <gsl/gsl_complex_math.h>")

  (include "../complex-foreign-lambda.scm")

  (define-external (scheme_make_rect (double r) (double i)) scheme-object
    (%make-rectangular r i))

  ;; Representation/conversion
  (define make-rectangular (complex-foreign-lambda (complex double) "gsl_complex_rect" double double))
  (define make-polar (complex-foreign-lambda (complex double) "gsl_complex_polar" double double))

  ;; Properties
  (define angle (complex-foreign-lambda double "gsl_complex_arg" (complex double)))
  (define magnitude (complex-foreign-lambda double "gsl_complex_abs" (complex double)))

  (define magnitude-squared (complex-foreign-lambda double "gsl_complex_abs2" (complex double)))
  (define log-magnitude (complex-foreign-lambda double "gsl_complex_logabs" (complex double)))

  ;; Arithmetic operators
  (define conjugate (complex-foreign-lambda (complex double) "gsl_complex_conjugate" (complex double)))
  (define inverse (complex-foreign-lambda (complex double) "gsl_complex_inverse" (complex double)))
  (define negative (complex-foreign-lambda (complex double) "gsl_complex_negative" (complex double)))
  (define %+ (complex-foreign-lambda (complex double) "gsl_complex_add" (complex double) (complex double)))
  (define (+ . args)
    (do ((r args (cdr r))
         (sum 0 (%+ (car r) sum)))
        ((null? r) sum)))

  (define %- (complex-foreign-lambda (complex double) "gsl_complex_sub" (complex double) (complex double)))
  (define (- . args)
    (if (= (length args) 1)
        (negative (car args))
        (do ((r args (cdr r))
             (sum 0 (%- (car r) sum)))
            ((null? r) sum))))

  (define %* (complex-foreign-lambda (complex double) "gsl_complex_mul" (complex double) (complex double)))
  (define (* . args)
    (do ((r args (cdr r))
         (prod 1 (%* (car r) prod)))
        ((null? r) prod)))

  (define %/ (complex-foreign-lambda (complex double) "gsl_complex_div" (complex double) (complex double)))
  (define (/ . args)
    (if (= (length args) 1)
        (inverse (car args))
        (do ((r (cdr args) (cdr r))
             (div (car args) (%/ div (car r))))
            ((null? r) div))))
  ;; (defcomplex (complex "add_real" (complex double) double))
  ;; (defcomplex (complex "sub_real" (complex double) double))
  ;; (defcomplex (complex "mul_real" (complex double) double))
  ;; (defcomplex (complex "div_real" (complex double) double))
  ;; (defcomplex (complex "add_imag" (complex double) double))
  ;; (defcomplex (complex "sub_imag" (complex double) double))
  ;; (defcomplex (complex "mul_imag" (complex double) double))
  ;; (defcomplex (complex "div_imag" (complex double) double))

  ;; Elementary functions
  (define sqrt (complex-foreign-lambda (complex double) "gsl_complex_sqrt" (complex double)))
  ;; (defcomplex (complex "sqrt_real" double))
  (define expt (complex-foreign-lambda (complex double) "gsl_complex_pow" (complex double) (complex double)))
  ;; (defcomplex (complex "pow_real" (complex double) double) expt-real)
  (define exp (complex-foreign-lambda (complex double) "gsl_complex_exp" (complex double)))
  (define log10 (complex-foreign-lambda (complex double) "gsl_complex_log10" (complex double)))
  (define log (complex-foreign-lambda (complex double) "gsl_complex_log" (complex double)))

  ;; Trigonometric functions
  (define sin (complex-foreign-lambda (complex double) "gsl_complex_sin" (complex double)))
  (define cos (complex-foreign-lambda (complex double) "gsl_complex_cos" (complex double)))
  (define tan (complex-foreign-lambda (complex double) "gsl_complex_tan" (complex double)))
  (define sec (complex-foreign-lambda (complex double) "gsl_complex_sec" (complex double)))
  (define csc (complex-foreign-lambda (complex double) "gsl_complex_csc" (complex double)))
  (define cot (complex-foreign-lambda (complex double) "gsl_complex_cot" (complex double)))

  (define asin (complex-foreign-lambda (complex double) "gsl_complex_arcsin" (complex double)))
  ;; (defcomplex (complex "arcsin_real" double) asin-real)
  (define acos (complex-foreign-lambda (complex double) "gsl_complex_arccos" (complex double)))
  ;; (defcomplex (complex "arccos_real" double) acos-real)
  (define atan (complex-foreign-lambda (complex double) "gsl_complex_arctan" (complex double)))

  (define asec (complex-foreign-lambda (complex double) "gsl_complex_arcsec" (complex double)))
  ;; (defcomplex (complex "arcsec_real" double) asec-real)
  (define acsc (complex-foreign-lambda (complex double) "gsl_complex_arccsc" (complex double)))
  ;; (defcomplex (complex "arccsc_real" double) acsc-real)
  (define acot (complex-foreign-lambda (complex double) "gsl_complex_arccot" (complex double)))

  (define sinh (complex-foreign-lambda (complex double) "gsl_complex_sinh" (complex double)))
  (define cosh (complex-foreign-lambda (complex double) "gsl_complex_cosh" (complex double)))
  (define tanh (complex-foreign-lambda (complex double) "gsl_complex_tanh" (complex double)))
  (define sech (complex-foreign-lambda (complex double) "gsl_complex_sech" (complex double)))
  (define csch (complex-foreign-lambda (complex double) "gsl_complex_csch" (complex double)))
  (define coth (complex-foreign-lambda (complex double) "gsl_complex_coth" (complex double)))

  (define asinh (complex-foreign-lambda (complex double) "gsl_complex_arcsinh" (complex double)))
  (define acosh (complex-foreign-lambda (complex double) "gsl_complex_arccosh" (complex double)))
  ;; (defcomplex (complex "arccosh_real" double) acosh-real)
  (define atanh (complex-foreign-lambda (complex double) "gsl_complex_arctanh" (complex double)))
  ;; (defcomplex (complex "arctanh_real" double) atanh-real)
  (define asech (complex-foreign-lambda (complex double) "gsl_complex_arcsech" (complex double)))
  (define acsch (complex-foreign-lambda (complex double) "gsl_complex_arccsch" (complex double)))
  (define acoth (complex-foreign-lambda (complex double) "gsl_complex_arccoth" (complex double))))
