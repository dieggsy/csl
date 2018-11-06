;; Compile with -L -lgsl -L -lgslcblas
(module csl.math.double (e
                         log2e
                         log10e
                         sqrt2
                         sqrt1/2
                         sqrt3
                         pi
                         pi/2
                         pi/4
                         sqrtpi
                         2/sqrtpi
                         1/pi
                         2/pi
                         ln10
                         ln2
                         lnpi
                         euler
                         +inf
                         -inf
                         nan
                         nan?
                         infinite?
                         finite?
                         log1+
                         exp-1
                         hypot
                         cosh
                         acosh
                         sinh
                         asinh
                         tanh
                         atanh
                         ldexp
                         frexp
                         expt-int
                         expt2
                         expt3
                         expt4
                         expt5
                         expt6
                         expt7
                         expt8
                         expt9
                         sign
                         odd?
                         even?
                         max-dbl
                         min-dbl
                         max-int
                         min-int
                         max-ldbl
                         min-ldbl
                         fcmp)

  (import (except scheme
                  odd?
                  even?
                  max
                  min)
          (chicken foreign))

  (foreign-declare "#include <gsl/gsl_math.h>")
  (foreign-declare "#include <math.h>")

  ;; Constants
  (define e (foreign-value "M_E" double))
  (define log2e (foreign-value "M_LOG2E" double))
  (define log10e (foreign-value "M_LOG10E" double))
  (define sqrt2 (foreign-value "M_SQRT2" double))
  (define sqrt1/2 (foreign-value "M_SQRT1_2" double))
  (define sqrt3 (foreign-value "M_SQRT3" double))
  (define pi (foreign-value "M_PI" double))
  (define pi/2 (foreign-value "M_PI_2" double))
  (define pi/4 (foreign-value "M_PI_4" double))
  (define sqrtpi (foreign-value "M_SQRTPI" double))
  (define 2/sqrtpi (foreign-value "M_2_SQRTPI" double))
  (define 1/pi (foreign-value "M_1_PI" double))
  (define 2/pi (foreign-value "M_2_PI" double))
  (define ln10 (foreign-value "M_LN10" double))
  (define ln2 (foreign-value "M_LN2" double))
  (define lnpi (foreign-value "M_LNPI" double))
  (define euler (foreign-value "M_EULER" double))

  ;; Infinities and NAN
  (define +inf (foreign-value "GSL_POSINF" double))
  (define -inf (foreign-value "GSL_NEGINF" double))
  (define nan (foreign-value "GSL_NAN" double))


  (define nan? (foreign-lambda bool "gsl_isnan" (const double)))
  (define infinite? (foreign-lambda bool "gsl_isinf" (const double)))
  (define finite? (foreign-lambda bool "gsl_finite" (const double)))

  ;; Elementary functions
  (define log1+ (foreign-lambda double "gsl_log1p" (const double)))
  (define exp-1 (foreign-lambda double "gsl_expm1" (const double)))
  (define hypot2 (foreign-lambda double "gsl_hypot" (const double) (const double)))
  (define hypot3 (foreign-lambda double "gsl_hypot3" (const double) (const double) (const double)))
  (define (hypot a b #!optional c)
    (if c
        (hypot3 a b c)
        (hypot2 a b)))
  (define cosh (foreign-lambda double "cosh" (const double)))
  (define acosh (foreign-lambda double "gsl_acosh" (const double)))
  (define sinh (foreign-lambda double "sinh" (const double)))
  (define asinh (foreign-lambda double "gsl_asinh" (const double)))
  (define tanh (foreign-lambda double "tanh" (const double)))
  (define atanh (foreign-lambda double "gsl_atanh" (const double)))
  (define ldexp (foreign-lambda double "gsl_ldexp" double int))
  (define frexp
    (foreign-primitive ((double x))
        "int e;"
      "double f = gsl_frexp(x,&e);"
      "C_word *fptr = C_alloc(C_SIZEOF_FLONUM);"
      "C_word *iptr = C_alloc(C_SIZEOF_FIX_BIGNUM);"
      "C_word av[4] = {C_SCHEME_UNDEFINED, C_k, C_flonum(&fptr, f), C_int_to_num(&iptr, e)};"
      "C_values(4,av);"))

  ;; Small integer powers
  (define expt-int (foreign-lambda  double "gsl_pow_int" double int))
  ;; (define expt-uint (foreign-lambda  double "pow-uint" double int))
  (define expt2 (foreign-lambda double "gsl_pow_2" (const double)))
  (define expt3 (foreign-lambda double "gsl_pow_3" (const double)))
  (define expt4 (foreign-lambda double "gsl_pow_4" (const double)))
  (define expt5 (foreign-lambda double "gsl_pow_5" (const double)))
  (define expt6 (foreign-lambda double "gsl_pow_6" (const double)))
  (define expt7 (foreign-lambda double "gsl_pow_7" (const double)))
  (define expt8 (foreign-lambda double "gsl_pow_8" (const double)))
  (define expt9 (foreign-lambda double "gsl_pow_9" (const double)))

  (define sign (foreign-lambda* int ((double x)) "C_return(GSL_SIGN(x));"))
  (define odd? (foreign-lambda* bool ((int x)) "C_return(GSL_IS_ODD(x));"))
  (define even? (foreign-lambda* bool ((int x)) "C_return(GSL_IS_EVEN(x));"))

  ;; Maximum and minimum functions
  (define %max (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MAX(a,b));"))
  (define (max . args)
    (do ((r args (cdr r))
         (m 0 (%max (car r) m)))
        ((null? r) m)))
  (define %min (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MIN(a,b));"))
  (define (min . args)
    (do ((r args (cdr r))
         (m 0 (%min (car r) m)))
        ((null? r) m)))
  (define %max-dbl (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MAX_DBL(a,b));"))
  (define (max-dbl . args)
    (do ((r args (cdr r))
         (m 0 (%max-dbl (car r) m)))
        ((null? r) m)))
  (define %min-dbl (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MIN_DBL(a,b));"))
  (define (min-dbl . args)
    (do ((r args (cdr r))
         (m 0 (%min-dbl (car r) m)))
        ((null? r) m)))
  (define %max-int (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MAX_INT(a,b));"))
  (define (max-int . args)
    (do ((r args (cdr r))
         (m 0 (%max-int (car r) m)))
        ((null? r) m)))
  (define %min-int (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MIN_INT(a,b));"))
  (define (min-int . args)
    (do ((r args (cdr r))
         (m 0 (%min-int (car r) m)))
        ((null? r) m)))
  (define %max-ldbl (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MAX_LDBL(a,b));"))
  (define (max-ldbl . args)
    (do ((r args (cdr r))
         (m 0 (%max-ldbl (car r) m)))
        ((null? r) m)))
  (define %min-ldbl (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MIN_LDBL(a,b));"))
  (define (min-ldbl . args)
    (do ((r args (cdr r))
         (m 0 (%min-ldbl (car r) m)))
        ((null? r) m)))

  ;; Approximate comparison of floating point numbers
  (define fcmp (foreign-lambda int "gsl_fcmp" double double double)))
