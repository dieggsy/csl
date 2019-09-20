(include "utils/declarations.scm")

(module gsl.math.double (e
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
                         log1p
                         expm1
                         hypot
                         cosh
                         acosh
                         sinh
                         asinh
                         tanh
                         atanh
                         ldexp
                         frexp
                         pow-int
                         pow-2
                         pow-3
                         pow-4
                         pow-5
                         pow-6
                         pow-7
                         pow-8
                         pow-9
                         sign
                         odd?
                         even?
                         max
                         min
                         max-dbl
                         min-dbl
                         max-int
                         min-int
                         fcmp)

  (import (except scheme
                  odd?
                  even?
                  max
                  min)
          (only chicken.base foldr include)
          (chicken foreign))

  (include "utils/error-handler.scm")

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
  (define log1p (foreign-lambda double "gsl_log1p" (const double)))
  (define expm1 (foreign-lambda double "gsl_expm1" (const double)))
  (define hypot (foreign-lambda double "gsl_hypot" (const double) (const double)))
  (define hypot3 (foreign-lambda double "gsl_hypot3" (const double) (const double) (const double)))

  ;; added from math.h
  (define cosh (foreign-lambda double "cosh" (const double)))
  (define sinh (foreign-lambda double "sinh" (const double)))
  (define tanh (foreign-lambda double "tanh" (const double)))

  (define acosh (foreign-lambda double "gsl_acosh" (const double)))
  (define asinh (foreign-lambda double "gsl_asinh" (const double)))
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
  (define pow-int (foreign-lambda  double "gsl_pow_int" double int))
  (define pow-2 (foreign-lambda double "gsl_pow_2" (const double)))
  (define pow-3 (foreign-lambda double "gsl_pow_3" (const double)))
  (define pow-4 (foreign-lambda double "gsl_pow_4" (const double)))
  (define pow-5 (foreign-lambda double "gsl_pow_5" (const double)))
  (define pow-6 (foreign-lambda double "gsl_pow_6" (const double)))
  (define pow-7 (foreign-lambda double "gsl_pow_7" (const double)))
  (define pow-8 (foreign-lambda double "gsl_pow_8" (const double)))
  (define pow-9 (foreign-lambda double "gsl_pow_9" (const double)))

  ;; Testing the sign of numbers
  (define sign (foreign-lambda* int ((double x)) "C_return(GSL_SIGN(x));"))

  ;; Testing for odd and even numbers
  (define odd? (foreign-lambda* bool ((int x)) "C_return(GSL_IS_ODD(x));"))
  (define even? (foreign-lambda* bool ((int x)) "C_return(GSL_IS_EVEN(x));"))

  ;; Maximum and minimum functions
  (define max (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MAX(a,b));"))
  (define min (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MIN(a,b));"))
  (define max-dbl (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MAX_DBL(a,b));"))
  (define min-dbl (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MIN_DBL(a,b));"))
  (define max-int (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MAX_INT(a,b));"))
  (define min-int (foreign-lambda* double ((double a) (double b)) "C_return(GSL_MIN_INT(a,b));"))

  ;; Approximate comparison of floating point numbers
  (define fcmp (foreign-lambda int "gsl_fcmp" double double double)))
