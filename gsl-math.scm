(foreign-declare "#include <gsl/gsl_math.h>")

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
(define posinf (foreign-value "GSL_POSINF" double))
(define neginf (foreign-value "GSL_NEGINF" double))
(define nan (foreign-value "GSL_NAN" double))
(define nan? (foreign-lambda bool "gsl_isnan" (const double)))
(define inf? (foreign-lambda bool "gsl_isinf" (const double)))
(define finite? (foreign-lambda bool "gsl_finite" (const double)))

;; Elementary functions
(define log1p (foreign-lambda double "gsl_log1p" (const double)))
(define expm1 (foreign-lambda double "gsl_expm1" (const double)))
(define hypot (foreign-lambda double "gsl_hypot" (const double) (const double)))
(define hypot3 (foreign-lambda double "gsl_hypot3"
                 (const double)
                 (const double)
                 (const double)))
(define acosh (foreign-lambda double "gsl_acosh" (const double)))
(define asinh (foreign-lambda double "gsl_asinh" (const double)))
(define atanh (foreign-lambda double "gsl_atanh" (const double)))
(define aldexp (foreign-lambda double "gsl_ldexp"  double int))
(define afrexp (foreign-lambda double "gsl_frexp"  double (c-pointer int)))

;; Small integer powers
(define pow-int (foreign-lambda double "gsl_pow_int" double int))
(define pow-uint (foreign-lambda double "gsl_pow_uint" double unsigned-int))
(define pow-2 (foreign-lambda double "gsl_pow_2" (const double)))
(define pow-3 (foreign-lambda double "gsl_pow_3" (const double)))
(define pow-4 (foreign-lambda double "gsl_pow_4" (const double)))
(define pow-5 (foreign-lambda double "gsl_pow_5" (const double)))
(define pow-6 (foreign-lambda double "gsl_pow_6" (const double)))
(define pow-7 (foreign-lambda double "gsl_pow_7" (const double)))
(define pow-8 (foreign-lambda double "gsl_pow_8" (const double)))
(define pow-9 (foreign-lambda double "gsl_pow_9" (const double)))
(define sign (foreign-lambda* int ((double x)) "C_return(GSL_SIGN(x));"))
(define odd? (foreign-lambda* bool ((int x)) "C_return(GSL_IS_ODD(x));"))
(define even? (foreign-lambda* bool ((int x)) "C_return(GSL_IS_EVEN(x));"))

;; Maximum and minimum functions
(define max (foreign-lambda* double ((double a) (double b))
              "C_return(GSL_MAX(a,b));"))
(define min (foreign-lambda* double ((double a) (double b))
              "C_return(GSL_MIN(a,b));"))
(define max-dbl (foreign-lambda* double ((double a) (double b))
                  "C_return(GSL_MAX_DBL(a, b));"))
(define min-dbl (foreign-lambda* double ((double a) (double b))
                  "C_return(GSL_MIN_DBL(a, b));"))
(define max-int (foreign-lambda* int ((int a) (int b))
                  "C_return(GSL_MAX_INT(a, b));"))
(define min-int (foreign-lambda* int ((int a) (int b))
                  "C_return(GSL_MIN_INT(a, b));"))
(define max-ldbl (foreign-lambda* long ((long a) (long b))
                   "C_return(GSL_MAX_LDBL(a, b));"))
(define min-ldbl (foreign-lambda* long ((long a) (long b))
                   "C_return(GSL_MIN_LDBL(a, b));"))

;; Approximate comparison of floating point numbers
(define fcmp (foreign-lambda bool "gsl_fcmp" double double double))
