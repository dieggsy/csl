(foreign-declare "#include <gsl/gsl_math.h>")

(define-syntax def-foreign-values
  (ir-macro-transformer
   (lambda (e i c)
     `(begin
        ,@(let loop ((names (cdr e))
                     (res '()))
            (if (null? names)
                res
                (loop
                 (cdr names)
                 (append
                  (let* ((name (if (list? (car names))
                                   (caar names)
                                   (car names)))
                         (scheme-name
                          (if (list? (car names))
                              (string-append
                               "csl:"
                               (cadar names))
                              (string-append
                               "csl:"
                               (string-downcase
                                (substring name
                                           (+ 1 (substring-index "_" name))))))))
                    `((define ,(string->symbol scheme-name)
                        (foreign-value ,name double))
                      (define ,(string->symbol name)
                        ,(string->symbol scheme-name))))
                  res))))))))

(define-syntax def-foreign-fns
  (ir-macro-transformer
   (lambda (e i c)
     `(begin
        ,@(let loop ((names (cdr e))
                     (res '()))
            (if (null? names)
                res
                (loop
                 (cdr names)
                 (append
                  (let* ((name (if (list? (cadar names))
                                   (caadar names)
                                   (cadar names)))
                         (scheme-name
                          (if (list? (cadar names))
                              (string-append
                               "csl:"
                               (car (cdadar names)))
                              (string-append
                               "csl:"
                               (string-downcase
                                (substring name
                                           (+ 1 (substring-index "_" name))))))))
                    `((define ,(string->symbol scheme-name)
                        (foreign-lambda ,(caar names) ,name
                          ,@(cddar names)))
                      (define ,(string->symbol name)
                        ,(string->symbol scheme-name))))
                  res))))))))

;; Constants
(def-foreign-values
  "M_E"
  "M_LOG2E"
  "M_LOG10E"
  "M_SQRT2"
  ("M_SQRT1_2" "sqrt1/2")
  "M_SQRT3"
  "M_PI"
  ("M_PI_2" "pi/2")
  ("M_PI_4" "pi/4")
  "M_SQRTPI"
  ("M_2_SQRTPI" "2/sqrtpi")
  ("M_1_PI" "1/pi")
  ("M_2_PI" "2/pi")
  "M_LN10"
  "M_LN2"
  "M_LNPI"
  "M_EULER")

;; Infinities and NAN
(def-foreign-values
  ("GSL_POSINF" "+inf")
  ("GSL_NEGINF" "-inf")
  "GSL_NAN")

(def-foreign-fns
  (bool ("gsl_isnan" "nan?") (const double))
  (bool ("gsl_isinf" "inf?") (const double))
  (bool ("gsl_finite" "finite?") (const double)))

;; Elementary functions
(def-foreign-fns
  (double "gsl_log1p" (const double))
  (double "gsl_hypot" (const double) (const double))
  (double "gsl_hypot3" (const double) (const double) (const double))
  (double "gsl_acosh" (const double))
  (double "gsl_asinh" (const double))
  (double "gsl_atanh" (const double))
  (double "gsl_ldexp" double int)
  (double "gsl_frexp" double (c-pointer int)))

;; Small integer powers
(def-foreign-fns
  (double ("gsl_pow_int" "expt-int") double int)
  (double ("gsl_pow_uint" "expt-uint") double unsigned-int)
  (double ("gsl_pow_2" "expt2") (const double))
  (double ("gsl_pow_3" "expt3") (const double))
  (double ("gsl_pow_4" "expt4") (const double))
  (double ("gsl_pow_5" "expt5") (const double))
  (double ("gsl_pow_6" "expt6") (const double))
  (double ("gsl_pow_7" "expt7") (const double))
  (double ("gsl_pow_8" "expt8") (const double))
  (double ("gsl_pow_9" "expt9") (const double)))

(define GSL_SIGN (foreign-lambda* int ((double x)) "C_return(GSL_SIGN(x));"))
(define csl:sign GSL_SIGN)
(define GSL_IS_ODD (foreign-lambda* bool ((int x)) "C_return(GSL_IS_ODD(x));"))
(define csl:odd? GSL_IS_ODD)
(define GSL_IS_EVEN (foreign-lambda* bool ((int x)) "C_return(GSL_IS_EVEN(x));"))
(define csl:even? GSL_IS_EVEN)

;; Maximum and minimum functions
(define GSL_MAX (foreign-lambda* double ((double a) (double b))
                  "C_return(GSL_MAX(a,b));"))
(define csl:max GSL_MAX)
(define GSL_MIN (foreign-lambda* double ((double a) (double b))
                  "C_return(GSL_MIN(a,b));"))
(define csl:min GSL_MIN)
(define GSL_MAX_DBL (foreign-lambda* double ((double a) (double b))
                      "C_return(GSL_MAX_DBL(a, b));"))
(define csl:max-dbl GSL_MAX_DBL)
(define min-dbl (foreign-lambda* double ((double a) (double b))
                  "C_return(GSL_MIN_DBL(a, b));"))
(define GSL_MAX_INT (foreign-lambda* int ((int a) (int b))
                      "C_return(GSL_MAX_INT(a, b));"))
(define csl:max-int GSL_MAX_INT)

(define GSL_MIN_INT (foreign-lambda* int ((int a) (int b))
                      "C_return(GSL_MIN_INT(a, b));"))
(define csl:min-int GSL_MIN_INT)
(define GSL_MAX_LDBL (foreign-lambda* long ((long a) (long b))
                       "C_return(GSL_MAX_LDBL(a, b));"))
(define csl:max-ldbl GSL_MAX_LDBL)
(define GSL_MIN_LDBL (foreign-lambda* long ((long a) (long b))
                       "C_return(GSL_MIN_LDBL(a, b));"))
(define csl:min-ldbl GSL_MIN_LDBL)

;; Approximate comparison of floating point numbers
(def-foreign-fns
  (bool "gsl_fcmp" double double double))
