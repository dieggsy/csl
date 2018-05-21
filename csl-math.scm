(module csl-math ()
  (import chicken scheme foreign irregex extras)
  (foreign-declare "#include <gsl/gsl_math.h>")

  (define-syntax def-const
    (ir-macro-transformer
     (lambda (e i c)
       (let* ((type (cadr e))
              (name (caddr e))
              (gsl-name (string-upcase (conc type "_" name)))
              (csl-name
               (string->symbol
                (conc "csl:"
                      (if (null? (cdddr e))
                          (string-downcase
                           (irregex-replace/all "_" name "-"))
                          (strip-syntax
                           (cadddr e)))))))
         `(begin
            (export ,(string->symbol gsl-name))
            (define ,(string->symbol gsl-name)
              (foreign-value ,gsl-name double))
            (export ,csl-name)
            (define ,csl-name ,(string->symbol gsl-name)))))))

  (define-syntax def-cfn
    (ir-macro-transformer
     (lambda (e i c)
       (let* ((name (cadadr e))
              (gsl-name (conc "gsl_" name))
              (csl-name
               (string->symbol
                (conc "csl:"
                      (if (null? (cddr e))
                          (string-downcase
                           (irregex-replace/all "_" name "-"))
                          (strip-syntax
                           (caddr e))))))
              (args (cddadr e)))
         `(begin
            (export ,(string->symbol gsl-name))
            (define ,(string->symbol gsl-name)
              (foreign-lambda ,(caadr e) ,gsl-name ,@args))
            (export ,csl-name)
            (define ,csl-name
              ,(string->symbol gsl-name)))))))

  (define-syntax def-cmacro
    (ir-macro-transformer
     (lambda (e i c)
       (let* ((name (cadadr e))
              (gsl-name (conc "GSL_" name))
              (csl-name
               (string->symbol
                (conc "csl:"
                      (if (null? (cddr e))
                          (string-downcase
                           (irregex-replace/all "_" name "-"))
                          (strip-syntax
                           (caddr e))))))
              (args (map strip-syntax (car (cddadr e)))))
         `(begin
            (export ,(string->symbol gsl-name))
            (define ,(string->symbol gsl-name)
              (foreign-lambda* ,(caadr e) ,args
                ,(format "C_return(~a(~a));" gsl-name
                         (string-join
                          (map symbol->string (map cadr args))
                          ","))))
            (export ,csl-name)
            (define ,csl-name
              ,(string->symbol gsl-name)))))))

  ;; Constants
  (def-const m: "E")
  (def-const m: "LOG2E")
  (def-const m: "LOG10E")
  (def-const m: "SQRT2")
  (def-const m: "SQRT1_2" sqrt1/2)
  (def-const m: "SQRT3")
  (def-const m: "PI")
  (def-const m: "PI_2" pi/2)
  (def-const m: "PI_4" pi/4)
  (def-const m: "SQRTPI")
  (def-const m: "2_SQRTPI" 2/sqrtpi)
  (def-const m: "1_PI" 1/pi)
  (def-const m: "2_PI" 2/pi)
  (def-const m: "LN10")
  (def-const m: "LN2")
  (def-const m: "LNPI")
  (def-const m: "EULER")

  ;; Infinities and NAN
  (def-const gsl: "POSINF" +inf)
  (def-const gsl: "NEGINF" -inf)
  (def-const gsl: "NAN")

  (def-cfn (bool "isnan" (const double)) nan?)
  (def-cfn (bool "isinf" (const double)) inf?)
  (def-cfn (bool "finite" (const double)) finite?)

  ;; Elementary functions
  (def-cfn (double "log1p" (const double)))
  (def-cfn (double "hypot" (const double) (const double)))
  (def-cfn (double "hypot3" (const double) (const double) (const double)))
  (def-cfn (double "acosh" (const double)))
  (def-cfn (double "asinh" (const double)))
  (def-cfn (double "atanh" (const double)))
  (def-cfn (double "ldexp" double int))
  (def-cfn (double "frexp" double (c-pointer int)))

  ;; Small integer powers
  (def-cfn (double "pow_int" double int) expt-int)
  (def-cfn (double "pow_uint" double unsigned-int) expt-uint)
  (def-cfn (double "pow_2" (const double)) expt2)
  (def-cfn (double "pow_3" (const double)) expt3)
  (def-cfn (double "pow_4" (const double)) expt4)
  (def-cfn (double "pow_5" (const double)) expt5)
  (def-cfn (double "pow_6" (const double)) expt6)
  (def-cfn (double "pow_7" (const double)) expt7)
  (def-cfn (double "pow_8" (const double)) expt8)
  (def-cfn (double "pow_9" (const double)) expt9)

  (def-cmacro (int "SIGN" ((double x))))
  (def-cmacro (bool "IS_ODD" ((int x))) odd?)
  (def-cmacro (bool "IS_EVEN" ((int x))) even?)

  ;; Maximum and minimum functions
  (def-cmacro (double "MAX" ((double a) (double b))))
  (def-cmacro (double "MIN" ((double a) (double b))))
  (def-cmacro (double "MAX_DBL" ((double a) (double b))))
  (def-cmacro (double "MIN_DBL" ((double a) (double b))))
  (def-cmacro (int "MAX_INT" ((int a) (int b))))
  (def-cmacro (int "MIN_INT" ((int a) (int b))))
  (def-cmacro (long "MAX_LDBL" ((long a) (long b))))
  (def-cmacro (long "MIN_LDBL" ((long a) (long b))))

  ;; Approximate comparison of floating point numbers
  (def-cfn (double "fcmp" double double double)))
