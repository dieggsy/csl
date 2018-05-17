(foreign-declare "#include <gsl/gsl_complex.h>")
(foreign-declare "#include <gsl/gsl_complex_math.h>")


(define-external (numbers_make_rect (double r) (double i)) scheme-object
  (make-rectangular r i))

(define gsl_complex_rect
  (foreign-safe-lambda* scheme-object ((double x) (double y))
    "gsl_complex z = gsl_complex_rect(x,y);"
    "C_return(numbers_make_rect(GSL_REAL(z),GSL_IMAG(z)));"))

(define gsl_complex_polar
  (foreign-safe-lambda* scheme-object ((double x) (double t))
    "gsl_complex z = gsl_complex_polar(x,t);"
    "C_return(numbers_make_rect(GSL_REAL(z),GSL_IMAG(z)));"))

(define (gsl_complex_arg z)
  (let ((r (real-part z))
        (i (imag-part z)))
    ((foreign-safe-lambda* double ((double r) (double i))
       "gsl_complex z = gsl_complex_rect(r,i);"
       "C_return(gsl_complex_arg(z));")
     r
     i)))

(define (gsl_complex_abs z)
  (let ((r (real-part z))
        (i (imag-part z)))
    ((foreign-safe-lambda* double ((double r) (double i))
       "gsl_complex z = gsl_complex_rect(r,i);"
       "C_return(gsl_complex_abs(z));")
     r
     i)))

(define (gsl_complex_abs2 z)
  (let ((r (real-part z))
        (i (imag-part z)))
    ((foreign-safe-lambda* double ((double r) (double i))
       "gsl_complex z = gsl_complex_rect(r,i);"
       "C_return(gsl_complex_abs2(z));")
     r
     i)))

(define (gsl_complex_logabs z)
  (let ((r (real-part z))
        (i (imag-part z)))
    ((foreign-safe-lambda* double ((double r) (double i))
       "gsl_complex z = gsl_complex_rect(r,i);"
       "C_return(gsl_complex_logabs(z));")
     r
     i)))

(define-syntax complex-defmath2
  (ir-macro-transformer
   (lambda (e i c)
     `(define (,(cadr e) z1 z2)
        (let ((r1 (real-part z1))
              (r2 (real-part z2))
              (i1 (imag-part z1))
              (i2 (imag-part z2)))
          ((foreign-safe-lambda* scheme-object ((double r1) (double i1)
                                                (double r2) (double i2))
             "gsl_complex zin1 = gsl_complex_rect(r1,i1);"
             "gsl_complex zin2 = gsl_complex_rect(r2,i2);"
             ,(conc "gsl_complex zout = " (strip-syntax (cadr e)) "(zin1,zin2);")
             "C_return(numbers_make_rect(GSL_REAL(zout),GSL_IMAG(zout)));")
           r1 i1 r2 i2))))))

(define-syntax complex-defmath2-dbl
  (ir-macro-transformer
   (lambda (e i c)
     `(define (,(cadr e) z d)
        (let ((r (real-part z))
              (i (imag-part z)))
          ((foreign-safe-lambda* scheme-object ((double r) (double i) (double d))
             "gsl_complex zin = gsl_complex_rect(r,i);"
             ,(conc "gsl_complex zout = " (strip-syntax (cadr e)) "(zin,d);")
             "C_return(numbers_make_rect(GSL_REAL(zout),GSL_IMAG(zout)));")
           r i d))))))

(define-syntax complex-defmath1
  (ir-macro-transformer
   (lambda (e i c)
     `(define (,(cadr e) z)
        (let ((r (real-part z))
              (i (imag-part z)))
          ((foreign-safe-lambda* scheme-object ((double r) (double i))
             "gsl_complex zin = gsl_complex_rect(r,i);"
             ,(conc "gsl_complex zout = " (strip-syntax (cadr e)) "(zin);")
             "C_return(numbers_make_rect(GSL_REAL(zout),GSL_IMAG(zout)));")
           r i))))))

(complex-defmath2 gsl_complex_add)
(complex-defmath2 gsl_complex_sub)
(complex-defmath2 gsl_complex_mul)
(complex-defmath2 gsl_complex_div)
(complex-defmath2-dbl gsl_complex_add_real)
(complex-defmath2-dbl gsl_complex_sub_real)
(complex-defmath2-dbl gsl_complex_mul_real)
(complex-defmath2-dbl gsl_complex_div_real)
(complex-defmath2-dbl gsl_complex_add_imag)
(complex-defmath2-dbl gsl_complex_sub_imag)
(complex-defmath2-dbl gsl_complex_mul_imag)
(complex-defmath2-dbl gsl_complex_div_imag)
(complex-defmath1 gsl_complex_conjugate)
(complex-defmath1 gsl_complex_inverse)
(complex-defmath1 gsl_complex_negative)

(complex-defmath1 gsl_complex_sqrt)
(define gsl_complex_sqrt_real
  (foreign-safe-lambda* scheme-object ((double x))
    "gsl_complex z = gsl_complex_sqrt_real(x);"
    "C_return(numbers_make_rect(GSL_REAL(z),GSL_IMAG(z)));"))
(complex-defmath2 gsl_complex_pow)
(complex-defmath2-dbl gsl_complex_pow_real)
(complex-defmath1 gsl_complex_exp)
(complex-defmath1 gsl_complex_log)
(complex-defmath1 gsl_complex_log10)
(complex-defmath2 gsl_complex_log_b)

(complex-defmath1 gsl_complex_sin)
(complex-defmath1 gsl_complex_cos)
(complex-defmath1 gsl_complex_tan)
(complex-defmath1 gsl_complex_sec)
(complex-defmath1 gsl_complex_csc)
(complex-defmath1 gsl_complex_cot)

(complex-defmath1 gsl_complex_arcsin)
(define gsl_complex_arcsin_real
  (foreign-safe-lambda* scheme-object ((double x))
    "gsl_complex z = gsl_complex_arcsin_real(x);"
    "C_return(numbers_make_rect(GSL_REAL(z),GSL_IMAG(z)));"))

(complex-defmath1 gsl_complex_arccos)
(define gsl_complex_arccos_real
  (foreign-safe-lambda* scheme-object ((double x))
    "gsl_complex z = gsl_complex_arccos_real(x);"
    "C_return(numbers_make_rect(GSL_REAL(z),GSL_IMAG(z)));"))

(complex-defmath1 gsl_complex_arctan)

(complex-defmath1 gsl_complex_arcsec)
(define gsl_complex_arcsec_real
  (foreign-safe-lambda* scheme-object ((double x))
    "gsl_complex z = gsl_complex_arcsec_real(x);"
    "C_return(numbers_make_rect(GSL_REAL(z),GSL_IMAG(z)));"))

(complex-defmath1 gsl_complex_arccsc)
(define gsl_complex_arccsc_real
  (foreign-safe-lambda* scheme-object ((double x))
    "gsl_complex z = gsl_complex_arccsc_real(x);"
    "C_return(numbers_make_rect(GSL_REAL(z),GSL_IMAG(z)));"))

(complex-defmath1 gsl_complex_arccot)

(complex-defmath1 gsl_complex_sinh)
(complex-defmath1 gsl_complex_cosh)
(complex-defmath1 gsl_complex_tanh)
(complex-defmath1 gsl_complex_sech)
(complex-defmath1 gsl_complex_csch)
(complex-defmath1 gsl_complex_coth)

(complex-defmath1 gsl_complex_arcsinh)
(complex-defmath1 gsl_complex_arccosh)
(define gsl_complex_arccosh_real
  (foreign-safe-lambda* scheme-object ((double x))
    "gsl_complex z = gsl_complex_arccosh_real(x);"
    "C_return(numbers_make_rect(GSL_REAL(z),GSL_IMAG(z)));"))
(complex-defmath1 gsl_complex_arctanh)
(define gsl_complex_arctanh_real
  (foreign-safe-lambda* scheme-object ((double x))
    "gsl_complex z = gsl_complex_arctanh_real(x);"
    "C_return(numbers_make_rect(GSL_REAL(z),GSL_IMAG(z)));"))
(complex-defmath1 gsl_complex_arcsech)
(complex-defmath1 gsl_complex_arccsch)
(complex-defmath1 gsl_complex_arccoth)
