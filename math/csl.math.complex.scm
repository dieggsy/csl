(module csl.math.complex (complex-rect
                          complex-polar
                          complex-arg
                          complex-abs
                          complex-logabs
                          complex-conjugate
                          complex-inverse
                          complex-negative
                          complex-add
                          complex-sub
                          complex-mul
                          complex-div
                          ;; complex-add-real
                          ;; complex-sub-real
                          ;; complex-mul-real
                          ;; complex-div-real
                          ;; complex-add-imag
                          ;; complex-sub-imag
                          ;; complex-mul-imag
                          ;; complex-div-imag
                          complex-sqrt
                          complex-sqrt-real
                          complex-pow
                          complex-pow-real
                          complex-exp
                          complex-log10
                          complex-log
                          complex-sin
                          complex-arcsin
                          complex-cos
                          complex-arccos
                          complex-tan
                          complex-arctan
                          complex-sec
                          complex-arcsec
                          complex-csc
                          complex-arccsc
                          complex-cot
                          complex-arccot
                          complex-sinh
                          complex-arcsinh
                          complex-cosh
                          complex-arccosh
                          complex-tanh
                          complex-arctanh
                          complex-sech
                          complex-arcsech
                          complex-csch
                          complex-arccsch
                          complex-coth
                          complex-arccoth
                          )

  (import scheme
          (only chicken.base include foldl error)
          bind
          (chicken foreign))

  (include "csl-error.scm")
  (include "bind-transformers.scm")

  (foreign-declare "#include <gsl/gsl_complex.h>")
  (foreign-declare "#include <gsl/gsl_complex_math.h>")

  (bind-options default-renaming: "")
  (bind-rename/pattern "^gsl-" "")

  ;;; Representation/conversion
  (bind "struct gsl_complex gsl_complex_rect(double, double)")
  (bind "struct gsl_complex gsl_complex_polar(double, double)")

  ;;; Properties
  (bind "double gsl_complex_arg(struct gsl_complex)")
  (bind "double gsl_complex_abs(struct gsl_complex)")
  (bind "double gsl_complex_abs2(struct gsl_complex)")
  (bind "double gsl_complex_logabs(struct gsl_complex)")


  ;;; Arithmetic operators
  (bind "struct gsl_complex gsl_complex_conjugate(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_inverse(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_negative(struct gsl_complex)")

  (bind-rename "gsl_complex_add" "%complex-add")
  (bind "struct gsl_complex gsl_complex_add(struct gsl_complex, struct gsl_complex)")
  (define (complex-add . args) (foldl %complex-add 0 args))

  (bind-rename "gsl_complex_sub" "%complex-sub")
  (bind "struct gsl_complex gsl_complex_sub(struct gsl_complex, struct gsl_complex)")
  (define (complex-sub . args)
    (cond ((null? args)
           (error 'complex-sub "too few arguments - received 0 but expected at least 1"))
          ((null? (cdr args))
           (complex-negative (car args)))
          (else (foldl %complex-sub (car args) (cdr args)))))
  (bind-rename "gsl_complex_mul" "%complex-mul")
  (bind "struct gsl_complex gsl_complex_mul(struct gsl_complex, struct gsl_complex)")
  (define (complex-mul . args) (foldl %complex-mul 1 args))

  (bind-rename "gsl_complex_div" "%complex-div")
  (bind "struct gsl_complex gsl_complex_div(struct gsl_complex, struct gsl_complex)")
  (define (complex-div . args)
    (cond ((null? args)
           (error 'comple-div "too few arguments - received 0 but expected at least 1"))
          ((null? (cdr args))
           (complex-inverse (car args)))
          (else (foldl %complex-div (car args) (cdr args)))))

  ;; (bind "struct gsl_complex gsl_complex_add_real(struct gsl_complex, double)")
  ;; (bind "struct gsl_complex gsl_complex_sub_real(struct gsl_complex, double)")
  ;; (bind "struct gsl_complex gsl_complex_mul_real(struct gsl_complex, double)")
  ;; (bind "struct gsl_complex gsl_complex_div_real(struct gsl_complex, double)")

  ;; (bind "struct gsl_complex gsl_complex_add_imag(struct gsl_complex, double)")
  ;; (bind "struct gsl_complex gsl_complex_sub_imag(struct gsl_complex, double)")
  ;; (bind "struct gsl_complex gsl_complex_mul_imag(struct gsl_complex, double)")
  ;; (bind "struct gsl_complex gsl_complex_div_imag(struct gsl_complex, double)")

  ;;; Elementary functions
  (bind "struct gsl_complex gsl_complex_sqrt(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_sqrt_real(double)")
  (bind "struct gsl_complex gsl_complex_pow(struct gsl_complex, struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_pow_real(struct gsl_complex,double)")
  (bind "struct gsl_complex gsl_complex_exp(struct gsl_complex)")

  (bind-rename "gsl_complex_log" "%complex-log")
  (bind "struct gsl_complex gsl_complex_log(struct gsl_complex)")

  (bind "struct gsl_complex gsl_complex_log10(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_log_b(struct gsl_complex, struct gsl_complex)")

  (define (complex-log a #!optional b)
    (if b
        (complex-log-b a b)
        (%complex-log a)))

  ;;; Trigonometric functions
  (bind "struct gsl_complex gsl_complex_sin(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_cos(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_tan(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_sec(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_csc(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_cot(struct gsl_complex)")


  ;;; Inverse trig
  (bind "struct gsl_complex gsl_complex_arcsin(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_arccos(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_arctan(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_arcsec(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_arccsc(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_arccot(struct gsl_complex)")

  ;;; Hyperbolic functions
  (bind "struct gsl_complex gsl_complex_sinh(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_cosh(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_tanh(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_sech(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_csch(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_coth(struct gsl_complex)")

  ;;; Inverse hyperbolic functions
  (bind "struct gsl_complex gsl_complex_arcsinh(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_arccosh(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_arctanh(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_arcsech(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_arccsch(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_arccoth(struct gsl_complex)"))
