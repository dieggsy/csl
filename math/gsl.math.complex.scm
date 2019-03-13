(module gsl.math.complex (rect
                          polar
                          arg
                          abs
                          logabs
                          conjugate
                          inverse
                          negative
                          add
                          sub
                          mul
                          div
                          ;; add-real
                          ;; sub-real
                          ;; mul-real
                          ;; div-real
                          ;; add-imag
                          ;; sub-imag
                          ;; mul-imag
                          ;; div-imag
                          sqrt
                          sqrt-real
                          pow
                          pow-real
                          exp
                          log10
                          log
                          sin
                          arcsin
                          cos
                          arccos
                          tan
                          arctan
                          sec
                          arcsec
                          csc
                          arccsc
                          cot
                          arccot
                          sinh
                          arcsinh
                          cosh
                          arccosh
                          tanh
                          arctanh
                          sech
                          arcsech
                          csch
                          arccsch
                          coth
                          arccoth
                          )

  (import (except scheme
                  abs
                  sqrt
                  exp
                  log
                  sin
                  cos
                  tan)
          (only chicken.base include foldl error)
          bind
          (chicken foreign))

  (include "csl-error.scm")
  (include "bind-transformers.scm")

  (foreign-declare "#include <gsl/gsl_complex.h>")
  (foreign-declare "#include <gsl/gsl_complex_math.h>")

  (bind-options default-renaming: "")

  (bind-rename/pattern "^gsl-complex-" "")

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

  (bind-rename "gsl_complex_add" "%add")
  (bind "struct gsl_complex gsl_complex_add(struct gsl_complex, struct gsl_complex)")
  (define (add . args) (foldl %add 0 args))

  (bind-rename "gsl_complex_sub" "%sub")
  (bind "struct gsl_complex gsl_complex_sub(struct gsl_complex, struct gsl_complex)")
  (define (sub . args)
    (cond ((null? args)
           (error 'sub "too few arguments - received 0 but expected at least 1"))
          ((null? (cdr args))
           (negative (car args)))
          (else (foldl %sub (car args) (cdr args)))))
  (bind-rename "gsl_complex_mul" "%mul")
  (bind "struct gsl_complex gsl_complex_mul(struct gsl_complex, struct gsl_complex)")
  (define (mul . args) (foldl %mul 1 args))

  (bind-rename "gsl_complex_div" "%div")
  (bind "struct gsl_complex gsl_complex_div(struct gsl_complex, struct gsl_complex)")
  (define (div . args)
    (cond ((null? args)
           (error 'comple-div "too few arguments - received 0 but expected at least 1"))
          ((null? (cdr args))
           (inverse (car args)))
          (else (foldl %div (car args) (cdr args)))))

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

  (bind-rename "gsl_complex_log" "%log")
  (bind "struct gsl_complex gsl_complex_log(struct gsl_complex)")

  (bind "struct gsl_complex gsl_complex_log10(struct gsl_complex)")
  (bind "struct gsl_complex gsl_complex_log_b(struct gsl_complex, struct gsl_complex)")

  (define (log a #!optional b)
    (if b
        (log-b a b)
        (%log a)))

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
