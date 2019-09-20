(include "utils/declarations.scm")

(module gsl.math.complex (arg
                          abs
                          abs2
                          logabs
                          conjugate
                          inverse
                          negative
                          add
                          sub
                          mul
                          div
                          add-real
                          sub-real
                          mul-real
                          div-real
                          add-imag
                          sub-imag
                          mul-imag
                          div-imag
                          sqrt
                          sqrt-real
                          pow
                          pow-real
                          exp
                          log10
                          log-b
                          sin
                          cos
                          tan
                          sec
                          csc
                          cot
                          arcsin
                          arccos
                          arctan
                          arcsec
                          arccsc
                          arccot
                          arcsin-real
                          arccos-real
                          arcsec-real
                          arccsc-real
                          sinh
                          cosh
                          tanh
                          sech
                          csch
                          coth
                          arcsinh
                          arccosh
                          arctanh
                          arcsech
                          arccsch
                          arccoth
                          arccosh-real
                          arctanh-real)

  (import (except scheme
                  abs
                  sqrt
                  exp
                  log
                  sin
                  cos
                  tan)
          (only chicken.base include foldl error)
          (chicken foreign))

  (include "utils/error-handler.scm")
  (include "utils/complex-types.scm")
  ;; (include "csl-error.scm")
  ;; (include "bind-transformers.scm")

  ;; (foreign-declare "#include <gsl/gsl_complex.h>")
  (foreign-declare "#include <gsl/gsl_complex_math.h>")

  ;; (bind-options default-renaming: "")

  ;; (bind-rename/pattern "^gsl-complex-" "")

  ;;; Representation/conversion
  ;; Skipped rect, polar - this does absolutely nothing for us, seeing as we
  ;; have to convert back into scheme complex with make-rectangular in a
  ;; define-external fn. Literally only overhead.

  ;;; Properties
  (define arg
    (foreign-lambda* double ((complex z))
      "C_return(gsl_complex_arg(gsl_complex_rect(z[0],z[1])));"))
  (define abs
    (foreign-lambda* double ((complex z))
      "C_return(gsl_complex_abs(gsl_complex_rect(z[0],z[1])));"))
  (define abs2
    (foreign-lambda* double ((complex z))
      "C_return(gsl_complex_abs2(gsl_complex_rect(z[0],z[1])));"))
  (define logabs
    (foreign-lambda* double ((complex z))
      "C_return(gsl_complex_logabs(gsl_complex_rect(z[0],z[1])));"))

  ;;; Arithmetic operators
  (define conjugate
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_conjugate(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define inverse
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_inverse(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define negative
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_negative(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))

  ;; Largely useless
  (define add
    (foreign-safe-lambda* scheme-object ((complex z) (complex z1))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _z1 = gsl_complex_rect(z1[0],z1[1]);"
      "gsl_complex _res = gsl_complex_add(_z, _z1);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))
  (define sub
    (foreign-safe-lambda* scheme-object ((complex z) (complex z1))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _z1 = gsl_complex_rect(z1[0],z1[1]);"
      "gsl_complex _res = gsl_complex_sub(_z, _z1);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))
  (define mul
    (foreign-safe-lambda* scheme-object ((complex z) (complex z1))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _z1 = gsl_complex_rect(z1[0],z1[1]);"
      "gsl_complex _res = gsl_complex_mul(_z, _z1);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))
  (define div
    (foreign-safe-lambda* scheme-object ((complex z) (complex z1))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _z1 = gsl_complex_rect(z1[0],z1[1]);"
      "gsl_complex _res = gsl_complex_div(_z, _z1);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))

  (define add-real
    (foreign-safe-lambda* scheme-object ((complex z) (double n))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _res = gsl_complex_add_real(_z, n);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))
  (define sub-real
    (foreign-safe-lambda* scheme-object ((complex z) (double n))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _res = gsl_complex_sub_real(_z, n);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))
  (define mul-real
    (foreign-safe-lambda* scheme-object ((complex z) (double n))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _res = gsl_complex_mul_real(_z, n);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))
  (define div-real
    (foreign-safe-lambda* scheme-object ((complex z) (double n))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _res = gsl_complex_div_real(_z, n);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))

  (define add-imag
    (foreign-safe-lambda* scheme-object ((complex z) (double n))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _res = gsl_complex_add_imag(_z, n);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))
  (define sub-imag
    (foreign-safe-lambda* scheme-object ((complex z) (double n))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _res = gsl_complex_sub_imag(_z, n);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))
  (define mul-imag
    (foreign-safe-lambda* scheme-object ((complex z) (double n))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _res = gsl_complex_mul_imag(_z, n);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))
  (define div-imag
    (foreign-safe-lambda* scheme-object ((complex z) (double n))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _res = gsl_complex_div_imag(_z, n);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))

  ;;; Elementary functions
  (define sqrt
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_sqrt(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define sqrt-real
    (foreign-safe-lambda* scheme-object ((double n))
      "gsl_complex _z = gsl_complex_sqrt_real(n);"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define pow
    (foreign-safe-lambda* scheme-object ((complex z) (complex z1))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _z1 = gsl_complex_rect(z1[0],z1[1]);"
      "gsl_complex _res = gsl_complex_pow(_z, _z1);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))
  (define pow-real
    (foreign-safe-lambda* scheme-object ((complex z) (double n))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _res = gsl_complex_pow_real(_z, n);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))
  (define exp
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_exp(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define log
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_log(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define log10
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_log10(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define log-b
    (foreign-safe-lambda* scheme-object ((complex z) (complex z1))
      "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
      "gsl_complex _z1 = gsl_complex_rect(z1[0],z1[1]);"
      "gsl_complex _res = gsl_complex_log_b(_z, _z1);"
      "C_return(scheme_make_rect(GSL_REAL(_res),GSL_IMAG(_res)));"))

  ;;; Trigonometric functions
  (define sin
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_sin(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define cos
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_cos(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define tan
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_tan(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define sec
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_sec(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define csc
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_csc(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define cot
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_cot(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))

  ;;; Inverse trig
  (define arcsin
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arcsin(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arccos
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arccos(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arctan
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arctan(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arcsec
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arcsec(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arccsc
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arccsc(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arccot
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arccot(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))

  (define arcsin-real
    (foreign-safe-lambda* scheme-object ((double n))
      "gsl_complex _z = gsl_complex_arcsin_real(n);"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arccos-real
    (foreign-safe-lambda* scheme-object ((double n))
      "gsl_complex _z = gsl_complex_arccos_real(n);"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arcsec-real
    (foreign-safe-lambda* scheme-object ((double n))
      "gsl_complex _z = gsl_complex_arcsec_real(n);"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arccsc-real
    (foreign-safe-lambda* scheme-object ((double n))
      "gsl_complex _z = gsl_complex_arccsc_real(n);"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))

  ;;; Hyperbolic functions
  (define sinh
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_sinh(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define cosh
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_cosh(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define tanh
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_tanh(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define sech
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_sech(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define csch
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_csch(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define coth
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_coth(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))

  ;;; Inverse hyperbolic functions
  (define arcsinh
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arcsinh(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arccosh
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arccosh(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arctanh
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arctanh(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arcsech
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arcsech(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arccsch
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arccsch(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arccoth
    (foreign-safe-lambda* scheme-object ((complex z))
      "gsl_complex _z = gsl_complex_arccoth(gsl_complex_rect(z[0],z[1]));"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))

  (define arccosh-real
    (foreign-safe-lambda* scheme-object ((double n))
      "gsl_complex _z = gsl_complex_arccosh_real(n);"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));"))
  (define arctanh-real
    (foreign-safe-lambda* scheme-object ((double n))
      "gsl_complex _z = gsl_complex_arctanh_real(n);"
      "C_return(scheme_make_rect(GSL_REAL(_z), GSL_IMAG(_z)));")))

