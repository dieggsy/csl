(module gsl.mode (mode/double
                  mode/single
                  mode/approx)
  (import scheme chicken.foreign)
  (foreign-declare "#include <gsl/gsl_mode.h>")
  (define mode/double (foreign-value "GSL_PREC_DOUBLE" unsigned-int))
  (define mode/single (foreign-value "GSL_PREC_SINGLE" unsigned-int))
  (define mode/approx (foreign-value "GSL_PREC_APPROX" unsigned-int)))
