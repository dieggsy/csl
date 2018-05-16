(use bind foreigners numbers)

(foreign-declare "#include <gsl/gsl_complex.h>")
(foreign-declare "#include <gsl/gsl_complex_math.h>")

(define-foreign-record-type (gcomplex "gsl_complex")
  (double dat gcomplex-dat))

(define complex-rect (foreign-lambda gcomplex "gsl_complex_rect" double double))
