(import (only chicken.format format)
        (only chicken.base error)
        chicken.foreign)

(foreign-declare "#include <gsl/gsl_errno.h>")

(define gsl_set_error_handler
  (foreign-lambda void "gsl_set_error_handler" (c-pointer void)))

(define-external (csl_err (c-string reason) (c-string file) (int line) (int gsl_errno)) void
  (error (format "gsl: ~a:~a: ~a" file line reason)))

(gsl_set_error_handler (location csl_err))
