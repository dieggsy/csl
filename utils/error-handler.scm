(define gsl_set_error_handler
  (foreign-lambda void "gsl_set_error_handler" (c-pointer void)))

(gsl_set_error_handler (location csl_err))
