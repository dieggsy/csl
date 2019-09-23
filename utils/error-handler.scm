(import (only gsl.errno errno->error))

(define-foreign-type gsl-errno
  int
  #f
  (lambda (errno) (errno->error errno 'return)))
