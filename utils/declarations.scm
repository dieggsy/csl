(import (only chicken.format format)
        (only chicken.base error)
        chicken.foreign)

(foreign-declare "#include <gsl/gsl_errno.h>")

(define-external (scheme_make_rect (double r) (double i)) scheme-object
  (make-rectangular r i))

(define-foreign-type unsigned_byte unsigned-byte)
(define-foreign-type unsigned_short unsigned-short)
(define-foreign-type unsigned_int unsigned-int)
(define-foreign-type unsigned_long unsigned-long)

(foreign-declare "typedef char byte;")
(foreign-declare "typedef unsigned char unsigned_byte;")
(foreign-declare "typedef unsigned short unsigned_short;")
(foreign-declare "typedef unsigned int unsigned_int;")
(foreign-declare "typedef unsigned long unsigned_long;")


(define-external (csl_err (c-string reason) (c-string file) (int line) (int gsl_errno)) void
  (error (format "gsl: ~a:~a: ~a" file line reason)))
