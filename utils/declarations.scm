(import (only chicken.base error when alist-ref define-constant)
        (only chicken.condition signal condition)
        (only gsl.errno errno->error set-error-handler!)
        chicken.foreign)

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
(foreign-declare "
#define CONCAT3_SPECIAL(a,b,c) a##b##_##c
#define CONCAT2_SPECIAL(a,b) a##b
#define TYPED_FN(prefix, type, fn) CONCAT3_SPECIAL(prefix,type,fn)
#define SUBTYPE(type, subtype) CONCAT2_SPECIAL(type,subtype)
")

(define-external (csl_err (c-string reason) (c-string file) (int line) (int errno)) void
  (errno->error errno 'handler file line reason))

(set-error-handler! (location csl_err))
