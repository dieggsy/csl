(import chicken.syntax)

(import-for-syntax number-limits
                   (only chicken.base symbol-append)
                   (only chicken.string substring-index))

(define-for-syntax (complex-ctype-string type)
  (case type
    ((complex)
     "gsl_complex")
    ((complex-float)
     "gsl_complex_float")
    (else "ERROR")))

(define-for-syntax (sizeof-type-str type)
  (case type
    ((byte int long short unsigned_byte unsigned_int unsigned_long unsigned_short)
     "C_SIZEOF_FIX_BIGNUM")
    ((double float)
     "C_SIZEOF_FLONUM")
    (else "ERROR")))

(define-for-syntax (c-constructor-str type)
  (case type
    ((byte int long short unsigned_byte unsigned_int unsigned_long unsigned_short)
     "C_int_to_num")
    ((double float)
     "C_flonum")
    (else "ERROR")))

(define-for-syntax sizet-array-foreign-type
  (cond-expand (64bit 'u64vector) (else 'u32vector)))

(define-for-syntax (ctype-array-foreign-type base-type)
  (let ((prefix (cond ((or (eqv? base-type 'double)
                           (eqv? base-type 'float))
                       'f)
                      ((substring-index "unsigned" (symbol->string base-type))
                       'u)
                      (else 's))))
    (symbol-append
     prefix
     (string->symbol
      (number->string
       (* 8
          (case base-type
            ((byte unsigned_byte) char-size)
            ((short unsigned_short) short-size)
            ((int unsigned_int) int-size)
            ((long unsigned_long) long-size)
            ((double) 8)
            ((float) 4)))))
     'vector)))

