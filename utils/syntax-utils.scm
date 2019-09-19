(import chicken.syntax)

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

(define-for-syntax (sizeof-ctype-expr type)
  (case type
    ((complex) (* 2 (foreign-value "sizeof(double)" size_t)))
    ((double) (foreign-value "sizeof(double)" size_t))
      ))
