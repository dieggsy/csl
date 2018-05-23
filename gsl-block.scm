(foreign-declare "#include <gsl/gsl_block.h>")
;; (foreign-declare "#include <gsl/gsl_block_complex_double.h>")

(define-foreign-record-type (gsl_block_complex "gsl_block_complex")
  (unsigned-int size gsl_block_complex.size)
  ((c-pointer double) data gsl_block_complex.data))

;; Block allocation
(define gsl_block_complex_free
  (foreign-safe-lambda void "gsl_block_complex_free" gsl_block_complex))

(define gsl_block_complex_alloc
  (foreign-safe-lambda gsl_block_complex "gsl_block_complex_alloc" unsigned-int))
(define (gsl_block_complex_alloc_gc n)
  (set-finalizer! (gsl_block_complex_alloc n) gsl_block_complex_free))

(define gsl_block_complex_calloc
  (foreign-safe-lambda gsl_block_complex "gsl_block_complex_calloc" unsigned-int))
(define (gsl_block_complex_calloc_gc n)
  (set-finalizer! (gsl_block_complex_calloc n) gsl_block_complex_free))
