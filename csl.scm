;; (compile-file "gsl.scm" options: '("-C" "-lgsl" "`pkg-config --cflags --libs gsl`"))
(module csl *
  (import scheme
          chicken
          extras
          data-structures
          ports
          foreign)
  (use numbers
       bind
       foreigners
       generics
       fmt
       srfi-13)

  (foreign-declare "#include <gsl/gsl_errno.h>")

  (define gsl_set_error_handler
    (foreign-lambda void "gsl_set_error_handler" (c-pointer void)))

  (define-external (csl_err (c-string reason) (c-string file) (int line) (int gsl_errno)) void
    (error (format "gsl: ~a:~a: ERROR: ~a" file line reason)))

  (gsl_set_error_handler (location csl_err))

  (include "csl-math.scm")

  (foreign-declare "#include <gsl/gsl_block.h>")

  (define-foreign-record-type (gsl_block "gsl_block")
    (unsigned-int size block-size)
    ((c-pointer double) data block-data))

  ;; Block allocation
  (define gsl_block_free
    (foreign-safe-lambda void "gsl_block_free" gsl_block))

  (define gsl_block_alloc
    (foreign-safe-lambda gsl_block "gsl_block_alloc" unsigned-int))
  (define (gsl_block_alloc_gc n)
    (set-finalizer! (gsl_block_alloc n) gsl_block_free))

  (define gsl_block_calloc
    (foreign-safe-lambda gsl_block "gsl_block_calloc" unsigned-int))
  (define (gsl_block_calloc_gc n)
    (set-finalizer! (gsl_block_calloc n) gsl_block_free))

  (include "gsl-vector.scm")
  (include "gsl-matrix.scm")
  (include "csl-vector.scm")
  (include "csl-matrix.scm")
  ;; (include "gsl-complex.scm")

  )
