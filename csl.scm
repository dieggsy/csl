;; (compile-file "gsl.scm" options: '("-C" "-lgsl" "`pkg-config --cflags --libs gsl`"))
(module csl *
  (import scheme
          chicken
          extras
          data-structures
          ports
          foreign)
  (use numbers
       foreigners
       generics
       fmt
       srfi-13
       srfi-4
       srfi-1
       irregex)

  (include "gsl-complex.scm")

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

  (include "csl-const.scm")

  )
