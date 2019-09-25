(module gsl.permutation (ptr->permutation
                         permutation->ptr
                         permutation?
                         permutation-alloc
                         permutation-calloc
                         permutation-init!
                         permutation-free!
                         permutation-memcpy!
                         permutation-size
                         permutation-data
                         permutation-get
                         permutation-swap!
                         permutation-size
                         permutation-valid
                         permutation-reverse!
                         permutation-inverse!
                         permutation-next!
                         permutation-prev!
                         permutation-mul!
                         permutation-fwrite
                         permutation-fread!
                         permutation-fprintf
                         permutation-fscanf!
                         permutation-linear-to-canonical!
                         permutation-canonical-to-linear!
                         permutation-inversions
                         permutation-linear-cycles
                         permutation-canonical-cycles)
  (import scheme
          chicken.foreign
          (only chicken.base include define-record-type)
          (only srfi-4 make-u64vector make-u32vector))

  (include "utils/error-handler.scm")
  (include "utils/stdio.scm")

  (foreign-declare "#include <gsl/gsl_permutation.h>")

  (define-record-type permutation
    (ptr->permutation ptr)
    permutation?
    (ptr permutation->ptr))

  (define-foreign-type gsl-permutation
    (nonnull-c-pointer "gsl_permutation")
    permutation->ptr
    ptr->permutation)

  ;;; Permutation allocation
  (define permutation-alloc
    (foreign-lambda gsl-permutation "gsl_permutation_alloc" size_t))
  (define permutation-calloc
    (foreign-lambda gsl-permutation "gsl_permutation_calloc" size_t))

  (define permutation-init!
    (foreign-lambda void "gsl_permutation_init" gsl-permutation))
  (define permutation-free!
    (foreign-lambda void "gsl_permutation_free" gsl-permutation))
  (define permutation-memcpy!
    (foreign-safe-lambda void "gsl_permutation_memcpy" gsl-permutation gsl-permutation))

  ;;; Accessing permutation elements
  (define permutation-get
    (foreign-safe-lambda size_t "gsl_permutation_get" gsl-permutation size_t))
  (define permutation-swap!
    (foreign-safe-lambda gsl-errno "gsl_permutation_swap" gsl-permutation size_t size_t))

  ;;; Permutation properties
  (define permutation-size
    (foreign-lambda size_t "gsl_permutation_size" gsl-permutation))
  (define (permutation-data p)
    (let ((ret ((cond-expand (64bit make-u64vector) (else make-u32vector))
                (permutation-size p))))
      ((foreign-lambda* void ((gsl-permutation p) (c-pointer ret))
         "memcpy(ret, p->data, sizeof(size_t)*p->size);")
       p
       (location ret))
      ret))
  (define permutation-valid
    (foreign-safe-lambda gsl-errno "gsl_permutation_valid" gsl-permutation))

  ;;; Permutation functions
  (define permutation-reverse!
    (foreign-lambda void "gsl_permutation_reverse" gsl-permutation))
  (define permutation-inverse!
    (foreign-safe-lambda gsl-errno "gsl_permutation_inverse" gsl-permutation gsl-permutation))

  (define permutation-next!
    (foreign-safe-lambda int "gsl_permutation_next" gsl-permutation))
  (define permutation-prev!
    (foreign-safe-lambda int "gsl_permutation_prev" gsl-permutation))

  ;;; Applying permutations
  (define permutation-mul!
    (foreign-safe-lambda gsl-errno "gsl_permutation_mul" gsl-permutation gsl-permutation gsl-permutation))

  ;;; Reading and writing permutations
  (define (permutation-fwrite fileport permutation)
    (let* ((FILE (get-c-file 'permutation-fwrite fileport)))
      ((foreign-safe-lambda gsl-errno "gsl_permutation_fwrite"
         (c-pointer "FILE") gsl-permutation)
       FILE permutation)))

  (define (permutation-fread! fileport permutation)
    (let* ((FILE (get-c-file 'permutation-fread fileport)))
      ((foreign-safe-lambda gsl-errno "gsl_permutation_fread"
         (c-pointer "FILE") gsl-permutation)
       FILE permutation)))

  (define (permutation-fprintf fileport permutation format)
    (let* ((FILE (get-c-file 'permutation-fprintf fileport)))
      ((foreign-safe-lambda gsl-errno "gsl_permutation_fprintf"
         (c-pointer "FILE") gsl-permutation c-string)
       FILE permutation format)))

  (define (permutation-fscanf! fileport permutation)
    (let* ((FILE (get-c-file 'permutation-fscanf fileport)))
      ((foreign-safe-lambda gsl-errno "gsl_permutation_fscanf"
         (c-pointer "FILE") gsl-permutation)
       FILE permutation)))

  ;;; Permutations in cyclic form
  (define permutation-linear-to-canonical!
    (foreign-safe-lambda gsl-errno "gsl_permutation_linear_to_canonical" gsl-permutation gsl-permutation))
  (define permutation-canonical-to-linear!
    (foreign-safe-lambda gsl-errno "gsl_permutation_linear_to_canonical" gsl-permutation gsl-permutation))

  (define permutation-inversions
    (foreign-lambda size_t "gsl_permutation_inversions" gsl-permutation))
  (define permutation-linear-cycles
    (foreign-lambda size_t "gsl_permutation_linear_cycles" gsl-permutation))
  (define permutation-canonical-cycles
    (foreign-lambda size_t "gsl_permutation_canonical_cycles" gsl-permutation)))
