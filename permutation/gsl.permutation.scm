(module gsl.permutation (ptr->permutation
                         permutation->ptr
                         permutation?
                         permutation-alloc
                         permutation-calloc
                         permutation-init!
                         permutation-free!
                         permutation-memcpy!
                         permutation-size
                         permutation-get
                         permutation-swap!
                         permutation-size
                         permutation-valid?
                         permutation-reverse!
                         permutation-inverse!
                         permutation-next!
                         permutation-prev!
                         permutation-mul!
                         permutation-linear-to-canonical!
                         permutation-canonical-to-linear!
                         permutation-inversions
                         permutation-linear-cycles
                         permutation-canonical-cycles)
  (import scheme
          bind
          chicken.foreign
          (only chicken.base include define-record-type)
          (only chicken.gc set-finalizer!))

  (include "csl-error.scm")
  (include "bind-transformers.scm")

  (foreign-declare "#include <gsl/gsl_permutation.h>")

  (bind-options default-renaming: "")

  (bind-rename/pattern "^gsl-" "")

  (define-record-type permutation
    (ptr->permutation ptr)
    permutation?
    (ptr permutation->ptr))

  (bind-type
   csl_permutation
   (c-pointer "gsl_permutation")
   permutation->ptr
   ptr->permutation)

  ;;; Permutation allocation
  (bind-rename/pattern
   "(init|free|memcpy|swap|reverse|inverse|next|prev|mul|linear|canonical)$"
   "\\1!")

  (bind-rename "gsl_permutation_alloc" "%permutation-alloc")
  (bind-rename "gsl_permutation_calloc" "%permutation-calloc")
  (bind "csl_permutation gsl_permutation_alloc(size_t)")
  (bind "csl_permutation gsl_permutation_calloc(size_t)")

  (define (permutation-alloc n)
    (set-finalizer! (%permutation-alloc n) permutation-free!))

  (define (permutation-calloc n)
    (set-finalizer! (%permutation-calloc n) permutation-free!))

  (bind "void gsl_permutation_init(csl_permutation)")
  (bind "void gsl_permutation_free(csl_permutation)")
  (bind "___safe void gsl_permutation_memcpy(csl_permutation, csl_permutation)")

  (bind "size_t gsl_permutation_size(csl_permutation)")

  ;;; Accessing permutation elements
  (bind "___safe size_t gsl_permutation_get(csl_permutation, size_t)")
  (bind "___safe size_t gsl_permutation_swap(csl_permutation, size_t, size_t)")

  ;;; Permutation properties
  (bind "size_t gsl_permutation_size(csl_permutation)")
  (bind-rename "gsl_permutation_valid" "permutation-valid?")
  (bind "___safe int gsl_permutation_valid(csl_permutation)")

  ;;; Permutation functions
  (bind "void gsl_permutation_reverse(csl_permutation)")
  (bind "___safe int gsl_permutation_inverse(csl_permutation, csl_permutation)")

  (bind "int gsl_permutation_next(csl_permutation)")
  (bind "int gsl_permutation_prev(csl_permutation)")

  ;; Applying permutations
  (bind "___safe int gsl_permutation_mul(csl_permutation, csl_permutation, csl_permutation);")

  ;; Permutations in cyclic form
  (bind "___safe int gsl_permutation_linear_to_canonical (gsl_permutation * q, csl_permutation p);")
  (bind "___safe int gsl_permutation_canonical_to_linear (gsl_permutation * p, csl_permutation q);")

  (bind "size_t gsl_permutation_inversions (csl_permutation p);")
  (bind "size_t gsl_permutation_linear_cycles (csl_permutation p);")
  (bind "size_t gsl_permutation_canonical_cycles (csl_permutation q);"))
