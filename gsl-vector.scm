(foreign-declare "#include <gsl/gsl_vector.h>")

(define-foreign-record-type (gsl_vector "gsl_vector")
  (unsigned-int size gsl_vector.size)
  (unsigned-int stride gsl_vector.stride)
  ((c-pointer double) data gsl_vector.data)
  (gsl_block block gsl_vector.block)
  (int owner gsl_vector.owner))

;; Vector allocation
(define gsl_vector_free
  (foreign-safe-lambda void "gsl_vector_free" gsl_vector))

(define gsl_vector_alloc
  (foreign-safe-lambda gsl_vector "gsl_vector_alloc" unsigned-int))
(define (gsl_vector_alloc_gc n)
  (set-finalizer! (gsl_vector_alloc n) gsl_vector_free))

(define gsl_vector_calloc
  (foreign-safe-lambda gsl_vector "gsl_vector_calloc" unsigned-int))
(define (gsl_vector_calloc_gc n)
  (set-finalizer! (gsl_vector_calloc n) gsl_vector_free))

;; Accessing vector elements
(define gsl_vector_get
  (foreign-safe-lambda double "gsl_vector_get"
    (const gsl_vector)
    (const unsigned-int)))

(define gsl_vector_set
  (foreign-safe-lambda void "gsl_vector_set"
    gsl_vector
    (const unsigned-int)
    (const double)))

(define gsl_vector_ptr
  (foreign-safe-lambda (c-pointer double) "gsl_vector_ptr"
    gsl_vector
    unsigned-int))

(define gsl_vector_const_ptr
  (foreign-safe-lambda (c-pointer double) "gsl_vector_const_ptr"
    gsl_vector
    unsigned-int))

;; Initializing vector elements
(define gsl_vector_set_all
  (foreign-safe-lambda void "gsl_vector_set_all" gsl_vector double))

(define gsl_vector_set_zero
  (foreign-safe-lambda void "gsl_vector_set_zero" gsl_vector))

(define gsl_vector_set_basis
  (foreign-safe-lambda void "gsl_vector_set_basis" gsl_vector unsigned-int))

;; Vector views
(define-syntax define-gsl-subview-binding
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((type (conc "gsl_" (second form))))
       `(define ,(string->symbol (conc "gsl_" (second form) "_" (third form)))
          (foreign-safe-lambda*
              ,(string->symbol (conc "gsl_" (fourth form)))
              ,(fifth form)
            ,(conc type " *p0 = malloc(sizeof(" type "));")
            ,(conc "gsl_" (fourth form) "_view p1 = gsl_" (second form) "_" (third form) "("
                   (string-join (map (lambda (a) (symbol->string (second a))) (fifth form)) ",")
                   ");")
            ;; ,(conc "memcpy(p0, &p1." (fourth form) ", sizeof(" type "));")
            ,(conc "memcpy(p0, &p1." (fourth form) ", sizeof(" (conc "gsl_" (fourth form)) "));")
            "C_return(p0);"))))))

(define-gsl-subview-binding vector subvector vector
  ((gsl_vector v)
   (unsigned-int offset)
   (unsigned-int n)))

(define-gsl-subview-binding vector subvector_with_stride vector
  ((gsl_vector v)
   (unsigned-int offset)
   (unsigned-int stride)
   (unsigned-int n)))

;; TODO Complex and array operations skipped here

;; Copying vectors
(define gsl_vector_memcpy
  (foreign-safe-lambda int "gsl_vector_memcpy" gsl_vector gsl_vector))
(define gsl_vector_swap
  (foreign-safe-lambda int "gsl_vector_swap" gsl_vector gsl_vector))

;; Exchanging elements
(define gsl_vector_swap_elements
  (foreign-safe-lambda int "gsl_vector_swap_elements"
    gsl_vector unsigned-int unsigned-int))
(define gsl_vector_reverse
  (foreign-safe-lambda int "gsl_vector_reverse" gsl_vector))

;; Vector operations
(define gsl_vector_add
  (foreign-safe-lambda int "gsl_vector_add" gsl_vector gsl_vector))

(define gsl_vector_sub
  (foreign-safe-lambda int "gsl_vector_sub" gsl_vector gsl_vector))

(define gsl_vector_mul
  (foreign-safe-lambda int "gsl_vector_mul" gsl_vector gsl_vector))

(define gsl_vector_div
  (foreign-safe-lambda int "gsl_vector_div" gsl_vector gsl_vector))

(define gsl_vector_scale
  (foreign-safe-lambda int "gsl_vector_scale" gsl_vector double))

(define gsl_vector_add_constant
  (foreign-safe-lambda int "gsl_vector_add_constant" gsl_vector double))

;; Finding max and min elements of vectors
(define gsl_vector_max (foreign-safe-lambda double "gsl_vector_max" gsl_vector))
(define gsl_vector_min (foreign-safe-lambda double "gsl_vector_min" gsl_vector))
(define gsl_vector_minmax
  (foreign-primitive ((gsl_vector v))
      "double max_out, min_out;"
    "gsl_vector_minmax(v, &min_out, &max_out);"
    "C_word *ptr = C_alloc(C_SIZEOF_FLONUM);"
    "C_word av[4] = {C_SCHEME_UNDEFINED, C_k, C_flonum(&ptr,min_out), C_flonum(&ptr,max_out)};"
    "C_values(4, av);"))
(define gsl_vector_max_index (foreign-safe-lambda int "gsl_vector_max_index" gsl_vector))
(define gsl_vector_min_index (foreign-safe-lambda int "gsl_vector_min_index" gsl_vector))
(define gsl_vector_minmax_index
  (foreign-primitive ((gsl_vector v))
      "size_t imin, imax;"
    "gsl_vector_minmax_index(v,&imin,&imax);"
    "C_word av[4] = {C_SCHEME_UNDEFINED, C_k, C_fix(imin), C_fix(imax)};"
    "C_values(4,av);"))

;; Vector properties
(define gsl_vector_isnull (foreign-safe-lambda bool "gsl_vector_isnull" gsl_vector))
(define gsl_vector_ispos (foreign-safe-lambda bool "gsl_vector_ispos" gsl_vector))
(define gsl_vector_isneg (foreign-safe-lambda bool "gsl_vector_isneg" gsl_vector))
(define gsl_vector_isnonneg (foreign-safe-lambda bool "gsl_vector_isnonneg" gsl_vector))
(define gsl_vector_equal (foreign-safe-lambda bool "gsl_vector_equal" gsl_vector gsl_vector))
