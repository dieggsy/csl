(foreign-declare "#include <gsl/gsl_vector.h>")
(foreign-declare "#include <gsl/gsl_complex_math.h>")

(define-foreign-record-type (gsl_vector_complex "gsl_vector_complex")
  (unsigned-int size gsl_vector_complex.size)
  (unsigned-int stride gsl_vector_complex.stride)
  ((c-pointer double) data gsl_vector_complex.data)
  (gsl_block_complex block gsl_vector_complex.block)
  (int owner gsl_vector_complex.owner))

(define-external (numbers_make_rect (double r) (double i)) scheme-object
  (make-rectangular r i))

;; Vector allocation
(define gsl_vector_complex_free
  (foreign-safe-lambda void "gsl_vector_complex_free" gsl_vector_complex))

(define gsl_vector_complex_alloc
  (foreign-safe-lambda gsl_vector_complex "gsl_vector_complex_alloc" unsigned-int))
(define (gsl_vector_complex_alloc_gc n)
  (set-finalizer! (gsl_vector_complex_alloc n) gsl_vector_complex_free))

(define gsl_vector_complex_calloc
  (foreign-safe-lambda gsl_vector_complex "gsl_vector_complex_calloc" unsigned-int))
(define (gsl_vector_complex_calloc_gc n)
  (set-finalizer! (gsl_vector_complex_calloc n) gsl_vector_complex_free))

;; Accessing vector elements
(define gsl_vector_complex_get
  (foreign-safe-lambda* scheme-object (((const gsl_vector_complex) v)
                                       ((const unsigned-int) i))
    "gsl_complex zout = gsl_vector_complex_get(v,i);"
    "C_return(numbers_make_rect(GSL_REAL(zout),GSL_IMAG(zout)));"))


(define (gsl_vector_complex_set v i z)
  (let ((rz (real-part z))
        (iz (imag-part z)))
    ((foreign-safe-lambda* void ((gsl_vector_complex v)
                                 ((const unsigned-int) i)
                                 (double rz) (double iz))
       "gsl_complex zset = gsl_complex_rect(rz,iz);"
       "gsl_vector_complex_set(v,i,zset);")
     v i rz iz)))

(define gsl_vector_complex_ptr
  (foreign-safe-lambda (c-pointer double) "gsl_vector_complex_ptr"
    gsl_vector_complex
    unsigned-int))

(define gsl_vector_complex_const_ptr
  (foreign-safe-lambda (c-pointer double) "gsl_vector_complex_const_ptr"
    gsl_vector_complex
    unsigned-int))

;; Initializing vector elements
(define (gsl_vector_complex_set_all v z)
  (let ((rz (real-part z))
        (iz (imag-part z)))
    ((foreign-safe-lambda* void ((gsl_vector_complex v)
                                 (double rz) (double iz))
       "gsl_complex zset = gsl_complex_rect(rz,iz);"
       "gsl_vector_complex_set_all(v,zset);")
     v rz iz)))

(define gsl_vector_complex_set_zero
  (foreign-safe-lambda void "gsl_vector_complex_set_zero" gsl_vector_complex))

(define gsl_vector_complex_set_basis
  (foreign-safe-lambda void "gsl_vector_complex_set_basis" gsl_vector_complex unsigned-int))

;; Vector views
(define gsl_vector_complex_subvector
  (foreign-safe-lambda* gsl_vector_complex ((gsl_vector_complex v)
                                            (unsigned-int offset)
                                            (unsigned-int n))
    "gsl_vector_complex *p0 = gsl_vector_complex_alloc(v->size);"
    "gsl_vector_complex_view p1 = gsl_vector_complex_subvector(v,offset,n);"
    "memcpy(p0, &p1.vector, sizeof(gsl_vector_complex));"
    "C_return(p0);"))

(define gsl_vector_complex_subvector_with_stride
  (foreign-safe-lambda* gsl_vector_complex ((gsl_vector_complex v)
                                            (unsigned-int offset)
                                            (unsigned-int stride)
                                            (unsigned-int n))
    "gsl_vector_complex *p0 = gsl_vector_complex_alloc(v->size);"
    "gsl_vector_complex_view p1 = gsl_vector_complex_subvector_with_stride(v,offset,stride,n);"
    "memcpy(p0, &p1.vector, sizeof(gsl_vector_complex));"
    "C_return(p0);"))

(define gsl_vector_complex_real
  (foreign-safe-lambda* gsl_vector_complex ((gsl_vector_complex v))
    "gsl_vector_complex *p0 = gsl_vector_complex_alloc(v->size);"
    "gsl_vector_view p1 = gsl_vector_complex_real(v);"
    "int i;"
    "for (i = 0; i < v->size; i++) { "
    "double rz = gsl_vector_get(&p1.vector, i);"
    "gsl_vector_complex_set(p0,i,gsl_complex_rect(rz, 0));"
    "}"
    "C_return(p0);"))

(define gsl_vector_complex_imag
  (foreign-safe-lambda* gsl_vector_complex ((gsl_vector_complex v))
    "gsl_vector_complex *p0 = gsl_vector_complex_alloc(v->size);"
    "gsl_vector_view p1 = gsl_vector_complex_imag(v);"
    "int i;"
    "for (i = 0; i < v->size; i++) { "
    "double iz = gsl_vector_get(&p1.vector, i);"
    "gsl_vector_complex_set(p0,i,gsl_complex_rect(0, iz));"
    "}"
    "C_return(p0);"))

;; Copying vectors
(define gsl_vector_complex_memcpy
  (foreign-safe-lambda int "gsl_vector_complex_memcpy" gsl_vector_complex gsl_vector_complex))
(define gsl_vector_complex_swap
  (foreign-safe-lambda int "gsl_vector_complex_swap" gsl_vector_complex gsl_vector_complex))

;; Exchanging elements
(define gsl_vector_complex_swap_elements
  (foreign-safe-lambda int "gsl_vector_complex_swap_elements"
    gsl_vector_complex unsigned-int unsigned-int))
(define gsl_vector_complex_reverse
  (foreign-safe-lambda int "gsl_vector_complex_reverse" gsl_vector_complex))

;; Vector operations
(define gsl_vector_complex_add
  (foreign-safe-lambda int "gsl_vector_complex_add" gsl_vector_complex gsl_vector_complex))

(define gsl_vector_complex_sub
  (foreign-safe-lambda int "gsl_vector_complex_sub" gsl_vector_complex gsl_vector_complex))

(define gsl_vector_complex_mul
  (foreign-safe-lambda int "gsl_vector_complex_mul" gsl_vector_complex gsl_vector_complex))

(define gsl_vector_complex_div
  (foreign-safe-lambda int "gsl_vector_complex_div" gsl_vector_complex gsl_vector_complex))

(define (gsl_vector_complex_scale v z)
  (let ((rz (real-part z))
        (iz (imag-part z)))
    ((foreign-safe-lambda* int ((gsl_vector_complex v)
                                (double rz) (double iz))
       "gsl_complex zin = gsl_complex_rect(rz,iz);"
       "gsl_vector_complex_scale(v,zin);")
     v rz iz)))

(define (gsl_vector_complex_add_constant v z)
  (let ((rz (real-part z))
        (iz (imag-part z)))
    ((foreign-safe-lambda* int ((gsl_vector_complex v)
                                (double rz) (double iz))
       "gsl_complex zin = gsl_complex_rect(rz,iz);"
       "gsl_vector_complex_add_constant(v,zin);")
     v rz iz)))

;; Finding max and min elements of vectors
;; (define gsl_vector_max (foreign-safe-lambda double "gsl_vector_max" gsl_vector))
;; (define gsl_vector_min (foreign-safe-lambda double "gsl_vector_min" gsl_vector))
;; (define gsl_vector_minmax
;;   (foreign-primitive ((gsl_vector v))
;;       "double max_out, min_out;"
;;     "gsl_vector_minmax(v, &min_out, &max_out);"
;;     "C_word *ptr = C_alloc(C_SIZEOF_FLONUM);"
;;     "C_word av[4] = {C_SCHEME_UNDEFINED, C_k, C_flonum(&ptr,min_out), C_flonum(&ptr,max_out)};"
;;     "C_values(4, av);"))
;; (define gsl_vector_max_index (foreign-safe-lambda int "gsl_vector_max_index" gsl_vector))
;; (define gsl_vector_min_index (foreign-safe-lambda int "gsl_vector_min_index" gsl_vector))
;; (define gsl_vector_minmax_index
;;   (foreign-primitive ((gsl_vector v))
;;       "size_t imin, imax;"
;;     "gsl_vector_minmax_index(v,&imin,&imax);"
;;     "C_word av[4] = {C_SCHEME_UNDEFINED, C_k, C_fix(imin), C_fix(imax)};"
;;     "C_values(4,av);"))

;; Vector properties
(define gsl_vector_complex_isnull (foreign-safe-lambda bool "gsl_vector_complex_isnull" gsl_vector_complex))
(define gsl_vector_complex_ispos (foreign-safe-lambda bool "gsl_vector_complex_ispos" gsl_vector_complex))
(define gsl_vector_complex_isneg (foreign-safe-lambda bool "gsl_vector_complex_isneg" gsl_vector_complex))
(define gsl_vector_complex_isnonneg (foreign-safe-lambda bool "gsl_vector_complex_isnonneg" gsl_vector_complex))
(define gsl_vector_complex_equal (foreign-safe-lambda bool "gsl_vector_complex_equal" gsl_vector_complex gsl_vector_complex))
