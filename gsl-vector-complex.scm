(foreign-declare "#include <gsl/gsl_vector_complex.h>")
(foreign-declare "#include <gsl/gsl_complex_math.h>")

(define-foreign-record-type (gsl_vector_complex "gsl_vector_complex")
  (unsigned-int size gsl_vector_complex.size)
  (unsigned-int stride gsl_vector_complex.stride)
  ((c-pointer double) data gsl_vector_complex.data)
  (gsl_block block gsl_vector_complex.block)
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
    (foreign-safe-lambda* void ((gsl_vector_complex v)
                                (double rz) (double iz))
      "gsl_complex zin = gsl_complex_rect(rz,iz);"
      "gsl_vector_complex_set_all(v,zin);")))

(define gsl_vector_complex_set_zero
  (foreign-safe-lambda void "gsl_vector_complex_set_zero" gsl_vector_complex))

(define gsl_vector_complex_set_basis
  (foreign-safe-lambda void "gsl_vector_complex_set_basis" gsl_vector_complex unsigned-int))

;; Vector views
(define-syntax define-gsl-complex-subview-binding
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
            ,(conc "memcpy(p0, &p1.vector, sizeof(" (conc "gsl_" (fourth form)) "));")))))))

(define-gsl-complex-subview-binding vector_complex subvector vector_complex
  ((gsl_vector_complex v)
   (unsigned-int offset)
   (unsigned-int n)))

(define-gsl-complex-subview-binding vector_complex subvector_with_stride vector_complex
  ((gsl_vector_complex v)
   (unsigned-int offset)
   (unsigned-int stride)
   (unsigned-int n)))

(define-gsl-complex-subview-binding vector_complex real vector
  ((gsl_vector_complex v)))

(define-gsl-complex-subview-binding vector_complex imag vector
  ((gsl_vector_complex v)))

;; TODO Complex and array operations skipped here

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
    (foreign-safe-lambda* int ((gsl_vector_complex v)
                               (double rz) (double iz))
      "gsl_complex zin = gsl_complex_rect(rz,iz);"
      "gsl_vector_complex_scale(v,zin);")))

(define (gsl_vector_complex_add_constant v z)
  (let ((rz (real-part z))
        (iz (imag-part z)))
    (foreign-safe-lambda* int ((gsl_vector_complex v)
                               (double rz) (double iz))
      "gsl_complex zin = gsl_complex_rect(rz,iz);"
      "gsl_vector_complex_add_constant(v,zin);")))

;; Vector properties
(define gsl_vector_complex_isnull (foreign-safe-lambda bool "gsl_vector_complex_isnull" gsl_vector_complex))
(define gsl_vector_complex_ispos (foreign-safe-lambda bool "gsl_vector_complex_ispos" gsl_vector_complex))
(define gsl_vector_complex_isneg (foreign-safe-lambda bool "gsl_vector_complex_isneg" gsl_vector_complex))
(define gsl_vector_complex_isnonneg (foreign-safe-lambda bool "gsl_vector_complex_isnonneg" gsl_vector_complex))
(define gsl_vector_complex_equal (foreign-safe-lambda bool "gsl_vector_complex_equal" gsl_vector_complex gsl_vector_complex))
