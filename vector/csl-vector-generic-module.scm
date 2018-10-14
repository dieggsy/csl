(functor (generic-vector (M (valloc
                             vset!
                             vlength
                             vref
                             vfill!
                             vcopy!
                             vswap!
                             vreverse!
                             vnull?
                             vpositive?
                             vnegative?
                             vnonnegative?
                             vequal?
                             vreal
                             vimag
                             v+
                             v+c
                             v-
                             v*
                             v/
                             vmax
                             vmin
                             vargmax
                             vargmin
                             vbasis!
                             vsubvector)))
    (vector?
     list->vector
     vector->list
     vector
     make-vector
     vector-length
     vector-map
     vector-map!
     vector-ref
     vector-set
     vector-set!
     vector-fill
     vector-fill!
     vector-copy
     vector-copy!
     vector-swap
     vector-swap!
     vector-reverse
     vector-reverse!
     vector-append
     vector-real-part
     vector-imag-part
     vector+
     vector+!
     vector-
     vector-!
     vector*
     vector*!
     vector/
     vector/!
     vector-scale
     vector-scale!
     vector-add-constant
     vector-add-constant!
     vector-zero?
     vector-positive?
     vector-negative?
     vector-nonnegative?
     vector=
     vector-max
     vector-min
     vector-argmax
     vector-argmin
     vector-basis!
     subvector)
    (import (except scheme
                    vector
                    vector?
                    list->vector
                    vector->list
                    make-vector
                    vector-length
                    vector-ref
                    vector-fill!
                    vector-set!)
            (except (chicken base) subvector vector-copy!)
            M)

  (define-record-type vector
    (ptr->vector ptr)
    vector?
    (ptr vector->ptr))

  (define (list->vector lst #!optional complex)
    (let* ((len (length lst))
           (v (valloc (length lst))))
      (do ((i 0 (+ i 1))
           (lst lst (cdr lst)))
          ((= i len) (ptr->vector v))
        (vset! v i (car lst)))))

  (define (vector->list v)
    (let* ((ptr (vector->ptr v))
           (len (vlength ptr)))
      (do ((i (- len 1) (- i 1))
           (res '() (cons (vref ptr i) res)))
          ((= i -1) res))))

  (define (vector . args)
    (list->vector args))

  (define (make-vector n #!optional fill)
    (let ((v (valloc n)))
      (when fill
        (vfill! v fill))
      (ptr->vector v)))

  (define (vector-length v)
    (vlength (vector->ptr v)))

  (define (vector-map f . v)
    (let* ((len (apply min (map vector-length v)))
           (d (map vector->ptr v))
           (r (valloc len)))
      (do ((i 0 (+ i 1)))
          ((= i len) (ptr->vector r))
        (vset! r i (apply f (map (cut vref <> i) d))))))

  (define (vector-map! f . v)
    (let* ((len (apply min (map vector-length v)))
           (d (map vector->ptr v)))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (vset! (car d) i (apply f (map (cut vref <> i) d))))))

  (define (vector-ref v i)
    (vref (vector->ptr v) i))

  (define (subvector v a b #!optional (stride 1))
    (ptr->vector (vsubvector (vector->ptr v)
                             a
                             stride
                             (inexact->exact
                              (ceiling (/ (- b a) stride))))))

  (define (vector-set v i n)
    (let* ((ptr (vector->ptr v))
           (new (valloc (vlength ptr))))
      (vcopy! new ptr)
      (vset! new i n)
      (ptr->vector new)))

  (define (vector-set! v i n)
    (vset! (vector->ptr v) i n))

  (define (vector-fill v n)
    (let* ((ptr (vector->ptr v))
           (new (valloc (vlength ptr))))
      (vcopy! new ptr)
      (vfill! new n)
      (ptr->vector new)))

  (define (vector-fill! v n)
    (vfill! (vector->ptr v) n))

  (define (vector-copy v)
    (let ((new (valloc (vector-length v))))
      (vcopy! new (vector->ptr v))
      (ptr->vector new)))

  (define (vector-copy! v1 v2)
    (vcopy! (vector->ptr v1) (vector->ptr v2)))

  (define (vector-swap v n1 n2)
    (let ((new ((valloc (vector-length v)))))
      (vswap! new n1 n2)
      (ptr->vector new)))

  (define (vector-swap! v n1 n2)
    (vswap! (vector->ptr v) n1 n2))

  (define (vector-reverse v)
    (let ((new (valloc (vector-length v))))
      (vcopy! new (vector->ptr v))
      (vreverse! new)
      (ptr->vector new)))

  (define (vector-reverse! v)
    (vreverse! (vector->ptr v)))

  (define (vector-append v1 v2)
    (let* ((ptr1 (vector->ptr v1))
           (ptr2 (vector->ptr v2))
           (len1 (vlength ptr1))
           (len2 (vlength ptr2))
           (newlen (+ len1 len2))
           (new (valloc newlen)))
      (do ((i 0 (+ i 1)))
          ((= i len1))
        (vset! new i (vref ptr1 i)))
      (do ((i len1 (+ i 1)))
          ((= i newlen))
        (vset! new i (vref ptr2 (- i len1))))
      (ptr->vector new)))

  (define (vector-real-part v)
    (ptr->vector (vreal (vector->ptr v))))

  (define (vector-imag-part v)
    (ptr->vector (vimag (vector->ptr v))))

  (define (vector+ v1 v2)
    (let ((c (vector-copy v1)))
      (v+ (vector->ptr c) (vector->ptr v2))
      c))

  (define (vector+! v1 v2)
    (v+ (vector->ptr v1) (vector->ptr v2))
    (void))

  (define (vector- v1 v2)
    (let ((c (vector-copy v1)))
      (v- (vector->ptr c) (vector->ptr v2))
      c))

  (define (vector-! v1 v2)
    (v- (vector->ptr v1) (vector->ptr v2))
    (void))

  (define (vector* v1 v2)
    (let ((c (vector-copy v1)))
      (v* (vector->ptr c) (vector->ptr v2))
      c))

  (define (vector*! v1 v2)
    (v* (vector->ptr v1) (vector->ptr v2))
    (void))

  (define (vector/ v1 v2)
    (let ((c (vector-copy v1)))
      (v/ (vector->ptr c) (vector->ptr v2))
      c))

  (define (vector/! v1 v2)
    (v/ (vector->ptr v1) (vector->ptr v2))
    (void))


  (define (vector-scale v n)
    (let* ((ptr (vector->ptr v))
           (new (valloc (vlength ptr))))
      (vcopy! new ptr)
      (v*c new n)
      (ptr->vector new)))

  (define (vector-scale! v n)
    (v*c (vector->ptr v) n)
    (void))

  (define (vector-add-constant v n)
    (let* ((ptr (vector->ptr v))
           (new (valloc (vlength ptr))))
      (vcopy! new ptr)
      (v+c new n)
      (ptr->vector new)))

  (define (vector-add-constant! v n)
    (v+c (vector->ptr v) n)
    (void))

  (define (vector-max v)
    (vmax (vector->ptr v)))

  (define (vector-min v)
    (vmin (vector->ptr v)))

  (define (vector-argmax v)
    (vargmax (vector->ptr v)))

  (define (vector-argmin v)
    (vargmin (vector->ptr v)))

  (define (vector-zero? v)
    (vnull? (vector->ptr v)))

  (define (vector-positive? v)
    (vpositive? (vector->ptr v)))

  (define (vector-negative? v)
    (vnegative? (vector->ptr v)))

  (define (vector-nonnegative? v)
    (vnonnegative? (vector->ptr v)))

  (define (vector= v1 v2)
    (vequal? (vector->ptr v1) (vector->ptr v2)))

  (define (vector-basis! v n)
    (vbasis! (vector->ptr v) n)
    (void)))

(import-for-syntax (chicken format))

(define-syntax make-rvector-module
  (ir-macro-transformer
   (lambda (e i c)
     (let ((file-prefix (caddr e))
           (base-type (cadddr e)))
       `(module ,(cadr e) = generic-vector
                (import scheme
                        (chicken base)
                        (chicken gc)
                        (chicken foreign)
                        foreigners)

                (include "../csl-error.scm")


                (foreign-declare ,(format "#include <gsl/gsl_block~a.h>"
                                          (if (eq? base-type (i 'double))
                                              ""
                                              (string-append "_" (symbol->string (strip-syntax base-type))))))

                (define-foreign-record-type (gsl_block ,(format "gsl_block~a"
                                                                (if (eq? base-type (i 'double))
                                                                    ""
                                                                    (string-append "_" (symbol->string (strip-syntax base-type))))))
                  (unsigned-int size gsl_block.size)
                  ((c-pointer ,base-type) data gsl_block.data))
                ;; (include "gsl-block.scm")

                (foreign-declare ,(format "#include <gsl/~a.h>"
                                          (if (equal? file-prefix "gsl_vector")
                                              "gsl_vector_double"
                                              file-prefix)))

                (define-foreign-record-type (gsl_vector ,file-prefix)
                  (unsigned-int size vlength)
                  (unsigned-int stride gsl_vector.stride)
                  ((c-pointer ,base-type) data gsl_vector.data)
                  (gsl_block block gsl_vector.block)
                  (int owner gsl_vector.owner))

                ;; Vector allocation
                (define gsl_vector_free
                  (foreign-safe-lambda void ,(format "~a_free" file-prefix) gsl_vector))

                (define gsl_vector_alloc
                  (foreign-safe-lambda gsl_vector ,(format "~a_alloc" file-prefix) unsigned-int))
                (define (valloc n)
                  (set-finalizer! (gsl_vector_alloc n) gsl_vector_free))

                ;; Accessing vector elements
                (define vref
                  (foreign-safe-lambda ,base-type ,(format "~a_get" file-prefix)
                    (const gsl_vector)
                    (const unsigned-int)))

                (define vset!
                  (foreign-safe-lambda void ,(format "~a_set" file-prefix)
                    gsl_vector
                    (const unsigned-int)
                    (const ,base-type)))

                ;; Initializing vector elements
                (define vfill!
                  (foreign-safe-lambda void ,(format "~a_set_all" file-prefix) gsl_vector ,base-type))

                ;; (define gsl_vector_set_zero
                ;;   (foreign-safe-lambda void ,(format "~a_set_zero" file-prefix) gsl_vector))

                (define vbasis!
                  (foreign-safe-lambda void ,(format "~a_set_basis" file-prefix) gsl_vector unsigned-int))

                ;; Vector views
                (define vsubvector
                  (foreign-safe-lambda* gsl_vector ((gsl_vector v)
                                                    (unsigned-int offset)
                                                    (unsigned-int stride)
                                                    (unsigned-int n))
                    ,(format "~a *p0 = malloc(sizeof(~a));" file-prefix file-prefix)
                    ,(format "~a_view p1 = ~a_subvector_with_stride(v,offset,stride,n);"
                             file-prefix file-prefix)
                    ,(format "memcpy(p0, &p1.vector, sizeof(~a));" file-prefix)
                    "C_return(p0);"))

                ;; Copying vectors
                (define vcopy!
                  (foreign-safe-lambda int ,(format "~a_memcpy" file-prefix) gsl_vector gsl_vector))

                ;; Exchanging elements
                (define vswap!
                  (foreign-safe-lambda int ,(format "~a_swap_elements" file-prefix)
                    gsl_vector unsigned-int unsigned-int))
                (define vreverse!
                  (foreign-safe-lambda int ,(format "~a_reverse" file-prefix) gsl_vector))

                ;; Vector operations
                (define v+
                  (foreign-safe-lambda int ,(format "~a_add" file-prefix) gsl_vector gsl_vector))

                (define v-
                  (foreign-safe-lambda int ,(format "~a_sub" file-prefix) gsl_vector gsl_vector))

                (define v*
                  (foreign-safe-lambda int ,(format "~a_mul" file-prefix) gsl_vector gsl_vector))

                (define v/
                  (foreign-safe-lambda int ,(format "~a_div" file-prefix) gsl_vector gsl_vector))

                (define v*c
                  (foreign-safe-lambda int ,(format "~a_scale" file-prefix) gsl_vector ,base-type))

                (define v+c
                  (foreign-safe-lambda int ,(format "~a_add_constant" file-prefix) gsl_vector ,base-type))

                ;; Finding max and min elements of vectors
                (define vmax (foreign-safe-lambda ,base-type ,(format "~a_max" file-prefix) gsl_vector))
                (define vmin (foreign-safe-lambda ,base-type ,(format "~a_min" file-prefix) gsl_vector))
                (define vargmax (foreign-safe-lambda int ,(format "~a_max_index" file-prefix) gsl_vector))
                (define vargmin (foreign-safe-lambda int ,(format "~a_min_index" file-prefix) gsl_vector))

                ;; Vector properties
                (define vnull? (foreign-safe-lambda bool ,(format "~a_isnull" file-prefix) gsl_vector))
                (define vpositive? (foreign-safe-lambda bool ,(format "~a_ispos" file-prefix) gsl_vector))
                (define vnegative? (foreign-safe-lambda bool ,(format "~a_isneg" file-prefix) gsl_vector))
                (define vnonnegative? (foreign-safe-lambda bool ,(format "~a_isnonneg" file-prefix) gsl_vector))
                (define vequal? (foreign-safe-lambda bool ,(format "~a_equal" file-prefix) gsl_vector gsl_vector))
                (define (vimag v)
                  (let ((v (valloc (vlength v))))
                    (vfill! v 0)
                    v))
                (define (vreal v) v))))))

(define-syntax make-zvector-module
  (ir-macro-transformer
   (lambda (e i c)
     (let* ((file-prefix (caddr e))
            (base-type (cadddr e))
            (type-suffix (if (eq? (strip-syntax base-type) 'double)
                             ""
                             (string-append "_" (symbol->string (strip-syntax base-type))))))
       `(module ,(cadr e) = generic-vector
                (import scheme
                        (chicken base)
                        (chicken gc)
                        (chicken foreign)
                        foreigners)

                (include "../csl-error.scm")
                (include "../complex-foreign-lambda.scm")

                (define-external (scheme_make_rect (,base-type r) (,base-type i)) scheme-object
                  (%make-rectangular r i))

                (foreign-declare ,(format "#include <gsl/gsl_block_complex_~a.h>"
                                          (symbol->string (strip-syntax base-type))))

                (define-foreign-record-type (gsl_block ,(format "gsl_block_complex~a" type-suffix))
                  (unsigned-int size gsl_block.size)
                  ((c-pointer ,base-type) data gsl_block.data))
                ;; (include "gsl-block.scm")

                (foreign-declare ,(format "#include <gsl/gsl_vector_complex_~a.h>"
                                          (symbol->string (strip-syntax base-type))))
                (foreign-declare "#include <gsl/gsl_complex.h>")
                ;; (foreign-declare "#include <gsl/gsl_complex_math.h>")

                (define-foreign-record-type (gsl_vector ,file-prefix)
                  (unsigned-int size vlength)
                  (unsigned-int stride gsl_vector.stride)
                  ((c-pointer ,base-type) data gsl_vector.data)
                  (gsl_block block gsl_vector.block)
                  (int owner gsl_vector.owner))

                ;; Vector allocation
                (define gsl_vector_free
                  (foreign-safe-lambda void ,(format "~a_free" file-prefix) gsl_vector))

                (define gsl_vector_alloc
                  (foreign-safe-lambda gsl_vector ,(format "~a_alloc" file-prefix) unsigned-int))
                (define (valloc n)
                  (set-finalizer! (gsl_vector_alloc n) gsl_vector_free))

                ;; Accessing vector elements
                (define vref
                  (complex-foreign-lambda (complex ,base-type) ,(format "~a_get" file-prefix)
                                          (const gsl_vector)
                                          (const unsigned-int)))

                (define vset!
                  (complex-foreign-lambda void ,(format "~a_set" file-prefix)
                                          gsl_vector
                                          (const unsigned-int)
                                          (complex ,base-type)))

                ;; Initializing vector elements
                (define vfill!
                  (complex-foreign-lambda void ,(format "~a_set_all" file-prefix) gsl_vector (complex ,base-type)))

                ;; (define gsl_vector_set_zero
                ;;   (complex-foreign-lambda void ,(format "~a_set_zero" file-prefix) gsl_vector))

                (define vbasis!
                  (foreign-safe-lambda void ,(format "~a_set_basis" file-prefix) gsl_vector unsigned-int))

                ;; Vector views
                (define vsubvector
                  (foreign-safe-lambda* gsl_vector ((gsl_vector v)
                                                    (unsigned-int offset)
                                                    (unsigned-int stride)
                                                    (unsigned-int n))
                    ,(format "~a *p0 = ~a_alloc(v->size);" file-prefix file-prefix)
                    ,(format "~a_view p1 = ~a_subvector_with_stride(v,offset,stride,n);" file-prefix file-prefix)
                    ,(format "memcpy(p0, &p1.vector, sizeof(~a));" file-prefix)
                    "C_return(p0);"))

                ;; Copying vectors
                (define vcopy!
                  (foreign-safe-lambda int ,(format "~a_memcpy" file-prefix) gsl_vector gsl_vector))

                ;; Exchanging elements
                (define vswap!
                  (foreign-safe-lambda int ,(format "~a_swap_elements" file-prefix)
                    gsl_vector unsigned-int unsigned-int))
                (define vreverse!
                  (foreign-safe-lambda int ,(format "~a_reverse" file-prefix) gsl_vector))

                ;; Vector operations
                (define v+
                  (foreign-safe-lambda int ,(format "~a_add" file-prefix) gsl_vector gsl_vector))

                (define v-
                  (foreign-safe-lambda int ,(format "~a_sub" file-prefix) gsl_vector gsl_vector))

                (define v*
                  (foreign-safe-lambda int ,(format "~a_mul" file-prefix) gsl_vector gsl_vector))

                (define v/
                  (foreign-safe-lambda int ,(format "~a_div" file-prefix) gsl_vector gsl_vector))

                (define v*c
                  (complex-foreign-lambda
                   int ,(format "~a_scale" file-prefix)
                   gsl_vector (complex ,base-type)))

                (define v+c
                  (complex-foreign-lambda
                   int ,(format "~a_add_constant" file-prefix)
                   gsl_vector (complex ,base-type)))

                ;; Finding max and min elements of vectors
                (define (vmax v) (error "Complex numbers have no ordering."))
                (define (vmin v) (error "Complex numbers have no ordering."))
                (define (vargmax v) (error "Complex numbers have no ordering."))
                (define (vargmin v) (error "Complex numbers have no ordering."))

                ;; Vector properties
                (define vnull? (foreign-safe-lambda bool ,(format "~a_isnull" file-prefix) gsl_vector))
                (define vpositive? (foreign-safe-lambda bool ,(format "~a_ispos" file-prefix) gsl_vector))
                (define vnegative? (foreign-safe-lambda bool ,(format "~a_isneg" file-prefix) gsl_vector))
                (define vnonnegative? (foreign-safe-lambda bool ,(format "~a_isnonneg" file-prefix) gsl_vector))
                (define vequal? (foreign-safe-lambda bool ,(format "~a_equal" file-prefix) gsl_vector gsl_vector))
                (define vimag
                  (foreign-safe-lambda* gsl_vector ((gsl_vector v))
                    ,(format "~a *p0 = ~a_alloc(v->size);" file-prefix file-prefix)
                    ,(format "gsl_vector~a_view p1 = ~a_imag(v);" type-suffix file-prefix)
                    "for (int i = 0; i < v->size; i++) { "
                    ,(format "~a iz = gsl_vector~a_get(&p1.vector, i);" (strip-syntax base-type) type-suffix)
                    ,(format "gsl_complex~a z;" type-suffix)
                    "GSL_SET_COMPLEX(&z, 0, iz);"
                    ,(format "~a_set(p0,i,z);" file-prefix)
                    "}"
                    "C_return(p0);"))
                (define vreal
                  (foreign-safe-lambda* gsl_vector ((gsl_vector v))
                    ,(format "~a *p0 = ~a_alloc(v->size);" file-prefix file-prefix)
                    ,(format "gsl_vector~a_view p1 = ~a_real(v);" type-suffix file-prefix)
                    "for (int i = 0; i < v->size; ++i) { "
                    ,(format "~a rz = gsl_vector~a_get(&p1.vector, i);" (strip-syntax base-type) type-suffix)
                    ,(format "gsl_complex~a z;" type-suffix)
                    "GSL_SET_COMPLEX(&z, rz, 0);"
                    ,(format "~a_set(p0,i,z);" file-prefix)
                    "}"
                    "C_return(p0);")))))))

