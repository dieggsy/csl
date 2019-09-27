(export
 vector?
 vector->ptr
 ptr->vector
 vector-size
 vector-alloc
 vector-calloc
 vector-free!
 vector-get
 vector-set!
 vector-set-all!
 vector-set-zero!
 vector-set-basis!
 vector-fwrite
 vector-fread!
 vector-fprintf
 vector-fscanf!
 vector-subvector
 vector-subvector-set!
 vector-subvector-with-stride
 vector-subvector-with-stride-set!
 vector-imag
 vector-real
 vector-memcpy!
 vector-swap!
 vector-swap-elements!
 vector-reverse!
 vector-add!
 vector-sub!
 vector-mul!
 vector-div!
 vector-scale!
 vector-add-constant!
 vector-axpby!
 vector-max
 vector-min
 vector-minmax
 vector-max-index
 vector-min-index
 vector-minmax-index
 vector-isnull?
 vector-ispos?
 vector-isneg?
 vector-isnonneg?
 vector-equal?)

(include "utils/error-handler.scm")
(include "utils/stdio.scm")

(define-record-type vector
  (ptr->vector ptr)
  vector?
  (ptr vector->ptr))

(define-foreign-type gsl-vector
  (nonnull-c-pointer "SUBTYPE(gsl_vector, _TYPE)")
  vector->ptr
  ptr->vector)

(define vector-size
  (foreign-lambda* size_t ((gsl-vector v))
    "C_return(v->size);"))

(define vector-alloc
  (foreign-lambda gsl-vector "TYPED_FN(gsl_vector, _TYPE, alloc)" size_t))
(define vector-calloc
  (foreign-lambda gsl-vector "TYPED_FN(gsl_vector, _TYPE, calloc)" size_t))
(define vector-free!
  (foreign-lambda void "TYPED_FN(gsl_vector, _TYPE, free)" gsl-vector))

;;; Accessing vector elements
(define vector-get
  (cond-expand
    ((or complex complex-float)
     (foreign-safe-lambda* scheme-object
         ((gsl-vector v) ((const size_t) i))
       "SUBTYPE(gsl, _TYPE) z = TYPED_FN(gsl_vector, _TYPE, get)(v,i);"
       "C_return(scheme_make_rect(GSL_REAL(z),GSL_IMAG(z)));"))
    (else
     (foreign-safe-lambda TYPE "TYPED_FN(gsl_vector, _TYPE, get)"
       gsl-vector (const size_t)))))

(define vector-set!
  (cond-expand
    ((or complex complex-float)
     (foreign-safe-lambda* void ((gsl-vector v) ((const size_t) i)
                                 (TYPE z))
       "SUBTYPE(gsl, _TYPE) _z;"
       "GSL_SET_COMPLEX(&_z, z[0], z[1]);"
       "TYPED_FN(gsl_vector, _TYPE, set)(v, i, _z);"))
    (else
     (foreign-safe-lambda void "TYPED_FN(gsl_vector, _TYPE, set)"
       gsl-vector (const size_t) TYPE))))
;; gsl_vector_ptr omitted

;;; Initializing vector elements
(define vector-set-all!
  (cond-expand
    ((or complex complex-float)
     (foreign-safe-lambda* void ((gsl-vector v) (TYPE z))
       "SUBTYPE(gsl, _TYPE) _z;"
       "GSL_SET_COMPLEX(&_z, z[0], z[1]);"
       "TYPED_FN(gsl_vector, _TYPE, set_all)(v, _z);"))
    (else
     (foreign-safe-lambda void
         "TYPED_FN(gsl_vector, _TYPE, set_all)"
       gsl-vector TYPE))))
(define vector-set-zero!
  (foreign-lambda void "TYPED_FN(gsl_vector, _TYPE, set_zero)"
    gsl-vector))
(define vector-set-basis!
  (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, set_basis)"
    gsl-vector size_t))

;;; Reading and writing vectors
(define (vector-fwrite fileport vector)
  (let* ((FILE (get-c-file 'vector-fwrite fileport)))
    ((foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, fwrite)"
       (c-pointer "FILE") gsl-vector)
     FILE vector)))

(define (vector-fread! fileport vector)
  (let* ((FILE (get-c-file 'vector-fread fileport)))
    ((foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, fread)"
       (c-pointer "FILE") gsl-vector)
     FILE vector)))

(define (vector-fprintf fileport vector format)
  (let* ((FILE (get-c-file 'vector-fprintf fileport)))
    ((foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, fprintf)"
       (c-pointer "FILE") gsl-vector c-string)
     FILE vector format)))

(define (vector-fscanf! fileport vector)
  (let* ((FILE (get-c-file 'vector-fscanf fileport)))
    ((foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, fscanf)"
       (c-pointer "FILE") gsl-vector)
     FILE vector)))

;;; Vector views
(define vector-subvector
  (foreign-safe-lambda* gsl-vector ((gsl-vector v) (size_t offset) (size_t n))
    "TYPED_FN(gsl_vector, _TYPE, view) view;"
    "SUBTYPE(gsl_vector, _TYPE) *vec;"
    "view = TYPED_FN(gsl_vector, _TYPE, subvector)(v, offset, n);"
    "vec = TYPED_FN(gsl_vector, _TYPE, alloc)(view.vector.size);"
    "TYPED_FN(gsl_vector, _TYPE, memcpy)(vec, &view.vector);"
    "C_return(vec);"))

(define vector-subvector-set!
  (foreign-safe-lambda* void ((gsl-vector v) (size_t offset) (size_t n) (gsl-vector sub))
    "TYPED_FN(gsl_vector, _TYPE, view) view;"
    "view = TYPED_FN(gsl_vector, _TYPE, subvector)(v, offset, n);"
    "TYPED_FN(gsl_vector, _TYPE, memcpy)(&view.vector, sub);"))


(define vector-subvector-with-stride
  (foreign-safe-lambda* gsl-vector ((gsl-vector v) (size_t offset) (size_t stride) (size_t n))
    "TYPED_FN(gsl_vector, _TYPE, view) view;"
    "SUBTYPE(gsl_vector, _TYPE) *vec;"
    "view = TYPED_FN(gsl_vector, _TYPE, subvector_with_stride)(v, offset, stride, n);"
    "vec = TYPED_FN(gsl_vector, _TYPE, alloc)(view.vector.size);"
    "TYPED_FN(gsl_vector, _TYPE, memcpy)(vec, &view.vector);"
    "C_return(vec);"))

(define vector-subvector-with-stride-set!
  (foreign-safe-lambda* void ((gsl-vector v) (size_t offset) (size_t stride) (size_t n) (gsl-vector sub))
    "TYPED_FN(gsl_vector, _TYPE, view) view;"
    "view = TYPED_FN(gsl_vector, _TYPE, subvector_with_stride)(v, offset, stride, n);"
    "TYPED_FN(gsl_vector, _TYPE, memcpy)(&view.vector, sub);"))

(define vector-real
  (cond-expand
    ((or complex complex-float)
     (foreign-safe-lambda* gsl-vector ((gsl-vector v))
       "TYPED_FN(gsl_vector, _REAL_TYPE, view) view;"
       "SUBTYPE(gsl_vector, _TYPE) *vec;"
       "view = TYPED_FN(gsl_vector, _TYPE, real)(v);"
       "vec = TYPED_FN(gsl_vector, _TYPE, alloc)(view.vector.size);"
       "for (size_t i=0; i < view.vector.size; ++i) {
            SUBTYPE(gsl, _TYPE) z;
            GSL_SET_COMPLEX(&z, TYPED_FN(gsl_vector, _REAL_TYPE, get)(&view.vector, i), 0);
            TYPED_FN(gsl_vector, _TYPE, set)(vec, i, z);
        }"
       "C_return(vec);"))
    ;; all other types are definitely real.
    (else 'identity)))

(define vector-imag
  (cond-expand
    ((or complex complex-float)
     (foreign-safe-lambda* gsl-vector ((gsl-vector v))
       "TYPED_FN(gsl_vector, _REAL_TYPE, view) view;"
       "SUBTYPE(gsl_vector, _TYPE) *vec;"
       "view = TYPED_FN(gsl_vector, _TYPE, imag)(v);"
       "vec = TYPED_FN(gsl_vector, _TYPE, alloc)(view.vector.size);"
       "for (size_t i=0; i < view.vector.size; ++i) {
            SUBTYPE(gsl, _TYPE) z;
            GSL_SET_COMPLEX(&z, 0, TYPED_FN(gsl_vector, _REAL_TYPE, get)(&view.vector, i));
            TYPED_FN(gsl_vector, _TYPE, set)(vec, i, z);
        }"
       "C_return(vec);"))
    ;; all other types are definitely imag.
    (else 'identity)))

;; gsl_vector_view_array and view_array_with_stride omitted

;;; Copying vectors
(define vector-memcpy!
  (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, memcpy)"
    gsl-vector gsl-vector))
(define vector-swap!
  (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, swap)"
    gsl-vector gsl-vector))

;;; Exchanging elements
(define vector-swap-elements!
  (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, swap_elements)"
    gsl-vector size_t size_t))
(define vector-reverse!
  (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, reverse)"
    gsl-vector))

;;; Vector operations
(define vector-add!
  (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, add)"
    gsl-vector gsl-vector))

(define vector-sub!
  (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, sub)"
    gsl-vector gsl-vector))

(define vector-mul!
  (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, mul)"
    gsl-vector gsl-vector))

(define vector-div!
  (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, div)"
    gsl-vector gsl-vector))

(define vector-scale!
  (cond-expand
    ((or complex complex-float)
     (foreign-safe-lambda* gsl-errno ((gsl-vector v) (TYPE z))
       "SUBTYPE(gsl, _TYPE) _z;"
       "GSL_SET_COMPLEX(&_z, z[0], z[1]);"
       "C_return(TYPED_FN(gsl_vector, _TYPE, scale)(v, _z));"))
    (else
     (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, scale)"
       gsl-vector TYPE))))

(define vector-add-constant!
  (cond-expand
    ((or complex complex-float)
     (foreign-safe-lambda* gsl-errno ((gsl-vector v) (TYPE z))
       "SUBTYPE(gsl, _TYPE) _z;"
       "GSL_SET_COMPLEX(&_z, z[0], z[1]);"
       "C_return(TYPED_FN(gsl_vector, _TYPE, add_constant)(v, _z));"))
    (else
     (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, add_constant)"
       gsl-vector TYPE))))

(define vector-axpby!
  (cond-expand
    ((or complex complex-float)
     (foreign-safe-lambda* gsl-errno ((TYPE alpha)
                                      (gsl-vector x)
                                      (TYPE beta)
                                      (gsl-vector y))
       "SUBTYPE(gsl, _TYPE) _alpha, _beta;"
       "GSL_SET_COMPLEX(&_alpha, alpha[0], alpha[1]);"
       "GSL_SET_COMPLEX(&_beta, beta[0], beta[1]);"
       "C_return(TYPED_FN(gsl_vector, _TYPE, axpby)(_alpha, x, _beta, y));"))
    (else
     (foreign-safe-lambda gsl-errno "TYPED_FN(gsl_vector, _TYPE, axpby)"
       (const TYPE) gsl-vector (const TYPE) gsl-vector))))

;;; Maximum and minimum
(define vector-max
  (cond-expand
    ((or complex complex-float)
     (lambda (m)
       (error 'vector-max
              "bad argument type - complex numbers have no ordering")))
    (else
     (foreign-lambda TYPE "TYPED_FN(gsl_vector, _TYPE, max)"
       gsl-vector))))

(define vector-min
  (cond-expand
    ((or complex complex-float)
     (lambda (m)
       (error 'vector-min
              "bad argument type - complex numbers have no ordering")))
    (else
     (foreign-lambda TYPE "TYPED_FN(gsl_vector, _TYPE, min)"
       gsl-vector))))

(define vector-minmax
  (cond-expand
    ((or complex complex-float)
     (lambda (m)
       (error 'vector-minmax
              "bad argument type - complex numbers have no ordering")))
    (else
     (foreign-primitive ((gsl-vector v))
         "TYPE min_out, max_out;"
       "TYPED_FN(gsl_vector, _TYPE, minmax)(v, &min_out, &max_out);"
       "C_word *minptr = C_alloc(C_SIZEOF_TYPE);"
       "C_word *maxptr = C_alloc(C_SIZEOF_TYPE);"
       "C_word av[4] = {C_SCHEME_UNDEFINED, C_k,
                        C_MAKE_TYPE(&minptr, min_out),
                        C_MAKE_TYPE(&maxptr, max_out)};"
       "C_values(4,av);"))))

(define vector-max-index
  (cond-expand
    ((or complex complex-float)
     (lambda (m)
       (error 'vector-max-index
              "bad argument type - complex numbers have no ordering")))
    (else
     (foreign-lambda size_t "TYPED_FN(gsl_vector, _TYPE, max_index)"
       gsl-vector))))

(define vector-min-index
  (cond-expand
    ((or complex complex-float)
     (lambda (m)
       (error 'vector-min-index
              "bad argument type - complex numbers have no ordering")))
    (else
     (foreign-lambda size_t "TYPED_FN(gsl_vector, _TYPE, min_index)"
       gsl-vector))))

(define vector-minmax-index
  (cond-expand
    ((or complex complex-float)
     (lambda (m)
       (error 'vector-minmax-index
              "bad argument type - complex numbers have no ordering")))
    (else
     (foreign-primitive ((gsl-vector v))
         "size_t min_out, max_out;"
       "TYPED_FN(gsl_vector, _TYPE, minmax_index)(v, &min_out, &max_out);"
       "C_word *minptr = C_alloc(C_SIZEOF_FIX_BIGNUM);"
       "C_word *maxptr = C_alloc(C_SIZEOF_FIX_BIGNUM);"
       "C_word av[4] = {C_SCHEME_UNDEFINED, C_k,
                        C_int_to_num(&minptr, min_out),
                        C_int_to_num(&maxptr, max_out)};"
       "C_values(4,av);"))))

;;; Vector properties
(define vector-isnull?
  (foreign-lambda bool "TYPED_FN(gsl_vector, _TYPE, isnull)" gsl-vector))
(define vector-ispos?
  (foreign-lambda bool "TYPED_FN(gsl_vector, _TYPE, ispos)" gsl-vector))
(define vector-isneg?
  (foreign-lambda bool "TYPED_FN(gsl_vector, _TYPE, isneg)" gsl-vector))
(define vector-isnonneg?
  (foreign-lambda bool "TYPED_FN(gsl_vector, _TYPE, isnonneg)" gsl-vector))
(define vector-equal?
  (foreign-safe-lambda bool "TYPED_FN(gsl_vector, _TYPE, equal)" gsl-vector gsl-vector))
