(import-for-syntax (chicken format))

(define-syntax make-vector-module
  (er-macro-transformer
   (lambda (e i compare)
     (let* ((file-prefix (caddr e))
            (base-type* (cadddr e))
            (complex (pair? base-type*))
            (base-type (if complex
                           (cadr base-type*)
                           base-type*))
            (fsl (if complex
                     'complex-foreign-lambda
                     'foreign-safe-lambda))
            (rtype-suffix (if (or (and complex
                                       (compare (cadr base-type*) 'double))
                                  (compare base-type* 'double))
                              ""
                              (string-append "_"
                                             (symbol->string
                                              (if complex
                                                  (strip-syntax (cadr base-type*))
                                                  (strip-syntax base-type*))))))
            (type-suffix (if complex
                             (string-append "_complex" rtype-suffix)
                             rtype-suffix)))
       `(module ,(cadr e) *
          (import (except scheme
                          vector-set!)
                  (chicken base)
                  (chicken gc)
                  (chicken foreign)
                  foreigners)

          (include "../csl-error.scm")
          ,@(if complex
                '((include "../complex-foreign-lambda.scm"))
                '())

          ,@(if complex
                `((define-external (scheme_make_rect (,base-type r) (,base-type i)) scheme-object
                    (%make-rectangular r i)))
                '())

          (foreign-declare ,(format "#include <gsl/gsl_block~a~a.h>" type-suffix
                                    (if (and complex (compare base-type 'double))
                                        "_double"
                                        "")))

          (define-foreign-record-type (gsl_block ,(format "gsl_block~a" type-suffix))
            (unsigned-int size gsl_block.size)
            ((c-pointer ,base-type) data gsl_block.data))

          (foreign-declare ,(format "#include <gsl/gsl_vector~a.h>" type-suffix))

          ,@(if complex
                `((foreign-declare "#include <gsl/gsl_complex.h>")
                  (foreign-declare ,(format "#include <gsl/gsl_vector~a.h>" rtype-suffix)))
                '())

          (define-foreign-record-type (gsl_vector ,file-prefix)
            (unsigned-int size vector-size)
            (unsigned-int stride gsl_vector.stride)
            ((c-pointer ,base-type) data gsl_vector.data)
            (gsl_block block gsl_vector.block)
            (int owner gsl_vector.owner))

          ;; Vector allocation
          (define vector-alloc
            (foreign-safe-lambda gsl_vector ,(format "~a_alloc" file-prefix) unsigned-int))

          (define vector-calloc
            (foreign-safe-lambda gsl_vector ,(format "~a_calloc" file-prefix) unsigned-int))

          (define vector-free!
            (foreign-safe-lambda void ,(format "~a_free" file-prefix) gsl_vector))

          ;; added for convenience
          (define (vector-alloc-gc n)
            (set-finalizer! (vector-alloc n) vector-free!))

          ;; Accessing vector elements
          (define vector-get
            (,fsl ,base-type* ,(format "~a_get" file-prefix)
                  (const gsl_vector)
                  (const unsigned-int)))

          (define vector-set!
            (,fsl void ,(format "~a_set" file-prefix)
                  gsl_vector
                  (const unsigned-int)
                  ,base-type*))

          ;; vector-ptr omitted

          ;; Initializing vector elements
          (define vector-set-all!
            (,fsl void ,(format "~a_set_all" file-prefix) gsl_vector ,base-type*))

          (define vector-set-zero!
            (foreign-safe-lambda void ,(format "~a_set_zero" file-prefix) gsl_vector))

          (define vector-set-basis!
            (foreign-safe-lambda void ,(format "~a_set_basis" file-prefix) gsl_vector unsigned-int))

          ;; Reading and writing vectors
          ;; omitted

          ;; Vector views
          (define vector-subvector
            (foreign-safe-lambda* gsl_vector ((gsl_vector v)
                                              (unsigned-int offset)
                                              (unsigned-int n))
              ,(format "~a *p0 = ~a_alloc(v->size);" file-prefix file-prefix)
              ,(format "~a_view p1 = ~a_subvector(v,offset,n);" file-prefix file-prefix)
              ,(format "memcpy(p0, &p1.vector, sizeof(~a));" file-prefix)
              "C_return(p0);"))

          ;; vector-const-subvector omitted

          (define vector-subvector-with-stride
            (foreign-safe-lambda* gsl_vector ((gsl_vector v)
                                              (unsigned-int offset)
                                              (unsigned-int stride)
                                              (unsigned-int n))
              ,(format "~a *p0 = ~a_alloc(v->size);" file-prefix file-prefix)
              ,(format "~a_view p1 = ~a_subvector_with_stride(v,offset,stride,n);" file-prefix file-prefix)
              ,(format "memcpy(p0, &p1.vector, sizeof(~a));" file-prefix)
              "C_return(p0);"))

          ;; vector-const-subvector-with-stride omitted

          (define vector-imag
            ,(if complex
                 `(foreign-safe-lambda* gsl_vector ((gsl_vector v))
                    ,(format "~a *p0 = ~a_alloc(v->size);" file-prefix file-prefix)
                    ,(format "gsl_vector~a_view p1 = ~a_imag(v);" rtype-suffix file-prefix)
                    "for (int i = 0; i < v->size; i++) { "
                    ,(format "~a iz = gsl_vector~a_get(&p1.vector, i);" (strip-syntax base-type) rtype-suffix)
                    ,(format "gsl~a z;" type-suffix)
                    "GSL_SET_COMPLEX(&z, 0, iz);"
                    ,(format "~a_set(p0,i,z);" file-prefix)
                    "}"
                    "C_return(p0);")
                 '(lambda (v)
                    (let ((v (vector-alloc-gc (vector-size v))))
                      (vector-set-all! v 0)
                      v))))

          (define vector-real
            ,(if complex
                 `(foreign-safe-lambda* gsl_vector ((gsl_vector v))
                    ,(format "~a *p0 = ~a_alloc(v->size);" file-prefix file-prefix)
                    ,(format "gsl_vector~a_view p1 = ~a_real(v);" rtype-suffix file-prefix)
                    "for (int i = 0; i < v->size; ++i) { "
                    ,(format "~a rz = gsl_vector~a_get(&p1.vector, i);" (strip-syntax base-type) rtype-suffix)
                    ,(format "gsl~a z;" type-suffix)
                    "GSL_SET_COMPLEX(&z, rz, 0);"
                    ,(format "~a_set(p0,i,z);" file-prefix)
                    "}"
                    "C_return(p0);")
                 'identity))

          ;; vector-view-array omitted
          ;; vector-const-view-array omitted

          ;; vector-view-array-with-stride omitted
          ;; vector-const-view-array-with-stride omitted

          ;; Copying vectors
          (define vector-memcpy!
            (foreign-safe-lambda int ,(format "~a_memcpy" file-prefix) gsl_vector gsl_vector))

          (define vector-swap!
            (foreign-safe-lambda int ,(format "~a_swap" file-prefix) gsl_vector gsl_vector))

          ;; Exchanging elements
          (define vector-swap-elements!
            (foreign-safe-lambda int ,(format "~a_swap_elements" file-prefix)
              gsl_vector unsigned-int unsigned-int))

          (define vector-reverse!
            (foreign-safe-lambda int ,(format "~a_reverse" file-prefix) gsl_vector))

          ;; Vector operations
          (define vector-add!
            (foreign-safe-lambda int ,(format "~a_add" file-prefix) gsl_vector gsl_vector))

          (define vector-sub!
            (foreign-safe-lambda int ,(format "~a_sub" file-prefix) gsl_vector gsl_vector))

          (define vector-mul!
            (foreign-safe-lambda int ,(format "~a_mul" file-prefix) gsl_vector gsl_vector))

          (define vector-div!
            (foreign-safe-lambda int ,(format "~a_div" file-prefix) gsl_vector gsl_vector))

          (define vector-scale!
            (,fsl int ,(format "~a_scale" file-prefix) gsl_vector ,base-type*))

          (define vector-add-constant!
            (,fsl int ,(format "~a_add_constant" file-prefix) gsl_vector ,base-type*))

          ;; Finding max and min elements of vectors
          (define vector-max
            ,(if complex
                 '(lambda (v) (error "Complex numbers have no ordering."))
                 `(foreign-safe-lambda ,base-type ,(format "~a_max" file-prefix) gsl_vector)))

          (define vector-min
            ,(if complex
                 '(lambda (v) (error "Complex numbers have no ordering."))
                 `(foreign-safe-lambda ,base-type ,(format "~a_min" file-prefix) gsl_vector)))

          ;; vector-minmax omitted

          (define vector-max-index
            ,(if complex
                 '(lambda (v) (error "Complex numbers have no ordering."))
                 `(foreign-safe-lambda int ,(format "~a_max_index" file-prefix) gsl_vector)))

          (define vector-min-index
            ,(if complex
                 '(lambda (v) (error "Complex numbers have no ordering."))
                 `(foreign-safe-lambda int ,(format "~a_min_index" file-prefix) gsl_vector)))

          ;; vector-minmax-index omitted

          ;; Vector properties
          (define vector-isnull? (foreign-safe-lambda bool ,(format "~a_isnull" file-prefix) gsl_vector))
          (define vector-ispositive? (foreign-safe-lambda bool ,(format "~a_ispos" file-prefix) gsl_vector))
          (define vector-isnegative? (foreign-safe-lambda bool ,(format "~a_isneg" file-prefix) gsl_vector))
          (define vector-isnonneg? (foreign-safe-lambda bool ,(format "~a_isnonneg" file-prefix) gsl_vector))
          (define vector-equal? (foreign-safe-lambda bool ,(format "~a_equal" file-prefix) gsl_vector gsl_vector)))))))
