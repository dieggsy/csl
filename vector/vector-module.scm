(import-for-syntax (chicken format))

(define-syntax make-vector-module
  (ir-macro-transformer
   (lambda (e i c)
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
                                       (eq? (strip-syntax (cadr base-type*)) 'double))
                                  (eq? (strip-syntax base-type*) 'double))
                              ""
                              (string-append "_"
                                             (symbol->string
                                              (if complex
                                                  (strip-syntax (cadr base-type*))
                                                  (strip-syntax base-type*))))))
            (type-suffix (if complex
                             (string-append "_complex" rtype-suffix)
                             rtype-suffix)))
       `(module ,(cadr e) = generic-vector
                (import scheme
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
                                          (if (and complex (eq? (strip-syntax base-type) 'double))
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
                  (,fsl ,base-type* ,(format "~a_get" file-prefix)
                        (const gsl_vector)
                        (const unsigned-int)))

                (define vset!
                  (,fsl void ,(format "~a_set" file-prefix)
                        gsl_vector
                        (const unsigned-int)
                        ,base-type*))

                ;; Initializing vector elements
                (define vfill!
                  (,fsl void ,(format "~a_set_all" file-prefix) gsl_vector ,base-type*))

                ;; (define gsl_vector_set_zero
                ;;   (,fsl void ,(format "~a_set_zero" file-prefix) gsl_vector))

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
                  (,fsl int ,(format "~a_scale" file-prefix) gsl_vector ,base-type*))

                (define v+c
                  (,fsl int ,(format "~a_add_constant" file-prefix) gsl_vector ,base-type*))

                ;; Finding max and min elements of vectors
                ,@(if complex
                      '((define (vmax v) (error "Complex numbers have no ordering."))
                        (define (vmin v) (error "Complex numbers have no ordering."))
                        (define (vargmax v) (error "Complex numbers have no ordering."))
                        (define (vargmin v) (error "Complex numbers have no ordering.")))
                      `((define vmax (foreign-safe-lambda ,base-type ,(format "~a_max" file-prefix) gsl_vector))
                        (define vmin (foreign-safe-lambda ,base-type ,(format "~a_min" file-prefix) gsl_vector))
                        (define vargmax (foreign-safe-lambda int ,(format "~a_max_index" file-prefix) gsl_vector))
                        (define vargmin (foreign-safe-lambda int ,(format "~a_min_index" file-prefix) gsl_vector))))

                ;; Vector properties
                (define vnull? (foreign-safe-lambda bool ,(format "~a_isnull" file-prefix) gsl_vector))
                (define vpositive? (foreign-safe-lambda bool ,(format "~a_ispos" file-prefix) gsl_vector))
                (define vnegative? (foreign-safe-lambda bool ,(format "~a_isneg" file-prefix) gsl_vector))
                (define vnonnegative? (foreign-safe-lambda bool ,(format "~a_isnonneg" file-prefix) gsl_vector))
                (define vequal? (foreign-safe-lambda bool ,(format "~a_equal" file-prefix) gsl_vector gsl_vector))
                ,@(if complex
                      `((define vimag
                          (foreign-safe-lambda* gsl_vector ((gsl_vector v))
                                                ,(format "~a *p0 = ~a_alloc(v->size);" file-prefix file-prefix)
                                                ,(format "gsl_vector~a_view p1 = ~a_imag(v);" rtype-suffix file-prefix)
                                                "for (int i = 0; i < v->size; i++) { "
                                                ,(format "~a iz = gsl_vector~a_get(&p1.vector, i);" (strip-syntax base-type) rtype-suffix)
                                                ,(format "gsl~a z;" type-suffix)
                                                "GSL_SET_COMPLEX(&z, 0, iz);"
                                                ,(format "~a_set(p0,i,z);" file-prefix)
                                                "}"
                                                "C_return(p0);"))
                        (define vreal
                          (foreign-safe-lambda* gsl_vector ((gsl_vector v))
                                                ,(format "~a *p0 = ~a_alloc(v->size);" file-prefix file-prefix)
                                                ,(format "gsl_vector~a_view p1 = ~a_real(v);" rtype-suffix file-prefix)
                                                "for (int i = 0; i < v->size; ++i) { "
                                                ,(format "~a rz = gsl_vector~a_get(&p1.vector, i);" (strip-syntax base-type) rtype-suffix)
                                                ,(format "gsl~a z;" type-suffix)
                                                "GSL_SET_COMPLEX(&z, rz, 0);"
                                                ,(format "~a_set(p0,i,z);" file-prefix)
                                                "}"
                                                "C_return(p0);")))
                      '((define (vimag v)
                          (let ((v (valloc (vlength v))))
                            (vfill! v 0)
                            v))
                        (define (vreal v) v))))))))
