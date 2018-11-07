(import-for-syntax (chicken format)
                   (chicken base))

(define-syntax make-matrix-module
  (er-macro-transformer
   (lambda (e i c)
     (let* ((module-name (cadr e))
            (file-prefix (caddr e))
            (base-type* (cadddr e))
            (complex (pair? base-type*))
            (base-type (if complex
                           (cadr base-type*)
                           base-type*))
            (vector-module-name
             (symbol-append
              'gsl.vector.
              (if complex
                  'complex.
                  '||)
              base-type))
            (fsl (if complex
                     'complex-foreign-lambda
                     'foreign-safe-lambda))
            (rtype-suffix (if (or (and complex
                                       (eq? (strip-syntax (cadr base-type*)) 'double))
                                  (eq? (strip-syntax base-type*) 'double))
                              ""
                              (string-append "_" (symbol->string (strip-syntax base-type)))))
            (type-suffix (if complex
                             (string-append "_complex" rtype-suffix)
                             rtype-suffix)))
       `(module ,module-name (matrix-rows
                              matrix-cols
                              matrix-alloc
                              matrix-calloc
                              matrix-free!
                              matrix-get
                              matrix-set!
                              matrix-set-all!
                              matrix-set-identity!
                              matrix-submatrix
                              matrix-submatrix-with-stride
                              matrix-view-vector
                              matrix-diagonal
                              matrix-subdiagonal
                              matrix-superdiagonal
                              matrix-memcpy!
                              matrix-swap!
                              matrix-get-row
                              matrix-get-col
                              matrix-set-row!
                              matrix-set-col!
                              matrix-swap-rows!
                              matrix-swap-columns!
                              matrix-swap-rowcol!
                              matrix-transpose-memcpy!
                              matrix-transpose!
                              matrix-add!
                              matrix-sub!
                              matrix-mul-elements!
                              matrix-div-elements!
                              matrix-scale!
                              matrix-add-constant!
                              matrix-max
                              matrix-min
                              matrix-max-index
                              matrix-min-index
                              matrix-isnull?
                              matrix-ispositive?
                              matrix-isnegative?
                              matrix-isnonneg?
                              matrix-equal?)
          (import (except scheme vector-set!)
                  (chicken base)
                  (chicken module)
                  (chicken gc)
                  (chicken foreign)
                  (chicken memory)
                  foreigners
                  (except
                   (gsl vector ,@(if complex
                                     base-type*
                                     `(,base-type)))
                   vector-size))
          (reexport (only (csl vector ,@(if complex
                                            base-type*
                                            `(,base-type)))
                          ptr->vector
                          vector->ptr))
          (include-relative "../csl-error.scm")
          ,@(if complex
                '((include-relative "../complex-foreign-lambda.scm"))
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

          ,@(if complex
                `((foreign-declare ,(format "#include <gsl/gsl_matrix_complex_~a.h>" base-type))
                  (foreign-declare ,(format "#include <gsl/gsl_vector_complex_~a.h>" base-type))
                  (foreign-declare "#include <gsl/gsl_complex.h>"))
                `((foreign-declare ,(format "#include <gsl/gsl_matrix_~a.h>" base-type))
                  (foreign-declare ,(format "#include <gsl/gsl_vector_~a.h>" base-type))))

          (define-foreign-record-type (gsl_matrix ,file-prefix)
            (unsigned-int size1 matrix-rows)
            (unsigned-int size2 matrix-cols)
            (unsigned-int tda gsl_matrix.tda)
            ((c-pointer ,base-type) data gsl_matrix.data)
            (gsl_block block gsl_matrix.block)
            (int owner gsl_matrix.owner))

          (define-foreign-record-type (gsl_vector ,(format "gsl_vector~a" type-suffix))
            (unsigned-int size vector-size)
            (unsigned-int stride gsl_vector.stride)
            ((c-pointer ,base-type) data gsl_vector.data)
            (gsl_block block gsl_vector.block)
            (int owner gsl_vector.owner))

          (define (matrix-alloc rows cols)
            (tag-pointer
             (set-finalizer!
              ((foreign-safe-lambda gsl_matrix ,(format "~a_alloc" file-prefix) unsigned-int unsigned-int)
               rows cols)
              matrix-free!)
             ',module-name))

          (define (matrix-calloc rows cols)
            (tag-pointer
             (set-finalizer!
              ((foreign-safe-lambda gsl_matrix ,(format "~a_calloc" file-prefix) unsigned-int unsigned-int)
               rows cols)
              matrix-free!)
             ',module-name))

          (define matrix-free!
            (foreign-safe-lambda void ,(format "~a_free" file-prefix) gsl_matrix))

          ;; Accessing matrix elements
          (define matrix-get
            (,fsl ,base-type* ,(format "~a_get" file-prefix)
                  gsl_matrix
                  unsigned-int
                  unsigned-int))

          (define matrix-set!
            (,fsl void ,(format "~a_set" file-prefix)
                  gsl_matrix
                  unsigned-int
                  unsigned-int
                  ,base-type*))

          ;; matrix-ptr omitted
          ;; matrix-const-ptr omitted

          ;; Initializing matrix elements
          (define matrix-set-all!
            (,fsl void ,(format "~a_set_all" file-prefix) gsl_matrix ,base-type*))

          (define matrix-set-identity!
            (foreign-safe-lambda void ,(format "~a_set_identity" file-prefix) gsl_matrix))

          ;; Reading and writing matrices
          ;; omitted

          ;; Matrix views
          (define (matrix-submatrix m k1 k2 n1 n2)
            (tag-pointer
             (set-finalizer!
              ((foreign-safe-lambda* gsl_matrix ((gsl_matrix m)
                                                 (unsigned-int k1)
                                                 (unsigned-int k2)
                                                 (unsigned-int n1)
                                                 (unsigned-int n2))
                 ,(format "~a *p0 = ~a_alloc(n1,n2);" file-prefix file-prefix)
                 ,(format "~a_view p1 = ~a_submatrix(m,k1,k2,n1,n2);" file-prefix file-prefix)
                 ,(format "memcpy(p0, &p1.matrix, sizeof(~a));" file-prefix)
                 "C_return(p0);")
               m k1 k2 n1 n2)
              matrix-free!)
             ',module-name))

          ;; matrix-const-submatrix omitted

          ;; added for convenience
          (define (matrix-submatrix-with-stride m k1 k2 s1 s2 n1 n2)
            (tag-pointer
             (set-finalizer!
              ((foreign-safe-lambda* gsl_matrix ((gsl_matrix m)
                                                 (unsigned-int k1)
                                                 (unsigned-int k2)
                                                 (unsigned-int s1)
                                                 (unsigned-int s2)
                                                 (unsigned-int n1)
                                                 (unsigned-int n2))
                 ,(format "~a *r = ~a_alloc(n1,n2);" file-prefix file-prefix)
                 "int rm, rr;"
                 "for (rm = k1, rr = 0; rr < n1; rr++, rm += s1){"
                 ,(format "gsl_vector~a *row = gsl_vector~a_alloc(m->size2);" type-suffix type-suffix)
                 ,(format "~a_get_row(row,m,rm);" file-prefix)
                 ,(format "gsl_vector~a_view row_with_stride = gsl_vector~a_subvector_with_stride(row,k2,s2,n2);" type-suffix type-suffix)
                 ,(format "gsl_matrix~a_set_row(r,rr,&row_with_stride.vector);" type-suffix)
                 ,(format "gsl_vector~a_free(row);" type-suffix)
                 "}"
                 "C_return(r);")
               m k1 k2 s1 s2 n1 n2)
              matrix-free!)
             ',module-name))

          ;; matrix-view-array omitted
          ;; matrix-const-view-array omitted
          ;; matrix-view-array-with-tda omitted
          ;; matrix-const-view-array-with-tda omitted

          (define (matrix-view-vector v rows cols)
            (tag-pointer
             (set-finalizer!
              ((foreign-safe-lambda* gsl_matrix ((gsl_vector v)
                                                 (unsigned-int n1)
                                                 (unsigned-int n2))
                 ,(format "~a *p0 = ~a_alloc(n1, n2);" file-prefix file-prefix)
                 ,(format "~a_view p1 = ~a_view_vector(v,n1,n2);" file-prefix file-prefix)
                 ,(format "memcpy(p0,&p1.matrix, sizeof(~a));" file-prefix)
                 "C_return(p0);")
               v rows cols)
              matrix-free!)
             ',module-name))

          ;; matrix-const-view-vector omitted
          ;; matrix-view-vector-with-tda omitted
          ;; matrix-const-view-vector-with-tda ommitted

          ;; Creating row and column views
          ;; matrix-row omitted
          ;; matrix-const-row amitted
          ;; matrix-column omitted
          ;; matrix-const-column omitted
          ;; matrix-subrow omitted
          ;; matrix-const-subrow omitted
          ;; matrix-subcolumn omitted
          ;; matrix-const-subcolumn omitted

          (define (matrix-diagonal m)
            (tag-pointer
             (set-finalizer!
              ((foreign-safe-lambda* gsl_vector ((gsl_matrix m))
                 ,(format "gsl_vector~a_view p1 = ~a_diagonal(m);" type-suffix file-prefix)
                 ,(format "gsl_vector~a *p0 = gsl_vector~a_alloc(p1.vector.size);" type-suffix type-suffix)
                 ,(format "memcpy(p0,&p1.vector, sizeof(gsl_vector~a));" type-suffix)
                 "C_return(p0);")
               m)
              vector-free!)
             ',vector-module-name))

          ;; matrix-const-diagonal omitted

          (define (matrix-subdiagonal m k)
            (tag-pointer
             (set-finalizer!
              ((foreign-safe-lambda* gsl_vector ((gsl_matrix m) (unsigned-int k))
                 ,(format "gsl_vector~a_view p1 = ~a_subdiagonal(m, k);" type-suffix file-prefix)
                 ,(format "gsl_vector~a *p0 = gsl_vector~a_alloc(p1.vector.size);" type-suffix type-suffix)
                 ,(format "memcpy(p0,&p1.vector, sizeof(gsl_vector~a));" type-suffix)
                 "C_return(p0);")
               m k)
              vector-free!)
             ',vector-module-name))

          ;; matrix-const-subdiagonal omitted

          (define (matrix-superdiagonal m k)
            (tag-pointer
             (set-finalizer!
              ((foreign-safe-lambda* gsl_vector ((gsl_matrix m) (unsigned-int k))
                 ,(format "gsl_vector~a_view p1 = ~a_superdiagonal(m, k);" type-suffix file-prefix)
                 ,(format "gsl_vector~a *p0 = gsl_vector~a_alloc(p1.vector.size);" type-suffix type-suffix)
                 ,(format "memcpy(p0,&p1.vector, sizeof(gsl_vector~a));" type-suffix)
                 "C_return(p0);")
               m k)
              vector-free!)
             ',vector-module-name))

          ;; matrix-const-superdiagonal omitted

          ;; Copying matrices
          (define matrix-memcpy!
            (foreign-safe-lambda int ,(format "~a_memcpy" file-prefix) gsl_matrix gsl_matrix))


          (define matrix-swap!
            (foreign-safe-lambda int ,(format "~a_swap" file-prefix) gsl_matrix gsl_matrix))

          ;; Copying rows and columns
          (define (matrix-get-row m i)
            (let ((v (vector-alloc (matrix-cols m))))
              ((foreign-safe-lambda int ,(format "~a_get_row" file-prefix) gsl_vector gsl_matrix unsigned-int)
               v m i)
              v))

          (define (matrix-get-col m i)
            (let ((v (vector-alloc (matrix-rows m))))
              ((foreign-safe-lambda int ,(format "~a_get_col" file-prefix) gsl_vector gsl_matrix unsigned-int)
               v m i)
              v))

          (define matrix-set-row!
            (foreign-safe-lambda int ,(format "~a_set_row" file-prefix) gsl_matrix unsigned-int gsl_vector))

          (define matrix-set-col!
            (foreign-safe-lambda int ,(format "~a_set_col" file-prefix) gsl_matrix unsigned-int gsl_vector))

          ;; Exchanging rows an dcolumns
          (define matrix-swap-rows!
            (foreign-safe-lambda int ,(format "~a_swap_rows" file-prefix) gsl_matrix unsigned-int unsigned-int))

          (define matrix-swap-columns!
            (foreign-safe-lambda int ,(format "~a_swap_columns" file-prefix) gsl_matrix unsigned-int unsigned-int))

          (define matrix-swap-rowcol!
            (foreign-safe-lambda int ,(format "~a_swap_rowcol" file-prefix) gsl_matrix unsigned-int unsigned-int))

          (define matrix-transpose-memcpy!
            (foreign-safe-lambda int ,(format "~a_transpose_memcpy" file-prefix) gsl_matrix gsl_matrix))

          (define matrix-transpose!
            (foreign-safe-lambda int ,(format "~a_transpose" file-prefix) gsl_matrix))

          ;; Matrix-operations
          (define matrix-add!
            (foreign-safe-lambda int ,(format "~a_add" file-prefix) gsl_matrix gsl_matrix))

          (define matrix-sub!
            (foreign-safe-lambda int ,(format "~a_sub" file-prefix) gsl_matrix gsl_matrix))

          (define matrix-mul-elements!
            (foreign-safe-lambda int ,(format "~a_mul_elements" file-prefix) gsl_matrix gsl_matrix))

          (define matrix-div-elements!
            (foreign-safe-lambda int ,(format "~a_div_elements" file-prefix) gsl_matrix gsl_matrix))

          (define matrix-scale!
            (,fsl int ,(format "~a_scale" file-prefix) gsl_matrix ,base-type*))

          (define matrix-add-constant!
            (,fsl int ,(format "~a_add_constant" file-prefix) gsl_matrix ,base-type*))

          ;; Finding max and min elements of matrixs
          (define matrix-max
            ,(if complex
                 '(lambda (v) (error "Complex numbers have no ordering."))
                 `(foreign-safe-lambda ,base-type ,(format "~a_max" file-prefix) gsl_matrix)))

          (define matrix-min
            ,(if complex
                 '(lambda (v) (error "Complex numbers have no ordering."))
                 `(foreign-safe-lambda ,base-type ,(format "~a_min" file-prefix) gsl_matrix)))

          ;; matrix-minmax omitted

          (define matrix-max-index
            ,(if complex
                 '(lambda (v) (error "Complex numbers have no ordering."))
                 `(foreign-primitive ((gsl_matrix m))
                      "size_t imax, jmax;"
                    ,(format "~a_max_index(m,&imax,&jmax);" file-prefix)
                    "C_word *ptr = C_alloc(C_SIZEOF_FIX_BIGNUM);"
                    "C_word av[4] = { C_SCHEME_UNDEFINED, C_k, C_int_to_num(&ptr, imax), C_int_to_num(&ptr, jmax)};"
                    "C_values(4,av);")))

          (define matrix-min-index
            ,(if complex
                 '(lambda (v) (error "Complex numbers have no ordering."))
                 `(foreign-primitive ((gsl_matrix m))
                      "size_t imin, jmin;"
                    ,(format "~a_min_index(m,&imin,&jmin);" file-prefix)
                    "C_word *ptr = C_alloc(C_SIZEOF_FIX_BIGNUM);"
                    "C_word av[4] = { C_SCHEME_UNDEFINED, C_k, C_int_to_num(&ptr, imin), C_int_to_num(&ptr, jmin)};"
                    "C_values(4,av);")))

          ;; matrix-minmax-index omitted

          ;; Matrix properties
          (define matrix-isnull? (foreign-safe-lambda bool ,(format "~a_isnull" file-prefix) gsl_matrix))
          (define matrix-ispositive? (foreign-safe-lambda bool ,(format "~a_ispos" file-prefix) gsl_matrix))
          (define matrix-isnegative? (foreign-safe-lambda bool ,(format "~a_isneg" file-prefix) gsl_matrix))
          (define matrix-isnonneg? (foreign-safe-lambda bool ,(format "~a_isnonneg" file-prefix) gsl_matrix))
          (define matrix-equal? (foreign-safe-lambda bool ,(format "~a_equal" file-prefix) gsl_matrix gsl_matrix)))))))
