(declare (unit gsl.matrix))
(include "utils/syntax-utils.scm")
(include "utils/declarations.scm")

(import-for-syntax (only chicken.format format)
                   (only chicken.string string-translate*)
                   (only matchable match))

(define-syntax make-matrix-module
  (er-macro-transformer
   (lambda (e i compare)
     (let* ((module-name (cadr e))
            (vector-module-name
             (string->symbol
              (string-translate* (symbol->string module-name) '(("matrix" . "vector")))))
            (file-prefix (caddr e))
            (vector-file-prefix
             (string-translate* file-prefix '(("matrix" . "vector"))))
            (base-type (cadddr e))
            (complex-ctype (complex-ctype-string base-type))
            (real-prefix (string-translate* file-prefix '(("_complex" . ""))))
            (c-sizeof-type (sizeof-type-str base-type))
            (c-type-constructor (c-constructor-str base-type))
            (substitutions
             `(("#{base-type}" . ,(symbol->string base-type))
               ("#{file-prefix}" . ,file-prefix)
               ("#{complex-ctype}" . ,complex-ctype)
               ("#{real-prefix}" . ,real-prefix)
               ("#{c-sizeof-type}" . ,c-sizeof-type)
               ("#{c-type-constructor}" . ,c-type-constructor)
               ("#{vector-file-prefix}" . ,vector-file-prefix)))
            (csl-matrix (string->symbol file-prefix))
            (csl-vector (string->symbol vector-file-prefix)))
       `(module ,module-name (matrix?
                              matrix->ptr
                              ptr->matrix
                              matrix-size1
                              matrix-size2
                              matrix-alloc
                              matrix-calloc
                              matrix-free!
                              matrix-get
                              matrix-set!
                              matrix-set-all!
                              matrix-set-zero!
                              matrix-set-identity!
                              matrix-fwrite
                              matrix-fread!
                              matrix-fprintf
                              matrix-fscanf!
                              matrix-submatrix
                              matrix-submatrix-set!
                              matrix-submatrix-with-stride
                              matrix-submatrix-with-stride-set!
                              matrix-view-vector
                              matrix-row
                              matrix-column
                              matrix-subrow
                              matrix-subcolumn
                              matrix-diagonal
                              matrix-subdiagonal
                              matrix-superdiagonal
                              matrix-memcpy!
                              matrix-swap!
                              matrix-get-row!
                              matrix-get-col!
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
                              matrix-minmax
                              matrix-max-index
                              matrix-min-index
                              matrix-minmax-index
                              matrix-isnull?
                              matrix-ispos?
                              matrix-isneg?
                              matrix-isnonneg?
                              matrix-equal?)
          (import (except scheme vector-set! vector?)
                  srfi-4
                  ;; bind
                  chicken.foreign
                  (only chicken.module reexport)
                  (only chicken.base
                        void
                        error
                        include
                        define-record-type
                        identity)
                  (only chicken.gc set-finalizer!)
                  (only chicken.file file-exists?)
                  (only chicken.file.posix file-size)
                  (only chicken.io read-list)
                  (only miscmacros ensure)
                  (only matchable match)
                  ,vector-module-name)

          (reexport (only ,vector-module-name vector-free!))
          (include "utils/error-handler.scm")
          (include "utils/complex-types.scm")
          (include "utils/stdio.scm")

          (foreign-declare ,(format "#include <gsl/~a.h>"
                                    (match file-prefix
                                      ("gsl_matrix_complex" "gsl_matrix_complex_double")
                                      ("gsl_matrix" "gsl_matrix_double")
                                      (else file-prefix))))

          (define-record-type matrix
            (ptr->matrix ptr)
            matrix?
            (ptr matrix->ptr))


          (define-foreign-type ,csl-matrix
            (nonnull-c-pointer ,file-prefix)
            matrix->ptr
            ptr->matrix)

          (define-foreign-type ,csl-vector
            (nonnull-c-pointer ,vector-file-prefix)
            vector->ptr
            ptr->vector)

          (define matrix-size1
            (foreign-lambda* size_t ((,csl-matrix m))
              "C_return(m->size1);"))

          (define matrix-size2
            (foreign-lambda* size_t ((,csl-matrix m))
              "C_return(m->size2);"))

          ;;; Matrix Allocation
          (define matrix-alloc
            (foreign-lambda ,csl-matrix ,(string-append file-prefix "_alloc") size_t size_t))
          (define matrix-calloc
            (foreign-lambda ,csl-matrix ,(string-append file-prefix "_calloc") size_t size_t))
          (define matrix-free!
            (foreign-lambda void ,(string-append file-prefix "_free") ,csl-matrix))

          ;;; Accessing matrix elements
          (define matrix-get
            ,(case base-type
               ((complex complex-float)
                `(foreign-lambda* scheme-object
                    ((,csl-matrix m) ((const size_t) i) ((const size_t) j))
                  ,(string-translate*
                    "#{complex-ctype} z = #{file-prefix}_get(m,i,j);
                     C_return(scheme_make_rect(GSL_REAL(z),GSL_IMAG(z)));"
                    substitutions)))
               (else
                `(foreign-safe-lambda ,base-type
                     ,(string-append file-prefix "_get")
                   ,csl-matrix (const size_t) (const size_t)))))

          (define matrix-set!
            ,(case base-type
               ((complex complex-float)
                `(foreign-lambda* void ((,csl-matrix m) ((const size_t) i)
                                        ((const size_t) j) (,base-type z))
                   ,(string-translate*
                     "#{complex-ctype} _z;
                      GSL_SET_COMPLEX(&_z, z[0], z[1]);
                      #{file-prefix}_set(m, i, j, _z);"
                     substitutions)))
               (else
                `(foreign-safe-lambda void
                     ,(string-append file-prefix "_set")
                   ,csl-matrix (const size_t) (const size_t) ,base-type))))
          ;; gsl_matrix_ptr omitted

          ;;; Initializing matrix elements
          (define matrix-set-all!
            ,(case base-type
               ((complex complex-float)
                `(foreign-safe-lambda* void ((,csl-matrix m) (,base-type z))
                   ,(string-translate*
                     "#{complex-ctype} _z;
                      GSL_SET_COMPLEX(&_z, z[0], z[1]);
                      #{file-prefix}_set_all(m, _z);"
                     substitutions)))
               (else
                `(foreign-safe-lambda void
                     ,(string-append file-prefix "_set_all")
                   ,csl-matrix  ,base-type))))
          (define matrix-set-zero!
            (foreign-lambda void ,(string-append file-prefix "_set_zero")
              ,csl-matrix))
          (define matrix-set-identity!
            (foreign-lambda void ,(string-append file-prefix "_set_identity")
              ,csl-matrix))

          ;;; Reading and writing matrices
          (define (matrix-fwrite fileport matrix)
            (let* ((FILE (get-c-file 'matrix-fwrite fileport))
                   (ret ((foreign-lambda int ,(string-append file-prefix "_fwrite")
                           (c-pointer "FILE") ,csl-matrix)
                         FILE matrix)))
              (if (= ret (foreign-value GSL_EFAILED int))
                  (error 'matrix-fwrite! "error writing to port")
                  (void))))

          (define (matrix-fread! fileport matrix)
            (let* ((FILE (get-c-file 'matrix-fread fileport))
                   (ret ((foreign-lambda int ,(string-append file-prefix "_fread")
                           (c-pointer "FILE") ,csl-matrix)
                         FILE matrix)))
              (if (= ret (foreign-value GSL_EFAILED int))
                  (error 'matrix-fread! "error reading from port")
                  (void))))

          (define (matrix-fprintf fileport matrix format)
            (let* ((FILE (get-c-file 'matrix-fprintf fileport))
                   (ret ((foreign-lambda int ,(string-append file-prefix "_fprintf")
                           (c-pointer "FILE") ,csl-matrix c-string)
                         FILE matrix format)))
              (if (= ret (foreign-value GSL_EFAILED int))
                  (error 'matrix-fprintf! "error writing port")
                  (void))))

          (define (matrix-fscanf! fileport matrix)
            (let* ((FILE (get-c-file 'matrix-fscanf fileport))
                   (ret ((foreign-lambda int ,(string-append file-prefix "_fscanf")
                           (c-pointer "FILE") ,csl-matrix)
                         FILE matrix)))
              (if (= ret (foreign-value GSL_EFAILED int))
                  (error 'matrix-fscanf! "error writing port")
                  (void))))

          ;;; Matrix views
          (define matrix-submatrix
            (foreign-safe-lambda* ,csl-matrix ((,csl-matrix m) (size_t k1) (size_t k2)
                                          (size_t n1) (size_t n2))
              ,(string-translate*
                "#{file-prefix}_view view = #{file-prefix}_submatrix(m, k1, k2, n1, n2);
                 #{file-prefix} *mat = #{file-prefix}_alloc(view.matrix.size1, view.matrix.size2);
                 #{file-prefix}_memcpy(mat, &view.matrix);
                 C_return(mat);"
                substitutions)))

          (define matrix-submatrix-set!
            (foreign-safe-lambda* ,csl-matrix ((,csl-matrix m) (size_t k1) (size_t k2)
                                          (size_t n1) (size_t n2) (,csl-matrix sub))
              ,(string-translate*
                "#{file-prefix}_view view = #{file-prefix}_submatrix(m, k1, k2, n1, n2);
                 #{file-prefix}_memcpy(&view.matrix, sub);"
                substitutions)))

          (define matrix-submatrix-with-stride
            (foreign-safe-lambda* ,csl-matrix ((,csl-matrix m) (size_t k1) (size_t k2)
                                         (size_t s1) (size_t s2)
                                         (size_t n1) (size_t n2))
              ,(string-translate*
                "#{file-prefix} *sub = #{file-prefix}_alloc(n1,n2);
                 int row_matrix, row_sub;
                 for (row_matrix = k1, row_sub = 0; row_sub < n1; ++row_sub, row_matrix +=s1) {
                     #{vector-file-prefix}_view row = #{file-prefix}_row(m, row_matrix);
                     #{vector-file-prefix}_view row_with_stride = #{vector-file-prefix}_subvector_with_stride(&row.vector, k2, s2, n2);
                     #{file-prefix}_set_row(sub, row_sub, &row_with_stride.vector);
                 }
                 C_return(sub);"
                substitutions)))

          (define matrix-submatrix-with-stride-set!
            (foreign-safe-lambda* ,csl-matrix ((,csl-matrix m) (size_t k1) (size_t k2)
                                               (size_t s1) (size_t s2)
                                               (size_t n1) (size_t n2)
                                               (,csl-matrix sub))
              ,(string-translate*
                "int row_matrix, row_sub;
                 for (row_matrix = k1, row_sub = 0; row_sub < n1; ++row_sub, row_matrix +=s1) {
                     #{vector-file-prefix}_view row = #{file-prefix}_row(m, row_matrix);
                     #{vector-file-prefix}_view subrow = #{file-prefix}_row(sub, row_sub);
                     #{vector-file-prefix}_view row_with_stride = #{vector-file-prefix}_subvector_with_stride(&row.vector, k2, s2, n2);
                     #{vector-file-prefix}_memcpy(&row_with_stride.vector,&subrow.vector);
                 }"
                substitutions)))
          ;; matrix-view-array omitted
          ;; matrix-const-view-array omitted
          ;; matrix-view-array-with-tda omitted
          ;; matrix-const-view-array-with-tda omitted
          ;; matrix-const-submatrix omitted

          (define matrix-view-vector
            (foreign-safe-lambda* ,csl-matrix ((,csl-vector v) (size_t n1) (size_t n2))
              ,(string-translate*
               "#{file-prefix}_view view = #{file-prefix}_view_vector(v, n1, n2);
                #{file-prefix} *mat = #{file-prefix}_alloc(n1, n2);
                #{file-prefix}_memcpy(mat, &view.matrix);
                C_return(mat);"
               substitutions)))

          ;; matrix-const-view-vector omitted
          ;; matrix-view-vector-with-tda omitted
          ;; matrix-const-view-vector-with-tda ommitted

          ;;; Creating row and column views
          (define matrix-row
            (foreign-safe-lambda* ,csl-vector ((,csl-matrix m) (size_t i))
              ,(string-translate*
                "#{vector-file-prefix}_view view = #{file-prefix}_row(m, i);
                #{vector-file-prefix} *vec = #{vector-file-prefix}_alloc(view.vector.size);
                #{vector-file-prefix}_memcpy(vec, &view.vector);
                C_return(vec);"
                substitutions)))

          (define matrix-column
            (foreign-safe-lambda* ,csl-vector ((,csl-matrix m) (size_t j))
              ,(string-translate*
                "#{vector-file-prefix}_view view = #{file-prefix}_column(m, j);
                #{vector-file-prefix} *vec = #{vector-file-prefix}_alloc(view.vector.size);
                #{vector-file-prefix}_memcpy(vec, &view.vector);
                C_return(vec);"
                substitutions)))

          (define matrix-subrow
            (foreign-safe-lambda* ,csl-vector ((,csl-matrix m) (size_t i)
                                               (size_t offset) (size_t n))
              ,(string-translate*
                "#{vector-file-prefix}_view view = #{file-prefix}_subrow(m, i, offset, n);
                #{vector-file-prefix} *vec = #{vector-file-prefix}_alloc(view.vector.size);
                #{vector-file-prefix}_memcpy(vec, &view.vector);
                C_return(vec);"
                substitutions)))

          (define matrix-subcolumn
            (foreign-safe-lambda* ,csl-vector ((,csl-matrix m) (size_t j)
                                               (size_t offset) (size_t n))
              ,(string-translate*
                "#{vector-file-prefix}_view view = #{file-prefix}_subcolumn(m, j, offset, n);
                #{vector-file-prefix} *vec = #{vector-file-prefix}_alloc(view.vector.size);
                #{vector-file-prefix}_memcpy(vec, &view.vector);
                C_return(vec);"
                substitutions)))

          (define matrix-diagonal
            (foreign-safe-lambda* ,csl-vector ((,csl-matrix m))
              ,(string-translate*
                "#{vector-file-prefix}_view view = #{file-prefix}_diagonal(m);
                #{vector-file-prefix} *vec = #{vector-file-prefix}_alloc(view.vector.size);
                #{vector-file-prefix}_memcpy(vec, &view.vector);
                C_return(vec);"
                substitutions)))

          (define matrix-subdiagonal
            (foreign-safe-lambda* ,csl-vector ((,csl-matrix m) (size_t k))
              ,(string-translate*
                "#{vector-file-prefix}_view view = #{file-prefix}_subdiagonal(m, k);
                #{vector-file-prefix} *vec = #{vector-file-prefix}_alloc(view.vector.size);
                #{vector-file-prefix}_memcpy(vec, &view.vector);
                C_return(vec);"
                substitutions)))

          (define matrix-superdiagonal
            (foreign-safe-lambda* ,csl-vector ((,csl-matrix m) (size_t k))
              ,(string-translate*
                "#{vector-file-prefix}_view view = #{file-prefix}_superdiagonal(m, k);
                #{vector-file-prefix} *vec = #{vector-file-prefix}_alloc(view.vector.size);
                #{vector-file-prefix}_memcpy(vec, &view.vector);
                C_return(vec);"
                substitutions)))

          ;;; Copying matrices
          (define matrix-memcpy!
            (foreign-safe-lambda int ,(string-append file-prefix "_memcpy")
              ,csl-matrix ,csl-matrix))
          (define matrix-swap!
            (foreign-safe-lambda int ,(string-append file-prefix "_swap")
              ,csl-matrix ,csl-matrix))

          ;;; Copying rows and columns
          (define matrix-get-row!
            (foreign-safe-lambda int ,(string-append file-prefix "_get_row")
              ,csl-vector ,csl-matrix size_t))
          (define matrix-get-col!
            (foreign-safe-lambda int ,(string-append file-prefix "_get_col")
              ,csl-vector ,csl-matrix size_t))

          (define matrix-set-row!
            (foreign-safe-lambda int ,(string-append file-prefix "_set_row")
              ,csl-matrix size_t ,csl-vector))
          (define matrix-set-col!
            (foreign-safe-lambda int ,(string-append file-prefix "_set_col")
              ,csl-matrix size_t ,csl-vector))

          ;;; Exchanging rows an dcolumns
          (define matrix-swap-rows!
            (foreign-safe-lambda int ,(string-append file-prefix "_swap_rows")
              ,csl-matrix size_t size_t))

          (define matrix-swap-columns!
            (foreign-safe-lambda int ,(string-append file-prefix "_swap_columns")
              ,csl-matrix size_t size_t))

          (define matrix-swap-rowcol!
            (foreign-safe-lambda int ,(string-append file-prefix "_swap_rowcol")
              ,csl-matrix size_t size_t))

          (define matrix-transpose-memcpy!
            (foreign-safe-lambda int ,(string-append file-prefix "_transpose_memcpy")
              ,csl-matrix ,csl-matrix))

          (define matrix-transpose!
            (foreign-safe-lambda int ,(string-append file-prefix "_transpose")
              ,csl-matrix))

          ;;; Matrix-operations
          (define matrix-add!
            (foreign-safe-lambda int ,(string-append file-prefix "_add")
              ,csl-matrix ,csl-matrix))

          (define matrix-sub!
            (foreign-safe-lambda int ,(string-append file-prefix "_sub")
              ,csl-matrix ,csl-matrix))

          (define matrix-mul-elements!
            (foreign-safe-lambda int ,(string-append file-prefix "_mul_elements")
              ,csl-matrix ,csl-matrix))

          (define matrix-div-elements!
            (foreign-safe-lambda int ,(string-append file-prefix "_div_elements")
              ,csl-matrix ,csl-matrix))

          (define matrix-scale!
            ,(case base-type
               ((complex complex-float)
                `(foreign-safe-lambda* int ((,csl-matrix v) (,base-type z))
                   ,(string-translate*
                     "#{complex-ctype} _z;
                      GSL_SET_COMPLEX(&_z, z[0], z[1]);
                      #{file-prefix}_scale(v, _z);"
                     substitutions)))
               (else
                `(foreign-safe-lambda int ,(string-append file-prefix "_scale")
                   ,csl-matrix ,base-type))))


          (define matrix-add-constant!
            ,(case base-type
               ((complex complex-float)
                `(foreign-safe-lambda* int ((,csl-matrix v) (,base-type z))
                   ,(string-translate*
                     "#{complex-ctype} _z;
                      GSL_SET_COMPLEX(&_z, z[0], z[1]);
                      #{file-prefix}_add_constant(v, _z);"
                     substitutions)))
               (else
                `(foreign-safe-lambda int ,(string-append file-prefix "_add_constant")
                   ,csl-matrix ,base-type))))

          ;;; Finding max and min elements of matrixs
          (define matrix-max
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'matrix-max "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-lambda ,base-type ,(string-append file-prefix "_max")
                   ,csl-matrix))))

          (define matrix-min
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'matrix-min "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-lambda ,base-type ,(string-append file-prefix "_min")
                   ,csl-matrix))))

          (define matrix-minmax
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'matrix-minmax "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-primitive ((,csl-matrix v))
                     ,(string-translate*
                       "#{base-type} min_out, max_out;
                        #{file-prefix}_minmax(v, &min_out, &max_out);
                        C_word *minptr = C_alloc(#{c-sizeof-type});
                        C_word *maxptr = C_alloc(#{c-sizeof-type});
                        C_word av[4] = {C_SCHEME_UNDEFINED, C_k, #{c-type-constructor}(&minptr, min_out), #{c-type-constructor}(&maxptr, max_out)};
                        C_values(4,av);"
                       substitutions)))))

          (define matrix-max-index
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'matrix-max-index "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-primitive ((,csl-matrix v))
                     ,(string-translate*
                       "size_t imax, jmax;
                        #{file-prefix}_max_index(v, &imax, &jmax);
                        C_word *iptr = C_alloc(C_SIZEOF_FIX_BIGNUM);
                        C_word *jptr = C_alloc(C_SIZEOF_FIX_BIGNUM);
                        C_word av[4] = {C_SCHEME_UNDEFINED, C_k, C_int_to_num(&iptr, imax), C_int_to_num(&jptr, jmax)};
                        C_values(4,av);"
                       substitutions)))))

          (define matrix-min-index
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'matrix-min-index "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-primitive ((,csl-matrix v))
                     ,(string-translate*
                       "size_t imin, jmin;
                        #{file-prefix}_min_index(v, &imin, &jmin);
                        C_word *iptr = C_alloc(C_SIZEOF_FIX_BIGNUM);
                        C_word *jptr = C_alloc(C_SIZEOF_FIX_BIGNUM);
                        C_word av[4] = {C_SCHEME_UNDEFINED, C_k, C_int_to_num(&iptr, imin), C_int_to_num(&jptr, jmin)};
                        C_values(4,av);"
                       substitutions)))))

          (define matrix-minmax-index
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'matrix-min-index "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-primitive ((,csl-matrix v))
                     ,(string-translate*
                       "size_t imin, jmin, imax, jmax;
                        #{file-prefix}_minmax_index(v, &imin, &jmin, &imax, &jmax);
                        C_word *iminptr = C_alloc(C_SIZEOF_FIX_BIGNUM);
                        C_word *jminptr = C_alloc(C_SIZEOF_FIX_BIGNUM);
                        C_word *imaxptr = C_alloc(C_SIZEOF_FIX_BIGNUM);
                        C_word *jmaxptr = C_alloc(C_SIZEOF_FIX_BIGNUM);
                        C_word av[6] = {C_SCHEME_UNDEFINED, C_k,
                                        C_int_to_num(&iminptr, imin), C_int_to_num(&jminptr, jmin),
                                        C_int_to_num(&imaxptr, imax), C_int_to_num(&jmaxptr, jmax)};
                        C_values(6,av);"
                       substitutions)))))

          ;;; Matrix properties
          (define matrix-isnull?
            (foreign-lambda bool ,(string-append file-prefix "_isnull") ,csl-matrix))
          (define matrix-ispos?
            (foreign-lambda bool ,(string-append file-prefix "_ispos") ,csl-matrix))
          (define matrix-isneg?
            (foreign-lambda bool ,(string-append file-prefix "_isneg") ,csl-matrix))
          (define matrix-isnonneg?
            (foreign-lambda bool ,(string-append file-prefix "_isnonneg") ,csl-matrix))
          (define matrix-equal?
            (foreign-safe-lambda bool ,(string-append file-prefix "_equal") ,csl-matrix ,csl-matrix)))))))

(make-matrix-module gsl.matrix.char "gsl_matrix_char" byte)
(make-matrix-module gsl.matrix.complex.double "gsl_matrix_complex" complex)
(make-matrix-module gsl.matrix.complex.float "gsl_matrix_complex_float" complex-float)
(make-matrix-module gsl.matrix.double "gsl_matrix" double)
(make-matrix-module gsl.matrix.float "gsl_matrix_float" float)
(make-matrix-module gsl.matrix.int "gsl_matrix_int" int)
(make-matrix-module gsl.matrix.long "gsl_matrix_long" long)
(make-matrix-module gsl.matrix.short "gsl_matrix_short" short)
(make-matrix-module gsl.matrix.uchar "gsl_matrix_uchar" unsigned_byte)
(make-matrix-module gsl.matrix.uint "gsl_matrix_uint" unsigned_int)
(make-matrix-module gsl.matrix.ulong "gsl_matrix_ulong" unsigned_long)
(make-matrix-module gsl.matrix.ushort "gsl_matrix_ushort" unsigned_short)
