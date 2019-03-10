(import-for-syntax (only chicken.format format)
                   (only chicken.irregex irregex-replace irregex-replace/all)
                   (only matchable match))

(define-syntax make-matrix-module
  (er-macro-transformer
   (lambda (e i compare)
     (let* ((module-name (cadr e))
            (vector-module-name
             (string->symbol
              (irregex-replace "matrix"
                               (symbol->string module-name)
                               "vector")))
            (file-prefix (caddr e))
            (vector-file-prefix
             (irregex-replace "matrix" file-prefix "vector"))
            (base-type (cadddr e)))
       `(module ,module-name (matrix?
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
                              matrix-fread
                              matrix-fprintf
                              matrix-fscanf
                              matrix-submatrix
                              ;; matrix-submatrix-with-stride
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
                  bind
                  chicken.foreign
                  (only chicken.base include add1 warning define-record-type)
                  (only chicken.gc set-finalizer!)
                  (only chicken.file file-exists?)
                  (only chicken.file.posix file-size)
                  (only chicken.io read-list)
                  (only miscmacros ensure)
                  (only matchable match)
                  ,vector-module-name)

          (include "csl-error.scm")

          (foreign-declare ,(format "#include <gsl/~a.h>"
                                    (match file-prefix
                                      ("gsl_matrix_complex" "gsl_matrix_complex_double")
                                      ("gsl_matrix" "gsl_matrix_double")
                                      (else file-prefix))))

          (foreign-declare ,(format "#include <gsl/~a.h>"
                                    (match vector-file-prefix
                                      ("gsl_vector_complex" "gsl_vector_complex_double")
                                      ("gsl_vector" "gsl_vector_double")
                                      (else file-prefix))))


          (include "bind-transformers.scm")

          (bind-options default-renaming: "")
          (bind-rename/pattern ,(irregex-replace/all "_" (format "^~a" file-prefix) "-")
                               "matrix")

          (define-record-type matrix
            (ptr->matrix ptr)
            matrix?
            (ptr matrix->ptr))

          (bind-type
           csl_matrix
           (c-pointer ,file-prefix)
           matrix->ptr
           ptr->matrix)

          (bind-type
           csl_vector
           (c-pointer ,vector-file-prefix)
           vector->ptr
           ptr->vector)

          (bind* ,(format "size_t ~a_size1 (csl_matrix m) {" file-prefix)
                 "return m->size1;"
                 "}")

          (bind* ,(format "size_t ~a_size2 (csl_matrix m) {" file-prefix)
                 "return m->size2;"
                 "}")

          (bind-rename ,(string-append file-prefix "_alloc") "%matrix-alloc")
          (bind-rename ,(string-append file-prefix "_calloc") "%matrix-calloc")

          ;;; Matrix Allocation
          (bind ,(format "csl_matrix ~a_alloc (size_t, size_t)" file-prefix))
          (bind ,(format "csl_matrix ~a_calloc (size_t, size_t)" file-prefix))
          (bind-rename ,(string-append file-prefix "_free") "matrix-free!")
          (bind ,(format "void ~a_free(csl_matrix)" file-prefix))

          (define (matrix-alloc n1 n2)
            (set-finalizer! (%matrix-alloc n1 n2) matrix-free!))

          (define (matrix-calloc n1 n2)
            (set-finalizer! (%matrix-calloc n1 n2) matrix-free!))

          ;;; Accessing matrix elements
          (bind ,(format "~a ~a_get(csl_matrix, const size_t, const size_t)" base-type file-prefix))
          (bind-rename ,(string-append file-prefix "_set") "matrix-set!")
          (bind ,(format "void ~a_set(csl_matrix, const size_t, const size_t, ~a)" file-prefix base-type))
          ;; gsl_matrix_ptr omitted

          ;;; Initializing matrix elements
          (bind-rename ,(string-append file-prefix "_set_all") "matrix-set-all!")
          (bind ,(format "void ~a_set_all(csl_matrix, ~a)" file-prefix base-type))
          (bind-rename ,(string-append file-prefix "_set_zero") "matrix-set-zero!")
          (bind ,(format "void ~a_set_zero(csl_matrix)" file-prefix))
          (bind-rename ,(string-append file-prefix "_set_identity") "matrix-set-identity!")
          (bind ,(format "void ~a_set_identity(csl_matrix)" file-prefix))

          ;;; Reading and writing matrices
          (bind-opaque-type cfile (c-pointer "FILE"))
          (bind "cfile fopen(char *, char *)")
          (bind "cfile stdout")
          (bind "int fclose(cfile)")
          (bind-rename ,(string-append file-prefix "_fwrite") "%matrix-fwrite")
          (bind ,(format "int ~a_fwrite(cfile, csl_matrix)" file-prefix))
          (bind-rename ,(string-append file-prefix "_fread") "%matrix-fread")
          (bind ,(format "int ~a_fread(cfile, csl_matrix)" file-prefix))
          (bind-rename ,(string-append file-prefix "_fprintf") "%matrix-fprintf")
          (bind ,(format "int ~a_fprintf(cfile, csl_matrix, char *)" file-prefix))
          (bind-rename ,(string-append file-prefix "_fscanf") "%matrix-fscanf")
          (bind ,(format "int ~a_fscanf(cfile, csl_matrix)" file-prefix))

          (define (matrix-fwrite filename matrix)
            (ensure string? filename "not a valid string filename" filename)
            (let ((f (fopen filename "w")))
              (%matrix-fwrite f matrix)
              (fclose f)))

          (define (matrix-fread filename n1 n2)
            (ensure file-exists? filename "file does not exist" filename)
            (define type-size
              (let ((double-size (foreign-value "sizeof(double)" size_t))
                    (float-size (foreign-value "sizeof(float)" size_t))
                    (long-size (foreign-value "sizeof(long)" size_t))
                    (int-size (foreign-value "sizeof(int)" size_t))
                    (short-size (foreign-value "sizeof(short)" size_t))
                    (char-size (foreign-value "sizeof(char)" size_t)))
                (match ,base-type
                  ("struct gsl_complex" (* 2 double-size))
                  ("struct gsl_complex_float" (* 2 float-size))
                  ("double" double-size)
                  ("float" float-size)
                  ("long" long-size)
                  ("int" int-size)
                  ("short" short-size)
                  ("___byte" char-size)
                  ("unsigned long" long-size)
                  ("unsigned int" int-size)
                  ("unsigned short" short-size)
                  ("unsigned ___byte" char-size)
                  (else
                   (warning "Unmatched base type - may not read vector correctly.")
                   8))))
            (let ((res-size (* n1 n2))
                  (fmat-size (/ (file-size filename) type-size)))
              (cond ((> res-size fmat-size)
                     (warning "file appears to have fewer elements than specified" fmat-size res-size))
                    ((< res-size fmat-size)
                     (warning "file appears to have more elements than specified" fmat-size res-size))))
            (let ((matrix (matrix-alloc n1 n2))
                  (f (fopen filename "r")))
              (%matrix-fread f matrix)
              (fclose f)
              matrix))

          (define (matrix-fprintf filename matrix format)
            (if (and (boolean? filename)
                     filename)
                (%matrix-fprintf (stdout) matrix format)
                (begin
                  (ensure string? filename "not a valid filename" filename)
                  (let ((f (fopen filename "w")))
                    (%matrix-fprintf f matrix format)
                    (fclose f)))))

          (define (matrix-fscanf filename n1 n2)
            (ensure file-exists? filename "file does not exist" filename)
            (define fmat-size (length (call-with-input-file filename read-list)))
            (let ((res-size (* n1 n2)))
              (cond ((> res-size fmat-size)
                     (warning "file appears to have fewer elements than specified" fmat-size res-size))
                    ((< res-size fmat-size)
                     (warning "file appears to have more elements than specified" fmat-size res-size))))
            (let* ((matrix (matrix-alloc n1 n2))
                   (f (fopen filename "r")))
              (%matrix-fscanf f matrix)
              (fclose f)
              matrix))

          ;;; Matrix views
          (bind ,(format "struct ~a_view ~a_submatrix(csl_matrix, size_t, size_t, size_t, size_t)" file-prefix file-prefix))

          ;; ;; matrix-view-array omitted
          ;; ;; matrix-const-view-array omitted
          ;; ;; matrix-view-array-with-tda omitted
          ;; ;; matrix-const-view-array-with-tda omitted
          ;; ;; matrix-const-submatrix omitted

          (bind ,(format "struct ~a_view ~a_view_vector(csl_vector, size_t, size_t)" file-prefix file-prefix))

          ;; ;; added for convenience
          ;; (define (matrix-submatrix-with-stride m k1 k2 s1 s2 n1 n2)
          ;;   (tag-pointer
          ;;    (set-finalizer!
          ;;     ((foreign-safe-lambda* gsl_matrix ((gsl_matrix m)
          ;;                                        (unsigned-int k1)
          ;;                                        (unsigned-int k2)
          ;;                                        (unsigned-int s1)
          ;;                                        (unsigned-int s2)
          ;;                                        (unsigned-int n1)
          ;;                                        (unsigned-int n2))
          ;;        ,(format "~a *r = ~a_alloc(n1,n2);" file-prefix file-prefix)
          ;;        "int rm, rr;"
          ;;        "for (rm = k1, rr = 0; rr < n1; rr++, rm += s1){"
          ;;        ,(format "gsl_vector~a *row = gsl_vector~a_alloc(m->size2);" type-suffix type-suffix)
          ;;        ,(format "~a_get_row(row,m,rm);" file-prefix)
          ;;        ,(format "gsl_vector~a_view row_with_stride = gsl_vector~a_subvector_with_stride(row,k2,s2,n2);" type-suffix type-suffix)
          ;;        ,(format "gsl_matrix~a_set_row(r,rr,&row_with_stride.vector);" type-suffix)
          ;;        ,(format "gsl_vector~a_free(row);" type-suffix)
          ;;        "}"
          ;;        "C_return(r);")
          ;;      m k1 k2 s1 s2 n1 n2)
          ;;     matrix-free!)
          ;;    ',module-name))

          ;; ;; matrix-const-view-vector omitted
          ;; ;; matrix-view-vector-with-tda omitted
          ;; ;; matrix-const-view-vector-with-tda ommitted

          ;;; Creating row and column views
          (bind ,(format "struct ~a_view ~a_row(csl_matrix, size_t)" vector-file-prefix file-prefix))
          (bind ,(format "struct ~a_view ~a_column(csl_matrix, size_t)" vector-file-prefix file-prefix))
          (bind ,(format "struct ~a_view ~a_diagonal(csl_matrix)" vector-file-prefix file-prefix))
          (bind ,(format "struct ~a_view ~a_subrow(csl_matrix, size_t, size_t, size_t)" vector-file-prefix file-prefix))
          (bind ,(format "struct ~a_view ~a_subcolumn(csl_matrix, size_t, size_t, size_t)" vector-file-prefix file-prefix))
          (bind ,(format "struct ~a_view ~a_subdiagonal(csl_matrix, size_t)" vector-file-prefix file-prefix))
          (bind ,(format "struct ~a_view ~a_superdiagonal(csl_matrix, size_t)" vector-file-prefix file-prefix))

          ;;; Copying matrices
          (bind-rename ,(string-append file-prefix "_memcpy") "matrix-memcpy!")
          (bind ,(format "int ~a_memcpy(csl_matrix, csl_matrix)" file-prefix))
          (bind-rename ,(string-append file-prefix "_swap") "matrix-swap!")
          (bind ,(format "int ~a_swap(csl_matrix, csl_matrix)" file-prefix))

          ;;; Copying rows and columns
          (bind-rename ,(string-append file-prefix "_get_row") "matrix-get-row!")
          (bind ,(format "int ~a_get_row(csl_vector, csl_matrix, size_t)" file-prefix))
          (bind-rename ,(string-append file-prefix "_get_col") "matrix-get-col!")
          (bind ,(format "int ~a_get_col(csl_vector, csl_matrix, size_t)" file-prefix))

          (bind-rename ,(string-append file-prefix "_set_row") "matrix-set-row!")
          (bind ,(format "int ~a_set_row(csl_matrix, size_t, csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_set_col") "matrix-set-col!")
          (bind ,(format "int ~a_set_col(csl_matrix, size_t, csl_vector)" file-prefix))

          ;;; Exchanging rows an dcolumns
          (bind-rename ,(string-append file-prefix "_swap_rows") "matrix-swap-rows!")
          (bind ,(format "int ~a_swap_rows(csl_matrix, size_t, size_t)" file-prefix))

          (bind-rename ,(string-append file-prefix "_swap_columns") "matrix-swap-columns!")
          (bind ,(format "int ~a_swap_columns(csl_matrix, size_t, size_t)" file-prefix))

          (bind-rename ,(string-append file-prefix "_swap_rowcol") "matrix-swap-rowcol!")
          (bind ,(format "int ~a_swap_rowcol(csl_matrix, size_t, size_t)" file-prefix))

          (bind-rename ,(string-append file-prefix "_transpose_memcpy") "matrix-transpose-memcpy!")
          (bind ,(format "int ~a_transpose_memcpy(csl_matrix, csl_matrix)" file-prefix))

          (bind-rename ,(string-append file-prefix "_transpose") "matrix-transpose!")
          (bind ,(format "int ~a_transpose(csl_matrix)" file-prefix))

          ;;; Matrix-operations
          (bind-rename ,(string-append file-prefix "_add") "matrix-add!")
          (bind ,(format "int ~a_add(csl_matrix, csl_matrix)" file-prefix))

          (bind-rename ,(string-append file-prefix "_sub") "matrix-sub!")
          (bind ,(format "int ~a_sub(csl_matrix, csl_matrix)" file-prefix))

          (bind-rename ,(string-append file-prefix "_mul_elements") "matrix-mul-elements!")
          (bind ,(format "int ~a_mul_elements(csl_matrix, csl_matrix)" file-prefix))

          (bind-rename ,(string-append file-prefix "_div_elements") "matrix-div-elements!")
          (bind ,(format "int ~a_div_elements(csl_matrix, csl_matrix)" file-prefix))

          (bind-rename ,(string-append file-prefix "_scale") "matrix-scale!")
          (bind ,(format "int ~a_scale(csl_matrix, ~a)" file-prefix base-type))

          (bind-rename ,(string-append file-prefix "_add_constant") "matrix-add-constant!")
          (bind ,(format "int ~a_add_constant(csl_matrix, ~a)" file-prefix base-type))

          ;;; Finding max and min elements of matrixs
          (define (matrix-max m)
            (let ((size1 (matrix-size1 m))
                  (size2 (matrix-size2 m))
                  (first (matrix-get m 0 0)))
              (let loop ((i 0)
                         (j 0)
                         (res first))
                (cond ((= i size1)
                       res)
                      ((= j size2)
                       (loop (add1 i) 0 res))
                      (else
                       (loop i (add1 j) (max res (matrix-get m i j))))))))

          (define (matrix-min m)
            (let ((size1 (matrix-size1 m))
                  (size2 (matrix-size2 m))
                  (first (matrix-get m 0 0)))
              (let loop ((i 0)
                         (j 0)
                         (res first))
                (cond ((= i size1)
                       res)
                      ((= j size2)
                       (loop (add1 i) 0 res))
                      (else
                       (loop i (add1 j) (min res (matrix-get m i j))))))))

          (define (matrix-minmax m)
            (let ((size1 (matrix-size1 m))
                  (size2 (matrix-size2 m))
                  (first (matrix-get m 0 0)))
              (let loop ((i 0)
                         (j 0)
                         (maxres first)
                         (minres first))
                (cond ((= i size1)
                       (values minres maxres))
                      ((= j size2)
                       (loop (add1 i) 0 maxres minres))
                      (else
                       (loop i
                             (add1 j)
                             (max maxres (matrix-get m i j))
                             (min maxres (matrix-get m i j))))))))

          (define (matrix-max-index m)
            (let ((size1 (matrix-size1 m))
                  (size2 (matrix-size2 m))
                  (first (matrix-get m 0 0)))
              (let loop ((i 0)
                         (j 0)
                         (maxind1 0)
                         (maxind2 0)
                         (maxval first))
                (cond ((= i size1)
                       (values maxind1 maxind2))
                      ((= j size2)
                       (loop (add1 i) 0 maxind1 maxind2 maxval))
                      (else
                       (let* ((currval (matrix-get m i j))
                              (bigger (> currval maxval)))
                         (loop i
                               (add1 j)
                               (if bigger i maxind1)
                               (if bigger j maxind2)
                               (if bigger currval maxval))))))))

          (define (matrix-min-index m)
            (let ((size1 (matrix-size1 m))
                  (size2 (matrix-size2 m))
                  (first (matrix-get m 0 0)))
              (let loop ((i 0)
                         (j 0)
                         (minind1 0)
                         (minind2 0)
                         (minval first))
                (cond ((= i size1)
                       (values minind1 minind2))
                      ((= j size2)
                       (loop (add1 i) 0 minind1 minind2 minval))
                      (else
                       (let* ((currval (matrix-get m i j))
                              (smaller (< currval minval)))
                         (loop i
                               (add1 j)
                               (if smaller i minind1)
                               (if smaller j minind2)
                               (if smaller currval minval))))))))

          (define (matrix-minmax-index m)
            (let ((size1 (matrix-size1 m))
                  (size2 (matrix-size2 m))
                  (first (matrix-get m 0 0)))
              (let loop ((i 0)
                         (j 0)
                         (minind1 0)
                         (minind2 0)
                         (minval first)
                         (maxind1 0)
                         (maxind2 0)
                         (maxval first))
                (cond ((= i size1)
                       (values minind1 minind2 maxind1 maxind2))
                      ((= j size2)
                       (loop (add1 i) 0 minind1 minind2 minval maxind1 maxind2 maxval))
                      (else
                       (let* ((currval (matrix-get m i j))
                              (smaller (< currval minval))
                              (bigger (> currval maxval)))
                         (loop i
                               (add1 j)
                               (if smaller i minind1)
                               (if smaller j minind2)
                               (if smaller currval minval)
                               (if bigger i maxind1)
                               (if bigger j maxind2)
                               (if bigger currval maxval))))))))

          ;;; Matrix properties
          (bind-rename ,(string-append file-prefix "_isnull") "matrix-isnull?")
          (bind ,(format "bool ~a_isnull(csl_matrix)" file-prefix))
          (bind-rename ,(string-append file-prefix "_ispos") "matrix-ispos?")
          (bind ,(format "bool ~a_ispos(csl_matrix)" file-prefix))
          (bind-rename ,(string-append file-prefix "_isneg") "matrix-isneg?")
          (bind ,(format "bool ~a_isneg(csl_matrix)" file-prefix))
          (bind-rename ,(string-append file-prefix "_isnonneg") "matrix-isnonneg?")
          (bind ,(format "bool ~a_isnonneg(csl_matrix)" file-prefix))
          (bind-rename ,(string-append file-prefix "_equal") "matrix-equal?")
          (bind ,(format "bool ~a_equal(csl_matrix, csl_matrix)" file-prefix)))))))
