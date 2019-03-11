(import-for-syntax (only chicken.format format)
                   (only chicken.irregex irregex-replace/all)
                   (only matchable match))

(define-syntax make-vector-module
  (er-macro-transformer
   (lambda (e i compare)
     (let* ((module-name (cadr e))
            (file-prefix (caddr e))
            (base-type (cadddr e)))
       `(module ,module-name (vector?
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
                              vector-fread
                              vector-fprintf
                              vector-fscanf
                              vector-subvector
                              vector-subvector-with-stride
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
          (import (except scheme vector-set! vector vector?)
                  bind
                  chicken.foreign
                  (only chicken.base include add1 warning define-record-type)
                  (only chicken.gc set-finalizer!)
                  (only chicken.file file-exists?)
                  (only chicken.file.posix file-size)
                  (only chicken.io read-list)
                  (only miscmacros ensure)
                  (only matchable match))

          (include "csl-error.scm")

          (foreign-declare ,(format "#include <gsl/~a.h>"
                                    (match file-prefix
                                      ("gsl_vector_complex" "gsl_vector_complex_double")
                                      ("gsl_vector" "gsl_vector_double")
                                      (else file-prefix))))

          (include "bind-transformers.scm")

          (bind-options default-renaming: "")

          (bind-rename/pattern ,(irregex-replace/all "_" (format "^~a" file-prefix) "-")
                               "vector")

          (define-record-type vector
            (ptr->vector ptr)
            vector?
            (ptr vector->ptr))

          (bind-type
           csl_vector
           (c-pointer ,file-prefix)
           vector->ptr
           ptr->vector)

          (bind* ,(format "size_t ~a_size (csl_vector v) {" file-prefix)
                 "return v->size;"
                 "}")

          (bind-rename ,(string-append file-prefix "_alloc") "%vector-alloc")
          (bind-rename ,(string-append file-prefix "_calloc") "%vector-calloc")

          ;;; Vector Allocation
          (bind ,(format "csl_vector ~a_alloc (size_t)" file-prefix))
          (bind ,(format "csl_vector ~a_calloc (size_t)" file-prefix))
          (bind-rename ,(string-append file-prefix "_free") "vector-free!")
          (bind ,(format "void ~a_free(csl_vector)" file-prefix))

          (define (vector-alloc n)
            (set-finalizer! (%vector-alloc n) vector-free!))

          (define (vector-calloc n)
            (set-finalizer! (%vector-calloc n) vector-free!))

          ;;; Accessing vector elements
          (bind ,(format "___safe ~a ~a_get(csl_vector, const size_t)" base-type file-prefix))

          (bind-rename ,(string-append file-prefix "_set") "vector-set!")
          (bind ,(format "___safe void ~a_set(csl_vector, const size_t, ~a)" file-prefix base-type))
          ;; gsl_vector_ptr omitted

          ;;; Initializing vector elements
          (bind-rename ,(string-append file-prefix "_set_all") "vector-set-all!")
          (bind ,(format "void ~a_set_all(csl_vector, ~a)" file-prefix base-type))
          (bind-rename ,(string-append file-prefix "_set_zero") "vector-set-zero!")
          (bind ,(format "void ~a_set_zero(csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_set_basis") "vector-set-basis!")
          (bind ,(format "___safe void ~a_set_basis(csl_vector, size_t)" file-prefix))

          ;;; Reading and writing vectors
          (bind-opaque-type cfile (c-pointer "FILE"))
          (bind "cfile fopen(char *, char *)")
          (bind "cfile stdout")
          (bind "int fclose(cfile)")
          (bind-rename ,(string-append file-prefix "_fwrite") "%vector-fwrite")
          (bind ,(format "int ~a_fwrite(cfile, csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_fread") "%vector-fread")
          (bind ,(format "int ~a_fread(cfile, csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_fprintf") "%vector-fprintf")
          (bind ,(format "int ~a_fprintf(cfile, csl_vector, char *)" file-prefix))
          (bind-rename ,(string-append file-prefix "_fscanf") "%vector-fscanf")
          (bind ,(format "int ~a_fscanf(cfile, csl_vector)" file-prefix))

          (define (vector-fwrite filename vector)
            (ensure string? filename "not a valid string filename" filename)
            (let ((f (fopen filename "w")))
              (%vector-fwrite f vector)
              (fclose f)))

          (define (vector-fread filename)
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
            (let ((vector (vector-alloc (/ (file-size filename) type-size)))
                  (f (fopen filename "r")))
              (%vector-fread f vector)
              (fclose f)
              vector))

          (define (vector-fprintf filename vector format)
            (if (and (boolean? filename)
                     filename)
                (%vector-fprintf (stdout) vector format)
                (begin
                  (ensure string? filename "not a valid filename" filename)
                  (let ((f (fopen filename "w")))
                    (%vector-fprintf f vector format)
                    (fclose f)))))

          (define (vector-fscanf filename)
            (ensure file-exists? filename "file does not exist" filename)
            (define size (length (call-with-input-file filename read-list)))
            (let* ((vector (vector-alloc size))
                   (f (fopen filename "r")))
              (%vector-fscanf f vector)
              (fclose f)
              vector))

          ;;; Vector views
          (bind ,(format "___safe struct ~a_view ~a_subvector(csl_vector, size_t, size_t)" file-prefix file-prefix))
          (bind ,(format "___safe struct ~a_view ~a_subvector_with_stride(csl_vector, size_t, size_t, size_t)" file-prefix file-prefix))

          (define (vector-real v)
            (let* ((size (vector-size v))
                   (newv (vector-alloc size)))
              (do ((i 0 (add1 i)))
                  ((= i size) newv)
                (vector-set! newv i (real-part (vector-get v i))))))

          (define (vector-imag v)
            (let* ((size (vector-size v))
                   (newv (vector-alloc size)))
              (do ((i 0 (add1 i)))
                  ((= i size) newv)
                (vector-set! newv i (imag-part (vector-get v i))))))

          ;; gsl_vector_view_array and view_array_with_stride omitted

          ;;; Copying vectors
          (bind-rename ,(string-append file-prefix "_memcpy") "vector-memcpy!")
          (bind ,(format "___safe int ~a_memcpy(csl_vector, csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_swap") "vector-swap!")
          (bind ,(format "___safe int ~a_swap(csl_vector, csl_vector)" file-prefix))

          ;;; Exchanging elements
          (bind-rename ,(string-append file-prefix "_swap_elements") "vector-swap-elements!")
          (bind ,(format "___safe int ~a_swap_elements(csl_vector, size_t, size_t)" file-prefix))
          (bind-rename ,(string-append file-prefix "_reverse") "vector-reverse!")
          (bind ,(format "int ~a_reverse(csl_vector)" file-prefix))

          ;;; Vector operations
          (bind-rename ,(string-append file-prefix "_add") "vector-add!")
          (bind ,(format "___safe int ~a_add(csl_vector, csl_vector)" file-prefix))

          (bind-rename ,(string-append file-prefix "_sub") "vector-sub!")
          (bind ,(format "___safe int ~a_sub(csl_vector, csl_vector)" file-prefix))

          (bind-rename ,(string-append file-prefix "_mul") "vector-mul!")
          (bind ,(format "___safe int ~a_mul(csl_vector, csl_vector)" file-prefix))

          (bind-rename ,(string-append file-prefix "_div") "vector-div!")
          (bind ,(format "___safe int ~a_div(csl_vector, csl_vector)" file-prefix))

          (bind-rename ,(string-append file-prefix "_scale") "vector-scale!")
          (bind ,(format "int ~a_scale(csl_vector, ~a)" file-prefix base-type))

          (bind-rename ,(string-append file-prefix "_add_constant") "vector-add-constant!")
          (bind ,(format "int ~a_add_constant(csl_vector, ~a)" file-prefix base-type))

          ;;; Maximum and minimum
          (define (vector-max v)
            (let ((size (vector-size v))
                  (first (vector-get v 0)))
              (let loop ((i 0)
                         (res first))
                (if (= i size)
                    res
                    (loop (add1 i) (max res (vector-get v i)))))))

          (define (vector-min v)
            (let ((size (vector-size v))
                  (first (vector-get v 0)))
              (let loop ((i 0)
                         (res first))
                (if (= i size)
                    res
                    (loop (add1 i) (min res (vector-get v i)))))))

          (define (vector-minmax v)
            (let ((size (vector-size v))
                  (first (vector-get v 0)))
              (let loop ((i 0)
                         (maxres first)
                         (minres first))
                (if (= i size)
                    (values minres maxres)
                    (let ((cur (vector-get v i)))
                      (loop (add1 i) (max maxres cur) (min minres cur)))))))

          (define (vector-max-index v)
            (let ((size (vector-size v))
                  (first (vector-get v 0)))
              (let loop ((i 0)
                         (maxind 0)
                         (maxval first))
                (if (= i size)
                    maxind
                    (let* ((currval (vector-get v i))
                           (bigger (> currval maxval)))
                      (loop (add1 i)
                            (if bigger i maxind)
                            (if bigger currval maxval)))))))

          (define (vector-min-index v)
            (let ((size (vector-size v))
                  (first (vector-get v 0)))
              (let loop ((i 0)
                         (minind 0)
                         (minval first))
                (if (= i size)
                    minind
                    (let* ((currval (vector-get v i))
                           (smaller (< currval minval)))
                      (loop (add1 i)
                            (if smaller i minind)
                            (if smaller currval minval)))))))

          (define (vector-minmax-index v)
            (let ((size (vector-size v))
                  (first (vector-get v 0)))
              (let loop ((i 0)
                         (minind 0)
                         (minval first)
                         (maxind 0)
                         (maxval first))
                (if (= i size)
                    (values minind maxind)
                    (let* ((currval (vector-get v i))
                           (smaller (< currval minval))
                           (bigger (> currval maxval)))
                      (loop (add1 i)
                            (if smaller i minind)
                            (if smaller currval minval)
                            (if bigger i maxind)
                            (if bigger currval maxval)))))))

          ;;; Vector properties
          (bind-rename ,(string-append file-prefix "_isnull") "vector-isnull?")
          (bind ,(format "bool ~a_isnull(csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_ispos") "vector-ispos?")
          (bind ,(format "bool ~a_ispos(csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_isneg") "vector-isneg?")
          (bind ,(format "bool ~a_isneg(csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_isnonneg") "vector-isnonneg?")
          (bind ,(format "bool ~a_isnonneg(csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_equal") "vector-equal?")
          (bind ,(format "___safe bool ~a_equal(csl_vector, csl_vector)" file-prefix))


          )))))
