(import-for-syntax (only chicken.format format)
                   (only chicken.base warning)
                   (only chicken.irregex irregex-replace/all)
                   (only matchable match))

(define-syntax make-vector-module
  (er-macro-transformer
   (lambda (e i compare)
     (let* ((module-name (cadr e))
            (file-prefix (caddr e))
            (base-type (cadddr e)))
       `(module ,module-name (vector-size
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
                              ;; vector-max-index
                              ;; vector-min-index
                              vector-isnull?
                              vector-ispos?
                              vector-isneg?
                              vector-isnonneg?
                              vector-equal?)
          (import (except scheme vector-set!)
                  bind
                  chicken.foreign
                  ;; (only chicken.locative make-locative)
                  (only chicken.syntax begin-for-syntax)
                  (only chicken.base include-relative add1)
                  (only chicken.gc set-finalizer!)
                  (only chicken.file file-exists?)
                  (only chicken.file.posix file-size)
                  (only miscmacros ensure))

          (include-relative "../csl-error.scm")

          (foreign-declare ,(format "#include <gsl/~a.h>"
                                    (if (string=? file-prefix "gsl_vector_complex")
                                        "gsl_vector_complex_double"
                                        file-prefix)))

          (include-relative "../bind-transformers.scm")

          ;; (begin-for-syntax
          ;;   (define ,(symbol-append module-name '|#| 'gsl-arg-transformer*)
          ;;     gsl-arg-transformer*))
          ;; (bind-options default-renaming: ""
          ;;               foreign-transformer: ,(symbol-append module-name '|#| 'gsl-arg-transformer*))
          (bind-options default-renaming: ""
                        foreign-transformer: foo#gsl-arg-transformer*)

          (bind-rename/pattern ,(irregex-replace/all "_" (format "^~a" file-prefix) "-")
                               "vector")

          (bind-opaque-type
           csl_vector
           ;; ,(symbol-append
           ;;   csl_vector_
           ;;   (if (irregex-match "^struct.*" base-type)
           ;;       'complex_
           ;;       '||)
           ;;   (cond ((irregex-match "^struct .*_float" base-type)
           ;;          'float)
           ;;         ((irregex-match "^struct .*" base-type)
           ;;          'double)
           ;;         (else (string->symbol base-type))))
           (c-pointer ,file-prefix))

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
          (bind ,(format "~a ~a_get(csl_vector, const size_t)" base-type file-prefix))
          (bind-rename ,(string-append file-prefix "_set") "vector-set!")
          (bind ,(format "void ~a_set(csl_vector, const size_t, ~a)" file-prefix base-type))
          ;; gsl_vector_ptr omitted

          ;;; Initializing vector elements
          (bind-rename ,(string-append file-prefix "_set_all") "vector-set-all!")
          (bind ,(format "void ~a_set_all(csl_vector, ~a)" file-prefix base-type))
          (bind-rename ,(string-append file-prefix "_set_zero") "vector-set-zero!")
          (bind ,(format "void ~a_set_zero(csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_set_basis") "vector-set-basis!")
          (bind ,(format "void ~a_set_basis(csl_vector, size_t)" file-prefix))

          ;;; Reading and writing vectors
          (bind-opaque-type cfile (c-pointer "FILE"))
          (bind "cfile fopen(char *, char *)")
          (bind "int fclose(cfile)")
          (bind-rename ,(string-append file-prefix "_fwrite") "%vector-fwrite")
          (bind ,(format "int ~a_fwrite(cfile, csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_fread") "%vector-fread")
          (bind ,(format "int ~a_fread(cfile, csl_vector)" file-prefix))

          (define (vector-fwrite filename vector)
            (let ((f (fopen filename "w")))
              (%vector-fwrite f vector)
              (fclose f)))

          (define (vector-fread filename)
            (ensure file-exists? filename "file does not exist" filename)
            (define size
              ,(match base-type
                 ("float" 4)
                 ("double" 8)
                 ("struct gsl_complex" 16)
                 (else
                  (warning "Unmatched base type - may not read vector correctly.")
                  8)))
            (let* ((vector (vector-alloc (/ (file-size filename) size)))
                   (f (fopen filename "r")))
              (%vector-fread f vector)
              (fclose f)
              vector))

          ;;; Vector views
          (bind ,(format "struct ~a_view ~a_subvector(csl_vector, size_t, size_t)" file-prefix file-prefix))
          (bind ,(format "struct ~a_view ~a_subvector_with_stride(csl_vector, size_t, size_t, size_t)" file-prefix file-prefix))

          (define (vector-real v)
            (let* ((size (vector-size v))
                   (newv (vector-alloc size)))
              (do ((i 0 (add1 i)))
                  ((= i size) newv)
                (vector-set! newv i (real-part (vector-ref v i))))))
          (define (vector-imag v)
            (let* ((size (vector-size v))
                   (newv (vector-alloc size)))
              (do ((i 0 (add1 i)))
                  ((= i size) newv)
                (vector-set! newv i (imag-part (vector-ref v i))))))

          ;; gsl_vector_view_array and view_array_with_stride omitted

          ;;; Copying vectors
          (bind-rename ,(string-append file-prefix "_memcpy") "vector-memcpy!")
          (bind ,(format "int ~a_memcpy(csl_vector, csl_vector)" file-prefix))
          (bind-rename ,(string-append file-prefix "_swap") "vector-swap!")
          (bind ,(format "int ~a_swap(csl_vector, csl_vector)" file-prefix))

          ;;; Exchanging elements
          (bind-rename ,(string-append file-prefix "_swap_elements") "vector-swap-elements!")
          (bind ,(format "int ~a_swap_elements(csl_vector, size_t, size_t)" file-prefix))
          (bind-rename ,(string-append file-prefix "_reverse") "vector-reverse!")
          (bind ,(format "int ~a_reverse(csl_vector)" file-prefix))

          ;;; Vector operations
          (bind-rename ,(string-append file-prefix "_add") "vector-add!")
          (bind ,(format "int ~a_add(csl_vector, csl_vector)" file-prefix))

          (bind-rename ,(string-append file-prefix "_sub") "vector-sub!")
          (bind ,(format "int ~a_sub(csl_vector, csl_vector)" file-prefix))

          (bind-rename ,(string-append file-prefix "_mul") "vector-mul!")
          (bind ,(format "int ~a_mul(csl_vector, csl_vector)" file-prefix))

          (bind-rename ,(string-append file-prefix "_div") "vector-div!")
          (bind ,(format "int ~a_div(csl_vector, csl_vector)" file-prefix))

          (bind-rename ,(string-append file-prefix "_scale") "vector-scale!")
          (bind ,(format "int ~a_scale(csl_vector, ~a)" file-prefix base-type))

          (bind-rename ,(string-append file-prefix "_add_constant") "vector-add-constant!")
          (bind ,(format "int ~a_add_constant(csl_vector, ~a)" file-prefix base-type))

          ;;; Maximum and minimum
          (define (vector-max vector)
            (let ((size (vector-size vector)))
              (let loop ((i 0)
                         (res 0))
                (if (= i size)
                    res
                    (loop (add1 i) (max res (vector-get vector i)))))))

          (define (vector-min vector)
            (let ((size (vector-size vector)))
              (let loop ((i 0)
                         (res 0))
                (if (= i size)
                    res
                    (loop (add1 i) (min res (vector-get vector i)))))))

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
          (bind ,(format "bool ~a_equal(csl_vector, csl_vector)" file-prefix))


          )))))
