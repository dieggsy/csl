(declare (unit gsl.vector))
(include "utils/syntax-utils.scm")
(include "utils/declarations.scm")

(import-for-syntax (only chicken.format format)
                   (only matchable match)
                   (only chicken.string string-translate*))

(define-syntax make-vector-module
  (er-macro-transformer
   (lambda (e rename compare)
     (let* ((module-name (cadr e))
            (file-prefix (caddr e))
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
               ("#{c-type-constructor}" . ,c-type-constructor)))
            (csl-vector (string->symbol file-prefix)))
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
                              vector-fread!
                              vector-fprintf
                              vector-fscanf!
                              vector-subvector
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
                  srfi-4
                  ;; bind
                  chicken.foreign
                  chicken.type
                  (only chicken.base
                        void
                        error
                        include
                        define-record-type
                        identity)
                  (only chicken.file file-exists?)
                  (only chicken.file.posix file-size)
                  (only chicken.pathname pathname-directory)
                  (only chicken.io read-list)
                  (only matchable match))

          (include "utils/error-handler.scm")
          (include "utils/complex-types.scm")
          (include "utils/stdio.scm")

          (foreign-declare ,(format "#include <gsl/~a.h>"
                                    (match file-prefix
                                      ("gsl_vector_complex" "gsl_vector_complex_double")
                                      ("gsl_vector" "gsl_vector_double")
                                      (else file-prefix))))

          (define-record-type vector
            (ptr->vector ptr)
            vector?
            (ptr vector->ptr))

          (define-foreign-type ,csl-vector
            (nonnull-c-pointer ,file-prefix)
            vector->ptr
            ptr->vector)

          (define vector-size
            (foreign-lambda* size_t ((,csl-vector v))
              "C_return(v->size);"))

          (define vector-alloc
            (foreign-lambda ,csl-vector ,(string-append file-prefix "_alloc") size_t))
          (define vector-calloc
            (foreign-lambda ,csl-vector ,(string-append file-prefix "_calloc") size_t))
          (define vector-free!
            (foreign-lambda void ,(string-append file-prefix "_free") ,csl-vector))

          ;;; Accessing vector elements
          (define vector-get
            ,(case base-type
               ((complex complex-float)
                `(foreign-safe-lambda* scheme-object
                     ((,csl-vector v) ((const size_t) i))
                   ,(string-translate*
                     "#{complex-ctype} z = #{file-prefix}_get(v,i);
                      C_return(scheme_make_rect(GSL_REAL(z),GSL_IMAG(z)));"
                     substitutions)))
               (else
                `(foreign-safe-lambda ,base-type
                     ,(string-append file-prefix "_get")
                   ,csl-vector (const size_t)))))

          ;; Fix a weird compiler warning?
          (: vector-set! (pointer fixnum * -> void))
          (define vector-set!
            ,(case base-type
               ((complex complex-float)
                `(foreign-safe-lambda* void ((,csl-vector v) ((const size_t) i)
                                             (,base-type z))
                   ,(string-translate*
                     "#{complex-ctype} _z;
                      GSL_SET_COMPLEX(&_z, z[0], z[1]);
                      #{file-prefix}_set(v, i, _z);"
                     substitutions)))
               (else
                `(foreign-safe-lambda void
                     ,(string-append file-prefix "_set")
                   ,csl-vector (const size_t) ,base-type))))
          ;; gsl_vector_ptr omitted

          ;;; Initializing vector elements
          (define vector-set-all!
            ,(case base-type
               ((complex complex-float)
                `(foreign-safe-lambda* void ((,csl-vector v) (,base-type z))
                   ,(string-translate*
                     "#{complex-ctype} _z;
                      GSL_SET_COMPLEX(&_z, z[0], z[1]);
                      #{file-prefix}_set_all(v, _z);"
                     substitutions)))
               (else
                `(foreign-safe-lambda void
                     ,(string-append file-prefix "_set_all")
                   ,csl-vector ,base-type))))
          (define vector-set-zero!
            (foreign-lambda void ,(string-append file-prefix "_set_zero")
              ,csl-vector))
          (define vector-set-basis!
            (foreign-safe-lambda void ,(string-append file-prefix "_set_basis")
              ,csl-vector size_t))

          ;;; Reading and writing vectors
          (define (vector-fwrite fileport vector)
            (let* ((FILE (get-c-file 'vector-fwrite fileport))
                   (ret ((foreign-lambda int ,(string-append file-prefix "_fwrite")
                           (c-pointer "FILE") ,csl-vector)
                         FILE vector)))
              (if (= ret (foreign-value GSL_EFAILED int))
                  (error 'vector-fwrite! "error writing to port")
                  (void))))

          (define (vector-fread! fileport vector)
            (let* ((FILE (get-c-file 'vector-fread fileport))
                   (ret ((foreign-lambda int ,(string-append file-prefix "_fread")
                           (c-pointer "FILE") ,csl-vector)
                         FILE vector)))
              (if (= ret (foreign-value GSL_EFAILED int))
                  (error 'vector-fread! "error reading from port")
                  (void))))

          (define (vector-fprintf fileport vector format)
            (let* ((FILE (get-c-file 'vector-fprintf fileport))
                   (ret ((foreign-lambda int ,(string-append file-prefix "_fprintf")
                           (c-pointer "FILE") ,csl-vector c-string)
                         FILE vector format)))
              (if (= ret (foreign-value GSL_EFAILED int))
                  (error 'vector-fprintf! "error writing port")
                  (void))))

          (define (vector-fscanf! fileport vector)
            (let* ((FILE (get-c-file 'vector-fscanf fileport))
                   (ret ((foreign-lambda int ,(string-append file-prefix "_fscanf")
                           (c-pointer "FILE") ,csl-vector)
                         FILE vector)))
              (if (= ret (foreign-value GSL_EFAILED int))
                  (error 'vector-fscanf! "error writing port")
                  (void))))

          ;;; Vector views
          (define vector-subvector
            (foreign-safe-lambda* ,csl-vector ((,csl-vector v) (size_t offset) (size_t n))
              ,(string-translate*
                "#{file-prefix}_view view = #{file-prefix}_subvector(v, offset, n);
                 #{file-prefix} *vec = #{file-prefix}_alloc(view.vector.size);
                 #{file-prefix}_memcpy(vec, &view.vector);
                 C_return(vec);"
                substitutions)))

          (define vector-subvector-with-stride
            (foreign-safe-lambda* ,csl-vector ((,csl-vector v) (size_t offset) (size_t stride) (size_t n))
              ,(string-translate*
                "#{file-prefix}_view view = #{file-prefix}_subvector_with_stride(v, offset, stride, n);
                 #{file-prefix} *vec = #{file-prefix}_alloc(view.vector.size);
                 #{file-prefix}_memcpy(vec, &view.vector);
                 C_return(vec);"
                substitutions)))

          (define vector-subvector-with-stride-set!
            (foreign-safe-lambda* void ((,csl-vector v) (size_t offset) (size_t stride) (size_t n) (,csl-vector sub))
              ,(string-translate*
                "#{file-prefix}_view view = #{file-prefix}_subvector_with_stride(v, offset, stride, n);
                 #{file-prefix}_memcpy(&view.vector, sub);"
                substitutions)))

          (define vector-real
            ,(case base-type
               ((complex complex-float)
                'identity
                `(foreign-safe-lambda* ,csl-vector ((,csl-vector v))
                   ,(string-translate*
                     "#{real-prefix}_view view = #{file-prefix}_real(v);
                      #{file-prefix} *vec = #{file-prefix}_alloc(view.vector.size);
                      for (size_t i=0; i < view.vector.size; ++i) {
                         #{complex-ctype} z;
                         GSL_SET_COMPLEX(&z, #{real-prefix}_get(&view.vector, i), 0);
                         #{file-prefix}_set(vec, i, z);
                      }
                      C_return(vec);"
                     substitutions)))
               ;; all other types are definitely real.
               (else 'identity)))

          (define (vector-imag v)
            ,(case base-type
               ((complex complex-float)
                'identity
                `(foreign-safe-lambda* ,csl-vector ((,csl-vector v))
                   ,(string-translate*
                     "#{real-prefix}_view view = #{file-prefix}_imag(v);
                      #{file-prefix} *vec = #{file-prefix}_alloc(view.vector.size);
                      for (size_t i=0; i < view.vector.size; ++i) {
                         #{complex-ctype} z;
                         GSL_SET_COMPLEX(&z, 0, #{real-prefix}_get(&view.vector, i));
                         #{file-prefix}_set(vec, i, z);
                      }
                      C_return(vec);"
                     substitutions)))
               ;; all other types are definitely real.
               (else 'identity)))

          ;; gsl_vector_view_array and view_array_with_stride omitted

          ;;; Copying vectors
          (define vector-memcpy!
            (foreign-safe-lambda int ,(string-append file-prefix "_memcpy")
              ,csl-vector ,csl-vector))
          (define vector-swap!
            (foreign-safe-lambda int ,(string-append file-prefix "_swap")
              ,csl-vector ,csl-vector))

          ;;; Exchanging elements
          (define vector-swap-elements!
            (foreign-safe-lambda int ,(string-append file-prefix "_swap_elements")
              ,csl-vector size_t size_t))
          (define vector-reverse!
            (foreign-safe-lambda int ,(string-append file-prefix "_reverse")
              ,csl-vector))

          ;;; Vector operations
          (define vector-add!
            (foreign-safe-lambda int ,(string-append file-prefix "_add")
              ,csl-vector ,csl-vector))

          (define vector-sub!
            (foreign-safe-lambda int ,(string-append file-prefix "_sub")
              ,csl-vector ,csl-vector))

          (define vector-mul!
            (foreign-safe-lambda int ,(string-append file-prefix "_mul")
              ,csl-vector ,csl-vector))

          (define vector-div!
            (foreign-safe-lambda int ,(string-append file-prefix "_div")
              ,csl-vector ,csl-vector))

          (define vector-scale!
            ,(case base-type
               ((complex complex-float)
                `(foreign-safe-lambda* int ((,csl-vector v) (,base-type z))
                   ,(string-translate*
                     "#{complex-ctype} _z;
                      GSL_SET_COMPLEX(&_z, z[0], z[1]);
                      #{file-prefix}_scale(v, _z);"
                     substitutions)))
               (else
                `(foreign-safe-lambda int ,(string-append file-prefix "_scale")
                   ,csl-vector ,base-type))))


          (define vector-add-constant!
            ,(case base-type
               ((complex complex-float)
                `(foreign-safe-lambda* int ((,csl-vector v) (,base-type z))
                   ,(string-translate*
                     "#{complex-ctype} _z;
                      GSL_SET_COMPLEX(&_z, z[0], z[1]);
                      #{file-prefix}_add_constant(v, _z);"
                     substitutions)))
               (else
                `(foreign-safe-lambda int ,(string-append file-prefix "_add_constant")
                   ,csl-vector ,base-type))))

          ;;; Maximum and minimum
          (define vector-max
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'vector-max "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-lambda ,base-type ,(string-append file-prefix "_max")
                   ,csl-vector))))

          (define vector-min
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'vector-min "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-lambda ,base-type ,(string-append file-prefix "_min")
                   ,csl-vector))))

          (define vector-minmax
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'vector-minmax "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-primitive ((,csl-vector v))
                     ,(string-translate*
                       "#{base-type} min_out, max_out;
                        #{file-prefix}_minmax(v, &min_out, &max_out);
                        C_word *minptr = C_alloc(#{c-sizeof-type});
                        C_word *maxptr = C_alloc(#{c-sizeof-type});
                        C_word av[4] = {C_SCHEME_UNDEFINED, C_k, #{c-type-constructor}(&minptr, min_out), #{c-type-constructor}(&maxptr, max_out)};
                        C_values(4,av);"
                       substitutions)))))

          (define vector-max-index
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'vector-max-index "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-lambda size_t ,(string-append file-prefix "_max_index")
                   ,csl-vector))))

          (define vector-min-index
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'vector-min-index "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-lambda size_t ,(string-append file-prefix "_min_index")
                   ,csl-vector))))

          (define vector-minmax-index
            ,(case base-type
               ((complex complex-float)
                `(lambda (m) (error 'vector-minmax-index "bad argument type - complex numbers have no ordering")))
               (else
                `(foreign-primitive ((,csl-vector v))
                     ,(string-translate*
                       "size_t min_out, max_out;
                        #{file-prefix}_minmax_index(v, &min_out, &max_out);
                        C_word *minptr = C_alloc(C_SIZEOF_FIX_BIGNUM);
                        C_word *maxptr = C_alloc(C_SIZEOF_FIX_BIGNUM);
                        C_word av[4] = {C_SCHEME_UNDEFINED, C_k, C_int_to_num(&minptr, min_out), C_int_to_num(&maxptr, max_out)};
                        C_values(4,av);"
                       substitutions)))))

          ;;; Vector properties
          (define vector-isnull?
            (foreign-lambda bool ,(string-append file-prefix "_isnull") ,csl-vector))
          (define vector-ispos?
            (foreign-lambda bool ,(string-append file-prefix "_ispos") ,csl-vector))
          (define vector-isneg?
            (foreign-lambda bool ,(string-append file-prefix "_isneg") ,csl-vector))
          (define vector-isnonneg?
            (foreign-lambda bool ,(string-append file-prefix "_isnonneg") ,csl-vector))
          (define vector-equal?
            (foreign-safe-lambda bool ,(string-append file-prefix "_equal") ,csl-vector ,csl-vector)))))))

(make-vector-module gsl.vector.char "gsl_vector_char" byte)
(make-vector-module gsl.vector.complex.double "gsl_vector_complex" complex)
(make-vector-module gsl.vector.complex.float "gsl_vector_complex_float" complex-float)
(make-vector-module gsl.vector.double "gsl_vector" double)
(make-vector-module gsl.vector.float "gsl_vector_float" float)
(make-vector-module gsl.vector.int "gsl_vector_int" int)
(make-vector-module gsl.vector.long "gsl_vector_long" long)
(make-vector-module gsl.vector.short "gsl_vector_short" short)
(make-vector-module gsl.vector.uchar "gsl_vector_uchar" unsigned_byte)
(make-vector-module gsl.vector.uint "gsl_vector_uint" unsigned_int)
(make-vector-module gsl.vector.ulong "gsl_vector_ulong" unsigned_long)
(make-vector-module gsl.vector.ushort "gsl_vector_ushort" unsigned_short)
