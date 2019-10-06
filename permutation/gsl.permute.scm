(declare (unit gsl.permute))
(include "utils/declarations.scm")
(include "utils/syntax-utils.scm")

(import-for-syntax (only chicken.string string-translate*))

(define-syntax make-permute-module
  (er-macro-transformer
   (lambda (e rename compare)
     (let* ((module-name (cadr e))
            (file-prefix (caddr e))
            (base-type (cadddr e))
            (substitutions
             `(("#{file-prefix}" . ,file-prefix))))
       `(module ,module-name (permute!
                              permute-inverse!)
          (import scheme
                  chicken.foreign
                  (only chicken.base include)
                  gsl.permutation
                  srfi-4)

          (include "utils/error-handler.scm")
          (include "utils/complex-types.scm")

          (foreign-declare
           ,(string-append "#include <gsl/"
                           (cond ((string=? file-prefix "gsl_permute_complex")
                                  "gsl_permute_complex_double")
                                 ((string=? file-prefix "gsl_permute")
                                  "gsl_permute_double")
                                 (else file-prefix))
                           ".h>"))

          (define-foreign-type gsl-permutation
            (nonnull-c-pointer "gsl_permutation")
            permutation->ptr
            ptr->permutation)

          (define (permute! p data stride)
            ((foreign-lambda gsl-errno ,file-prefix
               ,sizet-array-foreign-type
               ,(ctype-array-foreign-type base-type)
               size_t
               size_t)
             p
             data
             stride
             (,(symbol-append
                (ctype-array-foreign-type base-type)
                '-length)
              data)))

          (define (permute-inverse! p data stride)
            ((foreign-lambda gsl-errno ,(string-append file-prefix "_inverse")
              ,sizet-array-foreign-type
              ,(ctype-array-foreign-type base-type)
              size_t
              size_t)
             p
             data
             stride
             (,(symbol-append
               (ctype-array-foreign-type base-type)
               '-length)
              data))))))))

(make-permute-module gsl.permute.char "gsl_permute_char" byte)
;; (make-vector-module gsl.vector.complex.double "gsl_vector_complex" complex)
;; (make-vector-module gsl.vector.complex.float "gsl_vector_complex_float" complex-float)
(make-permute-module gsl.permute.double "gsl_permute" double)
(make-permute-module gsl.permute.float "gsl_permute_float" float)
(make-permute-module gsl.permute.int "gsl_permute_int" int)
(make-permute-module gsl.permute.long "gsl_permute_long" long)
(make-permute-module gsl.permute.short "gsl_permute_short" short)
(make-permute-module gsl.permute.uchar "gsl_permute_uchar" unsigned_byte)
(make-permute-module gsl.permute.uint "gsl_permute_uint" unsigned_int)
(make-permute-module gsl.permute.ulong "gsl_permute_ulong" unsigned_long)
(make-permute-module gsl.permute.ushort "gsl_permute_ushort" unsigned_short)

