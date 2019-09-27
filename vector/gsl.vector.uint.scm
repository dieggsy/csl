(include "utils/syntax-utils.scm")
(include "utils/declarations.scm")

(module gsl.vector.uint ()
  (import (except scheme vector-set! vector vector?)
          chicken.foreign
          chicken.module
          (only chicken.base
                void
                error
                include
                include-relative
                define-record-type
                identity))

  (foreign-declare "#define _TYPE _uint")
  (foreign-declare "#define TYPE unsigned int")
  (foreign-declare "#define C_SIZEOF_TYPE C_SIZEOF_FIX_BIGNUM")
  (foreign-declare "#define C_MAKE_TYPE C_int_to_num")
  (foreign-declare "#include <gsl/gsl_vector_uint.h>")
  (define-foreign-type TYPE unsigned-int)

  (include-relative "gsl.vector-impl.scm"))
