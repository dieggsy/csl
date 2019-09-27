(include "utils/syntax-utils.scm")
(include "utils/declarations.scm")

(module gsl.vector.long ()
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

  (foreign-declare "#define _TYPE _long")
  (foreign-declare "#define TYPE long")
  (foreign-declare "#define C_SIZEOF_TYPE C_SIZEOF_FIX_BIGNUM")
  (foreign-declare "#define C_MAKE_TYPE C_int_to_num")
  (foreign-declare "#include <gsl/gsl_vector_long.h>")
  (define-foreign-type TYPE long)

  (include-relative "gsl.vector-impl.scm"))
