(include "utils/syntax-utils.scm")
(include "utils/declarations.scm")

(module gsl.vector.ushort ()
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

  (foreign-declare "#define _TYPE _ushort")
  (foreign-declare "#define TYPE unsigned short")
  (foreign-declare "#define C_SIZEOF_TYPE C_SIZEOF_FIX_BIGNUM")
  (foreign-declare "#define C_MAKE_TYPE C_int_to_num")
  (foreign-declare "#include <gsl/gsl_vector_ushort.h>")
  (define-foreign-type TYPE unsigned-short)

  (include-relative "gsl.vector-impl.scm"))
