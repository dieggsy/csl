(include "utils/syntax-utils.scm")
(include "utils/declarations.scm")

(module gsl.vector.float ()
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

  (foreign-declare "#define _TYPE _float")
  (foreign-declare "#define TYPE float")
  (foreign-declare "#define C_SIZEOF_TYPE C_SIZEOF_FLONUM")
  (foreign-declare "#define C_MAKE_TYPE C_flonum")
  (foreign-declare "#include <gsl/gsl_vector_float.h>")
  (define-foreign-type TYPE float)

  (include-relative "gsl.vector-impl.scm"))
