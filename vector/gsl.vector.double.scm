(include "utils/syntax-utils.scm")
(include "utils/declarations.scm")

(module gsl.vector.double ()
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

  (foreign-declare "#define _TYPE")
  (foreign-declare "#define TYPE double")
  (foreign-declare "#define C_SIZEOF_TYPE C_SIZEOF_FLONUM")
  (foreign-declare "#define C_MAKE_TYPE C_flonum")
  (foreign-declare "#include <gsl/gsl_vector_double.h>")
  (define-foreign-type TYPE double)

  (include-relative "gsl.vector-impl.scm"))
