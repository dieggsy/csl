(include "utils/syntax-utils.scm")
(include "utils/declarations.scm")
(import-for-syntax chicken.platform)
(begin-for-syntax (register-feature! 'complex))

(module gsl.vector.complex ()
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
  (include "utils/complex-types.scm")
  (foreign-declare "#define _TYPE _complex")
  (foreign-declare "#define _REAL_TYPE")
  (foreign-declare "#include <gsl/gsl_vector_complex_double.h>")

  (include-relative "gsl.vector-impl.scm"))
