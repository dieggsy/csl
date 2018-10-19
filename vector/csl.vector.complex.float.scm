(include "vector-functor.scm")
(include "vector-module.scm")

(make-vector-module csl.vector.complex.float "gsl_vector_complex_float" (complex float))

;; Local Variables:
;; compile-command: "csc -s csl.vector.complex.float.scm -J -L -lgsl -L -lgslcblas"
;; End:
