(include "vector-functor.scm")
(include "vector-module.scm")

(make-vector-module csl.vector.complex.double "gsl_vector_complex" (complex double))

;; Local Variables:
;; compile-command: "csc -s csl.vector.complex.double.scm -J -L -lgsl -L -lgslcblas"
;; End:
