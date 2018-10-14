(include "csl-vector-generic-module.scm")

(make-zvector-module csl.vector.complex.double "gsl_vector_complex" double)

;; Local Variables:
;; compile-command: "csc -s csl.vector.complex.double.scm -J -L -lgsl -L -lgslcblas"
;; End:
