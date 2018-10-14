(include "csl-vector-generic-module.scm")

(make-zvector-module csl.vector.complex.float "gsl_vector_complex_float" float)

;; Local Variables:
;; compile-command: "csc -s csl.vector.complex.float.scm -J -L -lgsl -L -lgslcblas"
;; End:
