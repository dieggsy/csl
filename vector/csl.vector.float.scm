(include "csl-vector-generic-module.scm")

(make-rvector-module csl.vector.float "gsl_vector_float" float)

;; Local Variables:
;; compile-command: "csc -s csl.vector.float.scm -j csl.vector.float -L -lgsl -L -lgslcblas"
;; End:
