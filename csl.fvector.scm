(include "csl-vector-generic-module.scm")

(make-rvector-module csl.fvector "gsl_vector_float" float)

;; Local Variables:
;; compile-command: "csc -s csl.fvector.scm -j csl.fvector -L -lgsl -L -lgslcblas"
;; End:
