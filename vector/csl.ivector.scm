(include "csl-vector-generic-module.scm")

(make-rvector-module csl.ivector "gsl_vector_int" int)

;; Local Variables:
;; compile-command: "csc -s csl.ivector.scm -j csl.ivector -L -lgsl -L -lgslcblas"
;; End:
