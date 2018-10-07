(include "csl-vector-generic-module.scm")

(make-rvector-module csl.dvector "gsl_vector" double)

;; Local Variables:
;; compile-command: "csc -s csl.dvector.scm -j csl.dvector -L -lgsl -L -lgslcblas"
;; End:
