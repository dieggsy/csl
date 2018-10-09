(include "csl-vector-generic-module.scm")

(make-rvector-module csl.lvector "gsl_vector_long" long)

;; Local Variables:
;; compile-command: "csc -s csl.lvector.scm -j csl.lvector -L -lgsl -L -lgslcblas"
;; End:
