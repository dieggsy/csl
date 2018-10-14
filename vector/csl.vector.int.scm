(include "csl-vector-generic-module.scm")

(make-rvector-module csl.vector.int "gsl_vector_int" int)

;; Local Variables:
;; compile-command: "csc -s csl.vector.int.scm -j csl.vector.int -L -lgsl -L -lgslcblas"
;; End:
