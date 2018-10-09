(include "csl-vector-generic-module.scm")

(make-zvector-module csl.zdvector "gsl_vector_complex" double)

;; Local Variables:
;; compile-command: "csc -s csl.zdvector.scm -j csl.zdvector -L -lgsl -L -lgslcblas"
;; End:
