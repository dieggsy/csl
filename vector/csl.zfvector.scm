(include "csl-vector-generic-module.scm")

(make-zvector-module csl.zfvector "gsl_vector_complex_float" float)

;; Local Variables:
;; compile-command: "csc -s csl.zfvector.scm -J -L -lgsl -L -lgslcblas"
;; End: