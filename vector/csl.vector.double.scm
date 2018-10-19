(include "vector-functor.scm")
(include "vector-module.scm")

(make-vector-module csl.vector.double "gsl_vector" double)

;; Local Variables:
;; compile-command: "csc -s csl.vector.double.scm -j csl.vector.double -L -lgsl -L -lgslcblas"
;; End:
