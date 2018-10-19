(include "vector-functor.scm")
(include "real-vector-module.scm")

(make-rvector-module csl.vector.long "gsl_vector_long" long)

;; Local Variables:
;; compile-command: "csc -s csl.vector.long.scm -j csl.vector.long -L -lgsl -L -lgslcblas"
;; End:
