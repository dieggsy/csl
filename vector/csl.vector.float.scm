(include "vector-functor.scm")
(include "vector-module.scm")

(make-vector-module csl.vector.float "gsl_vector_float" float)

;; Local Variables:
;; compile-command: "csc -s csl.vector.float.scm -j csl.vector.float -L -lgsl -L -lgslcblas"
;; End:
