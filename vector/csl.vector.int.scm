(include "vector-functor.scm")
(include "gsl.vector.int.scm")

(module csl.vector.int = (generic-vector gsl.vector.int))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
