(include "vector-functor.scm")
(include "gsl.vector.double.scm")

(module csl.vector.double = (generic-vector gsl.vector.double))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.double.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
