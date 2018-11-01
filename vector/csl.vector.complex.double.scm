(include "vector-functor.scm")
(include "gsl.vector.complex.double.scm")

(module csl.vector.complex.double = (generic-vector gsl.vector.complex.double))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.complex.double.scm -J -L -lgsl -L \"$(pkg-config --libs gsl)\""
;; End:
