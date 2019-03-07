(include-relative "vector-functor.scm")
(import gsl.vector.complex.double)

(module csl.vector.complex.double = (generic-vector gsl.vector.complex.double))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.complex.double.scm -J -L -lgsl -L \"$(pkg-config --libs gsl)\""
;; End:
