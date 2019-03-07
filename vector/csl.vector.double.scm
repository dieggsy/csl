(include-relative "vector-functor.scm")
(import gsl.vector.double)

(module csl.vector.double = (generic-vector gsl.vector.double))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.double.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
