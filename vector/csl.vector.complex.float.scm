(include-relative "vector-functor.scm")
(import gsl.vector.complex.float)

(module csl.vector.complex.float = (generic-vector gsl.vector.complex.float))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.complex.float.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
