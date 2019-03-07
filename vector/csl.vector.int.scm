(include-relative "vector-functor.scm")
(import gsl.vector.int)

(module csl.vector.int = (generic-vector gsl.vector.int))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
