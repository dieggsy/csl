(include-relative "vector-functor.scm")
(import gsl.vector.long)

(module csl.vector.long = (generic-vector gsl.vector.long))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.long.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
