(include-relative "vector-functor.scm")
(import gsl.vector.char)

(module csl.vector.char = (generic-vector gsl.vector.char))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.char.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
