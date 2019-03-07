(include-relative "vector-functor.scm")
(import gsl.vector.short)

(module csl.vector.short = (generic-vector gsl.vector.short))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.short.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
