(include-relative "matrix-functor.scm")
(import gsl.matrix.short)

(module csl.matrix.short = (generic-matrix gsl.matrix.short))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.short.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
