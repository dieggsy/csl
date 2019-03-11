(include-relative "matrix-functor.scm")
(import gsl.matrix.long)

(module csl.matrix.long = (generic-matrix gsl.matrix.long))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.long.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
