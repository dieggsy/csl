(include-relative "matrix-functor.scm")
(include-relative "gsl.matrix.long.scm")

(csl-matrix-module csl.matrix.long = (generic-matrix gsl.matrix.long))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.long.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
