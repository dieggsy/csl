(include-relative "matrix-functor.scm")
(include-relative "gsl.matrix.int.scm")

(csl-matrix-module csl.matrix.int = (generic-matrix gsl.matrix.int))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
