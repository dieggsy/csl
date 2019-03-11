(include-relative "matrix-functor.scm")
(import gsl.matrix.int)

(module csl.matrix.int = (generic-matrix gsl.matrix.int))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
