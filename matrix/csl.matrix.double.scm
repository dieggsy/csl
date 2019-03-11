(include-relative "matrix-functor.scm")
(import gsl.matrix.double)

(module csl.matrix.double = (generic-matrix gsl.matrix.double))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.double.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
