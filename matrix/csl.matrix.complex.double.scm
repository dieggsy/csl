(include-relative "matrix-functor.scm")
(import gsl.matrix.complex.double)

(module csl.matrix.complex.double = (generic-matrix gsl.matrix.complex.double))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.complex.double.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
