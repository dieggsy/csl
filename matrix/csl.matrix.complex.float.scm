(include-relative "matrix-functor.scm")
(import gsl.matrix.complex.float)

(module csl.matrix.complex.float = (generic-matrix gsl.matrix.complex.float))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.complex.float.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
