(include-relative "matrix-functor.scm")
(include-relative "gsl.matrix.complex.float.scm")

(csl-matrix-module csl.matrix.complex.float = (generic-matrix gsl.matrix.complex.float))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.complex.float.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
