(include-relative "matrix-functor.scm")
(import gsl.matrix.float)

(module csl.matrix.float = (generic-matrix gsl.matrix.float))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.float.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
