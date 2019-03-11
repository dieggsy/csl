(include-relative "matrix-functor.scm")
(import gsl.matrix.char)

(module csl.matrix.char = (generic-matrix gsl.matrix.char))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.char.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
