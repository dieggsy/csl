(include-relative "matrix-functor.scm")
(import gsl.matrix.ulong)

(module csl.matrix.ulong = (generic-matrix gsl.matrix.ulong))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.ulong.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
