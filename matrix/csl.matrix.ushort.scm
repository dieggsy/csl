(include-relative "matrix-functor.scm")
(import gsl.matrix.ushort)

(module csl.matrix.ushort = (generic-matrix gsl.matrix.ushort))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.ushort.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
