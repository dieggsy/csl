(include-relative "matrix-functor.scm")
(import gsl.matrix.uint)

(module csl.matrix.uint = (generic-matrix gsl.matrix.uint))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.uint.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
