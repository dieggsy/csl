(include-relative "matrix-functor.scm")
(import gsl.matrix.uchar)

(module csl.matrix.uchar = (generic-matrix gsl.matrix.uchar))

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.uchar.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
