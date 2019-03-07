(include-relative "vector-functor.scm")
(import gsl.vector.uchar)

(module csl.vector.uchar = (generic-vector gsl.vector.uchar))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.uchar.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
