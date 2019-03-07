(include-relative "vector-functor.scm")
(import gsl.vector.ulong)

(module csl.vector.ulong = (generic-vector gsl.vector.ulong))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.ulong.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
