(include-relative "vector-functor.scm")
(import gsl.vector.ushort)

(module csl.vector.ushort = (generic-vector gsl.vector.ushort))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.ushort.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
