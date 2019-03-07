(include-relative "vector-functor.scm")
(import gsl.vector.uint)

(module csl.vector.uint = (generic-vector gsl.vector.uint))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.uint.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
