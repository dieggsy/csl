(include-relative "vector-functor.scm")
(include-relative "gsl.vector.float.scm")

(module csl.vector.float = (generic-vector gsl.vector.float))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.float.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
