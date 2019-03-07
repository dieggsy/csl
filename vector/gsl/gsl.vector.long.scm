(include-relative "vector-module.scm")

(make-vector-module gsl.vector.long "gsl_vector_long" "long")

;; Local Variables:
;; compile-command: "csc -vs csl.vector.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
