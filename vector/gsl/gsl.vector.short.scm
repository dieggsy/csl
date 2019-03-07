(include-relative "vector-module.scm")

(make-vector-module gsl.vector.short "gsl_vector_short" "short")

;; Local Variables:
;; compile-command: "csc -vs csl.vector.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
