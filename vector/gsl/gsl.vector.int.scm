(include-relative "vector-module.scm")

(make-vector-module gsl.vector.int "gsl_vector_int" "int")

;; Local Variables:
;; compile-command: "csc -vs csl.vector.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
