(include-relative "vector-module.scm")

(make-vector-module gsl.vector.float "gsl_vector_float" "float")

;; Local Variables:
;; compile-command: "csc -vs gsl.vector.float.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
