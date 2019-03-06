(include-relative "vector-module.scm")

(make-vector-module gsl.vector.double "gsl_vector" "double")

;; Local Variables:
;; compile-command: "csc -vs gsl.vector.double.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
