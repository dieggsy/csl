(include-relative "vector-module.scm")

(make-vector-module gsl.vector.uint "gsl_vector_uint" "unsigned int")

;; Local Variables:
;; compile-command: "csc -vs csl.vector.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
