(include-relative "vector-module.scm")

(make-vector-module gsl.vector.uchar "gsl_vector_uchar" "unsigned ___byte")

;; Local Variables:
;; compile-command: "csc -vs csl.vector.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
