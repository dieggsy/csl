(include-relative "vector-module.scm")

(make-vector-module gsl.vector.ulong "gsl_vector_ulong" "unsigned long")

;; Local Variables:
;; compile-command: "csc -vs csl.vector.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
