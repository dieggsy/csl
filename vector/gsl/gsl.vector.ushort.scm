(include-relative "vector-module.scm")

(make-vector-module gsl.vector.ushort "gsl_vector_ushort" "unsigned short")

;; Local Variables:
;; compile-command: "csc -vs csl.vector.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
