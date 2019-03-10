(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.uint "gsl_matrix_uint" "unsigned int")

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
