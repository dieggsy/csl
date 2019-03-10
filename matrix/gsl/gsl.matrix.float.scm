(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.float "gsl_matrix_float" "float")

;; Local Variables:
;; compile-command: "csc -vs gsl.matrix.float.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
