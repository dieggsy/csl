(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.double "gsl_matrix" "double")

;; Local Variables:
;; compile-command: "csc -vs gsl.matrix.double.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
