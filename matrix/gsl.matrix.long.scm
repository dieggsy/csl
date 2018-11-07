(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.long "gsl_matrix_long" long)

;; Local Variables:
;; compile-command: "csc -vs gsl.matrix.long.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
