(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.short "gsl_matrix_short" "short")

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
