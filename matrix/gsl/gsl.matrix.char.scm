(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.char "gsl_matrix_char" "___byte")

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
