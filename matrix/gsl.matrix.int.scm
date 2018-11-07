(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.int "gsl_matrix_int" int)

;; Local Variables:
;; compile-command: "csc -vs gsl.matrix.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
