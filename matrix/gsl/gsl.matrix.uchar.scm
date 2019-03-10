(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.uchar "gsl_matrix_uchar" "unsigned ___byte")

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
