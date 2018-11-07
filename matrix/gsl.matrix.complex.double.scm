(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.complex.double "gsl_matrix_complex" (complex double))

;; Local Variables:
;; compile-command: "csc -vs gsl.matrix.complex.double.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
