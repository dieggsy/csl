(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.complex.float "gsl_matrix_complex_float" "struct gsl_complex_float")

;; Local Variables:
;; compile-command: "csc -vs gsl.matrix.complex.float.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
