(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.ushort "gsl_matrix_ushort" "unsigned short")

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
