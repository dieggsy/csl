(include-relative "matrix-module.scm")

(make-matrix-module gsl.matrix.ulong "gsl_matrix_ulong" "unsigned long")

;; Local Variables:
;; compile-command: "csc -vs csl.matrix.int.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
