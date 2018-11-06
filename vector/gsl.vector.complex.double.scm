(include-relative "vector-module.scm")

(make-vector-module gsl.vector.complex.double "gsl_vector_complex" (complex double))

;; Local Variables:
;; compile-command: "csc -vs gsl.vector.complex.double.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
