(include-relative "vector-module.scm")

(make-vector-module gsl.vector.complex.float "gsl_vector_complex_float" (complex float))

;; Local Variables:
;; compile-command: "csc -vs gsl.vector.complex.float.scm -J -L \"$(pkg-config --libs gsl)\""
;; End:
