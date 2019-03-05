(import (only chicken.foreign foreign-declare)
        bind)

;; (foreign-declare "

;; ")
(foreign-declare "
#include <string.h>
#include <gsl/gsl_matrix_int.h>
#include <gsl/gsl_complex_math.h>
")

(include "bind-transformers.scm")
(bind-options default-renaming: ""
              foreign-transformer: gsl-arg-transformer*)

(bind-opaque-type cfile (c-pointer "FILE"))
(bind "cfile fopen(char *, char *)")
(bind "int fflush(cfile)")
(bind "cfile stdout")

(bind-opaque-type gsl_vector (c-pointer "gsl_vector_int"))
(bind "int gsl_vector_int_get(gsl_vector, size_t)")

(bind-opaque-type matrix (c-pointer "gsl_matrix_int"))
(bind "matrix gsl_matrix_int_alloc(size_t, size_t)")
;; (bind-rename "gsl_matrix_int_get" matrix-ref)
(bind "int gsl_matrix_int_get(matrix, size_t, size_t)")
(bind "int gsl_matrix_int_fscanf(cfile, matrix)")
(bind "int gsl_matrix_int_fprintf(cfile, matrix, char *)")
;; (bind-file* "wrap.c")
(bind "struct gsl_complex gsl_complex_rect(double, double)")
(bind "struct gsl_complex gsl_complex_add(struct gsl_complex, struct gsl_complex)")
(bind "struct gsl_vector_int_view gsl_matrix_int_diagonal(matrix);")
(bind "int gsl_vector_int_get(gsl_vector, const size_t);")



(define f (fopen "data/p11-grid.txt" "r"))
(define m (gsl-matrix-int-alloc 20 20))
(print (gsl-matrix-int-fscanf f m))
(print (stdout))
(print (gsl-matrix-int-get m 0 19))
;; (print (gsl-matrix-diag m))
(print (gsl-complex-rect 1 2))
(print (gsl-complex-add 1+2i 3+4i))
(print (gsl-vector-int-get (gsl-matrix-int-diagonal m) 0))

(define this '(hey (struct "gsl_complex") c d))
(import matchable
        srfi-13)

(match this
  ((a ('struct (? (lambda (x) (string-prefix? "gsl_complex" x)) x)) c d)
   (print
    (list a x c d))))

