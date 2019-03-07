(module gsl.poly (poly-eval
                  ;; poly-complex-eval
                  ;; complex-poly-complex-eval
                  poly-eval-derivs
                  poly-dd-init
                  poly-dd-eval
                  poly-dd-taylor
                  poly-dd-hermite-init
                  poly-solve-quadratic
                  poly-solve-cubic
                  poly-solve)

  (import scheme
          (only chicken.base include chop let-values cut)
          chicken.foreign
          bind
          (only srfi-1 last drop-right)
          (only srfi-4 list->f64vector f64vector->list))

  (include "csl-error.scm")
  (include "bind-transformers.scm")

  (define-external (scheme_make_rect (double r) (double i)) scheme-object
    (make-rectangular r i))

  (foreign-declare "#include <gsl/gsl_poly.h>")
  (foreign-declare "#include <gsl/gsl_complex_math.h>")

  (bind-rename/pattern "^gsl-" "")

  ;; (bind-rename "gsl_poly_eval" "%poly-eval")
  ;; (bind "double gsl_poly_eval(double* c, ___length(c) int, double)")
  ;; (define (poly-eval coeffs x)
  ;;   (%poly-eval (list->f64vector coeffs) x))

  ;; (bind-rename "gsl_poly_complex_eval" "%poly-complex-eval")
  ;; (bind "struct gsl_complex gsl_poly_complex_eval(double* c, ___length(c) int, struct gsl_complex)")
  ;; (define (poly-complex-eval coeffs z)
  ;;   (%poly-complex-eval (list->f64vector coeffs) z))

  ;; (bind-rename "gsl_complex_poly_complex_eval" "%complex-poly-complex-eval")
  ;; (bind "struct gsl_complex gsl_complex_poly_complex_eval()")
  (define (poly-eval c z)
    (let ((r (real-part z))
          (i (imag-part z))
          (cr (map real-part c))
          (ci (map imag-part c)))
      ((foreign-safe-lambda* scheme-object (((c-pointer double) cr) ((c-pointer double) ci)
                                            (int len)
                                            (double r) (double i))
         "gsl_complex c[sizeof(cr)];"
         "int k;"
         "for (k = 0; k < len; k++) {c[k] = gsl_complex_rect(cr[k],ci[k]);};"
         "gsl_complex zin = gsl_complex_rect(r,i);"
         "gsl_complex zout = gsl_complex_poly_complex_eval(c, len, zin);"
         "C_return(scheme_make_rect(GSL_REAL(zout),GSL_IMAG(zout)));")
       (location (list->f64vector cr))
       (location (list->f64vector ci))
       (length c)
       r
       i)))

  (bind-rename "gsl_poly_eval_derivs" "%poly-eval-derivs")
  (bind "int gsl_poly_eval_derivs(double* c, ___length(c) size_t, double, double*, size_t)")
  (define (poly-eval-derivs c x #!optional (num-derivs (length c)))
    (let ((res (make-f64vector num-derivs)))
      (%poly-eval-derivs (list->f64vector c) x res num-derivs)
      (f64vector->list res)))

  (bind-rename "gsl_poly_dd_init" "%poly-dd-init")
  (bind "int gsl_poly_dd_init(double*, double* xa, double*, ___length(xa) size_t)")
  (define (poly-dd-init xa ya)
    (let ((res (make-f64vector (length xa))))
      (%poly-dd-init res (list->f64vector xa) (list->f64vector ya))
      (f64vector->list res)))

  (bind-rename "gsl_poly_dd_eval" "%poly-dd-eval")
  (bind "double gsl_poly_dd_eval(double* dd, double*, ___length(dd) size_t, double)")
  (define (poly-dd-eval dd xa x)
    (%poly-dd-eval (list->f64vector dd) (list->f64vector xa) x))

  (bind-rename "gsl_poly_dd_taylor" "%poly-dd-taylor")
  (bind "double gsl_poly_dd_taylor(double* c, double, double*, double*, ___length(c) size_t, double*)")
  (define (poly-dd-taylor xp dd xa)
    (let* ((len (length dd))
           (w (make-f64vector len))
           (res (make-f64vector len)))
      (%poly-dd-taylor res xp dd xa w)))


  (bind-rename "gsl_poly_dd_hermite_init" "%poly-dd-hermite-init")
  (bind "int gsl_poly_dd_hermite_init(double*, double*, double* xa, double*, double*, ___length(xa) size_t)")
  (define (poly-dd-hermite-init xa ya dya)
    (let* ((len (length xa))
           (dd (make-f64vector (* 2 len)))
           (za (make-f64vector (* 2 len))))
      (%poly-dd-hermite-init dd za xa ya dya)
      (values dd za)))

  (define (poly-solve-quadratic a b c)
    (let-values (((r0 i0 r1 i1)
                  ((foreign-primitive ((double a) (double b) (double c))
                       "gsl_complex z0, z1;"
                     "C_word *ptr = C_alloc(C_SIZEOF_FLONUM);"
                     "int res = gsl_poly_complex_solve_quadratic(a,b,c,&z0,&z1);"
                     "C_word av[6];"
                     "av[0] = C_SCHEME_UNDEFINED;"
                     "av[1] = C_k;"
                     "if (res == 1) {"
                     "av[2] = C_flonum(&ptr,GSL_REAL(z0));"
                     "av[3] = C_flonum(&ptr,GSL_IMAG(z0));"
                     "av[4] = C_SCHEME_FALSE;"
                     "av[5] = C_SCHEME_FALSE;"
                     "}"
                     "else {"
                     "av[2] = C_flonum(&ptr,GSL_REAL(z0));"
                     "av[3] = C_flonum(&ptr,GSL_IMAG(z0));"
                     "av[4] = C_flonum(&ptr,GSL_REAL(z1));"
                     "av[5] = C_flonum(&ptr,GSL_IMAG(z1));"
                     "}"
                     "C_values(6,av);")
                   a b c)))
      (let ((z0 (if r0 (make-rectangular r0 i0) #f))
            (z1 (if r1 (make-rectangular r1 i1) #f)))
        (values z0 z1))))


  (define (poly-solve-cubic a b c)
    (let-values (((r1 i1 r2 i2 r3 i3)
                  ((foreign-primitive ((double a) (double b) (double c))
                       "gsl_complex z0, z1, z2;"
                     "gsl_poly_complex_solve_cubic(a,b,c,&z0,&z1,&z2);"
                     "C_word *ptr = C_alloc(C_SIZEOF_FLONUM);"
                     "C_word av[8] = {C_SCHEME_UNDEFINED, C_k, "
                     "C_flonum(&ptr,GSL_REAL(z0)), C_flonum(&ptr,GSL_IMAG(z0)), "
                     "C_flonum(&ptr,GSL_REAL(z1)), C_flonum(&ptr,GSL_IMAG(z1)),"
                     "C_flonum(&ptr,GSL_REAL(z2)), C_flonum(&ptr,GSL_IMAG(z2))"
                     "};"
                     "C_values(8,av);")
                   a b c)))
      (values (make-rectangular r1 i1)
              (make-rectangular r2 i2)
              (make-rectangular r3 i3))))


  (define (poly-solve a)
    (let* ((a (let loop ((coeffs a))
                (if (zero? (last coeffs))
                    (loop (drop-right coeffs 1))
                    coeffs)))
           (n (length a))
           (fv (make-f64vector (* 2 (- n 1)))))
      ((foreign-safe-lambda* int ((c-pointer a) (unsigned-int n) (c-pointer z))
         ;; "double z[(n - 1)*2];"
         "gsl_poly_complex_workspace * w = gsl_poly_complex_workspace_alloc(n);"
         "gsl_poly_complex_solve(a,n,w,z);"
         "gsl_poly_complex_workspace_free(w);")
       (location (list->f64vector a))
       n
       (location fv))
      (let* ((lst (chop (f64vector->list fv) 2))
             (zs (map (cut apply make-rectangular <>) lst)))
        (apply values zs)))))

;; Local Variables:
;; compile-command: "csc -s csl.poly.scm -J -L -lgsl -L -lgslcblas"
;; End:
