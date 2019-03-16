(module gsl.poly (poly-eval
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
          (only srfi-4 list->f64vector f64vector->list f64vector-length f64vector-ref))

  (include "csl-error.scm")
  (include "bind-transformers.scm")

  (define-external (scheme_make_rect (double r) (double i)) scheme-object
    (make-rectangular r i))

  (foreign-declare "#include <gsl/gsl_poly.h>")
  (foreign-declare "#include <gsl/gsl_complex_math.h>")

  (bind-options default-renaming: "")
  (bind-rename/pattern "^gsl-" "")

  (define (poly-eval c z)
    (let ((r (real-part z))
          (i (imag-part z))
          (cr (map real-part c))
          (ci (map imag-part c)))
      ((foreign-safe-lambda* scheme-object ((f64vector cr) (f64vector ci)
                                            (int len)
                                            (double r) (double i))
         "gsl_complex c[len];"
         "int k;"
         "for (k = 0; k < len; k++) {c[k] = gsl_complex_rect(cr[k],ci[k]);};"
         "gsl_complex zin = gsl_complex_rect(r,i);"
         "gsl_complex zout = gsl_complex_poly_complex_eval(c, len, zin);"
         "C_return(scheme_make_rect(GSL_REAL(zout),GSL_IMAG(zout)));")
       (list->f64vector cr)
       (list->f64vector ci)
       (length c)
       r
       i)))

  (bind-rename "gsl_poly_eval_derivs" "%poly-eval-derivs")
  (bind "int gsl_poly_eval_derivs(double* c, ___length(c) size_t, double, double*, size_t)")
  (define (poly-eval-derivs c x #!optional (num-derivs (f64vector-length c)))
    (let ((res (make-f64vector num-derivs)))
      (%poly-eval-derivs c x res num-derivs)
      res))

  (bind-rename "gsl_poly_dd_init" "%poly-dd-init")
  (bind "int gsl_poly_dd_init(double*, double* xa, double*, ___length(xa) size_t)")
  (define (poly-dd-init xa ya)
    (let ((res (make-f64vector (f64vector-length xa))))
      (%poly-dd-init res xa ya)
      res))

  (bind "double gsl_poly_dd_eval(double* dd, double*, ___length(dd) size_t, double)")

  (bind-rename "gsl_poly_dd_taylor" "%poly-dd-taylor")
  (bind "double gsl_poly_dd_taylor(double* c, double, double*, double*, ___length(c) size_t, double*)")
  (define (poly-dd-taylor xp dd xa)
    (let* ((len (f64vector-length dd))
           (w (make-f64vector len))
           (res (make-f64vector len)))
      (%poly-dd-taylor res xp dd xa w)
      res))


  (bind-rename "gsl_poly_dd_hermite_init" "%poly-dd-hermite-init")
  (bind "int gsl_poly_dd_hermite_init(double*, double*, double* xa, double*, double*, ___length(xa) size_t)")
  (define (poly-dd-hermite-init xa ya dya)
    (let* ((len (f64vector-length xa))
           (dd (make-f64vector (* 2 len)))
           (za (make-f64vector (* 2 len))))
      (%poly-dd-hermite-init dd za xa ya dya)
      (values dd za)))

  (define (poly-solve-quadratic a b c)
    (let* ((zout0 (make-f64vector 2))
           (zout1 (make-f64vector 2))
           (num ((foreign-safe-lambda* int ((double a) (double b) (double c)
                                            (f64vector zout0) (f64vector zout1))
                   "gsl_complex z0, z1;"
                   "int res = gsl_poly_complex_solve_quadratic(a,b,c,&z0,&z1);"
                   "zout0[0] = GSL_REAL(z0);"
                   "zout0[1] = GSL_IMAG(z0);"
                   "zout1[0] = GSL_REAL(z1);"
                   "zout1[1] = GSL_IMAG(z1);")
                 a b c
                 zout0
                 zout1))
           (z0 (make-rectangular (f64vector-ref zout0 0)
                                 (f64vector-ref zout0 1)))

           (z1 (make-rectangular (f64vector-ref zout1 0)
                                 (f64vector-ref zout1 1))))
      (if (= num 1)
          (values z0 #f)
          (values z0 z1))))


  (define (poly-solve-cubic a b c)
    (let* ((zout0 (make-f64vector 2))
           (zout1 (make-f64vector 2))
           (zout2 (make-f64vector 2))
           (num ((foreign-safe-lambda* int ((double a) (double b) (double c)
                                            (f64vector zout0) (f64vector zout1)
                                            (f64vector zout2))
                   "gsl_complex z0, z1, z2;"
                   "int res = gsl_poly_complex_solve_cubic(a,b,c,&z0,&z1,&z2);"
                   "zout0[0] = GSL_REAL(z0);"
                   "zout0[1] = GSL_IMAG(z0);"
                   "zout1[0] = GSL_REAL(z1);"
                   "zout1[1] = GSL_IMAG(z1);"
                   "zout2[0] = GSL_REAL(z2);"
                   "zout2[1] = GSL_IMAG(z2);")
                 a b c
                 zout0
                 zout1
                 zout2))
           (z0 (make-rectangular (f64vector-ref zout0 0)
                                 (f64vector-ref zout0 1)))

           (z1 (make-rectangular (f64vector-ref zout1 0)
                                 (f64vector-ref zout1 1)))
           (z2 (make-rectangular (f64vector-ref zout2 0)
                                 (f64vector-ref zout2 1))))
      (values z0 z1 z2)))


  (define (poly-solve a)
    (let* ((a (list->f64vector
               (let loop ((coeffs (f64vector->list a)))
                 (if (zero? (last coeffs))
                     (loop (drop-right coeffs 1))
                     coeffs))))
           (n (f64vector-length a))
           (fv (make-f64vector (* 2 (- n 1)))))
      ((foreign-safe-lambda* int ((f64vector a) (unsigned-int n) (f64vector z))
         ;; "double z[(n - 1)*2];"
         "gsl_poly_complex_workspace * w = gsl_poly_complex_workspace_alloc(n);"
         "gsl_poly_complex_solve(a,n,w,z);"
         "gsl_poly_complex_workspace_free(w);")
       a
       n
       fv)
      (let* ((lst (chop (f64vector->list fv) 2))
             (zs (map (cut apply make-rectangular <>) lst)))
        (apply values zs)))))

;; Local Variables:
;; compile-command: "csc -s csl.poly.scm -J -L -lgsl -L -lgslcblas"
;; End:
