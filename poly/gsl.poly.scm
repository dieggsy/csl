(include "utils/declarations.scm")

(module gsl.poly (poly-eval
                  poly-complex-eval
                  complex-poly-complex-eval
                  poly-eval-derivs
                  poly-dd-init
                  poly-dd-eval
                  poly-dd-taylor
                  poly-dd-hermite-init
                  poly-solve-quadratic
                  poly-complex-solve-quadratic
                  poly-solve-cubic
                  poly-complex-solve-cubic
                  poly-complex-solve)

  (import scheme
          (only chicken.base include chop cut)
          chicken.foreign
          (only srfi-4
                make-f64vector
                list->f64vector
                f64vector->list
                f64vector-length
                f64vector-ref))

  (include "utils/error-handler.scm")
  (include "utils/complex-types.scm")

  (foreign-declare "#include <gsl/gsl_poly.h>")
  (foreign-declare "#include <gsl/gsl_complex_math.h>")

  (define (poly-eval c x)
    ((foreign-safe-lambda double "gsl_poly_eval"
       (const f64vector) (const int) (const double))
     c
     (f64vector-length c)
     x))

  (define (poly-complex-eval c x)
    ((foreign-safe-lambda* scheme-object
         (((const f64vector) c) ((const int) len) (complex z))
       "gsl_complex _z = gsl_complex_rect(z[0],z[1]);"
       "gsl_complex res = gsl_poly_complex_eval(c, len, _z);"
       "C_return(scheme_make_rect(GSL_REAL(res), GSL_IMAG(res)));")
     c
     (f64vector-length c)
     x))

  (define (complex-poly-complex-eval c z)
    (let ((r (real-part z))
          (i (imag-part z))
          (cr (map real-part c))
          (ci (map imag-part c)))
      ((foreign-safe-lambda* scheme-object (((const f64vector) cr) ((const f64vector) ci)
                                            ((const int) len)
                                            ((const double) r) ((const double) i))
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

  (define (poly-eval-derivs c x num-derivs)
    (let ((res (make-f64vector num-derivs)))
      ((foreign-safe-lambda gsl-errno "gsl_poly_eval_derivs"
         (const f64vector) (const size_t) (const double)
         f64vector (const size_t))
       c (f64vector-length c) x res num-derivs)
      res))

  (define (poly-dd-init xa ya)
    (let* ((size (f64vector-length xa))
           (dd (make-f64vector size)))
      ((foreign-safe-lambda gsl-errno "gsl_poly_dd_init"
         f64vector (const f64vector) (const f64vector) size_t)
       dd
       xa
       ya
       size)))

  (define (poly-dd-eval dd xa x)
    (let* ((size (f64vector-length dd)))
      ((foreign-safe-lambda double "gsl_poly_dd_eval"
         (const f64vector) (const f64vector) (const size_t) (const double))
       dd xa size x)))

  (define (poly-dd-taylor xp dd xa)
    (let* ((size (f64vector-length dd))
           (w (make-f64vector size))
           (res (make-f64vector size)))
      ((foreign-safe-lambda gsl-errno "gsl_poly_dd_taylor"
         f64vector double (const f64vector) (const f64vector) size_t f64vector)
       res xp dd xa size w)
      res))

  ;; (bind-rename "gsl_poly_dd_hermite_init" "%poly-dd-hermite-init")
  ;; (bind "int gsl_poly_dd_hermite_init(double*, double*, double* xa, double*, double*, ___length(xa) size_t)")
  (define (poly-dd-hermite-init xa ya dya)
    (let* ((size (f64vector-length xa))
           (dd (make-f64vector (* 2 size)))
           (za (make-f64vector (* 2 size))))
      ((foreign-safe-lambda gsl-errno "gsl_poly_dd_hermite_init"
         f64vector f64vector (const f64vector) (const f64vector) (const f64vector) (const size_t))
        dd za xa ya dya size)
      (values dd za)))

  (define (poly-solve-quadratic a b c)
    (let* ((x0 (make-f64vector 1))
           (x1 (make-f64vector 1))
           (roots ((foreign-safe-lambda gsl-errno "gsl_poly_solve_quadratic"
                       double double double f64vector f64vector)
                   a b c x0 x1)))
      (let ((x0 (f64vector-ref x0 0))
            (x1 (f64vector-ref x1 0)))
        (cond ((zero? roots) '())
              ((= roots 1) (list x0))
              (else (list x0 x1))))))

  (define (poly-complex-solve-quadratic a b c)
    (let* ((zout0 (make-f64vector 2))
           (zout1 (make-f64vector 2))
           (roots ((foreign-safe-lambda* gsl-errno
                       ((double a) (double b) (double c) (f64vector zout0) (f64vector zout1))
                   "gsl_complex z0, z1;"
                   "int res = gsl_poly_complex_solve_quadratic(a,b,c,&z0,&z1);"
                   "zout0[0] = GSL_REAL(z0);"
                   "zout0[1] = GSL_IMAG(z0);"
                   "zout1[0] = GSL_REAL(z1);"
                   "zout1[1] = GSL_IMAG(z1);"
                   "C_return(res);")
                 a b c
                 zout0
                 zout1))
           (z0 (make-rectangular (f64vector-ref zout0 0)
                                 (f64vector-ref zout0 1)))

           (z1 (make-rectangular (f64vector-ref zout1 0)
                                 (f64vector-ref zout1 1))))
      (if (= roots 1)
          (list z0)
          (list z0 z1))))

  (define (poly-solve-cubic a b c)
    (let* ((x0 (make-f64vector 1))
           (x1 (make-f64vector 1))
           (x2 (make-f64vector 1))
           (roots ((foreign-safe-lambda gsl-errno "gsl_poly_solve_cubic"
                       double double double f64vector f64vector f64vector)
                   a b c x0 x1 x2)))
      (let ((x0 (f64vector-ref x0 0))
            (x1 (f64vector-ref x1 0))
            (x2 (f64vector-ref x2 0)))
        (cond ((= roots 1) (list x0))
              (else (list x0 x1 x2))))))

  (define (poly-complex-solve-cubic a b c)
    (let* ((zout0 (make-f64vector 2))
           (zout1 (make-f64vector 2))
           (zout2 (make-f64vector 2))
           (num ((foreign-safe-lambda* gsl-errno ((double a) (double b) (double c)
                                            (f64vector zout0) (f64vector zout1)
                                            (f64vector zout2))
                   "gsl_complex z0, z1, z2;"
                   "int res = gsl_poly_complex_solve_cubic(a,b,c,&z0,&z1,&z2);"
                   "zout0[0] = GSL_REAL(z0);"
                   "zout0[1] = GSL_IMAG(z0);"
                   "zout1[0] = GSL_REAL(z1);"
                   "zout1[1] = GSL_IMAG(z1);"
                   "zout2[0] = GSL_REAL(z2);"
                   "zout2[1] = GSL_IMAG(z2);"
                   "C_return(res);")
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
      (list z0 z1 z2)))


  (define (poly-complex-solve a)
    (let* ((n (f64vector-length a))
           (z (make-f64vector (* 2 (- n 1)))))
      ((foreign-safe-lambda* gsl-errno (((const f64vector) a) (unsigned-int n) (f64vector z))
         "gsl_poly_complex_workspace * w = gsl_poly_complex_workspace_alloc(n);"
         "int res = gsl_poly_complex_solve(a,n,w,z);"
         "gsl_poly_complex_workspace_free(w);"
         "C_return(res);")
       a
       n
       z)
      (let* ((lst (chop (f64vector->list z) 2))
             (zs (map (cut apply make-rectangular <>) lst)))
        zs))))

;; Local Variables:
;; compile-command: "csc -s csl.poly.scm -J -L -lgsl -L -lgslcblas"
;; End:
