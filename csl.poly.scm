(module csl.poly (poly-eval
                  poly-eval*
                  poly-eval-derivs
                  poly-eval-derivs*
                  poly-dd-init
                  poly-dd-eval
                  poly-dd-taylor
                  solve-quadratic
                  solve-quadratic*
                  solve-cubic
                  poly-solve)

  (import scheme
          (chicken base)
          (chicken foreign)
          (srfi 1)
          (srfi 4))

  (foreign-declare "#include <gsl/gsl_poly.h>")
  (foreign-declare "#include <gsl/gsl_complex_math.h>")

  (include "csl-error.scm")

  (define-external (scheme_make_rect (double r) (double i)) scheme-object
    (make-rectangular r i))

  ;; (include "gsl-poly.scm")
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

  (define (poly-eval* c z)
    (let loop ((const c)
               (pow 0)
               (val 0))
      (if (null? const)
          val
          (loop (cdr const)
                (+ pow 1)
                (+ val (* (car const) (expt z pow)))))))

  (define (poly-eval-derivs c x #!optional (num-derivs (length c)))
    (let* ((len (length c))
           (fv (make-f64vector num-derivs)))
      ((foreign-safe-lambda int "gsl_poly_eval_derivs"
         (c-pointer double) unsigned-int double (c-pointer double) unsigned-int)
       (location (list->f64vector c))
       len
       x
       (location fv)
       num-derivs)
      (f64vector->list fv)))

  (define (poly-eval-derivs* c x #!optional (num-derivs (length c)))
    (define (next-deriv const)
      (if (null? (cdr const))
          '(0)
          (let loop ((c (cdr const))
                     (pow 1))
            (if (null? c)
                '()
                (cons (* pow (car c))
                      (loop (cdr c) (+ pow 1)))))))
    (cons
     (poly-eval* c x)
     (let loop ((num num-derivs)
                (c (next-deriv c)))
       (if (zero? (- num 1))
           '()
           (let ((nd (next-deriv c)))
             (cons (poly-eval* nd x)
                   (loop (- num 1) (next-deriv c))))))))

  (define (poly-dd-init xa ya)
    (let* ((size (length xa))
           (dd (make-f64vector size)))
      ((foreign-safe-lambda int "gsl_poly_dd_init"
         (c-pointer double) (c-pointer double) (c-pointer double) unsigned-int)
       (location dd)
       (location (list->f64vector xa))
       (location (list->f64vector ya))
       size)
      (f64vector->list dd)))

  (define (poly-dd-eval dd xa x)
    (let ((size (length dd)))
      ((foreign-safe-lambda double "gsl_poly_dd_eval"
         (c-pointer double) (c-pointer double) unsigned-int double)
       (location (list->f64vector dd))
       (location (list->f64vector xa))
       size
       x)))

  (define (poly-dd-taylor xp dd xa)
    (let* ((size (length dd))
           (c (make-f64vector size))
           (w (make-f64vector size))
           (ddf64 (list->f64vector dd))
           (xaf64 (list->f64vector xa)))
      ((foreign-safe-lambda int "gsl_poly_dd_taylor"
         (c-pointer double) double (c-pointer double) (c-pointer double)
         unsigned-int (c-pointer double))
       (location c)
       xp
       (location dd)
       (location xa)
       size
       (location w))
      (f64vector->list c)))

  ;; ;; TODO
  ;; ;; (define (csl:dd-hermite-init xa ya dya size))

  (define (solve-quadratic a b c)
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

  (define (solve-quadratic* a b c)
    (define (sign n) (if (positive? n) -1 1))
    (let ((q (* -1/2 (+ b (* (sign b) (sqrt (- (expt b 2) (* 4 a c))))))))
      (values (/ q a) (/ c q))))

  (define (solve-cubic a b c)
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
