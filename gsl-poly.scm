(foreign-declare "#include <gsl/gsl_poly.h>")

(define (gsl_poly_eval c len x)
  ((foreign-safe-lambda double "gsl_poly_eval" (c-pointer double) int (const double))
   (location (list->f64vector c))
   len
   x))

(define (gsl_poly_complex_eval c len z)
  (let ((r (real-part z))
        (i (imag-part z)))
    ((foreign-safe-lambda* scheme-object (((c-pointer double) c) (int len)
                                          (double r) (double i))
       "gsl_complex zin = gsl_complex_rect(r,i);"
       "gsl_complex zout = gsl_poly_complex_eval(c, len, zin);"
       "C_return(numbers_make_rect(GSL_REAL(zout),GSL_IMAG(zout)));")
     (location (list->f64vector c))
     len
     r
     i)))

(define (gsl_complex_poly_complex_eval c len z)
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
       "C_return(numbers_make_rect(GSL_REAL(zout),GSL_IMAG(zout)));")
     (location (list->f64vector cr))
     (location (list->f64vector ci))
     len
     r
     i)))

(define (gsl_poly_eval_derivs c lenc x)
  (let* ((len (length c))
         (fv (make-f64vector lenc)))
    ((foreign-safe-lambda int "gsl_poly_eval_derivs"
       (c-pointer double) unsigned-int double (c-pointer double) unsigned-int)
     (location (list->f64vector c))
     lenc
     x
     (location fv)
     len)
    (apply values (f64vector->list fv))))

(define (gsl_poly_dd_init xa xy size)
  (let ((dd (make-f64vector size)))
    ((foreign-safe-lambda int "gsl_poly_dd_init"
       (c-pointer double) (c-pointer double) (c-pointer double) unsigned-int)
     (location dd)
     (location (list->f64vector xa))
     (location (list->f64vector xy))
     size)
    (f64vector->list dd)))

(define (gsl_poly_dd_eval dd xa size x)
  ((foreign-safe-lambda double "gsl_poly_dd_eval"
     (c-pointer double) (c-pointer double) unsigned-int double)
   (location (list->f64vector dd))
   (location (list->f64vector xa))
   size
   x))

(define (gsl_poly_dd_taylor xp dd xa size)
  (let ((c (make-f64vector size))
        (w (make-f64vector size)))
    ((foreign-safe-lambda int "gsl_poly_dd_taylor"
       (c-pointer double) double (c-pointer double) (c-pointer double)
       unsigned-int (c-pointer double))
     (location c)
     xp
     (list->f64vector dd)
     (list->f64vector xa)
     size
     (location w))
    (f64vector->list c)))

;; TODO
;; (define (gsl_poly_dd_hermite_init xa ya dya size))

(define gsl_poly_solve_quadratic
  (foreign-primitive ((double a) (double b) (double c))
      "double x0, x1;"
    "C_word *ptr = C_alloc(C_SIZEOF_FLONUM);"
    "int res = gsl_poly_solve_quadratic(a,b,c,&x0,&x1);"
    "C_word av[4];"
    "av[0] = C_SCHEME_UNDEFINED;"
    "av[1] = C_k;"
    "if (res == 0) {"
    "av[2] = C_SCHEME_FALSE;"
    "av[3] = C_SCHEME_FALSE;"
    "}"
    "else if (res == 1) {"
    "av[2] = C_flonum(&ptr,x0);"
    "av[3] = C_SCHEME_FALSE;"
    "}"
    "else {"
    "av[2] = C_flonum(&ptr,x0);"
    "av[3] = C_flonum(&ptr,x1);"
    "}"
    "C_values(4,av);"))

(define (gsl_poly_complex_solve_quadratic a b c)
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

(define gsl_poly_solve_cubic
  (foreign-primitive ((double a) (double b) (double c))
      "double x0, x1, x2;"
    "C_word *ptr = C_alloc(C_SIZEOF_FLONUM);"
    "int res = gsl_poly_solve_cubic(a,b,c,&x0,&x1,&x2);"
    "C_word av[5];"
    "av[0] = C_SCHEME_UNDEFINED;"
    "av[1] = C_k;"
    "if (res == 1) {"
    "av[2] = C_flonum(&ptr,x0);"
    "av[3] = C_SCHEME_FALSE;"
    "av[4] = C_SCHEME_FALSE;"
    "}"
    "else {"
    "av[2] = C_flonum(&ptr,x0);"
    "av[3] = C_flonum(&ptr,x1);"
    "av[4] = C_flonum(&ptr,x2);"
    "}"
    "C_values(5,av);"))

(define (gsl_poly_complex_solve_cubic a b c)
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

(define (gsl_poly_complex_solve a n)
  (let ((fv (make-f64vector (* 2 (- n 1)))))
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
      (apply values zs))))
