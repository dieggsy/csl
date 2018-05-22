(module csl-poly *
  (use numbers
       foreigners
       srfi-4
       srfi-1)

  (include "gsl-poly.scm")
  (define (csl:poly-eval c x)
    (cond ((find complex? c)
           (gsl_complex_poly_complex_eval c (length c) x))
          ((complex? x)
           (gsl_poly_complex_eval c (length c) x))
          (else (gsl_poly_eval c (length c) x))))

  (define (csl:poly-eval-derivs c x)
    (gsl_poly_eval_derivs c (length c) x))

  (define (csl:poly-dd-init xa xy)
    (gsl_poly_dd_init xa xy (length xa)))

  (define (csl:poly-dd-eval dd xa x)
    (gsl_poly_dd_eval dd xa (length dd) x))

  (define (csl:poly-dd-taylor xp dd xa)
    (gsl_poly_dd_talyor xp dd xa (length xp)))

  ;; TODO
  ;; (define (csl:poly-dd-hermite-init xa ya dya size))

  (define (csl:quadratic-solve-real a b c)
    (gsl_poly_solve_quadratic a b c))

  (define (csl:quadratic-solve a b c)
    (gsl_poly_complex_solve_quadratic a b c))

  (define (csl:cubic-solve-real a b c)
    (gsl_poly_solve_cubic a b c))

  (define (csl:cubic-solve a b c)
    (gsl_poly_complex_solve_cubic a b c))

  (define (csl:poly-solve c)
    (let ((c (let loop ((coeffs c))
               (if (zero? (last coeffs))
                   (loop (drop-right coeffs 1))
                   coeffs))))
      (gsl_poly_complex_solve c (length c)))))
