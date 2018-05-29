(module csl-rng *
  (import chicken scheme foreigners foreign extras)
  (include "csl-error.scm")

  (include "gsl-rng.scm")

  (define-record-type csl:rng
    (csl:ptr->rng data)
    csl:rng?
    (data csl:rng->ptr))

  (define (csl:rng type)
    (case type
      ((taus) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_taus)))))

  (define-record-printer (csl:rng rng out)
    (let ((name (gsl_rng_name (csl:rng->ptr rng))))
      (format out "#,(csl:rng ~a)" name)))

  (define-reader-ctor 'csl:rng csl:rng)

  )
