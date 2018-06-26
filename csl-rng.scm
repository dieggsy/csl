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
      ((mt19937) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_mt19937)))
      ((ranlxs0) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_ranlxs0)))
      ((ranlxs1) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_ranlxs1)))
      ((ranlxs2) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_ranlxs2)))
      ((ranlxd1) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_ranlxd1)))
      ((ranlxd2) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_ranlxd2)))
      ((ranlux) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_ranlux)))
      ((ranlux389) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_ranlux389)))
      ((cmrg) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_cmrg)))
      ((mrg) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_mrg)))
      ((taus) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_taus)))
      ((taus2) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_taus2)))
      ((gfsr4) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_gfsr4)))
      ((rand) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_rand)))
      ((random_bsd) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_random_bsd)))
      ((random_libc5) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_random_libc5)))
      ((random_glibc2) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_random_glibc2)))
      ((rand48) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_rand48)))
      ((ranf) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_ranf)))
      ((ranmar) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_ranmar)))
      ((r250) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_r250)))
      ((tt800) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_tt800)))
      ((vax) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_vax)))
      ((transputer) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_transputer)))
      ((randu) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_randu)))
      ((minstd) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_minstd)))
      ((uni) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_uni)))
      ((uni32) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_uni32)))
      ((slatec) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_slatec)))
      ((zuf) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_zuf)))
      ((knuthran2) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_knuthran2)))
      ((knuthran2002) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_knuthran2002)))
      ((knuthran) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_knuthran)))
      ((borosh13) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_borosh13)))
      ((fishman18) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_fishman18)))
      ((fishman20) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_fishman20)))
      ((lecuyer21) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_lecuyer21)))
      ((waterman14) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_waterman14)))
      ((fishman2x) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_fishman2x)))
      ((coveyou) (csl:ptr->rng (gsl_rng_alloc_gc gsl_rng_coveyou)))))

  (define csl:rng-default
    (make-parameter (csl:rng 'mt19937)
                    (lambda (x)
                      (if (not (csl:rng? x))
                          (error "rng-default must be a csl:rng")
                          x))))

  (define (csl:rng-max #!optional (rng (csl:rng-default)))
    (gsl_rng_max (csl:rng->ptr rng)))
  (define (csl:rng-min #!optional (rng (csl:rng-default)))
    (gsl_rng_min (csl:rng->ptr rng)))

  (define-record-printer (csl:rng rng out)
    (let ((name (gsl_rng_name (csl:rng->ptr rng))))
      (format out "#,(csl:rng ~a)" name)))

  (define-reader-ctor 'csl:rng csl:rng)

  (define (csl:random-integer #!key (rng (csl:rng-default)))
    (gsl_rng_get (csl:rng->ptr rng)))

  (define (csl:random-uniform #!key (rng (csl:rng-default)))
    (gsl_rng_uniform (csl:rng->ptr rng)))

  (define (csl:random-uniform+ #!key (rng (csl:rng-default)))
    (gsl_rng_uniform (csl:rng->ptr rng)))

  (define (csl:random-uniform* n #!key (rng (csl:rng-default)))
    (gsl_rng_uniform_int (csl:rng->ptr rng) n)))
