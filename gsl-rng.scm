(foreign-declare "#include <gsl/gsl_rng.h>")

(define-foreign-record-type (gsl_rng_type "gsl_rng_type")
  (unsigned-long max gsl_rng_type.max)
  (unsigned-long min gsl_rng_type.min)
  (unsigned-int size gsl_rng_type.size)
  ((function (c-pointer void) ((c-pointer void)
                               unsigned-long))
   set gsl_rng_type.set)
  ((function (c-pointer unsigned-long) ((c-pointer void)))
   get gsl_rng_type.get)
  ((function (c-pointer double) ((c-pointer void)))
   get_double gsl_rng_type.get_double))

(define-foreign-record-type (gsl_rng "gsl_rng")
  ((const gsl_rng_type) type gsl_rng.type)
  ((c-pointer void) state gsl_rng.state))

(define gsl_rng_free
  (foreign-safe-lambda void "gsl_rng_free" gsl_rng))

(define gsl_rng_alloc
  (foreign-safe-lambda gsl_rng "gsl_rng_alloc" (const gsl_rng_type)))

(define gsl_rng_alloc_gc
  (set-finalizer!
   (foreign-safe-lambda gsl_rng "gsl_rng_alloc" (const gsl_rng_type))
   gsl_rng_free))

(define gsl_rng_set
  (foreign-safe-lambda void "gsl_rng_set" (const gsl_rng) unsigned-long))

(define gsl_rng_get
  (foreign-safe-lambda unsigned-long "gsl_rng_get" (const gsl_rng)))

(define gsl_rng_uniform
  (foreign-safe-lambda double "gsl_rng_uniform" (const gsl_rng)))

(define gsl_rng_uniform_pos
  (foreign-safe-lambda double "gsl_rng_uniform_pos" (const gsl_rng)))

(define gsl_rng_uniform_int
  (foreign-safe-lambda unsigned-long "gsl_rng_uniform_int"
    (const gsl_rng) unsigned-long))

(define gsl_rng_name
  (foreign-safe-lambda (const c-string) "gsl_rng_name" (const gsl_rng)))

(define gsl_rng_max
  (foreign-safe-lambda unsigned-long "gsl_rng_max" (const gsl_rng)))

(define gsl_rng_min
  (foreign-safe-lambda unsigned-long "gsl_rng_min" (const gsl_rng)))

(define gsl_rng_size
  (foreign-safe-lambda unsigned-int "gsl_rng_size" (const gsl_rng)))

(define gsl_rng_types_setup
  (foreign-safe-lambda (c-pointer (const gsl_rng)) "gsl_rng_types_setup"))

(define gsl_rng_default
  (foreign-value "gsl_rng_default" gsl_rng_type))

(define gsl_rng_default_seet
  (foreign-value "gsl_rng_default_seed" unsigned-long))

(define gsl_rng_env_setup
  (foreign-safe-lambda (const gsl_rng_type) "gsl_rng_env_setup"))

(define gsl_rng_memcpy
  (foreign-safe-lambda int "gsl_rng_memcpy" gsl_rng (const gsl_rng)))

(define gsl_rng_clone
  (foreign-safe-lambda gsl_rng "gsl_rng_clone" (const gsl_rng)))

(define gsl_rng_mt19937
  (foreign-value "gsl_rng_mt19937" gsl_rng_type))

(define gsl_rng_ranlxs0
  (foreign-value "gsl_rng_ranlxs0" gsl_rng_type))

(define gsl_rng_ranlxs1
  (foreign-value "gsl_rng_ranlxs1" gsl_rng_type))

(define gsl_rng_ranlxs2
  (foreign-value "gsl_rng_ranlxs2" gsl_rng_type))

(define gsl_rng_ranlxd1
  (foreign-value "gsl_rng_ranlxd1" gsl_rng_type))

(define gsl_rng_ranlxd2
  (foreign-value "gsl_rng_ranlxd2" gsl_rng_type))

(define gsl_rng_ranlux
  (foreign-value "gsl_rng_ranlux" gsl_rng_type))

(define gsl_rng_ranlux389
  (foreign-value "gsl_rng_ranlux389" gsl_rng_type))

(define gsl_rng_cmrg
  (foreign-value "gsl_rng_cmrg" gsl_rng_type))

(define gsl_rng_mrg
  (foreign-value "gsl_rng_mrg" gsl_rng_type))


(define gsl_rng_taus
  (foreign-value "gsl_rng_taus" gsl_rng_type))

(define gsl_rng_taus2
  (foreign-value "gsl_rng_taus2" gsl_rng_type))

(define gsl_rng_gfsr4
  (foreign-value "gsl_rng_gfsr4" gsl_rng_type))

(define gsl_rng_rand
  (foreign-value "gsl_rng_rand" gsl_rng_type))

(define gsl_rng_random_bsd
  (foreign-value "gsl_rng_random_bsd" gsl_rng_type))

(define gsl_rng_random_libc5
  (foreign-value "gsl_rng_random_libc5" gsl_rng_type))

(define gsl_rng_random_glibc2
  (foreign-value "gsl_rng_random_glibc2" gsl_rng_type))

(define gsl_rng_rand48
  (foreign-value "gsl_rng_rand48" gsl_rng_type))

(define gsl_rng_ranf
  (foreign-value "gsl_rng_ranf" gsl_rng_type))

(define gsl_rng_ranmar
  (foreign-value "gsl_rng_ranmar" gsl_rng_type))

(define gsl_rng_r250
  (foreign-value "gsl_rng_r250" gsl_rng_type))

(define gsl_rng_tt800
  (foreign-value "gsl_rng_tt800" gsl_rng_type))

(define gsl_rng_vax
  (foreign-value "gsl_rng_vax" gsl_rng_type))

(define gsl_rng_transputer
  (foreign-value "gsl_rng_transputer" gsl_rng_type))

(define gsl_rng_randu
  (foreign-value "gsl_rng_randu" gsl_rng_type))

(define gsl_rng_minstd
  (foreign-value "gsl_rng_minstd" gsl_rng_type))

(define gsl_rng_uni
  (foreign-value "gsl_rng_uni" gsl_rng_type))

(define gsl_rng_uni32
  (foreign-value "gsl_rng_uni32" gsl_rng_type))

(define gsl_rng_slatec
  (foreign-value "gsl_rng_slatec" gsl_rng_type))

(define gsl_rng_zuf
  (foreign-value "gsl_rng_zuf" gsl_rng_type))

(define gsl_rng_knuthran2
  (foreign-value "gsl_rng_knuthran2" gsl_rng_type))

(define gsl_rng_knuthran2002
  (foreign-value "gsl_rng_knuthran2002" gsl_rng_type))

(define gsl_rng_knuthran
  (foreign-value "gsl_rng_knuthran" gsl_rng_type))

(define gsl_rng_borosh13
  (foreign-value "gsl_rng_borosh13" gsl_rng_type))

(define gsl_rng_fishman18
  (foreign-value "gsl_rng_fishman18" gsl_rng_type))

(define gsl_rng_fishman20
  (foreign-value "gsl_rng_fishman20" gsl_rng_type))

(define gsl_rng_lecuyer21
  (foreign-value "gsl_rng_lecuyer21" gsl_rng_type))

(define gsl_rng_waterman14
  (foreign-value "gsl_rng_waterman14" gsl_rng_type))

(define gsl_rng_fishman2x
  (foreign-value "gsl_rng_fishman2x" gsl_rng_type))

(define gsl_rng_coveyou
  (foreign-value "gsl_rng_coveyou" gsl_rng_type))
