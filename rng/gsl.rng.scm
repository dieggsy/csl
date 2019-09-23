(include "utils/declarations.scm")

(module gsl.rng (rng-alloc
                 rng-set!
                 rng-free!
                 rng-get
                 rng-uniform
                 rng-uniform-pos
                 rng-uniform-int
                 rng-name
                 rng-max
                 rng-min
                 rng-default
                 rng-default-seed
                 rng-env-setup
                 rng-memcpy!
                 rng-clone
                 rng-fwrite
                 rng-fread
                 rng-mt19937
                 rng-ranlxs0
                 rng-ranlxs1
                 rng-ranlxs2
                 rng-ranlxd1
                 rng-ranlxd2
                 rng-ranlux
                 rng-ranlux389
                 rng-cmrg
                 rng-mrg
                 rng-taus
                 rng-taus2
                 rng-gfsr4
                 rng-rand
                 rng-random-bsd
                 rng-random-libc5
                 rng-random-glibc2
                 rng-rand48
                 rng-ranf
                 rng-ranmar
                 rng-r250
                 rng-tt800
                 rng-vax
                 rng-transputer
                 rng-randu
                 rng-minstd
                 rng-uni
                 rng-uni32
                 rng-slatec
                 rng-zuf
                 rng-knuthran2
                 rng-knuthran2002
                 rng-knuthran
                 rng-borosh13
                 rng-fishman18
                 rng-fishman20
                 rng-lecuyer21
                 rng-waterman14
                 rng-fishman2x
                 rng-coveyou)
  (import scheme
          chicken.foreign
          (only chicken.base error void when include define-record-type)
          (only chicken.file file-exists?)
          (only miscmacros ensure))

  (include "utils/error-handler.scm")
  (include "utils/stdio.scm")

  (foreign-declare "#include <gsl/gsl_rng.h>")

  (define-record-type rng
    (ptr->rng ptr)
    rng?
    (ptr rng->ptr))

  (define-record-type rng-type
    (ptr->rng-type ptr)
    rng-type?
    (ptr rng-type->ptr))

  (define-foreign-type gsl-rng
    (nonnull-c-pointer "gsl_rng")
    rng->ptr
    ptr->rng)

  (define-foreign-type gsl-rng-type
    (nonnull-c-pointer "gsl_rng_type")
    rng-type->ptr
    ptr->rng-type)

  ;;; Random number generator initialization
  (define rng-alloc (foreign-lambda gsl-rng "gsl_rng_alloc" gsl-rng-type))

  (define rng-set! (foreign-lambda void "gsl_rng_set" gsl-rng unsigned-long))

  (define rng-free! (foreign-lambda void "gsl_rng_free" gsl-rng))

  ;;; Sampling from a random number generator
  (define rng-get (foreign-lambda unsigned-long "gsl_rng_get" gsl-rng))

  (define rng-uniform (foreign-lambda double "gsl_rng_uniform" gsl-rng))
  (define rng-uniform-pos (foreign-lambda double "gsl_rng_uniform_pos" gsl-rng))
  (define rng-uniform-int (foreign-lambda double "gsl_rng_uniform_int"
                            gsl-rng unsigned-long))

  ;;; Auxiliary random number generator functions
  (define rng-name (foreign-lambda c-string "gsl_rng_name" gsl-rng))

  (define rng-max (foreign-lambda c-string "gsl_rng_max" gsl-rng))
  (define rng-min (foreign-lambda c-string "gsl_rng_min" gsl-rng))

  ;; gsl_rng_state, gsl_rng_size, and gsl_rng_types_setup omitted

  ;;; Random number environment variables
  (define-foreign-variable gsl_rng_default gsl-rng-type)
  (define (rng-default #!optional rng-type)
    (when rng-type
      (set! gsl_rng_default rng-type))
    gsl_rng_default)

  (define-foreign-variable gsl_rng_default_seed unsigned-long)
  (define (rng-default-seed #!optional rng-type)
    (when rng-type
      (set! gsl_rng_default_seed rng-type))
    gsl_rng_default_seed)

  (define rng-env-setup (foreign-safe-lambda gsl-rng-type "gsl_rng_env_setup"))

  ;;; Copying random number generator state
  (define rng-memcpy! (foreign-safe-lambda gsl-errno "gsl_rng_memcpy" gsl-rng gsl-rng))

  (define rng-clone (foreign-lambda gsl-rng "gsl_rng_clone" gsl-rng))

  ;;; Reading and writing random number generator state
  (define (rng-fwrite fileport rng)
    (let* ((FILE (get-c-file 'rng-fwrite fileport)))
      ((foreign-lambda gsl-errno "gsl_rng_fwrite"
         (c-pointer "FILE") gsl-rng)
       FILE rng)))

  (define (rng-fread fileport rng)
    (let* ((FILE (get-c-file 'rng-fread fileport)))
      ((foreign-lambda int "gsl_rng_fread"
         (c-pointer "FILE") gsl-rng)
       FILE rng)))

  ;;; Random number generator algorithms
  (define rng-mt19937 (foreign-value "gsl_rng_mt19937" gsl-rng-type))
  (define rng-ranlxs0 (foreign-value "gsl_rng_ranlxs0" gsl-rng-type))
  (define rng-ranlxs1 (foreign-value "gsl_rng_ranlxs1" gsl-rng-type))
  (define rng-ranlxs2 (foreign-value "gsl_rng_ranlxs2" gsl-rng-type))
  (define rng-ranlxd1 (foreign-value "gsl_rng_ranlxd1" gsl-rng-type))
  (define rng-ranlxd2 (foreign-value "gsl_rng_ranlxd2" gsl-rng-type))
  (define rng-ranlux (foreign-value "gsl_rng_ranlux" gsl-rng-type))
  (define rng-ranlux389 (foreign-value "gsl_rng_ranlux389" gsl-rng-type))
  (define rng-cmrg (foreign-value "gsl_rng_cmrg" gsl-rng-type))
  (define rng-mrg (foreign-value "gsl_rng_mrg" gsl-rng-type))
  (define rng-taus (foreign-value "gsl_rng_taus" gsl-rng-type))
  (define rng-taus2 (foreign-value "gsl_rng_taus2" gsl-rng-type))
  (define rng-gfsr4 (foreign-value "gsl_rng_gfsr4" gsl-rng-type))

  ;;; Unix random number generators
  (define rng-rand (foreign-value "gsl_rng_rand" gsl-rng-type))
  (define rng-random-bsd (foreign-value "gsl_rng_random_bsd" gsl-rng-type))
  (define rng-random-libc5 (foreign-value "gsl_rng_random_libc5" gsl-rng-type))
  (define rng-random-glibc2 (foreign-value "gsl_rng_random_glibc2" gsl-rng-type))
  (define rng-rand48 (foreign-value "gsl_rng_rand48" gsl-rng-type))

  ;;; Other random number generators
  (define rng-ranf (foreign-value "gsl_rng_ranf" gsl-rng-type))
  (define rng-ranmar (foreign-value "gsl_rng_ranmar" gsl-rng-type))
  (define rng-r250 (foreign-value "gsl_rng_r250" gsl-rng-type))
  (define rng-tt800 (foreign-value "gsl_rng_tt800" gsl-rng-type))
  (define rng-vax (foreign-value "gsl_rng_vax" gsl-rng-type))
  (define rng-transputer (foreign-value "gsl_rng_transputer" gsl-rng-type))
  (define rng-randu (foreign-value "gsl_rng_randu" gsl-rng-type))
  (define rng-minstd (foreign-value "gsl_rng_minstd" gsl-rng-type))
  (define rng-uni (foreign-value "gsl_rng_uni" gsl-rng-type))
  (define rng-uni32 (foreign-value "gsl_rng_uni32" gsl-rng-type))
  (define rng-slatec (foreign-value "gsl_rng_slatec" gsl-rng-type))
  (define rng-zuf (foreign-value "gsl_rng_zuf" gsl-rng-type))
  (define rng-knuthran2 (foreign-value "gsl_rng_knuthran2" gsl-rng-type))
  (define rng-knuthran2002 (foreign-value "gsl_rng_knuthran2002" gsl-rng-type))
  (define rng-knuthran (foreign-value "gsl_rng_knuthran" gsl-rng-type))
  (define rng-borosh13 (foreign-value "gsl_rng_borosh13" gsl-rng-type))
  (define rng-fishman18 (foreign-value "gsl_rng_fishman18" gsl-rng-type))
  (define rng-fishman20 (foreign-value "gsl_rng_fishman20" gsl-rng-type))
  (define rng-lecuyer21 (foreign-value "gsl_rng_lecuyer21" gsl-rng-type))
  (define rng-waterman14 (foreign-value "gsl_rng_waterman14" gsl-rng-type))
  (define rng-fishman2x (foreign-value "gsl_rng_fishman2x" gsl-rng-type))
  (define rng-coveyou (foreign-value "gsl_rng_coveyou" gsl-rng-type)))
