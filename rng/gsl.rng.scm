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
          bind
          chicken.foreign
          (only chicken.base include define-record-type)
          (only chicken.gc set-finalizer!)
          (only chicken.file file-exists?)
          (only miscmacros ensure))

  (include "csl-error.scm")
  (include "bind-transformers.scm")

  (foreign-declare "#include <gsl/gsl_rng.h>")

  (bind-options default-renaming: "")

  (bind-rename/pattern "^gsl-" "")

  (define-record-type rng
    (ptr->rng ptr)
    rng?
    (ptr rng->ptr))

  (define-record-type rng-type
    (ptr->rng-type ptr)
    rng-type?
    (ptr rng-type->ptr))

  (bind-type
   csl_rng
   (c-pointer "gsl_rng")
   rng->ptr
   ptr->rng)

  (bind-type
   csl_rng_type
   (c-pointer "gsl_rng_type")
   rng-type->ptr
   ptr->rng-type)

  ;;; Random number generator initialization
  (bind-rename "gsl_rng_alloc" "%rng-alloc")
  (bind "csl_rng gsl_rng_alloc(csl_rng_type)")

  (bind-rename "gsl_rng_set" "rng-set!")
  (bind "void gsl_rng_set(csl_rng, unsigned long)")

  (bind-rename "gsl_rng_free" "rng-free!")
  (bind "void gsl_rng_free(csl_rng)")

  (define (rng-alloc type)
    (set-finalizer! (%rng-alloc type) rng-free!))

  ;;; Sampling from a random number generator
  (bind "unsigned long gsl_rng_get(csl_rng)")

  (bind "double gsl_rng_uniform(csl_rng)")

  (bind "double gsl_rng_uniform_pos(csl_rng)")

  (bind "double gsl_rng_uniform_int(csl_rng, unsigned long)")

  ;;; Auxiliary random number generator functions
  (bind "char* gsl_rng_name(csl_rng)")

  (bind "unsigned long gsl_rng_max(csl_rng)")

  (bind "unsigned long gsl_rng_min(csl_rng)")

  ;; gsl_rng_state, gsl_rng_size, and gsl_rng_types_setup omitted

  ;;; Random number environment variables
  (bind "csl_rng_type gsl_rng_default")

  (bind "unsigned long gsl_rng_default_seed")

  (bind "___safe csl_rng_type gsl_rng_env_setup()")

  ;;; Copying random number generator state
  (bind-rename "gsl_rng_memcpy" "rng-memcpy!")
  (bind "___safe int gsl_rng_memcpy(csl_rng, csl_rng)")

  (bind "csl_rng gsl_rng_clone(csl_rng)")

  ;;; Reading and writing random number generator state
  (bind "FILE * fopen(char *, char *)")
  (bind "int fclose(FILE *)")
  (bind-rename "gsl_rng_fwrite" "%rng-fwrite")
  (bind "int gsl_rng_fwrite(FILE*, csl_rng)")

  (define (rng-fwrite filename rng)
    (ensure string? filename "not a valid string filename" filename)
    (let ((f (fopen filename "w")))
      (%rng-fwrite f rng)
      (fclose f)))

  (bind-rename "gsl_rng_fread" "%rng-fread")
  (bind "int gsl_rng_fread(FILE*, csl_rng)")

  (define (rng-fread filename rng)
    (ensure file-exists? filename "file does not exist" filename)
    (let ((f (fopen filename "r")))
      (%rng-fread f rng)
      (fclose f)))

  ;;; Random number generator algorithms
  (bind "const csl_rng_type gsl_rng_mt19937")
  (bind "const csl_rng_type gsl_rng_ranlxs0")
  (bind "const csl_rng_type gsl_rng_ranlxs1")
  (bind "const csl_rng_type gsl_rng_ranlxs2")
  (bind "const csl_rng_type gsl_rng_ranlxd1")
  (bind "const csl_rng_type gsl_rng_ranlxd2")
  (bind "const csl_rng_type gsl_rng_ranlux")
  (bind "const csl_rng_type gsl_rng_ranlux389")
  (bind "const csl_rng_type gsl_rng_cmrg")
  (bind "const csl_rng_type gsl_rng_mrg")
  (bind "const csl_rng_type gsl_rng_taus")
  (bind "const csl_rng_type gsl_rng_taus2")
  (bind "const csl_rng_type gsl_rng_gfsr4")

  ;;; Unix random number generators
  (bind "const csl_rng_type gsl_rng_rand")
  (bind "const csl_rng_type gsl_rng_random_bsd")
  (bind "const csl_rng_type gsl_rng_random_libc5")
  (bind "const csl_rng_type gsl_rng_random_glibc2")
  (bind "const csl_rng_type gsl_rng_rand48")

  ;;; Other random number generators
  (bind "const csl_rng_type gsl_rng_ranf")
  (bind "const csl_rng_type gsl_rng_ranmar")
  (bind "const csl_rng_type gsl_rng_r250")
  (bind "const csl_rng_type gsl_rng_tt800")
  (bind "const csl_rng_type gsl_rng_vax")
  (bind "const csl_rng_type gsl_rng_transputer")
  (bind "const csl_rng_type gsl_rng_randu")
  (bind "const csl_rng_type gsl_rng_minstd")
  (bind "const csl_rng_type gsl_rng_uni")
  (bind "const csl_rng_type gsl_rng_uni32")
  (bind "const csl_rng_type gsl_rng_slatec")
  (bind "const csl_rng_type gsl_rng_zuf")
  (bind "const csl_rng_type gsl_rng_knuthran2")
  (bind "const csl_rng_type gsl_rng_knuthran2002")
  (bind "const csl_rng_type gsl_rng_knuthran")
  (bind "const csl_rng_type gsl_rng_borosh13")
  (bind "const csl_rng_type gsl_rng_fishman18")
  (bind "const csl_rng_type gsl_rng_fishman20")
  (bind "const csl_rng_type gsl_rng_lecuyer21")
  (bind "const csl_rng_type gsl_rng_waterman14")
  (bind "const csl_rng_type gsl_rng_fishman2x")
  (bind "const csl_rng_type gsl_rng_coveyou"))
