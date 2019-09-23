(module csl.rng (rng-alloc
                 rng-clone
                 make-rng
                 rng-random-seed
                 rng-set-seed!
                 rng-uniform-positive
                 rng-uniform-integer
                 rng-copy!
                 rng-copy)
  (import scheme
          chicken.foreign
          (only chicken.module reexport)
          (only chicken.blob make-blob)
          (only chicken.random random-bytes)
          (only chicken.gc set-finalizer!)
          (prefix gsl.rng gsl:))

  (reexport (except gsl.rng rng-alloc rng-clone))
  (define (rng-alloc type)
    (set-finalizer! (gsl:rng-alloc type) gsl:rng-free!))
  (define (rng-clone rng)
    (set-finalizer! (gsl:rng-clone rng) gsl:rng-free!))

  (define (rng-random-seed)
    (let ((bytes (random-bytes (make-blob 8))))
      ((foreign-lambda* unsigned-long ((c-pointer l))
         "C_return(*(unsigned long*)l);")
       (location bytes))))

  (define (make-rng rng-type)
    (let ((rng (rng-alloc rng-type)))
      (gsl:rng-set! rng (rng-random-seed))
      rng))

  (define rng-set-seed! gsl:rng-set!)
  (define rng-uniform-positive gsl:rng-uniform-pos)
  (define rng-uniform-integer gsl:rng-uniform-int)
  (define rng-copy! gsl:rng-memcpy!)
  (define rng-copy rng-clone))
