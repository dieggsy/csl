(module csl.rng (make-rng
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
          (prefix gsl.rng gsl:))

  (reexport gsl.rng)

  (define (rng-random-seed)
    (let ((bytes (random-bytes (make-blob 8))))
      ((foreign-lambda* unsigned-long ((c-pointer l))
         "C_return(*(unsigned long*)l);")
       (location bytes))))

  (define (make-rng rng-type)
    (let ((rng (gsl:rng-alloc rng-type)))
      (gsl:rng-set! rng (rng-random-seed))
      rng))

  (define rng-set-seed! gsl:rng-set!)
  (define rng-uniform-positive gsl:rng-uniform-pos)
  (define rng-uniform-integer gsl:rng-uniform-int)
  (define rng-copy! gsl:rng-memcpy!)
  (define rng-copy gsl:rng-clone))
