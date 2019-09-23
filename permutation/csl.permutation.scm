(module csl.permutation (permutation-alloc
                         permutation-calloc
                         make-identity-permutation
                         permutation-reverse
                         permutation-inverse
                         permutation-next!
                         permutation-prev!
                         permutation-next
                         permutation-prev
                         permutation-mul
                         permutation-linear->canonical!
                         permutation-canonical->linear!
                         permutation-linear->canonical
                         permutation-canonical->linear
                         )
  (import scheme
          (only chicken.module reexport)
          (only chicken.gc set-finalizer!)
          (only chicken.type the)
          (prefix gsl.permutation gsl:)
          (prefix gsl.errno gsl:))

  (reexport (except gsl.permutation
                    permutation-alloc
                    permutation-calloc
                    permutation-next!
                    permutation-prev!)) 

  (define (permutation-alloc size)
    (set-finalizer! (gsl:permutation-alloc size) gsl:permutation-free!))
  (define (permutation-calloc size)
    (set-finalizer! (gsl:permutation-calloc size) gsl:permutation-free!))

  (define (make-identity-permutation size)
    (let ((p (permutation-alloc size)))
      (gsl:permutation-init! p)
      p))

  (define (permutation-reverse p)
    (let ((new (permutation-alloc (gsl:permutation-size p))))
      (gsl:permutation-memcpy! new p)
      (gsl:permutation-reverse! new)
      new))

  (define (permutation-inverse p)
    (let ((new (permutation-alloc (gsl:permutation-size p))))
      (gsl:permutation-memcpy! new p)
      (gsl:permutation-inverse! new)
      new))

  (define (permutation-next! p)
    (let ((ret (gsl:permutation-next! p)))
      (if (= ret gsl:errno/failure)
          #f
          #t)))

  (define (permutation-prev! p)
    (let ((ret (gsl:permutation-prev! p)))
      (if (= ret gsl:errno/failure)
          #f
          #t)))

  (define (permutation-next p)
    (let ((new (permutation-alloc (gsl:permutation-size p))))
      (gsl:permutation-memcpy! new p)
      (let ((ret (gsl:permutation-next! new)))
        (if (= ret gsl:errno/success)
            new
            #f))))

  (define (permutation-prev p)
    (let ((new (permutation-alloc (gsl:permutation-size p))))
      (gsl:permutation-memcpy! new p)
      (let ((ret (gsl:permutation-prev! new)))
        (if (= ret gsl:errno/success)
            new
            #f))))

  (define (permutation-mul p1 p2)
    (let ((new (permutation-alloc (gsl:permutation-size p1))))
      (gsl:permutation-mul! new p1 p2)
      new))

  (define permutation-linear->canonical! gsl:permutation-linear-to-canonical!)
  (define permutation-canonical->linear! gsl:permutation-canonical-to-linear!)

  (define (permutation-linear->canonical p)
    (let ((new (permutation-alloc (gsl:permutation-size p))))
      (gsl:permutation-memcpy! new p)
      (gsl:permutation-linear-to-canonical! new)
      new))

  (define (permutation-canonical->linear p)
    (let ((new (permutation-alloc (gsl:permutation-size p))))
      (gsl:permutation-memcpy! new p)
      (gsl:permutation-canonical-to-linear! new)
      new))
)
