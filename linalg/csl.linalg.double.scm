(module csl.linalg.double (LU-decomp)
  (import scheme
          csl.matrix
          gsl.linalg.double)

  (reexport gsl.linalg.double)

  (define (LU-decomp A p)
    (let ((B (matrix-copy A)))
      (let-values  (((p signum) (LU-decomp! B p)))
        (values B p signum))))
  )
