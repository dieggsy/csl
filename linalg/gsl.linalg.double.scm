(module gsl.linalg.double (LU-decomp!
                           LU-solve
                           LU-svx!
                           LU-refine!
                           LU-invert
                           LU-det
                           LU-lndet
                           LU-sgndet
                           QR-decomp!
                           QR-solve
                           QR-svx!
                           QR-lssolve
                           QR-QTvec!
                           QR-Qvec!
                           QR-QTmat!
                           QR-Rsolve
                           QR-Rsvx
                           QR-unpack)
  (import scheme
          bind
          gsl.permutation
          chicken.foreign
          (only chicken.base include let*-values)
          gsl.vector.double
          gsl.matrix.double)

  (include "csl-error.scm")
  (include "bind-transformers.scm")

  (foreign-declare "#include <gsl/gsl_linalg.h>")

  (bind-rename/pattern "_" "-")
  (bind-rename/pattern "^gsl-linalg-" "")

  (bind-type
   csl_vector
   (c-pointer "gsl_vector")
   vector->ptr
   ptr->vector)

  (bind-type
   csl_matrix
   (c-pointer "gsl_matrix")
   matrix->ptr
   ptr->matrix)

  (bind-type
   csl_permutation
   (c-pointer "gsl_permutation")
   permutation->ptr
   ptr->permutation)

  (bind-rename/pattern "^(.*?)-(solve|refine|invert|decomp)$" "%\\1-\\2")

  ;; LU Decomposition
  (bind "___safe int gsl_linalg_LU_decomp (csl_matrix A, csl_permutation p, ___out int *signum);")
  (define (LU-decomp! A)
    (let*-values (((p) (permutation-alloc (matrix-size1 A)))
                  ((_ signum) (%LU-decomp A p)))
      (values p signum)))

  (bind "___safe int gsl_linalg_LU_solve (csl_matrix LU,csl_permutation p,csl_vector b,csl_vector x);")
  (define (LU-solve LU p b)
    (let ((x (vector-alloc (vector-size b))))
      (%LU-solve LU p b x)
      x))

  (bind-rename/pattern "(svx)" "\\1!")
  (bind "___safe int gsl_linalg_LU_svx(csl_matrix LU, csl_permutation p, csl_vector x)")

  (bind "___safe int gsl_linalg_LU_refine(csl_matrix A, csl_matrix LU, csl_permutation p, csl_vector b, csl_vector x, csl_vector work)")
  (define (LU-refine! A LU p b x)
    (let ((work (vector-alloc (matrix-size1 A))))
      (%LU-refine A LU p b x work)))

  (bind "___safe int gsl_linalg_LU_invert(csl_matrix LU, csl_permutation p, csl_matrix inverse)");
  (define (LU-invert LU p)
    (let ((m (matrix-alloc (matrix-size1 LU) (matrix-size2 LU))))
      (%LU-invert LU p m)
      m))

  (bind "double gsl_linalg_LU_det(csl_matrix LU, int signum)")
  (bind "double gsl_linalg_LU_lndet(csl_matrix LU)")
  (bind "int gsl_linalg_LU_sgndet(csl_matrix LU, int signum)")

  ;; QR Decomposition
  (bind "___safe int gsl_linalg_QR_decomp (csl_matrix A, csl_vector tau);")
  (define (QR-decomp! A)
    (let ((tau (vector-alloc (min (matrix-size2 A) (matrix-size1 A)))))
      (%QR-decomp A tau)
      tau))

  (bind "___safe int gsl_linalg_QR_solve (csl_matrix QR,csl_vector tau,csl_vector b,csl_vector x);")
  (define (QR-solve QR tau b)
    (let ((x (vector-alloc (vector-size b))))
      (%QR-solve QR tau b x)
      x))

  (bind "___safe int gsl_linalg_QR_svx(csl_matrix QR, csl_vector tau, csl_vector x)")

  (bind-rename/pattern "^(.*?)-(lssolve|Rsolve|unpack)$" "%\\1-\\2")

  (bind "___safe int gsl_linalg_QR_lssolve(csl_matrix QR, csl_vector tau, csl_vector b, csl_vector x, csl_vector residual)")
  (define (QR-lssolve QR tau b)
    (let ((x (vector-alloc (vector-size b)))
          (residual (vector-alloc (vector-size b))))
      (values x residual)))

  (bind-rename/pattern "(QT?vec|QTmat)" "\\1!")

  (bind "___safe int gsl_linalg_QR_QTvec(csl_matrix QR, csl_vector tau, csl_vector v)")

  (bind "___safe int gsl_linalg_QR_Qvec(csl_matrix QR, csl_vector tau, csl_vector v)")

  (bind "___safe int gsl_linalg_QR_QTmat(csl_matrix QR, csl_vector tau, csl_matrix A)")

  (bind "___safe int gsl_linalg_QR_Rsolve(csl_matrix QR, csl_vector b, csl_vector x)")
  (define (QR-Rsolve QR b)
    (let ((x (vector-alloc (vector-size b))))
      (%QR-Rsolve QR b x)
      x))

  (bind "___safe int gsl_linalg_QR_Rsvx(csl_matrix QR, csl_vector x)")
  (bind "___safe int gsl_linalg_QR_unpack(csl_matrix QR, csl_vector tau, csl_matrix Q, csl_matrix R)")
  (define (QR-unpack QR tau)
    (let* ((m (matrix-size1 QR))
           (n (matrix-size2 QR))
           (Q (matrix-alloc m m))
           (R (matrix-alloc m n)))
      (%QR-unpack QR tau Q R)
      (values Q R)))

  ;; (bind "___safe int gsl_linalg_QRPT_rank(csl_matrix QR, double tol)")

  ;; (bind-rename/pattern "^(.*?)-(rcond)$" "%\\1-\\2")
  ;; (bind "___safe int gsl_linalg_QRPT_rcond(csl_matrix, double tol)")

  ;; (define (QRPT))

  )
