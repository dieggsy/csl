(module gsl.blas (sdsdot
                  sdot
                  dsdot
                  ddot
                  cdotu
                  zdotu
                  snrm2
                  sasum
                  dnrm2
                  dasum
                  scnrm2
                  scasum
                  dznrm2
                  dzasum
                  isamax
                  idamax
                  icamax
                  izamax
                  sswap!
                  scopy!
                  saxpy!
                  dswap!
                  dcopy!
                  daxpy!
                  cswap!
                  ccopy!
                  caxpy!
                  zswap!
                  zcopy!
                  zaxpy!
                  sscal!
                  dscal!
                  cscal!
                  zscal!
                  csscal!
                  zdscal!
                  srotg
                  srotmg
                  srot!
                  srotm!
                  drotg
                  drotmg
                  drot!
                  drotm!

                  ;; cblas-no-trans
                  )
  (import scheme
          bind
          chicken.foreign
          (only chicken.base include)
          (only srfi-4
                make-f64vector
                f64vector-ref
                make-f32vector
                f32vector-ref)
          (prefix gsl.vector.float float:)
          (prefix gsl.vector.double double:)
          (prefix gsl.vector.complex.float zfloat:)
          (prefix gsl.vector.complex.double zdouble:)

          (prefix gsl.matrix.float float:)
          (prefix gsl.matrix.double double:)
          (prefix gsl.matrix.complex.float zfloat:)
          (prefix gsl.matrix.complex.double zdouble:)
          )

  (include "csl-error.scm")
  (include "bind-transformers.scm")

  (foreign-declare "#include <gsl/gsl_blas.h>")

  (bind-options default-renaming: "")

  (bind-rename/pattern "^gsl-blas-" "")

  (bind-type
   csl_vector_float
   (c-pointer "gsl_vector_float")
   float:vector->ptr
   float:ptr->vector)

  (bind-type
   csl_vector
   (c-pointer "gsl_vector")
   double:vector->ptr
   double:ptr->vector)

  (bind-type
   csl_vector_complex_float
   (c-pointer "gsl_vector_complex_float")
   zfloat:vector->ptr
   zfloat:ptr->vector)

  (bind-type
   csl_vector_complex
   (c-pointer "gsl_vector_complex")
   zdouble:vector->ptr
   zdouble:ptr->vector)

  (bind-type
   csl_matrix_float
   (c-pointer "gsl_matrix_float")
   float:matrix->ptr
   float:ptr->matrix)

  (bind-type
   csl_matrix
   (c-pointer "gsl_matrix")
   double:matrix->ptr
   double:ptr->matrix)

  (bind-type
   csl_matrix_complex_float
   (c-pointer "gsl_matrix_complex_float")
   zfloat:matrix->ptr
   zfloat:ptr->matrix)

  (bind-type
   csl_matrix_complex
   (c-pointer "gsl_matrix_complex")
   zdouble:matrix->ptr
   zdouble:ptr->matrix)

  ;;; Level 1
  (bind "___safe void gsl_blas_sdsdot(float, csl_vector_float, csl_vector_float, ___out float* result)")

  (bind "___safe void gsl_blas_sdot(csl_vector_float, csl_vector_float, ___out float* result)")
  (bind "___safe void gsl_blas_dsdot(csl_vector_float, csl_vector_float, ___out double* result)")
  (bind "___safe void gsl_blas_ddot(csl_vector, csl_vector, ___out double* result)")

  ;; (bind "void gsl_blas_cdotu(csl_vector_complex_float, csl_vector_complex_float, )")
  (define (cdotu x y)
    (let ((res (make-f32vector 2)))
      ((foreign-safe-lambda* int ((c-pointer x) (c-pointer y) (f32vector dotu))
         "gsl_complex_float z;"
         "gsl_blas_cdotu(x, y, &z);"
         "dotu[0] = GSL_REAL(z);"
         "dotu[1] = GSL_IMAG(z);")
       (zfloat:vector->ptr x)
       (zfloat:vector->ptr y)
       res)
      (make-rectangular (f32vector-ref res 0)
                        (f32vector-ref res 1))))

  (define (cdotc x y)
    (let ((res (make-f32vector 2)))
      ((foreign-safe-lambda* int ((c-pointer x) (c-pointer y) (f32vector dotu))
         "gsl_complex_float z;"
         "gsl_blas_cdotc(x, y, &z);"
         "dotu[0] = GSL_REAL(z);"
         "dotu[1] = GSL_IMAG(z);")
       (zfloat:vector->ptr x)
       (zfloat:vector->ptr y)
       res)
      (make-rectangular (f32vector-ref res 0)
                        (f32vector-ref res 1))))

  (define (zdotu x y)
    (let ((res (make-f64vector 2)))
      ((foreign-safe-lambda* int ((c-pointer x) (c-pointer y) (f64vector dotu))
         "gsl_complex z;"
         "gsl_blas_zdotu(x, y, &z);"
         "dotu[0] = GSL_REAL(z);"
         "dotu[1] = GSL_IMAG(z);")
       (zdouble:vector->ptr x)
       (zdouble:vector->ptr y)
       res)
      (make-rectangular (f64vector-ref res 0)
                        (f64vector-ref res 1))))

  (define (zdotc x y)
    (let ((res (make-f64vector 2)))
      ((foreign-safe-lambda* int ((c-pointer x) (c-pointer y) (f64vector dotu))
         "gsl_complex z;"
         "gsl_blas_zdotc(x, y, &z);"
         "dotu[0] = GSL_REAL(z);"
         "dotu[1] = GSL_IMAG(z);")
       (zdouble:vector->ptr x)
       (zdouble:vector->ptr y)
       res)
      (make-rectangular (f64vector-ref res 0)
                        (f64vector-ref res 1))))


  (bind "___safe float  gsl_blas_snrm2  (csl_vector_float X);")
  (bind "___safe float  gsl_blas_sasum  (csl_vector_float X);")
  (bind "___safe double gsl_blas_dnrm2  (csl_vector X);")
  (bind "___safe double gsl_blas_dasum  (csl_vector X);")
  (bind "___safe float  gsl_blas_scnrm2 (csl_vector_complex_float X);")
  (bind "___safe float  gsl_blas_scasum (csl_vector_complex_float X);")
  (bind "___safe double gsl_blas_dznrm2 (csl_vector_complex X);")
  (bind "___safe double gsl_blas_dzasum (csl_vector_complex X);")


  (bind "___safe size_t gsl_blas_isamax (csl_vector_float X);")
  (bind "___safe size_t gsl_blas_idamax (csl_vector X);")
  (bind "___safe size_t gsl_blas_icamax (csl_vector_complex_float X);")
  (bind "___safe size_t gsl_blas_izamax (csl_vector_complex X);")

  (bind-rename/pattern "swap$" "swap!")
  (bind-rename/pattern "copy$" "copy!")
  (bind-rename/pattern "axpy$" "axpy!")
  (bind "___safe int  gsl_blas_sswap (csl_vector_float, csl_vector_float Y)")
  (bind "___safe int  gsl_blas_scopy (csl_vector_float X, csl_vector_float Y)")
  (bind "___safe int  gsl_blas_saxpy (float alpha, const csl_vector_float X, csl_vector_float Y)")

  (bind "___safe int  gsl_blas_dswap (csl_vector X, csl_vector Y)")
  (bind "___safe int  gsl_blas_dcopy (csl_vector X, csl_vector Y)")
  (bind "___safe int  gsl_blas_daxpy (double alpha, csl_vector X, csl_vector Y)")

  (bind "___safe int  gsl_blas_cswap (csl_vector_complex_float X, csl_vector_complex_float Y)")
  (bind "___safe int  gsl_blas_ccopy (csl_vector_complex_float X, csl_vector_complex_float Y)")
  (bind "___safe int  gsl_blas_caxpy (struct gsl_complex_float alpha, csl_vector_complex_float X, csl_vector_complex_float Y)")

  (bind "___safe int  gsl_blas_zswap (csl_vector_complex X, csl_vector_complex Y)")
  (bind "___safe int  gsl_blas_zcopy (csl_vector_complex X, csl_vector_complex Y)")
  (bind "___safe int  gsl_blas_zaxpy (struct gsl_complex alpha, csl_vector_complex X, csl_vector_complex Y)")

  (bind-rename/pattern "scal$" "scal!")
  (bind "___safe void gsl_blas_sscal  (float alpha, csl_vector_float X);")
  (bind "___safe void gsl_blas_dscal  (double alpha, csl_vector X);")
  (bind "___safe void gsl_blas_cscal  (struct gsl_complex_float alpha, csl_vector_complex_float X);")
  (bind "___safe void gsl_blas_zscal  (struct gsl_complex alpha, csl_vector_complex X);")
  (bind "___safe void gsl_blas_csscal (float  alpha, csl_vector_complex_float X);")
  (bind "___safe void gsl_blas_zdscal (double alpha, csl_vector_complex X);")

  (bind "void  gsl_blas_srotg (___in float a[], ___in float b[], ___out float c[], ___out float s[]);")
  (bind-rename "gsl_blas_srotmg" "%srotmg")
  (bind "___safe int  gsl_blas_srotmg (___in float d1[], ___in float d2[], ___in float b1[], float b2, float P[]);")
  (define (srotmg d1 d2 b1 b2)
    (let ((res (make-f32vector 5 0)))
      (%srotmg d1 d2 b1 b2 res)
      res))
  (bind-rename/pattern "rot$" "rot!")
  (bind-rename/pattern "rotm$" "rotm!")
  (bind "___safe int  gsl_blas_srot (csl_vector_float X, csl_vector_float Y, float c, float s);")
  (bind "___safe int  gsl_blas_srotm (csl_vector_float X, csl_vector_float Y, const float P[]);")

  (bind "___safe int  gsl_blas_drotg (___in double a[], ___in double b[], ___out double c[], ___out double s[]);")
  (bind-rename "gsl_blas_drotmg" "%drotmg")
  (bind "___safe int  gsl_blas_drotmg (___in double d1[], ___in double d2[], ___in double b1[], double b2, double P[]);")
  (define (drotmg d1 d2 b1 b2)
    (let ((res (make-f64vector 5 0)))
      (%drotmg d1 d2 b1 b2 res)
      res))
  (bind "___safe int  gsl_blas_drot (csl_vector X, csl_vector Y, const double c, const double s);")
  (bind "___safe int  gsl_blas_drotm (csl_vector X, csl_vector Y, const double P[]);")

  ;;; Level 2
  ;; (bind "const int CblasNoTrans")
  ;; (bind "const int CblasTrans")
  ;; (bind "const int CblasConjTrans")

  ;; (bind-rename/pattern "gemv$" "gemv!")
  ;; (bind "int gsl_blas_sgemv(int TransA, float alpha, csl_matrix_float A, csl_vector_float x, float beta, csl_vector_float y)")



  )
