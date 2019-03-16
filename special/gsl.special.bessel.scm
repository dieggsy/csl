(module gsl.special.bessel (bessel-J0-e
                            bessel-J0
                            bessel-J1-e bessel-J1
                            bessel-Jn-e bessel-Jn-array

                            bessel-Y0-e bessel-Y0
                            bessel-Y1-e bessel-Y1
                            bessel-Yn-e bessel-Yn-array

                            bessel-I0-e bessel-I0
                            bessel-I1-e bessel-I1
                            bessel-In-e bessel-In-array

                            bessel-I0-scaled-e bessel-I0-scaled
                            bessel-I1-scaled-e bessel-I1-scaled
                            bessel-In-scaled-e bessel-In-scaled-array

                            bessel-K0-e bessel-K0
                            bessel-K1-e bessel-K1
                            bessel-Kn-e bessel-Kn-array

                            bessel-K0-scaled-e bessel-K0-scaled
                            bessel-K1-scaled-e bessel-K1-scaled
                            bessel-Kn-scaled-e bessel-Kn-scaled-array

                            bessel-j0-e bessel-j0
                            bessel-j1-e bessel-j1
                            bessel-j2-e bessel-j2
                            bessel-jl-e bessel-jl-array
                            bessel-jl-steed-array

                            bessel-y0-e bessel-y0
                            bessel-y1-e bessel-y1
                            bessel-y2-e bessel-y2
                            bessel-yl-e bessel-yl-array

                            bessel-i0-scaled-e bessel-i0-scaled
                            bessel-i1-scaled-e bessel-i1-scaled
                            bessel-i2-scaled-e bessel-i2-scaled
                            bessel-il-scaled-e bessel-il-scaled-array

                            bessel-k0-scaled-e bessel-k0-scaled
                            bessel-k1-scaled-e bessel-k1-scaled
                            bessel-k2-scaled-e bessel-k2-scaled
                            bessel-kl-scaled-e bessel-kl-scaled-array

                            bessel-Jnu-e bessel-Jnu
                            bessel-sequence-Jnu-e

                            bessel-Ynu-e bessel-Ynu

                            bessel-Inu-scaled-e bessel-Inu-scaled
                            bessel-Inu-e bessel-Inu

                            bessel-Knu-scaled-e bessel-Knu-scaled
                            bessel-Knu-e bessel-Knu
                            bessel-Knu-scaled-e10-e
                            bessel-lnKnu-e bessel-lnKnu

                            bessel-zero-J0-e bessel-zero-J0
                            bessel-zero-J1-e bessel-zero-J1
                            bessel-zero-Jnu-e bessel-zero-Jnu)
  (import scheme
          (only chicken.base include)
          (only srfi-4 make-f64vector f64vector-length subf64vector)
          chicken.foreign
          bind)


  (include "csl-error.scm")
  (include "bind-transformers.scm")

  (foreign-declare "#include <gsl/gsl_sf_bessel.h>")

  (bind-rename/pattern "_" "-")
  (bind-rename/pattern "^gsl-sf-" "")

  ;;; Regular Cylindrical Bessel functions
  (bind "___safe int gsl_sf_bessel_J0_e(const double x,  gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_J0(const double x);")

  (bind "___safe int gsl_sf_bessel_J1_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_J1(const double x);")

  (bind "___safe int gsl_sf_bessel_Jn_e(int n, double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Jn(const int n, const double x);")

  (bind-rename "gsl_sf_bessel_Jn_array" "%bessel-Jn-array")
  (bind "___safe int gsl_sf_bessel_Jn_array(int nmin, int nmax, double x, double * result_array);")
  (define (bessel-Jn-array nmin nmax x)
    (let* ((len (+ 1 (- nmax nmin)))
           (fvec (make-f64vector len)))
      (%bessel-Jn-array nmin nmax x fvec)
      fvec))

  ;;; Irregular cylindrical bessel functions
  (bind "___safe int gsl_sf_bessel_Y0_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Y0(const double x);")

  (bind "___safe int gsl_sf_bessel_Y1_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Y1(const double x);")

  (bind "___safe int gsl_sf_bessel_Yn_e(int n,const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Yn(const int n,const double x);")

  (bind-rename "gsl_sf_bessel_Yn_array" "%bessel-Yn-array")
  (bind "___safe int gsl_sf_bessel_Yn_array(const int nmin, const int nmax, const double x, double * result_array);")
  (define (bessel-Yn-array nmin nmax x)
    (let* ((len (+ 1 (- nmax nmin)))
           (fvec (make-f64vector len)))
      (%bessel-Yn-array nmin nmax x fvec)
      fvec))

  ;;; Regular modified cylindrical bessel functions
  (bind "___safe int gsl_sf_bessel_I0_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_I0(const double x);")

  (bind "___safe int gsl_sf_bessel_I1_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_I1(const double x);")

  (bind "___safe int gsl_sf_bessel_In_e(const int n, const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_In(const int n, const double x);")

  (bind-rename "gsl_sf_bessel_In_array" "%bessel-In-array")
  (bind "___safe int gsl_sf_bessel_In_array(const int nmin, const int nmax, const double x, double * result_array);")
  (define (bessel-In-array nmin nmax x)
    (let* ((len (+ 1 (- nmax nmin)))
           (fvec (make-f64vector len)))
      (%bessel-In-array nmin nmax x fvec)
      fvec))


  (bind "___safe int gsl_sf_bessel_I0_scaled_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_I0_scaled(const double x);")

  (bind "___safe int gsl_sf_bessel_I1_scaled_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_I1_scaled(const double x);")

  (bind "___safe int gsl_sf_bessel_In_scaled_e(int n, const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_In_scaled(const int n, const double x);")

  (bind-rename "gsl_sf_bessel_In_scaled_array" "%bessel-In-scaled-array")
  (bind "___safe int gsl_sf_bessel_In_scaled_array(const int nmin, const int nmax, const double x, double * result_array);")
  (define (bessel-In-scaled-array nmin nmax x)
    (let* ((len (+ 1 (- nmax nmin)))
           (fvec (make-f64vector len)))
      (%bessel-In-scaled-array nmin nmax x fvec)
      fvec))

  ;;; Irregular modified cylindrical bessel functions
  (bind "___safe int gsl_sf_bessel_K0_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_K0(const double x);")

  (bind "___safe int gsl_sf_bessel_K1_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_K1(const double x);")

  (bind "___safe int gsl_sf_bessel_Kn_e(const int n, const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Kn(const int n, const double x);")

  (bind-rename "gsl_sf_bessel_Kn_array" "%bessel-Kn-array")
  (bind "___safe int gsl_sf_bessel_Kn_array(const int nmin, const int nmax, const double x, double * result_array);")
  (define (bessel-Kn-array nmin nmax x)
    (let* ((len (+ 1 (- nmax nmin)))
           (fvec (make-f64vector len)))
      (%bessel-Kn-array nmin nmax x fvec)
      fvec))

  (bind "___safe int gsl_sf_bessel_K0_scaled_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_K0_scaled(const double x);")

  (bind "___safe int gsl_sf_bessel_K1_scaled_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_K1_scaled(const double x);")

  (bind "___safe int gsl_sf_bessel_Kn_scaled_e(int n, const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Kn_scaled(const int n, const double x);")

  (bind-rename "gsl_sf_bessel_Kn_scaled_array" "%bessel-Kn-scaled-array")
  (bind "___safe int gsl_sf_bessel_Kn_scaled_array(const int nmin, const int nmax, const double x, double * result_array);")
  (define (bessel-Kn-scaled-array nmin nmax x)
    (let* ((len (+ 1 (- nmax nmin)))
           (fvec (make-f64vector len)))
      (%bessel-Kn-scaled-array nmin nmax x fvec)
      fvec))

  ;;; Regular Spherical Bessel Functions
  (bind "___safe int gsl_sf_bessel_j0_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_j0(const double x);")

  (bind "___safe int gsl_sf_bessel_j1_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_j1(const double x);")

  (bind "___safe int gsl_sf_bessel_j2_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_j2(const double x);")

  (bind "___safe int gsl_sf_bessel_jl_e(const int l, const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_jl(const int l, const double x);")

  (bind-rename "gsl_sf_bessel_jl_array" "%bessel-jl-array")
  (bind "___safe int gsl_sf_bessel_jl_array(const int lmax, const double x, double * result_array);")
  (define (bessel-jl-array lmax x)
    (let* ((len (+ 1 (- lmax 0)))
           (fvec (make-f64vector len)))
      (%bessel-jl-array lmax x fvec)
      fvec))

  (bind-rename "gsl_sf_bessel_jl_steed_array" "%bessel-jl-steed-array")
  (bind "___safe int gsl_sf_bessel_jl_steed_array(const int lmax, const double x, double * jl_x_array);")
  (define (bessel-jl-steed-array lmax x)
    (let* ((len (+ 1 (- lmax 0)))
           (fvec (make-f64vector len)))
      (%bessel-jl-steed-array lmax x fvec)
      fvec))

  ;;; Irregular Spherical bessel functions
  (bind "___safe int gsl_sf_bessel_y0_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_y0(const double x);")

  (bind "___safe int gsl_sf_bessel_y1_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_y1(const double x);")

  (bind "___safe int gsl_sf_bessel_y2_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_y2(const double x);")

  (bind "___safe int gsl_sf_bessel_yl_e(int l, const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_yl(const int l, const double x);")

  (bind-rename "gsl_sf_bessel_yl_array" "%bessel-yl-array")
  (bind "___safe int gsl_sf_bessel_yl_array(const int lmax, const double x, double * result_array);")
  (define (bessel-yl-array lmax x)
    (let* ((len (+ 1 (- lmax 0)))
           (fvec (make-f64vector len)))
      (%bessel-yl-array lmax x fvec)
      fvec))

  ;;; Regular modified spherical bessel functions
  (bind "___safe int gsl_sf_bessel_i0_scaled_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_i0_scaled(const double x);")

  (bind "___safe int gsl_sf_bessel_i1_scaled_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_i1_scaled(const double x);")

  (bind "___safe int gsl_sf_bessel_i2_scaled_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_i2_scaled(const double x);")

  (bind "___safe int gsl_sf_bessel_il_scaled_e(const int l, double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_il_scaled(const int l, const double x);")

  (bind-rename "gsl_sf_bessel_il_scaled_array" "%bessel-il-scaled-array")
  (bind "___safe int gsl_sf_bessel_il_scaled_array(const int lmax, const double x, double * result_array);")
  (define (bessel-il-scaled-array lmax x)
    (let* ((len (+ 1 (- lmax 0)))
           (fvec (make-f64vector len)))
      (%bessel-il-scaled-array lmax x fvec)
      fvec))

  ;;; Irregular modified spherical bessel functions
  (bind "___safe int gsl_sf_bessel_k0_scaled_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_k0_scaled(const double x);")

  (bind "___safe int gsl_sf_bessel_k1_scaled_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_k1_scaled(const double x);")

  (bind "___safe int gsl_sf_bessel_k2_scaled_e(const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_k2_scaled(const double x);")

  (bind "___safe int gsl_sf_bessel_kl_scaled_e(int l, const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_kl_scaled(const int l, const double x);")

  (bind-rename "gsl_sf_bessel_kl_scaled_array" "%bessel-kl-scaled-array")
  (bind "___safe int gsl_sf_bessel_kl_scaled_array(const int lmax, const double x, double * result_array);")
  (define (bessel-kl-scaled-array lmax x)
    (let* ((len (+ 1 (- lmax 0)))
           (fvec (make-f64vector len)))
      (%bessel-kl-scaled-array lmax x fvec)
      fvec))

  ;;; Regular Bessel function - fractional order
  (bind "___safe int gsl_sf_bessel_Jnu_e(const double nu, const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Jnu(const double nu, const double x);")

  (bind "___safe int gsl_sf_bessel_sequence_Jnu_e(double nu, gsl_mode_t mode, ___length(v) size_t size, double * v);")

  ;;; Irregular bessel functions - fractional order
  (bind "___safe int gsl_sf_bessel_Ynu_e(double nu, double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Ynu(const double nu, const double x);")

  ;;; Regular Modified Bessel Functions—Fractional Order
  (bind "___safe int gsl_sf_bessel_Inu_scaled_e(double nu, double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Inu_scaled(double nu, double x);")

  (bind "___safe int gsl_sf_bessel_Inu_e(double nu, double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Inu(double nu, double x);")

  (bind "___safe int gsl_sf_bessel_Knu_scaled_e(const double nu, const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Knu_scaled(const double nu, const double x);")

  (bind "___safe int gsl_sf_bessel_Knu_scaled_e10_e(const double nu, const double x, gsl_sf_result_e10 * result);")

  (bind "___safe int gsl_sf_bessel_Knu_e(const double nu, const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_Knu(const double nu, const double x);")

  (bind "___safe int gsl_sf_bessel_lnKnu_e(const double nu, const double x, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_lnKnu(const double nu, const double x);")

  ;;; Zeros of Regular Bessel Functions
  (bind "___safe int gsl_sf_bessel_zero_J0_e(unsigned int s, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_zero_J0(unsigned int s);")

  (bind "___safe int gsl_sf_bessel_zero_J1_e(unsigned int s, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_zero_J1(unsigned int s);")

  (bind "___safe int gsl_sf_bessel_zero_Jnu_e(double nu, unsigned int s, gsl_sf_result * result);")
  (bind "___safe double gsl_sf_bessel_zero_Jnu(double nu, unsigned int s);"))
