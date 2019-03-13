(module gsl.special.airy (airy-Ai
                          airy-Ai-e
                          airy-Bi airy-Bi-e
                          airy-Ai-scaled airy-Ai-scaled-e
                          airy-Bi-scaled airy-Bi-scaled-e
                          airy-Ai-deriv airy-Ai-deriv-e
                          airy-Bi-deriv airy-Bi-deriv-e
                          airy-Ai-deriv-scaled airy-Ai-deriv-scaled-e
                          airy-Bi-deriv-scaled airy-Bi-deriv-scaled-e
                          airy-zero-Ai airy-zero-Ai-e
                          airy-zero-Bi airy-zero-Bi-e
                          airy-zero-Ai-deriv airy-zero-Ai-deriv-e
                          airy-zero-Bi-deriv airy-zero-Bi-deriv-e)
  (import scheme
          (only chicken.base include include-relative)
          chicken.foreign
          bind)


  (include "csl-error.scm")
  (include "bind-transformers.scm")
  ;; (include-relative "bind-sfres.scm")

  (foreign-declare "#include <gsl/gsl_sf_airy.h>")

  ;; (bind-options default-renaming: #f)
  (bind-rename/pattern "_" "-")
  (bind-rename/pattern "^gsl-sf-" "")

  ;;; Airy Functions
  (bind "___safe double gsl_sf_airy_Ai(double, gsl_mode_t)")
  (bind "___safe double gsl_sf_airy_Ai_e(double, gsl_mode_t, gsl_sf_result*)")
  (bind "___safe double gsl_sf_airy_Bi(double, gsl_mode_t)")
  (bind "___safe double gsl_sf_airy_Bi_e(double, gsl_mode_t, gsl_sf_result*)")

  (bind "___safe double gsl_sf_airy_Ai_scaled(double, gsl_mode_t)")
  (bind "___safe double gsl_sf_airy_Ai_scaled_e(double, gsl_mode_t, gsl_sf_result*)")

  (bind "___safe double gsl_sf_airy_Bi_scaled(double, gsl_mode_t)")
  (bind "___safe double gsl_sf_airy_Bi_scaled_e(double, gsl_mode_t, gsl_sf_result*)")

  ;;; Derivatives of Airy Functions
  (bind "___safe double gsl_sf_airy_Ai_deriv(double, gsl_mode_t)")
  (bind "___safe double gsl_sf_airy_Ai_deriv_e(double, gsl_mode_t, gsl_sf_result*)")

  (bind "___safe double gsl_sf_airy_Bi_deriv(double, gsl_mode_t)")
  (bind "___safe double gsl_sf_airy_Bi_deriv_e(double, gsl_mode_t, gsl_sf_result*)")

  (bind "___safe double gsl_sf_airy_Ai_deriv_scaled(double, gsl_mode_t)")
  (bind "___safe double gsl_sf_airy_Ai_deriv_scaled_e(double, gsl_mode_t, gsl_sf_result*)")

  (bind "___safe double gsl_sf_airy_Bi_deriv_scaled(double, gsl_mode_t)")
  (bind "___safe double gsl_sf_airy_Bi_deriv_scaled_e(double, gsl_mode_t, gsl_sf_result*)")

  ;;; Zeros of airy functions
  (bind "___safe double gsl_sf_airy_zero_Ai(unsigned int)")
  (bind "___safe double gsl_sf_airy_zero_Ai_e(unsigned int, gsl_sf_result*)")

  (bind "___safe double gsl_sf_airy_zero_Bi(unsigned int)")
  (bind "___safe double gsl_sf_airy_zero_Bi_e(unsigned int, gsl_sf_result*)")

  ;; ;;; Zeros of derivatives of airy functions
  (bind "___safe double gsl_sf_airy_zero_Ai_deriv(unsigned int)")
  (bind "___safe double gsl_sf_airy_zero_Ai_deriv_e(unsigned int, gsl_sf_result*)")

  (bind "___safe double gsl_sf_airy_zero_Bi_deriv(unsigned int)")
  (bind "___safe double gsl_sf_airy_zero_Bi_deriv_e(unsigned int, gsl_sf_result*)"))
