(module gsl.special.clausen (clausen clausen-e)
  (import scheme
          (only chicken.base include)
          chicken.foreign
          bind)

  (include "csl-error.scm")
  (include "bind-transformers.scm")

  (foreign-declare "#include <gsl/gsl_sf_clausen.h>")

  (bind-rename/pattern "_" "-")
  (bind-rename/pattern "^gsl-sf-" "")

  (bind "___safe double gsl_sf_clausen(double)")
  (bind "___safe double gsl_sf_clausen_e(double, gsl_sf_result*)"))
