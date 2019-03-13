(module gsl.num (fine-structure
                 avogadro
                 yotta
                 zetta
                 exa
                 peta
                 tera
                 giga
                 mega
                 kilo
                 milli
                 micro
                 nano
                 pico
                 femto
                 atto
                 zepto
                 yocto)

  (import scheme chicken.foreign bind)

  (foreign-declare "#include <gsl/gsl_const_num.h>")

  (bind-options default-renaming: "")

  (bind-rename/pattern "^gsl-const-num-" "")

  (bind "const double GSL_CONST_NUM_FINE_STRUCTURE")
  (bind "const double GSL_CONST_NUM_AVOGADRO")
  (bind "const double GSL_CONST_NUM_YOTTA")
  (bind "const double GSL_CONST_NUM_ZETTA")
  (bind "const double GSL_CONST_NUM_EXA")
  (bind "const double GSL_CONST_NUM_PETA")
  (bind "const double GSL_CONST_NUM_TERA")
  (bind "const double GSL_CONST_NUM_GIGA")
  (bind "const double GSL_CONST_NUM_MEGA")
  (bind "const double GSL_CONST_NUM_KILO")
  (bind "const double GSL_CONST_NUM_MILLI")
  (bind "const double GSL_CONST_NUM_MICRO")
  (bind "const double GSL_CONST_NUM_NANO")
  (bind "const double GSL_CONST_NUM_PICO")
  (bind "const double GSL_CONST_NUM_FEMTO")
  (bind "const double GSL_CONST_NUM_ATTO")
  (bind "const double GSL_CONST_NUM_ZEPTO")
  (bind "const double GSL_CONST_NUM_YOCTO"))
