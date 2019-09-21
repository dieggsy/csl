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

  (import scheme chicken.foreign)

  (foreign-declare "#include <gsl/gsl_const_num.h>")

  (define fine-structure (foreign-value "GSL_CONST_NUM_FINE_STRUCTURE" (const double)))
  (define avogadro (foreign-value "GSL_CONST_NUM_AVOGADRO" (const double)))
  (define yotta (foreign-value "GSL_CONST_NUM_YOTTA" (const double)))
  (define zetta (foreign-value "GSL_CONST_NUM_ZETTA" (const double)))
  (define exa (foreign-value "GSL_CONST_NUM_EXA" (const double)))
  (define peta (foreign-value "GSL_CONST_NUM_PETA" (const double)))
  (define tera (foreign-value "GSL_CONST_NUM_TERA" (const double)))
  (define giga (foreign-value "GSL_CONST_NUM_GIGA" (const double)))
  (define mega (foreign-value "GSL_CONST_NUM_MEGA" (const double)))
  (define kilo (foreign-value "GSL_CONST_NUM_KILO" (const double)))
  (define milli (foreign-value "GSL_CONST_NUM_MILLI" (const double)))
  (define micro (foreign-value "GSL_CONST_NUM_MICRO" (const double)))
  (define nano (foreign-value "GSL_CONST_NUM_NANO" (const double)))
  (define pico (foreign-value "GSL_CONST_NUM_PICO" (const double)))
  (define femto (foreign-value "GSL_CONST_NUM_FEMTO" (const double)))
  (define atto (foreign-value "GSL_CONST_NUM_ATTO" (const double)))
  (define zepto (foreign-value "GSL_CONST_NUM_ZEPTO" (const double)))
  (define yocto (foreign-value "GSL_CONST_NUM_YOCTO" (const double))))
