(module csl.num (fine-structure
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

  (import scheme (chicken foreign))

  (foreign-declare "#include <gsl/gsl_const_num.h>")

  (define fine-structure (foreign-value "GSL_CONST_NUM_FINE_STRUCTURE" double))
  (define avogadro (foreign-value "GSL_CONST_NUM_AVOGADRO" double))
  (define yotta (foreign-value "GSL_CONST_NUM_YOTTA" double))
  (define zetta (foreign-value "GSL_CONST_NUM_ZETTA" double))
  (define exa (foreign-value "GSL_CONST_NUM_EXA" double))
  (define peta (foreign-value "GSL_CONST_NUM_PETA" double))
  (define tera (foreign-value "GSL_CONST_NUM_TERA" double))
  (define giga (foreign-value "GSL_CONST_NUM_GIGA" double))
  (define mega (foreign-value "GSL_CONST_NUM_MEGA" double))
  (define kilo (foreign-value "GSL_CONST_NUM_KILO" double))
  (define milli (foreign-value "GSL_CONST_NUM_MILLI" double))
  (define micro (foreign-value "GSL_CONST_NUM_MICRO" double))
  (define nano (foreign-value "GSL_CONST_NUM_NANO" double))
  (define pico (foreign-value "GSL_CONST_NUM_PICO" double))
  (define femto (foreign-value "GSL_CONST_NUM_FEMTO" double))
  (define atto (foreign-value "GSL_CONST_NUM_ATTO" double))
  (define zepto (foreign-value "GSL_CONST_NUM_ZEPTO" double))
  (define yocto (foreign-value "GSL_CONST_NUM_YOCTO" double)))
