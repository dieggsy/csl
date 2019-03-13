(module gsl.cgsm (speed-of-light
                  gravitational-constant
                  plancks-constant-h
                  plancks-constant-hbar
                  astronomical-unit
                  light-year
                  parsec
                  grav-accel
                  electron-volt
                  mass-electron
                  mass-muon
                  mass-proton
                  mass-neutron
                  rydberg
                  boltzmann
                  molar-gas
                  standard-gas-volume
                  minute
                  hour
                  day
                  week
                  inch
                  foot
                  yard
                  mile
                  nautical-mile
                  fathom
                  mil
                  point
                  texpoint
                  micron
                  angstrom
                  hectare
                  acre
                  barn
                  liter
                  us-gallon
                  quart
                  pint
                  cup
                  fluid-ounce
                  tablespoon
                  teaspoon
                  canadian-gallon
                  uk-gallon
                  miles-per-hour
                  kilometers-per-hour
                  knot
                  pound-mass
                  ounce-mass
                  ton
                  metric-ton
                  uk-ton
                  troy-ounce
                  carat
                  unified-atomic-mass
                  gram-force
                  pound-force
                  kilopound-force
                  poundal
                  calorie
                  btu
                  therm
                  horsepower
                  bar
                  std-atmosphere
                  torr
                  meter-of-mercury
                  inch-of-mercury
                  inch-of-water
                  psi
                  poise
                  stokes
                  stilb
                  lumen
                  lux
                  phot
                  footcandle
                  lambert
                  footlambert
                  curie
                  roentgen
                  rad
                  solar-mass
                  bohr-radius
                  newton
                  dyne
                  joule
                  erg
                  stefan-boltzmann-constant
                  thomson-cross-section
                  bohr-magneton
                  nuclear-magneton
                  electron-magnetic-moment
                  proton-magnetic-moment
                  faraday
                  electron-charge)

  (import scheme chicken.foreign bind)

  (foreign-declare "#include <gsl/gsl_const_cgsm.h>")

  (bind-options default-renaming: "")

  (bind-rename/pattern "^gsl-const-cgsm-" "")

  (bind "const double GSL_CONST_CGSM_SPEED_OF_LIGHT")
  (bind "const double GSL_CONST_CGSM_GRAVITATIONAL_CONSTANT")
  (bind "const double GSL_CONST_CGSM_PLANCKS_CONSTANT_H")
  (bind "const double GSL_CONST_CGSM_PLANCKS_CONSTANT_HBAR")
  (bind "const double GSL_CONST_CGSM_ASTRONOMICAL_UNIT")
  (bind "const double GSL_CONST_CGSM_LIGHT_YEAR")
  (bind "const double GSL_CONST_CGSM_PARSEC")
  (bind "const double GSL_CONST_CGSM_GRAV_ACCEL")
  (bind "const double GSL_CONST_CGSM_ELECTRON_VOLT")
  (bind "const double GSL_CONST_CGSM_MASS_ELECTRON")
  (bind "const double GSL_CONST_CGSM_MASS_MUON")
  (bind "const double GSL_CONST_CGSM_MASS_PROTON")
  (bind "const double GSL_CONST_CGSM_MASS_NEUTRON")
  (bind "const double GSL_CONST_CGSM_RYDBERG")
  (bind "const double GSL_CONST_CGSM_BOLTZMANN")
  (bind "const double GSL_CONST_CGSM_MOLAR_GAS")
  (bind "const double GSL_CONST_CGSM_STANDARD_GAS_VOLUME")
  (bind "const double GSL_CONST_CGSM_MINUTE")
  (bind "const double GSL_CONST_CGSM_HOUR")
  (bind "const double GSL_CONST_CGSM_DAY")
  (bind "const double GSL_CONST_CGSM_WEEK")
  (bind "const double GSL_CONST_CGSM_INCH")
  (bind "const double GSL_CONST_CGSM_FOOT")
  (bind "const double GSL_CONST_CGSM_YARD")
  (bind "const double GSL_CONST_CGSM_MILE")
  (bind "const double GSL_CONST_CGSM_NAUTICAL_MILE")
  (bind "const double GSL_CONST_CGSM_FATHOM")
  (bind "const double GSL_CONST_CGSM_MIL")
  (bind "const double GSL_CONST_CGSM_POINT")
  (bind "const double GSL_CONST_CGSM_TEXPOINT")
  (bind "const double GSL_CONST_CGSM_MICRON")
  (bind "const double GSL_CONST_CGSM_ANGSTROM")
  (bind "const double GSL_CONST_CGSM_HECTARE")
  (bind "const double GSL_CONST_CGSM_ACRE")
  (bind "const double GSL_CONST_CGSM_BARN")
  (bind "const double GSL_CONST_CGSM_LITER")
  (bind "const double GSL_CONST_CGSM_US_GALLON")
  (bind "const double GSL_CONST_CGSM_QUART")
  (bind "const double GSL_CONST_CGSM_PINT")
  (bind "const double GSL_CONST_CGSM_CUP")
  (bind "const double GSL_CONST_CGSM_FLUID_OUNCE")
  (bind "const double GSL_CONST_CGSM_TABLESPOON")
  (bind "const double GSL_CONST_CGSM_TEASPOON")
  (bind "const double GSL_CONST_CGSM_CANADIAN_GALLON")
  (bind "const double GSL_CONST_CGSM_UK_GALLON")
  (bind "const double GSL_CONST_CGSM_MILES_PER_HOUR")
  (bind "const double GSL_CONST_CGSM_KILOMETERS_PER_HOUR")
  (bind "const double GSL_CONST_CGSM_KNOT")
  (bind "const double GSL_CONST_CGSM_POUND_MASS")
  (bind "const double GSL_CONST_CGSM_OUNCE_MASS")
  (bind "const double GSL_CONST_CGSM_TON")
  (bind "const double GSL_CONST_CGSM_METRIC_TON")
  (bind "const double GSL_CONST_CGSM_UK_TON")
  (bind "const double GSL_CONST_CGSM_TROY_OUNCE")
  (bind "const double GSL_CONST_CGSM_CARAT")
  (bind "const double GSL_CONST_CGSM_UNIFIED_ATOMIC_MASS")
  (bind "const double GSL_CONST_CGSM_GRAM_FORCE")
  (bind "const double GSL_CONST_CGSM_POUND_FORCE")
  (bind "const double GSL_CONST_CGSM_KILOPOUND_FORCE")
  (bind "const double GSL_CONST_CGSM_POUNDAL")
  (bind "const double GSL_CONST_CGSM_CALORIE")
  (bind "const double GSL_CONST_CGSM_BTU")
  (bind "const double GSL_CONST_CGSM_THERM")
  (bind "const double GSL_CONST_CGSM_HORSEPOWER")
  (bind "const double GSL_CONST_CGSM_BAR")
  (bind "const double GSL_CONST_CGSM_STD_ATMOSPHERE")
  (bind "const double GSL_CONST_CGSM_TORR")
  (bind "const double GSL_CONST_CGSM_METER_OF_MERCURY")
  (bind "const double GSL_CONST_CGSM_INCH_OF_MERCURY")
  (bind "const double GSL_CONST_CGSM_INCH_OF_WATER")
  (bind "const double GSL_CONST_CGSM_PSI")
  (bind "const double GSL_CONST_CGSM_POISE")
  (bind "const double GSL_CONST_CGSM_STOKES")
  (bind "const double GSL_CONST_CGSM_STILB")
  (bind "const double GSL_CONST_CGSM_LUMEN")
  (bind "const double GSL_CONST_CGSM_LUX")
  (bind "const double GSL_CONST_CGSM_PHOT")
  (bind "const double GSL_CONST_CGSM_FOOTCANDLE")
  (bind "const double GSL_CONST_CGSM_LAMBERT")
  (bind "const double GSL_CONST_CGSM_FOOTLAMBERT")
  (bind "const double GSL_CONST_CGSM_CURIE")
  (bind "const double GSL_CONST_CGSM_ROENTGEN")
  (bind "const double GSL_CONST_CGSM_RAD")
  (bind "const double GSL_CONST_CGSM_SOLAR_MASS")
  (bind "const double GSL_CONST_CGSM_BOHR_RADIUS")
  (bind "const double GSL_CONST_CGSM_NEWTON")
  (bind "const double GSL_CONST_CGSM_DYNE")
  (bind "const double GSL_CONST_CGSM_JOULE")
  (bind "const double GSL_CONST_CGSM_ERG")
  (bind "const double GSL_CONST_CGSM_STEFAN_BOLTZMANN_CONSTANT")
  (bind "const double GSL_CONST_CGSM_THOMSON_CROSS_SECTION")
  (bind "const double GSL_CONST_CGSM_BOHR_MAGNETON")
  (bind "const double GSL_CONST_CGSM_NUCLEAR_MAGNETON")
  (bind "const double GSL_CONST_CGSM_ELECTRON_MAGNETIC_MOMENT")
  (bind "const double GSL_CONST_CGSM_PROTON_MAGNETIC_MOMENT")
  (bind "const double GSL_CONST_CGSM_FARADAY")
  (bind "const double GSL_CONST_CGSM_ELECTRON_CHARGE"))
