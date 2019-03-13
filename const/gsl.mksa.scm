(module gsl.mksa (speed-of-light
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
                  electron-charge
                  vacuum-permittivity
                  vacuum-permeability
                  debye
                  gauss)

  (import scheme chicken.foreign bind)

  (foreign-declare "#include <gsl/gsl_const_mksa.h>")

  (bind-options default-renaming: "")

  (bind-rename/pattern "^gsl-const-mksa-" "")

  (bind "const double GSL_CONST_MKSA_SPEED_OF_LIGHT")
  (bind "const double GSL_CONST_MKSA_GRAVITATIONAL_CONSTANT")
  (bind "const double GSL_CONST_MKSA_PLANCKS_CONSTANT_H")
  (bind "const double GSL_CONST_MKSA_PLANCKS_CONSTANT_HBAR")
  (bind "const double GSL_CONST_MKSA_ASTRONOMICAL_UNIT")
  (bind "const double GSL_CONST_MKSA_LIGHT_YEAR")
  (bind "const double GSL_CONST_MKSA_PARSEC")
  (bind "const double GSL_CONST_MKSA_GRAV_ACCEL")
  (bind "const double GSL_CONST_MKSA_ELECTRON_VOLT")
  (bind "const double GSL_CONST_MKSA_MASS_ELECTRON")
  (bind "const double GSL_CONST_MKSA_MASS_MUON")
  (bind "const double GSL_CONST_MKSA_MASS_PROTON")
  (bind "const double GSL_CONST_MKSA_MASS_NEUTRON")
  (bind "const double GSL_CONST_MKSA_RYDBERG")
  (bind "const double GSL_CONST_MKSA_BOLTZMANN")
  (bind "const double GSL_CONST_MKSA_MOLAR_GAS")
  (bind "const double GSL_CONST_MKSA_STANDARD_GAS_VOLUME")
  (bind "const double GSL_CONST_MKSA_MINUTE")
  (bind "const double GSL_CONST_MKSA_HOUR")
  (bind "const double GSL_CONST_MKSA_DAY")
  (bind "const double GSL_CONST_MKSA_WEEK")
  (bind "const double GSL_CONST_MKSA_INCH")
  (bind "const double GSL_CONST_MKSA_FOOT")
  (bind "const double GSL_CONST_MKSA_YARD")
  (bind "const double GSL_CONST_MKSA_MILE")
  (bind "const double GSL_CONST_MKSA_NAUTICAL_MILE")
  (bind "const double GSL_CONST_MKSA_FATHOM")
  (bind "const double GSL_CONST_MKSA_MIL")
  (bind "const double GSL_CONST_MKSA_POINT")
  (bind "const double GSL_CONST_MKSA_TEXPOINT")
  (bind "const double GSL_CONST_MKSA_MICRON")
  (bind "const double GSL_CONST_MKSA_ANGSTROM")
  (bind "const double GSL_CONST_MKSA_HECTARE")
  (bind "const double GSL_CONST_MKSA_ACRE")
  (bind "const double GSL_CONST_MKSA_BARN")
  (bind "const double GSL_CONST_MKSA_LITER")
  (bind "const double GSL_CONST_MKSA_US_GALLON")
  (bind "const double GSL_CONST_MKSA_QUART")
  (bind "const double GSL_CONST_MKSA_PINT")
  (bind "const double GSL_CONST_MKSA_CUP")
  (bind "const double GSL_CONST_MKSA_FLUID_OUNCE")
  (bind "const double GSL_CONST_MKSA_TABLESPOON")
  (bind "const double GSL_CONST_MKSA_TEASPOON")
  (bind "const double GSL_CONST_MKSA_CANADIAN_GALLON")
  (bind "const double GSL_CONST_MKSA_UK_GALLON")
  (bind "const double GSL_CONST_MKSA_MILES_PER_HOUR")
  (bind "const double GSL_CONST_MKSA_KILOMETERS_PER_HOUR")
  (bind "const double GSL_CONST_MKSA_KNOT")
  (bind "const double GSL_CONST_MKSA_POUND_MASS")
  (bind "const double GSL_CONST_MKSA_OUNCE_MASS")
  (bind "const double GSL_CONST_MKSA_TON")
  (bind "const double GSL_CONST_MKSA_METRIC_TON")
  (bind "const double GSL_CONST_MKSA_UK_TON")
  (bind "const double GSL_CONST_MKSA_TROY_OUNCE")
  (bind "const double GSL_CONST_MKSA_CARAT")
  (bind "const double GSL_CONST_MKSA_UNIFIED_ATOMIC_MASS")
  (bind "const double GSL_CONST_MKSA_GRAM_FORCE")
  (bind "const double GSL_CONST_MKSA_POUND_FORCE")
  (bind "const double GSL_CONST_MKSA_KILOPOUND_FORCE")
  (bind "const double GSL_CONST_MKSA_POUNDAL")
  (bind "const double GSL_CONST_MKSA_CALORIE")
  (bind "const double GSL_CONST_MKSA_BTU")
  (bind "const double GSL_CONST_MKSA_THERM")
  (bind "const double GSL_CONST_MKSA_HORSEPOWER")
  (bind "const double GSL_CONST_MKSA_BAR")
  (bind "const double GSL_CONST_MKSA_STD_ATMOSPHERE")
  (bind "const double GSL_CONST_MKSA_TORR")
  (bind "const double GSL_CONST_MKSA_METER_OF_MERCURY")
  (bind "const double GSL_CONST_MKSA_INCH_OF_MERCURY")
  (bind "const double GSL_CONST_MKSA_INCH_OF_WATER")
  (bind "const double GSL_CONST_MKSA_PSI")
  (bind "const double GSL_CONST_MKSA_POISE")
  (bind "const double GSL_CONST_MKSA_STOKES")
  (bind "const double GSL_CONST_MKSA_STILB")
  (bind "const double GSL_CONST_MKSA_LUMEN")
  (bind "const double GSL_CONST_MKSA_LUX")
  (bind "const double GSL_CONST_MKSA_PHOT")
  (bind "const double GSL_CONST_MKSA_FOOTCANDLE")
  (bind "const double GSL_CONST_MKSA_LAMBERT")
  (bind "const double GSL_CONST_MKSA_FOOTLAMBERT")
  (bind "const double GSL_CONST_MKSA_CURIE")
  (bind "const double GSL_CONST_MKSA_ROENTGEN")
  (bind "const double GSL_CONST_MKSA_RAD")
  (bind "const double GSL_CONST_MKSA_SOLAR_MASS")
  (bind "const double GSL_CONST_MKSA_BOHR_RADIUS")
  (bind "const double GSL_CONST_MKSA_NEWTON")
  (bind "const double GSL_CONST_MKSA_DYNE")
  (bind "const double GSL_CONST_MKSA_JOULE")
  (bind "const double GSL_CONST_MKSA_ERG")
  (bind "const double GSL_CONST_MKSA_STEFAN_BOLTZMANN_CONSTANT")
  (bind "const double GSL_CONST_MKSA_THOMSON_CROSS_SECTION")
  (bind "const double GSL_CONST_MKSA_BOHR_MAGNETON")
  (bind "const double GSL_CONST_MKSA_NUCLEAR_MAGNETON")
  (bind "const double GSL_CONST_MKSA_ELECTRON_MAGNETIC_MOMENT")
  (bind "const double GSL_CONST_MKSA_PROTON_MAGNETIC_MOMENT")
  (bind "const double GSL_CONST_MKSA_FARADAY")
  (bind "const double GSL_CONST_MKSA_ELECTRON_CHARGE")
  (bind "const double GSL_CONST_MKSA_VACUUM_PERMITTIVITY")
  (bind "const double GSL_CONST_MKSA_VACUUM_PERMEABILITY")
  (bind "const double GSL_CONST_MKSA_DEBYE")
  (bind "const double GSL_CONST_MKSA_GAUSS"))
