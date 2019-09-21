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

  (import scheme chicken.foreign)

  (foreign-declare "#include <gsl/gsl_const_cgsm.h>")

  (define speed-of-light (foreign-value "GSL_CONST_CGSM_SPEED_OF_LIGHT" (const double)))
  (define gravitational-constant (foreign-value "GSL_CONST_CGSM_GRAVITATIONAL_CONSTANT" (const double)))
  (define plancks-constant-h (foreign-value "GSL_CONST_CGSM_PLANCKS_CONSTANT_H" (const double)))
  (define plancks-constant-hbar (foreign-value "GSL_CONST_CGSM_PLANCKS_CONSTANT_HBAR" (const double)))
  (define astronomical-unit (foreign-value "GSL_CONST_CGSM_ASTRONOMICAL_UNIT" (const double)))
  (define light-year (foreign-value "GSL_CONST_CGSM_LIGHT_YEAR" (const double)))
  (define parsec (foreign-value "GSL_CONST_CGSM_PARSEC" (const double)))
  (define grav-accel (foreign-value "GSL_CONST_CGSM_GRAV_ACCEL" (const double)))
  (define electron-volt (foreign-value "GSL_CONST_CGSM_ELECTRON_VOLT" (const double)))
  (define mass-electron (foreign-value "GSL_CONST_CGSM_MASS_ELECTRON" (const double)))
  (define mass-muon (foreign-value "GSL_CONST_CGSM_MASS_MUON" (const double)))
  (define mass-proton (foreign-value "GSL_CONST_CGSM_MASS_PROTON" (const double)))
  (define mass-neutron (foreign-value "GSL_CONST_CGSM_MASS_NEUTRON" (const double)))
  (define rydberg (foreign-value "GSL_CONST_CGSM_RYDBERG" (const double)))
  (define boltzmann (foreign-value "GSL_CONST_CGSM_BOLTZMANN" (const double)))
  (define molar-gas (foreign-value "GSL_CONST_CGSM_MOLAR_GAS" (const double)))
  (define standard-gas-volume (foreign-value "GSL_CONST_CGSM_STANDARD_GAS_VOLUME" (const double)))
  (define minute (foreign-value "GSL_CONST_CGSM_MINUTE" (const double)))
  (define hour (foreign-value "GSL_CONST_CGSM_HOUR" (const double)))
  (define day (foreign-value "GSL_CONST_CGSM_DAY" (const double)))
  (define week (foreign-value "GSL_CONST_CGSM_WEEK" (const double)))
  (define inch (foreign-value "GSL_CONST_CGSM_INCH" (const double)))
  (define foot (foreign-value "GSL_CONST_CGSM_FOOT" (const double)))
  (define yard (foreign-value "GSL_CONST_CGSM_YARD" (const double)))
  (define mile (foreign-value "GSL_CONST_CGSM_MILE" (const double)))
  (define nautical-mile (foreign-value "GSL_CONST_CGSM_NAUTICAL_MILE" (const double)))
  (define fathom (foreign-value "GSL_CONST_CGSM_FATHOM" (const double)))
  (define mil (foreign-value "GSL_CONST_CGSM_MIL" (const double)))
  (define point (foreign-value "GSL_CONST_CGSM_POINT" (const double)))
  (define texpoint (foreign-value "GSL_CONST_CGSM_TEXPOINT" (const double)))
  (define micron (foreign-value "GSL_CONST_CGSM_MICRON" (const double)))
  (define angstrom (foreign-value "GSL_CONST_CGSM_ANGSTROM" (const double)))
  (define hectare (foreign-value "GSL_CONST_CGSM_HECTARE" (const double)))
  (define acre (foreign-value "GSL_CONST_CGSM_ACRE" (const double)))
  (define barn (foreign-value "GSL_CONST_CGSM_BARN" (const double)))
  (define liter (foreign-value "GSL_CONST_CGSM_LITER" (const double)))
  (define us-gallon (foreign-value "GSL_CONST_CGSM_US_GALLON" (const double)))
  (define quart (foreign-value "GSL_CONST_CGSM_QUART" (const double)))
  (define pint (foreign-value "GSL_CONST_CGSM_PINT" (const double)))
  (define cup (foreign-value "GSL_CONST_CGSM_CUP" (const double)))
  (define fluid-ounce (foreign-value "GSL_CONST_CGSM_FLUID_OUNCE" (const double)))
  (define tablespoon (foreign-value "GSL_CONST_CGSM_TABLESPOON" (const double)))
  (define teaspoon (foreign-value "GSL_CONST_CGSM_TEASPOON" (const double)))
  (define canadian-gallon (foreign-value "GSL_CONST_CGSM_CANADIAN_GALLON" (const double)))
  (define uk-gallon (foreign-value "GSL_CONST_CGSM_UK_GALLON" (const double)))
  (define miles-per-hour (foreign-value "GSL_CONST_CGSM_MILES_PER_HOUR" (const double)))
  (define kilometers-per-hour (foreign-value "GSL_CONST_CGSM_KILOMETERS_PER_HOUR" (const double)))
  (define knot (foreign-value "GSL_CONST_CGSM_KNOT" (const double)))
  (define pound-mass (foreign-value "GSL_CONST_CGSM_POUND_MASS" (const double)))
  (define ounce-mass (foreign-value "GSL_CONST_CGSM_OUNCE_MASS" (const double)))
  (define ton (foreign-value "GSL_CONST_CGSM_TON" (const double)))
  (define metric-ton (foreign-value "GSL_CONST_CGSM_METRIC_TON" (const double)))
  (define uk-ton (foreign-value "GSL_CONST_CGSM_UK_TON" (const double)))
  (define troy-ounce (foreign-value "GSL_CONST_CGSM_TROY_OUNCE" (const double)))
  (define carat (foreign-value "GSL_CONST_CGSM_CARAT" (const double)))
  (define unified-atomic-mass (foreign-value "GSL_CONST_CGSM_UNIFIED_ATOMIC_MASS" (const double)))
  (define gram-force (foreign-value "GSL_CONST_CGSM_GRAM_FORCE" (const double)))
  (define pound-force (foreign-value "GSL_CONST_CGSM_POUND_FORCE" (const double)))
  (define kilopound-force (foreign-value "GSL_CONST_CGSM_KILOPOUND_FORCE" (const double)))
  (define poundal (foreign-value "GSL_CONST_CGSM_POUNDAL" (const double)))
  (define calorie (foreign-value "GSL_CONST_CGSM_CALORIE" (const double)))
  (define btu (foreign-value "GSL_CONST_CGSM_BTU" (const double)))
  (define therm (foreign-value "GSL_CONST_CGSM_THERM" (const double)))
  (define horsepower (foreign-value "GSL_CONST_CGSM_HORSEPOWER" (const double)))
  (define bar (foreign-value "GSL_CONST_CGSM_BAR" (const double)))
  (define std-atmosphere (foreign-value "GSL_CONST_CGSM_STD_ATMOSPHERE" (const double)))
  (define torr (foreign-value "GSL_CONST_CGSM_TORR" (const double)))
  (define meter-of-mercury (foreign-value "GSL_CONST_CGSM_METER_OF_MERCURY" (const double)))
  (define inch-of-mercury (foreign-value "GSL_CONST_CGSM_INCH_OF_MERCURY" (const double)))
  (define inch-of-water (foreign-value "GSL_CONST_CGSM_INCH_OF_WATER" (const double)))
  (define psi (foreign-value "GSL_CONST_CGSM_PSI" (const double)))
  (define poise (foreign-value "GSL_CONST_CGSM_POISE" (const double)))
  (define stokes (foreign-value "GSL_CONST_CGSM_STOKES" (const double)))
  (define stilb (foreign-value "GSL_CONST_CGSM_STILB" (const double)))
  (define lumen (foreign-value "GSL_CONST_CGSM_LUMEN" (const double)))
  (define lux (foreign-value "GSL_CONST_CGSM_LUX" (const double)))
  (define phot (foreign-value "GSL_CONST_CGSM_PHOT" (const double)))
  (define footcandle (foreign-value "GSL_CONST_CGSM_FOOTCANDLE" (const double)))
  (define lambert (foreign-value "GSL_CONST_CGSM_LAMBERT" (const double)))
  (define footlambert (foreign-value "GSL_CONST_CGSM_FOOTLAMBERT" (const double)))
  (define curie (foreign-value "GSL_CONST_CGSM_CURIE" (const double)))
  (define roentgen (foreign-value "GSL_CONST_CGSM_ROENTGEN" (const double)))
  (define rad (foreign-value "GSL_CONST_CGSM_RAD" (const double)))
  (define solar-mass (foreign-value "GSL_CONST_CGSM_SOLAR_MASS" (const double)))
  (define bohr-radius (foreign-value "GSL_CONST_CGSM_BOHR_RADIUS" (const double)))
  (define newton (foreign-value "GSL_CONST_CGSM_NEWTON" (const double)))
  (define dyne (foreign-value "GSL_CONST_CGSM_DYNE" (const double)))
  (define joule (foreign-value "GSL_CONST_CGSM_JOULE" (const double)))
  (define erg (foreign-value "GSL_CONST_CGSM_ERG" (const double)))
  (define stefan-boltzmann-constant (foreign-value "GSL_CONST_CGSM_STEFAN_BOLTZMANN_CONSTANT" (const double)))
  (define thomson-cross-section (foreign-value "GSL_CONST_CGSM_THOMSON_CROSS_SECTION" (const double)))
  (define bohr-magneton (foreign-value "GSL_CONST_CGSM_BOHR_MAGNETON" (const double)))
  (define nuclear-magneton (foreign-value "GSL_CONST_CGSM_NUCLEAR_MAGNETON" (const double)))
  (define electron-magnetic-moment (foreign-value "GSL_CONST_CGSM_ELECTRON_MAGNETIC_MOMENT" (const double)))
  (define proton-magnetic-moment (foreign-value "GSL_CONST_CGSM_PROTON_MAGNETIC_MOMENT" (const double)))
  (define faraday (foreign-value "GSL_CONST_CGSM_FARADAY" (const double)))
  (define electron-charge (foreign-value "GSL_CONST_CGSM_ELECTRON_CHARGE" (const double))))
