(module csl.cgsm (speed-of-light
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

  (import scheme (chicken foreign))

  (foreign-declare "#include <gsl/gsl_const_cgsm.h>")

  (define speed-of-light (foreign-value "GSL_CONST_CGSM_SPEED_OF_LIGHT" double))
  (define gravitational-constant (foreign-value "GSL_CONST_CGSM_GRAVITATIONAL_CONSTANT" double))
  (define plancks-constant-h (foreign-value "GSL_CONST_CGSM_PLANCKS_CONSTANT_H" double))
  (define plancks-constant-hbar (foreign-value "GSL_CONST_CGSM_PLANCKS_CONSTANT_HBAR" double))
  (define astronomical-unit (foreign-value "GSL_CONST_CGSM_ASTRONOMICAL_UNIT" double))
  (define light-year (foreign-value "GSL_CONST_CGSM_LIGHT_YEAR" double))
  (define parsec (foreign-value "GSL_CONST_CGSM_PARSEC" double))
  (define grav-accel (foreign-value "GSL_CONST_CGSM_GRAV_ACCEL" double))
  (define electron-volt (foreign-value "GSL_CONST_CGSM_ELECTRON_VOLT" double))
  (define mass-electron (foreign-value "GSL_CONST_CGSM_MASS_ELECTRON" double))
  (define mass-muon (foreign-value "GSL_CONST_CGSM_MASS_MUON" double))
  (define mass-proton (foreign-value "GSL_CONST_CGSM_MASS_PROTON" double))
  (define mass-neutron (foreign-value "GSL_CONST_CGSM_MASS_NEUTRON" double))
  (define rydberg (foreign-value "GSL_CONST_CGSM_RYDBERG" double))
  (define boltzmann (foreign-value "GSL_CONST_CGSM_BOLTZMANN" double))
  (define molar-gas (foreign-value "GSL_CONST_CGSM_MOLAR_GAS" double))
  (define standard-gas-volume (foreign-value "GSL_CONST_CGSM_STANDARD_GAS_VOLUME" double))
  (define minute (foreign-value "GSL_CONST_CGSM_MINUTE" double))
  (define hour (foreign-value "GSL_CONST_CGSM_HOUR" double))
  (define day (foreign-value "GSL_CONST_CGSM_DAY" double))
  (define week (foreign-value "GSL_CONST_CGSM_WEEK" double))
  (define inch (foreign-value "GSL_CONST_CGSM_INCH" double))
  (define foot (foreign-value "GSL_CONST_CGSM_FOOT" double))
  (define yard (foreign-value "GSL_CONST_CGSM_YARD" double))
  (define mile (foreign-value "GSL_CONST_CGSM_MILE" double))
  (define nautical-mile (foreign-value "GSL_CONST_CGSM_NAUTICAL_MILE" double))
  (define fathom (foreign-value "GSL_CONST_CGSM_FATHOM" double))
  (define mil (foreign-value "GSL_CONST_CGSM_MIL" double))
  (define point (foreign-value "GSL_CONST_CGSM_POINT" double))
  (define texpoint (foreign-value "GSL_CONST_CGSM_TEXPOINT" double))
  (define micron (foreign-value "GSL_CONST_CGSM_MICRON" double))
  (define angstrom (foreign-value "GSL_CONST_CGSM_ANGSTROM" double))
  (define hectare (foreign-value "GSL_CONST_CGSM_HECTARE" double))
  (define acre (foreign-value "GSL_CONST_CGSM_ACRE" double))
  (define barn (foreign-value "GSL_CONST_CGSM_BARN" double))
  (define liter (foreign-value "GSL_CONST_CGSM_LITER" double))
  (define us-gallon (foreign-value "GSL_CONST_CGSM_US_GALLON" double))
  (define quart (foreign-value "GSL_CONST_CGSM_QUART" double))
  (define pint (foreign-value "GSL_CONST_CGSM_PINT" double))
  (define cup (foreign-value "GSL_CONST_CGSM_CUP" double))
  (define fluid-ounce (foreign-value "GSL_CONST_CGSM_FLUID_OUNCE" double))
  (define tablespoon (foreign-value "GSL_CONST_CGSM_TABLESPOON" double))
  (define teaspoon (foreign-value "GSL_CONST_CGSM_TEASPOON" double))
  (define canadian-gallon (foreign-value "GSL_CONST_CGSM_CANADIAN_GALLON" double))
  (define uk-gallon (foreign-value "GSL_CONST_CGSM_UK_GALLON" double))
  (define miles-per-hour (foreign-value "GSL_CONST_CGSM_MILES_PER_HOUR" double))
  (define kilometers-per-hour (foreign-value "GSL_CONST_CGSM_KILOMETERS_PER_HOUR" double))
  (define knot (foreign-value "GSL_CONST_CGSM_KNOT" double))
  (define pound-mass (foreign-value "GSL_CONST_CGSM_POUND_MASS" double))
  (define ounce-mass (foreign-value "GSL_CONST_CGSM_OUNCE_MASS" double))
  (define ton (foreign-value "GSL_CONST_CGSM_TON" double))
  (define metric-ton (foreign-value "GSL_CONST_CGSM_METRIC_TON" double))
  (define uk-ton (foreign-value "GSL_CONST_CGSM_UK_TON" double))
  (define troy-ounce (foreign-value "GSL_CONST_CGSM_TROY_OUNCE" double))
  (define carat (foreign-value "GSL_CONST_CGSM_CARAT" double))
  (define unified-atomic-mass (foreign-value "GSL_CONST_CGSM_UNIFIED_ATOMIC_MASS" double))
  (define gram-force (foreign-value "GSL_CONST_CGSM_GRAM_FORCE" double))
  (define pound-force (foreign-value "GSL_CONST_CGSM_POUND_FORCE" double))
  (define kilopound-force (foreign-value "GSL_CONST_CGSM_KILOPOUND_FORCE" double))
  (define poundal (foreign-value "GSL_CONST_CGSM_POUNDAL" double))
  (define calorie (foreign-value "GSL_CONST_CGSM_CALORIE" double))
  (define btu (foreign-value "GSL_CONST_CGSM_BTU" double))
  (define therm (foreign-value "GSL_CONST_CGSM_THERM" double))
  (define horsepower (foreign-value "GSL_CONST_CGSM_HORSEPOWER" double))
  (define bar (foreign-value "GSL_CONST_CGSM_BAR" double))
  (define std-atmosphere (foreign-value "GSL_CONST_CGSM_STD_ATMOSPHERE" double))
  (define torr (foreign-value "GSL_CONST_CGSM_TORR" double))
  (define meter-of-mercury (foreign-value "GSL_CONST_CGSM_METER_OF_MERCURY" double))
  (define inch-of-mercury (foreign-value "GSL_CONST_CGSM_INCH_OF_MERCURY" double))
  (define inch-of-water (foreign-value "GSL_CONST_CGSM_INCH_OF_WATER" double))
  (define psi (foreign-value "GSL_CONST_CGSM_PSI" double))
  (define poise (foreign-value "GSL_CONST_CGSM_POISE" double))
  (define stokes (foreign-value "GSL_CONST_CGSM_STOKES" double))
  (define stilb (foreign-value "GSL_CONST_CGSM_STILB" double))
  (define lumen (foreign-value "GSL_CONST_CGSM_LUMEN" double))
  (define lux (foreign-value "GSL_CONST_CGSM_LUX" double))
  (define phot (foreign-value "GSL_CONST_CGSM_PHOT" double))
  (define footcandle (foreign-value "GSL_CONST_CGSM_FOOTCANDLE" double))
  (define lambert (foreign-value "GSL_CONST_CGSM_LAMBERT" double))
  (define footlambert (foreign-value "GSL_CONST_CGSM_FOOTLAMBERT" double))
  (define curie (foreign-value "GSL_CONST_CGSM_CURIE" double))
  (define roentgen (foreign-value "GSL_CONST_CGSM_ROENTGEN" double))
  (define rad (foreign-value "GSL_CONST_CGSM_RAD" double))
  (define solar-mass (foreign-value "GSL_CONST_CGSM_SOLAR_MASS" double))
  (define bohr-radius (foreign-value "GSL_CONST_CGSM_BOHR_RADIUS" double))
  (define newton (foreign-value "GSL_CONST_CGSM_NEWTON" double))
  (define dyne (foreign-value "GSL_CONST_CGSM_DYNE" double))
  (define joule (foreign-value "GSL_CONST_CGSM_JOULE" double))
  (define erg (foreign-value "GSL_CONST_CGSM_ERG" double))
  (define stefan-boltzmann-constant (foreign-value "GSL_CONST_CGSM_STEFAN_BOLTZMANN_CONSTANT" double))
  (define thomson-cross-section (foreign-value "GSL_CONST_CGSM_THOMSON_CROSS_SECTION" double))
  (define bohr-magneton (foreign-value "GSL_CONST_CGSM_BOHR_MAGNETON" double))
  (define nuclear-magneton (foreign-value "GSL_CONST_CGSM_NUCLEAR_MAGNETON" double))
  (define electron-magnetic-moment (foreign-value "GSL_CONST_CGSM_ELECTRON_MAGNETIC_MOMENT" double))
  (define proton-magnetic-moment (foreign-value "GSL_CONST_CGSM_PROTON_MAGNETIC_MOMENT" double))
  (define faraday (foreign-value "GSL_CONST_CGSM_FARADAY" double))
  (define electron-charge (foreign-value "GSL_CONST_CGSM_ELECTRON_CHARGE" double)))
