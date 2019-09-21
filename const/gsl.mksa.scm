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

  (import scheme chicken.foreign)

  (foreign-declare "#include <gsl/gsl_const_mksa.h>")

  (define speed-of-light (foreign-value "GSL_CONST_MKSA_SPEED_OF_LIGHT" (const double)))
  (define gravitational-constant (foreign-value "GSL_CONST_MKSA_GRAVITATIONAL_CONSTANT" (const double)))
  (define plancks-constant-h (foreign-value "GSL_CONST_MKSA_PLANCKS_CONSTANT_H" (const double)))
  (define plancks-constant-hbar (foreign-value "GSL_CONST_MKSA_PLANCKS_CONSTANT_HBAR" (const double)))
  (define astronomical-unit (foreign-value "GSL_CONST_MKSA_ASTRONOMICAL_UNIT" (const double)))
  (define light-year (foreign-value "GSL_CONST_MKSA_LIGHT_YEAR" (const double)))
  (define parsec (foreign-value "GSL_CONST_MKSA_PARSEC" (const double)))
  (define grav-accel (foreign-value "GSL_CONST_MKSA_GRAV_ACCEL" (const double)))
  (define electron-volt (foreign-value "GSL_CONST_MKSA_ELECTRON_VOLT" (const double)))
  (define mass-electron (foreign-value "GSL_CONST_MKSA_MASS_ELECTRON" (const double)))
  (define mass-muon (foreign-value "GSL_CONST_MKSA_MASS_MUON" (const double)))
  (define mass-proton (foreign-value "GSL_CONST_MKSA_MASS_PROTON" (const double)))
  (define mass-neutron (foreign-value "GSL_CONST_MKSA_MASS_NEUTRON" (const double)))
  (define rydberg (foreign-value "GSL_CONST_MKSA_RYDBERG" (const double)))
  (define boltzmann (foreign-value "GSL_CONST_MKSA_BOLTZMANN" (const double)))
  (define molar-gas (foreign-value "GSL_CONST_MKSA_MOLAR_GAS" (const double)))
  (define standard-gas-volume (foreign-value "GSL_CONST_MKSA_STANDARD_GAS_VOLUME" (const double)))
  (define minute (foreign-value "GSL_CONST_MKSA_MINUTE" (const double)))
  (define hour (foreign-value "GSL_CONST_MKSA_HOUR" (const double)))
  (define day (foreign-value "GSL_CONST_MKSA_DAY" (const double)))
  (define week (foreign-value "GSL_CONST_MKSA_WEEK" (const double)))
  (define inch (foreign-value "GSL_CONST_MKSA_INCH" (const double)))
  (define foot (foreign-value "GSL_CONST_MKSA_FOOT" (const double)))
  (define yard (foreign-value "GSL_CONST_MKSA_YARD" (const double)))
  (define mile (foreign-value "GSL_CONST_MKSA_MILE" (const double)))
  (define nautical-mile (foreign-value "GSL_CONST_MKSA_NAUTICAL_MILE" (const double)))
  (define fathom (foreign-value "GSL_CONST_MKSA_FATHOM" (const double)))
  (define mil (foreign-value "GSL_CONST_MKSA_MIL" (const double)))
  (define point (foreign-value "GSL_CONST_MKSA_POINT" (const double)))
  (define texpoint (foreign-value "GSL_CONST_MKSA_TEXPOINT" (const double)))
  (define micron (foreign-value "GSL_CONST_MKSA_MICRON" (const double)))
  (define angstrom (foreign-value "GSL_CONST_MKSA_ANGSTROM" (const double)))
  (define hectare (foreign-value "GSL_CONST_MKSA_HECTARE" (const double)))
  (define acre (foreign-value "GSL_CONST_MKSA_ACRE" (const double)))
  (define barn (foreign-value "GSL_CONST_MKSA_BARN" (const double)))
  (define liter (foreign-value "GSL_CONST_MKSA_LITER" (const double)))
  (define us-gallon (foreign-value "GSL_CONST_MKSA_US_GALLON" (const double)))
  (define quart (foreign-value "GSL_CONST_MKSA_QUART" (const double)))
  (define pint (foreign-value "GSL_CONST_MKSA_PINT" (const double)))
  (define cup (foreign-value "GSL_CONST_MKSA_CUP" (const double)))
  (define fluid-ounce (foreign-value "GSL_CONST_MKSA_FLUID_OUNCE" (const double)))
  (define tablespoon (foreign-value "GSL_CONST_MKSA_TABLESPOON" (const double)))
  (define teaspoon (foreign-value "GSL_CONST_MKSA_TEASPOON" (const double)))
  (define canadian-gallon (foreign-value "GSL_CONST_MKSA_CANADIAN_GALLON" (const double)))
  (define uk-gallon (foreign-value "GSL_CONST_MKSA_UK_GALLON" (const double)))
  (define miles-per-hour (foreign-value "GSL_CONST_MKSA_MILES_PER_HOUR" (const double)))
  (define kilometers-per-hour (foreign-value "GSL_CONST_MKSA_KILOMETERS_PER_HOUR" (const double)))
  (define knot (foreign-value "GSL_CONST_MKSA_KNOT" (const double)))
  (define pound-mass (foreign-value "GSL_CONST_MKSA_POUND_MASS" (const double)))
  (define ounce-mass (foreign-value "GSL_CONST_MKSA_OUNCE_MASS" (const double)))
  (define ton (foreign-value "GSL_CONST_MKSA_TON" (const double)))
  (define metric-ton (foreign-value "GSL_CONST_MKSA_METRIC_TON" (const double)))
  (define uk-ton (foreign-value "GSL_CONST_MKSA_UK_TON" (const double)))
  (define troy-ounce (foreign-value "GSL_CONST_MKSA_TROY_OUNCE" (const double)))
  (define carat (foreign-value "GSL_CONST_MKSA_CARAT" (const double)))
  (define unified-atomic-mass (foreign-value "GSL_CONST_MKSA_UNIFIED_ATOMIC_MASS" (const double)))
  (define gram-force (foreign-value "GSL_CONST_MKSA_GRAM_FORCE" (const double)))
  (define pound-force (foreign-value "GSL_CONST_MKSA_POUND_FORCE" (const double)))
  (define kilopound-force (foreign-value "GSL_CONST_MKSA_KILOPOUND_FORCE" (const double)))
  (define poundal (foreign-value "GSL_CONST_MKSA_POUNDAL" (const double)))
  (define calorie (foreign-value "GSL_CONST_MKSA_CALORIE" (const double)))
  (define btu (foreign-value "GSL_CONST_MKSA_BTU" (const double)))
  (define therm (foreign-value "GSL_CONST_MKSA_THERM" (const double)))
  (define horsepower (foreign-value "GSL_CONST_MKSA_HORSEPOWER" (const double)))
  (define bar (foreign-value "GSL_CONST_MKSA_BAR" (const double)))
  (define std-atmosphere (foreign-value "GSL_CONST_MKSA_STD_ATMOSPHERE" (const double)))
  (define torr (foreign-value "GSL_CONST_MKSA_TORR" (const double)))
  (define meter-of-mercury (foreign-value "GSL_CONST_MKSA_METER_OF_MERCURY" (const double)))
  (define inch-of-mercury (foreign-value "GSL_CONST_MKSA_INCH_OF_MERCURY" (const double)))
  (define inch-of-water (foreign-value "GSL_CONST_MKSA_INCH_OF_WATER" (const double)))
  (define psi (foreign-value "GSL_CONST_MKSA_PSI" (const double)))
  (define poise (foreign-value "GSL_CONST_MKSA_POISE" (const double)))
  (define stokes (foreign-value "GSL_CONST_MKSA_STOKES" (const double)))
  (define stilb (foreign-value "GSL_CONST_MKSA_STILB" (const double)))
  (define lumen (foreign-value "GSL_CONST_MKSA_LUMEN" (const double)))
  (define lux (foreign-value "GSL_CONST_MKSA_LUX" (const double)))
  (define phot (foreign-value "GSL_CONST_MKSA_PHOT" (const double)))
  (define footcandle (foreign-value "GSL_CONST_MKSA_FOOTCANDLE" (const double)))
  (define lambert (foreign-value "GSL_CONST_MKSA_LAMBERT" (const double)))
  (define footlambert (foreign-value "GSL_CONST_MKSA_FOOTLAMBERT" (const double)))
  (define curie (foreign-value "GSL_CONST_MKSA_CURIE" (const double)))
  (define roentgen (foreign-value "GSL_CONST_MKSA_ROENTGEN" (const double)))
  (define rad (foreign-value "GSL_CONST_MKSA_RAD" (const double)))
  (define solar-mass (foreign-value "GSL_CONST_MKSA_SOLAR_MASS" (const double)))
  (define bohr-radius (foreign-value "GSL_CONST_MKSA_BOHR_RADIUS" (const double)))
  (define newton (foreign-value "GSL_CONST_MKSA_NEWTON" (const double)))
  (define dyne (foreign-value "GSL_CONST_MKSA_DYNE" (const double)))
  (define joule (foreign-value "GSL_CONST_MKSA_JOULE" (const double)))
  (define erg (foreign-value "GSL_CONST_MKSA_ERG" (const double)))
  (define stefan-boltzmann-constant (foreign-value "GSL_CONST_MKSA_STEFAN_BOLTZMANN_CONSTANT" (const double)))
  (define thomson-cross-section (foreign-value "GSL_CONST_MKSA_THOMSON_CROSS_SECTION" (const double)))
  (define bohr-magneton (foreign-value "GSL_CONST_MKSA_BOHR_MAGNETON" (const double)))
  (define nuclear-magneton (foreign-value "GSL_CONST_MKSA_NUCLEAR_MAGNETON" (const double)))
  (define electron-magnetic-moment (foreign-value "GSL_CONST_MKSA_ELECTRON_MAGNETIC_MOMENT" (const double)))
  (define proton-magnetic-moment (foreign-value "GSL_CONST_MKSA_PROTON_MAGNETIC_MOMENT" (const double)))
  (define faraday (foreign-value "GSL_CONST_MKSA_FARADAY" (const double)))
  (define electron-charge (foreign-value "GSL_CONST_MKSA_ELECTRON_CHARGE" (const double)))
  (define vacuum-permittivity (foreign-value "GSL_CONST_MKSA_VACUUM_PERMITTIVITY" (const double)))
  (define vacuum-permeability (foreign-value "GSL_CONST_MKSA_VACUUM_PERMEABILITY" (const double)))
  (define debye (foreign-value "GSL_CONST_MKSA_DEBYE" (const double)))
  (define gauss (foreign-value "GSL_CONST_MKSA_GAUSS" (const double))))
