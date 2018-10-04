(module csl.mksa (speed-of-light
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

  (import scheme (chicken foreign))

  (foreign-declare "#include <gsl/gsl_const_mksa.h>")

  (define speed-of-light (foreign-value "GSL_CONST_MKSA_SPEED_OF_LIGHT" double))
  (define gravitational-constant (foreign-value "GSL_CONST_MKSA_GRAVITATIONAL_CONSTANT" double))
  (define plancks-constant-h (foreign-value "GSL_CONST_MKSA_PLANCKS_CONSTANT_H" double))
  (define plancks-constant-hbar (foreign-value "GSL_CONST_MKSA_PLANCKS_CONSTANT_HBAR" double))
  (define astronomical-unit (foreign-value "GSL_CONST_MKSA_ASTRONOMICAL_UNIT" double))
  (define light-year (foreign-value "GSL_CONST_MKSA_LIGHT_YEAR" double))
  (define parsec (foreign-value "GSL_CONST_MKSA_PARSEC" double))
  (define grav-accel (foreign-value "GSL_CONST_MKSA_GRAV_ACCEL" double))
  (define electron-volt (foreign-value "GSL_CONST_MKSA_ELECTRON_VOLT" double))
  (define mass-electron (foreign-value "GSL_CONST_MKSA_MASS_ELECTRON" double))
  (define mass-muon (foreign-value "GSL_CONST_MKSA_MASS_MUON" double))
  (define mass-proton (foreign-value "GSL_CONST_MKSA_MASS_PROTON" double))
  (define mass-neutron (foreign-value "GSL_CONST_MKSA_MASS_NEUTRON" double))
  (define rydberg (foreign-value "GSL_CONST_MKSA_RYDBERG" double))
  (define boltzmann (foreign-value "GSL_CONST_MKSA_BOLTZMANN" double))
  (define molar-gas (foreign-value "GSL_CONST_MKSA_MOLAR_GAS" double))
  (define standard-gas-volume (foreign-value "GSL_CONST_MKSA_STANDARD_GAS_VOLUME" double))
  (define minute (foreign-value "GSL_CONST_MKSA_MINUTE" double))
  (define hour (foreign-value "GSL_CONST_MKSA_HOUR" double))
  (define day (foreign-value "GSL_CONST_MKSA_DAY" double))
  (define week (foreign-value "GSL_CONST_MKSA_WEEK" double))
  (define inch (foreign-value "GSL_CONST_MKSA_INCH" double))
  (define foot (foreign-value "GSL_CONST_MKSA_FOOT" double))
  (define yard (foreign-value "GSL_CONST_MKSA_YARD" double))
  (define mile (foreign-value "GSL_CONST_MKSA_MILE" double))
  (define nautical-mile (foreign-value "GSL_CONST_MKSA_NAUTICAL_MILE" double))
  (define fathom (foreign-value "GSL_CONST_MKSA_FATHOM" double))
  (define mil (foreign-value "GSL_CONST_MKSA_MIL" double))
  (define point (foreign-value "GSL_CONST_MKSA_POINT" double))
  (define texpoint (foreign-value "GSL_CONST_MKSA_TEXPOINT" double))
  (define micron (foreign-value "GSL_CONST_MKSA_MICRON" double))
  (define angstrom (foreign-value "GSL_CONST_MKSA_ANGSTROM" double))
  (define hectare (foreign-value "GSL_CONST_MKSA_HECTARE" double))
  (define acre (foreign-value "GSL_CONST_MKSA_ACRE" double))
  (define barn (foreign-value "GSL_CONST_MKSA_BARN" double))
  (define liter (foreign-value "GSL_CONST_MKSA_LITER" double))
  (define us-gallon (foreign-value "GSL_CONST_MKSA_US_GALLON" double))
  (define quart (foreign-value "GSL_CONST_MKSA_QUART" double))
  (define pint (foreign-value "GSL_CONST_MKSA_PINT" double))
  (define cup (foreign-value "GSL_CONST_MKSA_CUP" double))
  (define fluid-ounce (foreign-value "GSL_CONST_MKSA_FLUID_OUNCE" double))
  (define tablespoon (foreign-value "GSL_CONST_MKSA_TABLESPOON" double))
  (define teaspoon (foreign-value "GSL_CONST_MKSA_TEASPOON" double))
  (define canadian-gallon (foreign-value "GSL_CONST_MKSA_CANADIAN_GALLON" double))
  (define uk-gallon (foreign-value "GSL_CONST_MKSA_UK_GALLON" double))
  (define miles-per-hour (foreign-value "GSL_CONST_MKSA_MILES_PER_HOUR" double))
  (define kilometers-per-hour (foreign-value "GSL_CONST_MKSA_KILOMETERS_PER_HOUR" double))
  (define knot (foreign-value "GSL_CONST_MKSA_KNOT" double))
  (define pound-mass (foreign-value "GSL_CONST_MKSA_POUND_MASS" double))
  (define ounce-mass (foreign-value "GSL_CONST_MKSA_OUNCE_MASS" double))
  (define ton (foreign-value "GSL_CONST_MKSA_TON" double))
  (define metric-ton (foreign-value "GSL_CONST_MKSA_METRIC_TON" double))
  (define uk-ton (foreign-value "GSL_CONST_MKSA_UK_TON" double))
  (define troy-ounce (foreign-value "GSL_CONST_MKSA_TROY_OUNCE" double))
  (define carat (foreign-value "GSL_CONST_MKSA_CARAT" double))
  (define unified-atomic-mass (foreign-value "GSL_CONST_MKSA_UNIFIED_ATOMIC_MASS" double))
  (define gram-force (foreign-value "GSL_CONST_MKSA_GRAM_FORCE" double))
  (define pound-force (foreign-value "GSL_CONST_MKSA_POUND_FORCE" double))
  (define kilopound-force (foreign-value "GSL_CONST_MKSA_KILOPOUND_FORCE" double))
  (define poundal (foreign-value "GSL_CONST_MKSA_POUNDAL" double))
  (define calorie (foreign-value "GSL_CONST_MKSA_CALORIE" double))
  (define btu (foreign-value "GSL_CONST_MKSA_BTU" double))
  (define therm (foreign-value "GSL_CONST_MKSA_THERM" double))
  (define horsepower (foreign-value "GSL_CONST_MKSA_HORSEPOWER" double))
  (define bar (foreign-value "GSL_CONST_MKSA_BAR" double))
  (define std-atmosphere (foreign-value "GSL_CONST_MKSA_STD_ATMOSPHERE" double))
  (define torr (foreign-value "GSL_CONST_MKSA_TORR" double))
  (define meter-of-mercury (foreign-value "GSL_CONST_MKSA_METER_OF_MERCURY" double))
  (define inch-of-mercury (foreign-value "GSL_CONST_MKSA_INCH_OF_MERCURY" double))
  (define inch-of-water (foreign-value "GSL_CONST_MKSA_INCH_OF_WATER" double))
  (define psi (foreign-value "GSL_CONST_MKSA_PSI" double))
  (define poise (foreign-value "GSL_CONST_MKSA_POISE" double))
  (define stokes (foreign-value "GSL_CONST_MKSA_STOKES" double))
  (define stilb (foreign-value "GSL_CONST_MKSA_STILB" double))
  (define lumen (foreign-value "GSL_CONST_MKSA_LUMEN" double))
  (define lux (foreign-value "GSL_CONST_MKSA_LUX" double))
  (define phot (foreign-value "GSL_CONST_MKSA_PHOT" double))
  (define footcandle (foreign-value "GSL_CONST_MKSA_FOOTCANDLE" double))
  (define lambert (foreign-value "GSL_CONST_MKSA_LAMBERT" double))
  (define footlambert (foreign-value "GSL_CONST_MKSA_FOOTLAMBERT" double))
  (define curie (foreign-value "GSL_CONST_MKSA_CURIE" double))
  (define roentgen (foreign-value "GSL_CONST_MKSA_ROENTGEN" double))
  (define rad (foreign-value "GSL_CONST_MKSA_RAD" double))
  (define solar-mass (foreign-value "GSL_CONST_MKSA_SOLAR_MASS" double))
  (define bohr-radius (foreign-value "GSL_CONST_MKSA_BOHR_RADIUS" double))
  (define newton (foreign-value "GSL_CONST_MKSA_NEWTON" double))
  (define dyne (foreign-value "GSL_CONST_MKSA_DYNE" double))
  (define joule (foreign-value "GSL_CONST_MKSA_JOULE" double))
  (define erg (foreign-value "GSL_CONST_MKSA_ERG" double))
  (define stefan-boltzmann-constant (foreign-value "GSL_CONST_MKSA_STEFAN_BOLTZMANN_CONSTANT" double))
  (define thomson-cross-section (foreign-value "GSL_CONST_MKSA_THOMSON_CROSS_SECTION" double))
  (define bohr-magneton (foreign-value "GSL_CONST_MKSA_BOHR_MAGNETON" double))
  (define nuclear-magneton (foreign-value "GSL_CONST_MKSA_NUCLEAR_MAGNETON" double))
  (define electron-magnetic-moment (foreign-value "GSL_CONST_MKSA_ELECTRON_MAGNETIC_MOMENT" double))
  (define proton-magnetic-moment (foreign-value "GSL_CONST_MKSA_PROTON_MAGNETIC_MOMENT" double))
  (define faraday (foreign-value "GSL_CONST_MKSA_FARADAY" double))
  (define electron-charge (foreign-value "GSL_CONST_MKSA_ELECTRON_CHARGE" double))
  (define vacuum-permittivity (foreign-value "GSL_CONST_MKSA_VACUUM_PERMITTIVITY" double))
  (define vacuum-permeability (foreign-value "GSL_CONST_MKSA_VACUUM_PERMEABILITY" double))
  (define debye (foreign-value "GSL_CONST_MKSA_DEBYE" double))
  (define gauss (foreign-value "GSL_CONST_MKSA_GAUSS" double)))
