/*
Unit conversions.

Orion Sky Lawlor, olawlor@acm.org, 2006/06/12
  from public domain osl/units.h
*/
#ifndef __ASF_UNITS_H
#define __ASF_UNITS_H

#ifndef PI
# define PI 3.141592653589793238462643383279502884197169399375105820974944592307
#endif
#ifndef M_PI
# define M_PI PI
#endif

#define RADIANS_FROM_DEGREES (M_PI/180.0)
#define DEGREES_FROM_RADIANS (180.0/M_PI)

/** Speed of light--exact value (meters/second) */
#define SPEED_OF_LIGHT 2.997924562e8

/** Gravitational constant ( newton meter^2 / kilogram^2 ) */
#define GRAVITATIONAL_CONSTANT 6.6725985e-11

#if 0 /* These are all probably useless, and do take up space in every .o file.
  Uncomment as needed */
namespace constants {

/// Meters per centimeter (exact)
const double metersFmCm=1.0/100.0;

/// Centimeters per US inch (exact)
const double cmFmInch=2.54;

/// Meters per US inch (exact)
const double metersFmInch=metersFmCm*cmFmInch;

/// Inches per US feet (exact)
const double inchFmFeet=12.0;

/// Meters per US feet (exact)
const double metersFmFeet=metersFmInch*inchFmFeet;

/// Speed of light in meters per second (exact)
const double metersFmSeconds = 299792458.0; 

/// Zero of the Celsius scale in Kelvin (exact)
const double celciusFmKelvin = 273.15;

/// Standard atmosphere atm in Pascals (exact)
const double atmFmPa = 101325.0;

/// Ratio of circumference to diameter [pure]
const double pi=3.14159265358979323;

/// Speed of light in vacuum [ meters/second ]
const double c=constants::metersFmSeconds;

/// Planck's constant [ Joule-seconds ]
const double h=6.6260687652e-34;
/// Planck's constant, barred (Dirac's constant)
const double hBar=h/(2*pi);

/// Boltzmann constant [ Joule / Kelvin ]
const double boltzmann=1.380650324e-23;
/// Avogadro's number [ atoms per mole ]
const double avagadro=6.0221419947e+23;

/// Stefan-Boltzmann constant [ Watts / (meter^2 K^4) ]
const double stefanBoltzmann=hBar*c/boltzmann;

/// Gravitational constant [ newton meter^2 / kilogram^2 ]
const double g=6.6725985e-11;
};
#endif

#endif
