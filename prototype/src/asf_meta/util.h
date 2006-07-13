/*
Metadata utility routines.

Orion Sky Lawlor, olawlor@acm.org, 2006/07/13 (ASF)
*/
#ifndef __ASF_META_UTIL_H
#define __ASF_META_UTIL_H

#include "asf/dll_support.h" /* for ASF_COREDLL */
#include "osl/vector2d.h"
#include "osl/vector3d.h"
#include "asf_meta/enum.h"
#include <string>

namespace asf {

/***************** Datatypes used for metadata handling *****************/
/** 1D metadata value */
typedef double meta1D_t;
/** 2D metadata value */
typedef osl::Vector2d meta2D_t;
/** 3D metadata value */
typedef osl::Vector3d meta3D_t;
/** Metadata string value */
typedef std::string meta_string_t;
/** A "glob" is a user-defined struct */
typedef const void *meta_glob_t;
typedef int meta_int_t;

/** State vector: the position and velocity of some object; typically a satellite. */
class ASF_COREDLL meta_state_t {
public:
	/** Position of object in 3D space (meters) */
	meta3D_t pos;
	/** Velocity of object in 3D space (meters/second) */
	meta3D_t vel;
	
	meta_state_t() {}
	meta_state_t(const meta3D_t &p,const meta3D_t &v) 
		:pos(p), vel(v) {}

	/** Rotate this state vector about the Z axis counterclockwise by angle phi
	  degrees, then add Coriolis velocity corresponding to a counterclockwise
	  frame rotation rate  of omega radians per second. This is the core of a
	  fixed-Earth to inertial (and back) transform.  */
	meta_state_t rotate_coriolis(double phi,double omega) const;
};

/** Backward compatability with old "geolocate.h" header routines. */
typedef meta3D_t vector;
typedef meta_state_t stateVector;

ASF_COREDLL vector vecNew(double x,double y,double z);
ASF_COREDLL void vecAdd(const vector a,const vector b, vector *c);
ASF_COREDLL void vecSub(const vector a,const vector b, vector *c);
ASF_COREDLL void vecScale(vector *v,double scale);
ASF_COREDLL double vecMagnitude(const vector v);
ASF_COREDLL void vecNormalize(vector *v);
ASF_COREDLL double vecDot(const vector a,const vector b);
ASF_COREDLL void vecCross(const vector a,const vector b,vector *aXb);
ASF_COREDLL void vecMul(const vector *matrix,const vector src,vector *dest);

ASF_COREDLL double atan2_check(double y, double x);
ASF_COREDLL void cart2sph(const vector v,double *r_out,double *theta,double *phi);
ASF_COREDLL void sph2cart(double r,double theta,double phi,vector *v);



/** Interpolate state vectors A and B, which are at times 
(in seconds) timeA and timeB, to state vector OUT, at time timeOut.
*/
ASF_COREDLL void interp_stVec(
		const stateVector *A,double timeA, 
		const stateVector *B, double timeB,
		      stateVector *OUT,double timeOut);

/** Date utility routines */

/** Return true if this year is a Gregorian leap year.
  Gregorian leap years occur every 4 years, except years 
  divisible by 100 aren't leap years unless also divisible by 400. */
ASF_COREDLL bool gregorian_leap_year(int year);

/** Return the julian day number of this 1-based month and 1-based day of year AD */
ASF_COREDLL double julian_day_from_ymd(int year,int month,int day);

/** Return the julian day number of this 1-based dayOfYear of year AD */
ASF_COREDLL double julian_day_from_yd(int year,int day);

/** Return the julian day number of midnight, January 1 of year AD */
ASF_COREDLL double julian_day_from_year(int year);

/** Return the modified julian day (MJD) number of this Julian Day */
inline double MJD_from_JD(double jd) {
	return jd-2400000.5;
}


/** Routines specific to the planet Earth. */

/** The rotation rate of the Earth relative to inertial coordinates (radians/second) */
ASF_COREDLL extern const double sidereal_rotation_rate_radians_earth;

/** Convert this state vector from fixed-earth to Geocentric Equatorial Inertial,
  assuming the Greenwich Hour Angle (GHA) is this many degrees. */
ASF_COREDLL inline void fixed2gei(meta_state_t *st,double gha_deg) {
	*st=st->rotate_coriolis(gha_deg,sidereal_rotation_rate_radians_earth);
}
/** Convert this state vector from Geocentric Equatorial Inertial to fixed-earth,
  assuming the Greenwich Hour Angle (GHA) is this many degrees. */
ASF_COREDLL inline void gei2fixed(meta_state_t *st,double gha_deg) {
	*st=st->rotate_coriolis(-gha_deg,-sidereal_rotation_rate_radians_earth);
}

/**
  Return UT1 seconds of day, which correspond exactly with the 
  rotation of the earth relative to the mean sun.
  
  The input is a UTC date and seconds of day.  UTC is the most common
  time coordinate system.

  This routine accounts for UTC leap seconds using a builtin table.
  This table must be updated every few years as leap seconds are added.
*/
ASF_COREDLL double UT1_from_UTC (int year,int dayOfYear, double secOfDay);

/**
  Return TAI seconds of day, which correspond exactly with atomic clocks, 
  given the UTC date and seconds of day. 

  This routine accounts for UTC leap seconds using a builtin table.
  This table must be updated every few years as leap seconds are added.
*/
ASF_COREDLL double TAI_from_UTC (int year,int dayOfYear, double secOfDay);

/** Convert Universal Time Coordinates to Greenwich Hour Angle (gha), in degrees.
*/
ASF_COREDLL double utc2gha (int year,int dayOfYear, int hour,int min, double sec);


/** Return the spheroid generally associated with a given datum.
   Unfortunately, in the larger world, a given datum isn't really
   always necessarily associated with a particular spheroid.  For the
   purposes of the data on the insdie of the ASF tools world, however,
   we consider the correspondence to be as described in this function.
   So be sure to test on import if this correspondence is really
   true!  

   This function fails if given a datum it hasn't been taught about
   yet.  */
ASF_COREDLL spheroid_type_t
datum_spheroid (datum_type_t datum);

/** Return the semimajor and semiminor axes lengths of spheroid in
   meters.  Fails for spheroids it hasn't been taught about yet.  */
ASF_COREDLL void
spheroid_axes_lengths (spheroid_type_t spheroid, double *major, double *minor);


};

#endif
