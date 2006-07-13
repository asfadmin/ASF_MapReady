/*
Metadata handling for ASF plugins.  The central abstraction here 
is a metadata "field", which is just an int that identifies a value,
like SLANT_RANGE.  You look up the value by passing the int and an
image location to a metadata_source.

The nice part is the metadata_source is then free to do any of:
	- Return a value from one of its internal records.
	- Compute the value based on other metadata.
	- Adjust coordinates and call another metadata_source.
	- Throw an exception because the field just isn't there.
	- Transform coordinates and call some other metadata_source.
	- Call some other metadata_source and sanity-check its outputs.

This file should include everything in the old asf_meta.h header,
and everything that might be profitably used in more than one place.

Orion Sky Lawlor, olawlor@acm.org, 2006/06/12
*/
#ifndef __ASF_META_H
#define __ASF_META_H

#include "asf/plugin.h"
#include "osl/vector2d.h"
#include "osl/vector3d.h"

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


/************************* Metadata Fields *******************
  Metadata fields can be looked up only at a single image point.  
  No assumptions are made here about how metadata fields vary 
  across space, although many values (e.g., SENSOR) are fixed 
  for the entire image, while others (e.g., SLANT_RANGE in a slant 
  range image) vary linearly or smoothly.
  
  WARNING: adding fields anywhere but the end of this list, or reordering
  fields, will both confuse the order of the implementations in every metadata_source,
  and break binary backward compatability (if you care about it).
*/

/** 1D metadata fields, accessed with metadata_source::meta1D */
enum metadata_1D_enum {
	METADATA_1D_FIRST=1000, ///< Start of metadata enum
	TIME_SINCE_START, ///< Time of this point's aquisition relative to image start (seconds)
	TIME_SECONDS_OF_DAY, ///< Time of this point since midnight UTC (seconds).  This value will not jump around across midnight, which I claim is what you want!
	IMAGE_START_SECONDS_OF_DAY, ///< Time of start of image since midnight UTC (seconds).
	
	ELLIPSOID_LOCAL, ///< Local ellipsoid radius (meters)
	ELLIPSOID_EQUATORIAL, ///< Equatorial radius, or major radius of planet (meters)
	ELLIPSOID_POLAR, ///< Polar radius, or minor radius of planet (meters)
	ELLIPSOID_FLATTENING, ///< Ellipsoid flattening f=(EQUATORIAL-POLAR)/EQUATORIAL (pure number)
	ELLIPSOID_ECCENTRICITY, ///< First eccentricity e=sqrt(1-EQUATORIAL^2/POLAR^2) (pure number)
	
	SIDEREAL_ROTATION_RATE_RADIANS, ///< Rotation rate of planet, relative to inertial coordinates (radians/second)
	G_TIMES_MASS_PLANET, ///< Gravitational constant times mass of planet (newtons/meters^2)
	GHA_DEGREES, ///< Greenwich Hour Angle--location of longitude 0 relative to inertial (degrees)
	GHA_DEGREES_FROM_TIME, ///< Input: time in seconds past image start.  Output: Greenwich Hour Angle (degrees)
	
	SATELLITE_HEIGHT, ///< Distance from earth center to satellite (meters).
	SATELLITE_HEIGHT_FROM_TIME, ///< Input is TIME_SINCE_START, output is SATELLITE_HEIGHT.

/* SAR-specific fields */
	SLANT_RANGE, ///< Slant range, or distance from satellite to target point (meters)
	DOPPLER, ///< Actual central radar doppler shift used while processing (Hz)
	DOPPLER_RATE, ///< Time-rate-of-change of doppler while processing (Hz/second)
	PRF, ///< Azimuth pulse repetition frequency (Hz).
	WAVELENGTH, ///< Radar mean wavelength, or lambda (meters per cycle).
	FREQUENCY, ///< Radar mean carrier frequency (Hz, cycles per second).
	WAVENUMBER, ///< Radar mean wavenumber = 2*pi/wavelength (radians of phase/meter).
	AZIMUTH_PROCESSING_BANDWIDTH, ///< Doppler bandwidth passed by processor (Hz)
	CHIRP_RATE, ///< Range chirp rate (Hz/second)
	PULSE_DURATION, ///< Range pulse duration (seconds)
	RANGE_SAMPLING_RATE, ///< Sampling rate of range return (Hz)
	INCIDENCE_DEGREES, ///< Incidence angle; angle at target from up vector to satellite (degrees)
	INCIDENCE_RADIANS, ///< Incidence angle; angle at target from up vector to satellite (radians)
	LOOK_DEGREES, ///< Look angle; angle at satellite from down vector to target (degrees)
	LOOK_RADIANS, ///< Look angle; angle at satellite from down vector to target (radians)
	
/* Optical-specific fields */
	CLOUD_COVER, ///< Percent of image covered by clouds (percent)
	
	BIT_ERROR_RATE, ///< Fraction of downlinked bits which are in error (fraction).
	
	// See the PROJECTION_ZONE and PROJECTION_PARAMETER_GLOB for other projection parameters.
	FALSE_EASTING, ///< Value to subtract from X coordinates to get to plain projection (meters)
	FALSE_NORTHING, ///< Value to subtract from Y coordinates to get to plain projection (meters)
	GEOCODING_HEIGHT, ///< Fake elevation that's been used for geocoding (meters)
	
	INTERFEROMETRIC_REFERENCE_LOOK_RADIANS, ///< Reference look angle used for baseline (radians of look angle)
	INTERFEROMETRIC_LOOK_RADIANS, ///< Look angle minus reference, used for baseline (radians of look angle)
	INTERFEROMETRIC_FLAT_PHASE, ///< Expected interferometric phase difference based on ellipsoid height (radians of phase)
	INTERFEROMETRIC_PHASE_RATE, ///< Phase to topographic elevation conversion (meters height per radian of phase)

	METADATA_1D_LAST
};

/** 2D metadata fields, accessed with metadata_source::meta2D */
enum metadata_2D_enum {
	METADATA_2D_FIRST=2000, ///< Start of metadata enum
	LATITUDE_LONGITUDE_DEGREES, ///< Location of target on planet surface (decimal degrees)
	LATITUDE_LONGITUDE_RADIANS, ///< Location of target on planet surface (radians)
	PROJECTION_COORDINATES, ///< Map projection coordinates (meters)
	INTERFEROMETRIC_BASELINE, ///< Interferometric baseline: x=parallel, y=normal (meters of baseline)
	METADATA_2D_LAST
};

/** 3D metadata fields, accessed with metadata_source::meta3D */
enum metadata_3D_enum {
	METADATA_3D_FIRST=3000, ///< Start of metadata enum
	TARGET_POSITION, ///< Location of target in 3D body-fixed coordinates (meters)
	TARGET_SATELLITE_DIRECTION, ///< From target, unit vector pointing to satellite (unit vector)
	TARGET_UP_DIRECTION, ///< From target, unit vector pointing upward (unit vector)
	TARGET_NORTH_DIRECTION, ///< From target, unit vector pointing north (unit vector)
	TARGET_EAST_DIRECTION, ///< From target, unit vector pointing east (unit vector)
	TARGET_SUN_DIRECTION, ///< From target, unit vector pointing toward the sun (unit vector)
	TARGET_MOON_DIRECTION, ///< From target, unit vector pointing toward the moon (unit vector)
	
	SATELLITE_POSITION, ///< Location of satellite in 3D body-fixed coordinates (meters)
	SATELLITE_VELOCITY, ///< Velocity of satellite in 3D body-fixed coordinates (meters/second)
	SATELLITE_TARGET_DIRECTION, ///< From satellite, unit vector pointing to target (unit vector)
	SATELLITE_DOWN_DIRECTION, ///< From satellite, unit vector pointing downward (unit vector)
	SATELLITE_FLIGHT_DIRECTION, ///< From satellite, unit vector pointing along body-fixed velocity vector (unit vector)
	
	LATITUDE_LONGITUDE_ELEVATION_DEGREES, ///< Location of target on planet surface (degrees,degrees,meters from ellipsoid)
	LATITUDE_LONGITUDE_ELEVATION_RADIANS, ///< Location of target on planet surface (radians,radians,meters from ellipsoid)
	SLANT_TIME_DOPPLER, ///< Slant range (x), time (y), and doppler (z) at this image point (meters,seconds,Hz)
	
	LLE_FROM_STD, ///< Input is SLANT_TIME_DOPPLER.  Output is LATITUDE_LONGITUDE_ELEVATION_DEGREES.
	STD_FROM_LLE, ///< Input is LATITUDE_LONGITUDE_ELEVATION_DEGREES.  Output is SLANT_TIME_DOPPLER.
	IMAGE_FROM_LLE, ///< Input is LATITUDE_LONGITUDE_ELEVATION_DEGREES.  Output is image pixels.
	METADATA_3D_LAST
};

/** State vector metadata fields, accessed with metadata_source::meta_state 
  Suffixes give the coordinate system:
	_BODYFIXED: earth-body-fixed (corotating) coordinate system.
	_GEI: Geocentric Equatorial Inertial equinox-aligned coordinate system.
	_GEI0: Like GEI, but assuming a Greenwich Hour Angle (GHA) of 0 degrees.
*/
enum metadata_state_enum {
	METADATA_STATE_FIRST=4000, ///< Start of metadata enum
	SATELLITE_BODYFIXED, ///< Satellite state vector, body-fixed coordinates (meters)
	SATELLITE_GEI,  ///< Satellite state vector, geocentric equatorial inertial coordinates (meters)
	SATELLITE_GEI0, ///< Satellite state vector, GEI-0 coordinates (meters)
	TARGET_BODYFIXED, ///< Target point state vector, body-fixed coordinates (meters)
	TARGET_GEI,  ///< Target point state vector, geocentric equatorial inertial coordinates (meters)
	TARGET_GEI0, ///< Target point state vector, GEI-0 coordinates (meters)
	SATELLITE_FROM_TIME, ///< Input x is time since image start (seconds).  Output is SATELLITE_BODYFIXED.
	METADATA_STATE_LAST
};

/** String metadata fields, accessed with metadata_source::meta_string */
enum metadata_string_enum {
	METADATA_STRING_FIRST=6000, ///< Start of metadata enum
	SENSOR, ///< Short name of aquiring sensor, e.g. "ERS1".
	MODE, ///< Short name of sensor mode, e.g. "FN1".
	PROCESSOR, ///< Short name of image processing software, e.g., "ASF/AISP/3.4"
	METADATA_STRING_LAST
};

/** Big allocated objects with complicated types, returned by pointer from metadata_source::meta_glob */
enum metadata_glob_enum {
	METADATA_GLOB_FIRST=9000, ///< Start of metadata enum
	PROJECTION_PARAMETER_GLOB, ///< Map projection information (meta_projection struct)
	CALIBRATION_DATA_GLOB, ///< Antenna gain and noise profile (to be determined)
	METADATA_GLOB_LAST
};

/** Integer metadata fields, accessed with metadata_source::meta_int */
enum metadata_int_enum {
	METADATA_INT_FIRST=5000, ///< Start of metadata enum
	IMAGE_DATA_TYPE, ///< image_data_type_t for what this image represents (see below: signal data, phase image, etc.)
	PROJECTION_TYPE, ///< projection_type_t of this image (see below)
	PROJECTION_ZONE, ///< Zone code for UTM and state plane projected images (zone number)
	SPHEROID_TYPE, ///< The spheroid in use: see spheroid_type_t below.
	DATUM_TYPE, ///< The horizontal datum in use: see datum_type_t below.
	
	TIME_DAY_OF_YEAR, ///< Day of year of image start; 1 on January 1 (days)
	TIME_YEAR, ///< Year of image start (years AD)
	TIME_MONTH, ///< Month of image start (1-12)
	TIME_DAY_OF_MONTH, ///< Day of month of image start (1-31)
	
	ORBIT, ///< Satellite orbit number.  This is used to identify ground tracks.
	FRAME, ///< Frame number within orbit.  For swaths, this is the center frame number.
	IS_DESCENDING, ///< If 1, satellite is descending (moving south); if 0, ascending (moving north).
	IS_RIGHT_LOOKING, ///< If 1, satellite is right-looking (like most SARs); if 0, left-looking.
	IS_DESKEWED, ///< If 1, image is moved to a doppler of 0Hz; if 0, image is positioned at natural doppler.
	MISSING_LINES, ///< Number of missing lines in data take.
	METADATA_INT_LAST
};

/** Values returned by meta_int(IMAGE_DATA_TYPE) */
typedef enum {
  RAW_IMAGE=1,		 ///<  Level 0 raw signal data (STF or CEOS)  
  COMPLEX_IMAGE,	 ///<  Level 1 (processed) complex data       
  AMPLITUDE_IMAGE,	 ///<  Amplitude image  		
  PHASE_IMAGE,		 ///<  Phase image (e.g. interferogram) 
  POWER_IMAGE,		 ///<  Power (magnitude) image  	
  SIGMA_IMAGE,		 ///<  Sigma image (calibrated amplitude) [dB]
  GAMMA_IMAGE,		 ///<  Gamma image (calibrated amplitude) [dB]
  BETA_IMAGE,		 ///<  Beta image (calibrated amplitude) [dB]
  COHERENCE_IMAGE,	 ///<  Coherence image  		
  GEOCODED_IMAGE,	 ///<  Geocoded image (DEPRECATED--check projection type) 
  LUT_IMAGE,		 ///<  Lookup table
  ELEVATION,             ///<  Pixels correspond to elevations, but may be slant range
  DEM,                   ///<  Digital elevation model 
  IMAGE                  ///<  Unknown image.
} image_type_t; 

/** Values returned by meta_int(PROJECTION_TYPE) */
typedef enum {
  UNIVERSAL_TRANSVERSE_MERCATOR,
  POLAR_STEREOGRAPHIC,
  ALBERS_EQUAL_AREA,
  LAMBERT_CONFORMAL_CONIC,
  LAMBERT_AZIMUTHAL_EQUAL_AREA,
  STATE_PLANE_PROJECTION,
  SCANSAR_PROJECTION, ///< along-track/across-track is a ScanSAR specific projection
  /** A simple "projection" in which the image pixels are arranged such
     that latitude and longitude lines form an regular rectangular
     grid over the image.  */
  LAT_LONG_PSEUDO_PROJECTION,
  /** "Slant range" SAR image: x axis is distance from satellite, y axis is time. */
  SLANT_RANGE_PROJECTION,
  /** "Ground range" SAR image: doppler deskewed, and corrected to spherical earth. */
  GROUND_RANGE_PROJECTION
} projection_type_t;

/** Values returned by meta_int(SPHEROID_TYPE) */
typedef enum {
  BESSEL_SPHEROID,
  CLARKE1866_SPHEROID,
  CLARKE1880_SPHEROID,
  GEM6_SPHEROID,
  GEM10C_SPHEROID,
  GRS1980_SPHEROID,
  INTERNATIONAL1924_SPHEROID,
  INTERNATIONAL1967_SPHEROID,
  WGS72_SPHEROID,
  WGS84_SPHEROID
} spheroid_type_t;

/** Values returned by meta_int(DATUM_TYPE) */
typedef enum {
  EGM96_DATUM,   /* Earth Gravity Model 1996 (spheroid: WGS84) */
  ED50_DATUM,    /* European Datum 1950 (International 1924) */
  ETRF89_DATUM,  /* European Terrestrial Reference Frame 1989 (WGS84) */
  ETRS89_DATUM,  /* European Terrestrial Reference System 1989 (GRS 1980) */
  ITRF_DATUM,    /* International Terrestrial Reference Frame (GRS 1980) */
  NAD27_DATUM,   /* North American Datum 1927 (Clarke 1866) */
  NAD83_DATUM,   /* North American Datum 1983 (GRS 1980) */
  WGS72_DATUM,   /* World Geodetic System 1972 (WGS72) */
  WGS84_DATUM    /* World Geodetic System 1984 (WGS84) */
} datum_type_t;

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




/********************** Metadata sources ****************/

/** Metadata is indexed based on these coordinates. 
Normally these are 2D image meta coordinates--that is, XY pixels.
The Z axis is the target height in meters, or 0.0 if unknown.
For a very few other metadata values, the coordinates are strange,
like latitude/longitude/elevation, or time/slantrange/doppler, 
or just "time", and so on.
*/
typedef osl::Vector3d metaCoord_t;



/**
 Get metadata values.  This interface class is extended by all sources of metadata.
*/
class ASF_COREDLL metadata_source {
public:
	virtual ~metadata_source();

/** Virtual methods that subclasses can override */
	/// Look up the real-valued 1D field "v" at image location "loc".
	virtual double meta1D(asf::metadata_1D_enum v,const asf::metaCoord_t &loc) const =0;
	
	/// Look up the 2D vector field "v" at location "loc".
	virtual asf::meta2D_t meta2D(asf::metadata_2D_enum v,const asf::metaCoord_t &loc) const =0;
	
	/// Look up the 3D vector field "v" at location "loc".
	virtual asf::meta3D_t meta3D(asf::metadata_3D_enum v,const asf::metaCoord_t &loc) const =0;
	
	/// Look up the state vector field "v" at location "loc".
	virtual asf::meta_state_t meta_state(asf::metadata_state_enum v,const asf::metaCoord_t &loc) const =0;
	
	/// Look up the integer field "v".
	virtual int meta_int(asf::metadata_int_enum v) const =0;
	
	/// Look up the string field "v".
	virtual asf::meta_string_t meta_string(asf::metadata_string_enum v) const =0;
	
	/// Look up the user-defined type field "v".  Returns NULL if none exists.
	virtual asf::meta_glob_t meta_glob(asf::metadata_glob_enum v) const =0;
	
/** Convenience wrapper routines-- use like "double d=someMetaObject(SLANT_RANGE,imageLoc);" */
	inline double operator() (asf::metadata_1D_enum v,const asf::metaCoord_t &loc) const 
		{return meta1D(v,loc);}
	inline meta2D_t operator() (asf::metadata_2D_enum v,const asf::metaCoord_t &loc) const
		{return meta2D(v,loc);}
	inline meta3D_t operator() (asf::metadata_3D_enum v,const asf::metaCoord_t &loc) const
		{return meta3D(v,loc);}
	inline meta_state_t operator() (asf::metadata_state_enum v,const asf::metaCoord_t &loc) const
		{return meta_state(v,loc);}
	inline int operator() (asf::metadata_int_enum v) const
		{return meta_int(v);}
	inline std::string operator() (asf::metadata_string_enum v) const
		{return meta_string(v);}
	inline asf::meta_glob_t operator() (asf::metadata_glob_enum v) const
		{return meta_glob(v);}
};

/**
 Get metadata values based on a simple image-to-image transform 
 from another metadata source.
*/
class ASF_COREDLL metadata_transform : public metadata_source {
public:
	metadata_transform(const metadata_source *source_meta_);
	
	/// These implementations all transform coordinates with "source_from_user"
	///   when needed, and then call source_meta to get the actual values.
	virtual double meta1D(asf::metadata_1D_enum v,const asf::metaCoord_t &loc) const;
	virtual asf::meta2D_t meta2D(asf::metadata_2D_enum v,const asf::metaCoord_t &loc) const;
	virtual asf::meta3D_t meta3D(asf::metadata_3D_enum v,const asf::metaCoord_t &loc) const;
	virtual asf::meta_state_t meta_state(asf::metadata_state_enum v,const asf::metaCoord_t &loc) const;
	virtual int meta_int(asf::metadata_int_enum v) const;
	virtual asf::meta_string_t meta_string(asf::metadata_string_enum v) const;
	virtual asf::meta_glob_t meta_glob(asf::metadata_glob_enum v) const;
protected:
	/// Return source image coordinates given user image coordinates.
	///   Subclasses must implement this routine.
	virtual asf::metaCoord_t source_from_user(int v,const asf::metaCoord_t &user) const =0;
	
	/// Return user image coordinates given source image coordinates.
	///   Subclasses must implement this routine.
	virtual asf::metaCoord_t user_from_source(int v,const asf::metaCoord_t &source) const =0;
	
	/// This is the source of all our metadata values.
	const metadata_source *source_meta;
};

/** This metadata field is missing--throw an exception. */
ASF_COREDLL void metadata_missing(int field_enum,const metadata_source &fromClass);

/**
 Compute metadata values based on the known features of the planet Earth.
 This includes radii, rotation rate, and the features of the moon and sun.
 Note that if the ASF tools end up being used off-planet a lot (e.g., Mars), 
 these should be pulled out into a "planet_parameters" object.
*/
class ASF_COREDLL metadata_earth : public metadata_source {
public:
	virtual double meta1D(asf::metadata_1D_enum v,const asf::metaCoord_t &loc) const;
	virtual asf::meta2D_t meta2D(asf::metadata_2D_enum v,const asf::metaCoord_t &loc) const;
	virtual asf::meta3D_t meta3D(asf::metadata_3D_enum v,const asf::metaCoord_t &loc) const;
	virtual asf::meta_state_t meta_state(asf::metadata_state_enum v,const asf::metaCoord_t &loc) const;
	virtual int meta_int(asf::metadata_int_enum v) const;
	virtual asf::meta_string_t meta_string(asf::metadata_string_enum v) const;
	virtual asf::meta_glob_t meta_glob(asf::metadata_glob_enum v) const;
};

/**
 Compute metadata values based on a few key Synthetic Aperture
 Radar fields.

 These implementations can compute almost every possible geolocation
  field based on these "bedrock" SAR fields:
	SLANT_TIME_DOPPLER
	SATELLITE_FROM_TIME
	WAVELENGTH and PRF
 You should inherit from this class and override meta1D and meta_state
 to provide at least these values.

 These implementations respond to unknown fields by calling the metadata_earth
 routines.
*/
class ASF_COREDLL metadata_sar : public metadata_earth {
	typedef asf::metadata_earth super;
public:
	virtual double meta1D(asf::metadata_1D_enum v,const asf::metaCoord_t &loc) const;
	virtual asf::meta2D_t meta2D(asf::metadata_2D_enum v,const asf::metaCoord_t &loc) const;
	virtual asf::meta3D_t meta3D(asf::metadata_3D_enum v,const asf::metaCoord_t &loc) const;
};


}; /* End ASF namespace */

#endif
