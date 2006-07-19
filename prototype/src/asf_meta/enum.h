/*
Enumerated constants used in metadata handling.

The formatting of this file is specially designed so that:
	- It's valid C.  This means no // comments, and "typedef enum {...} foo" instead of just "enum foo {".
	- It's valid C++.  This is easy if it's already C.
	- Humans can read it.
	- Doxygen can parse the comments.  This is why the comments start with two asterixes.
	- The "enum_parse.pl" script can turn these enum names and values 
	  into a string table.  This requires every enum definition to occupy just one line.

Orion Sky Lawlor, olawlor@acm.org, 2006/07/13 (ASF)
*/
#ifndef __ASF_META_ENUM_H
#define __ASF_META_ENUM_H

namespace asf {

/** Describes how the data in each pixel is stored on disk.
Stored in meta_parameters->general->data_type.
*/
typedef enum {
  BYTE=1,              /**< 8-bit unsigned integer; "unsigned char" on most systems. */
  INTEGER16,           /**< 16-bit signed integer; "short int" on most systems. */
  INTEGER32,           /**< 32-bit signed integer; "int" on most systems. */
  REAL32,              /**< 32-bit IEEE floating-point; "float" (sign bit, 8 bit exponent, 23 bit mantissa) */
  REAL64,              /**< 64-bit IEEE floating-point; "double" (sign bit, 11 bit exponent, 52 bit mantissa) */
  COMPLEX_BYTE,        /**< two 8-bit signed values: real first, then imaginary */
  COMPLEX_INTEGER16,   /**< two 16-bit signed values: real first, then imaginary */
  COMPLEX_INTEGER32,   /**< two 32-bit signed values: real first, then imaginary */
  COMPLEX_REAL32,      /**< two 32-bit float values: real first, then imaginary */
  COMPLEX_REAL64       /**< two 64-bit double values: real first, then imaginary */
} data_type_t;

/** Describes what the image represents.
Values returned by meta_int(IMAGE_DATA_TYPE).
Stored in meta_parameters->general->image_data_type.
*/
typedef enum {
  RAW_IMAGE=1,		 /**<  Level 0 raw signal data (STF or CEOS)   */
  COMPLEX_IMAGE,	 /**<  Level 1 (processed) complex data        */
  AMPLITUDE_IMAGE,	 /**<  Amplitude image  		 */
  PHASE_IMAGE,		 /**<  Phase image (e.g. interferogram)  */
  POWER_IMAGE,		 /**<  Power (magnitude) image  	 */
  SIGMA_IMAGE,		 /**<  Sigma image (calibrated amplitude) [dB] */
  GAMMA_IMAGE,		 /**<  Gamma image (calibrated amplitude) [dB] */
  BETA_IMAGE,		 /**<  Beta image (calibrated amplitude) [dB] */
  COHERENCE_IMAGE,	 /**<  Coherence image  		 */
  GEOCODED_IMAGE,	 /**<  Geocoded image (DEPRECATED--check projection type)  */
  LUT_IMAGE,		 /**<  Lookup table */
  ELEVATION,             /**<  Pixels correspond to elevations, but may be slant range */
  DEM,                   /**<  Digital elevation model  */
  IMAGE                  /**<  Unknown image. */
} image_data_type_t; 

/** Describes the map projection of the image.
Values returned by meta_int(PROJECTION_TYPE),
Stored in meta_parameters->projection->type.
*/
typedef enum {
  UNIVERSAL_TRANSVERSE_MERCATOR, /**< Extremely common map projection.  Lowest distortion at the equator. */
  POLAR_STEREOGRAPHIC, /**< Rather uncommon map projection; lowest distortion is at the poles. */
  ALBERS_EQUAL_AREA, /**< Complicated but portable map projection. */
  LAMBERT_CONFORMAL_CONIC, /**< The common "Lambert" projection. */
  LAMBERT_AZIMUTHAL_EQUAL_AREA, /**< Rarely used variant of Lambert projection. */
  STATE_PLANE, /**< Each US state legally defines its own separate projection */
  SCANSAR_PROJECTION, /**< Along-track/across-track is a ScanSAR-specific, JPL-defined projection */
  LAT_LONG_PSEUDO_PROJECTION, /**< X is proportional to longitude; Y is proportional to latitude.  Used heavily by USGS. */
  SLANT_RANGE_PROJECTION, /**< "Slant range" SAR image: x axis is distance from satellite, y axis is time. */
  GROUND_RANGE_PROJECTION,  /**< "Ground range" SAR image: doppler deskewed, and corrected to spherical earth. */
  UNKNOWN_INVALID_PROJECTION
} projection_type_t;

/** Describes the Earth equator and polar radius used--the "spheroid".
Values returned by meta_int(SPHEROID_TYPE);
Stored in meta_parameters->projection->spheroid.
*/
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

/** Describes the vertical coordinate reference frame for elevations--the "datum".
Values returned by meta_int(DATUM_TYPE);
Stored in meta_parameters->projection->datum.
*/
typedef enum {
  EGM96_DATUM,   /**< Earth Gravity Model 1996 (spheroid: WGS84) */
  ED50_DATUM,    /**< European Datum 1950 (International 1924) */
  ETRF89_DATUM,  /**< European Terrestrial Reference Frame 1989 (WGS84) */
  ETRS89_DATUM,  /**< European Terrestrial Reference System 1989 (GRS 1980) */
  ITRF_DATUM,    /**< International Terrestrial Reference Frame (GRS 1980) */
  NAD27_DATUM,   /**< North American Datum 1927 (Clarke 1866) */
  NAD83_DATUM,   /**< North American Datum 1983 (GRS 1980) */
  WGS72_DATUM,   /**< World Geodetic System 1972 (WGS72) */
  WGS84_DATUM    /**< World Geodetic System 1984 (WGS84) */
} datum_type_t;


/************************* Metadata Fields *******************
  Metadata fields can be looked up only at a single image point.  
  No assumptions are made here about how metadata fields vary 
  across space, although many values (e.g., SENSOR) are fixed 
  for the entire image, while others (e.g., SLANT_RANGE in a slant 
  range image) vary linearly or smoothly.
  
  WARNING: adding fields anywhere but the end of this list, or reordering
  fields, will both confuse the order of the implementations in every metadata_source,
  and break binary backward compatability! (if you care about it)
*/

/** 1D metadata fields, accessed with metadata_source::meta1D */
typedef enum {
	METADATA_1D_FIRST=1000, /**<  Start of metadata enum */
	TIME_SINCE_START, /**<  Time of this point's aquisition relative to image start (seconds).  Computed from SLANT_TIME_DOPPLER in metadata_sar.cpp. */
	TIME_SECONDS_OF_DAY, /**<  Time of this point since midnight UTC (seconds).  This value will not jump around across midnight, which I claim is what you want!  Computed from IMAGE_START_SECONDS_OF_DAY and TIME_SINCE_START in metadata.cpp. */
	IMAGE_START_SECONDS_OF_DAY, /**<  Time of start of image since midnight UTC (seconds). */
	
	ELLIPSOID_LOCAL, /**<  Local ellipsoid radius (meters) */
	ELLIPSOID_EQUATORIAL, /**<  Equatorial radius, or major radius of planet (meters) */
	ELLIPSOID_POLAR, /**<  Polar radius, or minor radius of planet (meters) */
	ELLIPSOID_FLATTENING, /**<  Ellipsoid flattening f=(EQUATORIAL-POLAR)/EQUATORIAL (pure number) */
	ELLIPSOID_ECCENTRICITY, /**<  First eccentricity e=sqrt(1-EQUATORIAL^2/POLAR^2) (pure number) */
	
	SIDEREAL_ROTATION_RATE_RADIANS, /**<  Rotation rate of planet, relative to inertial coordinates (radians/second) */
	G_TIMES_MASS_PLANET, /**<  Gravitational constant times mass of planet (newtons/meters^2) */
	GHA_DEGREES, /**<  Greenwich Hour Angle--location of longitude 0 relative to inertial (degrees) */
	GHA_DEGREES_FROM_TIME, /**<  Input: time in seconds past image start.  Output: Greenwich Hour Angle (degrees) */
	
	SATELLITE_HEIGHT, /**<  Distance from earth center to satellite (meters). */
	SATELLITE_HEIGHT_FROM_TIME, /**<  Input is TIME_SINCE_START, output is SATELLITE_HEIGHT. */

/* SAR-specific fields */
	SLANT_RANGE, /**<  Slant range, or distance from satellite to target point (meters) */
	DOPPLER, /**<  Actual central radar doppler shift used while processing (Hz) */
	DOPPLER_RATE, /**<  Time-rate-of-change of doppler while processing (Hz/second) */
	PRF, /**<  Azimuth pulse repetition frequency (Hz). */
	WAVELENGTH, /**<  Radar mean wavelength, or lambda (meters per cycle). */
	FREQUENCY, /**<  Radar mean carrier frequency (Hz, cycles per second). */
	WAVENUMBER, /**<  Radar mean wavenumber = 2*pi/wavelength (radians of phase/meter). */
	AZIMUTH_PROCESSING_BANDWIDTH, /**<  Doppler bandwidth passed by processor (Hz) */
	CHIRP_RATE, /**<  Range chirp rate (Hz/second) */
	PULSE_DURATION, /**<  Range pulse duration (seconds) */
	RANGE_SAMPLING_RATE, /**<  Sampling rate of range return (Hz) */
	INCIDENCE_DEGREES, /**<  Incidence angle; angle at target from up vector to satellite (degrees) */
	INCIDENCE_RADIANS, /**<  Incidence angle; angle at target from up vector to satellite (radians) */
	LOOK_DEGREES, /**<  Look angle; angle at satellite from down vector to target (degrees) */
	LOOK_RADIANS, /**<  Look angle; angle at satellite from down vector to target (radians) */
	
/* Optical-specific fields */
	CLOUD_COVER, /**<  Percent of image covered by clouds (percent) */
	
	BIT_ERROR_RATE, /**<  Fraction of downlinked bits which are in error (fraction). */
	
	// See the PROJECTION_PARAMETER_GLOB for other projection parameters.
	GEOCODING_HEIGHT, /**<  Fake elevation that's been used for geocoding (meters) */
	
	INTERFEROMETRIC_REFERENCE_LOOK_RADIANS, /**<  Reference look angle used for baseline (radians of look angle) */
	INTERFEROMETRIC_LOOK_RADIANS, /**<  Look angle minus reference, used for baseline (radians of look angle) */
	INTERFEROMETRIC_FLAT_PHASE, /**<  Expected interferometric phase difference based on ellipsoid height (radians of phase) */
	INTERFEROMETRIC_PHASE_RATE, /**<  Phase to topographic elevation conversion (meters height per radian of phase) */

	METADATA_1D_LAST
} metadata_1D_enum;

/** 2D metadata fields, accessed with metadata_source::meta2D */
typedef enum {
	METADATA_2D_FIRST=2000, /**<  Start of metadata enum */
	INTERFEROMETRIC_BASELINE, /**<  Interferometric baseline along this row: x=parallel, y=normal (meters of baseline) */
	METADATA_2D_LAST
} metadata_2D_enum;

/** 3D metadata coordinate systems, accessed with metadata_source::meta3D or transform */
typedef enum {
	METADATA_3D_FIRST=3000, /**<  Start of metadata enum */
	
	/* Coordinate spaces */
	IMAGE_PIXELS, /**< XY meta image pixels (measured in meta pixels).  Z can be used as elevation. */
	LONGITUDE_LATITUDE_DEGREES, /**<  Longitude (x), geodetic latitude (y), and elevation (z) of target on planet surface (degrees,degrees,meters from ellipsoid). */
	LATITUDE_LONGITUDE_DEGREES, /**<  Geodetic latitude (x), longitude (y), and elevation (z) of target on planet surface (degrees,degrees,meters from ellipsoid). */
	TARGET_POSITION, /**<  Location of dirt on planet surface in 3D body-fixed XYZ coordinates (meters). */
	SLANT_TIME_DOPPLER, /**<  Slant range (x), time (y), and doppler (z) at this image point (meters,seconds,Hz). */
	MAP_COORDINATES, /**< Return map projection coordinates in XY; elevation in Z (units vary; but normally all meters). */
	
	/* Output-only direction vectors.  All these vectors are in the 3D body-fixed XYZ coordinate system. */
	TARGET_SATELLITE_DIRECTION, /**<  From target, unit vector pointing to satellite (unit vector) */
	TARGET_UP_DIRECTION, /**<  From target, unit vector pointing upward (unit vector) */
	TARGET_NORTH_DIRECTION, /**<  From target, unit vector pointing north (unit vector) */
	TARGET_EAST_DIRECTION, /**<  From target, unit vector pointing east (unit vector) */
	TARGET_SUN_DIRECTION, /**<  From target, unit vector pointing toward the sun (unit vector) */
	TARGET_MOON_DIRECTION, /**<  From target, unit vector pointing toward the moon (unit vector) */
	
	SATELLITE_POSITION, /**<  Location of satellite in 3D body-fixed coordinates (meters) */
	SATELLITE_VELOCITY, /**<  Velocity of satellite in 3D body-fixed coordinates (meters/second) */
	SATELLITE_TARGET_DIRECTION, /**<  From satellite, unit vector pointing to target (unit vector) */
	SATELLITE_DOWN_DIRECTION, /**<  From satellite, unit vector pointing downward (unit vector) */
	SATELLITE_FLIGHT_DIRECTION, /**<  From satellite, unit vector pointing along body-fixed velocity vector (unit vector) */
	METADATA_3D_LAST
} metadata_3D_enum;

/** State vector metadata fields, accessed with metadata_source::meta_state 
  Suffixes give the coordinate system:
	_BODYFIXED: earth-body-fixed (corotating) coordinate system.
	_GEI: Geocentric Equatorial Inertial equinox-aligned coordinate system.
	_GEI0: Like GEI, but assuming a Greenwich Hour Angle (GHA) of 0 degrees.
*/
typedef enum {
	METADATA_STATE_FIRST=4000, /**<  Start of metadata enum */
	SATELLITE_BODYFIXED, /**<  Satellite state vector, body-fixed coordinates (meters) */
	SATELLITE_GEI,  /**<  Satellite state vector, geocentric equatorial inertial coordinates (meters) */
	SATELLITE_GEI0, /**<  Satellite state vector, GEI-0 coordinates (meters) */
	TARGET_BODYFIXED, /**<  Target point state vector, body-fixed coordinates (meters) */
	TARGET_GEI,  /**<  Target point state vector, geocentric equatorial inertial coordinates (meters) */
	TARGET_GEI0, /**<  Target point state vector, GEI-0 coordinates (meters) */
	SATELLITE_FROM_TIME, /**<  Input x is time since image start (seconds).  Output is SATELLITE_BODYFIXED. */
	METADATA_STATE_LAST
} metadata_state_enum;

/** String metadata fields, accessed with metadata_source::meta_string */
typedef enum {
	METADATA_STRING_FIRST=6000, /**<  Start of metadata enum */
	METADATA_SENSOR, /**<  Short name of aquiring sensor, e.g. "ERS1". */
	METADATA_SENSOR_MODE, /**<  Short name of sensor mode, e.g. "FN1". */
	METADATA_PROCESSOR, /**<  Short name of image processing software, e.g., "ASF/AISP/3.4" */
	METADATA_STRING_LAST
} metadata_string_enum;

/** Big allocated objects with complicated types, returned by pointer from metadata_source::meta_glob.
  WARNING: Any of these can be NULL, and should not be assumed to exist.
*/
typedef enum {
	METADATA_GLOB_FIRST=9000, /**<  Start of metadata enum */
	META_PARAMETER_GLOB, /**<  Stored metadata, as a meta_parameters struct (see asf_meta/meta_parameters.h) */
	PROJECTION_PARAMETER_GLOB, /**<  Map projection information, as a meta_projection struct */
	CALIBRATION_DATA_GLOB, /**<  Antenna gain and noise profile (to be determined) */
	METADATA_GLOB_LAST
} metadata_glob_enum;

/** Integer metadata fields, accessed with metadata_source::meta_int */
typedef enum {
	METADATA_INT_FIRST=5000, /**<  Start of metadata enum */
	IMAGE_DATA_TYPE, /**<  image_data_type_t for what this image represents (see below: signal data, phase image, etc.) */
	PROJECTION_TYPE, /**<  projection_type_t of this image (see below) */
	SPHEROID_TYPE, /**<  The spheroid in use: see spheroid_type_t below. */
	DATUM_TYPE, /**<  The vertical datum in use: see datum_type_t below. */
	
	TIME_DAY_OF_YEAR, /**<  Day of year of image start; 1 on January 1 (days) */
	TIME_YEAR, /**<  Year of image start (years AD) */
	TIME_MONTH, /**<  Month of image start (1-12) */
	TIME_DAY_OF_MONTH, /**<  Day of month of image start (1-31) */
	
	ORBIT, /**<  Satellite orbit number.  This is used to identify ground tracks. */
	FRAME, /**<  Frame number within orbit.  For swaths, this is the center frame number. */
	IS_DESCENDING, /**<  If 1, satellite is descending (moving south); if 0, ascending (moving north). */
	IS_RIGHT_LOOKING, /**<  If 1, satellite is right-looking (like most SARs); if 0, left-looking. */
	IS_DESKEWED, /**<  If 1, image is moved to a doppler of 0Hz; if 0, image is positioned at natural doppler. */
	MISSING_LINES, /**<  Number of missing lines in data take. */
	METADATA_INT_LAST
} metadata_int_enum;


/** For each of the enums above, we keep a table of these structs to describe
the possible values for that enum.
*/
struct enum_value_description_t {
	int value; /**< Integer value of enum.  In .h file order.  -1 indicates the end of the table. */
	const char *name; /**< Machine-string name of that value, like "SLANT_RANGE". */
	const char *description; /** Human-readable string description of that value, like "Distance between yadda and yadda (m)" */
};

/** Look up this enum value's description.  Aborts if not found. */
ASF_COREDLL const enum_value_description_t *lookup_enum_value(int value,const enum_value_description_t *table);
/** Look up this enum value's description.  Returns NULL if not found. */
ASF_COREDLL const enum_value_description_t *lookup_enum_value_NULL(int value,const enum_value_description_t *table);

/** Look up this value's description by name.  Aborts if not found. */
ASF_COREDLL const enum_value_description_t *lookup_enum_name(const char *name,const enum_value_description_t *table);
/** Look up this value's description by name.  Returns NULL if not found. */
ASF_COREDLL const enum_value_description_t *lookup_enum_name_NULL(const char *name,const enum_value_description_t *table);

/* Tables for all the enums above.  Defined in enum_table.cpp, generated from this header by enum_parse.pl.
  Stores the string name and description of each possible enum value.
*/
ASF_COREDLL extern const enum_value_description_t data_type_t_table[];
ASF_COREDLL extern const enum_value_description_t image_data_type_t_table[];
ASF_COREDLL extern const enum_value_description_t projection_type_t_table[];
ASF_COREDLL extern const enum_value_description_t spheroid_type_t_table[];
ASF_COREDLL extern const enum_value_description_t datum_type_t_table[];
ASF_COREDLL extern const enum_value_description_t metadata_1D_enum_table[];
ASF_COREDLL extern const enum_value_description_t metadata_2D_enum_table[];
ASF_COREDLL extern const enum_value_description_t metadata_3D_enum_table[];
ASF_COREDLL extern const enum_value_description_t metadata_state_enum_table[];
ASF_COREDLL extern const enum_value_description_t metadata_string_enum_table[];
ASF_COREDLL extern const enum_value_description_t metadata_glob_enum_table[];
ASF_COREDLL extern const enum_value_description_t metadata_int_enum_table[];


};

#endif
