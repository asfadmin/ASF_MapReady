#ifndef __LIBASF_META_H__
#define __LIBASF_META_H__

#include "asf_reporting.h"
#include "geolocate.h"		/* For stateVector.  */
#include "ceos.h"
#include "las.h"
#include "meta_fetch.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define META_VERSION 1.4

/* These are the dummy values that meta variables get set to at initialization */
#define MAGIC_UNSET_CHAR ('?')
#define MAGIC_UNSET_STRING ("???")
#define MAGIC_UNSET_INT (-999999999)
/* For MAGIC_UNSET_DOUBLE to work, you must include asf_nan.h */
#define MAGIC_UNSET_DOUBLE (NAN)

/* Maximum length of most string fields, including trailing null.  */
#define FIELD_STRING_MAX 256

/* Maximum length of mode field, including trailing null.  In case its
   so short for some good reason.  */
#define MODE_FIELD_STRING_MAX 5

/* version 1.3 */
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

/* version 1.3 */
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

/* version 1.2 */
typedef enum {
  BYTE=1,
  INTEGER16,
  INTEGER32,
  REAL32,
  REAL64,
  COMPLEX_BYTE,
  COMPLEX_INTEGER16,
  COMPLEX_INTEGER32,
  COMPLEX_REAL32,
  COMPLEX_REAL64
} data_type_t;

/* version 1.2 */
typedef enum {
  RAW_IMAGE=1,
  COMPLEX_IMAGE,
  AMPLITUDE_IMAGE,
  PHASE_IMAGE,
  POWER_IMAGE,
  SIGMA_IMAGE,
  GAMMA_IMAGE,
  BETA_IMAGE,
  COHERENCE_IMAGE,
  GEOCODED_IMAGE,
  ELEVATION,
  DEM,
  IMAGE
} image_data_type_t;

/* version 1.2 */
typedef enum {
  UNIVERSAL_TRANSVERSE_MERCATOR,
  POLAR_STEREOGRAPHIC,
  ALBERS_EQUAL_AREA,
  LAMBERT_CONFORMAL_CONIC,
  LAMBERT_AZIMUTHAL_EQUAL_AREA,
  STATE_PLANE,
  SCANSAR_PROJECTION, /* along-track/across-track is ScanSAR specific projection */
} projection_type_t;

/* version 1.1 */
typedef struct {
  char sensor[FIELD_STRING_MAX];    /* Name of imaging sensor.             */
  char mode[MODE_FIELD_STRING_MAX]; /* Mode of imaging sensor.             */
  char processor[FIELD_STRING_MAX]; /* Name and version of SAR processor.  */
  data_type_t data_type;            /* Type of samples - version 1.2       */
  image_data_type_t image_data_type;/* Image data type - version 1.2       */
  char system[FIELD_STRING_MAX];    /* System of samples (e.g. "ieee-std") */
/*  Possible string values for system:                                     *
 *    OUR NAME      DDR EQUIVALENT      WHAT IT IS                         *
 *    big_ieee        ieee-std       IEEE standard (ref 754)               *
 *    lil_ieee        ieee-lil       IEEE standard (ref 754) little-endian *
 *      ???           ibm-mvs        IBM MVS                               *
 *   cray_float       cray-unicos    Cray Y-MP Unicos                      *
 *      ???           other-msc      Misc. Other systems not covered       */
  int orbit;                 /* Orbit number of satellite.                 */
  char orbit_direction;	     /* Ascending 'A', or descending 'D'.          */
  int frame;                 /* Frame for this image or -1 if inapplicable.*/
  int band_number;           /* Band number; first band is 0               */
  int line_count;            /* Number of lines in image.                  */
  int sample_count;          /* Number of samples in image.                */
  int start_line;            /* First line relative to original image.     */
  int start_sample;          /* First sample relative to original image.   */
  double x_pixel_size;       /* Range pixel size, in meters                */
  double y_pixel_size;       /* Azimuth pixel size, in meters              */
  double center_latitude;    /* Approximate image center latitude.         */
  double center_longitude;   /* Approximage image center longitude.        */
  double re_major;           /* Semimajor axis length (equator) (meters).  */
  double re_minor;           /* Semiminor axis length (poles) (meters).    */
  double bit_error_rate;     /* Fraction of bits which are in error.       */
  int missing_lines;         /* Number of missing lines in data take       */
} meta_general;

/* version 1.1 */
typedef struct {
  /* 'S'-> Slant Range; 'G'-> Ground Range; 'P'-> Map Projected.  */
  char image_type;
  char look_direction;            /* 'L'-> Left Looking; 'R'-> Right Looking*/
  int look_count;                 /* Number of looks to take from SLC.      */
  int deskewed;                   /* True if image moved to zero doppler.   */
  int original_line_count;        /* Number of lines in original image      */
  int original_sample_count;      /* Number of samples in original image    */
  double line_increment;          /* line increment for sampling            */
  double sample_increment;        /* sample increment for sampling          */
  double range_time_per_pixel;    /* Time per pixel in range.               */
  double azimuth_time_per_pixel;  /* Time per pixel in azimuth.             */
  double slant_shift;             /* Error correction factor, in slant range*/
  double time_shift;              /* Error correction factor, in time.      */
  double slant_range_first_pixel; /* Slant range to first pixel.            */
  double wavelength;              /* SAR carrier wavelength, in meters.     */
  double prf;                     /* Pulse Repition Frequency.              */
  double earth_radius;            /* Earth radius at scene center.          */
  double satellite_height;        /* Satellite height from earth's center.  */
  char satellite_binary_time[FIELD_STRING_MAX];  /* Satellite binary time   */
  char satellite_clock_time[FIELD_STRING_MAX];   /* Satellite UTC clock time*/
    /* Doppler centroid, doppler per pixel, and doppler per pixel squared.  */
  double range_doppler_coefficients[3];
    /* Doppler centroid, doppler per pixel, and doppler per pixel squared.  */
  double azimuth_doppler_coefficients[3];
  double azimuth_processing_bandwidth; /* version 1.4 */
  double chirp_rate;                   /* version 1.4 */
  double pulse_duration;               /* version 1.4 */
  double range_sampling_rate;          /* version 1.4 */
} meta_sar;

/* Albers Conical Equal Area - version 1.2 */
typedef struct {
  double std_parallel1;     /* First standard parallel           */
  double std_parallel2;     /* Second standard parallel          */
  double center_meridian;   /* Longitude of center meridian      */
  double orig_latitude;     /* Latitude of the projection origin */
  double false_easting;     /* False Easting - version 1.3       */
  double false_northing;    /* False Northing - version 1.3      */
} proj_albers;

/* Along-track/cross-track - version 1.1 */
typedef struct {
  double rlocal;              /* Radius of earth at scene center (meters)*/
  double alpha1,alpha2,alpha3;/* Rotation angles, in degrees             */
} proj_atct;

/* Lambert Azimuthal Equal Area - version 1.2 */
typedef struct {
  double center_lon;        /* Longitude at center of projection */
  double center_lat;        /* Latitude at center of projection  */
  double false_easting;     /* False Easting - version 1.3       */
  double false_northing;    /* False Northing - version 1.3      */
} proj_lamaz;

/* Lambert Conformal Conic - version 0.9 */
typedef struct {
  double plat1;             /* First standard parallel      */
  double plat2;             /* Second standard parallel     */
  double lat0;              /* Latitude of Origin           */
  double lon0;              /* Center Meridian              */
  double false_easting;     /* False Easting - version 1.3  */
  double false_northing;    /* False Northing - version 1.3 */
  double scale_factor;      /* Scale Factor - version 1.3   */
} proj_lamcc;

/* Polar Sterographic - version 0.9 */
typedef struct {
  double slat;              /* Standard Parallel 1                  */
  double slon;              /* Center Meridian                      */
  int is_north_pole;        /* 1 if North, 0 if South - version 1.3 */
  double false_easting;     /* False Easting - version 1.3          */
  double false_northing;    /* False Northing - version 1.3         */
} proj_ps;

/* Universal Transverse Mercator - version 0.9 */
typedef struct {
  int zone;                 /* Zone                                             */
  double false_easting;     /* False Easting - version 1.3                      */
  double false_northing;    /* False Northing - version 1.3                     */
  double lat0;              /* Latitude - version 1.3                           */
  double lon0;              /* Longitude - version 1.3                          */
  double scale_factor;      /* Scale factor: 0.9996 by definition - version 1.3 */
} proj_utm;

/* State Plane - version 1.1 */
typedef struct {
  int zone;
} proj_state;

/* Projection parameters for the projection in use - version 0.9 */
typedef union {
  proj_albers   albers;   /* Albers Conical Equal Area             */
  proj_atct     atct;     /* Along-track/cross-track               */
  proj_lamaz    lamaz;    /* Lambert Azimuthal Equal Area          */
  proj_lamcc    lamcc;    /* Lambert Conformal Conic               */
  proj_lamcc    lambert;  /* Lambert Conformal Conic - version 1.1 */
  proj_ps       ps;       /* Polar Sterographic                    */
  proj_utm      utm;      /* Universal Transverse Mercator         */
  proj_state    state;    /* State Plane                           */
} param_t;
typedef param_t project_parameters_t;

/* version 0.9 */
typedef struct {
  projection_type_t type;  /* Projection types                                     */
  double startX,startY;  /* Projection coordinates of top, lefthand corner.        */
  double perX,perY;      /* Projection coordinates per X and Y pixel.              */
  char units[12];        /* Units of projection (meters, arcsec)                   */
  char hem;              /* Hemisphere Code: 'S'->southern; other northern.        */
  spheroid_type_t spheroid; /* Spheroid - version 1.3                              */
  double re_major;       /* Semimajor axis length (equator) (meters).              */
  double re_minor;       /* Semiminor axis length (poles) (meters).                */
  double ecc;            /* Eccentricity - version 0.9 (deprecated in version 1.x) */
  datum_type_t datum;    /* Geodetic datum - height reference system - version 1.3 */
  param_t param;         /* Projection parameters for each projection.             */
} meta_projection;
 /* Compatibility alias.  proj_parameters is DEPRECATED.  */
typedef meta_projection proj_parameters;

/* version 1.1 */
typedef struct {
  double min, max;           /* Minimum and maximum image values                    */
  double mean;               /* Mean average of image values                        */
  double rmse;               /* Root mean squared error                             */
  double std_deviation;      /* Standard deviation                                  */
  double mask;               /* Value ignored while taking statistics - version 1.2 */
} meta_stats;

/* version 0.9 */
typedef struct {
  double time;     /* Time of state vector, in seconds from the
                      start of the image.  */
  stateVector vec; /* Fixed-earth state vector.  */
} state_loc;

/* version 0.9 */
typedef struct {
  int year;           /* Year for first state vector                  */
  int julDay;         /* Julian day of year for first state vector.   */
  double second;      /* Seconds of day for first state vector.       */
  int vector_count;   /* Number of state vectors.                     */
  int num;            /* Same as vector_count.  For backward compat.  */
  state_loc *vecs;    /* Array sized at run-time.                     */
} meta_state_vectors;

/* version 0.9 */
typedef struct {
  char type;                    /* 'S'-> Slant Range; 'G'-> Ground Range;
                                   'P'-> Map Projected.                         */
  proj_parameters *proj;        /* Projection parameters, for map-projected images*/
  char lookDir;                 /* 'L'-> Left Looking; 'R'-> Right Looking.     */
  int deskew;                   /* Image moved to zero-doppler? (1-> yes; 0->no)*/
  double xPix,yPix;             /* Range, azimuth pixel size, in m              */
  double rngPixTime,azPixTime;  /* Range, Azimuth pixel time, in s.             */
  double timeShift, slantShift; /* Image correction (fudge)
                                   factors in azimuth and range, in s and m.    */
  double slantFirst;            /* Slant range to first pixel, in m.            */
  double wavelen;               /* Satellite wavelength, in meters.             */
  double dopRange[3], dopAz[3]; /* Doppler centroid constant, linear, and
                                   quadratic terms, in azimuth and range (Hz)   */
} geo_parameters;

/* version 0.9 */
typedef struct {
  double er;  /* Earth radius at scene center.                               */
  double ht;  /* Satellite height from earth's center.                       */
  int nLooks; /* Number of looks to take on SLC data to make square pixels   */
  int orig_nLines,orig_nSamples;
/*  double lookCenter; * Look angle to image center (CALCULATED but not used)*/
} ifm_parameters;

/* version 0.9 */
typedef struct {
  char   sensor[FIELD_STRING_MAX];      /* Name of imaging sensor.          */
  char   mode[MODE_FIELD_STRING_MAX];   /* Mode of imaging sensor.          */
  char   processor[FIELD_STRING_MAX];   /* Name and version of SAR processor*/
  int    orbit;                         /* Orbit number of satellite.       */
  double bitErrorRate;                  /* Exactly what it says.            */
  char   satBinTime[FIELD_STRING_MAX];  /* Satellite binary clock time.     */
  char   satClkTime[FIELD_STRING_MAX];  /* Satellite UTC time.              */
  double prf;                           /* Pulse Repition Frequency.        */
} extra_info;

/* version 0.9 */
typedef struct {
  double meta_version;     /* Version of metadata format conformed to*/
  meta_general       *general;        /* version 1.1 */
  meta_sar           *sar;            /* version 1.1 */
  meta_projection    *projection;     /* version 1.1 */
  meta_stats         *stats;          /* version 1.1 */
  meta_state_vectors *state_vectors;  /* version 1.1 */
  meta_state_vectors *stVec;	      /* version 0.9 */
  geo_parameters     *geo;            /* version 0.9 */
  ifm_parameters     *ifm;            /* version 0.9 */
  extra_info         *info;           /* version 0.9 */
} meta_parameters;

/* Prototypes for name handling */
void create_name(char *out,const char *in,const char *newExt);
int fileExists(const char *name);
char *appendExt(const char *name,const char *newExt);

/* Prototypes for initialization */
meta_general *asf_meta_general_init(void);
meta_sar *asf_meta_sar_init(void);
meta_projection *asf_meta_projection_init(void);
meta_state_vectors *asf_meta_state_vectors_init(int vector_count);
meta_stats *asf_meta_stats_init(void);
geo_parameters *asf_meta_geo_init(void);
ifm_parameters *asf_meta_ifm_init(void);
meta_parameters *asf_meta_init(char *version);

/* Prototypes for various asf_meta_read functions */
meta_parameters *asf_meta_read09(meta_parameter_t *meta_struct);
meta_parameters *asf_meta_read1x(meta_parameter_t *meta_struct);
void asf_meta_read(char *inName, char *requested_version, 
		   meta_parameters **meta, struct DDR **ddr);

/* Prototypes for various asf_meta_write functions */
void asf_meta_write09(meta_parameters *meta, char *file_name);
void asf_meta_write1x(meta_parameters *meta, char *requsted_version, char *file_name);
void asf_meta_write(meta_parameters *meta, char *requested_version, struct DDR *ddr,
		    char *inName);

/* Prototypes for metadata conversion */
void asf_meta1x_to_meta09(meta_parameters *meta1x, char *version,
			  meta_parameters *meta09, struct DDR *ddr);
meta_parameters *asf_meta09_to_meta1x(meta_parameters *meta09, struct DDR *ddr,
				      char *version);

/* Some metadata structure parameters */
typedef struct {
    char             base_name[1024];
    meta_parameters *meta;
    struct DDR      *ddr;
} META_DDR_STRUCT;

#define NUM_META_DDR_STRUCTS 10
extern META_DDR_STRUCT meta_ddr_structs[NUM_META_DDR_STRUCTS];

#endif

/****************** begin of nan.h ***************/
#ifndef __ASF_NAN_H
#define __ASF_NAN_H

#ifndef NAN

# if defined (lil_endian)
#  define __nan_bytes { 0xff, 0xff, 0xbf, 0x7f }
# else
#  define __nan_bytes { 0x7f, 0xbf, 0xff, 0xff }
# endif
static union { unsigned char __c[4]; float __d; } __nan_union = { __nan_bytes };
# define NAN __nan_union.__d
# endif
# define ISNAN(X) ( ((X)==(X)) ? FALSE : TRUE )

#else

# define ISNAN(X) (isnan(X))

#endif
/******************* end of nan.h *****************/

/******************* begin of caplib.h *************/
#ifndef _CAPLIB_H_
#define _CAPLIB_H_

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>

#ifndef win32
#include <unistd.h>
#endif

void *MALLOC(size_t size);
void FREE(void *ptr);
FILE *FOPEN(const char *file,const char *mode);
size_t FREAD(void *ptr,size_t size,size_t nitems,FILE *stream);
size_t FWRITE(const void *ptr,size_t size,size_t nitems,FILE *stream);
int FSEEK(FILE *stream,int offset,int ptrname);
int FSEEK64(FILE *stream,long long offset, int ptrname);
long long FTELL64(FILE *stream);
int FCLOSE(FILE *stream);
int FFLUSH(FILE *stream);

void programmer_error(char *mess);

#endif
/****************** end of caplib.h *****************/
