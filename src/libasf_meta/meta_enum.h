/* Enumerated type definition are used , where possible, to simplify the 
   validation of individual parameters.*/

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

typedef enum {
  RAW_IMAGE=1,
  COMPLEX_IMAGE,
  AMPLITUDE_IMAGE,
  POWER_IMAGE,
  SIGMA_IMAGE,
  GAMMA_IMAGE,
  BETA_IMAGE,
  INTERFEROGRAM,
  COHERENCE_IMAGE,
  PHASE_IMAGE,
  GEOCODED_IMAGE,
  ELEVATION,
  DEM,
  IMAGE
} image_data_type_t;

typedef enum {
  BIG_IEEE=1,
  LIL_IEEE,
  CRAY_FLOAT
} endianess_t;

typedef enum {
  ERS1=1,
  ERS2,
  RSAT1,
  JERS1
} sensor_t;

typedef enum {
  STD=1,
  FN1,
  FN2,
  FN3,
  FN4,
  FN5,
  ST1,
  ST2,
  ST3,
  ST4,
  ST5,
  ST6,
  ST7,
  SWA,
  SWB
} beam_mode_t;

typedef enum {
  HH=1,
  HV,
  VH,
  VV
} polarization_t;

typedef enum {
  ASCENDING=1,
  DESCENDING
} orbit_direction_t;

typedef enum {
  SLANT_RANGE=1,
  GROUND_RANGE,
  PROJECTED
} image_type_t;

typedef enum {
  RIGHT=1,
  LEFT
} look_direction_t;

typedef enum {
  UNIVERSAL_TRANSVERSE_MERCATOR,
  POLAR_STEREOGRAPHIC,
  ALBERS_EQUAL_AREA,
  LAMBERT_CONFORMAL_CONIC,
  LAMBERT_AZIMUTHAL_EQUAL_AREA,
  SCANSAR_PROJECTION, /* along-track/across-track is ScanSAR specific projection */
} projection_type_t;

typedef enum {
  BESSEL,
  CLARKE1866,
  CLARKE1880,
  GEM6,
  GEM10C,
  GRS1980,
  INTERNATIONAL1924,
  INTERNATIONAL1967,
  WGS72,
  WGS84
} spheroid_type_t;

typedef enum {
  EGM96,   /* Earth Gravity Model 1996 (spheroid: WGS84) */
  ED50,    /* European Datum 1950 (International 1924) */
  ETRF89,  /* European Terrestrial Reference Frame 1989 (WGS84) */
  ETRS89,  /* European Terrestrial Reference System 1989 (GRS 1980) */
  ITRF,    /* International Terrestrial Reference Frame (GRS 1980) */
  NAD27,   /* North American Datum 1927 (Clarke 1866) */
  NAD83,   /* North American Datum 1983 (GRS 1980) */
  WGS72,   /* World Geodetic System 1972 (WGS72) */
  WGS84    /* World Geodetic System 1984 (WGS84) */
} datum_type_t;

typedef enum {
  INTERNAL=1,
  CEOS,
  GEOTIFF,
  TIFF,
  JPEG,
  PPM
} file_format_t;
