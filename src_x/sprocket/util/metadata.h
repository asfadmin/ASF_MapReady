#define VERSION "converter_version"
#define PROCESSING_INFO "system_version"
#define PROCESSING_DATE "date_of_processing"
#define FREQUENCY "frequency"
#define POLARIZATION "polarization"
#define MODE "mode"
#define PRODUCT_TYPE "product_type"
#define CENTER_TIME  "center_time"
#define IMAGE_ID "image_id"
#define PLATFORM "platform"
#define TRACK_ANGLE "track_angle"
#define CLOCK_ANGLE "clock_angle"
#define PROJECTION "projection"
#define NUMBER_OF_PIXELS "number_of_pixels"
#define NUMBER_OF_LINES  "number_of_lines"
#define RNG_PIXEL_SPACING "pixel_size_range"
#define AZ_PIXEL_SPACING "pixel_size_az"
#define CENTER_GMT "center_gmt"

#define SLANT_RANGE_TO_FIRST_PIXEL "slant_range_to_first_pixel"
#define EARTH_RADIUS_AT_IMAGE_CENTER "earth_radius_at_center"
#define EARTH_RADIUS_AT_IMAGE_NARIR "earth_radius_at_nadir"
#define PLATFORM_ALITITUDE "platform_altitude"
#define IMAGE_FORMAT "image_format"

#define TOP_RIGHT_CORNER_LAT "far_early_lat"
#define TOP_RIGHT_CORNER_LONG "far_early_long"

#define TOP_LEFT_CORNER_LAT "near_early_lat"
#define TOP_LEFT_CORNER_LONG "near_early_long"

#define BOTTOM_RIGHT_CORNER_LAT "far_late_lat"
#define BOTTOM_RIGHT_CORNER_LONG "far_late_long"

#define BOTTOM_LEFT_CORNER_LAT "near_late_lat"
#define BOTTOM_LEFT_CORNER_LONG "near_late_long"

        /* Definitions of lat/long for */
#define UPPER_LEFT_LAT TOP_LEFT_CORNER_LAT
#define UPPER_LEFT_LONG TOP_LEFT_CORNER_LONG
#define UPPER_RIGHT_LAT TOP_RIGHT_CORNER_LAT
#define UPPER_RIGHT_LONG TOP_RIGHT_CORNER_LONG
#define LOWER_LEFT_LAT BOTTOM_LEFT_CORNER_LAT
#define LOWER_LEFT_LONG BOTTOM_LEFT_CORNER_LONG
#define LOWER_RIGHT_LAT BOTTOM_RIGHT_CORNER_LAT
#define LOWER_RIGHT_LONG BOTTOM_RIGHT_CORNER_LONG


#define ELLIP_MAJOR "ellip_major"
#define ELLIP_MIN "ellip_minor"

#define SIGMA_EXT ".sigma0"
#define DATA_EXT ".img"
#define LOOK_EXT ".look"
#define METADATA_EXT ".metadata"
#define COMPLEX_I_PLANE ".dataI"
#define COMPLEX_Q_PLANE ".dataQ"
#define SIZE_OF_ASF_COMPLEX (4)
#define COMPLEX_FORMAT 		"complex"
#define STANDARD_FORMAT 	"detected"

#define ELLIPS_MAJ_AXIS 	"ellipsoid_major_axis"
#define ELLIPS_MIN_AXIS 	"ellipsoid_minor_axis"
#define REVOLUTION 		"revolution"
#define FLIGHT_DIRECTION 	"assending/desending"
#define BEAM_MODE		"beam_mode"

/* Vexcel "Unique" stuff */
#define VEXCEL_BETA_OR_SIGMA "vexcel_beta_or_sigma"
#define VEXCEL_ANTENNA_PATTERN "vexcel_antenna_pattern"

#define RANGE_REFERENCE_DOPPLER "range_reference"
#define PRF 			"PRF"
#define DOPPLER_POLY_A0 	"doppler_poly_a0"
#define DOPPLER_POLY_A1		"doppler_poly_a1"
#define DOPPLER_POLY_A2 	"doppler_poly_a2"
