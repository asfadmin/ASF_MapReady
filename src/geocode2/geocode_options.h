#ifndef GEOCODE_OPTIONS_H
#define GEOCODE_OPTIONS_H

#include "asf_meta.h"

/*
  Pass the argc,argv pair to get_geocode_options.

  GENERAL FORMAT
  ==============
  --projection <name> <<projection specific options>>
  -p <name> <<projection specific options>>

  UTM
  ===
     --zone, -z                  : Zone
     --center-latitude           : Latitude
     --central-meridian          : Longitude

    The zone will be determined from the longitude if the zone is not
    otherwise specified.

  POLAR STEREO
  ============
    --center-latitude            : Center Latitude
    --central-meridian           : Center Longitude
    -n, --north_pole             : Center on North Pole (no argument)
    -s, --south_pole             : Center on South Pole (no argument)
    --false_easting              : False Easting
    --false_northing             : False Northing

    Examples:
       --projection ps --center-latitude <lat> --central-meridian <lon> -n
       -p ps --center-latitude <lat> --central-meridian <lon> --south-pole

   LAMBERT CONFORMAL CONIC
   =======================
     --first-standard-parallel   : First Standard Parallel
     --second-standard-parallel  : Second Standard Parallel
     --center-latitude           : Original lat
     --central-meridian          : Original lon
     --false-easting             : False Easting
     --false-northing            : False Northing
     --scale-factor              : Scale Factor

   LAMBERT AZIMUTHAL EQUAL AREA
   ============================
     --center-latitude           : Original lat
     --central-meridian          : Original lon
     --false-easting             : False Easting
     --false-northing            : False Northing

   ALBERS CONICAL EQUAL AREA
   =========================
     --first-standard-parallel   : First Standard Parallel
     --second-standard-parallel  : Second Standard Parallel
     --center-latitude           : Original lat
     --central-meridian          : Original lon
     --false-easting             : False Easting
     --false-northing            : False Northing

*/

project_parameters_t * get_geocode_options(int *argc, char ***argv,
					   projection_type_t * proj_type,
					   double *height, double *pixel_size);

/* Might want to make these static... they are called from get_geocode_options
   before it returns. */
void sanity_check(projection_type_t proj_type, project_parameters_t * pps);
void apply_defaults(projection_type_t proj_type, project_parameters_t * pps);

void to_radians(projection_type_t proj_type, project_parameters_t * pps);
void to_degrees(projection_type_t proj_type, project_parameters_t * pps);

/* this allows testing failure cases without seeing a lot of error
   messages go by.  Kind of a hack I guess */
void set_options_testing(int is_testing);

#endif
