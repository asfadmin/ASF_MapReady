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
     --zone, -z   : Zone
     --lat0, --center_latitude, --slat, -slat, --lat_0  : Latitude
     --lon0, --center_longitude, --slon, -slon, --lon_0  : Longitude

    The zone will be determined from the longitude if the zone is not
    otherwise specified.

    Examples:
      --projection utm --zone <zone>
      --projection utm -z <zone>

      -p utm --zone <zone>
      -p utm -z <zone>

  POLAR STEREO
  ============
    --slat, -slat, --center_latitude, --lat_ts  : Center Latitude
    --slon, -slon, --center_longitude, --lon_0  : Center Longitude
    -n, --north_pole : Center on North Pole (no argument)
    -s, --south_pole : Center on South Pole (no argument)
    --false_easting, -fe : False Easting
    --false_northing, -fn : False Northing

    Examples:
       -p ps -slat <slat> -slon <slon> -n
       -p ps --slat <slat> -slon <slon> --north_pole
       -p ps --center_latitude <slat> --center_longitude <slon> -n
       -p ps -slat <slat> -slon <slon> -s

   LAMBERT CONFORMAL CONIC
   =======================
     --plat1, --lat_1  : First Standard Parallel
     --plat2, --lat_2  : Second Standard Parallel
     --lat0, --center_latitude, --slat, -slat, --lat_0  : Original lat
     --lon0, --center_longitude, --slon, -slon, --lon_0  : Original lon
     --false_easting, -fe : False Easting
     --false_northing, -fn : False Northing
     --scale_factor, -sf : Scale Factor

   LAMBERT AZIMUTHAL EQUAL AREA
   ============================
     --lat0, --center_latitude, --slat, -slat, --lat_0  : Center lat
     --lon0, --center_longitude, --slon, -slon, --lon_0  : Center lon
     --false_easting, -fe : False Easting
     --false_northing, -fn : False Northing

   ALBERS CONICAL EQUAL AREA
   =========================
     --plat1, --lat_1  : First Standard Parallel
     --plat2, --lat_2  : Second Standard Parallel
     --lat0, --center_latitude, --slat, -slat, --lat_0  : Latitude of origin
     --lon0, --center_longitude, --slon, -slon, --lon_0 : Longitude of meridian
     --false_easting, -fe : False Easting
     --false_northing, -fn : False Northing

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
