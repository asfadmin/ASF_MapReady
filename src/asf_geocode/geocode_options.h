#ifndef GEOCODE_OPTIONS_H
#define GEOCODE_OPTIONS_H

#include "asf_meta.h"
#include "parse_options.h"

/*
  Pass the argc,argv pair to get_geocode_options.

  GENERAL FORMAT
  ==============
  --projection <name> <<projection specific options>>
  -p <name> <<projection specific options>>

     UTM 
     --- 
          --zone,                     : Zone 
          --central-meridian          : Longitude of Central Meridian 
	   
	  Either the zone or center_longitude must be specified. 
 
     POLAR STEREO 
     ------------ 
          --first-standard-parallel    : Latitude of True Scale 
          --central-meridian           : Longitude of Central Meridian 
          -n, --north-pole             : Center on North Pole (no argument) 
          -s, --south-pole             : Center on South Pole (no argument) 
          --false-easting              : False Easting (optional) 
          --false-northing             : False Northing (optional) 
 
     LAMBERT CONFORMAL CONIC 
     ----------------------- 
          --first-standard-parallel   : First Standard Parallel 
          --second-standard-parallel  : Second Standard Parallel 
          --latitude-of-origin        : Latitude at projection's origin 
          --central-meridian          : Central Meridian 
          --false-easting             : False Easting (optional) 
          --false-northing            : False Northing (optional) 
 
     LAMBERT AZIMUTHAL EQUAL AREA 
     ---------------------------- 
          --latitude-of-origin        : Latitude at center of projection 
          --central-meridian          : Longitude at center of projection 
          --false-easting             : False Easting (optional) 
          --false-northing            : False Northing (optional) 
 
     ALBERS CONICAL EQUAL AREA 
     ------------------------- 
          --first-standard-parallel   : First Standard Parallel 
          --second-standard-parallel  : Second Standard Parallel 
          --latitude-of-origin        : Latitude of projection's origin 
          --central-meridian          : Central Meridian 
          --false-easting             : False Easting (optional) 
          --false-northing            : False Northing (optional) 


*/

project_parameters_t * get_geocode_options(int *argc, char ***argv,
					   projection_type_t * proj_type,
					   double *height, double *pixel_size,
					   datum_type_t *datum,
					   resample_method_t *resample_method,
					   int *override_checks);

/* Might want to make these static... they are called from get_geocode_options
   before it returns. */
void sanity_check(projection_type_t proj_type, project_parameters_t * pps);
void apply_defaults(projection_type_t proj_type, project_parameters_t * pps,
		    meta_parameters * meta, double *average_height,
		    double *pixel_size);

/* Get the UTM zone number in which a given longitude falls.  Puts
   longitudes that fall on zone thresholds into the higher numbered
   UTM zone.  */
int calc_utm_zone (double lon);

/* Convert all the angle measures in pps between radians and degree.  */
void to_radians(projection_type_t proj_type, project_parameters_t * pps);
void to_degrees(projection_type_t proj_type, project_parameters_t * pps);

/* this allows testing failure cases without seeing a lot of error
   messages go by.  Kind of a hack I guess */
void set_options_testing(int is_testing);

#endif
