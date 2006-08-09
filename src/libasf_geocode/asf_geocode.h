#include <glib.h>

#include "asf_meta.h"

// This is the old parse_options.h
/* Type describing the resampling method to be used.  */
typedef enum {
  RESAMPLE_NEAREST_NEIGHBOR,
  RESAMPLE_BILINEAR,
  RESAMPLE_BICUBIC
} resample_method_t;

int detect_string_options(int argc, char *argv[], char *val, ... );
int detect_flag_options(int argc, char **argv, ...);
void extract_double_options(int *argc, char **argv[], double *val, ... );

void set_options_testing(int is_testing);
project_parameters_t * parse_projection_options(int *argc, char **argv[],
                                                projection_type_t * proj_type,
                                                int *did_write_proj_file);
void parse_other_options(int *argc, char **argv[],
                         double *height, double *pixel_size,
                         datum_type_t *datum, 
                         resample_method_t *resample_method,
                         int *override_checks);

void parse_log_options(int *argc, char **argv[]);

///////////////////////////////////////////////////////////////////////////////
//
// Projection (or unprojection) function pointer types.  Like a lot of
// the other stuff in this file, these aren't intended to be part of
// the public API of this library.
//
///////////////////////////////////////////////////////////////////////////////

// Instances of this type are routines which use projection parameters
// pps to convert input coordinates xi, yi, zi to ouputs stored in xo,
// yo, zo.  Mainly intended to be set to project_utm, project_utm_inv,
// or some other function from this library, or a similar function.
typedef int (*projector_t) (project_parameters_t *pps, double xi, double yi,
			    double zi, double *xo, double *yo, double *zo);

// Like the above, but does a whole array of length points at once.
// Corresponds to for examplze project_utm_arr.
typedef int (*array_projector_t) (project_parameters_t *ppd, double *xi,
				  double *yi, double *zi, double **xo,
				  double **yo, double **zo, long length);

// Select the appropriate projection and unprojection routines for
// projection_type from libasf_proj.
void set_projection_functions (projection_type_t projection_type, 
			       projector_t *project, projector_t *unproject,
			       array_projector_t *array_project,
			       array_projector_t *array_unproject);

///////////////////////////////////////////////////////////////////////////////


// This is the old geocode_options.h
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

void parse_proj_args_file(const char * file, project_parameters_t * pps,
			  projection_type_t * proj_type);

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

// Perform geocoding of DEM input.
int geocode_dem(projection_type_t projection_type,
		project_parameters_t *pp,
		datum_type_t datum,            
		double pixel_size,                
		resample_method_t resample_method,
		const GString *input_image,
		const meta_parameters *imd,
		const GString *output_image);

/* this allows testing failure cases without seeing a lot of error
   messages go by.  Kind of a hack I guess */
void set_options_testing(int is_testing);

// Prototype from asf_geocode.c
int asf_geocode_from_proj_file (const char *projection_file,
		 int force_flag, resample_method_t resample_method, 
		 double average_height, datum_type_t datum, double pixel_size,
		 char *in_base_name, char *out_base_name,
                 float background_val);
int asf_geocode (project_parameters_t *pp, projection_type_t projection_type, 
		 int force_flag, resample_method_t resample_method, 
		 double average_height, datum_type_t datum, double pixel_size,
		 char *in_base_name, char *out_base_name,
                 float background_val);
void sigsegv_handler (int signal_number);

// Prototypes from geoid.c
float get_geoid_height(double lat, double lon);
