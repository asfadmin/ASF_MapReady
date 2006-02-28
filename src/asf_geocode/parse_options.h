#ifndef PARSE_OPTIONS_H
#define PARSE_OPTIONS_H

#include "asf_meta.h"

/* Type describing the resampling method to be used.  */
typedef enum {
  RESAMPLE_NEAREST_NEIGHBOR,
  RESAMPLE_BILINEAR,
  RESAMPLE_BICUBIC
} resample_method_t;

int detect_string_options(int argc, char *argv[], char *val, ... );
int detect_flag_options(int argc, char **argv, ...);

void set_options_testing(int is_testing);
project_parameters_t * parse_projection_options(int *argc, char **argv[],
						projection_type_t * proj_type);
void parse_other_options(int *argc, char **argv[],
			 double *height, double *pixel_size,
			 datum_type_t *datum, 
			 resample_method_t *resample_method,
			 int *override_checks);

void parse_log_options(int *argc, char **argv[]);
#endif
