#ifndef PARSE_OPTIONS_H
#define PARSE_OPTIONS_H

#include "asf_meta.h"

void set_options_testing(int is_testing);
project_parameters_t * parse_projection_options(int *argc, char **argv[],
						projection_type_t * proj_type);
void parse_other_options(int *argc, char **argv[],
			 double *height, double *pixel_size);
#endif
