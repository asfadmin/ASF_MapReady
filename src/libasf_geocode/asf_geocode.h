#include "geocode_options.h"

// Prototype from asf_geocode.c
int asf_geocode (project_parameters_t *pp, projection_type_t projection_type, 
		 int force_flag, resample_method_t resample_method, 
		 double average_height, double datum, double pixel_size,
		 char *in_base_name, char *out_base_name);
void sigsegv_handler (int signal_number);
