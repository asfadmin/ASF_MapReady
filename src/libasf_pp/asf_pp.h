#ifndef INC_LIBASF_PP
#define INC_LIBASF_PP

#include "asf_meta.h"

/** Determine the actual values used during PP processing for a
    couple fields that aren't put into the metadata.  
*/
void pp_get_corrected_values(char *sarName,
                             double *corrected_earth_radius,
                             double *corrected_azimuth_time_per_pixel);

#endif
