/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from various
metadata files and meta_parameters structure.
   Internal-only routine.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
****************************************************************/
#include "asf.h"
#include "meta_init.h"
#include "ardop_params.h"

/************************************************************
 * ardop_init:
 * Reads additional SAR structure parameters from ARDOP input
 * file into meta_parameters structure. */
void ardop_init(const char *fName,meta_parameters *meta)
{
	struct ARDOP_PARAMS g;
/*Check to see if the ARDOP input file exists.*/
	char inputFileName[256];
	if (!extExists(fName,".in"))
		return;/*If the ARDOP input file does not exist, just bail.*/

/*If the ARDOP input file exists, read parameters from it.*/
	create_name(inputFileName,fName,".in");
	read_params(inputFileName,&g);

/*Now sift these ARDOP globals into SAR parameters.*/
 	meta->sar->look_count              = g.nlooks;
	meta->sar->deskewed                = g.deskew;
	meta->sar->range_time_per_pixel    = 1.0/g.fs;
	meta->sar->azimuth_time_per_pixel  = 1.0/g.prf;
	meta->sar->slant_shift             = g.slantOff;
	meta->sar->time_shift              = g.timeOff;
	meta->sar->slant_range_first_pixel = g.r00;
	meta->sar->wavelength              = g.wavl;
	meta->sar->prf                     = g.prf;
	meta->sar->earth_radius            = g.re;
	meta->sar->satellite_height        = g.re+g.ht;
	meta->sar->range_doppler_coefficients[0] = g.fd;
	meta->sar->range_doppler_coefficients[1] = g.fdd;
	meta->sar->range_doppler_coefficients[2] = g.fddd;
	meta->sar->azimuth_doppler_coefficients[0] = g.fd;
	meta->sar->azimuth_doppler_coefficients[1] = 0.0;
	meta->sar->azimuth_doppler_coefficients[2] = 0.0;

	meta->general->x_pixel_size = meta->sar->range_time_per_pixel*speedOfLight/2.0;
	meta->general->y_pixel_size = meta->sar->azimuth_time_per_pixel*g.vel
		* g.re / (g.re+g.ht);

}
