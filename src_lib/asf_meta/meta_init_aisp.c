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
#include "aisp_params.h"

/************************************************************
 * aisp_init:
 * Reads additional SAR structure parameters from AISP input
 * file into meta_parameters structure. */
void aisp_init(const char *fName,meta_parameters *meta)
{
	struct AISP_PARAMS g;
/*Check to see if the AISP input file exists.*/
	char *inputFileName;
	if (!extExists(fName,".in"))
		return;/*If the AISP input file does not exist, just bail.*/

/*If the AISP input file exists, read parameters from it.*/
	inputFileName = appendExt(fName,".in");
	read_params(inputFileName,&g);
	free(inputFileName);

/*Now sift these AISP globals into SAR parameters.*/
/* DEPRICATED FIELDS
 *	meta->ifm->er = g.re;
 *	meta->ifm->ht = g.re+g.ht;
 */
 	meta->sar->look_count              = g.nlooks;
	meta->sar->deskewed                = g.deskew;
	meta->sar->range_time_per_pixel    = 1.0/g.fs;
	meta->sar->azimuth_time_per_pixel  = 1.0/g.prf;
	meta->sar->slant_shift             = g.slantOff;
	meta->sar->time_shift              = g.timeOff;
	meta->sar->slant_range_first_pixel = g.r00;
	meta->sar->wavelength              = g.wavl;
	meta->sar->prf                     = g.prf;
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
