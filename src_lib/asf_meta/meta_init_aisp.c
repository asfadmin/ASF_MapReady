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

/*aisp_init:
	Reads additional SAR structure parameters from
AISP input file into meta_parameters structure.
*/
void aisp_init(const char *fName,meta_parameters *sar)
{
	struct AISP_PARAMS g;
/*Check to see if the AISP input file exists.*/
	char *inputFileName;
	if (!extExists(fName,".in"))
		return;/*If the AISP input file does not exist, just bail.*/

/*If the AISP input file exists, read parameters from it.*/
	inputFileName=appendExt(fName,".in");
	read_params(inputFileName,&g);
	free(inputFileName);

/*Now sift these AISP globals into SAR parameters.*/
	sar->ifm->er=g.re;
	sar->ifm->ht=g.re+g.ht;
	sar->geo->wavelen=g.wavl;
	
	sar->geo->deskew=g.deskew;
	sar->geo->dopRange[0]=g.fd;
	sar->geo->dopRange[1]=g.fdd;
	sar->geo->dopRange[2]=g.fddd;
	sar->geo->dopAz[0]=g.fd;
	sar->geo->dopAz[1]=0.0;
	sar->geo->dopAz[2]=0.0;
	sar->geo->slantFirst=g.r00;
	sar->geo->rngPixTime=1.0/g.fs;
	sar->geo->xPix=sar->geo->rngPixTime*speedOfLight/2.0;
	sar->geo->azPixTime=1.0/g.prf;
	sar->geo->yPix=sar->geo->azPixTime*g.vel*
		sar->ifm->er/sar->ifm->ht;

	sar->geo->timeShift=g.timeOff;
	sar->geo->slantShift=g.slantOff;

}
