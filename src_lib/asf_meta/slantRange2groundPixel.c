/****************************************************************
FUNCTION NAME:

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"
#include "asf_meta.h"
#include <math.h>

/* Calculate the ground range pixel from the total slant range
 ------------------------------------------------------------*/
int slantRange2groundPixel(meta_parameters *sar,double slant)
  {
	double a,x,y,Gr0, GrX;
        double Rsc, Re;
	int    groundPixel;

	Rsc = sar->ifm->ht;
	Re  = sar->ifm->er;

	a = (Rsc-Re)/Re;
	x = 1+a;

	y = sar->geo->slantFirst / Re;

	Gr0 = Re * acos((1.0 + x*x - y*y) / (2.0 * x));

	y = slant/Re;
	GrX = Re * acos((1.0 + x*x - y*y) / (2.0 * x));

	groundPixel = (int) ((GrX-Gr0)/sar->geo->proj->perX)+0.5;
	printf("Slant %f :: ",slant);
	printf("GP = (%f-%f) / %f = %i\n",GrX,Gr0,sar->geo->proj->perX,groundPixel);





	return(groundPixel);
  }

