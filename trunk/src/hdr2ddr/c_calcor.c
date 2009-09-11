/******************************************************************************

NAME:	CALCOR

FUNCTION:
	CALCOR calculates the projection coordinates of the corners of an 
	image.  It is called when an image is windowed and new corner 
	coordinates have to be calculated.  The image is assumed to be
	rectangular.

PROGRAM HISTORY:
  Version	Date       Author       Request
  -------	----	   ------       -------
    1.0         10/87       B.Ailts      initial development
    1.1		12/87	    B.Ailts	Place bridge routine in a seperate file
					Change the include file directories
    1.2		04/88       B.Ailts	Replace newlas.h with las.h
				        Changed ddrdub to ddr
					Changed gdist to pdist
					Changed sl, ss, nl, and ns to double
					precision
    1.3         01/90       B.Ailts     Changed in order to handle arc-sec as
					a valid projection coordinate

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	
		Run under TAE

PROJECT:	new LAS

ALGORITHM (FLOW):
	Each of the corner coordinates are calculated starting with the
	upper left corner.  The three other corners are calculated from
	this corner.  The algorithm for upper left coordinate is shown below.

	upper_left_northing = (ground_distance_in_line_direction * 
	   		      (starting_line - 1)) + 
			      current_upper_left_northing_coordinate

	upper_left_easting = (ground_distance_in_sample_direction *
			     (starting_sample - 1)) +
			     current_upper_left_easting_coordinate

ALGORITHM REFERENCES:

******************************************************************************/
#include "asf.h"
#include "las.h"

void FUNCTION c_calcor(const double *sl,const double *ss,const double *nl,const double *ns,
	const struct DDR *ddr, double *upleft,double *upright,double *loleft,double *loright)
{

/*  This is statement is placed in here to compensate for using arc-seconds as
    a valid projection code.  Arc-seconds are reversed in order compared to
    projection coordinates
    --------------------------------------------------------------------------*/
if (ddr->loright[0] > ddr->upright[0])
   {
   *upleft = (ddr->pdist_y * (*sl - 1.0)) + ddr->upleft[0];
   *loleft = (ddr->pdist_y * (*nl - 1.0)) + *upleft;
   }
else
   {
   *upleft = ddr->upleft[0] - (ddr->pdist_y * (*sl - 1.0));
   *loleft = *upleft - (ddr->pdist_y * (*nl - 1.0));
   }

if (ddr->loright[1] < ddr->loleft[1])
   {
   *(upleft + 1) = ddr->upleft[1] - (ddr->pdist_x * (*ss - 1.0));
   *(upright + 1) = *(upleft + 1) - (ddr->pdist_x * (*ns - 1.0));
   }
else
   {
   *(upleft + 1) = (ddr->pdist_x * (*ss - 1.0)) + ddr->upleft[1];
   *(upright + 1) = (ddr->pdist_x * (*ns - 1.0)) + *(upleft + 1);
   }

*upright = *upleft;
*loright = *loleft;
*(loleft + 1) = *(upleft + 1);
*(loright + 1) = *(upright + 1);

return;
}
