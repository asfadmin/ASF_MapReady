/****************************************************************
FUNCTION NAME: c2i

SYNTAX: int c2i(amp,phase,image,table,nsamples,avg);


PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    amp		float *		amp data
    phase 	float *		phase data
    image	RGBDATA *	RGB image data
    table	RGBDATA *	RGB color table
    nsamples	int		number of samples to convert
    avg         float           avg value to scale data 

DESCRIPTION:
    Convert an amp & phase buffer into an interferogram image.

    C2I creates a byte file in which the values correspond to the color
    palette used by the RX program. This palette is based on a standard HIS
    color wheel with saturation held at an unknown constant level. 

RETURN VALUE:
   Returns TRUE if successful, FALSE otherwise.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
    1.0 - Mike Shindle - Original Development

****************************************************************/
#include "asf.h"

#include "ifm.h"
#include "pml.h"

int c2i(float *amp, float *phase, RGBDATA *image, RGBDATA *table, int nsamples, float avg)
{
	int j;
	int N, M;
	float *aP, *pP;
	RGBDATA *outPtr;

	aP=amp;
	pP=phase;
	outPtr=image;

	/*
	* loop over buffers; fill up the output
	*/
	for (j = 0; j < nsamples; j++,aP++,pP++,outPtr++) 
	{
     	
		/*
		* color scheme to match the 'rx' interferogram color table
		*/
	/* Adding 2 pi to everything below zero forces a discontinuity at phase = pi
	so instead, just shift all the phases by pi and we avoid this */
/*		if (*pP < 0.0) 
			*pP += TWOPI; */
	
		N  = (int)( ((*pP)+PI) / PIOVER3) * 42;
		M  = (int)((*aP)*ALL_SCALES(avg));
		M  = min(41, M);

		*outPtr = table[(Uchar)(M + N)];
	} 
 
return TRUE;
}

