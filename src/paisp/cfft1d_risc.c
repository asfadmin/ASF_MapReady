/****************************************************************************
*								            *
*   cfft1d_risc.c -- FFT routine that calls asf_fft.a routines         	    *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/
/****************************************************************
FUNCTION NAME: cfft1d - performs forward and reverse 1d ffts 
SYNTAX: cfft1d(int n, FCMPLX *c, int dir)
PARAMETERS:
    NAME:   TYPE:       PURPOSE:
    --------------------------------------------------------
    n       int	      length of the vector to transform
    c 	    FCMPLX *  pointer to vector to transform
    dir	    int	      operations flag: -1 forward, 1 reverse, 0 init.
    
DESCRIPTION:
    Performs a fourier transform of the input data using the asf_fft.a
  routines. 
  
RETURN VALUE:	None

SPECIAL CONSIDERATIONS:
   Automatically initializes fft cosine/coefficients array.
   Keeps work array EVEN if different size fft is called for.

****************************************************************/
#include "asf.h"
#include "paisp_defs.h"
#include "fft.h"

void cfft1d(int n, FCMPLX *c, int dir)
{
 	int m=(int)(log(n)/log(2.0)+0.5);
	if (dir == 0)
	{
		int ret=fftInit(m);
		if (ret!=0)
			{printf("Error %d in FFT!\n",ret);exit(1);}
	}
	if (dir > 0)  iffts((float *)c,m,1);
	if (dir < 0)  ffts((float *)c,m, 1);
}


