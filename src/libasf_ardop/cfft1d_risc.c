/****************************************************************************
*								            *
*   cfft1d_risc.c -- FFT routine that calls asf_fft.a routines         	    *
*  Parts of this code are Copyright Howard Zebker at Stanford University      *
*  Modifications are Copyright Geophysical Institute, University of Alaska    *
*  Fairbanks. All rights reserved.                                            *
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
SYNTAX: cfft1d(int n, complexFloat *c, int dir)
PARAMETERS:
    NAME:   TYPE:       PURPOSE:
    --------------------------------------------------------
    n       int	      length of the vector to transform
    c 	    complexFloat *  pointer to vector to transform
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

#include "ardop_defs.h"
#include "fft.h"

void cfft1d(int n, complexFloat *c, int dir)
{
    static int did_init = 0;
 	int m=(int)(log(n)/log(2.0)+0.5);
	if (dir == 0)
	{
            if (did_init == 0) {
		int ret=fftInit(m);
		if (ret!=0) {
		  sprintf(errbuf,"   ERROR: Problem %d in FFT!\n",ret);
		  printErr(errbuf);
		}
                did_init = 1;
            } else {
                printf("Already initialized FFT.\n");
            }
	}
	if (dir > 0)  iffts((float *)c,m,1);
	if (dir < 0)  ffts((float *)c,m, 1);
}


