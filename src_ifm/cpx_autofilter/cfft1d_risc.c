/****************************************************************************
*                                                                           *
*   cfft1d_risc.c -- FFT routine that calls asf_fft.a routines              *
*   Copyright (C) 1997  Alaska SAR Facility                                 *
*                                                                           *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.                                     *
*                                                                           *
*   This program is distributed in the hope that it will be useful,         *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).                                  *
*                                                                           *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software             *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*                                                                           *
*   ASF Advanced Product Development LAB Contacts:                          *
*        APD E-mail:  apd@asf.alaska.edu                                    *
*                                                                           *
*       Alaska SAR Facility                 APD Web Site:                   *        
*       Geophysical Institute               www.asf.alaska.edu/apd          *
*       University of Alaska Fairbanks                                      *
*       P.O. Box 757320                                                     *
*       Fairbanks, AK 99775-7320                                            *
*                                                                           *
****************************************************************************/
/****************************************************************
FUNCTION NAME: cfft1d - performs forward and reverse 1d ffts 
SYNTAX: cfft1d(int n, complexFloat *c, int dir)
PARAMETERS:
    NAME:   TYPE:       PURPOSE:
    --------------------------------------------------------
    n       int             length of the vector to transform
    c       complexFloat *  pointer to vector to transform
    dir     int             operations flag: -1 forward, 1 reverse, 0 init.
    
DESCRIPTION:
    Performs a fourier transform of the input data using the asf_fft.a
    routines. 
  
RETURN VALUE: None

SPECIAL CONSIDERATIONS:
    Automatically initializes fft cosine/coefficients array.
    Keeps work array EVEN if different size fft is called for.

****************************************************************/
#include "asf.h"
#include "aisp_defs.h"
#include "fft.h"

void cfft1d(int n, complexFloat *c, int dir)
{
 	int m = (int)(log(n)/log(2.0)+0.5);
	if (dir == 0)
	{
		int ret = fftInit(m);
		if (ret!=0) {
			printf("Error %d in FFT!\n",ret);
			exit(EXIT_FAILURE);
		}
	}
	if (dir > 0)  iffts((float *)c,m,1);
	if (dir < 0)  ffts((float *)c,m, 1);
}


