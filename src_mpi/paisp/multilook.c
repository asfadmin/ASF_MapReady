/****************************************************************************
*								            *
*   multilook.c -- Creates a multilooked amplitude image                    *
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
FUNCTION NAME: multilook - performs amplitude multilook of t1 array

SYNTAX: multilook(FCMPLX *, int, int, int, float *)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    patch	FCMPLX *	Complex Image Patch (n_range X n_looks)
    n_range	int		lines in patch
    nlooks	int		number of looks to perform
    amps	float *		return multiooked amplitude patch

DESCRIPTION:
    Given a complex input patch of size n_range * n_looks, returns a 
    multilooked amplitude lines using nlooks in the sample direction
    (output size is n_range X nlooks). 

RETURN VALUE: none

SPECIAL CONSIDERATIONS:
 assumes range resampling was done at time of range migration

PROGRAM HISTORY:
 Converted from H. Zebker's multilook.f - T. Logan Fall '96
 Extensively modified to allow simultaneous writing of AMP and CPX images -
 Mark Ayers 8/00
****************************************************************/
#include "asf.h"
#include "aisp_defs.h"

void multilook(FCMPLX *patch,int n_range,int nlooks,float *amps,float *pwrs)
{
	float   temp_amp,temp_real,temp_imag,temp_pwr;
	int     samp, line;	

	for (samp=0; samp<n_range; samp++)
	{
		temp_amp=0.0;
        	temp_real=0.0;
		temp_imag=0.0;
		temp_pwr=0.0;
		for (line=0; line<nlooks; line++)
		{
			temp_real=patch[samp+n_range*line].r;
			temp_imag=patch[samp+n_range*line].i;
			temp_amp+=sqrt(temp_real*temp_real+temp_imag*temp_imag);
			temp_pwr+=temp_real*temp_real+temp_imag*temp_imag;
		}
		amps[samp] = temp_amp/nlooks;
		if (pwrs!=NULL) pwrs[samp] = temp_pwr/nlooks;

	}
}
