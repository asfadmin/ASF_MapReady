/****************************************************************************
*								            *
*   rciq.c --  Performs range compression using I,Q data      		    *
*   Copyright (C) 1997  ASF STEP LAB 			   	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF STEP LAB Contacts:						    *
*	Lab Coordinator   - Rick Guritz		rguritz@images.alaska.edu   *
*	Software Engineer - Tom Logan		tlogan@images.alaska.edu    *
* 									    *
*	Alaska SAR Facility			STEP Lab Web Site:	    *	
*	Geophysical Institute			www.images.alaska.edu	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
/****************************************************************
FUNCTION NAME: rciq - Read a patch from file

SYNTAX: rciq(p,signalGetRec,r)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    p		patch 		Output storage
    signalGetRec getRec		Allow raw signal data input.
    r		rangeRef	Range Reference Function

DESCRIPTION:
    Read raw signal data for a single patch,
    Perform a forward transform on the data,
    Multiply the data by the reference function,
    Perform a reverse transform on the data.

RETURN VALUE: None

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:  Converted from H Zebker's rciq.c - T. Logan 8/96
****************************************************************/
#include "asf.h"
#include "paisp_defs.h"
#include "read_signal.h"

extern int my_pe;

void rciq(patch *p,const getRec *signalGetRec,const rangeRef *r)
{
	static FCMPLX *fft=NULL;
	register int i,lineNo;
	int readSamples=p->n_range+r->refLen;/*readSamples is the number of samples 
					of uncompressed signal which are to be read in.*/

/*Initialize fft buffer.*/
	if (fft==NULL)
		fft=(FCMPLX *)MALLOC(sizeof(FCMPLX)*r->rangeFFT);
	
/*Check to see if we're reading past the end of the file.*/
	if (p->fromSample+readSamples>signalGetRec->nSamples)
		readSamples=signalGetRec->nSamples-p->fromSample;
	
/* Initialize the FFT routine */
	cfft1d(r->rangeFFT,fft,0);	
	
	for (lineNo=0; lineNo<p->n_az; lineNo++)
	{
/*		if (IM_DSP)
		  if((lineNo%512) == 0) 
		    printf("  ...Processing Line %i\n",lineNo); */
		
	/*Read i/q values into fft input buffer.*/
		getSignalLine(signalGetRec,p->fromLine+lineNo,fft,p->fromSample,readSamples);
		
	/*Zero-fill the end of the FFT buffer.*/	
		for (i=readSamples;i<r->rangeFFT;i++)
			fft[i].r=fft[i].i=0.0;

	/* forward transform the data.*/
		cfft1d(r->rangeFFT,fft,-1);
			
	/*Multiply by the reference function*/
		for (i=0; i<r->rangeFFT; i++)
		{
			float tmp_r=fft[i].r;
			fft[i].r=tmp_r*r->ref[i].r-fft[i].i*r->ref[i].i;
			fft[i].i=tmp_r*r->ref[i].i+fft[i].i*r->ref[i].r;
		}
			
	/*Reverse transform the (now range-compressed) data.*/
		cfft1d(r->rangeFFT,fft,1);

	/* Copy data into the p->trans array - transposed */
		for (i=0; i<p->n_range; i++) 
			p->trans[i*p->n_az+lineNo]=fft[i];
			
	}
	return;
}
