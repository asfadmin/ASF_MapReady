/****************************************************************************
*								            *
*   rciq.c --  Performs range compression using I,Q data      		    *
*   Copyright (C) 1997  Alaska SAR Facility		   	    	    *
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
*   ASF APD Contacts:							    *
*	Rick Guritz				rguritz@asf.alaska.edu   *
*	Tom Logan				tlogan@asf.alaska.edu    *
* 									    *
*	Alaska SAR Facility			ASF APD Web Site:	    *	
*	Geophysical Institute			www.asf.alaska.edu	    *
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
#include "aisp_defs.h"
#include "read_signal.h"

extern struct AISP_PARAMS g;/*AISP Globals, defined in aisp_params.h*/

void rciq(patch *p,const getRec *signalGetRec,const rangeRef *r)
{
	static FCMPLX *fft=NULL;
	register int i,lineNo;
	int readSamples=p->n_range+r->refLen;/*readSamples is the number of samples 
					of uncompressed signal which are to be read in.*/
	patch *r_f=NULL, *raw_f=NULL, *raw_t=NULL, *r_x_f;
	
	if (g.iflag & RANGE_REF_MAP) r_f=copyPatch(p);
	if (g.iflag & RANGE_RAW_F) raw_f=copyPatch(p);
	if (g.iflag & RANGE_RAW_T) raw_t=copyPatch(p);
	if (g.iflag & RANGE_X_F) r_x_f=copyPatch(p);
		
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
		if(!quietflag && ((lineNo%1024) == 0)) 
			printf("   ...Processing Line %i\n",lineNo); 
		
	/*Read i/q values into fft input buffer.*/
		getSignalLine(signalGetRec,p->fromLine+lineNo,fft,p->fromSample,readSamples);
		
	/*Zero-fill the end of the FFT buffer.*/	
		for (i=readSamples;i<r->rangeFFT;i++)
			fft[i].r=fft[i].i=0.0;
		if (raw_t) {for (i=0; i<p->n_range; i++) 
			raw_t->trans[i*p->n_az+lineNo]=fft[i];}
	/* forward transform the data.*/
		cfft1d(r->rangeFFT,fft,-1);
		if (raw_f) {for (i=0; i<p->n_range; i++) 
			raw_f->trans[i*p->n_az+lineNo]=fft[i];}	
	/*Multiply by the reference function*/
	if (!(g.iflag & NO_RANGE))
	    {
		for (i=0; i<r->rangeFFT; i++)
		{
			float tmp_r=fft[i].r;
			fft[i].r=tmp_r*r->ref[i].r-fft[i].i*r->ref[i].i;
			fft[i].i=tmp_r*r->ref[i].i+fft[i].i*r->ref[i].r;
		}
	     }	
	     if (r_x_f) {for (i=0; i<p->n_range; i++) 
			r_x_f->trans[i*p->n_az+lineNo]=fft[i];}	
	/*Reverse transform the (now range-compressed) data.*/
		cfft1d(r->rangeFFT,fft,1);

	/* Copy data into the p->trans array - transposed */
		for (i=0; i<p->n_range; i++) 
			p->trans[i*p->n_az+lineNo]=fft[i];
	if (r_f) {for (i=0; i<p->n_range; i++) 
			r_f->trans[i*p->n_az+lineNo] = r->ref[i];}		
	}
	if (r_f) {debugWritePatch(r_f,"range_ref_map"); destroyPatch(r_f);}
	if (raw_f) {debugWritePatch(raw_f,"range_raw_f"); destroyPatch(raw_f);}
	if (raw_t) {debugWritePatch(raw_t,"range_raw_t"); destroyPatch(raw_t);}
	if (r_x_f) {debugWritePatch(r_x_f,"range_X_f"); destroyPatch(r_x_f);}
	return;
}

