/****************************************************************************
*								            *
*   rciq.c --  Performs range compression using I,Q data      		    *
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
  static complexFloat *fft=NULL;
  register int i,lineNo;
  int readSamples=p->n_range+r->refLen;/*readSamples is the number of samples 
				  of uncompressed signal which are to be read in.*/
  patch *r_f=NULL, *raw_f=NULL, *raw_t=NULL, *r_x_f = NULL;

  if (g.iflag & RANGE_REF_MAP) r_f=copyPatch(p);
  if (g.iflag & RANGE_RAW_F) raw_f=copyPatch(p);
  if (g.iflag & RANGE_RAW_T) raw_t=copyPatch(p);
  if (g.iflag & RANGE_X_F) r_x_f=copyPatch(p);

/*Initialize fft buffer.*/
  if (fft==NULL)
    fft=(complexFloat *)MALLOC(sizeof(complexFloat)*r->rangeFFT);

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
      fft[i].real = fft[i].imag = 0.0;
    if (raw_t) {
      for (i=0; i<p->n_range; i++) 
        raw_t->trans[i*p->n_az+lineNo]=fft[i];
    }
  /* forward transform the data.*/
      cfft1d(r->rangeFFT,fft,-1);
      if (raw_f) {
        for (i=0; i<p->n_range; i++)
          raw_f->trans[i*p->n_az+lineNo]=fft[i];
      }
  /*Multiply by the reference function*/
    if (!(g.iflag & NO_RANGE))
    {
      for (i=0; i<r->rangeFFT; i++)
      {
        float tmp_r = fft[i].real;
        fft[i].real = tmp_r*r->ref[i].real - fft[i].imag*r->ref[i].imag;
        fft[i].imag = tmp_r*r->ref[i].imag + fft[i].imag*r->ref[i].real;
      }
    }
    if (r_x_f) {
      for (i=0; i<p->n_range; i++) 
	r_x_f->trans[i*p->n_az+lineNo] = fft[i];
    }

  /*Reverse transform the (now range-compressed) data.*/
    cfft1d(r->rangeFFT,fft,1);

  /* Copy data into the p->trans array - transposed */
    for (i=0; i<p->n_range; i++) 
      p->trans[i*p->n_az+lineNo]=fft[i];
    if (r_f) {
      for (i=0; i<p->n_range; i++)
       r_f->trans[i*p->n_az+lineNo] = r->ref[i];
    }
  }
  if (r_f) {debugWritePatch(r_f,"range_ref_map"); destroyPatch(r_f);}
  if (raw_f) {debugWritePatch(raw_f,"range_raw_f"); destroyPatch(raw_f);}
  if (raw_t) {debugWritePatch(raw_t,"range_raw_t"); destroyPatch(raw_t);}
  if (r_x_f) {debugWritePatch(r_x_f,"range_X_f"); destroyPatch(r_x_f);}
  return;
}

