/****************************************************************************
*								            *
*   acpatch.c - Performs Azimuth Compression on a Patch of data.            *
*   Copyright (C) 1997  Alaska SAR Facility 			    	    *
*									    *
*   ASF APD Contacts:							    *
*	Rick Guritz				rguritz@asf.alaska.edu      *
*	Tom Logan				tlogan@asf.alaska.edu       *
* 									    *
*	Alaska SAR Facility			ASF APD Web Site:	    *	
*	Geophysical Institute			www.asf.alaska.edu	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
/***************************************************************************
FUNCTION NAME: acpatch - Performs azimuth compression of a CCSD patch

SYNTAX:   acpatch(p,s)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    p		patch		On input, contains range migrated data.
    				On output, contains azimuth compressed data.
    s		satellite	Sundry coefficients.

DESCRIPTION: Performs azimuth compression for a SAR signal patch

    For each range line in the patch
       	create reference function,
        inverse transform the reference, 
        multiply the reference by the data,
        forward transform the product

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:   Converted from FORTRAN subroutine for ROI.f 	T. Logan 8/96
***************************************************************************/
#include "asf.h"
#include "aisp_defs.h"

int ac_direction;/*Azimuth compression direction.*/

void acpatch(patch *p,const satellite *s)
{
#define sinCosTableEntries 4096
#define sinCosTableBitmask 0x0fff
	static FCMPLX *sinCosTable=NULL;
	float sinCosTableConv=1.0/pi2*sinCosTableEntries;
#define sinCos(phase) (sinCosTable[((int)((phase)*sinCosTableConv))&sinCosTableBitmask])

	FILE *dbg_az_time=NULL,*dbg_az_fft=NULL;
	float  r, y, f0, f_rate;
	int    np;
	FCMPLX *ref=(FCMPLX *)MALLOC(sizeof(FCMPLX)*p->n_az);
	float  phase, az_resamp, rd0;
	float  dx, v1, dop_deskew;
	int    n, nfc, nf0;
	int    lineNo, j, k;
	FCMPLX cZero=Czero();
	float pixel2time=1.0/s->prf;
	   
	if (s->debugFlag & 64)
	{
		dbg_az_fft=fopenImage("az_fft.cpx","wb");
		dbg_az_time=fopenImage("az_time.cpx","wb");
	}
	
	if (sinCosTable==NULL)
	{
		int tableIndex;
		sinCosTable=(FCMPLX *)MALLOC(sizeof(FCMPLX)*sinCosTableEntries);
		for (tableIndex=0;tableIndex<sinCosTableEntries;tableIndex++)
		{
			float tablePhase=(float)tableIndex/sinCosTableConv;
			sinCosTable[tableIndex].r=cos(tablePhase);
			sinCosTable[tableIndex].i=sin(tablePhase);
		}
	}

	dx = s->vel/s->prf;
	v1 = pow(s->wavl,2.0)/(8.0*pow(dx,2));

	for (lineNo=0; lineNo< p->n_range; lineNo++)
	{
		int lineOffset=lineNo*p->n_az;/*Offset to the current line in the trans array.*/

		r = p->slantToFirst + (float)lineNo*p->slantPer;
		f0 = p->fd + p->fdd*lineNo + p->fddd*lineNo*lineNo;
		rd0 = r/(1 + v1*pow((f0/s->prf),2.0));
		f_rate = -2.0 * pow(s->vel,2.0)*pow((rd0/r),2.0)/(s->wavl*r);
		np = (int)(r*s->refPerRange)/2;

		/*Compute the pixel shift for this line.*/
		/*az_resamp=Pixel shift caused by resampling function*/
		az_resamp = p->yResampScale * lineNo + p->yResampOffset;
		dop_deskew = s->a2*f0*r-s->dop_precomp;
		y =  (az_resamp - dop_deskew)*pi2/(float)p->n_az;
		
		/* create reference function */
		for (j=0; j<p->n_az ; j++) 
			ref[j] = cZero;
		
		phase = pi * pow(f0,2.0)/f_rate;
		ref[0] = sinCos(phase);
		if (ac_direction>=0)
		for (j = 1; j <= np; j++)
		{
			float t = j*pixel2time;
			float quadratic_phase=pi * f_rate*t*t;
			float linear_phase=pi2*f0*t;
			ref[j] = sinCos(quadratic_phase+linear_phase);
		}
		else if (ac_direction<=0)
		for (j = 1; j <= np; j++)
		{
			float t = j*pixel2time;
			float quadratic_phase=pi * f_rate*t*t;
			float linear_phase=pi2*f0*t;
			ref[p->n_az-j] = sinCos(quadratic_phase-linear_phase);
		}
		
		if (dbg_az_time)
			fwrite(ref,sizeof(FCMPLX),p->n_az,dbg_az_time);
		
		/* forward transform the reference */
		cfft1d(p->n_az,ref,-1);
		
		if (dbg_az_fft)
			fwrite(ref,sizeof(FCMPLX),p->n_az,dbg_az_fft);
		
		/* multiply the reference by the data */
		n = NINT(f0/s->prf);
		nf0 = p->n_az*(f0-n*s->prf)/s->prf;
		nfc = nf0 + p->n_az/2;
		if (nfc > p->n_az) nfc = nfc - p->n_az;
		phase = - y * nf0;
		for (k = 0; k<nfc; k++)
		{
			p->trans[lineOffset+k] =
			    Cmul(Cmul(p->trans[lineOffset+k],Cconj(ref[k])),sinCos(phase));
			phase += y;
		} 
		phase = - y * nf0;
		for (k = p->n_az-1; k>= nfc; k--)
		{
			p->trans[lineOffset+k]  =
			    Cmul(Cmul(p->trans[lineOffset+k],Cconj(ref[k])),sinCos(phase));
			phase -= y;
		}

		/* inverse transform the product */
		cfft1d(p->n_az,&(p->trans[lineOffset]),1);

		if (!quietflag && (lineNo%1024 == 0)) printf("   ...Processing Line %i\n",lineNo);
	}
	FREE((void *)ref);
}
