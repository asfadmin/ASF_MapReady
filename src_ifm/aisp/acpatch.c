/****************************************************************************
*								            *
*   acpatch.c - Performs Azimuth Compression on a Patch of data.            *
*   Copyright (C) 1997  Alaska Sar Facility		   	    	    *
*									    *
*   ASF APD Contacts:						   	    *
*	Rick Guritz				rguritz@asf.alaska.edu 	    *
*	Tom Logan				tlogan@asf.alaska.edu       *
* 									    *
*	Alaska SAR Facility			APD Web Site:		    *	
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

int ac_direction=0;/*Used only by dop_prf*/

void acpatch(patch *p,const satellite *s)
{
#define sinCosTableEntries 4096
#define sinCosTableBitmask 0x0fff


	static complexFloat *sinCosTable=NULL;
	float sinCosTableConv=1.0/pi2*sinCosTableEntries;
#define sinCos(phase) (sinCosTable[((int)((phase)*sinCosTableConv))&sinCosTableBitmask])

	float  r, y, f0, f_rate;
	int    np/*, ind*/;
	complexFloat *ref=(complexFloat *)MALLOC(sizeof(complexFloat)*p->n_az);
	float  phase, az_resamp;
	float  dop_deskew;
	int    n, nfc, nf0;
	int    lineNo, j, k;
	complexFloat cZero=Czero();
	float pixel2time=1.0/s->prf;
	float *win;
	patch *d_t=NULL, *d_f=NULL, *d_x=NULL;
	/*float alpha;*/

	if (s->debugFlag & AZ_REF_F) d_f=copyPatch(p);
	if (s->debugFlag & AZ_REF_T) d_t=copyPatch(p);
	if (s->debugFlag & AZ_X_F) d_x=copyPatch(p);
	
	if (sinCosTable==NULL)
	{
		int tableIndex;
		sinCosTable=(complexFloat *)MALLOC(sizeof(complexFloat)*sinCosTableEntries);
		for (tableIndex=0;tableIndex<sinCosTableEntries;tableIndex++)
		{
			float tablePhase=(float)tableIndex/sinCosTableConv;
			sinCosTable[tableIndex].real = cos(tablePhase);
			sinCosTable[tableIndex].imag = sin(tablePhase);
		}
	}

	for (lineNo=0; lineNo< p->n_range; lineNo++)
	{
		int lineOffset=lineNo*p->n_az;/*Offset to the current line in the trans array.*/

		r = p->slantToFirst + (float)lineNo*p->slantPer;
		f0 = p->fd + p->fdd*lineNo + p->fddd*lineNo*lineNo;
		f_rate=getDopplerRate(r,f0,p->g);
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
		
		/* Check to see if we are going to truncate the bandwidth in azimuth */
/* Jeremy Made a big change here!
		s->pctbwaz=0.5; */
		if (s->pctbwaz!=0)
			np=np*(1-s->pctbwaz);

		if (ac_direction==0)
		  for (j = 1; j <= np; j++)
		  { /*Normal case: write both halves of reference function*/
			float t = j*pixel2time;
			float quadratic_phase=pi * f_rate*t*t;
			float linear_phase=pi2*f0*t;
			ref[j] = sinCos(quadratic_phase+linear_phase);
			ref[p->n_az-j] = sinCos(quadratic_phase-linear_phase);
		  }
		else
		  for (j = 1; j <= np; j++)
		  { /*Loop for dop_prf: write only one half of reference function*/
			float t = j*pixel2time;
			float quadratic_phase=pi * f_rate*t*t;
			float linear_phase=pi2*f0*t;
			if (ac_direction>0)
			  ref[j] = sinCos(quadratic_phase+linear_phase);
			else
			  ref[p->n_az-j] = sinCos(quadratic_phase-linear_phase);
		  }
		
		if (s->hamming == 1)
		{
			FILE *hamFile;
			float weight;
			hamFile=FOPEN("Hamming.window","w");

			win=(float *)MALLOC(sizeof(float)*p->n_az);
			for(j=0;j<p->n_az;j++)
				win[j]=0.0;

			/* Use a azimuth reference weighting function (Hamming Window) */
			for(j=0;j<np;j++)
			{
				weight=0.8;
				win[j]=weight-(1.0-weight)*-cos(2.0*pi*j/(2*np));
				win[p->n_az-j-1]=weight-(1.0-weight)*-cos(2.0*pi*j/(2*np));
			}	
			for(j=0;j<p->n_az;j++)
			{
				fprintf(hamFile,"%f\n",win[j]);
				ref[j]=Csmul(win[j],ref[j]);
			}
			FCLOSE(hamFile);
			free(win);
		}

	/*	if (s->kaiser == 1)
                {


			FILE *kaiIn;

			kaiIn=FOPEN("Kaiser.window","r");

                        win=(float *)MALLOC(sizeof(float)*p->n_az);
                        for(j=0;j<p->n_az;j++)
                        {       
				fscanf(kaiIn,"%f",&win[j]);
			
                        }
			FCLOSE(kaiIn);
                        for(j=0;j<p->n_az;j++)
                                ref[j]=Csmul(win[j],ref[j]);
                                
                                free(win);
                } */

		if (d_t) {for (k = 0; k<p->n_az; k++) d_t->trans[lineOffset+k] = ref[k];}
		
		/* forward transform the reference */
		cfft1d(p->n_az,ref,-1);
		
		if (d_f) {for (k = 0; k<p->n_az; k++) d_f->trans[lineOffset+k] = ref[k];}
		
		/* multiply the reference by the data */
		if (!(s->debugFlag & NO_AZIMUTH))
		{
	
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
		}
		if (d_x) {for (k = 0; k<p->n_az; k++) d_x->trans[lineOffset+k] = p->trans[lineOffset+k];}
		/* inverse transform the product */
		cfft1d(p->n_az,&(p->trans[lineOffset]),1);

		if (!quietflag && (lineNo%1024 == 0)) printf("   ...Processing Line %i\n",lineNo);
	}
	FREE((void *)ref);
	if (d_t) {debugWritePatch(d_t,"az_ref_t"); destroyPatch(d_t);}
	if (d_f) {debugWritePatch(d_f,"az_ref_f"); destroyPatch(d_f);}
	if (d_x) {debugWritePatch(d_x,"az_X_f"); destroyPatch(d_x);}
	if (s->debugFlag & AZ_X_T) debugWritePatch(p,"az_X_t");
}

