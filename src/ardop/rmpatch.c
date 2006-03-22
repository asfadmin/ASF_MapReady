/****************************************************************************
*								            *
*   rmpatch.c --  Performs range migrations for a patch                     *
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
/************************************************************************
FUNCTION NAME: rmpatch - perform range migration on a patch

SYNTAX:rmpatch(trans,xResampScale,xResampOffset,n_az,n_range,
slantToFirst,slantPer,wavl,vel,fd,fdd,fddd,prf,ideskew)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    trans 	complexFloat *	Input data patch
    xResampScale	float		Resampling function xResampScale
    xResampOffset	float		Resampling function xResampOffsetcept
    n_az		int		Number of lines in the patch
    n_range		int		Number of samples in the patch
    slantToFirst  float           Near slant range of image
    slantPer    float           Slant Range delta per range sample
    wavl        float           Carrier wave length
    vel         float           Platform velocity
    fd,fdd,fddd float           Doppler rate coefficients
    prf         float           Beam pulse repitition frequency
    ideskew	int		Deskew flag (1 = deskew)

DESCRIPTION:

RETURN VALUE: None

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:  converted from H. Zebker's RMpatch.f - T. Logan 8/96
		Changed variable names - O. Lawlor 8/97
************************************************************************/
#include "asf.h"
#include "ardop_defs.h"
void create_sinc(int nfilter, float *xintp);

void rmpatch(patch *p,const satellite *s)
{
#define OVERLAP 10 /*Zero pixels to append to end of single-line buffer*/
#define NUM_SINC 2048
	static float *sincInterp=NULL;
	static complexFloat *trans_buf,*interpolated_line;
	static double  *SR;
	static float   *f0, *f_rate, *xResampVec;
	
	double wavPerPix;/*Wavelengths per pixel*/
	double invN_azPRF,invPRF;
	int     azimuth_line;
	register int i;
	float outScale,outOffset;

	/********* initializations *********/
	if (sincInterp==NULL)
	{
		sincInterp=(float *)MALLOC(8*sizeof(float)*NUM_SINC);
		create_sinc(NUM_SINC,sincInterp);
		trans_buf=(complexFloat *)MALLOC(sizeof(complexFloat)*(p->n_range+2*OVERLAP));
		interpolated_line=(complexFloat *)MALLOC(sizeof(complexFloat)*p->n_range);
		SR=(double *)MALLOC(sizeof(double)*p->n_range);
		f0=(float *)MALLOC(sizeof(float)*p->n_range);
		f_rate=(float *)MALLOC(sizeof(float)*p->n_range);
		xResampVec=(float *)MALLOC(sizeof(float)*p->n_range);
	}
	
	/*Azimuth distance on the ground per pulse.*/
	wavPerPix=s->wavl/p->slantPer;

	invN_azPRF=s->prf/(float)(p->n_az);
	invPRF=1.0/s->prf;
	
/*Since we resample based on the output pixel, but we are given the scale
and offset as a function of input pixel, we must convert:*/
	outScale=1.0/(1.0-p->xResampScale)-1.0;
	outOffset=p->xResampOffset*(outScale+1.0);

	for (i=0; i<p->n_range; i++)
	{
		SR[i]     = p->slantToFirst + i*p->slantPer;/* slant range to the line */
		f0[i]= p->fd+p->fdd*i+p->fddd*i*i;    /* Doppler coefficient*/
		f_rate[i] = getDopplerRate(SR[i],f0[i],p->g);
		xResampVec[i]   = outScale * i + outOffset; /* interferogram range resampling. */
		if (s->ideskew == 1)
		  xResampVec[i]+=((SR[i]-SR[0]-(s->wavl/4.0)*f0[i]*f0[i]/f_rate[i]))/p->slantPer-i;
	}
	/*For each line along range...*/
	for (azimuth_line=0; azimuth_line<p->n_az; azimuth_line++)
	{
	/*Buffer this line of complex data (adding zeros at the ends).*/
		for (i=0;i<OVERLAP;i++)
			trans_buf[i]=Czero();
		for (i=0; i<p->n_range; i++)
			trans_buf[i+OVERLAP]=p->trans[i*p->n_az+azimuth_line];
		for (i=0;i<OVERLAP;i++)
			trans_buf[i+OVERLAP+p->n_range]=Czero();
		/*.. for each pixel along range...*/
		for (i=0; i<p->n_range; i++)
		{
			/*Get the amount to move this pixel along range. */
			register float interp_real,interp_imag;
			float st,offset,offset_frac;
			int offset_int;
			float freq=(float)azimuth_line*invN_azPRF;
			/* frequencies must be within 0.5*prf of centroid */
			freq -= (float) (NINT((freq-f0[i])*invPRF) * s->prf);
			
			/*Figure out the slow time for this line*/
			st=(freq-f0[i])/f_rate[i];
			offset = xResampVec[i]+i-0.5*wavPerPix*(
				     f0[i]*st+f_rate[i]*0.5*st*st);
			offset_int = (int) offset;
			offset_frac = offset - floor(offset);
			/*Now interpolate 8 pixels of the trans array into one pixel of this new array,*/
			interp_real=interp_imag=0.0;
			if (offset_int >= 0 && offset_int < p->n_range)
			{
				register int k,index=offset_int-3+OVERLAP;
				int kernelNo = (int)(offset_frac*(float)NUM_SINC);
				if (kernelNo>=NUM_SINC)
				{
					if (!quietflag) printf("   Kernel_no=%i,offset_frac=%f!\n",kernelNo,offset_frac);
					kernelNo=NUM_SINC-1;
				}
				kernelNo*=8;/*Each interpolation kernel has size 8.*/
				for (k = 0; k < 8; k++) 
				{
					float scale=sincInterp[kernelNo+k];
					interp_real += scale*trans_buf[index].real;
					interp_imag += scale*trans_buf[index++].imag;
				}
			}
			interpolated_line[i].real = interp_real;
			interpolated_line[i].imag = interp_imag;
		}
		/*Write this interpolated range line back into the trans array.*/
		for (i=0; i<p->n_range; i++) 
			p->trans[i*p->n_az+azimuth_line] = interpolated_line[i];
	}  
	/* ... end of along-range line loop */
}
/****************************************************************
FUNCTION NAME: 	create_sinc

SYNTAX: 	create_sinc(nfilter, xitnp)

PARAMETERS:    	nfilter	int 		number of filter points
    		xintp	float *		returned interpolation array

DESCRIPTION:
  This functions computes an array of interpolation points of the form:

  | |		 	    (k+1)  (1/pi) * sin(pi*i/n) | 4	| n
  | | xintp[8i + (k+3)] = -1     * -------------------- |	|
  | |				       k - (i/n)        | k=-3	| i=0
  
  This is an 8-input sinc interpolation kernel, for nfilter different
amounts of interpolation.

RETURN VALUE:	interpolation array with 8*(nfilter) points

PROGRAM HISTORY:  Converted from H. Zebker's intp_coef.f - 8/96 T. Logan
****************************************************************/
void create_sinc(int nfilter, float *xintp)
{
  int    i, j;
  float  dx, y/*,sum*/;

  /* compute the interpolation factors */
  for (i=0; i<nfilter; i++)
    {
       j = i*8;
       dx = (float)(i)/(float)(nfilter+1);
    /*Create one 8-entry kernel, used for interpolating by dx.*/
       y = sin(pi*dx)/pi;

       if (dx != 0.0 && dx != 1.0) 
         {
	   xintp[j  ] = -y/(3.0+dx);
	   xintp[j+1] =  y/(2.0+dx);
       	   xintp[j+2] = -y/(1.0+dx);
	   xintp[j+3] =  y/dx;
       	   xintp[j+4] =  y/(1.0-dx);
	   xintp[j+5] = -y/(2.0-dx);
       	   xintp[j+6] =  y/(3.0-dx);
	   xintp[j+7] = -y/(4.0-dx);
         }
       else if (dx == 0.0) 
         {
	   xintp[j  ] = xintp[j+1] = xintp[j+2] = 0.0;
           xintp[j+3] = 1.0;
	   xintp[j+4] = xintp[j+5] = xintp[j+6] = xintp[j+7] = 0.0;
	 }
       else if (dx == 1.0)
         {
	   xintp[j  ] = xintp[j+1] = xintp[j+2] = xintp[j+3] = 0.0;
	   xintp[j+4] = 1.0;
	   xintp[j+5] = xintp[j+6] = xintp[j+7] = 0.0;
	 }
    }
  return;
}
