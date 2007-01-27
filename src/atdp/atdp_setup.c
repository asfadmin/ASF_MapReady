/****************************************************************************
*								            *
*   atdp_setup.c -- Routines used to set up the parameters for a run        *
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
/***********************************************************************
  aisp_setup.c  -- Routines to calculate input parameters for ASP.
  
     FUNCTIONS INCLUDED IN THIS FILE:
        give_usage	- displays usage for program
	parse_cla	- parses the cla's, fills parameters in aisp_params.h
	get_params	- reads from metadata & calculates parameters
	calc_range_ref  - creates range reference function

  2/97  T. Logan	Included ability to estimate dopplers, read
			doppler from file, & read offsets from file
  10/02 J. Nicoll 	Updated deskew options to remove wedges in data. 
***********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "ardop_defs.h"
#include "asf_meta.h"
#include "odl.h"
#include <math.h>


/*-------------------------------------------------------------------------*/
/*    The following is the list of all parameters needed to run aisp.c      */
/*-------------------------------------------------------------------------*/
struct ARDOP_PARAMS g;

int     n_az;         /* Number of lines in the Azimuth per patch   */
float azpix,rngpix; /*Size of pixel in azimuth and slant range.*/
float refPerRange;/*Reference function length per meter of range.*/

/*-------------------------------------------------------------------------*/

int smallestPow2(int num)
{
        int twoPower=2;

        while (twoPower<num)
        {
                twoPower*=2;
        }
        return twoPower;
}

file *newFile(void)
{
  file *f=(file *)MALLOC(sizeof(file));
  strcpy(f->in,g.in1);
  strcat(strcpy(f->out_cpx,g.out),"_cpx.img");
  strcat(strcpy(f->out_amp,g.out),"_amp.img");
  strcat(strcpy(f->out_pwr,g.out),"_pwr.img");
  strcat(strcpy(f->out_sig,g.out),"_sig.img");
  strcat(strcpy(f->out_gam,g.out),"_gam.img");
  strcat(strcpy(f->out_bet,g.out),"_bet.img");
  f->azpix=azpix;
  f->rngpix=rngpix;
  f->firstLineToProcess=g.ifirstline;
  f->skipFile=g.ifirst+g.isave;
  
  f->firstOutputLine = (n_az-g.na_valid)/2;
  f->skipSamp=g.isave;
  f->n_az_valid=g.na_valid;
  f->nlooks=g.nlooks;
  f->nPatches=g.npatches;
  return f;
}

void atdp_setup(struct ARDOP_PARAMS *g_in, meta_parameters *meta, file **f,
		getRec **signalGetRec)
{
  int az_reflen, skew_lines;
  float slantToLast, a2;
  char tmpOut[255];
  
  /*Set parameters*/
  g=*g_in;

  /*Compute a few bizarre numbers.*/
  n_az = default_n_az;      /* number of azimuth output lines per patch */
  
  /*	velFix=g.vel*sqrt(g.re/(g.re+g.ht)); <- ?? */
  
  azpix=(g.vel*g.re/(g.re+g.ht))/g.prf;
  
  rngpix=speedOfLight/(g.fs*2.0);
  
  refPerRange= g.wavl/(2.0*g.azres*azpix); /* for acpatch's np- the length of 
					      the azimuth reference function	*/
  
  slantToLast=g.r00+(g.isave+g.nla)*rngpix;
  /* For deskewed data, azimuth reference length must include the shift 
     due to deskewing */
  if (g.deskew==1)
    a2= g.wavl/(2.0*g.vel*azpix);
  else
    a2=0.0;
  skew_lines=fabs(a2 *(g.fd + g.fdd*g.nla + g.fddd*g.nla*g.nla)*g.prf*slantToLast
		  - a2 *g.fd*g.prf*g.r00);
  
  az_reflen=(int)(refPerRange*slantToLast+skew_lines);
  
  
  
  if (g.na_valid<0)
    {/*Automatically determine number of valid lines.*/
      /*set valid lines based on n_az.*/
      
      g.na_valid=n_az-az_reflen;
      
      
      
      /*Make g.na_valid an even multiple of g.nlooks.*/
      g.na_valid/=g.nlooks;
      
      g.na_valid*=g.nlooks;
      
    } else
      /*set n_az based on valid lines.*/
      
      n_az=smallestPow2(g.na_valid+az_reflen);
  
  
  meta->sar->slant_shift = 0.0;
  
  meta->sar->time_shift=(n_az-g.na_valid)/g.prf/2.0;
  
  /*Write the version of ATDP*/
  sprintf(meta->general->processor,"ASF/ATDP/%.2f",VERSION);
  
  *signalGetRec=fillOutGetRec(g.in1);
  
  /* Add secondary range compression */
  {
    float r1, fr1, dp1;
    /*float slope;*/
    
    if (!quietflag) printf("   Applying secondary range migration\n");
    
    r1 = g.r00 + rngpix*(g.nla/2.0);
    fr1 = -2.0 *g.vel*g.vel / (g.wavl*r1);
    dp1 = g.fd + g.fdd*r1 + g.fddd * r1*r1;
    
    g.slope = g.slope / (1.0 + g.slope * g.wavl*g.wavl*dp1*dp1 / fr1 / 
			 pow(speedOfLight,2.0)); 
  }
  
  /*Print out the parameters to .in and .meta files*/
  meta->sar->original_line_count = signalGetRec[0]->nLines;
  sprintf(tmpOut, "%s_cpx", g.out);
  meta_write(meta,tmpOut);
  
  /*Write out parameters/structures for main routine to use:*/
  *f=newFile();  
  
}
