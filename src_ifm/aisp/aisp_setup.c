/****************************************************************************
*								            *
*   aisp_setup.c -- Routines used to set up the parameters for a run        *
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
#include "aisp_defs.h"
#include "asf_meta.h"
#include "odl.h"
#include <math.h>


/*-------------------------------------------------------------------------*/
/*    The following is the list of all parameters needed to run aisp.c      */
/*-------------------------------------------------------------------------*/
struct AISP_PARAMS g;/*AISP Globals, defined in aisp_params.h*/

int     n_az;         /* Number of lines in the Azimuth per patch   */
float azpix,rngpix; /*Size of pixel in azimuth and slant range.*/
float refPerRange;/*Reference function length per meter of range.*/

/*-------------------------------------------------------------------------*/


/********************************************
Compute the doppler rate (Hz/sec) given:
r-- slant range to target (m)
f0-- doppler at target (Hz)
g-- Geolocation record, initialized with appropriate 
   inertial coordinates state vector

From equation B.1.16, p. 569, Curlander & McDonough SAR Bible
by Orion Lawlor, olawlor@acm.org, 7/21/2000
*/
double getDopplerRate(double r,double f0,GEOLOCATE_REC *my_g)
{
  /*Get inertial coordinates vectors for everything (satellite and target)*/
  vector Rs,Vs,As;/*Spacecraft position, velocity, accelleration*/
  vector Rt,We={0,0,0};/*Target position, rotation vector of earth (rad/s)*/

  double Rvel=-0.5*f0*g.wavl; /*Slant range velocity of target (m/s)*/
  double Racc;/*Slant range accelleration of target, (m/s^2)*/
  vector Rs_m_Rt,Vs_c_Rt,We_c_Rt,We_c_Rs;/*Temporary vectors*/
  double look,yaw,accMag;
  /*Find spacecraft, target position and velocity*/
  Rs=my_g->stVec.pos;Vs=my_g->stVec.vel;
  getLookYaw(my_g,r,f0,&look,&yaw);
  getDoppler(my_g,look,yaw,NULL,NULL,&Rt,NULL);

  /*Spacecraft orbital acceleration is g*Me/r^2 toward center of earth*/
  accMag=-my_g->gxMe/vecDot(Rs,Rs);
  As=Rs;
  vecNormalize(&As);
  vecScale(&As,accMag);
  We.z=my_g->angularVelocity;

  /*Compute the terms for Racc*/
  vecSub(Rs,Rt,&Rs_m_Rt);
  vecCross(Vs,Rt,&Vs_c_Rt);
  vecCross(We,Rt,&We_c_Rt);
  vecCross(We,Rs,&We_c_Rs);
  Racc=(vecDot(As,Rs_m_Rt) + vecDot(Vs,Vs) + 2*vecDot(We,Vs_c_Rt)
	+vecDot(We_c_Rt,We_c_Rs) - Rvel*Rvel) / r;
  return -2*Racc/g.wavl;
}


		

/******************************************************************
We begin with a collection of small utility routines.
*/

/*SmallestPow2:
	Returns the smallest power of 2 larger than the given number.
*/
int smallestPow2(int num)
{
	int twoPower=2;

	while (twoPower<num)
	{
		twoPower*=2;
	}
	return twoPower;
}

/************************************************************************
Structure creation routines: These set the values
in a structure based on the globals above.
*/

satellite *newSatellite(void)
{
	float slantToLast;
	int err=0; /* Error codes for the ODL interface */
	char errC=0;
	int my_precomp; /* far range precompensation value */
	int cols; /* Dummy Variable to hold the number of columns in the antenna pattern correction vector */
	satellite *s=(satellite *)MALLOC(sizeof(satellite));
	ODL odl;
/*Copy over a few parameters.*/
	s->wavl=g.wavl;
	s->vel=g.vel;
	s->prf=g.prf;
	s->debugFlag=g.iflag;
	s->ideskew=g.deskew;
	s->hamming=g.hamFlag;
	s->kaiser=g.kaiFlag;
	s->imageType.power=g.pwrFlag;
	s->imageType.sigma=g.sigmaFlag;
	s->imageType.gamma=g.gammaFlag;
	s->imageType.beta=g.betaFlag;
	s->vecLen=0;
	s->noise=0;
	s->gain=1;
	s->pctbw=g.pctbw;
	s->pctbwaz=g.pctbwaz;

	if(s->hamming==1 && s->kaiser==1)
	{
		printf("   You can't have both a Kaiser and a Hamming window simultaneously\n");
		printf("   Using a rectangular window in azimuth\n");
		if (logflag) {
		  printLog("   You can't have both a Kaiser and a Hamming window simultaneously\n");
		  printLog("   Using a rectangular window in azimuth\n");
		}
		s->hamming=0;
		s->kaiser=0;
	}
	if(s->hamming==1) {
	  printf("   Using Hamming window on Azimuth Reference function\n");
	  if (logflag) printLog("   Using Hamming window on Azimuth Reference function\n");
	}
	if(s->kaiser==1) {
	  printf("   Using Kaiser window on Azimuth Reference function\n");
	  if (logflag) printLog("   Using Kaiser window on Azimuth Reference function\n");
	}
	if(s->imageType.power) {
	  printf("   Calculating power image\n");	
	  if (logflag) printLog("   Calculating power image\n");
	}
	if(s->imageType.sigma) {
	  printf("   Calculating sigma_0 image\n");	
	  if (logflag) printLog("   Calculating sigma_0 image\n");
	}	
	if(s->imageType.gamma) {
	  printf("   Calculating gamma_0 image\n");	
	  if (logflag) printLog("   Calculating gamma_0 image\n");
	}	
	if(s->imageType.beta) {
	  printf("   Calculating beta_0 image\n");	
	  if (logflag) printLog("   Calculating beta_0 image\n");
	}	
	

	/* Check to make sure the bandwidth truncation values are in the correct range */
	if(s->pctbw<1 && s->pctbw>0) {
		if (!quietflag) printf("   Truncating range bandwidth by %.1f percent\n",s->pctbw*100);
		if (logflag) {
		  sprintf(logbuf,"\n   Truncating range bandwidth by %.1f percent\n",s->pctbw*100);
		  printLog(logbuf);
		}
	}
	else
		s->pctbw=0;

	if(s->pctbwaz<1 && s->pctbwaz>0) {
		if (!quietflag) printf("   Truncating azimuthal bandwidth by %.1f percent\n",s->pctbwaz*100);
		if (logflag) {
		  sprintf(logbuf,"\n   Truncating azimuthal bandwidth by %.1f percent\n",s->pctbwaz*100);
		  printLog(logbuf);
		}
	}
	else
		s->pctbwaz=0;




	/* If there is a new filename in place of the initial "NO", then read in the look angle and 
	the gain vectors and put them in the satellite structure */
	if(strcmp(g.CALPRMS,"NO")!=0)
	{
		/* If we have a CalParams file, then lets allocate the required memory for the vectors,
		   255 was chosen as an arbitrary length, I think that they are less than 100 entries but...*/
		s->ang_vec=(double *)MALLOC(sizeof(double)*255);
		s->gain_vec=(double *)MALLOC(sizeof(double)*255);
		
		/* Check to see if we have any trouble opening the CAL_PARAMS file */
		if((/*errI=*/ODLinit())!=0) {
		  sprintf(errbuf, "   ERROR: Cannot Initialize ODL Structure, exiting\n");
		  printErr(errbuf);
		}
		odl=ODLparse(g.CALPRMS,0,&errC);
		if(errC!=0) {
		  sprintf(errbuf, "   ERROR: Could not Parse the ODL Structure, Possible Invalid CAL_PARAMS file, exiting\n");
		  printErr(errbuf);
		}

		/* We opened the CAL_PARAMS file, read in the vectors */
		s->ang_vec=(double *)ODLGetArrayDouble(odl,"CAL_PARAM.DETAILED_METADATA.ANTPTN_OBJ.ELEVANG_VEC",0,&cols,&s->vecLen);
		s->gain_vec=(double *)ODLGetArrayDouble(odl,"CAL_PARAM.DETAILED_METADATA.ANTPTN_OBJ.GAIN_VEC",0,&cols,&s->vecLen);
		s->noise=(double)ODLGetDouble(odl,"CAL_PARAM.DETAILED_METADATA.CALIB_FAC.NOISE_FACT",&err);
		s->gain=(double)ODLGetDouble(odl,"CAL_PARAM.DETAILED_METADATA.CALIB_FAC.LINEAR_CONV_FACT",&err);
		/* Convert the look angle to radians to match the output of meta_look */
		for(cols=0;cols<s->vecLen;cols++)
		{
			s->ang_vec[cols]=(s->ang_vec[cols]/180)*pi;
			s->gain_vec[cols]=pow(10,-2*s->gain_vec[cols]/10);
/*			printf("%f %f\n",s->ang_vec[cols],s->gain_vec[cols]); */
		}
		printf("   Calibration Parameters File Read, Elevation Angle and Gain Vectors stored\n");
	}
			
/*Compute a few bizarre numbers.*/ 
 	/* s->a2*r*fd=amount of doppler deskewing in pixels.*/
	if (s->ideskew==1)
		s->a2= s->wavl/(2.0*s->vel*azpix);
	else
		s->a2=0.0;
	
	slantToLast=g.r00+(g.isave+g.nla)*rngpix;
	if (!quietflag) printf("   Slant to first=%.2f; slant to last=%.2f\n",g.r00+g.isave*rngpix,slantToLast);
	
	s->refPerRange=refPerRange;
	s->az_reflen=refPerRange*slantToLast;
	if (!quietflag) printf("   The azimuth reference function is at most %d lines long\n",s->az_reflen);

/*Shift patch to midpoint between near and far precompensation when deskew flag is on.*/
	
	s->dop_precomp=(int)(s->a2*g.fd*g.prf*g.r00);
	my_precomp = (int)(s->a2*(g.fd + g.fdd*g.nla + g.fddd*g.nla*g.nla)*g.prf*slantToLast);
	s->dop_precomp=(s->dop_precomp + my_precomp)/2;
		
	if (!quietflag && s->ideskew==1)
		printf("   Precompensating by %d pixels for doppler deskew...\n",s->dop_precomp);

/*Trade resampling offset for line/sample start.*/
	g.ifirstline+=(int)g.intera;
	g.intera-=(int)g.intera;
	g.ifirst+=(int)g.interr;
	g.interr-=(int)g.interr;

/*Save "original" parameters.*/
	s->orig_slantToFirst=g.r00;
	s->orig_fd=g.fd,s->orig_fdd=g.fdd,s->orig_fddd=g.fddd;
	s->sloper=g.sloper,s->interr=g.interr,s->slopea=g.slopea,s->intera=g.intera;
	s->dsloper=g.dsloper,s->dinterr=g.dinterr,s->dslopea=g.dslopea,s->dintera=g.dintera;
	return s;
}

rangeRef *newRangeRef(int num_range)
{
	rangeRef *r=(rangeRef *)MALLOC(sizeof(rangeRef));

	r->refLen=g.pulsedur*g.fs;
	r->rangeFFT=smallestPow2(num_range+r->refLen);
	r->ref=(complexFloat *)MALLOC(sizeof(complexFloat)*r->rangeFFT);
	calc_range_ref(r->ref,r->rangeFFT,r->refLen);

	return r;
}

file *newFile(void)
{
	file *f=(file *)MALLOC(sizeof(file));
	strcpy(f->in,g.in1);
	strcat(strcpy(f->out_cpx,g.out),".cpx");
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

/*
newPatch:
Creates a patch of data.  Sets relevant parameters from globals, 
and allocates memory for a patch of the given size.
*/
patch *newPatch(int num_az,int num_range)
{
	patch *p=(patch *)MALLOC(sizeof(patch));
	p->n_az=num_az;
	p->n_range=num_range;
	p->trans =(complexFloat *) MALLOC (p->n_range*p->n_az*sizeof(complexFloat));
	p->slantPer=rngpix;
	p->g=NULL;
	return p;
}

/*
copyPatch:
Copies a patch of data.  Sets relevant parameters from globals, 
and allocates memory for a patch of the given size.
*/
patch *copyPatch(patch *oldPatch)
{
        complexFloat *old_trans = oldPatch->trans;
	patch *p=(patch *)MALLOC(sizeof(patch));
	*p = *oldPatch;
	p->trans =(complexFloat *) MALLOC (p->n_range*p->n_az*sizeof(complexFloat));
	memcpy(p->trans,old_trans,p->n_range*p->n_az*sizeof(complexFloat));
	return p;
}


/*Find all other patch routines in patch.c*/


/*****************************************************************

*/

void aisp_setup(struct AISP_PARAMS *g_in,meta_parameters *meta,int *N_az,int *N_range,
	satellite **s,rangeRef **r,file **f,getRec **signalGetRec)
{
	int az_reflen, skew_lines;
	float slantToLast, a2;

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
	/* For deskewed data, azimuth reference length must include the shift due to deskewing */
	if (g.deskew==1)
		a2= g.wavl/(2.0*g.vel*azpix);
	else
		a2=0.0;
	 skew_lines=fabs(a2 *(g.fd + g.fdd*g.nla + g.fddd*g.nla*g.nla)*g.prf*slantToLast- a2 *g.fd*g.prf*g.r00);

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

	/*Write the version of AISP*/
	sprintf(meta->general->processor,"ASF/AISP/%.2f",VERSION);

	*signalGetRec=fillOutGetRec(g.in1);

	/* Add secondary range compression */
        {
          float r1, fr1, dp1;
          /*float slope;*/

          if (!quietflag) printf("   Applying secondary range migration\n");

          r1 = g.r00 + rngpix*(g.nla/2.0);
          fr1 = -2.0 *g.vel*g.vel / (g.wavl*r1);
          dp1 = g.fd + g.fdd*r1 + g.fddd * r1*r1;

          g.slope = g.slope / (1.0 + g.slope * g.wavl*g.wavl*dp1*dp1 / fr1 / pow(speedOfLight,2.0)); 
        }
	
/*Print out the parameters to .in and .meta files*/
	print_params(g.out,&g,"aisp");
	meta->sar->original_line_count = signalGetRec[0]->nLines;
	meta_write(meta,g.out);

/*Write out parameters/structures for main routine to use:*/
	*r=newRangeRef(g.nla);
	*s=newSatellite();
	*f=newFile();
	*N_az=n_az;
	*N_range=g.nla;
	

}


/****************************************************************************
calc_range_ref:
	Called by rciq during its first execution, calc_range_ref computes
the range reference function.  This is then forward-fft'd, conjugated, and
returned in ref, which must be rangeFFT long.

Global Inputs:
	g.in1,g.slope,g.rhww,g.iflag;

*/
void calc_range_ref(complexFloat *ref,int rangeFFT,int chirpSamples)
{
	float refGain;
	int i;
	FILE *refFP;

/*Read in pulse replica if one exists
	if (extExists(g.in1,".replica"))
		fetchReferenceFunction(g.in1,ref,chirpSamples);
	else */

	{
/*Else construct linear FM pulse replica.*/
		for (i=0; i<chirpSamples; i++)
		{
			float t = (i-chirpSamples/2) / g.fs;
			float phase = pi*g.slope*t*t;
			ref[i]=Cmplx(cos(phase),sin(phase));
		}
	}
	
/*Fill up rest of ref. buffer with zeros.*/
	for (i=chirpSamples; i<rangeFFT; i++)
		ref[i].real = ref[i].imag = 0.0;
	
/*Scale amplitude of ref. function via a cosine on a pedestal*/
	for (i=0; i < chirpSamples; i++)
		ref[i]=Csmul(g.rhww-(1.0-g.rhww)*cos(2.0*pi*i/chirpSamples),ref[i]);
	if (!quietflag) printf("\n   Range reference function calculated in time domain\n");
	
/*Output the reference function (for debugging)*/
	if (g.iflag & RANGE_REF_T)
	{
		refFP=FOPEN("range_ref_t","w");
		for (i=0;i<chirpSamples;i++)
			fprintf(refFP,"%i  %f %f  %f %f\n",i,Cabs(ref[i]),
                                atan2(ref[i].imag,ref[i].real),
                                ref[i].real, ref[i].imag);
		fclose(refFP);
	}
	
/*Now forward-fft the reference function.*/
	cfft1d(rangeFFT,ref,0);
	cfft1d(rangeFFT,ref,-1);
	
/*Take the reference function's complex conjugate.*/
	refGain=1.0/chirpSamples;
	for (i=0; i< rangeFFT; i++)
	{
		ref[i].real *= refGain;
		ref[i].imag *= -refGain;
	}
	
/*Eliminate the DC offset & scale away very low frequencies (FFT weighting) */
#define scaleLowFreq 3
	ref[0].real = ref[0].imag = 0.0;
	for (i=1; i< scaleLowFreq; i++)
	{
		float wgt = 0.5 - 0.5 * cos(pi*i/scaleLowFreq);
		ref[i]  = Csmul(wgt,ref[i]);
		ref[rangeFFT-i] = Csmul(wgt,ref[rangeFFT-i]);
	}

/*Output the FFT'd reference function (for debugging)*/
	if (g.iflag & RANGE_REF_F)
	{
		refFP=FOPEN("range_ref_f","w");
		for (i=0;i<rangeFFT;i++)
			fprintf(refFP,"%i  %f %f  %f %f\n",i,Cabs(ref[i]),
                            atan2(ref[i].imag, ref[i].real),
			    ref[i].real, ref[i].imag);
		fclose(refFP);
	}
}
