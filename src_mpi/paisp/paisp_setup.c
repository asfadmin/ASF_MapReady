/****************************************************************************
*								            *
*   aisp_setup.c -- Routines used to set up the parameters for a run        *
*   Copyright (C) 1997  ASF STEP LAB 			   	    	    *
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
/***********************************************************************
  aisp_setup.c  -- Routines to calculate input parameters for ASP.
  
     FUNCTIONS INCLUDED IN THIS FILE:
        give_usage	- displays usage for program
	parse_cla	- parses the cla's, fills parameters in aisp_global.h
	get_params	- reads from metadata & calculates parameters
	calc_range_ref  - creates range reference function

  2/97  T. Logan	Included ability to estimate dopplers, read
			doppler from file, & read offsets from file
***********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "paisp_defs.h"
#include "odl.h"
#include <mpi.h>
#include <math.h>

extern int my_pe;
extern int n_pes;


/*-------------------------------------------------------------------------*/
/*    The following is the list of all parameters needed to run aisp.c      */
/*-------------------------------------------------------------------------*/
struct AISP_PARAMS g;/*AISP Globals, defined in aisp_global.h*/

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
double getDopplerRate(double r,double f0,GEOLOCATE_REC *g)
{
  /*Get inertial coordinates vectors for everything (satellite and target)*/
  vector Rs,Vs,As;/*Spacecraft position, velocity, accelleration*/
  vector Rt,We={0,0,0};/*Target position, rotation vector of earth (rad/s)*/
        
  double Rvel=-0.5*f0*g->lambda; /*Slant range velocity of target (m/s)*/
  double Racc;/*Slant range accelleration of target, (m/s^2)*/
  vector Rs_m_Rt,Vs_c_Rt,We_c_Rt,We_c_Rs;/*Temporary vectors*/
  double look,yaw,accMag;

  /*Find spacecraft, target position and velocity*/
  Rs=g->stVec.pos;Vs=g->stVec.vel;
  getLookYaw(g,r,f0,&look,&yaw);
  getDoppler(g,look,yaw,NULL,NULL,&Rt,NULL);

  /*Spacecraft orbital acceleration is g*Me/r^2 toward center of earth*/
  accMag=-g->gxMe/vecDot(Rs,Rs);
  As=Rs;
  vecNormalize(&As);
  vecScale(&As,accMag);
  We.z=g->angularVelocity;
  
  /*Compute the terms for Racc*/
  vecSub(Rs,Rt,&Rs_m_Rt);
  vecCross(Vs,Rt,&Vs_c_Rt);
  vecCross(We,Rt,&We_c_Rt);
  vecCross(We,Rs,&We_c_Rs);
  Racc=(vecDot(As,Rs_m_Rt) + vecDot(Vs,Vs) + 2*vecDot(We,Vs_c_Rt)  
        +vecDot(We_c_Rt,We_c_Rs) - Rvel*Rvel) / r;
  return -2*Racc/g->lambda;
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
		twoPower*=2;
	return twoPower;
}

/************************************************************************
Structure creation routines: These set the values
in a structure based on the globals above.
*/

satellite *newSatellite(void)
{
	float slantToLast;
	int err=0; // Error codes for the ODL interface 
        char errC=0;
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
		if (IM_DSP)
		{
			printf("   You can't have both a Kaiser and a Hamming window simultaneously\n");
       	        	printf("   Using a rectangular window in azimuth\n");
       	        }
		s->hamming=0;
       	        s->kaiser=0;
      	}
	if(s->hamming==1)
		if (IM_DSP)
			printf("   Using Hamming window on Azimuth Reference function\n");
       	if(s->kaiser==1)
		if (IM_DSP)
			printf("   Using Kaiser window on Azimuth Reference function\n");
	
	if(s->imageType.power==1) 
		if (IM_DSP)
	  		printf("   Calculating power image\n");	

	if(s->imageType.sigma==1) 
		if (IM_DSP)	  
			printf("   Calculating sigma_0 image\n");	
	  
	if(s->imageType.gamma==1) 
	  	if (IM_DSP)
			printf("   Calculating gamma_0 image\n");	
	  
	if(s->imageType.beta==1) 
	  	if (IM_DSP)
			printf("   Calculating beta_0 image\n");
	
	/* Check to make sure the bandwidth truncation values are in the correct range */
	if(s->pctbw<1 && s->pctbw>0) {
		if(IM_DSP)
			printf("   Truncating range bandwidth by %.1f percent\n",s->pctbw*100);
	} else
		s->pctbw=0;

	if(s->pctbwaz<1 && s->pctbwaz>0) {
		if(IM_DSP)
			printf("   Truncating azimuthal bandwidth by %.1f percent\n",s->pctbwaz*100);
	} else
		s->pctbwaz=0;

/* If there is a new filename in place of the initial "NO", then read in the look angle and
        the gain vectors and put them in the satellite structure */
        if((strcmp(g.CALPRMS,"NO")!=0))
        {
                /* If we have a CalParams file, then lets allocate the required memory for the vectors,
                   255 was chosen as an arbitrary length, I think that they are less than 100 entries but...*/
                s->ang_vec=(double *)MALLOC(sizeof(double)*255);
                s->gain_vec=(double *)MALLOC(sizeof(double)*255);

                /* Check to see if we have any trouble opening the CAL_PARAMS file */
                if((/*errI=*/ODLinit())!=0)
                        {printf("Cannot Initialize ODL Structure, exiting\n"); exit(1);}
                odl=ODLparse(g.CALPRMS,0,&errC);
                if(errC!=0)
                        {printf("Could not Parse the ODL Structure, Possible Invalid CAL_PARAMS file, exiting\n"); exit(1);}

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
                }
		if (IM_DSP) printf("   Calibration Parameters File Read, Elevation Angle and Gain Vectors stored\n");
        }


/*Compute a few bizarre numbers.*/ 
 	/* s->a2*r*fd=amount of doppler deskewing in pixels.*/
	if (s->ideskew==1)
		s->a2= s->wavl/(2.0*s->vel*azpix);
	else
		s->a2=0.0;
	
	slantToLast=g.r00+(g.isave+g.nla)*rngpix;
/*	if (IM_DSP) printf("Slant to first=%.2f; slant to last=%.2f\n",g.r00+g.isave*rngpix,slantToLast);*/
	
	s->refPerRange=refPerRange;
	s->az_reflen=refPerRange*slantToLast;
/*	if (IM_DSP) 
	 printf("The azimuth reference function is at most %d lines long\n",s->az_reflen);*/

/*Trade doppler deskew for shifted line start.*/
	s->dop_precomp=(int)(s->a2*g.fd*g.prf*g.r00);
/*	if (s->ideskew==1)
 	  if (IM_DSP)
            printf("Precompensating by %d pixels for doppler deskew...\n",s->dop_precomp);*/

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
/*	if (my_pe == 0) printf("The range reference function is %d samples long\n",r->refLen);*/
	r->rangeFFT=smallestPow2(num_range+r->refLen);
	r->ref=(FCMPLX *)MALLOC(sizeof(FCMPLX)*r->rangeFFT);
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
	p->trans =(FCMPLX *) MALLOC (p->n_range*p->n_az*sizeof(FCMPLX));
	p->slantPer=rngpix;
	p->g=NULL;
	return p;
}

/*Find all other patch routines in patch.c*/


/*****************************************************************
parse_cla:
	Performs all initialization and parameter determination
for aisp.  Returns parameters in the given structures.
*/

void aisp_setup(struct AISP_PARAMS *g_in,meta_parameters *meta,int *N_az,int *N_range,
	satellite **s,rangeRef **r,file **f,getRec **signalGetRec)
{
	int az_reflen;
	float slantToLast;

/*Set parameters*/
	g=*g_in;
	
/*Compute a few bizarre numbers.*/
	if (g.n_az == -99)
		n_az = default_n_az;      /* number of azimuth output lines per patch */
	else 
		n_az = g.n_az;
	
	azpix=(g.vel*g.re/(g.re+g.ht))/g.prf;
	rngpix=speedOfLight/(g.fs*2.0);
	refPerRange= g.wavl/(2.0*g.azres*azpix); /* for acpatch's np- the length of 
			the azimuth reference function	*/
	slantToLast=g.r00+(g.isave+g.nla)*rngpix;
	az_reflen=refPerRange*slantToLast;
	
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
	
	meta->geo->slantShift=0;
	meta->geo->timeShift=(n_az-g.na_valid)/g.prf/2.0;
	if (meta->info)
	/*Write the version of AISP*/
		sprintf(meta->info->processor,"ASF/AISP/%.2f",VERSION);

	*signalGetRec=fillOutGetRec(g.in1);

	/* Add secondary range compression */
	{	
	  float r1, fr1, dp1;

/*	  if (my_pe == 0) printf("Applying secondary range migration\n");*/

	  r1 = g.r00 + rngpix*(g.nla/2.0);
	  fr1 = -2.0 *g.vel*g.vel / (g.wavl*r1);
	  dp1 = g.fd + g.fdd*r1 + g.fddd * r1*r1;
	
	  g.slope = g.slope / (1.0 + g.slope * g.wavl*g.wavl*dp1*dp1 / fr1 / pow(speedOfLight,2.0));
	}

/*Print out the parameters to .in and .meta files*/
	print_params(g.out,&g,"aisp");
	meta->ifm->orig_nLines=signalGetRec[0]->nLines;
	MPI_Barrier(MPI_COMM_WORLD);
	if (IM_DSP) meta_write(meta,g.out);

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
void calc_range_ref(FCMPLX *ref,int rangeFFT,int chirpSamples)
{
	float refGain;
	int i;
	FILE *refFP;


/*Read in pulse replica if one exists
	if (extExists(g.in1,".replica"))
		fetchReferenceFunction(g.in1,ref,chirpSamples);
	else 
*/
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
		ref[i].r=ref[i].i=0.0;
	
/*Scale amplitude of ref. function via a cosine on a pedestal*/
	for (i=0; i < chirpSamples; i++)
		ref[i]=Csmul(g.rhww-(1.0-g.rhww)*cos(2.0*pi*i/chirpSamples),ref[i]);
/*	if (IM_DSP) printf("\nRange reference function calculated in time domain\n");*/
	
/*Output the reference function (for debugging)*/
	if (g.iflag&2)
	{
		refFP=fopen("reference_time","w");
		for (i=0;i<chirpSamples;i++)
			fprintf(refFP,"%i  %f %f  %f %f\n",i,Cabs(ref[i]),atan2(ref[i].i,ref[i].r),
				ref[i].r,ref[i].i);
		fclose(refFP);
	}
	
/*Now forward-fft the reference function.*/
	cfft1d(rangeFFT,ref,0);
	cfft1d(rangeFFT,ref,-1);
	
/*Take the reference function's complex conjugate.*/
	refGain=1.0/chirpSamples;
	for (i=0; i< rangeFFT; i++)
	{
		ref[i].r*=refGain;
		ref[i].i*=-refGain;
	}
	
/*Eliminate the DC offset & scale away very low frequencies (FFT weighting) */
#define scaleLowFreq 3
	ref[0].r=ref[0].i=0.0;
	for (i=1; i< scaleLowFreq; i++)
	{
		float wgt = 0.5 - 0.5 * cos(pi*i/scaleLowFreq);
		ref[i]  = Csmul(wgt,ref[i]);
		ref[rangeFFT-i] = Csmul(wgt,ref[rangeFFT-i]);
	}

/*Output the FFT'd reference function (for debugging)*/
	if (g.iflag&2)
	{
		refFP=fopen("reference_freq","w");
		for (i=0;i<rangeFFT;i++)
			fprintf(refFP,"%i  %f %f  %f %f\n",i,Cabs(ref[i]),atan2(ref[i].i,ref[i].r),
			    ref[i].r,ref[i].i);
		fclose(refFP);
	}
}
