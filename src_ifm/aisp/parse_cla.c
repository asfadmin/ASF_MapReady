/****************************************************************************
*								            *
*   parse_cla.c:  Routines used to parse AISP command line parameters       *
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
  parse_cla.c  -- Routines to parse AISP's input parameters.
  
     FUNCTIONS INCLUDED IN THIS FILE:
	parse_cla	- parses the cla's, fills parameters in aisp_global.h
	get_params	- reads from metadata & calculates parameters
	calc_range_ref  - creates range reference function

  2/99    O. Lawlor   Initial separation from aisp_setup for quicklook.
  4/02    P. Denny    Rewrote commandline parsing.  A pity because
                         it was already beautiful.  That is the price
			 of conformity I guess.
  10/02	  J. Nicoll	Updated deskew options to remove wedges from data.
***********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "aisp_defs.h"
#include "geolocate.h"

/* this define is a hack to avoid calling usage() as done in cla.h */	
#define CHK_ARG_ASP(num_args) if (currArg+num_args>argc) \
  {printf("   *****You need %i arguments for the keyword %s.\n\n",currArg-argc+num_args,argv[currArg-1]);return 0;} \
  else currArg+=num_args;

/*Prototypes:*/
void get_params(char *,struct AISP_PARAMS *,meta_parameters **);


/******************************************
Parse_cla:
	Parses AISP command-line options to
determine SAR processing parameters.  Returns
0 on user command-line error, 1 on success, 2 
on debug help, or not at all on other errors.  
Meta_out will contain a pointer to the metadata 
for this scene.

******************************************/

int parse_cla(int argc,char *argv[],struct AISP_PARAMS *g,meta_parameters **meta_out)
{
	int read_offset = 0,   /* Flag - Read resampling offsets from file?   */
	    read_dopplr = 0;   /* Flag - Read doppler constant from file?     */
	int cal_check=0;       /* checks if output file needs input cal file  */
	int debug_help=-1;     /* If -debug 0 used, give debug usage          */
	char fName_slope[256], /* Input slope,intercept (offsets) file        */
	   fName_doppler[256]; /* Input doppler constant file                 */
	FILE  *fp;
	meta_parameters *meta;
	
	/* Preset Optional Command Line Parameters
	   ---------------------------------------------------------*/
	g->hamFlag=0;
	g->kaiFlag=0;
	g->pwrFlag=0;
	g->sigmaFlag=0;
	g->gammaFlag=0;
	g->betaFlag=0;
	g->nextend=0.0;
	g->pctbw=g->pctbwaz=0.0;
	g->fd=0;
	g->fdd=-99;   /*Doppler slope of -99 means to figure it out ourselves.*/
	g->fddd=-99;  /*Doppler slope of -99 means to figure it out ourselves.*/
	g->ifirstline = 0;  /* First line to process                          */
	g->npatches = 100;  /* (Absurdly Large) Number of patches             */
	g->ifirst = 0;      /* i,q byte samples to skip                       */
	g->isave = 0;       /* start range bin                                */
	g->na_valid = -99;  /* Valid output samples per patch (-99->determine)*/
	g->iflag = 1;       /* Debug Flag                                     */
	g->deskew=0;
	g->sloper = g->interr = g->slopea = g->intera = 0.0;
	g->dsloper = g->dinterr = g->dslopea = g->dintera = 0.0;  
	strcpy(g->CALPRMS,"NO");
	
	/* Process the Command Line Arguments
	   ------------------------------------*/
	if (argc<3)
		{printf("Must give input and output filenames.\n");return 0;}
	strcpy(g->in1,argv[argc-2]);
	strcpy(g->out,argv[argc-1]);

	
	/*Create AISP_PARAMS struct as well as meta_parameters.*/
	if (extExists(g->in1,".in"))
	{/*Read parameters from AISP parameter file*/
		read_params(g->in1,g);
		if (extExists(g->in1,".meta"))/*Input file has .meta attached: read it*/
			meta=meta_read(g->in1);
		else /*No .meta exists--fabricate one*/
			meta=raw_init();
	}
	else    /*Read parameters & .meta from CEOS.*/
		get_params(g->in1,g,&meta);

	while (currArg < (argc-2)) {
		char *key=argv[currArg++];
		if      (strmatch(key,"-log"))   {CHK_ARG_ASP(1); strcpy(logFile, GET_ARG(1));
						  logflag = 1; fLog = FOPEN(logFile, "a");}
		else if (strmatch(key,"-debug")) {
			CHK_ARG_ASP(1);
			g->iflag    = atoi(GET_ARG(1)); 
			if (g->iflag==0) return debug_help; }
		else if (strmatch(key,"-quiet")) {quietflag = 1;}
		else if (strmatch(key,"-power")) {g->pwrFlag=1;}
		else if (strmatch(key,"-sigma")) {g->sigmaFlag=1; cal_check=1;}
		else if (strmatch(key,"-gamma")) {g->gammaFlag=1; cal_check=1;}
		else if (strmatch(key,"-beta" )) {g->betaFlag=1; cal_check=1;}
		else if (strmatch(key,"-hamming")) {g->hamFlag = 1;}
		else if (strmatch(key,"-kaiser"))  {g->kaiFlag = 1;}
		else if (strmatch(key,"-l")) {CHK_ARG_ASP(1); g->ifirstline = atoi(GET_ARG(1));}
		else if (strmatch(key,"-p")) {CHK_ARG_ASP(1); g->npatches = atoi(GET_ARG(1));}
		else if (strmatch(key,"-f")) {CHK_ARG_ASP(1); g->isave   += atoi(GET_ARG(1));}
		else if (strmatch(key,"-s")) {CHK_ARG_ASP(1); g->ifirst  += atoi(GET_ARG(1));}
		else if (strmatch(key,"-n")) {CHK_ARG_ASP(1); g->nla      = atoi(GET_ARG(1));}
		else if (strmatch(key,"-r")) {CHK_ARG_ASP(1); g->azres    = atof(GET_ARG(1));}
		else if (strmatch(key,"-e")) {CHK_ARG_ASP(1); g->deskew   = atoi(GET_ARG(1));
		 			      g->na_valid =-99;}
	/* If you use the -e flag, then a deskew wedge in the data will need to be removed. Override  
	internal measurement of the number of valid azimuth lines with the -v option flag */
		else if (strmatch(key,"-v")) {CHK_ARG_ASP(1); g->na_valid = atoi(GET_ARG(1));}
		else if (strmatch(key,"-o")) {CHK_ARG_ASP(1); strcpy(fName_slope,GET_ARG(1));
						read_offset = 1;}
		else if (strmatch(key,"-c")) {CHK_ARG_ASP(1); strcpy(fName_doppler,GET_ARG(1));
						read_dopplr = 1;}
		else if (strmatch(key,"-m")) {CHK_ARG_ASP(1);strcpy(g->CALPRMS,GET_ARG(1));}
		else {printf("**Invalid option: %s\n\n",argv[currArg-1]); return 0;}
	}
	if ((strcmp(g->CALPRMS,"NO")==0)&&(cal_check==1))
	{
		printf("   You can only use -sigma, -beta, or -gamma in conjunction with -m\n");
		printf("   I will proceed, ignoring the option.\n");
		g->sigmaFlag=0;
		g->gammaFlag=0;
		g->betaFlag=0;
	}

	if (read_offset) {
		fp=FOPEN(fName_slope,"r");
		fscanf(fp,"%f %f %f %f",&g->sloper,&g->interr,&g->slopea,&g->intera);
		fscanf(fp,"%g %g %g %g",&g->dsloper,&g->dinterr,&g->dslopea,&g->dintera);
		FCLOSE(fp);
	}
	if (read_dopplr) {
		fp=FOPEN(fName_doppler,"r");
		g->fd=g->fdd=g->fddd=0.0;
		fscanf(fp,"%f %f %f", &g->fd,&g->fdd,&g->fddd);
		FCLOSE(fp);
	}
	
	if (g->fdd==-99.0)
	{
		double old_dop=g->fd;
	/*Estimate Doppler in scene center.*/
		estdop(g->in1, 1000, &g->fd,&g->fdd,&g->fddd);
	/*De-ambiguify doppler based on old value*/
		while (g->fd-old_dop<-0.5) g->fd+=1.0;
		while (g->fd-old_dop> 0.5) g->fd-=1.0;
	}

/*Copy fields from AISP_PARAMS struct to meta_parameters struct.*/
	meta->sar->image_type              = 'S';          /*Slant range image*/
	meta->sar->look_count              = g->nlooks;
	meta->sar->deskewed                = g->deskew;
	meta->sar->range_time_per_pixel    = 1.0/g->fs;
	meta->sar->azimuth_time_per_pixel  = 1.0/g->prf;
	meta->sar->slant_shift             = g->slantOff;
	meta->sar->time_shift              = g->timeOff;
	meta->sar->slant_range_first_pixel = g->r00;
	meta->sar->wavelength              = g->wavl;
	meta->sar->prf                     = g->prf;
	meta->sar->earth_radius            = g->re;
	meta->sar->satellite_height        = g->re+g->ht;
	meta->sar->range_doppler_coefficients[0] = g->fd*g->prf;
	meta->sar->range_doppler_coefficients[1] = g->fdd*g->prf;
	meta->sar->range_doppler_coefficients[2] = g->fddd*g->prf;
	meta->sar->azimuth_doppler_coefficients[0] = g->fd*g->prf;
	meta->sar->azimuth_doppler_coefficients[1] = 0.0;
	meta->sar->azimuth_doppler_coefficients[2] = 0.0;

	strcpy (meta->general->system, meta_get_system());
	meta->general->data_type = REAL32;
	meta->general->band_number = 0;
	meta->general->x_pixel_size = meta->sar->range_time_per_pixel
	                                                   * (speedOfLight/2.0);
	meta->general->y_pixel_size = meta->sar->azimuth_time_per_pixel
	                                               * g->vel * (g->re/g->ht);

	*meta_out = meta;

	return 1;
}

/******************************************************************************
NAME:	get_params - Creates input file for the ASF SAR processor
SYNOPSIS: get_params(in)
DESCRIPTION:
   Fills all values in aisp_global.h.  The purpose is to automate creation
   of input parameter files by g->interrogating the CEOS metadata structures.

EXTERNAL ASSOCIATES:
    get_asf_facdr	 Retrieves ASF facility data record
    get_ifiledr		 Retrieves image file descriptor record
    get_dssr		 Retrieves data set summary record
    jpl_odl_library	 Retrieves information in odl Engineering files

FILE REFERENCES:   in1.dat, in1.trl, in1.ldr	Extraction of metadata	

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    --------------------------------------------------------------------------
    0.0	     7/96  T. Logan 	Automatically Create SAR Processor Input File
    1.0	    11/96  T. Logan	Create version for SAR Processor (no INSAR)
    2.0	    1/98  O. Lawlor  Modfied to support RADARSAT

ALGORITHM DESCRIPTION: Fills each of the ASP globals with appropriate values.
  Parameters are read from CEOS, hardcoded, calculated, or estimated.
******************************************************************************/
#include "odl.h"

void get_params(char *file,struct AISP_PARAMS *g,meta_parameters **meta_out)
{
	meta_parameters *meta;
	struct VFDRECV asf_facdr1;
	struct IOF_VFDR ifiledr;
	struct dataset_sum_rec dssr1;

/* First, we set the hardcoded parameters: */
	g->rhww = 0.8;             /* Range Spectrum Weight - a little    */
	g->nlooks = 5;             /* Produces square pixels (5 X 1 look) */
	g->wavl = 0.0565646;       /* SAR wavelength, in meters.          */
	g->slope = 4.1913749E+11;  /* Chirp Frequency slope (Hz/Sec)      */
	g->azres = 8.0;            /* ERS-1 azimuth resolution, in meters */
	
/* Get .meta metadata parameters */
	meta=meta_create(file);
	
/* Read CEOS structures */
	get_asf_facdr(file,&asf_facdr1);
	get_ifiledr(file,&ifiledr);
	get_dssr(file,&dssr1);

/* Extract parameters of interest from CEOS metadata */
        g->nla      = ifiledr.datgroup;                /* number of range bins*/
        g->re       = asf_facdr1.eradcntr*1000.0;      /* Radius of the Earth */
        g->ht       = asf_facdr1.scalt*1000.0;         /* Spacecraft Altitude */
        g->r00      = asf_facdr1.sltrngfp*1000.0;      /* Slant Range 1st Pix */
        g->prf      = asf_facdr1.prfreq;               /* Pulse Rep. Frequency*/
        g->fs       = dssr1.rng_samp_rate * 1000000.0; /* Range sampling rate */
        g->pulsedur = dssr1.rng_length / 1000000.0;    /* Beam pulse length   */
        g->wavl     = dssr1.wave_length;               /* Beam wavength       */

/*Finally, add the computed parameters:*/
	
	/*Compute the spacecraft height from the state vector.*/
	g->ht=vecMagnitude(meta->state_vectors->vecs[0].vec.pos)-g->re;

	/*Magnitude of s/c orbital velocity (m/sec)*/
	g->vel=vecMagnitude(meta->state_vectors->vecs[0].vec.vel);

	g->nla-=g->pulsedur*g->fs;/*Subtract the (wasted) pulse length samples*/
	
	*meta_out=meta;
	
	return;
}
