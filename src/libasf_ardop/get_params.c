/******************************************************************************
NAME:	get_params - Creates input file for the ASF SAR processor
SYNOPSIS: get_params(in)
DESCRIPTION:
   Fills all values in ardop_global.h.  The purpose is to automate creation
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
#include "ardop_defs.h"

void get_params(char *file,struct ARDOP_PARAMS *g,meta_parameters **meta_out)
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

void fill_default_ardop_params(struct ARDOP_PARAMS *g)
{
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
}
