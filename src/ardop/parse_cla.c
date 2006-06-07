/****************************************************************************
*								            *
*   parse_cla.c:  Routines used to parse ARDOP command line parameters       *
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
  parse_cla.c  -- Routines to parse ARDOP's input parameters.
  
     FUNCTIONS INCLUDED IN THIS FILE:
	parse_cla	- parses the cla's, fills parameters in ardop_global.h
	get_params	- reads from metadata & calculates parameters
	calc_range_ref  - creates range reference function

  2/99    O. Lawlor   Initial separation from ardop_setup for quicklook.
  4/02    P. Denny    Rewrote commandline parsing.  A pity because
                         it was already beautiful.  That is the price
			 of conformity I guess.
  10/02	  J. Nicoll	Updated deskew options to remove wedges from data.
***********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "ardop_defs.h"
#include "geolocate.h"

/* this define is a hack to avoid calling usage() as done in cla.h */	
#define CHK_ARG_ASP(num_args) if (currArg+num_args>argc) \
  {printf("   *****You need %i arguments for the keyword %s.\n\n",currArg-argc+num_args,argv[currArg-1]);return 0;} \
  else currArg+=num_args;

/******************************************
Parse_cla:
	Parses ARDOP command-line options to
determine SAR processing parameters.  Returns
0 on user command-line error, 1 on success, 2 
on debug help, or not at all on other errors.  
Meta_out will contain a pointer to the metadata 
for this scene.

******************************************/


int parse_cla(int argc,char *argv[], struct ARDOP_PARAMS *g)
{
	int read_offset = 0,   /* Flag - Read resampling offsets from file?   */
	    read_dopplr = 0;   /* Flag - Read doppler constant from file?     */
	int cal_check=0;       /* checks if output file needs input cal file  */
	int debug_help=-1;     /* If -debug 0 used, give debug usage          */
	char fName_slope[256], /* Input slope,intercept (offsets) file        */
	   fName_doppler[256]; /* Input doppler constant file                 */
	FILE  *fp;
	
	/* Preset Optional Command Line Parameters
	   ---------------------------------------------------------*/
        fill_default_ardop_params(g);

	/* Process the Command Line Arguments
	   ------------------------------------*/
	if (argc<3)
		{printf("Must give input and output filenames.\n");return 0;}
	strcpy(g->in1,argv[argc-2]);
	strcpy(g->out,argv[argc-1]);

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

	return 1;
}
