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

static int* intParm(int i)
{
    int *ret = MALLOC(sizeof(int));
    *ret = i;
    return ret;
}

static float *floatParm(float f)
{
    float *ret = MALLOC(sizeof(float));
    *ret = f;
    return ret;
}

int parse_cla(int argc,char *argv[], struct INPUT_ARDOP_PARAMS *g)
{
	int read_offset = 0,   /* Flag - Read resampling offsets from file?   */
	    read_dopplr = 0;   /* Flag - Read doppler constant from file?     */
	int cal_check=0;       /* checks if output file needs input cal file  */
	int debug_help=-1;     /* If -debug 0 used, give debug usage          */
	char fName_slope[256], /* Input slope,intercept (offsets) file        */
	   fName_doppler[256]; /* Input doppler constant file                 */
	FILE  *fp;

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
			g->iflag    = intParm(atoi(GET_ARG(1))); 
			if (*(g->iflag)==0) return debug_help; }
		else if (strmatch(key,"-quiet")) {quietflag = 1;}
		else if (strmatch(key,"-power")) {g->pwrFlag=intParm(1);}
		else if (strmatch(key,"-sigma")) {g->sigmaFlag=intParm(1); cal_check=1;}
		else if (strmatch(key,"-gamma")) {g->gammaFlag=intParm(1); cal_check=1;}
		else if (strmatch(key,"-beta" )) {g->betaFlag=intParm(1); cal_check=1;}
		else if (strmatch(key,"-hamming")) {g->hamFlag = intParm(1);}
		else if (strmatch(key,"-kaiser"))  {g->kaiFlag = intParm(1);}
		else if (strmatch(key,"-l")) {CHK_ARG_ASP(1); g->ifirstline = intParm(atoi(GET_ARG(1)));}
		else if (strmatch(key,"-p")) {CHK_ARG_ASP(1); g->npatches = intParm(atoi(GET_ARG(1)));}
		else if (strmatch(key,"-f")) {CHK_ARG_ASP(1); g->isave   += atoi(GET_ARG(1));}
		else if (strmatch(key,"-s")) {CHK_ARG_ASP(1); g->ifirst  += atoi(GET_ARG(1));}
		else if (strmatch(key,"-n")) {CHK_ARG_ASP(1); g->nla      = intParm(atoi(GET_ARG(1)));}
		else if (strmatch(key,"-r")) {CHK_ARG_ASP(1); g->azres    = floatParm(atof(GET_ARG(1)));}
		else if (strmatch(key,"-e")) {CHK_ARG_ASP(1); g->deskew   = intParm(atoi(GET_ARG(1)));
		 			      g->na_valid = intParm(-99);}
	/* If you use the -e flag, then a deskew wedge in the data will need to be removed. Override  
	internal measurement of the number of valid azimuth lines with the -v option flag */
		else if (strmatch(key,"-v")) {CHK_ARG_ASP(1); g->na_valid = intParm(atoi(GET_ARG(1)));}
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
		g->sigmaFlag=intParm(0);
		g->gammaFlag=intParm(0);
		g->betaFlag=intParm(0);
	}

	if (read_offset) {
                float sloper, interr, slopea, intera;
                float dsloper, dinterr, dslopea, dintera;
		fp=FOPEN(fName_slope,"r");
		fscanf(fp,"%f %f %f %f",&sloper,&interr,&slopea,&intera);
                g->sloper = floatParm(sloper);
                g->interr = floatParm(interr);
                g->slopea = floatParm(slopea);
                g->intera = floatParm(intera);
		fscanf(fp,"%f %f %f %f",&dsloper,&dinterr,&dslopea,&dintera);
                g->dsloper = floatParm(dsloper);
                g->dinterr = floatParm(dinterr);
                g->dslopea = floatParm(dslopea);
                g->dintera = floatParm(dintera);
		FCLOSE(fp);
	}
	if (read_dopplr) {
                float fd, fdd, fddd;
		fp=FOPEN(fName_doppler,"r");
		fscanf(fp,"%f %f %f", &fd,&fdd,&fddd);
                g->fd = floatParm(fd);
                g->fdd = floatParm(fdd);
                g->fddd = floatParm(fddd);
		FCLOSE(fp);
	}

	return 1;
}
