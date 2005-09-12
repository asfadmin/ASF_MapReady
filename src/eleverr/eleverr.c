/****************************************************************
NAME:  eleverr
	
SYNOPSIS:  

eleverr [-i <init_err>] [-log <file>] [-mask <file>] 
		<coherence> <base> <outfile>

DESCRIPTION:

    Generate a slant range DEM error map from a coherence image and
    an unwrapped phase file. Requires metadata for the reference 
    image (image 1) of the interferogram pair.
   
EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:               PURPOSE:
    ---------------------------------------------------------------
    0.1    10/96                Original Creation
    1.0     6/97                Read Image Size from DDR.
            7/97   D. Corbett   updated version number
    2.0    12/98   O. Lawlor    New metadata routines.
    2.1     7/01   R. Gens      Added log file switch, made mask optional
    2.11   10/01   S. Watts     Added additional check for command line args
    2.5    12/03   P. Denny     Update command line parsing & usage()
                                  Update to meta1.1 from meta/ddr combo
    2.6    2/04    P. Denny     Added check for really close to 0 in a
                                  denominator (avoids inf)

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/******************************************************************************
*                                                                             *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                  www.asf.alaska.edu             *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/
#include "asf.h"
#include "asf_meta.h"

#define NUM_ARGS        3
#define DEFAULT_ERROR   0.0
#define VERSION         2.5

#define FLOAT_EQUALS_ZERO(X) (X<0.000000000001 && X>-0.000000000001)

/* local function declaration */
void usage(char *name);

int main(int argc, char **argv)
{
	int x, y;
	int maskflag=0;
	unsigned char *mask;
	double k;
	double *phase2elevBase,*sinFlat,*cosFlat;
	char datafile[256], basefile[256], outfile[256];
	char maskfile[256];
	int nrows,ncols; 
	float *f_coh;
	float *f_eleverr;
	float percent=0.0;
	double init_err=DEFAULT_ERROR;
	FILE *fdata, *fmask, *fout;
	meta_parameters *meta;
	baseline base;


/* Parse command line arguments */
	logflag=FALSE;
	while (currArg < (argc-NUM_ARGS)) {
	   char *key = argv[currArg++];
	   if (strmatch(key,"-log")) {
	      CHECK_ARG(1);
	      strcpy(logFile,GET_ARG(1));
	      fLog = FOPEN(logFile, "a");
	      logflag=TRUE;
	   }
	   else if (strmatch(key, "-mask")) {
	      CHECK_ARG(1);
	      strcpy(maskfile, GET_ARG(1));
	      maskflag = TRUE;
	   }
	   else if (strmatch(key,"-i")) {
	      CHECK_ARG(1);
	      init_err = atof(GET_ARG(1));
	      init_err *= init_err;
	   }
	   else {
	      printf("\n**Invalid option:  %s\n",argv[currArg-1]);
	      usage(argv[0]);
	   }
	}
	if ((argc-currArg) < NUM_ARGS) {
	   printf("Insufficient arguments.\n");
	   usage(argv[0]);
	}

	create_name(datafile, argv[currArg], ".img");
	strcpy(basefile, argv[currArg+1]);
	strcpy(outfile, argv[currArg+2]);

/* Start the program body! */
        printf("%s\n",date_time_stamp());
	printf("Program: eleverr\n\n");
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: eleverr\n\n");
	}

/* Get appropriate metadata */
	meta = meta_read(datafile);
	nrows = meta->general->line_count;
	ncols = meta->general->sample_count;
	meta->general->data_type = REAL32;
	meta_write(meta, outfile);
	k  = meta_get_k(meta);    /* wave number*/
	
/* Allocate space for vectors, matricies, and stuff*/
	mask = (unsigned char *)MALLOC(sizeof(unsigned char)*ncols);
	f_coh = (float *)MALLOC(sizeof(float)*ncols);
	f_eleverr = (float *)MALLOC(sizeof(float)*ncols);
	sinFlat = (double *)MALLOC(sizeof(double)*ncols);
	cosFlat = (double *)MALLOC(sizeof(double)*ncols);
	phase2elevBase = (double *)MALLOC(sizeof(double)*ncols);

/* Open data file & get seed phase*/
	fdata = fopenImage(datafile, "rb");
	fout = fopenImage(outfile,"wb");
	if (maskflag) fmask = fopenImage(maskfile,"rb");
	
/* Read in baseline values*/
	base = read_baseline(basefile);

/* Obtain information from metadata*/
	for (x=0;x<ncols;x++)
	{
		int img_x = x * meta->sar->sample_increment
		            + meta->general->start_sample;
		double incid=meta_incid(meta,0,img_x);
		double flat=meta_flat(meta,0,img_x);
		sinFlat[x]=sin(flat);
		cosFlat[x]=cos(flat);
		phase2elevBase[x]=meta_get_slant(meta,0,img_x)*sin(incid)/(2.0*k);
	}

/* Loop through each row & calculate height*/
	for (y=0;y<nrows;y++) {
		double Bn_y,Bp_y;

		/* Report progress */
		if ((y*100/nrows)>percent) {
		  printf("\r   Completed %3.0f percent", percent);
		  fflush(NULL);
		  percent+=5.0;
		}

		/* read in data */
		if (maskflag)
			FREAD(mask,sizeof(unsigned char),ncols,fmask);
		get_float_line(fdata, meta, y, f_coh);
		
		/* calculate baseline for this row*/
		meta_interp_baseline(meta, base,
			y*meta->sar->line_increment+meta->general->start_line+1,
			&Bn_y, &Bp_y);
		
		/* step through each pixel in row*/
		for (x=0;x<ncols;x++) {
			if ((mask[x] == 0x10 && maskflag) || (!maskflag)) {
				double tmp,tmp1,sigma_height;
				tmp = phase2elevBase[x]/(-Bp_y*sinFlat[x]-Bn_y*cosFlat[x]);
				tmp1 = (FLOAT_EQUALS_ZERO(f_coh[x])) 
					    ? 0.0 : sqrt((1-f_coh[x])/f_coh[x]);
				sigma_height = tmp*tmp1;
				f_eleverr[x] = (float)sqrt( init_err +
					sigma_height*sigma_height );
			}
			else
				f_eleverr[x] = -1.0;
		}
		put_float_line(fout, meta, y, f_eleverr);
	}
	printf("\r   Completed 100 percent\n\n");
	sprintf(logbuf, "   Wrote %lld bytes of data\n\n",
	        (long long)(nrows*ncols*4));
	printf("%s", logbuf);
	if (logflag) { printLog(logbuf); }
	
	/* free memory & scram*/
	meta_free(meta);
	FREE(mask);
	FREE(f_coh);
	FREE(f_eleverr);
	FREE(sinFlat);
	FREE(cosFlat);
	FREE(phase2elevBase);
	FCLOSE(fdata);
	FCLOSE(fout);
	if (maskflag) FCLOSE(fmask);

	exit(EXIT_SUCCESS);
}

void usage(char *name)
{ 
 printf("\n"
	"USAGE:\n"
	"   %s [-i <init_err>] [-log <file>] [-mask <file>]\n"
	"            <coherence> <base> <outfile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   coherence   Coherence file (from coh(1)).\n"
	"   base        File containing baseline parameterss.\n"
	"                  Format:  Bn_c   dBn   Bp_c	dBp\n"
	"   outfile     Output file containing one sigma errors.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -i      Error <init_err> associated with seed elevation in meters.\n"
	"             Default is %.1f meters.\n"
	"   -log    Allows the output to be written to a log <file>.\n"
	"   -mask   Use the mask <file> if you used escher for phase unwrapping.\n",
	DEFAULT_ERROR);
 printf("\n"
	"DESCRIPTION:\n"
	"   Generates DEM errors from coherence file\n");
 printf("\n"
	"Version: %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
