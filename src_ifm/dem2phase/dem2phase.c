/****************************************************************
NAME:  dem2phase

SYNOPSIS:  dem2phase [-log <file>] <sr_dem> <metadata> <base> <phase>
  
DESCRIPTION:

	dem2phase Simulates the phase of an interferogram, given a
        slant-range height image.  The baseline and ceos files are
        used  to generate the coefficients of the equation used to
        calculate height from a phase signal.

        The output file is a float file of the same dimensions  as
        the  input file.  Each value will be a phase in radians or
        0 if the region has no DEM.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:    NAME:    PURPOSE:
    ---------------------------------------------------------------
    .1      10/96      ?        Original Creation
    .2       4/97   T. Logan    Properly handles windowed images
    1.0	     6/97   O. Lawlor   Gets image size from ddr
    2.0	     12/98  O. Lawlor   Now uses asf_meta.a and asf.a
    2.01     7/01   R. Gens     Added logfile switch
    2.25     4/02   P. Denny    Standardized commandline parsing & usage()


HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   dem2phase -- produces a simulated phase file from a slant-range	    *
*		 evelation file.					    *
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

#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"

#define VERSION 2.25

/* local function declaration */
void usage(char *name);

int main(int argc, char **argv)
{
	int x, y, percent=5;
	double /*slantFirst,slantPer,*/xScale,yScale;
	int ss,sl;

	double k;
	double *phase2elevBase,*sinFlat,*cosFlat;
	baseline base;
	meta_parameters *meta;
	char *datafile, *basefile, *outfile;
	FILE *fdata, *fout;
	float *f_uwp,*f_elev;
	char *ceos;
	int nrows,ncols;
	/*double delta_phase,delta_height;*/
	struct DDR ddr,newddr;

	/* handle command line parameters
	StartWatch();*/
	/* parse command line */
	currArg=1; /*from cla.h in asf.h*/
	while (currArg < (argc-3)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-log")) {
			CHECK_ARG(1);
			strcpy(logFile, GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			StartWatchLog(fLog);
			printLog("Program: dem2phase\n\n");
 		}
		else {printf("\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < 3) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	datafile = argv[currArg];
	basefile = argv[currArg+1];
	outfile  = argv[currArg+2];

	system("date");
	printf("Program: dem2phase\n\n");

	meta = meta_read(datafile);
	ss = meta->general->start_sample;
	sl = meta->general->start_line;
	xScale = meta->sar->sample_increment;
	yScale = meta->sar->line_increment;
	nrows = meta->general->line_count;
	ncols = meta->general->sample_count;

	meta_write(meta, outfile);

	/* allocate space for vectors and matricies*/
	f_uwp = (float *)MALLOC(sizeof(float)*ncols);
	f_elev =(float *)MALLOC(sizeof(float)*ncols);

	/* Read in values from CEOS 
	   meta=meta_read(ceos);*/
	k=meta_get_k(meta);/*Wavenumber K.*/

	/* read in baseline values*/
	base=read_baseline(basefile);

	/* open data file & get seed phase*/
	fdata = fopenImage(datafile, "rb");

	/* calculate the sine of the incidence angle across cols*/
	sinFlat = (double *)MALLOC(sizeof(double)*ncols);
	cosFlat = (double *)MALLOC(sizeof(double)*ncols);
	phase2elevBase = (double *)MALLOC(sizeof(double)*ncols);
	for (x=0;x<ncols;x++)
	{
		int img_x=x*xScale+ss;
		double incid=meta_incid(meta,0.0,(float)img_x);
		double flat=meta_flat(meta,0.0,(float)img_x);
		sinFlat[x]=sin(flat);
		cosFlat[x]=cos(flat);
		phase2elevBase[x]=meta_get_slant(meta,0.0,(float)img_x)*sin(incid)/(2.0*k);
	}

	/* open other files*/
	fout = fopenImage(outfile,"wb");

	/* loop through each row & calculate height*/
	/*Note:
		To make this faster, we don't call 
	delta_height=delta_phase * meta_phase_rate(ceos,base,y*yScale+sl,x*xScale+ss).
		Instead, we use the annoying temporary arrays
	allocated above to calculate the same thing, quicker.
	*/
	for (y=0;y<nrows;y++) {
		double Bn_y,Bp_y;

		/* read in data */
		get_float_line(fdata, meta, y, f_elev);
		
		/* calculate baseline for this row*/
		meta_interp_baseline(meta,base,y*(int)yScale+sl,&Bn_y,&Bp_y);

		/* step through each pixel in row*/
		for (x=0;x<ncols;x++) {
			f_uwp[x]=f_elev[x]/phase2elevBase[x]*(-Bp_y*sinFlat[x]-Bn_y*cosFlat[x]);
		}
		put_float_line(fout, meta, y, f_uwp);
		if ((y*100/nrows)==percent) {
		  printf("   Completed %3d percent\n", percent);
		  percent+=5;
		}

	}
	printf("   Completed 100 percent\n\n");
	printf("   Wrote %d lines of simulated phase data.\n\n",nrows);
	if (logflag) {
	  sprintf(logbuf,"   Wrote %d lines of simulated phase data.\n\n",nrows);
	  printLog(logbuf);
	}

	/* free memory & scram*/
	FREE(f_uwp);
	FREE(f_elev);
	FCLOSE(fout);
	FCLOSE(fdata);
	return(0);
}

void usage(char *name)
{ 
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] <sr_DEM> <meta> <base> <phase>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <sr_DEM>  A slant-range DEM, possibly the output of reskew_dem,\n"
	"               used to create the phase image.\n"
	"    <phase>  Output simulated phase file (with .ddr)\n"
	"     <base>  File containing baseline parameters used to unwrap format:\n"
	"               Bn_c   dBn   Bp_c   dBp \n"
	"     <meta>  The name of the file that contains the metadata for\n"
	"               image 1 of the interferogram pair.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"     -log:   Allows the output to be written to a log <file>. (optional)\n");
 printf("\n"
	"DESCRITION:\n"
	"   %s produces a simulated phase file from a\n" 
        "   slant-range evelation file.\n",name);
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(1);
}
