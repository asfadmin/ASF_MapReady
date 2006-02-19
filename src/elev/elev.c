/****************************************************************
NAME:  elev

SYNOPSIS:  

  elev [-log <file>] [-quiet] <phase> <base> <outfile> <seed_file>

DESCRIPTION:

       Elev calculates the elevation at each integrated pixel  in
       an unwrapped phase file.

       The  user also needs to specify a seed point file, identi-
       cal to the format used by tandem_ifm.

       The output file is a float file of the same dimensions  as
       the  input file.  Each value will be a height in meters or
       0 if the pixel is non-integrated.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:  DATE:   PROGRAMMER:   PURPOSE:
    ---------------------------------------------------------------
    0.1    10/96                 Original Creation
    0.2     4/97   T. Logan      Properly handles windowed images
    1.0     6/97   O. Lawlor     Gets image size from ddr
    1.2    10/97   O. Lawlor     Eliminated unwrapping mask parameter
    1.3    11/97   O. Lawlor     Changed CLA's to accept list of seed points
    2.0     6/98   O. Lawlor     Re-derived equation; updated for new ceos
    2.1     6/00   D. Koster     Modified to handle files larger than 2GB
    2.2     7/01   R. Gens       Added log file switch
    2.5     3/03   P. Denny      Obliterate use of DDR, and replace with
                                   updated meta structure. Standardized
                                   command line parsing
    2.6     7/05   R. Gens       Took care of endianess.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   elev  -  produce  an  elevation file from a deramped igram phase file.  *
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

#define VERSION 2.6

static
void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] [-quiet] <phase> <base> <outfile> <seed_file>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"    phase:      Unwrapped phase file (.phase and .meta)\n"
	"    base:       File containing baseline params. used to unwrap\n"
	"                  format:  Bn_c   dBn   Bp_c   dBp \n"
	"    outfile:    Output file containing elevations.\n"
	"    seed_file:  Tandem_ifm style seed file.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -log:    Option to have output written to a log <file>.\n"
	"   -quiet:  Option to have output surpressed to essential.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Generate a DEM from unwrapped phase\n");
 printf("\n"
	"Version: %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
	int x, y;
	float percent=5.0;
	double xScale,yScale;
	int ss,sl;
	double k;
	double *phase2elevBase,*sinFlat,*cosFlat;
	baseline base;
	meta_parameters *meta;
	char *datafile, *basefile, *metafile, *outfile,*seedfile;
	FILE *fdata, *fout,*fseed;
	float *f_uwp,*f_elev;
	int nrows,ncols/*,arows,acols*/;
	double delta_phase,delta_height;
	double seed_phase,seed_height;
	extern int currArg; /* pre-initialized to 1; like optind */

	logflag=quietflag=0;

/* parse command line */
	while (currArg < (argc-4)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-log")) {
			CHECK_ARG(1)
			strcpy(logFile, GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			logflag=1;
		}
		else if (strmatch(key,"-quiet")) {
			quietflag=1;
		}
		else {
		  printf( "\n**Invalid option:  %s\n",argv[currArg-1]); 
		  usage(argv[0]);
		}
	}
	if ((argc-currArg) < 4) {
	  printf("Insufficient arguments.\n"); 
	  usage(argv[0]);
	}
	datafile = argv[currArg++];
	basefile = argv[currArg++];
	outfile  = argv[currArg++];
	seedfile = argv[currArg];

	printf("%s\n",date_time_stamp());
	printf("Program: elev\n\n");
	if (logflag) {
		StartWatchLog(fLog);
		printLog("Program: elev\n\n");
	}

/* Get input scene size and windowing info. Get datafile values*/
	meta = meta_read(datafile);
	nrows  = meta->general->line_count;
	ncols  = meta->general->sample_count;
	sl     = meta->general->start_line;
	ss     = meta->general->start_sample;
	yScale = meta->sar->line_increment;
	xScale = meta->sar->sample_increment;
	
	meta_write(meta, outfile);

/* Allocate space for vectors and matricies*/
	f_uwp  = (float *)MALLOC(sizeof(float)*ncols);
	f_elev = (float *)MALLOC(sizeof(float)*ncols);

/* Wavenumber K.*/
	k = meta_get_k(meta);

/* Read in baseline values*/
	base=read_baseline(basefile);

/* Open data file & get seed phase*/
	fdata = fopenImage(datafile, "rb");
	fseed = FOPEN(seedfile,"r");

/*Use least-squares fit to determine the optimal seed_phase and seed_height.*/
   {
	double x,xSum=0,xSqrSum=0,hSum=0,hxSum=0,pxSum=0,pxSqrSum=0;
	double a,b,c,d,e,f,det;
	int npts=0;
	float *phase_line;

	phase_line = (float *) MALLOC(sizeof(float)*meta->general->sample_count);

	while (1)
	{
		float seed_x,seed_y,height,phase;
		int seek_x,seek_y;
		/*Read in each seed point*/
		if (3!=fscanf(fseed,"%f%f%f",&seed_x,&seed_y,&height))
			break;/*Break out when no more points.*/
		seek_x=(int)((seed_x-ss)/xScale);
		seek_y=(int)((seed_y-sl)/yScale);
		get_float_line(fdata, meta, seek_y, phase_line);
		phase = phase_line[seek_y];
		if (phase==0)
			continue;/*Escher couldn't unwrap this tie point.*/

		/*Calculate that seed point's impact on fit.*/
		x         = meta_phase_rate(meta,base,seed_y,seed_x);
		xSum     += x;
		xSqrSum  += x * x;
		hSum     += height;
		hxSum    += height * x;
		pxSum    += phase * x;
		pxSqrSum += phase * x * x;
		npts++;
	}
	if (!quietflag)
		printf("   Read %d seed points\n",npts);
	/* The least-squares fit above leaves us with a matrix equation
	 *	[ a  b ]   [ seed_phase  ]   [ e ]
	 *	[      ] * [             ] = [   ]
	 *	[ c  d ]   [ seed_height ]   [ f ]
	 *
	 *	which has the solution
	 *
	 *	[ d  -b ]   [ e ]    1    [ seed_phase  ]
	 *	[       ] * [   ] * --- = [             ]
	 *	[ -c  a ]   [ f ]   det   [ seed_height ]
	 */
	a = -xSqrSum;
	b = xSum;
	c = -xSum;
	d = npts;
	e = hxSum-pxSqrSum;
	f = hSum-pxSum;
	det = a*d-b*c;
	seed_phase  = (e*d-f*b)/det;
	seed_height = (e*(-c)+f*a)/det;
   }

	if (!quietflag) 
	  printf("   Seed Phase: %f\n   Elevation: %f\n",seed_phase,seed_height);

	/* calculate the sine of the incidence angle across cols*/
	sinFlat = (double *)MALLOC(sizeof(double)*ncols);
	cosFlat = (double *)MALLOC(sizeof(double)*ncols);
	phase2elevBase = (double *)MALLOC(sizeof(double)*ncols);
	for (x=0;x<ncols;x++)
	{
		int img_x=x*xScale+ss;
		double incid=meta_incid(meta,0,img_x);
		double flat=meta_flat(meta,0,img_x);
		sinFlat[x]=sin(flat);
		cosFlat[x]=cos(flat);
		phase2elevBase[x]=meta_get_slant(meta,0,img_x)*sin(incid)/(2.0*k);
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
		get_float_line(fdata, meta, y, f_uwp);
		
		/* calculate baseline for this row*/
		meta_interp_baseline(meta,base,y*yScale+sl,&Bn_y,&Bp_y);

		/* step through each pixel in row*/
		for (x=0;x<ncols;x++) {
			if (f_uwp[x]!=0.0) {
				delta_phase = (double)f_uwp[x] - seed_phase;
				delta_height=delta_phase * phase2elevBase[x]/
				  (-Bp_y*sinFlat[x]-Bn_y*cosFlat[x]);
				f_elev[x] = delta_height+seed_height;
			}
			else 
				f_elev[x] = 0.0;
		}

		put_float_line(fout, meta, y, f_elev);
		if ((y*100/nrows)>percent) {
		  printf("   Completed %3.0f percent\n", percent);
		  percent+=5.0;
		}
	}
	printf("   Completed 100 percent\n\n   Wrote %lld bytes of data\n\n", 
	       (long long)(nrows*ncols*4));
	if (logflag) {
	  sprintf(logbuf, "   Wrote %lld bytes of data\n\n", 
		  (long long)(nrows*ncols*4));
	  printLog(logbuf);
	}

	/* free memory, close files, & scram*/
	meta_free(meta);
	FREE(f_uwp);
	FREE(f_elev);
	FREE(sinFlat);
	FREE(cosFlat);
	FREE(phase2elevBase);
	FCLOSE(fout);
	FCLOSE(fdata);
	FCLOSE(fseed);
	return(0);
}

