/****************************************************************
NAME:  eleverr
	
SYNOPSIS:  

eleverr [-i init_err] [-log <file>] [-mask <file>] 
		<coherence> <base> <meta> <outfile>

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
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    .1      10/96        Original Creation
    1.0     6/97         Read Image Size from DDR.
            7/97   dc    updated version number
    2.0     12/98  OL    New metadata routines.
    2.1	    7/01   RG	 Added log file switch, made mask optional
    2.11    10/01  SW	 Added additional check for command line args

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   eleverr: generate DEM errors from coherence file			    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
#include "asf.h"
#include "asf_meta.h"

#define DEFAULT_ERROR          0.0
#define VERSION                2.11

/* local function declaration */
void usage(char *name);

int main(int argc, char **argv)
{
	int x, y;
	int maskflag=0;
	unsigned char *mask;
	double k;
	double *phase2elevBase,*sinFlat,*cosFlat;
	char *datafile, *basefile, *outfile;
	char *maskfile;
	char *ceos;
	int nrows,ncols; 
	float *f_coh;
	float *f_eleverr;
	float percent=5.0;
	double init_err=DEFAULT_ERROR;
	FILE *fdata, *fmask, *fout/*, *fp*/;
	int i, optind=1;
	struct DDR ddr,newddr;
	meta_parameters *meta;
	baseline base;

	logflag=0;	

	if (argc==1 || argc>11) usage(argv[0]);

	/* handle options */
	for (i=1; i<argc; i++) {
	  if (strncmp(argv[i], "-i", 2)==0) {
	    init_err = atof(argv[i+1]);
	    init_err *= init_err;
	    i+=1;
	    optind+=2;
	  }
	  else if (strncmp(argv[i], "-log", 4)==0) {
	    sprintf(logFile, "%s", argv[i+1]);
	    fLog = FOPEN(logFile, "a");
	    logflag=1;
	    i+=1;
	    optind+=2;
	  }
	  else if (strncmp(argv[i], "-mask", 5)==0) {
	    maskfile = argv[i+1];
	    maskflag=1;
	    i+=1;
	    optind+=2;
	  }
	  else if (strncmp(argv[i], "-", 1)==0) {
	    sprintf(errbuf, "   ERROR: %s is not a valid option!\n", argv[i]);
	    printErr(errbuf);
	  }
	}

	/* handle required input */
	i=optind;

/* Check args again */
        if (argc!=(i+4))usage(argv[0]);

	datafile = appendExt(argv[i],".img");
        basefile = argv[i+1];
        ceos = argv[i+2];
        outfile = argv[i+3];

	StartWatch();
	system("date");
	printf("Program: eleverr\n\n");
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: eleverr\n\n");
	}
	
	c_getddr(datafile,&ddr);
	nrows=ddr.nl;
	ncols=ddr.ns;
	newddr=ddr;
	newddr.dtype=4;
	newddr.nbands=1;
	c_putddr(outfile,&newddr);
	
	/* set program variables*/
	meta=meta_init(ceos);
	k  = meta_get_k(meta);    /* wave number*/
	
	/* allocate space for vectors and matricies*/
	mask = (unsigned char *)MALLOC(sizeof(unsigned char)*ncols);
	f_coh = (float *)MALLOC(sizeof(float)*ncols);
	f_eleverr = (float *)MALLOC(sizeof(float)*ncols);
	
	/* open data file & get seed phase*/
/*	printf("Opening files...\n");*/
	fdata = fopenImage(datafile, "rb");
	if (maskflag) fmask = fopenImage(maskfile,"rb");
	fout = fopenImage(outfile,"wb");
	
	/* read in baseline values*/
	base=read_baseline(basefile);
	
	/* obtain information from ceos metadata*/
	sinFlat = (double *)MALLOC(sizeof(double)*ncols);
	cosFlat = (double *)MALLOC(sizeof(double)*ncols);
	phase2elevBase = (double *)MALLOC(sizeof(double)*ncols);
	
	for (x=0;x<ncols;x++)
	{
		int img_x=x*newddr.sample_inc+newddr.master_sample-1;
		double incid=meta_incid(meta,0,img_x);
		double flat=meta_flat(meta,0,img_x);
		sinFlat[x]=sin(flat);
		cosFlat[x]=cos(flat);
		phase2elevBase[x]=meta_get_slant(meta,0,img_x)*sin(incid)/(2.0*k);
	}
	
	/* loop through each row & calculate height*/
	for (y=0;y<nrows;y++) {
		double Bn_y,Bp_y;
		/* read in data */
		if (maskflag) FREAD(mask,sizeof(unsigned char),ncols,fmask);
		FREAD(f_coh,sizeof(float),ncols,fdata);
		
		/* calculate baseline for this row*/
		meta_interp_baseline(meta,base,
			y*newddr.line_inc+newddr.master_line,&Bn_y,&Bp_y);
		
		/* step through each pixel in row*/
		for (x=0;x<ncols;x++) {
			if ((mask[x] == 0x10 && maskflag) || (!maskflag)) {
				double tmp,tmp1,sigma_height;
				tmp = phase2elevBase[x]/(-Bp_y*sinFlat[x]-Bn_y*cosFlat[x]);
				tmp1 = sqrt( (1-f_coh[x])/f_coh[x] );
				sigma_height = tmp*tmp1;
				f_eleverr[x] = (float)sqrt( init_err +
					sigma_height*sigma_height );
			}
			else
				f_eleverr[x] = -1.0;
		}
		FWRITE(f_eleverr,sizeof(float),ncols,fout);
		if ((y*100/nrows)>percent) {
		  printf("   Completed %3.0f percent\n", percent);
		  percent+=5.0;
		}
	}
	printf("   Completed 100 percent\n\n   Wrote %lld bytes of data\n\n", (long long)(nrows*ncols*4));
	if (logflag) {
	  sprintf(logbuf, "   Wrote %lld bytes of data\n\n", (long long)(nrows*ncols*4));
	  printLog(logbuf);
	}
	
	/* free memory & scram*/
	FREE(f_coh);
	FREE(f_eleverr);
	FREE(mask);
	FCLOSE(fout);
	if (maskflag) FCLOSE(fmask);
	FCLOSE(fdata);

	StopWatch();
	if (logflag) {
	  StopWatchLog(fLog);
	}
	return(0);
}

void usage(char *name)
{ 
  printf("\n");
  printf("Usage: %s [-i init_err] [-log <file>] [-mask <file>]\n"
	 "\t\t<coherence> <base> <meta> <outfile>\n",name);
  printf("\n");
  printf("   -i init_err: error associated with seed elevation in meters."
	 "\n                Default is 0.0 meters.\n");
  printf("   -log <file>: allows the output to be written to a log file.\n");
  printf("  -mask <file>: use the mask if you used escher for phase unwrapping.\n");
  printf("   <coherence>: coherence file (from coh(1))\n");
  printf("        <base>: file containing baseline parameterss.\n");
  printf("                format:  Bn_c   dBn   Bp_c   dBp \n");
  printf("        <meta>: the name of the file that contains the\n");
  printf("                metadata for image 1 of the interferogram pair.\n");
  printf("     <outfile>: output file containing one sigma errors.\n");
  printf("\n");
  printf("eleverr: generate DEM errors from coherence file\n");
  printf("Version: %.2f, ASF SAR Tools\n\n",VERSION);
  exit(1);
}
