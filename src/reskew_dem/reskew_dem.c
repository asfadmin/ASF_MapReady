/****************************************************************
NAME:  reskew_dem

SYNOPSIS:  reskew_dem [-log <file>] <inGR_DEMfile> <outSR_DEM> <outSR_sim_amp>

DESCRIPTION:
	Reskew_dem maps an input, ground range DEM into slant range, and
	creates a simulated SAR image.  The input DEM must already be lined up
	with the image, but need not be precisely co-registered. In fact, the
	amplitude image is generated only so the images can be co-registered.

	This program is called by the dem2seeds script.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0      8/97        O. Lawlor   Reskew USGS DEMs for Interferometry/
    1.1      6/97        O. Lawlor   Made more consistent with deskew_dem.
    1.3     12/98        O. Lawlor   Allow ground and slant ranges to differ in
                                      length.
    1.31     7/01        R. Gens     Added logfile switch
    1.5     12/03        P. Denny    Update commandline parsing. Use meta 1.1
                                      instead of DDRs. This program is loaded
                                      with unnecessary globals. Yuk! Needs to
                                      be fixed sometime.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   reskew_dem -- this program remaps the input DEM to slant range,         *
*		  and creates a simulated slant-range amplitude image 	    *
*		  from it.						    *
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
#include "deskew.h"

#define VERSION 1.6
#define NUM_ARGS 4
//#define FALSE 1
//#define TRUE 0
double earth_radius; /* current earth radius, meters (FIXME: update with azimuth, range) */
double satHt; /* satellite height from center of earth, meters */
double slant_to_first, slant_per; /* slant ranges, meters (FIXME: split out DEM and SAR slant ranges) */
int gr_ns,sr_ns;


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] <inMeta> <inGR_DEM> <outSR_DEM> <outSR_simAmp>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   inMeta        Metadata slant range SAR image.\n"
	"   inGR_DEM      A lined-up ground-range DEM.\n"
	"   outSR_DEM     Output slant-range DEM.\n"
	"   outSR_simAmp  Output simulated amplitude image.\n");
 printf("\n"
	"OPTIONAL ARGUMENT:\n"
	"   -log   Allows output to be written to a log <file>.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program remaps the input DEM to slant range, and creates a simulated\n"
	"   slant-range amplitude image from it. This is useful for interferometry.\n");
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n", VERSION);
 exit(EXIT_FAILURE);
}


int main(int argc, char *argv[])
{
	float *grDEMline,*srDEMline,*outAmpLine;
	register int line,nl,percent;
	char inDEMfile[255],outDEMfile[255],outAmpFile[255],inMetafile[255];
	FILE *inDEM,*outDEM,*outAmp;
	meta_parameters *metaIn, *metaDEM;
//	struct DDR ddrDEM,outddr,inddr;

	system("date");
	printf("Program: reskew_dem\n\n");
	
/* parse commandline arguments */
	logflag=FALSE;
/* FIXME: WTF? */
currArg=1;
	while (currArg < (argc-NUM_ARGS)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-log")) {
			CHECK_ARG(1);
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			logflag = TRUE;
		}
		else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < NUM_ARGS) {
		printf("Insufficient arguments.\n");
		usage(argv[0]);
	}
	strcpy(inMetafile, argv[currArg]);
	strcpy(inDEMfile, argv[currArg+1]);
	strcpy(outDEMfile,argv[currArg+2]);
	strcpy(outAmpFile,argv[currArg+3]);

/*Extract DDRs*/
	//c_getddr(inDEMfile,&ddrDEM);
	//outddr=ddrDEM;
	//nl=ddrDEM.nl;
	//gr_ns=ddrDEM.ns;
	//sr_ns=gr_ns; 
	/*sr_ns=gr_ns-400;*/  /* The 400 pixels here has to match the 
	  extra amount added in the demIFM script.*/
	//outddr.ns=sr_ns;
	//c_putddr(outDEMfile,&outddr);
	//c_putddr(outAmpFile,&outddr);

/* Get metadata */
	metaIn = meta_read(inMetafile);
	metaDEM = meta_read(inDEMfile);
	nl = metaDEM->general->line_count;
	gr_ns = metaDEM->general->sample_count;
	sr_ns = metaIn->general->sample_count;
/* The 400 pixels here has to match theextra
	                         * amount added in the demIFM script.*/
	//metaOut->general->sample_count = sr_ns;
	
	earth_radius = meta_get_earth_radius(metaIn, nl/2, 0);
	satHt = meta_get_sat_height(metaIn, nl/2, 0);
	meta_get_slants(metaIn, &slant_to_first, &slant_per);

	

/*Open files.*/
	inDEM  = fopenImage(inDEMfile,"rb");
	outDEM = fopenImage(outDEMfile,"wb");
	outAmp = fopenImage(outAmpFile,"wb");

/*Allocate more memory (this time for data lines*/
	grDEMline  = (float *)MALLOC(sizeof(float)*gr_ns);
	srDEMline  = (float *)MALLOC(sizeof(float)*sr_ns);
	outAmpLine = (float *)MALLOC(sizeof(float)*sr_ns);
	
/* Read deskewed data, write out reskewed data */
	percent = 0;
	for (line=0; line<nl; line++)
	{
		if ((line*100/nl)==percent) {
		  printf("\r   Completed %3d percent",percent);
		  percent+=5;
		}
#if 0
		getFloatLine(inDEM,&ddrDEM,line,grDEMline);
		dem_gr2sr(grDEMline,srDEMline,outAmpLine);
		putFloatLine(outDEM,&outddr,line,srDEMline);
		putFloatLine(outAmp,&outddr,line,outAmpLine);
#else
		get_float_line(inDEM,metaDEM,line,grDEMline);
		dem_gr2sr(grDEMline,srDEMline,outAmpLine);
		put_float_line(outDEM,metaIn,line,srDEMline);
		put_float_line(outAmp,metaIn,line,outAmpLine);
#endif
	}
	printf("\r   Completed 100 percent\n\n");

	sprintf(logbuf,"   Converted %d lines from ground range to slant range.\n\n", nl);
	printf("%s", logbuf);
	// if (logflag) { printLog(logbuf); }

/* Write meta files */
	meta_write(metaIn, outDEMfile);
	meta_write(metaIn, outAmpFile);

/* Free memory, close files, & exit */
	meta_free(metaDEM);
	meta_free(metaIn);
	FREE(grDEMline);
	FREE(srDEMline);
	FREE(outAmpLine);
	FCLOSE(inDEM);
	FCLOSE(outDEM);
	FCLOSE(outAmp);

	exit(EXIT_SUCCESS);
}



float srE2srH(float srEpix,float height)
{ /* {{{ */
	double er;
	double srE;
	double cosPhi;
	double srH;
	er = earth_radius;
	srE=slant_to_first+slant_per*srEpix;
	/* Calculate ground angle to this slant range */
	cosPhi=(satHt*satHt + er*er - srE*srE)/(2*satHt*er);
	/* Calculate slant range with new earth height */
	er+=height;
	srH=sqrt(satHt*satHt + er*er - 2 * satHt * er * cosPhi);
	return (srH-slant_to_first)/slant_per;
} /* }}} */


//#endif
