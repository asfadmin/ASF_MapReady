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
*
****************************************************************************/									   
#include "deskew.h"

#define VERSION 1.5
#define NUM_ARGS 3

double grPixelSize;
int gr_ns,sr_ns;

double *slantGR;/*Slant range pixel #*/
double *heightShiftGR;
double *heightShiftSR;
double *groundSR;/*Ground range pixel #*/
double *slantPixel;
double *groundRange;
double *slantRangeSqr,*slantRange,*heightShift;
double *incidAng,*sinIncidAng,*cosIncidAng;



int main(int argc, char *argv[])
{
	float *grDEMline,*srDEMline,*outAmpLine;
	register int line,nl,percent;
	char inDEMfile[255],outDEMfile[255],outAmpFile[255];
	FILE *inDEM,*outDEM,*outAmp;
	meta_parameters *metaDEM, *metaOut;

	system("date");
	printf("Program: reskew_dem\n\n");
	
/* parse commandline arguments */
	logflag=FALSE;
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
	strcpy(inDEMfile, argv[currArg]);
	strcpy(outDEMfile,argv[currArg+1]);
	strcpy(outAmpFile,argv[currArg+2]);

/* Get metadata */
	metaDEM = meta_read(inDEMfile);
	metaOut = meta_copy(metaDEM);
	nl = metaDEM->general->line_count;
	gr_ns = metaDEM->general->sample_count;
	sr_ns = gr_ns-400;      /* The 400 pixels here has to match the extra
	                         * amount added in the demIFM script.*/
	metaOut->general->sample_count = sr_ns;
	
/*Allocate vectors.*/
	slantGR       = (double *)MALLOC(sizeof(double)*gr_ns);
	groundSR      = (double *)MALLOC(sizeof(double)*sr_ns);
	heightShiftSR = (double *)MALLOC(sizeof(double)*sr_ns);
	heightShiftGR = (double *)MALLOC(sizeof(double)*gr_ns);
	slantRange    = (double *)MALLOC(sizeof(double)*sr_ns);
	slantRangeSqr = (double *)MALLOC(sizeof(double)*sr_ns);
	incidAng      = (double *)MALLOC(sizeof(double)*sr_ns);
	sinIncidAng   = (double *)MALLOC(sizeof(double)*sr_ns);
	cosIncidAng   = (double *)MALLOC(sizeof(double)*sr_ns);

	grPixelSize = calc_ranges(metaOut);

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
		get_float_line(inDEM,metaDEM,line,grDEMline);
		dem_gr2sr(grDEMline,srDEMline,outAmpLine);
		put_float_line(outDEM,metaOut,line,srDEMline);
		put_float_line(outAmp,metaOut,line,outAmpLine);
	}
	printf("\r   Completed 100 percent\n\n");

	sprintf(logbuf,"   Converted %d lines from ground range to slant range.\n\n", nl);
	printf("%s", logbuf);
	if (logflag) { printLog(logbuf); }

/* Write meta files */
	meta_write(metaOut, outDEMfile);
	meta_write(metaOut, outAmpFile);

/* Free memory, close files, & exit */
	meta_free(metaDEM);
	meta_free(metaOut);
	FREE(slantGR);
	FREE(groundSR);
	FREE(heightShiftSR);
	FREE(heightShiftGR);
	FREE(slantRange);
	FREE(slantRangeSqr);
	FREE(incidAng);
	FREE(sinIncidAng);
	FREE(cosIncidAng);
	FREE(grDEMline);
	FREE(srDEMline);
	FREE(outAmpLine);
	FCLOSE(inDEM);
	FCLOSE(outDEM);
	FCLOSE(outAmp);

	exit(EXIT_SUCCESS);
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] <inGR_DEM> <outSR_DEM> <outSR_simAmp>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
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

#if 1
/*Use linearized arrays to do conversion.*/
float sr2gr(float srX,float height)
{
	double dx,srXSeaLevel=srX-height*heightShiftSR[(int)srX];
	int ix;
	if (srXSeaLevel<0) srXSeaLevel=0;
	if (srXSeaLevel>=sr_ns-1) srXSeaLevel=sr_ns-1;
	ix=(int)srXSeaLevel;
	dx=srXSeaLevel-ix;
/*Linear interpolation on groundSR array*/
	return groundSR[ix]+dx*(groundSR[ix+1]-groundSR[ix]);
}
float gr2sr(float grX,float height)
{
	double dx,grXSeaLevel=grX-height*heightShiftGR[(int)grX];
	int ix;
	if (grXSeaLevel<0) grXSeaLevel=0;
	if (grXSeaLevel>=gr_ns-1) grXSeaLevel=gr_ns-1;
	ix=(int)grXSeaLevel;
	dx=grXSeaLevel-ix;
/*Linear interpolation on slantGR array*/
	return slantGR[ix]+dx*(slantGR[ix+1]-slantGR[ix]);
}
#else
/*Use fundamental equations to do conversion.*/
float sr2gr(float srX,float height)
{
	double slant=slantFirst+srX*slantPer;
	double phi=acos((satHt*satHt+(er+height)*(er+height)-slant*slant)/
		(2.0*satHt*(er+height)));
	return phi2grX(phi);
}
float gr2sr(float grX,float height)
{
	double phi=grX2phi(x);
	double slant=sqrt(satHt*satHt+(er+height)*(er+height)-cos(phi)*
		(2.0*satHt*(er+height)));
	return (slant-slantFirst)/slantPer;
}
#endif
