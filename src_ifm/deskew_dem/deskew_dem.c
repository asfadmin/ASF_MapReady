/****************************************************************
NAME:  deskew_dem

USAGE:  deskew_dem [-i inSARfile bit] [-log <file>]]
			<inDEMfile> <outfile>

SYNOPSIS:

    deskew_dem removes incidence angle skew from a slant-range
    DEM, and interpolates across areas that didn't phase unwrap.

    If <outfile> has the extension .dem or .ht, deskew_dem will
    remove the incidence angle skew from the input slant-range DEM.

    If the <outfile> has the extention of .img, or .amp, deskew_dem
    will output a terrain-corrected amplitude image, based on the input file
    <inDEMfile>.

    If the -g option is passed, the terrain correction is only
    geometric-- no radiometric incidence angle normalization will
    occur.

    The -log switch allows the output to be written to a log file.

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:     PURPOSE:
    ---------------------------------------------------------------
    1.0	     7/97  O. Lawlor   Deskew Interferometry DEMs.
    1.1	     6/98  O. Lawlor   Made more consistent with reskew_dem.
    1.2	     7/01  R. Gens     Added log file switch.
    1.35     4/02  P. Denny    Updated commandline parsing & usage()
    2.0      2/04  P. Denny    Removed use of DDR; upgraded to meta v1.1
                                Removed <ceos> command line argument
                                Fix sr2gr & gr2sr functions from leaving memory
                                Use newer io functions (eg: get_float_line)

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   deskew_dem -- this program removes incidence-angle skew and maps from   *
*		  slant range to ground range.				    *
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

#include "deskew_dem.h"

void usage(char *name);


int numLines, numSamples;
double grPixelSize;

double *slantGR;/*Slant range pixel #*/
double *heightShiftGR;
double *heightShiftSR;
double *groundSR;/*Ground range pixel #*/
double *slantPixel;
double *groundRange;
double *slantRangeSqr,*slantRange,*heightShift;
double *incidAng,*sinIncidAng,*cosIncidAng;

#define VERSION 2.0
#define REQ_ARGS 2

int main(int argc, char *argv[])
{
	float *srDEMline,*grDEM,*grDEMline,*grDEMlast,*inSarLine,*outLine;
	char inDemName[255],inSarName[255],outName[255];
	char *ext=NULL;
	FILE *inDemFp,*inSarFp,*outFp;
	meta_parameters *inDemMeta, *outMeta, *inSarMeta;
	char msg[256];
	int inSarFlag=FALSE;
	int dem_is_ground_range=FALSE;
	int doRadiometric=0;
	register int x,y;
	extern int currArg; /* in cla.h in asf.h in deskew_dem.h */

	logflag=FALSE;

/* parse commandline arguments */
	while (currArg < (argc-REQ_ARGS)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-i")) {
			CHECK_ARG(2);
			strcpy(inSarName,GET_ARG(2));
			ext=findExt(inSarName);
			if ((0!=strcmp(ext,".amp")) && (0!=strcmp(ext,".img")))
			  {printf("**ERROR: <inSARfile> must have a \".amp\" or \".img\" extention.\n");usage(argv[0]);}
			doRadiometric = atoi(GET_ARG(1));
			if ((doRadiometric != 0) && (doRadiometric != 1))
			  {printf("**ERROR:  <bit> must be either 0 or 1\n"); usage(argv[0]);}
			inSarFlag = TRUE;
		}
		else if (strmatch(key,"-log")) {
			CHECK_ARG(1);
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			logflag = TRUE;
		}
		else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < REQ_ARGS) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	strcpy(inDemName,argv[currArg]);
	strcpy(outName,  argv[currArg+1]);

	ext=findExt(inDemName);
	if (0==strcmp(ext,".dem"))
	   dem_is_ground_range=TRUE;

	printf("Program: deskew_dem\n\n");
	if (logflag) {
	   printLog("Program: deskew_dem\n\n");
	}

/*Extract metadata*/
	inDemMeta = meta_read(inDemName);
	outMeta = meta_copy(inDemMeta);

	if (inDemMeta->sar->image_type=='P') {
		printf("DEM cannot be map projected for this program to work!\n");
		exit(EXIT_FAILURE);
	}
	if (inSarFlag) {
	   inSarMeta = meta_read(inSarName);
	   if (inSarMeta->sar->image_type=='P') {
	      printf("SAR image cannot be map projected for this program to work!\n");
	      exit(EXIT_FAILURE);
	   }
	   outMeta->general->data_type = inSarMeta->general->data_type;

	   if ((inSarMeta->general->line_count != inDemMeta->general->line_count) &&
	       (inSarMeta->general->sample_count != inDemMeta->general->sample_count))
	   {
	      printErr("ERROR: The DEM and the SAR image must be the same size.\n");
	   }
	}
	numLines = inDemMeta->general->line_count;
	numSamples = inDemMeta->general->sample_count;
  	printf("   Images are %i lines by %i samples.\n",numLines,numSamples);

/*Allocate vectors.*/
	slantGR       = (double*)MALLOC(sizeof(double)*numSamples);
	groundSR      = (double*)MALLOC(sizeof(double)*numSamples);
	heightShiftSR = (double*)MALLOC(sizeof(double)*numSamples);
	heightShiftGR = (double*)MALLOC(sizeof(double)*numSamples);
	slantRange    = (double*)MALLOC(sizeof(double)*numSamples);
	slantRangeSqr = (double*)MALLOC(sizeof(double)*numSamples);
	incidAng      = (double*)MALLOC(sizeof(double)*numSamples);
	sinIncidAng   = (double*)MALLOC(sizeof(double)*numSamples);
	cosIncidAng   = (double*)MALLOC(sizeof(double)*numSamples);

/*Set up the output meta file.*/
	grPixelSize = calc_ranges(inDemMeta);
	outMeta->sar->image_type='G';
	outMeta->general->x_pixel_size = grPixelSize;
	meta_write(outMeta, outName);

/*Open files.*/
	inDemFp = fopenImage(inDemName,"rb");
	outFp   = fopenImage(outName,"wb");
	if (inSarFlag) inSarFp = fopenImage(inSarName,"rb");

/* Blather at user about what is going on */
	strcpy(msg,"");
	if (dem_is_ground_range)
	  sprintf(msg,"%s   DEM is in ground range.\n",msg);
	else
	  sprintf(msg,"%s   DEM in slant range, but will be corrected.\n",msg);

	if (inSarFlag)
	  sprintf(msg,"%s   Correcting image",msg);
	else
	  sprintf(msg,"%s   Correcting DEM",msg);

	if (doRadiometric)
	  sprintf(msg,"%s geometrically and radiometrically.\n",msg);
	else
	  sprintf(msg,"%s geometrically.\n",msg);

	printf(msg);
	if (logflag) printLog(msg);

/*Allocate input buffers.*/
	if (inSarFlag) 
	   inSarLine = (float *)MALLOC(sizeof(float)*numSamples);
	outLine   = (float *)MALLOC(sizeof(float)*numSamples);
	srDEMline = (float *)MALLOC(sizeof(float)*numSamples);

/*Map DEM to ground range if necessary.*/
	/*It's much simpler if the DEM is already in ground range.*/
	if (dem_is_ground_range)
		grDEM=(float *)MALLOC(sizeof(float)*numSamples);
	/*If the dem is slant range, then we need to map it to ground range,
	 *all at once-- we have to read it ALL in to interpolate the columns.*/
	else {
		grDEM=(float *)MALLOC(sizeof(float)*numSamples*numLines);
		for (y=0;y<numLines;y++)
		{
			get_float_line(inDemFp,inDemMeta,y,srDEMline);
			dem_sr2gr(srDEMline,&grDEM[y*numSamples],numSamples);
		}
		/*Close gaps in y direction.*/
		for (x=0;x<numSamples;x++)
			dem_interp_col(&grDEM[x],numSamples,numLines);
	}

/*Rectify data.*/
	for (y=0;y<numLines;y++) {
		if (inSarFlag) {
			/*Read in DEM line-by-line (keeping two lines buffered)*/
			if (dem_is_ground_range) {
				float *tmp=srDEMline;
				srDEMline=grDEM;
				grDEM=tmp;
				get_float_line(inDemFp,inDemMeta,y,grDEM);
				grDEMline=grDEM;
				grDEMlast=srDEMline;
			}
			/*Fetch the appropriate lines from the big buffer.*/
			else {
				grDEMline=&grDEM[y*numSamples];
				grDEMlast=&grDEM[(y-1)*numSamples];
			}
			get_float_line(inSarFp,inSarMeta,y,inSarLine);
			geo_compensate(grDEMline,inSarLine,outLine,numSamples);
			if (y>0&&doRadiometric)
				radio_compensate(grDEMline,grDEMlast,outLine,
				                 numSamples);
			put_float_line(outFp,outMeta,y,outLine);
		}
		else
			put_float_line(outFp,outMeta,y,&grDEM[y*numSamples]);
	}

	sprintf(msg,"\n   Wrote %lld bytes of data\n\n",
	        (long long)(numLines*numSamples*4));
	printf(msg);
	if (logflag) {
	  printLog(msg);
	  FCLOSE(fLog);
	}

/* Clean up & skidattle */
	if (inSarFlag) {
	   FREE(inSarLine);
	   FCLOSE(inSarFp);
	   meta_free(inSarMeta);
	}
	FREE(srDEMline);
	FREE(outLine);
	FCLOSE(inDemFp);
	FCLOSE(outFp);
	meta_free(inDemMeta);
	meta_free(outMeta);
	FREE(slantGR);
	FREE(groundSR);
	FREE(heightShiftSR);
	FREE(heightShiftGR);
	FREE(slantRange);
	FREE(slantRangeSqr);
	FREE(incidAng);
	FREE(sinIncidAng);
	FREE(cosIncidAng);

	exit(EXIT_SUCCESS);
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-i <inSARfile> <bit>] [-log <file>]\n"
	"              <inDEMfile> <outfile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <inDEMfile>  IEEE float slant-range dem, with extension .ht or .dem.\n"
	"   <outfile>    Output image, with an extention.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -i	  The <inSARfile> is the image to be rectified (include the extention). If <bit>\n"
	"	     is 1 both radiometric and geometric rectification are performed; if <bit> is\n"
	"	     0 then only geometric rectification is performed.\n"
	"   -log  Allows the output to be written to a log <file>.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program removes incidence-angle skew and maps from slant range to\n"
	"   ground range. It will do this to a DEM or SAR image.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n", VERSION);
 exit(EXIT_FAILURE);
}


#if 1
/*
Use linearized arrays to do conversion:
Speedup: 40%.
*/
float sr2gr(float srX,float height)
{
	double dx,srXSeaLevel=srX-height*heightShiftSR[(int)srX];
	int ix;
    /*Prevent ix index (and ix+1) from leaving the bounds of allotted memory*/
	if (srXSeaLevel<0) srXSeaLevel=0;
	if (srXSeaLevel>=numSamples-1)  srXSeaLevel=numSamples-2;
	ix=(int)srXSeaLevel;
	dx=srXSeaLevel-ix;
    /*Linear interpolation on groundSR array*/
	return groundSR[ix]+dx*(groundSR[ix+1]-groundSR[ix]);
}
float gr2sr(float grX,float height)
{
	double dx,grXSeaLevel=grX-height*heightShiftGR[(int)grX];
	int ix;
    /*Prevent ix index (and ix+1) from leaving the bounds of allotted memory*/
	if (grXSeaLevel<0) grXSeaLevel=0;
	if (grXSeaLevel>=numSamples-1)  grXSeaLevel=numSamples-2;
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
	double phi=grX2phi(grX);
	double slant=sqrt(satHt*satHt+(er+height)*(er+height)-cos(phi)*
		(2.0*satHt*(er+height)));
	return (slant-slantFirst)/slantPer;
}
#endif
