/****************************************************************
NAME:  deskew_dem

USAGE:  deskew_dem [-i inSARfile bit] [-log <file>]]
			<inDEMfile> <ceos> <outfile> 

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
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0	     7/97        O. Lawlor-- Deskew Interferometry DEMs.
    1.1	     6/98	 O. Lawlor-- Made more consistent with reskew_dem.
    1.2	     7/01	 R. Gens -- Added log file switch.
    1.35     4/02        P. Denny -- Updated commandline parsing & usage()
    
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
#include "ddr.h"

void usage(char *name);


double minPhi,maxPhi,phiMul,grPixelSize;
meta_parameters *meta;
int ns;

double *slantGR;/*Slant range pixel #*/
double *heightShiftGR;
double *heightShiftSR;
double *groundSR;/*Ground range pixel #*/
double *slantPixel;
double *groundRange;
double *slantRangeSqr,*slantRange,*heightShift;
double *incidAng,*sinIncidAng,*cosIncidAng;
struct DDR ddrDEM,outddr,inddr;

#define VERSION 1.35

int main(int argc, char *argv[])
{
	float *srDEMline,*grDEM,*grDEMline,*grDEMlast,*inLine,*outLine;
	register int x,y,nl;
	char inDEMfile[255],ceos[255],outfile[255],infileStorage[255];
	char *infile=NULL, *ext=NULL;
	FILE *inDEM,*in=NULL,*out;
	int doRadiometric=0,dem_is_ground_range=0;

	logflag=0;

/* parse commandline arguments */
	currArg = 1; /* in cla.h in asf.h in deskew_dem.h */
	while (currArg < (argc-3)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-i")) {
			CHECK_ARG(2);
			strcpy(infile=infileStorage,GET_ARG(2));
			ext=findExt(infile);
			if ((0!=strcmp(ext,".amp")) && (0!=strcmp(ext,".img")))
			  {printf("**ERROR: <inSARfile> must have a \".amp\" or \".img\" extention.\n");usage(argv[0]);}
			doRadiometric = atoi(GET_ARG(1));
			if ((doRadiometric != 0) && (doRadiometric != 1))
			  {printf("**ERROR:  <bit> must be either 0 or 1\n"); usage(argv[0]);}
		}
		else if (strmatch(key,"-log")) {
			CHECK_ARG(1);
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			logflag = 1;
		}
		else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < 3) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	strcpy(inDEMfile,argv[currArg]);
	strcpy(ceos,     argv[currArg+1]);
	strcpy(outfile,  argv[currArg+2]);
	ext=findExt(inDEMfile);
	if (0==strcmp(ext,".dem"))
		dem_is_ground_range=1;

	StartWatch();
	system("date");
	printf("Program: deskew_dem\n\n");
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: deskew_dem\n\n");
	}

/*Get CEOS Parameters.*/
	meta=meta_init(ceos);

/*Extract DDRs*/
	if (infile)
		c_getddr(infile,&inddr);
	
	c_getddr(inDEMfile,&ddrDEM);
	
	outddr=ddrDEM;
	if (infile)
	{
		outddr.dtype=inddr.dtype;
		if ((inddr.nl!=ddrDEM.nl)&&(inddr.ns!=ddrDEM.ns))
		{
			sprintf(errbuf, "   ERROR: The DEM and the image must be the same size.\n");
			printErr(errbuf);
		}
	}
	nl=ddrDEM.nl;
	ns=ddrDEM.ns;
  	printf("   Images are %i lines by %i samples.\n",nl,ns);
	
/*Allocate vectors.*/
	slantGR=(double *)MALLOC(sizeof(double)*ns);
	groundSR=(double *)MALLOC(sizeof(double)*ns);
	heightShiftSR=(double *)MALLOC(sizeof(double)*ns);
	heightShiftGR=(double *)MALLOC(sizeof(double)*ns);
	slantRange=(double *)MALLOC(sizeof(double)*ns);
	slantRangeSqr=(double *)MALLOC(sizeof(double)*ns);
	incidAng=(double *)MALLOC(sizeof(double)*ns);
	sinIncidAng=(double *)MALLOC(sizeof(double)*ns);
	cosIncidAng=(double *)MALLOC(sizeof(double)*ns);

	grPixelSize=calc_ranges(&outddr);
	
/*Set up the output DDR.*/
	set_ddr_corners(ceos,&outddr);
	outddr.pdist_x=grPixelSize;
	outddr.pdist_y=meta->geo->yPix*ddrDEM.line_inc;
	
	c_putddr(outfile,&outddr);
	meta->geo->type='G';
	meta->geo->xPix=grPixelSize;
	meta_write(meta,outfile);
	
/*Open files.*/
	inDEM=fopenImage(inDEMfile,"rb");
	out=fopenImage(outfile,"wb");
	if (infile)
		in=fopenImage(infile,"rb");
	
	if (dem_is_ground_range)
	  printf("   DEM is in ground range.\n");
	else 
	  printf("   DEM in slant range, but will be corrected.\n");
	if (dem_is_ground_range && logflag)
	  printLog("   DEM is in ground range.\n");
	else if (logflag) 
	  printLog("   DEM in slant range, but will be corrected.\n");
	
	if (infile)
	  printf("   Correcting image ");
	else 
	  printf("   Correcting DEM ");
	if (doRadiometric)
	  printf("geometrically and radiometrically.\n");
	else 
	  printf("geometrically.\n");
	if (infile && logflag)
	  printLog("   Correcting image ");
	else if (logflag)
	  printLog("   Correcting DEM ");
	if (doRadiometric && logflag)
	  printLog("geometrically and radiometrically.\n");
	else if (logflag)
	  printLog("geometrically.\n");
	
/*Allocate input buffers.*/
	inLine=(float *)MALLOC(sizeof(float)*ns);
	outLine=(float *)MALLOC(sizeof(float)*ns);
	srDEMline=(float *)MALLOC(sizeof(float)*ns);
	
/*Map DEM to ground range.*/
	if (dem_is_ground_range) /*It's much simpler if the DEM is already in ground range.*/
		grDEM=(float *)MALLOC(sizeof(float)*ns);
	else
	{ /*If the dem is slant range, then we need to map it to ground range, 
	    all at once-- we have to read it ALL in to interpolate the columns.*/
/*		printf("Mapping DEM to ground range...\n");*/
		grDEM=(float *)MALLOC(sizeof(float)*ns*nl);
		for (y=0;y<nl;y++)
		{
			getFloatLine(inDEM,&ddrDEM,y,srDEMline);
			dem_sr2gr(srDEMline,&grDEM[y*ns],ns);
		}
		for (x=0;x<ns;x++)
			dem_interp_col(&grDEM[x],ns,nl);/*Close gaps in y direction.*/
	}
	
/*Rectify data.
	printf("Rectifying Data...\n");*/
	for (y=0;y<nl;y++)
	{
		if (infile) 
		{
			if (dem_is_ground_range)
			{ /*We read in the DEM line-by-line (keeping two lines buffered).*/
				float * tmp=srDEMline;srDEMline=grDEM;grDEM=tmp;
				getFloatLine(inDEM,&ddrDEM,y,grDEM);
				grDEMline=grDEM;
				grDEMlast=srDEMline;
			} else { /*Otherwise, we just fetch the appropriate lines from the big buffer.*/
				grDEMline=&grDEM[y*ns];
				grDEMlast=&grDEM[(y-1)*ns];
			}
			getFloatLine(in,&inddr,y,inLine);
			geo_compensate(grDEMline,inLine,outLine,ns);
			if (y>0&&doRadiometric)
				radio_compensate(grDEMline,grDEMlast,outLine,ns);
			putFloatLine(out,&outddr,y,outLine);
		} else 
			putFloatLine(out,&outddr,y,&grDEM[y*ns]);
	}
/*	printf("Program Complete!\n");*/

	printf("\n   Wrote %lld bytes of data\n\n", (long long) (nl*ns*4));
	StopWatch();
	if (logflag) {
	  sprintf(logbuf,"\n   Wrote %lld bytes of data\n\n", (long long) (nl*ns*4));
	  printLog(logbuf);
	  StopWatchLog(fLog);
	  FCLOSE(fLog);
	}

	return(0);
}
void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-i <inSARfile> <bit>] [-log <file>]\n"
	"              <inDEMfile> <ceos> <outfile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <inDEMfile>  IEEE float slant-range dem, with \n"
	"                  extension .ht or .dem.\n"
	"   <ceos>       Original image metadata.\n"
	"   <outfile>    Output LAS image, with an extention.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -i    The <inSARfile> is the LAS image to be rectified, with\n"
	"             extention.  If <bit> is 1 both radiometric and\n"
	"             geometric rectification are performed; if <bit> is\n"
	"             0 then only geometric rectification is performed.\n"
	"   -log  Allows the output to be written to a log <file>.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program removes incidence-angle skew and maps from\n"
	"   slant range to ground range.  It will do this to a DEM\n"
	"   or SAR image.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n", VERSION);
	exit(1);
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
	if (srXSeaLevel<0) srXSeaLevel=0;
	if (srXSeaLevel>=ns-1) srXSeaLevel=ns-1;
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
	if (grXSeaLevel>=ns-1) grXSeaLevel=ns-1;
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
