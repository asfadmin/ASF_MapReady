/****************************************************************
NAME:  reskew_dem

SYNOPSIS:  reskew_dem inGR_DEMfile ceos outSR_DEM outSR_sim_amp [-log <file>]
    
DESCRIPTION:

	Reskew_dem maps an input, ground range DEM into slant range,
	and creates a simulated SAR image.  The input DEM must already
	be lined up with the image, but need not be precisely co-registered.
	In fact, the amplitude image is generated only so the images can be
	co-registered.
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
    1.0	     8/97        O. Lawlor-- Reskew USGS DEMs for Interferometry/
    1.1	     6/97        O. Lawlor-- Made more consistent with deskew_dem.
    1.3      12/98       O. Lawlor-- Allow ground and slant ranges to differ in length.
    1.31     7/01	 R. Gens     Added logfile switch

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
#define VERSION 1.31


void usage(char *name);

/* Find the last extension in a filename.*/
char * get_extention(char *filename)
{
	char *nullExtention="";
/*We play "Hunt the last extention." by searching for periods starting at the end.*/
	int i=strlen(filename)-1;
	while ((i>0)&&(filename[i]!='.')) 
		i--;
	if (i!=0)
	/*...then return it.*/
		return &filename[i+1];
	else return nullExtention;
}

double minPhi,maxPhi,phiMul,grPixelSize;
meta_parameters *meta;
int gr_ns,sr_ns;

double *slantGR;/*Slant range pixel #*/
double *heightShiftGR;
double *heightShiftSR;
double *groundSR;/*Ground range pixel #*/
double *slantPixel;
double *groundRange;
double *slantRangeSqr,*slantRange,*heightShift;
double *incidAng,*sinIncidAng,*cosIncidAng;
struct DDR ddrDEM,outddr,inddr;

int main(int argc, char *argv[])
{
	float *grDEMline,*srDEMline,*outAmpLine;
	register int y,nl,percent=5;
	char inDEMfile[255],ceos[255],outDEMfile[255],outAmpFile[255];
	FILE *inDEM,*outDEM,*outAmp;

	system("date");
	printf("Program: reskew_dem\n\n");
	
/*Deal with command-line arguments.*/
	if (argc<5) usage(argv[0]);
	strcpy(inDEMfile,argv[1]);
	strcpy(ceos,argv[2]);
	strcpy(outDEMfile,argv[3]);
	strcpy(outAmpFile,argv[4]);
	if (argc==7 && strncmp(argv[5],"-log",4)==0) {
	  sprintf(logFile,"%s",argv[6]);
	  fLog = FOPEN(logFile, "a");
	  logflag=1;
	  StartWatchLog(fLog);
	  printLog("Program: reskew_dem\n\n");
	}

/*Extract DDRs*/
	c_getddr(inDEMfile,&ddrDEM);
	outddr=ddrDEM;
	nl=ddrDEM.nl;
	gr_ns=ddrDEM.ns;
	sr_ns=gr_ns-400;/*The 400 pixels here has to match the 
	  extra amount added in the demIFM script.*/
	outddr.ns=sr_ns;
	
/*Allocate vectors.*/
	meta=meta_init(ceos);
	slantGR=(double *)MALLOC(sizeof(double)*gr_ns);
	groundSR=(double *)MALLOC(sizeof(double)*sr_ns);
	heightShiftSR=(double *)MALLOC(sizeof(double)*sr_ns);
	heightShiftGR=(double *)MALLOC(sizeof(double)*gr_ns);
	slantRange=(double *)MALLOC(sizeof(double)*sr_ns);
	slantRangeSqr=(double *)MALLOC(sizeof(double)*sr_ns);
	incidAng=(double *)MALLOC(sizeof(double)*sr_ns);
	sinIncidAng=(double *)MALLOC(sizeof(double)*sr_ns);
	cosIncidAng=(double *)MALLOC(sizeof(double)*sr_ns);

	grPixelSize=calc_ranges(&outddr);


/*Set up the output DDR.*/
	c_putddr(outDEMfile,&outddr);
	c_putddr(outAmpFile,&outddr);

/*Open files.*/
	inDEM=fopenImage(inDEMfile,"rb");
	outDEM=fopenImage(outDEMfile,"wb");
	outAmp=fopenImage(outAmpFile,"wb");
	
	grDEMline=(float *)MALLOC(sizeof(float)*gr_ns);
	srDEMline=(float *)MALLOC(sizeof(float)*sr_ns);
	outAmpLine=(float *)MALLOC(sizeof(float)*sr_ns);
	
/*	printf("   Converting Data from ground range to slant range...\n");*/
	for (y=0;y<nl;y++)
	{
		getFloatLine(inDEM,&ddrDEM,y,grDEMline);
		dem_gr2sr(grDEMline,srDEMline,outAmpLine);
		putFloatLine(outDEM,&outddr,y,srDEMline);
		putFloatLine(outAmp,&outddr,y,outAmpLine);
		if ((y*100/nl)==percent) {
		  printf("   Completed %3d percent\n",percent);
		  percent+=5;
		}
	}
	printf("   Completed 100 percent\n\n");
	printf("   Converted %d lines from ground range to slant range.\n\n", nl);
	if (logflag) {
	  sprintf(logbuf,"   Converted %d lines from ground range to slant range.\n\n", nl);
	  printLog(logbuf);
	}
/*	printf("Program Complete!\n");*/
	return(0);
}
void usage(char *name)
{
 printf("\nUsage: %s <inGR_DEM> <ceos> <outSR_DEM> <outSR_simAmp> [-log <file>]\n\n",name);
 printf("   <inGR_DEM>: a lined-up ground-range dem.\n");
 printf("   <ceos>: original SAR image ceos.\n");
 printf("   <outSR_DEM>: output slant-range DEM.\n");
 printf("   <outSR_simAmp>: output simulated amplitude image.\n");
 printf("   -log: allows output to be written to a log file.\n");
 printf("\n"
	"This program remaps the input DEM to slant range,\n"
	"and creates a simulated slant-range amplitude image \n"
	"from it.  This is useful for interferometry.\n"
	"\nVersion %.2f, ASF SAR Tools\n\n", VERSION);
 exit(1);
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
