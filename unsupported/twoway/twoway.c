/****************************************************************
NAME:   twoway -- transforms image line, sample to geographic
                  lat, lon and vice-versa.

SYNOPSIS: twoway [-g or -i] [-h avg_elevation] meta_file  [ddr_file]

        meta_file: a CEOS .L or .ldr; or .meta metadata file.
        ddr_file: optional DDR file (for windowing/multilooking)
                 from which coordinates are taken.

DESCRIPTION:
        This program converts between image coordinates (line, sample
        in pixels) and geographic coordinates (latitude, longitude
        in decimal degrees) for a series of points within an image.

        If 'avg_elevation' is not set on the command line, twoway assumes an
        elevation of 0.0 and does not figure terrain height into its
        coordinate transformation.

        Twoway works for any SAR image (geocoded, slant range, ground range,
        etc.).  It will read the metadata information from a CEOS .L or .ldr;
        or a .meta file.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        AUTHOR:          PURPOSE:
    -----------------------------------------------------------
    1.0     4/95         Mike Shindle     Original Development
    2.1     7/95         M. Shindle       No change to code. Compiled
					   version uses itwoway 4.1
    3.0     11/98        O. Lawlor        Total rewrite with asf_meta.
    3.15    3/02         P. Denny         Update commandline parsing
                                           and usage()

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   twoway -- Transforms image line, sample to geographic lat, lon and      *
*	      vice-versa.						    *
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

#define VERSION  3.15

/* external function declaration */
void usage(char *name);


int main(int argc,char **argv)
{
   double line;
   double samp;
   double elev = 0.0;
   double lat;
   double lon;
   char sarfile[255];
   meta_parameters *meta;
   int has_ddr=0;
   struct DDR ddr;
   /* flag variables */
   int iter = 1;
   int gflag = 0, iflag = 0;

   currArg=1;   /* from cla.h which is in asf.h */

   /* Parse command line args */
   while (currArg < (argc-1))
   {
	char *key=argv[currArg++];
	if (strmatch(key,"-g")) {
		if (iflag) {printf("Cannot use both -g & -i\n"); usage(argv[0]);}
		gflag = 1;
	}
	else if (strmatch(key,"-i")) {
		if (gflag) {printf("Cannot use both -g & -i\n"); usage(argv[0]);}
		iflag = 1;
	}
	else if (strmatch(key,"-h")) {
		CHECK_ARG(1);
		elev = atof(GET_ARG(1));
	}
	else if (strmatch(key,"-d")) {
		CHECK_ARG(1);
		c_getddr(GET_ARG(1),&ddr);
		has_ddr=1;
	}
	else {printf("\n*****Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
   }
   if ((argc-currArg) < 1) {printf("Insufficient arguments.\n"); usage(argv[0]);}

   /* get SARfile */
   strcpy(sarfile,argv[currArg]);
   
   /* initialize transform */
   meta=meta_init(sarfile);  

   /* Print relevant information */
   printf("\n\n\tSAR Image: %s\tElevation: %.4f\n",sarfile,elev);
   
   /* convert coordinates */
   while (iter) {
     char data[255];
     printf("\t----------------------------------------------------\n");
     if (gflag) 
     {
        printf("\tEnter input latitude: ");
	gets(data);
	sscanf(data,"%lf",&lat);
	iter=(int)lat;
	if (data[0]==0x1B||data[0]=='q'||data[0]=='e'||iter==0) exit(0);
	printf("\tEnter input longitude: ");
	gets(data);
	sscanf(data,"%lf",&lon);
        meta_get_lineSamp(meta,lat,lon,elev,&line,&samp);
	if (has_ddr)
	{/*Translate from meta's coordinates to ddr's coordinates*/
		line=(line-(ddr.master_line-1))/ddr.line_inc;
		samp=(samp-(ddr.master_sample-1))/ddr.sample_inc;
	}
        printf("\t\tline: %.2f, sample: %.2f\n",line,samp);
     } else {
        printf("\tEnter input line: ");
	gets(data);
	sscanf(data,"%lf",&line);
	iter=(int)line;
	if (data[0]==0x1B||data[0]=='q'||data[0]=='e'||iter==0) exit(0);	
	printf("\tEnter input sample: ");
	gets(data);
	sscanf(data,"%lf",&samp);
	if (has_ddr)
	{/*Translate from ddr's coordinates to meta's coordinates*/
		line=(ddr.master_line-1)+ddr.line_inc*(line);
		samp=(ddr.master_sample-1)+ddr.sample_inc*(samp);
	}
        meta_get_latLon(meta,line,samp,elev,&lat,&lon);
        printf("\t\tlatitude: %10.6f, longitude: %10.6f\n",lat,lon);
     }
   }
   return 0;
}

/* Print usage & help information */
void usage(char *name) {
 printf("\n"
	"USAGE:\n"
	"   %s [-g | -i] [-h avg_elev] [-d ddr_file] <meta_file>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   meta_file - image metadata file name \n"
	"               (e.g. meta_file.L, or meta_file.meta).\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -g          - convert from geographic to image coordinates\n"
	"   -i          - convert from image to geographic coordinates\n"
	"                 g & i are mutually exclusive. (-i is the default)\n"
	"   -h avg_elev - use average elevation height meters.\n"
	"   -d ddr_file - optional image DDR file name \n"
	"                 (used for multilooked images).\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Transforms line by sample image to geographic lat, lon and\n"
	"   vice-versa. To exit, enter 0 at either the latitude or line\n"
	"   prompt.\n");
 printf("\n"
	"Version %.2f, ASF SAR TOOLS\n"
	"\n",VERSION);
 exit(1);
}

