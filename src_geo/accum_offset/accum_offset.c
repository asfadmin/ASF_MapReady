/****************************************************************
NAME:  accum_offset

SYNOPSIS: accum_offset METAfile1 METAfile2 ...

DESCRIPTION:
   This program calculates the accumulative offsets of a chain of images.

   While this program is stand alone, the output was formatted for use by
   auto_swath_mosaic. Accum_offset orders all input images in ascending
   latitude and calculates the cumulative offset of each image from the first
   image. Accum_offset uses the upper left-hand coordinates for its
   calculations. Accum-offset will then print out actual offsets followed
   by offsets in which the lowest column offset is considered to be 1.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    2.0     6/07/95      rewrote original to be use new metadata handler
    2.1     7/26/95      Fixed bug for Southern Hemisphere
    2.2     2/96         Removed file get facdr & replace w/get facdr
    2.3     5/96         Changing to update to new metadata readers
    3.0     12/98        Uses asf_meta instead of two-way.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:	Accum_offset does not take into account an average elevation.
        Elevation for all images is assumed to be zero.

****************************************************************/
/****************************************************************************
*								            *
*   Accum_offset -- puts metafiles in ascending order by latitude.	    *
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

#define VERSION 3.0

/* structures */
struct OFFSET {
    double line;
    double sample;
};

#define MAX(a,b) ((a) >= (b))
#define MIN(a,b) ((a) <= (b))

/* function declarations */
int get_cntr(char *, double *, double *);
int check_order(int,char **,char **);
void mosaic_image_pos(struct OFFSET *,int,char **,int*,int*);

int main(argc, argv) 
int   argc;
char  *argv[];
{
	int   i;
	meta_parameters *metaRef;
	struct OFFSET *image_pos;
	
	int filec,argNo=1;
	char **fNames;
	double elev=0.0;
	int npixels, nlines;
	
	if (argc < 3) {
		fprintf (stderr, "\nUsage: %s METAfile1 METAfile2 ...\n", argv[0]);
		fprintf (stderr, "\tMust provide at least two METAfiles.\n");
		fprintf (stderr, "\tNo extensions needed. Any extensions will be added.\n");
		fprintf (stderr, "\nPuts metafiles in ascending order by latitude.");
		fprintf (stderr, "\nVersion %.2f, ASF SAR TOOLS.\n\n",VERSION);
		exit(1);
	}
	fNames=&argv[argNo];
	filec=argc-argNo;
	
	/* declare space for image_pos */
	image_pos = (struct OFFSET *)MALLOC(filec * sizeof(struct OFFSET));
	
	/* calculate offsets */
	printf ("\n  File  \t  Line \t  Sample ");
	printf ("\n--------\t-------\t---------\n");
	
	metaRef=meta_init(argv[1]);
	for (i = 0; i < filec; i++) {
		meta_parameters *meta=meta_init(fNames[i]);
		double latCen,lonCen;
		double xPos,yPos;
		meta_get_latLon(meta,0,0,elev,&latCen,&lonCen);
		meta_get_lineSamp(metaRef,latCen,lonCen,elev,&yPos,&xPos);
		image_pos[i].line=yPos;
		image_pos[i].sample=xPos;
		printf("\n%s\t%.0f\t%.0f\n",fNames[i],yPos+1,xPos+1);
	}
	/* determine total # of lines and sample */
	mosaic_image_pos(image_pos,filec,fNames,&npixels,&nlines);
	printf("Total number of lines: %d\n", nlines + metaRef->ifm->orig_nLines);
	printf("Total number of columns: %d\n", npixels + metaRef->ifm->orig_nSamples);
	
	return 0;
}

/* calculate each images position in the mosaic 
   and return the width of the mosaic. */
void mosaic_image_pos(ip,n,name,ms,ml)
struct OFFSET *ip;
int n;
char **name;
int *ms;
int *ml;
{
	int maxpos, minpos;
	int minvalue;
	int i;
	
	/* find min col. values. */
	minpos = 0;
	for (i = 0; i < n; i++) 
		if (MIN(ip[i].sample,ip[minpos].sample)) 
			minpos = i;
	minvalue = ip[minpos].sample;
	/* adjust sample values so that min. is 1
	also find max sample */
	for (i=0, maxpos=0; i<n; i++) {
		ip[i].sample += (1 - minvalue);
		if (MAX(ip[i].sample,ip[maxpos].sample))
			maxpos = i;
	}
	*ms = ip[maxpos].sample;
	
	/* find min line values */
	minpos = 0;
	for (i = 0; i < n; i++) 
		if (MIN(ip[i].line,ip[minpos].line)) 
			minpos = i;
	minvalue = ip[minpos].line;
	/* adjust line values so that min. is 1
	also find max line */
	for (i=0, maxpos=0; i<n; i++) {
		ip[i].line += (1 - minvalue);
		if (MAX(ip[i].line,ip[maxpos].line))
			maxpos = i;
	}
	*ml = ip[maxpos].line;
	
	/* print out filenames and corresponding image pos. */
	printf("\nMosaic Image Positions:\n");
	for (i=0;i<n;i++) 
		printf("File: %s\tLine: %.2f\tSample: %.2f\n",
			name[i],ip[i].line,ip[i].sample);
	
	return;
}
