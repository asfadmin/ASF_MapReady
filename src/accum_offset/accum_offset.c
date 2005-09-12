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
    3.2     01/03        Update to use new meta structures

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:	Accum_offset does not take into account an average elevation.
        Elevation for all images is assumed to be zero.

****************************************************************/
/****************************************************************************
*								            *
*   Accum_offset -- puts metafiles in ascending order by latitude.	    *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include "asf.h"
#include "asf_meta.h"

#define VERSION 3.2

/* structures */
struct OFFSET {
    double line;
    double sample;
};

#define MAX(a,b) ((a) >= (b))
#define MIN(a,b) ((a) <= (b))

/* function declarations */
void mosaic_image_pos(struct OFFSET *ip, int n, char **name, int *ms, int *ml);
meta_parameters *get_metadata(const char *fName);

static
void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <metafile1> <metafile2> [...]\n", name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   metafile1   A meta file (.ldr, .trl, etc) (do not include extension)\n"
	"   metafile2   A meta file (.ldr, .trl, etc) (do not include extension)\n");	
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   ...   You may add as many meta files as you like.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Puts metafiles in ascending order by latitude.");
 printf("\n"
	"Version %.2f, ASF SAR Tools.\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}

int main(int argc, char **argv) 
{
	int ii;
	meta_parameters *meta1st;
	struct OFFSET *image_pos;
	
	int filec;
	char **fNames;
	double elev=0.0;
	int npixels, nlines;
	
	if (argc < 3) usage(argv[0]);
	fNames = &argv[1];
	filec  = argc-1;
	
	/* declare space for image_pos */
	image_pos = (struct OFFSET *)MALLOC(filec * sizeof(struct OFFSET));
	
	/* calculate offsets */
	printf ("\n  File  \t  Line \t  Sample ");
	printf ("\n--------\t-------\t---------\n");
	
	meta1st = get_metadata(argv[1]);
	for (ii = 0; ii < filec; ii++) {
		meta_parameters *meta = get_metadata(fNames[ii]);
		double latCen,lonCen;
		double xPos,yPos;
		meta_get_latLon(meta,0,0,elev,&latCen,&lonCen);
		meta_get_lineSamp(meta1st,latCen,lonCen,elev,&yPos,&xPos);
		image_pos[ii].line   = yPos;
		image_pos[ii].sample = xPos;
		printf("\n%s\t%.0f\t%.0f\n",fNames[ii],yPos+1,xPos+1);
	}
	/* determine total # of lines and sample */
	mosaic_image_pos(image_pos,filec,fNames,&npixels,&nlines);
	printf("Total number of lines: %d\n", nlines + meta1st->sar->original_line_count);
	printf("Total number of columns: %d\n", npixels + meta1st->sar->original_sample_count);
	
	return 0;
}

meta_parameters *get_metadata(const char *fName)
{
  if (extExists(fName,".meta")) /*Read .meta file if possible*/
    return meta_read(fName);
  else
    return meta_create(fName);
}


/****************************************************************************
 * mosaic_image_pos:
 * calculate each images position in the mosaic and return the width of the
 * mosaic. */
void mosaic_image_pos(struct OFFSET *ip, int n, char **name, int *ms, int *ml)
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



