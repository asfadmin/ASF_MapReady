/******************************************************************************
NAME:  sr2gr - converts slant range images to ground range images

SYNOPSIS:   sr2gr infile outmeta pixsiz

       infile     base name (.img,.meta)
       outfile    base name (.img,.meta)
       pixsiz     output pixel spacing (meters)
 
DESCRIPTION:
	This program converts slant range imagery into ground range imagery. 
	The algorithm calculates the ground range to the first pixel in the
	image and from the spacecraft ephemeris, earth ellipsoid, and slant
	range to first pixel given in the image's metadata.  It then uses the
	slant range spacing interval to determine appropriate ground range
	positions.  The remapping is performed using bi-linear interpolation.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    6/97   T. Logan	Original Implementation (Based on
				JPL provided routine sr2gr_vec)
    2.0	    3/98   T. Logan     Modify to work with AISP outputs
    3.0     4/98   T. Logan     Modified to work with RAMMS data
    3.1     8/98   O. Lawlor    Corrected azimuth pixel spacing equation.
    4.0     12/98  O. Lawlor    Modified for new metadata routines.
    4.5     02/04  P. Denny     Update to use new metastruct instead of ddr
                                  Use improved get/put_float_line

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   sr2gr - converts slant range images to ground range images		    *
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
#include "sr2gr.h"

#define VERSION 4.5

#define REQ_ARGS 3

/*Create vector for multilooking.*/
void ml_vec(float oldSize, float newSize,float *ml)
{
	float  gr=0;
	int    ii;
	
	for (ii=0; ii<MAX_IMG_SIZE; ii++)
	{
		ml[ii]=gr/oldSize;
		gr+=newSize;
	}
}

int main(int argc,char *argv[])
{
	int    in_np,  in_nl;               /* input number of pixels,lines   */
	int    out_np, out_nl;              /* output number of pixels,lines  */
	int    ii,line;
	float  newSize,oldX,oldY;
	float  sr2gr[MAX_IMG_SIZE];
	float  ml2gr[MAX_IMG_SIZE];
	int    a_lower[MAX_IMG_SIZE];
	int    lower[MAX_IMG_SIZE], upper[MAX_IMG_SIZE];
	float  a_ufrac[MAX_IMG_SIZE], a_lfrac[MAX_IMG_SIZE];
	float  ufrac[MAX_IMG_SIZE], lfrac[MAX_IMG_SIZE];
	float *ibuf1,*ibuf2,*obuf;
	char   infile_name[512],inmeta_name[512];
	char   outfile_name[512],outmeta_name[512];
	FILE  *fpi, *fpo;
	meta_parameters *in_meta;
	meta_parameters *out_meta;
	extern int currArg; /* in cla.h from asf.h; initialized to 1 */

	/* Make sure we've got the right amount of arguments */
	if (currArg != (argc-REQ_ARGS)) {usage(argv[0]);}

	/* Get required arguments */
	create_name (infile_name, argv[currArg], ".img");
	create_name (inmeta_name, argv[currArg], ".meta");
	create_name (outfile_name, argv[currArg+1], ".img");
	create_name (outmeta_name, argv[currArg+1], ".meta");
	newSize = atof(argv[currArg+2]);

	
	in_meta  = meta_read(inmeta_name);
	out_meta = meta_copy(in_meta);
	in_nl = in_meta->general->line_count;
	in_np = in_meta->general->sample_count;
	
	if (in_meta->sar->image_type != 'S')
	{
		bail("sr2gr only works with slant range images!\n");
		exit(EXIT_FAILURE);
	}
	
	printf("Output Pixels: %f (range)  %f (azimuth)\n",newSize,newSize);

	oldX = in_meta->general->x_pixel_size * in_meta->sar->sample_increment;
	oldY = in_meta->general->y_pixel_size * in_meta->sar->line_increment;

	/*Update metadata for new pixel size*/
	out_meta->sar->time_shift  += ((in_meta->general->start_line+1)
				* in_meta->sar->azimuth_time_per_pixel);
	out_meta->sar->slant_shift += ((in_meta->general->start_sample+1)
				* in_meta->general->x_pixel_size);
	out_meta->general->start_line   = 0.0;
	out_meta->general->start_sample = 0.0;
	out_meta->sar->azimuth_time_per_pixel *= newSize
					/ in_meta->general->y_pixel_size;
	out_meta->sar->line_increment   = 1.0;
        out_meta->sar->sample_increment = 1.0;
	
	/*Create ground/slant and azimuth conversion vectors*/
	out_meta->sar->image_type       = 'G'; 
	out_meta->general->x_pixel_size = newSize;
	out_meta->general->y_pixel_size = newSize;
	sr2gr_vec(out_meta,oldX,newSize,sr2gr);
	ml_vec(oldY,newSize,ml2gr);

	out_np = MAX_IMG_SIZE;
	out_nl = MAX_IMG_SIZE;
	for (ii=MAX_IMG_SIZE-1; ii>0; ii--)
		if ((int)sr2gr[ii] > in_np)
			out_np = ii;
	for (ii=MAX_IMG_SIZE-1; ii>0; ii--)
		if ((int)ml2gr[ii] > in_nl)
			out_nl = ii;
	
	out_meta->general->line_count   = out_nl;
	out_meta->general->sample_count = out_np;
	if (out_meta->projection) {
		out_meta->projection->perX = newSize;
		out_meta->projection->perY = newSize;
	}

	meta_write(out_meta,outmeta_name);
	
	fpi = fopenImage(infile_name,"rb");
	fpo = fopenImage(outfile_name,"wb");
	
	printf(" Input image is %s\n",infile_name);
	printf(" Input  lines, samples: %i %i\n",in_nl,in_np);
	printf(" Output image is %s\n",outfile_name);
	printf(" Output lines, samples: %i %i\n\n",out_nl,out_np);
	
	for (ii=0; ii<MAX_IMG_SIZE; ii++)
	{
		lower[ii] = (int) sr2gr[ii];
		upper[ii] = lower[ii] + 1;
		ufrac[ii] = sr2gr[ii] - (float) lower[ii];
		lfrac[ii] = 1.0 - ufrac[ii]; 
		
		a_lower[ii] = (int) ml2gr[ii];
		a_ufrac[ii] = ml2gr[ii] - (float) a_lower[ii];
		a_lfrac[ii] = 1.0 - a_ufrac[ii]; 
	}

	ibuf1 = (float *) MALLOC ((in_np+2)*sizeof(float));
	ibuf2 = (float *) MALLOC ((in_np+2)*sizeof(float));
	obuf = (float *) MALLOC (out_np*sizeof(float));

	printf("\n");
	for (line=0; line<out_nl; line++)
	{
		if (a_lower[line]+1 < in_nl)
		{
			get_float_line(fpi,in_meta,a_lower[line],  ibuf1);
			get_float_line(fpi,in_meta,a_lower[line]+1,ibuf2);
		}
		for (ii=0; ii<out_np; ii++)
		{
			int val00,val01,val10,val11,tmp1,tmp2;
			val00 = ibuf1[lower[ii]];
			val01 = ibuf1[upper[ii]];
			val10 = ibuf2[lower[ii]];
			val11 = ibuf2[upper[ii]];
			
			tmp1 = val00*lfrac[ii] + val01*ufrac[ii];
			tmp2 = val10*lfrac[ii] + val11*ufrac[ii];
			
			obuf[ii] = tmp1*a_lfrac[line] + tmp2*a_ufrac[line];
		}
		put_float_line(fpo,out_meta,line,obuf);
		if (line % 200 == 0)
			{printf("\rWriting line %i",line);fflush(NULL);}
	}
        meta_free(in_meta);
        meta_free(out_meta);
	FCLOSE(fpi);
	FCLOSE(fpo);
	
	printf("\rFinished. Wrote %i lines.\n\n",line);
	
	return 0;
}

void usage (char * name)
{
 printf("\n"
	"USAGE:\n"
	"%s <infile> <outfile> <pixsiz> \n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   infile   Input image base name (.img & .meta)\n"
	"   outfile  Output image base name (.img & .meta)\n"
	"   pixsiz   Output pixel spacing, in meters\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts the given image from slant range to ground range.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
