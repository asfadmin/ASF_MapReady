/******************************************************************
NAME:    sarin.c

SYNOPSIS: sarin sar_infile las_outfile 

DESCRIPTION:
     This program reads ASF SAR images and converts them to LAS 6.0
     DAL images.  The program takes one ASF .dat SAR input file, and
     creates a LAS .img output file with a .ddr metadata file.  If
     the image is geocoded, the LAS image will have a complete & valid
     DDR file.  The image file name must NOT be specified with the
     .dat file extension.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0		3/95   M. Shindle (ASF) 
  2.0		4/95   T. Logan   (ASF)	Removed TAE Dependencies
  3.0		4/95   T. Logan   (ASF) Modified to accept any type
					of standard SAR input file.
					(type 100, 200, 300, or 400)
  4.0		9/95   M. Shindle (ASF) Modified to accept as command
					line input data type. Still
					defaults to EBYTE.
  4.3		3/97   O. Lawlor  (ASF) Now actually WORKS with non-byte data.
  4.4		1/98   D. Corbett 	Determine geocoded/nongeocoded for
					SCANSAR using mpdr info
  4.5		4/98   T. Logan		Correctly read geocoded scansar
  5.0		12/98  O. Lawlor	Total re-write.  Now doesn't use LAS I/O.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
 
*******************************************************************/
/****************************************************************************
*								            *
*   sarin - converts CEOS SAR images to LAS 6.0 format			    *
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
#include "ceos_io.h"
#include "asf_meta.h"

#define VERSION 5.0

int main(int argc, char **argv)
{
	char  *inSAR, *outLAS;      /* Input & output file names              */
	int   *ibuff;               /* Input buffer                           */
	int    x, y;                /* loop counters                          */
	int    ns, nl;              /* number lines/samples in input & output */
	float *obuff;               /* Output buffer                          */
	FILE  *fpOut;               /* input and output file ptrs             */
	CEOS_FILE     *fpIn;
	unsigned char *obuff_char;

	StartWatch();

/* Parse command line */
	if (argc != 3 ) usage(argv[0]);
	inSAR  = argv[1];
	outLAS = argv[2];

/*Open input image.*/
	fpIn = fopenCeos(inSAR);
	nl = fpIn->meta->general->line_count;
	ns = fpIn->meta->general->sample_count;
	
/*Open file for output image & write metadata*/
	fpOut = fopenImage(outLAS,"wb");
	meta_write(fpIn->meta,outLAS);
	
	printf("Input and output nl=%i, ns=%i\n", nl, ns);

/* Allocate buffer space */
	ibuff = (int *)  MALLOC(ns * sizeof(int));
	obuff = (float *)MALLOC(ns * sizeof(float));
	obuff_char = (unsigned char *)MALLOC(ns * sizeof(unsigned char));

/* Read input file, convert, and write to output file */
	for (y=0; y < nl; y++ )
	{
		readCeosLine(ibuff,y,fpIn);

		/*Convert data.*/
		if (strcmp(fpIn->meta->general->data_type,"BYTE")==0)
		{/*For byte data, we have a special case*/
			for (x = 0; x < ns; x++)
				obuff_char[x]=(unsigned char)ibuff[x];
			FWRITE(obuff_char, ns, 1, fpOut);
		} 
		else 
		{/*For non-byte data, we use floats as a middle ground*/
			for (x = 0; x < ns; x++)
				obuff[x]=(float)ibuff[x];
			put_float_line(fpOut, fpIn->meta, y, obuff);
/*			putFloatLine(fpOut, &outddr, y, obuff);*/
		}
		
		if ((y % 100) == 0) 
			printf(" Now Processing Line %d      \r", y);
	}
	printf(" Processed %d Lines                     \n",y);
	closeCeos(fpIn);
	FCLOSE(fpOut);
	
	printf("Sarin is complete!\n\n");
	StopWatch();

	return(0);
}

void usage (char *name)
{
 printf("\n"
	"Usage:\n"
	"   %s <inSARfile> <outLASfile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   inSARfile    CEOS SAR image (input)\n"
	"   outLASfile   LAS image (.img and .meta) (output)\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Reads a CEOS SAR image, and converts it to the\n"
	"   LAS format used by our other tools. This is often\n"
	"   the first step in analysing a SAR image.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(1);
}
