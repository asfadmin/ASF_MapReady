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
#include "ceos_io.h"
#include "asf_meta.h"

#define VERSION 5.0

int main(int argc, char **argv)
{
  char *inSAR, *outLAS;		/* Input & output file names.  */
  int *ibuff;			/* Input buffer.  */
  int x, y;			/* Loop counters.  */
  /* Number of lines/samples in input & output.  */
  int ns, nl;         
  float *obuff;			/* Output buffer.  */
  /* Input and output file pointers.  */
  FILE *fpOut;	
  CEOS_FILE     *fpIn;
  unsigned char *obuff_char;	/* Character output buffer.  */

  /* Start timer to see how long program takes.  */
  StartWatch();
	
  /* Parse command line */
  if (argc != 3 ) {
    usage(argv[0]);
  }
  inSAR  = argv[1];
  outLAS = argv[2];
  
  /* Open input image.  */
  fpIn = fopenCeos(inSAR);
  nl = fpIn->meta->general->line_count;
  ns = fpIn->meta->general->sample_count;
  
  /* Open file for output image & write metadata.  */
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
		if (fpIn->meta->general->data_type == BYTE)
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
  /* Stop the timer and print info about how long the program took.  */
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
