/****************************************************************
NAME:  p2c

SYNOPSIS:    p2c inampName outcpxName

DESCRIPTION:
    Convert amp & phase files to a complex image file.

    inAPfile is the base file name for the amplitude and phase files.
    An extension of .amp & .phase will be appended.
    outcpxName is a complex image file. An extension of .cpx will be appended.


EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:    PURPOSE:
    ---------------------------------------------------------------
    1.0     4/01     Rudi Gens - Original Development
    1.25    6/03     P. Denny  - Forget about the DDR file, use meta
                                  Don't let input & output name be the same
                                  Use get/put_*_line routines instead of
                                   FREAD/FWRITE 

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   p2c: polar (amp + phase) to complex stream converter		    *
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
#include "asf_complex.h"

#define VERSION 1.25
#define BUF     256
/* #define CHUNK_OF_LINES 256 -- gotten from asf.h */

void usage(char *name);

int main (int argc, char *argv[])
{
  char cpxName[BUF],ampName[BUF],phsName[BUF]; /* File Names                  */
  FILE *fdCpx, *fdAmp, *pdPhs;            /* File Pointers                    */
  int line, sample;                       /* Line & sample indices for looping*/
  int percentComplete;                    /* Percent of data processed        */
  int ampBlockSize, phsBlockSize;         /* Number of samples gotten         */
  float *ampBuf, *aP, *phsBuf, *pP;       /* Output data buffers              */
  complexFloat *cpxBuf, *cP;              /* Input data buffers               */
  meta_parameters *inMeta, *outMeta;      /* In/Out meta structs              */

/* Make sure there are the correct number of args in the command line */
  if (argc < 3) { usage(argv[0]); }

/* Make sure input and output names are different */
  if (strcmp(argv[1],argv[2])==0) {
    printf("p2c: Input and output names cannot be the same. Exiting.\n");
    exit(EXIT_FAILURE);
  }
  
/* Get commandline args */
  create_name (ampName,argv[1],".amp"); 
  create_name (phsName,argv[1],".phase"); 
  create_name (cpxName,argv[2],".cpx");
  
/* Read the meta data. Write output meta with COMPLEX_* data type. */
  inMeta = meta_read(argv[1]);
  outMeta = meta_copy(inMeta);
  outMeta->general->data_type = meta_polar2complex(inMeta->general->data_type);
  meta_write(outMeta,argv[2]);

/* malloc buffers, check and open files */
  cpxBuf = (complexFloat *)MALLOC(sizeof(complexFloat)
                         * outMeta->general->sample_count * CHUNK_OF_LINES);
  ampBuf = (float *)MALLOC(sizeof(float)
                         * inMeta->general->sample_count * CHUNK_OF_LINES);
  phsBuf = (float *)MALLOC(sizeof(float)
                         * inMeta->general->sample_count * CHUNK_OF_LINES);
  fdCpx = fopenImage(cpxName, "wb");
  fdAmp = fopenImage(ampName, "rb");  
  pdPhs = fopenImage(phsName, "rb");  

/* Run thru the complex file, writing real data to amp and imag data to phase */
  printf("\n");
  percentComplete = 0;
  for (line=0; line<inMeta->general->line_count; line+=CHUNK_OF_LINES)
  {
    if ((line*100/inMeta->general->line_count == percentComplete)) {
      printf("\rConverting amp and phase to complex: %3d%% complete.",
             percentComplete++);
      fflush(NULL);
    }
    ampBlockSize = get_float_lines(fdAmp,inMeta,line,CHUNK_OF_LINES,ampBuf);
    phsBlockSize = get_float_lines(pdPhs,inMeta,line,CHUNK_OF_LINES,phsBuf);
    if (ampBlockSize != phsBlockSize) {
      printf("\n");
      printf("p2c: Failed to get the same number of samples from amplitude and phase files.\n");
      printf("p2c: Exiting...\n\n");
      exit(EXIT_FAILURE);
    }
    cP = cpxBuf;
    aP = ampBuf;
    pP = phsBuf;
    for (sample=0; sample<ampBlockSize; sample++) {
      cP->real = *aP * cos(*pP);
      cP->imag = *aP * sin(*pP);
      cP++;
      aP++;
      pP++;
    }
    put_complexFloat_lines(fdCpx,outMeta,line,CHUNK_OF_LINES,cpxBuf);
  }
  printf("\rConverted amp and phase to complex:  100%% complete.\n\n");

  /* close, free, halt */
  FCLOSE(fdCpx);
  FCLOSE(fdAmp);
  FCLOSE(pdPhs);
  FREE(cpxBuf);
  FREE(ampBuf);
  FREE(phsBuf);
  meta_free(inMeta);
  meta_free(outMeta);
  
  return 0;
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <in> <out>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <in>   Input files, assumed to be the base file name\n"
	"           of the amplitude and phase files.\n"
	"           (Called <in>.amp and <in>.phase.)\n"
	"   <out>  The complex output file (called <out>.cpx).\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts polar (amp + phase) to complex stream.\n");
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
