/****************************************************************
NAME:  p2c

SYNOPSIS:    p2c inAMPfile outCPXfile

DESCRIPTION:
    Convert amp & phase files to a complex image file.

    inAPfile is the base file name for the amplitude and phase files.
    An extension of .amp & .phase will be appended.
    outCPXfile is a complex image file. An extension of .cpx will be appended.


EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     4/01        Rudi Gens - Original Development

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
#include "ddr.h"
#include "ifm.h"

#define VERSION 1.00
#define BSZ	1024*64    /* buffer size */
#define BUF     255

void usage(char *name);

int main (int argc, char *argv[])
{
  char cpxfile[BUF], ampfile[BUF], phsfile[BUF];
  FILE *fdCpx, *fdAmp, *fdPhase;
  int i/*, j*/, k, loops/*, nLooks*/;
  float *amp, *aP, *phase, *pP;
  FComplex *cpx, *cP;

  /* work with args */
  if (argc < 3) {
    usage(argv[0]);
  }

  /* malloc buffers, check and open files, initialize counter */
  cpx = (FComplex *)MALLOC(sizeof(FComplex)*BSZ);
  amp = (float *)MALLOC(sizeof(float)*BSZ);
  phase = (float *)MALLOC(sizeof(float)*BSZ);
  
  sprintf(ampfile,"%s.amp",argv[1]); 
  sprintf(phsfile,"%s.phase",argv[1]); 
  sprintf(cpxfile,"%s.cpx",argv[2]);
  
  fdCpx = fopenImage(cpxfile, "wb");
  fdAmp = fopenImage(ampfile, "rb");  
  fdPhase = fopenImage(phsfile, "rb");  
  loops = 0;

  /*
   * stream while loop 
   * read in block of input data from input file 
   * set i = to number of var's read
   */
  while ((i=fread(amp,1,BSZ*sizeof(float),fdAmp)) && (/*j=*/fread(phase,1,BSZ*sizeof(float),fdPhase)) > 0)
    {
      /* loop over buffers; fill up the output */
      i /= sizeof(float);
      for (k=0, cP=cpx, aP=amp, pP=phase; k < i; k++, cP++, aP++, pP++) {
          cP->real = *aP * cos(*pP);
          cP->imag = *aP * sin(*pP);
      }
    
      /* write output */
      i *= sizeof(FComplex);
      FWRITE(cpx, 1, i, fdCpx);
      if (!(loops%100)) printf("   bytes read = %i\n",loops*BSZ*sizeof(FComplex));
      loops++;
    } /* end of main do-while */
  printf("\n");

  /* close, free, halt */
  FCLOSE(fdCpx);
  FCLOSE(fdAmp);
  FCLOSE(fdPhase);
  FREE(cpx);
  FREE(amp);
  FREE(phase);
  
  return 0;
}


void usage(char *name)
{
 printf("\nUSAGE: %s <in> <out>\n\n",name);
 printf("    <in>   is the input file, assumed to be the base file name\n"
	"           of the amplitude and phase files.");
 printf(" Program will add .amp\n\t   and .phase extensions.\n");
 printf("   <out>   is the complex output file");
 printf("  called <out>.cpx.\n");
 printf("\np2c: polar (amp + phase) to complex stream converter.\n");
 printf("Version %.2f, ASF SAR Tools\n\n",VERSION);
 exit(1);
}
