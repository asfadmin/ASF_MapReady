/****************************************************************
NAME:  c2p

SYNOPSIS:    c2p inCPXfile outAPfile

DESCRIPTION:
    Convert complex images to amp & phase files

    inCPXfile is a complex image file. An extension of .cpx will be appended.
    outAPfile is the base name for the resulting amplitude and phase files.
    An extension of .amp & .phase will be appended.


EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     9/96        M. Shindle
    			 & R. Fatland - Original Development
    1.05    5/00	P. Denny      - output a .ddr file

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   c2p:  complex to polar (amp + phase) stream converter		    *
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

#define VERSION 1.05
#define BSZ	1024*64    /* buffer size */
#define BUF     255
#define SAME	0

void usage(char *name);

int main (int argc, char *argv[])
{
  char cpxfile[BUF], ampfile[BUF], phsfile[BUF], command[BUF];
  FILE *fdCpx, *fdAmp, *fdPhase;
  int i, j, loops;
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
  
  sprintf(cpxfile,"%s.cpx",argv[1]);
  sprintf(ampfile,"%s.amp",argv[2]); 
  sprintf(phsfile,"%s.phase",argv[2]); 

  /* Copy .ddr file for output if necessary */
  if (strcmp(argv[1],argv[2])!=SAME)
   {
    sprintf(command,"cp %s.ddr %s.ddr",argv[1],argv[2]);
    system(command);
   }

  fdCpx = fopenImage(cpxfile, "rb");
  fdAmp = fopenImage(ampfile, "wb");  
  fdPhase = fopenImage(phsfile, "wb");

  loops = 0;

  /*
   * stream while loop 
   * read in block of input data from input file 
   * set i = to number of var's read
   */
  while ((i=fread(cpx,1,BSZ*sizeof(FComplex),fdCpx)) > 0)
    {
      /* loop over buffers; fill up the output */
      i /= sizeof(FComplex);
      for (j=0, cP=cpx, aP=amp, pP=phase; j < i; j++, cP++, aP++, pP++)
        if (cP->real != 0.0 || cP->imag != 0.0)
         {
          *aP = sqrt((cP->real)*(cP->real) + (cP->imag)*(cP->imag));
          *pP = atan2(cP->imag, cP->real);
         }
        else { *aP = 0.0; *pP = 0.0; }
    
      /* write output */
      i *= sizeof(float);
      FWRITE(amp, 1, i, fdAmp);
      FWRITE(phase, 1, i, fdPhase);
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
 printf("\n"
	"USAGE:\n"
	"   %s <in> <out>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <in> is the input file, assumed to be complex valued.\n"
	"         will add .cpx extension to create <in>.cpx\n"
	"   <out> is the base file name for the output; c2p creates\n"
	"         output files called <out>.amp, <out>.phase,\n"
	"         and, if necessary, <out>.ddr\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts a complex image file to polar image files (amp + phase)\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(1); 
}
