/****************************************************************
NAME: jpeg2asf

SYNOPSIS:  jpeg2asf <infile> <outfile>
        <infile> - a JPEG/JFIF graphic image
        <outfile> - a LAS 6.0 3-banded byte image

DESCRIPTION:  
	Reads a JPEG file in RGB order into memory, and then writes it out as a 
	3-banded LAS image.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     3/99         O. Lawlor - Original Development
    1.1     2/04         P. Denny  - Changed License to BSD
                                      Change name from jpeg2las to jpeg2asf

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
****************************************************************/
/******************************************************************************
*								              *
* jpeg2asf: convert JPEG image into a 3-banded ASF tools byte file            *
*                                                                             *
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
#include "ifm.h"
#include "ifm2ppm.h"
#include "ddr.h"

#define VERSION      1.0

/* global & local function declaration */

int main (int argc, char *argv[])
{
  char *infile,*outfile;
  FILE *fin;

  /* Start time keeping*/
  StartWatch();
   
  /* set constants & other variables  */
  if (argc != 3)  usage(argv[0]);  
  infile=argv[1];
  outfile=argv[2];
  
  /* open files */
  fin = FOPEN(infile,"rb");

  decode_jpeg(fin,outfile);

  return(0);
}

void usage(char *name)
{ 
  printf("\nUsage: %s <in> <out>\n",name);
  printf("\n");
  printf(" <in>   a JPEG/JFIF image file.\n"
	 " <out>  a 3-banded ASF tools byte image.\n");
  printf("\n");
  printf("Converts a JPEG image into a 3-banded ASF tools byte file\n");
  printf("Version %.2f, ASF SAR TOOLS\n\n",VERSION);
  exit(1);
}

