/****************************************************************
NAME: jpeg2las

SYNOPSIS:  jpeg2las <infile> <outfile>

        <infile> - a JPEG/JFIF graphic image
        <outfile> - a LAS 6.0 3-banded byte image

DESCRIPTION:  

	Reads a JPEG file in RGB order into memory, and
	then writes it out as a 3-banded LAS image.

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

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
****************************************************************/
/****************************************************************************
*								            *
*   jpeg2las: convert JPEG image into a 3-banded LAS 6.0 byte file          *
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
	 " <out>  a 3- banded LAS byte image.\n");
  printf("\n");
  printf("jpeg2las: convert JPEG image into a 3-banded LAS 6.0 byte file\n");
  printf("Version %.2f, ASF SAR TOOLS\n\n",VERSION);
  exit(1);
}

