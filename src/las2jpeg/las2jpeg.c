/****************************************************************
NAME: las2jpeg

SYNOPSIS: las2jpeg [-m | -p palfile] <infile> <outfile>

		 -m         input file is an unwrapped mask file.
		 -p         apply palfile to input byte file.
		 <infile>   A 1- or 3- banded LAS byte image.
		 <outfile>  a JPEG/JFIF image file.

DESCRIPTION:
	Las2jpeg  converts a 1- or 3- banded LAS byte image into a
       	JPEG image using either built-in palette files or  a  user
       	specified  palette  file.   JPEG  images  are usually much
       	smaller, and can be read in many other software  packages.
	Calls the jpeg library to JPEG compress it and write
	it out.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0                  O. Lawlor - Original Development
    1.5    12/03         P. Denny    Update commandline parsing
                                      Use meta 1.1 instead of DDR

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
****************************************************************/
/****************************************************************************
*								            *
*   las2jpeg -- reads a LAS file into RGB order in memory, and		    *
*		then calls the jpeg library to JPEG compress it and write   *
*		it out. 						    *
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
#include "asf_meta.h"
#include <sys/types.h>  /* for fstat function */
#include <sys/stat.h>   /* for fstat function */

#define BUF          256
#define VERSION      1.5
#define INC          5

#define zMASK     0x01
#define zXID      0x02
#define zUSER     0x04
#define zALL      0x07

#define zWRITE    0x10




int main (int argc, char *argv[])
{
  char infile[BUF];
  char outfile[BUF];
  char *outpal=NULL;
  unsigned char *out;
  RGBDATA table[256];
  unsigned char *band;
  FILE *fin, *fout;
  unsigned char flags = 0;
  int wid,len;
  int cnt, i;
  int x,y;
  meta_parameters *meta;
  struct stat fileInfo;

  /* Start time keeping*/
  StartWatch();
   
  /* Parse command line arguments */
  while (currArg < (argc-2)) {
     char *key = argv[currArg++];
     if (strmatch(key,"-mask")) {
       mask_colortable(table);
       flags |= zMASK;
     }
     else if (strmatch(key,"-pal")) {
	CHECK_ARG(1);
        user_colortable(table,GET_ARG(1));
        flags |= zUSER;
     }
     else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }
  if ((argc-currArg) < 2) {printf("Insufficient arguments.\n"); usage(argv[0]);}

  strcpy(infile,argv[currArg]);
  strcpy(outfile,argv[currArg+1]);

  /* Populate colortable in grayscale if no table has been specified */
  if ( !(flags & zALL) ) 
    grey_colortable(table);

  /* Get image dimensions */
  meta = meta_read(infile);
  wid = meta->general->sample_count;
  len = meta->general->line_count;
  meta_free(meta);

  /* malloc buffers, check and open files */
  band = (unsigned char *)MALLOC(wid * len * 3);
  out = (unsigned char *)MALLOC(len * wid * 3);
  fin = fopenImage(infile,"rb");
  fout = FOPEN(appendExt(outfile,".jpg"),"w");

  /* Hack to see if there are 3 bands, someday we'll use the .meta */
  fstat(fileno(fin), &fileInfo);
  if (fileInfo.st_size == 3*wid*len) {
    flags |= zXID;
  }

  /* Process data
   --------------*/
  /* RGB Color (3 bands) */
  if (flags & zXID) {
    FREAD(band,sizeof(unsigned char),3*wid*len,fin);
    for (cnt=0; cnt<3; cnt++) {
      for (y=0;y<len;y++)
         for (x=0;x<wid;x++)
 	    out[(y*wid+x)*3+cnt] = band[cnt*wid*len+y*wid+x];
    }
  }
  /* 1 band (Grayscale or apply a colortable to it) */
  else {
     FREAD(band,sizeof(unsigned char),len*wid,fin);
     for (x=0; x < len*wid; x++)
     {
	i=x*3;
        out[i  ] = table[band[x]].red;
        out[i+1] = table[band[x]].green;
        out[i+2] = table[band[x]].blue;
     }
  }
  
  if (flags & zWRITE) write_table(table,outpal);

  write_image(fout,out,wid,len);

  /* close output file & exit*/
  free(band);
  free(out);
  fclose(fin);
  fclose(fout);
  StopWatch();
  return(0);
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-mask] [-pal <file>] <infile> <outfile>",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   infile   A byte image in ASF tools format.\n"
	"   outfile  A JPEG/JFIF image file.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -mask        Input file is an unwrapped mask file.\n"
	"   -pal <file>  Apply palfile to input byte file.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts the given byte image into a JPEG file.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
