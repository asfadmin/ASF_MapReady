/****************************************************************
NAME: las2ppm - Convert LAS byte files to PPM format.

SYNOPSIS: las2ppm [-mask] [-pal palfile] <in> <out> 

DESCRIPTION:

	Las2ppm  converts  byte  data into a PPM file using either built-in
	palette files or a user specified palette file.

	The PPM file format is more common than the LAS file  for- mat  used 
	by  our tools.  You can view PPM files with the program xv(1), or
	convert them to yet more common  formats (gif, jpeg) using the NetPBM
	package.


EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0                  R. Fatland - Original Development
    1.1     11/95        M. Shindle - Cleaning, porting, and conversion into
			               a LAS 6.0 image.
    1.2     6/97         O. Lawlor  - Get width and length from ddr.
    1.3     6/98         O. Lawlor  - Automatically determine if 3-banded.
    1.5    12/03         P. Denny     Bring command line parsing to current
                                       standard. Use meta 1.1 instead of DDR.    

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
****************************************************************/
/****************************************************************************
*								            *
*   las2ppm - Convert LAS byte files to PPM format.			    *
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
#include "las2ppm.h"
#include "asf_meta.h"
#include <sys/types.h>  /* for fstat function */
#include <sys/stat.h>   /* for fstat function */


#define BUF          256
#define VERSION      1.5
#define INC          5

#define zMASK        0x01
#define zXID         0x02
#define zUSER        0x04
#define zWRITE       0x10

#define zALL         0x07

/* global & local function declaration */

int main (int argc, char *argv[])
{
  char infile[BUF];
  char outfile[BUF];
  char ppmbuf[BUF];
  char *outpal=NULL;
  RGBDATA *out, table[256];
  unsigned char *band;
  unsigned char *cptr;
  FILE *fin, *fout;
  unsigned char flags = 0;
  int wid,len;
  int cnt, data_read, i;
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
  if ( !(flags & zALL)) 
    grey_colortable(table);

  /* Get image dimensions */
  meta = meta_read(infile);
  wid = meta->general->sample_count;
  len = meta->general->line_count;
  meta_free(meta);

  /* open files */
  fin = FOPEN(infile,"rb");
  fout = FOPEN(outfile,"w");
  
  /* Hack to see if there are 3 bands; someday we'll be able to use the .meta */
  fstat(fileno(fin), &fileInfo);
  if (fileInfo.st_size == 3*wid*len) {
    flags |= zXID;
  }

  /* write out ppm header*/
  sprintf(ppmbuf,"P6 %d %d 255\n",wid,len);
  i = strlen(ppmbuf);
  fwrite(ppmbuf,sizeof(char),i,fout);
  
  /*
   * malloc buffers, check and open files 
   */
  if (flags & zXID || flags & zUSER) {
    band = (unsigned char *)MALLOC(wid * len * sizeof(unsigned char));
    out = (RGBDATA *)MALLOC(len * wid * sizeof(RGBDATA));
  } else {
    band = (unsigned char *)MALLOC(INC * wid * sizeof(unsigned char));
    out = (RGBDATA *)MALLOC(INC * wid * sizeof(RGBDATA));
  }

  /*
   * process data
   */
  if (flags & zXID) {
    cptr = (unsigned char *)out;
    for (cnt=0; cnt<3; cnt++) {
      data_read = FREAD(band,sizeof(unsigned char),wid*len,fin);
      for (i=0;i<wid*len;i++)
	*(cptr + sizeof(RGBDATA)*i + cnt) = band[i];
    }
    fwrite(out,sizeof(RGBDATA),wid*len,fout);
  } else { 
    for (cnt=0; cnt < len; cnt+=INC) {
      data_read = fread(band,sizeof(unsigned char),INC*wid,fin);
      for (i=0; i < data_read; i++)
        out[i] = table[band[i]];
      fwrite(out,sizeof(RGBDATA),data_read,fout);
    }
  }
 
  if (flags & zWRITE) write_table(table,outpal);

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
	"   %s [-mask] [-pal <palfile>] <in> <out>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <in>    Input file is a 1 or 3 banded LAS byte image.\n"
	"   <out>   Output file is a PPM image.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -mask           Input file is an unwrap mask file.\n"
	"   -pal <palfile>  Apply palfile to input byte file.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts 1- or 3-band LAS byte image into a PPM file. A PPM file can\n"
	"   be viewed with many graphics programs, such as xv(1).\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
