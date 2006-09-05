/****************************************************************
NAME: convert2jpeg

SYNOPSIS: convert2jpeg [-m | -p palfile] <infile> <outfile>

		 -m         input file is an unwrapped mask file.
		 -p         apply palfile to input byte file.
		 <infile>   A 1- or 3- banded ASF tools byte image.
		 <outfile>  a JPEG/JFIF image file.

DESCRIPTION:
	Converts a 1- or 3- banded ASF tools byte image into a
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
    VERS:   DATE:  AUTHOR:     PURPOSE:
    ---------------------------------------------------------------
    1.0            O. Lawlor   Original Development
    1.5    12/03   P. Denny    Update commandline parsing
                                 Use meta 1.1 instead of DDR
    1.6     2/04   P. Denny    Change name from las2jpeg to convert2jpeg
                                 Change license from GPL to BSD

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
****************************************************************/
/******************************************************************************
*                                                                             *
*   convert2jpeg: reads an ASF tools image file into RGB order in memory, and *
*            then calls the jpeg library to JPEG compress it and write it out *
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


unsigned char no_change(unsigned char x)
{
    return x;
}

unsigned char brighten(unsigned char x)
{
    return (unsigned char) (sqrt((double)x/255.0)*255.0);
}


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
  unsigned char (*scale) (unsigned char);
  scale = no_change;

  /* Start time keeping*/
  StartWatch();
   
  /* Parse command line arguments */
  while (currArg < (argc-2)) {
     char *key = argv[currArg++];
     if (strmatch(key,"-mask")) {
       mask_colortable(table);
       flags |= zMASK;
     }
     else if (strmatch(key,"-brighten")) {
       scale = brighten;
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
             out[(y*wid+x)*3+cnt] = scale(band[cnt*wid*len+y*wid+x]);
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
	"   -brighten    Apply a simple brightening filter.\n"
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
