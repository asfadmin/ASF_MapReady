/****************************************************************
NAME: convert2ppm - Convert ASF tools byte files to PPM format.

SYNOPSIS: convert2ppm [-mask] [-pal palfile] <in> <out> 

DESCRIPTION:
	Converts byte data into a PPM file using either built-in palette files
	or a user specified palette file.

	The PPM file format is more common than the ASF tools file format. You 
	can view PPM files with the program xv(1), or convert them to yet more 
	common  formats (gif, jpeg) using the NetPBM package.


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
    1.6     2/04         P. Denny     Change license to BSD, change name from
                                       convert2ppm to convert2ppm    

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
****************************************************************/
/******************************************************************************
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
#include "convert2ppm.h"
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
	"   <in>    Input file is a 1 or 3 banded ASF tools byte image.\n"
	"   <out>   Output file is a PPM image.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -mask           Input file is an unwrap mask file.\n"
	"   -pal <palfile>  Apply palfile to input byte file.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts 1- or 3-band ASF tools byte image into a PPM file.\n"
	"   A PPM file can be viewed with many graphics programs, such as xv(1).\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
