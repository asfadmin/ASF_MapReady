/******************************************************************
NAME:    airsarin.c
  
SYNOPSIS: airsarin sar_infile outfile
  
DESCRIPTION:
     This program reads AIRSAR images and converts them to images.  
     The program takes any AIRSAR input file, and 
     creates a .img output file with a .meta metadata file.  After 
     the image is ingested, the image can be geocoded using the
     program airsargeo
  
PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
1.0		3/01	J. Badgley  (ASF)  airsarin is created using
					   the original sarin (v 5.0) as 
					   a template
1.1		6/01	T. Logan	   Modified subroutines to make
					   more reliable
1.2             7/05    R. Gens            Removed DDR dependencies.

*******************************************************************************
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
#include "airsar_io.h"
#include "asf_meta.h"

#define VERSION 1.2

int main(int argc, char *argv[])
{
  char basename[256];
  char *airsarname;
  char *outname;
  char *ext;
  char *off_char;
  char *buf;

  int *ibuff;			/* Input and output buffers		*/
  float *obuff;
  unsigned char *obuff_char;

  FILE *fpOut;			
  FILE *fpIn;

  int nl, ns;			/* Number of lines, number of samples	*/
  int y, x;			/* Counters				*/
  int hb, lb;			/* Header Bytes, Line Bytes		*/
  int stati;			/* error message return			*/
  int demi2=0;			/* Is this a DEM?			*/

  float incr=1;
  float offset=0;

  meta_parameters *meta;

  if(argc != 3){
    printf("Usage:	%s inSARfile outLASfile\n",argv[0]);
    printf("\tinput: inSARfile, an AirSAR image\n");
    printf("\toutput: outLASfile, a LAS image (.img and .ddr)\n");

    printf("\nAirsarin reads an AirSAR image, and\n");
    printf("converts it to the format used by our \n");
    printf("other tools.\n");
    printf("This is often the first step in analysing a SAR image.\n");
    printf("\nVersion %.2f, ASF STEP Tools\n",VERSION);
    exit(1);
  }

  airsarname=argv[1];
  outname=argv[2];

/* Strip the extension off for the basename */
  strcpy(basename, airsarname);
  ext = strchr(basename, '.');

  if(!strcmp(".demi2",ext)) demi2=1;
  *ext = '\0';
  if(demi2==1) printf("It's a DEM!\n");
  else printf("Hrmmm....\n");

/* Create the output ddr */
  meta = raw_init();
  airsar2meta(airsarname, meta);
  if (demi2)
    meta->general->image_data_type = DEM;
  else
    meta->general->image_data_type = AMPLITUDE_IMAGE;
  meta_write(meta, outname);
  fpOut = fopenImage(outname, "wb");
  ns = meta->general->sample_count;
  nl = meta->general->line_count;

/* Allocate buffer space */
  ibuff = (int *) malloc(ns * sizeof(int));
  obuff = (float *)malloc(ns * sizeof(float));
  obuff_char =(unsigned char*)malloc(ns * sizeof(unsigned char));

/* Determine the headerBytes and the length of each line */
  buf = get_airsar(airsarname, "FIRST", "BYTE OFFSET OF FIRST DATA RECORD");
  hb = atoi(buf);
  free(buf);
  buf = get_airsar(airsarname, "FIRST", "RECORD LENGTH IN BYTES");
  lb = atoi(buf);
  free(buf);

  if(demi2){
    off_char = (char *)malloc(sizeof(char));
    buf = get_airsar(airsarname, "DEM", "ELEVATION OFFSET");
    strcpy(off_char,buf);
    free(buf);
    offset = atof(off_char); 
    buf = get_airsar(airsarname,"DEM","ELEVATION INCREMENT");
    incr = atof(buf);
    free(buf);
    printf("offset should be = \t%s\n", off_char);
    printf("offset = \t\t%f\n", offset);
    printf("increment = \t\t%f\n", incr);
  }

  printf("\nBeginning IMG conversion...\n");

  fpIn = FOPEN(airsarname, "r");

/* Read input file, convert, and write to output file */
  for (y = 0; y< nl; y++)
  {
    readAirSARLine(fpIn, ibuff, hb, lb, y, meta);
    
    if( meta->general->data_type == BYTE) {
      for (x = 0; x< ns; x++)
	obuff_char[x]=(unsigned char)ibuff[x];
      FWRITE(obuff_char, ns, 1, fpOut);
    }
    else {
      for (x = 0; x < ns; x++){
	obuff[x]=(float)ibuff[x];
	
	if(obuff[x]!=49152)obuff[x]=obuff[x]*incr+offset;
	else obuff[x]=0;
      }
      put_float_line(fpOut, meta, y, obuff);
    }
    if ((y % 500) == 0)
      printf("Now Processing Line No = %d \n", y);
  }

  FCLOSE(fpOut);
  FCLOSE(fpIn);

  return 0;
}

