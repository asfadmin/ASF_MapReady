/******************************************************************
NAME:    airsarin.c
  
SYNOPSIS: airsarin sar_infile las_outfile
  
DESCRIPTION:
     This program reads AIRSAR images and converts them to LAS 6.0 
     DAL images.  The program takes any AIRSAR input file, and 
     creates a LAS .img output file with a .ddr metadata file.  After 
     the image is ingested, the LAS image can be geocoded using the
     program airsargeo
  
PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
1.0		3/01	J. Badgley  (ASF)  airsarin is created using
					   the original sarin (v 5.0) as 
					   a template
1.1		6/01	T. Logan	   Modified subroutines to make
					   more reliable
*******************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the   
University of Alaska Technology Corporation, below.


                        Legalese:
                 COPYRIGHT NOTIFICATION
  
(C) COPYRIGHT 2001 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED
                                        
This software discloses material protectable under copyright
laws of the United States. Permission is hereby granted to
use, reproduce, and prepare derivative works for noncommercial
purposes at no charge, provided that this original copyright
notice, and disclaimer are retained and any changes are
clearly documented. Any entity desiring permission to
incorporate this software or a work based on the software
into a product for sale must contact the University of
Alaska Technology Corporation.
                                        
  
This software was authored by:

Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195

Any questions or comments on the software may be directed
to one of the authors: Rick Guritz, Tom Logan, Mike Shindle,
Rob Fatland, Orion Lawlor, and Dorothy Corbett; or to
http://www.images.alaska.edu
                        
                 
NEITHER THE UNIVERSITY OF ALASKA NOR ANY SUBUNIT THEREOF,
NOR ANY OF THEIR EMPLOYEES MAKES ANY WARRANTY, EXPRESS
OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR
RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR
USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR
PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD
NOT INFRINGE PRIVATELY OWNED RIGHTS.
LICENSING INQUIRES MAY BE DIRECTED TO THE UNIVERSITY   
OF ALASKA TECHNOLOGY DEVELOPMENT CORPORATION AT (907)451-0718.
************************************************************/
#include "asf.h"
#include "airsar_io.h"
#include "asf_meta.h"

#define VERSION 1.1

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

  struct DDR ddr;		/* Data Record file			*/

  StartWatch();
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
  airsar2ddr(airsarname, &ddr);
  fpOut = fopenImage(outname, "wb");
  stati = c_putddr(outname, &ddr);

  if (stati != E_SUCC)
  {
	printf("Error returned from putddr\n");
  }

  ns = ddr.ns;
  nl = ddr.nl;

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

  fpIn = fopen(airsarname, "r");

/* Read input file, convert, and write to output file */
  for (y = 0; y< nl; y++)
  {
	readAirSARLine(fpIn, ibuff, hb, lb, y, &ddr);

	if( ddr.dtype==DTYPE_BYTE) {
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
		putFloatLine(fpOut, &ddr, y, obuff);
	}
	if ((y % 100) == 0)
		printf("Now Processing Line No = %d \n", y);
  }

      printf("Version 1.1\n");


  FCLOSE(fpOut);
  FCLOSE(fpIn);
  printf("Finished!\n");

  StopWatch();
  return 0;
}

