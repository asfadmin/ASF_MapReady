/******************************************************************************
NAME:	sarout - Convert ASF meta to ASF CEOS

SYNOPSIS: sarout m|c|x|d [-L inCeos] inMeta inImg outCeos

DESCRIPTION:

	Sarout converts images that are formatted in the STEP internal
        .meta style into ASF CEOS formatted products.  The inputs can
        be of type .raw, .cpx, or .img to create output products of type
        CCSD, SLC, or detected images, respectively (see modes list below).

    Modes:
        m       Convert .raw into CCSD metadata (.L) file only
        c       Convert .raw into CCSD
        x       Convert .cpx into COMPLEX
        d       Convert .img into DETECTED IMAGE


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    writeAsfCeosDatafile	Just what it says
    modifyAsfLeader		Just what is says	

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    inCeos.L		Input leader file to use
    inMera.meta		input meta file
    inImg.img,.ddr	input image file
    outCeos.D,.L	output CEOS image

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    2/99   T. Logan	Create CEOS products
    1.01    7/99   T. Logan     Fixes for ceos_misc and modifyLeader
    1.2     3/02   P. Denny     Update command line parsing, prototyping

HARDWARE/SOFTWARE LIMITATIONS:
	This version of sarout does not fill all of the fields in the CEOS
        product.  Check the readme file to get a full list of the invalid
        fields.

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   sarout - Convert ASF meta to ASF CEOS				    *
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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "asf.h"
#include "asf_meta.h"
#include "ceos.h"
#include "ddr.h"
#include "const.h"
#include "sarout.h"

#define VERSION 1.2
void readLeader(char *, int, ceosLeader *);
void createLeader(ceosLeader *, char *);
void createCeosFromSignal(meta_parameters *, char *,ceosLeader *);
void create_mock_ddr(char *inFile, struct DDR *ddr,meta_parameters *meta);
void addMetaToCeos(meta_parameters *, struct DDR *, ceosLeader *, int mode);
void getSignalFormat(char *,int *,int *,float *,float *,char *,float *);
void writeAsfCeosLeader(int, ceosLeader *, char *);
void writeAsfCeosData(int,ceosLeader *,struct DDR *,
		meta_parameters *,char *,char *);
void writeAsfCeosSignal(ceosLeader *,meta_parameters *,char *,char *,int );
void modifyLeader(int,int,ceosLeader *,struct DDR *ddr, meta_parameters *meta);
void addSensorFields(int,char *,char *, ceosLeader *);
void usage(char *prgName);

int main(int argc,char *argv[])
{
   char inMetaFile[256],inCeosFile[256],inImgFile[256],outCeosFile[256];
   meta_parameters  *meta;
   ceosLeader       outCeos;
   struct DDR       ddr;         /* ddr structure                   */
   int  	    inputCeos=0;
   int 		    leaderOnly=0;
   int 		    mode;
   extern int       optind;      /* argv index of the next argument */
   extern char     *optarg;      /* current argv[]    	            */
   int              c;           /* option letter from getopt()     */

   StartWatch();

   /* Parse cla's
    ------------*/
   while ((c=getopt(argc,argv,"L:")) != EOF)
   {
      switch (c) {
      	 case 'L':
	    inputCeos=1;
	    strcpy(inCeosFile,optarg);
      	    break;
      	 default:
      	    usage(argv[0]);
      	    break;	
      }
   }
   if ((argc-optind) != 4)
   {
      if ((argc-optind) > 4) printf("Too many arguments.\n");
      if ((argc-optind) < 4) printf("Insufficient arguments.\n");
      usage(argv[0]);
   }

   switch(argv[optind][0])
   {
      case 'm': case 'M': leaderOnly = 1; mode = CEOS_CCSD; break;
      case 'c': case 'C': mode = CEOS_CCSD; break;
      case 'x': case 'X': mode = CEOS_SIC; break;
      case 'd': case 'D': mode = CEOS_LOW; break;
      default:  printf("Unknown mode %c\n",argv[optind][0]); usage(argv[0]);
   }		
   strcpy(inMetaFile, argv[++optind]);
   strcpy(inImgFile,  argv[++optind]);
   strcpy(outCeosFile,argv[++optind]);
   
   printf("==================================\n");
   printf(" ASF PRODUCT GENERATION TOOL\n");
   printf("==================================\n");

   printf("\nCreating ASF ");
   if (mode == CEOS_CCSD) printf("CCSD product\n");
   else if (mode == CEOS_SIC) printf("COMPLEX product\n");
   else if (mode == CEOS_LOW) printf("DETECTED DATA product\n");

   /* Read metadata files into memory
    --------------------------------*/
   meta=meta_init(inMetaFile);
   if (meta->info == NULL) { printf("No meta file found\n"); exit(1);}

   if (mode != CEOS_CCSD) c_getddr(inImgFile, &ddr);
   else create_mock_ddr(inImgFile,&ddr,meta);

   /* Establish initial leader file records 
    --------------------------------------*/
   if (inputCeos) readLeader(inCeosFile,mode,&outCeos);
   else 	  createLeader(&outCeos, outCeosFile);	
 
   /* Modify leader records based on meta & ddr file values
    ------------------------------------------------------*/
   addMetaToCeos(meta,&ddr,&outCeos, mode);
   if(radio_fill(&(outCeos.raddr),inMetaFile)==-1) 
   printf("There is a problem with the .ant or .noise file\n");
   if (mode == CEOS_CCSD) createCeosFromSignal(meta, inImgFile, &outCeos);
   printf("\nMeta file added to leader records\n");

   /* Fill Out Sensor/Mode based constants 
    -------------------------------------*/
   addSensorFields(mode,meta->info->sensor,meta->info->mode,&outCeos);

   /* Modify leader as appropriate to output type
    --------------------------------------------*/
   modifyLeader(mode,inputCeos,&outCeos,&ddr,meta);

   /* Create and write the data file & calculate histogram
    -----------------------------------------------------*/
   if (mode==CEOS_CCSD)
	writeAsfCeosSignal(&outCeos,meta,inImgFile,outCeosFile,leaderOnly);
   else writeAsfCeosData(mode,&outCeos,&ddr,meta,inImgFile,outCeosFile);

   /* Write out the leader file 
    --------------------------*/
   writeAsfCeosLeader(mode,&outCeos,outCeosFile);

   StopWatch();
   return 0;
}

void create_mock_ddr(char *inFile, struct DDR *ddr,meta_parameters *meta)
{
  int    inbytes, nhead;
  float  xmi, xmq, dwp;
  char   iqflip;
  char   infile[256];
  FILE   *fpi;

  getSignalFormat(inFile, &inbytes, &nhead, &xmi, &xmq, &iqflip,&dwp);

  ddr->ns = (inbytes-nhead) / 2;

  strcat(strcpy(infile,inFile),".raw");
  fpi = FOPEN(infile,"rb");
  fseek(fpi,0,SEEK_END);
  ddr->nl = ftell(fpi)/inbytes;
  FCLOSE(fpi);

  ddr->master_line = 1;
  ddr->master_sample = 1;
  ddr->pdist_x = meta->geo->xPix;
  ddr->pdist_y = meta->geo->yPix;
} 

void usage(char *prgName)
{
  printf("\n"
         "USAGE:\n"
	 "   %s [-L inCeos] <m|c|x|d> <inMeta> <inImg> <outCeos>\n",prgName);
  printf("\n"
      	 "REQUIRED ARGUMENTS:\n"
	 "   Mode:\n"
	 "      m      - Convert .raw into CCSD metadata (.L) file\n"
      	 "      c      - Convert .raw into CCSD\n"
      	 "      x      - Convert .cpx into COMPLEX\n"
      	 "      d      - Convert .img into DETECTED IMAGE\n"
	 "   inMeta    - .meta file name\n"
	 "   inImg     - Data file name (.raw, .cpx, .img)\n"
	 "   outCeos   - Name of output file\n");
  printf("\n"
	 "OPTIONS:\n"
	 "   -L inCeos - Modified CEOS file with all original information.\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   Converts input data into CEOS formatted products.\n");
  printf("\n"
	 "Version %.2f, ASF SAR TOOLS\n\n",VERSION);
  exit(1);
}
