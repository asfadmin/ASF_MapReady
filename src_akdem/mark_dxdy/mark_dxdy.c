/******************************************************************************
NAME: mark_dxdy - Annotate a LAS image with markers at tie point locations

SYNOPSIS:  mark_dxdy infile seedfile outfile

DESCRIPTION:
  At each location (seedfile), a marker (arrow) is combined with the input
  file (infile) to create the output file (outfile). 

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    concatm		Concatenates LAS images based on user specified 
			starting lines and samples.

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    infile.ddr		Metadata (image size)
    infile.img		Input image data - used by concatm

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    5/01   R. Gens	Created an arrow representing dx and dy instead of cross

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:
  Determine the size of the input (output) file
    calculate start line and start sample for cross placement
  execute a single concatm call to create the output image 

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute 
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the 
University of Alaska Technology Corporation, below.


                        Legalese:
                 COPYRIGHT NOTIFICATION

(C) COPYRIGHT 1997 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

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
k************************************************************/
#include "asf.h"
#include "ddr.h"
#define  DXDY 1
#define  FICO 2
#define  VERSION 1.0

void give_usage(char *name);
int make_dxdy (char *name, int dx, int dy);
 
int main(argc,argv)
  int    argc;
  char   *argv[];
{
  FILE   *fps, *fpc;
  char   ifile[256], sfile[256], cfile[256], ofile[256];
  char   cmd[256];
  struct DDR in_ddr,arrow_ddr;
  int    inl, ins;		/* Input file # lines & # samples       */
  int    cnl, cns;		/* Arrow #line,#samp  */
  int  	 line, sample, dx, dy, offx, offy;
  int    i, channel, mode;
  int    ntpl;
  float  x2, y2, snr, fx, fy;

  /* Command line arguments */
  if (argc < 6) give_usage(argv[0]);

  if (strcmp(argv[1], "d") == 0) {
    mode = DXDY;
    strcpy(ifile,argv[2]);
    strcpy(sfile,argv[3]);
    strcpy(ofile,argv[4]);
    channel = atoi(argv[5]);
  }
  else if (strcmp(argv[1], "f") == 0) {
    mode = FICO;
    strcpy(ifile,argv[2]);
    strcpy(sfile,argv[3]);
    strcpy(cfile,argv[4]);
    strcpy(ofile,argv[5]);
    channel = atoi(argv[6]);
  }
  else give_usage(argv[0]);

  /* Determine the size of input/output files */
  fps = fopen(sfile,"r");
  if (fps==NULL){fprintf(stderr,"Error opening seed file %s\n",sfile); exit(1);}

  i = c_getddr(ifile, &in_ddr);
  inl = in_ddr.nl;
  ins = in_ddr.ns;

  /* Create arrow mask and arrow shadow image */
  sprintf(cmd,"cp %s.img mask.img", ifile);
  system(cmd);
  sprintf(cmd,"cp %s.ddr mask.ddr", ifile);
  system(cmd);
  sprintf(cmd,"cp %s.img shadow.img", ifile);
  system(cmd);
  sprintf(cmd,"cp %s.ddr shadow.ddr", ifile);
  system(cmd);

  /* Reading tie point file */
  printf(" Reading tie point locations from %s\n",sfile);
  ntpl = 0;

  if (mode == DXDY)
   while (fscanf(fps,"%i %i %i %i\n",&sample,&line,&dx,&dy) != EOF)
    {
	if (dx == 0 && dy == 0) continue;

       sample=(int)(sample-in_ddr.master_sample)/in_ddr.sample_inc;
       line=(int)(line-in_ddr.master_line)/in_ddr.line_inc;

       /* Create arrow from dx and dy values */
       make_arrow("arrow", dx, dy);

       /* Determine the size of the arrow */
	i = c_getddr("arrow", &arrow_ddr);
	cnl = arrow_ddr.nl;
	cns = arrow_ddr.ns;
	sample = (int) sample-(cns-1)/2;
	line = (int) line-(cnl-1)/2;

       /* Add arrow to mask image */ 
       printf("concatm temp %i %i mask 1 1 arrow %i %i\n\n",inl, ins, line, sample);
       sprintf(cmd,"concatm temp %i %i mask 1 1 arrow %i %i",inl, ins, line, sample);
       system(cmd);

       system("mv temp.img mask.img");
       system("mv temp.ddr mask.ddr");

       /* Add inverted arrow to shadow image */ 
       printf("concatm -m 255 temp %i %i shadow 1 1 arrow2 %i %i\n\n",inl, ins, line, sample);
       sprintf(cmd,"concatm -m 255 temp %i %i shadow 1 1 arrow2 %i %i",inl, ins, line, sample);
       system(cmd);

       system("mv temp.img shadow.img");
       system("mv temp.ddr shadow.ddr");

       ntpl++;
    }

  else if (mode == FICO) {
    fpc = fopen(cfile,"r");
    if (fpc==NULL){fprintf(stderr,"Error opening fico control file %s\n",cfile); exit(1);}
    fscanf(fpc,"%i\n%i\n",&offx,&offy);
    close(fpc);
  
   while (fscanf(fps,"%i %i %f %f %f\n",&sample,&line,&x2,&y2,&snr) != EOF)
    {
	fx = (sample-x2-offx)/in_ddr.sample_inc;
        if (fx > 0) dx = (int) (fx+0.5);
        if (fx < 0) dx = (int) (fx-0.5);
	fy = (line-y2-offy)/in_ddr.line_inc;
        if (fy > 0) dy = (int) (fy+0.5);
        if (fy < 0) dy = (int) (fy-0.5);
	if (dx == 0 && dy == 0) continue;

       sample=(int)(sample-in_ddr.master_sample)/in_ddr.sample_inc;
       line=(int)(line-in_ddr.master_line)/in_ddr.line_inc;

       /* Create arrow from dx and dy values */
       make_arrow("arrow", dx, dy);

       /* Determine the size of the arrow */
	i = c_getddr("arrow", &arrow_ddr);
	cnl = arrow_ddr.nl;
	cns = arrow_ddr.ns;
	sample = (int) sample-(cns-1)/2;
	line = (int) line-(cnl-1)/2;

       /* Add arrow to mask image */ 
       printf("concatm temp %i %i mask 1 1 arrow %i %i\n\n",inl, ins, line, sample);
       sprintf(cmd,"concatm temp %i %i mask 1 1 arrow %i %i",inl, ins, line, sample);
       system(cmd);

       system("mv temp.img mask.img");
       system("mv temp.ddr mask.ddr");

       /* Add inverted arrow to shadow image */ 
       printf("concatm -m 255 temp %i %i shadow 1 1 arrow2 %i %i\n\n",inl, ins, line, sample);
       sprintf(cmd,"concatm -m 255 temp %i %i shadow 1 1 arrow2 %i %i",inl, ins, line, sample);
       system(cmd);

       system("mv temp.img shadow.img");
       system("mv temp.ddr shadow.ddr");

       ntpl++;
    }
  }

  switch (channel) {
  
    /* Creating output image with arrows in red channel */
    case 1:
    sprintf(cmd,"concatm -c %s %i %i mask 1 1 shadow 1 1 shadow 1 1", ofile, inl, ins);
    system(cmd);
    break;

    /* Creating output image with arrows in green channel */
    case 2:
    sprintf(cmd,"concatm -c %s %i %i shadow 1 1 mask 1 1 shadow 1 1", ofile, inl, ins);
    system(cmd);
    break;

    /* Creating output image with arrows in blue channel */
    case 3:
    sprintf(cmd,"concatm -c %s %i %i shadow 1 1 shadow 1 1 mask 1 1", ofile, inl, ins);
    system(cmd);
    break;

  }

  /* Cleaning up the place */
  system("rm arrow.img arrow.ddr arrow2.img arrow2.ddr mask.img mask.ddr shadow.img shadow.ddr");

  printf("MARK_DXDY COMPLETED: ADDED %i ARROWS\n\n",ntpl);

  exit(1);
}

void give_usage(char *name)
 {
  printf("\nUsage: %s d infile tplfile outfile channel\n",name);
  printf("       %s f infile fico ctrl outfile channel\n",name);
  printf("    tplswitch - d: tplfile containing sample line dx dy\n");
  printf("                f: fico output file format\n");
  printf("    infile  - LAS image to be marked\n");
  printf("    tplfile - input tie point file\n");
  printf("    fico    - fico output file\n");
  printf("    ctrl    - fico control file\n");
  printf("    outfile - LAS image with marker at given locations\n");
  printf("    channel - color of arrows (red=1, green=2, blue=3)\n");
  printf(" ASF Step Tools, Version %0.2f\n\n",VERSION);
  exit(1);
 }
