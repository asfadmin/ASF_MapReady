/****************************************************************
NAME: tpl_mult.c - Multiplies a tie point file by given factor

SYNOPSIS:  tpl_mult fact intpl outtpl

DESCRIPTION:

	Multiplies all of the points in the tie point file intpl
	by the factor, fact, to produce the output tie point file,
	outtpl.

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    intpl.tpl		 Input tie points
    outtpl.tpl		 Scaled tie points

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.1	    7/97   T. Logan     2 Pass correlation

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
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/

#include "asf.h"

#define  VERSION	1.1

main(argc,argv)
  int argc; char **argv;
{
    float rx,ry,sx,sy, fact;
    FILE  *fpin, *fpout;  
    char  ifile[255], ofile[255];

    if (argc != 4) give_usage();

    sscanf(argv[1],"%f",&fact);
    strcat(strcpy(ifile,argv[2]),".tpl");
    strcat(strcpy(ofile,argv[3]),".tpl");

    if ((fpin = fopen(ifile,"r")) == NULL)
      { printf("tpl_mult:  Unable to open input file %s\n",ifile); exit(-1); }
    if ((fpout = fopen(ofile,"w")) == NULL)
      { printf("tpl_mult:  Unable to open output file %s\n",ofile); exit(-1); }

    while (fscanf(fpin,"%f %f %f %f",&rx,&ry,&sx,&sy) != EOF) {
       rx *= fact; ry *= fact; sx *= fact; sy *= fact;
       fprintf(fpout,"%f %f %f %f\n",rx,ry,sx,sy);
    }

    fclose(fpin); fclose(fpout);
    printf("tpl_mult exited sucessfully\n"); 
    exit(0);
}

give_usage() {
   printf("Usage:  tpl_mult factor intpl outtpl\n");
   printf("           factor      floating point multiplication factor\n");
   printf("           intpl       input tie point location file\n");
   printf("           outtpl      output tie point location file\n");
   printf("\n  ASF STEP TOOLS, Version %.2f\n\n",VERSION);
   exit(-1);
} 

 
