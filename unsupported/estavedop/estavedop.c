/******************************************************************************
NAME:	      estavedop - Estimates the average doppler for two input images

SYNOPSIS:     estavedop ifile1 ifile2 ofile

		ifile1, ifile2	- ASF CCSD Product pairs
		ofile		- Output file containing a single float value
				  that is the estimate of the average doppler

DESCRIPTION:  This program calculates the doppler centroid in the range
	      direction for the two scenes that are input and creates
	      an output file that contains a single floating point value
	      that is the doppler centroid estimate.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    estdop		estdop(file, nlines, &ret_dop)

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    ifile1.D, ifile1.L	Input ASF CCSD file pair
    ifile2.D, ifile2.L	Input ASF CCSD file pair
    ofile		Output value

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    2/97   T. Logan     Initial creation for tandem_aisp run
    1.1     7/97   O. Lawlor    Linear and quadratic doppler terms.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:	Howard Zebker (zebker@jakey.stanford.edu)

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   estavedop - Estimates the average doppler for two input images          *
*   Copyright (C) 1997  Alaska SAR Facility		   	    	    *
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
*   ASF APD Contacts:						 	    *
*	Rick Guritz				rguritz@asf.alaska.edu      *
*	Tom Logan				tlogan@asf.alaska.edu       *
* 									    *
*	Alaska SAR Facility			APD Web Site:	    	    *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
#include "asf.h"
#include "estdop.h"

#define VERSION 1.1

int main(int argc, char *argv[])
{
  char 		fileA[256], fileB[256], ofile[256],outLine[255];
  int 		nDopLines;
  float		dop1A,dop2A,dop3A, dop1B,dop2B,dop3B;
  FILE 		*fpo;

  if (argc != 4)
    {
     printf("\nUsage: %s ccsd_file1 ccsd_file2 ave_dop_file\n",argv[0]);
     printf("       ccsd_file1        Input CCSD file 1\n");
     printf("       ccsd_file2        Input CCSD file 2\n");
     printf("       ave_dop_file      Output Average Doppler file\n");
     printf("\nestavedop -- Estimates the average doppler for two input images.");
     printf("\nVersion %.2f,  ASF SAR Tools\n\n",VERSION);
     exit(1);
    }
 
  StartWatch();

  strcpy(fileA,argv[1]);
  strcpy(fileB,argv[2]);
  strcpy(ofile, argv[3]);

  nDopLines = 4000;

  estdop(fileA, nDopLines, &dop1A,&dop2A,&dop3A);
  estdop(fileB, nDopLines, &dop1B,&dop2B,&dop3B);

  sprintf(outLine,"%20.18f %20.18f %20.18f",(dop1A+dop1B)/2,(dop2A+dop2B)/2,(dop3A+dop3B)/2);
  system("date");
  printf("Program: estavedop\n\n");
  printf(" Doppler estimate:\n"
  	" Constant term      Linear term      Quadratic term\n"
  	"%s\n",outLine);
  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: estavedop\n\n");
    sprintf(logbuf,"   Doppler estimate:\n"
  	"   Constant term      Linear term      Quadratic term\n"
  	"%s\n",outLine);
    printLog(logbuf);
  }

  fpo = FOPEN(ofile,"w");
  fprintf(fpo,"%s\n",outLine);
  fclose(fpo);

  exit(0);
 }
