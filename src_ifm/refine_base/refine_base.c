/*************************************************************************
 NAME:  refine_base

 SYNOPSIS:  refine_base [-k __iter__ ] [-log <file>] [-quiet] 
                        <phase> <tie_points> <meta> <old_base> <new_base>

  -k __iter__    Keep intermediate products.  Iterations=__iter__
  -log <file>	 Allows the output to be written to a log file
  -quiet	 Suppresses the output to the essential

       <phase>   unwrapped interferogram phase file" ;
  <tie_points>   tie-point location file.";
        <meta>   filename containing interferogram's metadata.";
    <old_base>   baseline file containg four parameters:" ;
                 Bn delta_Bn Bp delta_Bp" ;
    <new_base>   refined baseline file containg four parameters:" ;
                 Bn delta_Bn Bp delta_Bp" ;

 DESCRIPTION:
 
     Perform baseline refinement using a set of 4 functions: getphase, genab,
     bp, and test_base. The last one, test_base, does not refine the
     baseline at all. Its only purpose is to check the validity of the new
     baseline.

     Getphase extracts phase values from an associated tie point file. 

     Genab creates a matrix (A) and a vector (b) that are related by the
     equation Ax=b. 

     Bp solves the above equation in which the vector x is our new baseline
     values.


 EXTERNAL:

	bp


 PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0	    11/96	 M. Shindle - Original program
    1.1	    8/97	 O. Lawlor  - Takes Parameters from metadata & ddr
    1.2	    7/01	 R. Gens    - Conversion of script into program
				      Included external programs as functions 
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
	
*****************************************************************************
								            *
   refine_base - perform baseline refinement.				    *
   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
									    *
   This program is free software; you can redistribute it and/or modify     *
   it under the terms of the GNU General Public License as published by     *
   the Free Software Foundation; either version 2 of the License, or        *
   (at your option) any later version.					    *
									    *
   This program is distributed in the hope that it will be useful,	    *
   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
   GNU General Public License for more details.  (See the file LICENSE      *
   included in the asf_tools/ directory).				    *
									    *
   You should have received a copy of the GNU General Public License        *
   along with this program; if not, write to the Free Software		    *
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.                *
									    *
       ASF Advanced Product Development LAB Contacts:			    *
	APD E-mail:	apd@asf.alaska.edu 				    *
 									    *
	Alaska SAR Facility			APD Web Site:	            *	
	Geophysical Institute			www.asf.alaska.edu/apd	    *
      	University of Alaska Fairbanks					    *
	P.O. Box 757320							    *
	Fairbanks, AK 99775-7320					    *
								  	    *
*****************************************************************************/

#include "asf.h"

#define VERSION 1.2

void print_usage();

int main(int argc,char *argv[])
{
  int iter=0, keepIter=0, i, optind=1;
  char cmd[255], phase_file[255], tp_file[255], ctrlpt_file[255];
  char meta_file[255], oldbase_file[255], newbase_file[255], matrix_file[255], vec_file[255];

  if (argc==1 || argc>11) print_usage();

  logflag=quietflag=0;

  /* take care of the options */
  for (i=1; i<argc; i++) {
    if (strncmp(argv[i], "-k", 2)==0) {
      iter=atoi(argv[i+1]);
      keepIter=1;
      i+=1;
      optind+=2;
    }
    else if (strncmp(argv[i], "-log", 4)==0) {
      sprintf(logFile, "%s", argv[i+1]);
      fLog = FOPEN(logFile, "a");
      logflag=1;
      i+=1;
      optind+=2;
    } 
    else if (strncmp(argv[i], "-quiet", 6)==0) {
      quietflag=1;
      optind+=1;
    }
    else if (strncmp(argv[i], "-", 1)==0) {
      sprintf(errbuf, "   ERROR: %s is not a valid option!\n", argv[i]);
      printErr(errbuf);
    }
  }

  /* take care of required command line arguments */
  i=optind;
  sprintf(phase_file, "%s", argv[i]);
  sprintf(tp_file, "%s", argv[i+1]);
  sprintf(meta_file, "%s", argv[i+2]);
  sprintf(oldbase_file, "%s", argv[i+3]);
  sprintf(newbase_file, "%s", argv[i+4]);

  system("date");
  printf("Program: refine_base\n\n");
  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: refine_base\n\n");
  }

  sprintf(ctrlpt_file, "ctrlpts.%d", iter);
  sprintf(matrix_file, "matrix.%d", iter);
  sprintf(vec_file, "vecB.%d", iter);

  /* get phases from unwrapped phase file */
  getphase(phase_file, tp_file, ctrlpt_file);

  /* generate matricies from phases & tie points*/
  genab(ctrlpt_file, oldbase_file, meta_file, matrix_file, vec_file);
 
  /* create baseline and test it */
  bp(matrix_file, vec_file, newbase_file);
  test_base(newbase_file, matrix_file, vec_file);

  /* remove intermediate files unless requested not to */
  if (keepIter==0) {
    sprintf(cmd, "rm %s %s %s", ctrlpt_file, matrix_file, vec_file);
    system(cmd);
  }
  
  return(0);
}

void print_usage () {
      	printf("Usage: refine_base [-k __iter__] [-log <file>] [-quiet] "
	       "          phase tie_points meta old_base new_base\n");
      	printf("  phase         unwrapped interferogram phase file\n");
      	printf("  tie_points    tie-point location file.\n");
      	printf("  meta          filename containing interferogram's metadata.\n");
      	printf("  old_base      baseline file containg four parameters:\n");
      	printf("                Bn delta_Bn Bp delta_Bp\n");
      	printf("  new_base      refined baseline file containg four parameters:\n");
      	printf("                Bn delta_Bn Bp delta_Bp\n");
      	printf("  -k __iter__   Keep intermediate products.  Iterations=__iter__\n");
	printf("  -log <file>   Allows the output to be written to a log file\n");
	printf("  -quiet	Suppresses the output to the essential\n");
      	printf("Corrects a given baseline for an interferogram using tie points.\n");
        printf("Version: %.2f, ASF SAR TOOLS\n", VERSION);
        exit(1);
}


