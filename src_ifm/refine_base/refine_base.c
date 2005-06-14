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
    1.5    12/03         P. Denny   - Bring commandline parsing to our standard
                                      Use meta 1.1 instead of DDR (genab.c)

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
	
****************************************************************************/

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

#define VERSION 1.5

/* Prototypes */
int getphase(char *phasein, char *tiept, char *outfile);
int genab(char *datafile, char *basefile, char *metaName, char *matfile,
          char *vecfile);
int bp(char *matfile, char *vecfile, char *newbase);
int test_base(char *basefile, char *matfile, char *vecfile);

static
void usage (char *name) {
 printf("\n"
 	"USAGE:\n"
	"   %s [-keep <iter>] [-log <file>] [-quiet]\n"
	"            <phase> <tie_points> <meta> <old_base> <new_base>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   phase        Unwrapped interferogram phase file\n"
	"   tie_points   Tie-point location file.\n"
	"   meta         Filename containing interferogram's metadata.\n"
	"   old_base     Baseline file containg four parameters:\n"
	"                  Bn delta_Bn Bp delta_Bp\n"
	"   new_base     Refined baseline file containg four parameters:\n"
	"                  Bn delta_Bn Bp delta_Bp\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -keep <iter>  Keep intermediate products.  Iterations=<iter>\n"
	"   -log <file>   Allows the output to be written to a log file\n"
	"   -quiet	  Suppresses the output to the essential\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Corrects a given baseline for an interferogram using tie points.\n");
 printf("\n"
	"Version: %.2f, ASF SAR Tools\n"
	"\n", VERSION);
 exit(EXIT_FAILURE);
}

int main(int argc,char *argv[])
{
  int iter=0, keepIter=0;
  char cmd[255], phase_file[255], tp_file[255], ctrlpt_file[255];
  char meta_file[255], oldbase_file[255], newbase_file[255], matrix_file[255], vec_file[255];

  /* Parse command line arguments */
  logflag=quietflag=FALSE;
  while (currArg < (argc-5)) {
     char *key = argv[currArg++];
     if (strmatch(key,"-keep")) {
        CHECK_ARG(1);
	iter = atoi(GET_ARG(1));
	keepIter=TRUE;
     }
     else if (strmatch(key,"-log")) {
        CHECK_ARG(1);
        strcpy(logFile,GET_ARG(1));
        fLog = FOPEN(logFile, "a");
        logflag=TRUE;
     }
     else if (strmatch(key,"-quiet")) {
        quietflag=TRUE;
     }
     else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }
  if ((argc-currArg) < 5) {printf("Insufficient arguments.\n"); usage(argv[0]);}

  /* take care of required command line arguments */
  strcpy(phase_file,   argv[currArg]);
  strcpy(tp_file,      argv[currArg+1]);
  strcpy(meta_file,    argv[currArg+2]); /* Do we need this argument?? */
  strcpy(oldbase_file, argv[currArg+3]);
  strcpy(newbase_file, argv[currArg+4]);

  printf("%s\n",date_time_stamp());
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

