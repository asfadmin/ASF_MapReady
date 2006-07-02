/******************************************************************************
NAME: asf_igram_coh - Calculates an interferogram, a coherence image and
                      multilooks interfergram

SYNOPSIS: asf_igram_coh [-look linexsamp] [-step linexsample]
             <master> <slave> <output>

        -look   Set look box line and sample.  Default 15x3
        -step   Set step boc line and sample.  Default 5x1
        master  Complex master image
        slave   Complex slave image
        output  Basename of the output files

DESCRIPTION:

     Calculates the coherence for the interferogram using the following
     formula:
 
		|         Sum (n pixels) [ a x b* ] 	     |
         rho =  | __________________________________________ |
		|					     |
		|  Sqrt { Sum  [ a x a* ]  Sum  [ b x b* ] } |

 	the 'x' in the equation above indicates multiplication and
	the '*' indicates complex conjugation.

******************************************************************************/

/******************************************************************************
*								              *
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

#include "asf_igram_coh.h"

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-look lxs] [-step lxs] <master> <slave> <output>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   master   Complex master image\n"
	"   slave    Coregistered complex slave image\n"
	"   output   Basename of the output image\n\n");
 printf("OPTIONAL ARGUMENTS:\n"
	"   -look lxs   Change look box (l)ine and (s)ample.\n"
	"               (Read from meta file by default)\n"
	"   -step lxs   change step box (l)ine and (s)ample.\n"
	"               (Read from meta file by default)\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   A correlation calculator to estimate interferogram quality\n");
 printf("\n"
	"Version %4.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  char masterFile[255], slaveFile[255], outFile[255];
  int sample_count, line_count;
  int stepLine, stepSample, lookLine, lookSample, lookFlag=FALSE, stepFlag=FALSE;
  meta_parameters *inMeta;

  logflag = 0;

  /* parse command line */
  while (currArg < (argc-3)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-log")) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = 1;
    }
    else if (strmatch(key,"-look")) {
      CHECK_ARG(1)
      if (2!=sscanf(GET_ARG(1),"%dx%d", &lookLine, &lookSample)) {
	printf("   **ERROR: -look '%s' does not look like line x sample "
	       "(e.g. '10x2').\n",GET_ARG(1));
	usage(argv[0]);
      }
      lookFlag=TRUE;
    }
    else if (strmatch(key,"-step")) {
      CHECK_ARG(1)
      if (2!=sscanf(GET_ARG(1),"%dx%d",&stepLine,&stepSample)) {
	printf("   **ERROR: -step '%s' does not look like line x sample "
	       "(e.g. '5x1').\n",GET_ARG(1));
	usage(argv[0]);
      }
      stepFlag=TRUE;
    }
    else {printf("\n   ***Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }
  if ((argc-currArg) < 3) {printf("   Insufficient arguments.\n"); usage(argv[0]);}

  printf("%s\n",date_time_stamp());
  asfPrintStatus("Program: asf_igram_coh\n\n");

  create_name(masterFile, argv[currArg],  ".img");
  create_name(slaveFile, argv[currArg+1],".img");
  strcpy(outFile, argv[currArg+2]);

  // Read input meta file
  inMeta = meta_read(masterFile);
  line_count = inMeta->general->line_count; 
  sample_count = inMeta->general->sample_count;

  // Figure multilooking values if necessary
  if (!stepFlag) {
    stepLine = inMeta->sar->look_count;
    stepSample = 1;
  }
  if (!lookFlag) {
    lookLine = WINDOW_SIZE * stepLine;
    lookSample = WINDOW_SIZE;
  }
  if (stepLine>lookLine || stepSample>lookSample) {
    asfPrintWarning("Entire image needs to be covered in this calculation\n");
    asfPrintWarning("Setting step values to look values\n");
    stepLine = lookLine;
    stepSample = lookSample;
  }

  // Call the library function to get the work done
  asf_igram_coh(lookLine, lookSample, stepLine, stepSample, 
		masterFile, slaveFile, outFile);

  return(0);
}

