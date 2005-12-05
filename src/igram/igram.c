/****************************************************************
NAME: igram

SYNOPSIS:  igram <imageA> <imageB> <outfile>

    <imageA>     complex image file (imageA.cpx, imageA.ddr). 
                 Igram adds any extension
    <imageB>     complex image file (imageB.cpx, imageB.ddr).
                 Igram adds any extension
    <outfile>    Base name of amplitude and phase file to store interferogram. 

DESCRIPTION:
	Create an interferogram from the two input images. inIFMfile1 &
	inIFMfile2 are the names of the two inout images minus any
	extensions. The .amp & .phase extensions will be appended. 

	The output is multiplication of the inIFMfile1 and the complex
	conjugate of inIFMfile2. The resulting files will be a phase file
	(.phase) and an amplitude file (.amp). 

	The phase image created is not deramped for earth curvature. 
	The program deramp removes this artifact.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     10/95        Rob Fatland & M. Shindle - Orig. Development
    1.1      4/96        M. Shindle - changes to user interface
    2.0      8/96        M. Shindle - works with float numbers. Outputs an
				      amplitude instead of a power file.
    3.0	     5/97	 T. Logan   - copies ddr file from input.
     "       7/97        D.Corbett  - updated version number
    3.1	     6/98	 O. Lawlor  - Read from complex files.
                                     (saves a c2p during co-registration).
    3.2	     3/00	 M. Ayers   - Changed the output amplitude image to 
				      just be the first input's amplitude
    3.3	     6/00        M. Ayers   - Fixed igram so that it recognizes 
				      that one file is shorter and stops
                                      there without croaking.
    3.4      10/00	 M. Ayers   - Changed igram back to calculating the
				      amplitude of interferogram, also removed
				      the extra square root.
    3.5      2/04	 R. Gens    - Added log switch
    3.6      7/05        R. Gens    - Removed DDR dependency. Took care of
                                      endianess issue.


HARDWARE/SOFTWARE LIMITATIONS:

  Note:  I do not find it necessary to scale the output amplitude in this
         case; this may be added later, perhaps as an option.

ALGORITHM DESCRIPTION:
 
ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   igram -- Program will create an interferogram from the two input images.*
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
#include "ddr.h"
#include "ifm.h"
#include "asf_meta.h"

/* 
 *  Buffer Size
 */
#define BSZ      512
#define VERSION  3.6
#define ampScale 200

/* function declaration */
void usage(char *name);

int main(int argc, char *argv[])
{
  meta_parameters *metaIn, *metaOut;
  char fnm[256], master[255], igram_amp[255], igram_phase[255];
  FILE *inFile1, *inFile2, *outFileAmp, *outFilePhase;
  int ii, x, len, lines;
  float percent=5.0;
	
  complexFloat *in1,*in2;
  float *outAmp,*outPhase;
  char cmd[256];

  logflag = 0;
  /*
   * in-line usage
   */
  if(argc < 4){
    usage(argv[0]);
  }
  
  while (currArg < (argc-4)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-log")) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = 1;
    }
    else {printf("\n   ***Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }

  printf("%s\n",date_time_stamp());
  printf("Program: igram\n\n");
  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: igram\n\n");
  }

  /* 
   * open input files 
   */
  create_name(fnm,argv[currArg++],".img");
  sprintf(master, "%s", fnm);
  inFile1=fopenImage(fnm, "rb");

  create_name(fnm,argv[currArg++],".img");
  inFile2=fopenImage(fnm, "rb");

  create_name(fnm,argv[currArg],"_amp.img");
  sprintf(igram_amp, "%s", fnm);
  outFileAmp=fopenImage(fnm, "wb");
  
  create_name(fnm,argv[currArg],"_phase.img");
  sprintf(igram_phase, "%s", fnm);
  outFilePhase=fopenImage(fnm, "wb");

  /* Read and write metadata information */
  metaIn = meta_read(master);
  metaOut = meta_read(master);
  metaOut->general->data_type = REAL32;
  metaOut->general->image_data_type = AMPLITUDE_IMAGE;
  meta_write(metaOut, igram_amp);
  metaOut->general->image_data_type = PHASE_IMAGE;
  meta_write(metaOut, igram_phase);
  lines = metaIn->general->line_count;

  /* 
   * establish buffers
   */
  in1=(complexFloat *)MALLOC(sizeof(complexFloat)*BSZ*lines);
  in2=(complexFloat *)MALLOC(sizeof(complexFloat)*BSZ*lines);
  outAmp=(float *)MALLOC(sizeof(float)*BSZ*lines);
  outPhase=(float *)MALLOC(sizeof(float)*BSZ*lines);
  
  /*
   * Loop through each chunk of data 
   */
  for (ii=0; ii<lines; ii+=BSZ) {
    len = (ii+BSZ < lines) ? BSZ : lines-ii;
    get_complexFloat_lines(inFile1, metaIn, ii, len, in1);
    get_complexFloat_lines(inFile2, metaIn, ii, len, in2);

    /* if any data were obtained... */
    for (x=0;x<len*lines;x++)
    {
    /*Take complex product of img1 and the conjugate of img2.*/
    	double igram_real,igram_imag;
    	igram_real=in1[x].real*in2[x].real+in1[x].imag*in2[x].imag; 
    	igram_imag=in1[x].imag*in2[x].real-in1[x].real*in2[x].imag; 
    	
    	outAmp[x]=sqrt(igram_real*igram_real+igram_imag*igram_imag);
/*	outAmp[x]=sqrt(sqrt(in1[x].real*in1[x].real+in1[x].imag*in1[x].imag));  calculate the amplitude
	of the first image only. */

    	if (igram_real!=0.0 || igram_imag!=0.0)
    		outPhase[x]=atan2(igram_imag,igram_real);
    	else
    		outPhase[x]=0;
    }
    
    put_float_lines(outFileAmp, metaOut, ii, len, outAmp);
    put_float_lines(outFilePhase, metaOut, ii, len, outPhase);

    if ((ii*100/lines)>percent) {
      printf("   Completed %3.0f percent\n", percent);
      percent+=5.0;
    }
  }
  printf("   Completed 100 percent\n\n");

  FCLOSE(inFile1);FCLOSE(inFile2);
  FCLOSE(outFileAmp);FCLOSE(outFilePhase);
  FREE(in1);FREE(in2);
  FREE(outAmp);FREE(outPhase);
  FREE(metaIn);FREE(metaOut);
  
/*  printf("\nigram:  Ends successfully\n");*/
  if (logflag) {
     sprintf(logbuf, "   Wrote %ld lines of data\n\n", lines);
     printLog(logbuf);
  }
  return 0;
}

void usage(char *name)
{
    printf("\nUSAGE:  %s <imageA> <imageB> <outfile>\n\n",name);
    printf("    <imageA>     complex image file (imageA.cpx, imageA.meta). "
	   "\n\t         Igram adds any extension\n");
    printf("    <imageB>     complex image file (imageB.cpx, imageB.meta."
	   "\n\t         Igram adds any extension\n");
    printf("    <outfile>    Base name of amplitude and phase file to "
 	   "store interferogram. \n\n");
    printf("Program will create an interferogram from the two"
	   "\ninput images.  <outfile>.amp and <outfile>.phase are the "
	   "\noutput files.\n\n");
    printf("Version %.2f, ASF SAR Tools\n\n",VERSION);
    exit(1);
}

