/****************************************************************
NAME: pigram

SYNOPSIS:  pigram inIFMfile1 inIFMfile2 outfile

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
    4.0	     7/00	 T. Logan   - Created parallel version.
    4.1      8/00	 D. Koster  - Modified Program to handle files > 2GB
    4.11     7/01	 R. Gens    - Added logfile switch

HARDWARE/SOFTWARE LIMITATIONS:

  Note:  I do not find it necessary to scale the output amplitude in this
         case; this may be added later, perhaps as an option.

ALGORITHM DESCRIPTION:
 
ALGORITHM REFERENCES:

BUGS:

****************************************************************/
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
************************************************************/
#include "asf.h"
#include <mpi.h>
#include "ifm.h"
#include "asf_meta.h"

/* 
 *  Buffer Size
 */
#define BSZ      65536 
#define VERSION  4.11


int main(int argc, char *argv[])
{
  char fnm[256];
  int i, len, count=0;
  FILE *inFile1, *inFile2, *outFileAmp, *outFilePhase;
  double percent=5.0;
  long long bytesRead=0;  

  complexFloat *in1,*in2;
  float *outAmp,*outPhase;
  char cmd[256];

  int my_pe;
  int n_pes;

  long long input_offset;
  long long output_offset;
  long long end_of_input;

  /*
   * Establish MPI, Distribute Processors
   */

  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD, &n_pes);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_pe);

  /*
   * in-line usage
   */
  if (my_pe == 0) {  
	system("date");
	printf("Program: pigram\n\n");
  }

  StartWatch(); 
  if (argc<4)
   {
    if (my_pe == 0) usage(argv[0]);
    MPI_Finalize();
    exit(1);
   }

  /* 
   * establish buffers
   */
  in1=(complexFloat *)MALLOC(sizeof(complexFloat)*BSZ);
  in2=(complexFloat *)MALLOC(sizeof(complexFloat)*BSZ);
  outAmp=(float *)MALLOC(sizeof(float)*BSZ);
  outPhase=(float *)MALLOC(sizeof(float)*BSZ);
  
  logflag=0;
  /* 
   * open input files 
   */
  create_name(fnm,argv[1],".cpx");
  inFile1=fopenImage(fnm, "rb");
  
  create_name(fnm,argv[2],".cpx");
  inFile2=fopenImage(fnm, "rb");
 
  for (i=4; i<argc; i++) {
    if(strncmp(argv[i],"-log", 4)==0) {
      sscanf(argv[i+1], "%s", logFile);
      logflag=1;
      fLog = FOPEN(logFile, "a");
    }
  }

  if (logflag && (my_pe == 0)) {  
	StartWatchLog(fLog);
	printLog("Program: pigram\n\n");
  }
  /* 
   * create output files 
   */
  if (my_pe == 0)
   {
  	create_name(fnm,argv[3],".amp");
	outFileAmp=fopenImage(fnm, "wb");
	create_name(fnm,argv[3],".phase");
	outFilePhase=fopenImage(fnm, "wb");
	FCLOSE(outFileAmp);
	FCLOSE(outFilePhase);
   }
/*   if (my_pe == 0)
   {
	printf("\t Starting igram \n\n");
   }*/

  /* 
   * Open output files for random access 
   */

  MPI_Barrier(MPI_COMM_WORLD);
  create_name(fnm,argv[3],".amp");
  outFileAmp=fopenImage(fnm, "r+b");
  create_name(fnm,argv[3],".phase");
  outFilePhase=fopenImage(fnm, "r+b");

  FSEEK64(inFile1, 0, SEEK_END);
  end_of_input = FTELL64(inFile1);

  /*
   * Loop through each chunk of data 
   */

  input_offset = (long long) BSZ*sizeof(complexFloat)*(count*n_pes+my_pe);
  output_offset = (long long) BSZ*sizeof(float)*(count*n_pes+my_pe);

  while (input_offset < end_of_input)
   {
    int i,j,x;
    count++;

    /* 
     * read next chunk of data, check and calculate 
     * how many values were read
     */

    FSEEK64(inFile1, input_offset, SEEK_SET);
    i = fread(in1, 1, BSZ*sizeof(complexFloat),inFile1);

    FSEEK64(inFile2, input_offset, SEEK_SET);
    j = fread(in2, 1, BSZ*sizeof(complexFloat),inFile2);

    if (i != j ) bail("igram:  an input file was too short.");

    /* 
     * calculate how many values were read, print a brief diagnostic message 
     */
    len = i/sizeof(complexFloat);
    bytesRead += (len * n_pes * sizeof(complexFloat));
    
    if (my_pe==0)
    { 
	if ((bytesRead*100/end_of_input) == percent)
	{
		printf("   Completed %3.0f percent\n", percent);
		percent+=5.0; 
	}
    }

    /* 
     * if any data were obtained... 
     */
    for (x=0;x<len;x++)
    {
    /* 
     *	Take complex product of img1 and the conjugate of img2.
     */
    	double igram_real,igram_imag;
    	igram_real=in1[x].real*in2[x].real+in1[x].imag*in2[x].imag; 
    	igram_imag=in1[x].imag*in2[x].real-in1[x].real*in2[x].imag; 

	outAmp[x]=sqrt(igram_real*igram_real+igram_imag*igram_imag);    	
/*	outAmp[x]=sqrt(sqrt(in1[x].real*in1[x].real+in1[x].imag*in1[x].imag)); */

    	if (igram_real!=0.0 || igram_imag!=0.0)
    		outPhase[x]=atan2(igram_imag,igram_real);
    	else
    		outPhase[x]=0;
    }
    
    /* 
     * Save this batch 
     */
    FSEEK64(outFileAmp, output_offset, SEEK_SET);
    FWRITE(outAmp, 1, len*sizeof(float),outFileAmp);
    FSEEK64(outFilePhase, output_offset, SEEK_SET);
    FWRITE(outPhase, 1, len*sizeof(float),outFilePhase);

    input_offset = (long long) BSZ*sizeof(complexFloat)*(count*n_pes+my_pe);
    output_offset = (long long) BSZ*sizeof(float)*(count*n_pes+my_pe);

  }

  /* 
   * close files, free buffers, scram 
   */
  if (my_pe == 0)
  {
     printf("\n");
     sprintf(cmd,"cp %s.ddr %s.ddr\n",argv[1],argv[3]);
     system(cmd);
   }
  
  FCLOSE(inFile1);FCLOSE(inFile2);
  FCLOSE(outFileAmp);FCLOSE(outFilePhase);
  FREE(in1);FREE(in2);
  FREE(outAmp);FREE(outPhase);
 
  MPI_Barrier(MPI_COMM_WORLD); 
  if (my_pe == 0)
   {
/*    printf("\nigram:  Ends successfully\n");*/
    printf("   Wrote %lld bytes of data\n\n",end_of_input);
    StopWatch(); 
   }
  if (logflag && (my_pe == 0))
   {
     sprintf(logbuf, "   Wrote %lld bytes of data\n\n",end_of_input);
     printLog(logbuf);
     StopWatchLog(fLog); 
   }
  MPI_Finalize();
  return 0;
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <a> <b> <ab*> [-log <file>]\n", name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <a>    input:  a.cpx and a.ddr\n"
	"   <b>    input:  b.cpx and b.ddr\n"
	"   <ab*>  output: ab*.amp, ab*.phase, and ab*.ddr\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -log <file> allows the output to be written to a log file\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Creates <a x b*> from two source files: <a>.cpx, <b>.cpx.\n"
	"   This version uses stream-type i/o.  It creates two output\n"
	"   files: amplitude (<ab*>.amp) and phase (<ab*>.phase).\n");
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
}

