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

#include "asf.h"
#include "ddr.h"
#include "ifm.h"
#include "asf_meta.h"

/* 
 *  Buffer Size
 */
#define  BSZ     65535
#define VERSION  3.4
#define ampScale 200

/* function declaration */
void usage(char *name);

int main(int argc, char *argv[])
{
  char fnm[256];
  int count = 0,len;
  FILE *inFile1, *inFile2, *outFileAmp, *outFilePhase;
  unsigned int bytesRead=0;
  int bailout=0;
  struct DDR inDDR1, inDDR2;
	
  FComplex *in1,*in2;
  float *outAmp,*outPhase;
  char cmd[256];

  /*
   * in-line usage
   */
  StartWatch(); 
  if(argc < 4){
    usage(argv[0]);
  }
  
  /* 
   * establish buffers
   */
  in1=(FComplex *)MALLOC(sizeof(FComplex)*BSZ);
  in2=(FComplex *)MALLOC(sizeof(FComplex)*BSZ);
  outAmp=(float *)MALLOC(sizeof(float)*BSZ);
  outPhase=(float *)MALLOC(sizeof(float)*BSZ);
  
  /* 
   * open input files 
   */
  create_name(fnm,argv[1],".cpx");
  inFile1=fopenImage(fnm, "rb");
  c_getddr(fnm,&inDDR1);  

  create_name(fnm,argv[2],".cpx");
  inFile2=fopenImage(fnm, "rb");
  c_getddr(fnm,&inDDR2); 

  create_name(fnm,argv[3],".amp");
  outFileAmp=fopenImage(fnm, "wb");
  
  create_name(fnm,argv[3],".phase");
  outFilePhase=fopenImage(fnm, "wb");

  /*
   * Loop through each chunk of data 
   */
  printf("igram:\n\tstarting igram\n");
  do {
    int i,j,x;
    count++;  /* used in diagnostic */

    /* 
     * read next chunk of data, check and calculate 
     * how many values were read
     */
    i = fread(in1, 1, BSZ*sizeof(FComplex),inFile1);
    j = fread(in2, 1, BSZ*sizeof(FComplex),inFile2);
  
    /*if( i != j ) bail("One input file was too short"); */

    /* If the first input was shorter than the second */
    if ( i < j )
    {
	bailout=-1;
    }
    /* If the second input was shorter than the first */
    if ( i > j )
      {
	bailout=1;
	i = j;	
    }
 

    /* calculate how many values were read, print a brief diagnostic message */
    len = i/sizeof(FComplex);
    bytesRead += (len * sizeof(FComplex));
    
    printf("\tRead chunk %d, %d total bytes read\r", count, bytesRead);

    /* if any data were obtained... */
    for (x=0;x<len;x++)
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
    
    /* save this batch */
    FWRITE(outAmp, 1, len*sizeof(float),outFileAmp);
    FWRITE(outPhase, 1, len*sizeof(float),outFilePhase);
/*  printf("\r"); */
  } while ((len == BSZ) && (bailout==0));    /* while we have any action */

  /* 
   * close files, free buffers, scram 
   */
  if(bailout != 0)
  {
	if(bailout==1)
	{	
		printf("\n\nWarning, file %s is shorter than %s!\n",argv[2],argv[1]);
  		printf("Output will be truncated to the length of %s\n",argv[2]);
		sprintf(cmd,"cp %s.ddr %s.ddr\n",argv[2],argv[3]);
  		system(cmd);
	}
	else
	{
		printf("\n\nWarning, file %s is shorter than %s!\n",argv[1],argv[2]);
                sprintf(cmd,"cp %s.ddr %s.ddr\n",argv[1],argv[3]);
  		printf("Output will be truncated to the length of %s\n",argv[1]);
                system(cmd);	
	}
	
  }
  else
  {
  	printf("\n");
  	sprintf(cmd,"cp %s.ddr %s.ddr\n",argv[1],argv[3]);
  	system(cmd);
  }

  FCLOSE(inFile1);FCLOSE(inFile2);
  FCLOSE(outFileAmp);FCLOSE(outFilePhase);
  FREE(in1);FREE(in2);
  FREE(outAmp);FREE(outPhase);
  
  printf("\nigram:  Ends successfully\n");
  StopWatch(); 
  return 0;
}

void usage(char *name)
{
    printf("\nUSAGE:  %s <imageA> <imageB> <outfile>\n\n",name);
    printf("    <imageA>     complex image file (imageA.cpx, imageA.ddr). "
	   "\n\t         Igram adds any extension\n");
    printf("    <imageB>     complex image file (imageB.cpx, imageB.ddr)."
	   "\n\t         Igram adds any extension\n");
    printf("    <outfile>    Base name of amplitude and phase file to "
 	   "store interferogram. \n\n");
    printf("Program will create an interferogram from the two"
	   "\ninput images.  <outfile>.amp and <outfile>.phase are the "
	   "\noutput files.\n\n");
    printf("Version %.2f, ASF SAR Tools\n\n",VERSION);
    exit(1);
}

