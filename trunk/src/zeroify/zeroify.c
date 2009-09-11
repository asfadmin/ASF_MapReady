/******************************************************************************
NAME:  zeroify - utility used inside tandem_ifm

SYNOPSIS: zeroify  <input image> <test image> <output image>
      All images are arbitrary one-banded LAS images.
      The input and test images must exist.

DESCRIPTION:

        Zeroify creates an output image which is identical to
        the input image, but zero wherever the test image is zero.
        This is used in dem-guided phase unwrapping, in tandem_ifm.

        That is, for each pixel:
        if the test image is zero, the output is zero;
        otherwise, set the output image to the input image.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    2/99   O. Lawlor    Needed for correct dem-guided phase unwrapping.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   Zeroify creates an output image which is identical to		    *
*           the input image, but zero wherever the test image is zero.      *
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
#include "asf_meta.h"

const float VERSION=1.0;
int main(int argc,char *argv[])
{
	float *buf,*testbuf;
	char *outfile,*infile,*testfile;
	FILE *outF,*inF,*testF;
	meta_parameters *inMeta, *inTest;
	int x,y;
	int nl,ns;

	logflag=0;
	
/*Parse CLA's.*/
	switch (argc)
	{
	case 4:	
		infile=argv[1];
		testfile=argv[2];
		outfile=argv[3];
		break;
	case 6:
	  sprintf(logFile, "%s", argv[2]);
	  fLog = FOPEN(logFile, "a");
	  logflag=1;
	  infile=argv[3];
	  testfile=argv[4];
	  outfile=argv[5];
	  break;
	default:
		printf("\nUsage:  "
		"zeroify  [-log <logFile>] <input image> <test image> <output image>\n"
		"\t      All images are arbitrary one-banded LAS images.\n"
		"\t      The input and test images must exist.\n"
		"\n"
		"zeroify creates an output image which is identical to\n"
		"the input image, but zero where the test image is zero.\n"
		"This is used in dem-guided phase unwrapping, in tandem_ifm.\n"
		"\nVersion %.2f, ASF SAR TOOLS\n\n",VERSION);
		exit(1);
	}

  printf("%s\n",date_time_stamp());
  printf("Program: zeroify\n");
  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: zeroify\n\n");
  }
	
/*Open output files.*/
	inF=fopenImage(infile,"rb");
	testF=fopenImage(testfile,"rb");
	outF=fopenImage(outfile,"wb");

/*Read and copy over DDR.*/
	inMeta = meta_read(infile);
	inTest = meta_read(testfile);
	meta_write(inMeta, outfile);
	
/*Allocate buffers.*/
	buf=(float *)MALLOC(sizeof(float)*inMeta->general->sample_count);
	testbuf=(float *)MALLOC(sizeof(float)*inTest->general->sample_count);
	
/*Copy over each line of input to the output.*/
	nl=inMeta->general->line_count;
	ns=inMeta->general->sample_count;
	for (y=0;y<nl;y++)
	{
		get_float_line(inF,inMeta,y,buf);
		get_float_line(testF,inTest,y,testbuf);
		for (x=0;x<ns;x++)
			if (testbuf[x]==0)
				buf[x]=0;
		put_float_line(outF,inMeta,y,buf);
	}
/*Done!*/
	return 0;
}
