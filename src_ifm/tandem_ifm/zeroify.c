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

const float VERSION=1.0;
int main(int argc,char *argv[])
{
	float *buf,*testbuf;
	char *outfile,*infile,*testfile;
	FILE *outF,*inF,*testF;
	struct DDR outDDR,inDDR,testDDR;
	int x,y;
	int nl,ns;
	
/*Parse CLA's.*/
	switch (argc)
	{
	case 4:	
		infile=argv[1];
		testfile=argv[2];
		outfile=argv[3];
		break;
	default:
		printf("\nUsage:  "
		"zeroify  <input image> <test image> <output image>\n"
		"\t      All images are arbitrary one-banded LAS images.\n"
		"\t      The input and test images must exist.\n"
		"\n"
		"zeroify creates an output image which is identical to\n"
		"the input image, but zero where the test image is zero.\n"
		"This is used in dem-guided phase unwrapping, in tandem_ifm.\n"
		"\nVersion %.2f, ASF SAR TOOLS\n\n",VERSION);
		exit(1);
	}
	
/*Open output files.*/
	inF=fopenImage(infile,"rb");
	testF=fopenImage(testfile,"rb");
	outF=fopenImage(outfile,"wb");

/*Read and copy over DDR.*/
	c_getddr(infile,&inDDR);
	c_getddr(testfile,&testDDR);
	outDDR=inDDR;
	c_putddr(outfile,&outDDR);
	
/*Allocate buffers.*/
	buf=(float *)MALLOC(sizeof(float)*inDDR.ns);
	testbuf=(float *)MALLOC(sizeof(float)*testDDR.ns);
	
/*Copy over each line of input to the output.*/
	nl=inDDR.nl;
	ns=inDDR.ns;
	for (y=0;y<nl;y++)
	{
		getFloatLine(inF,&inDDR,y,buf);
		getFloatLine(testF,&testDDR,y,testbuf);
		for (x=0;x<ns;x++)
			if (testbuf[x]==0)
				buf[x]=0;
		putFloatLine(outF,&outDDR,y,buf);
	}
/*Done!*/
	return 0;
}
