/******************************************************************************
NAME: las_ramp

SYNOPSIS:

    las_ramp <in img> <out img> <topleft line,sample> <ht> <wth> <min> <max>

DESCRIPTION:

	Overlays a ramp-- a smooth graduation of grey tones-- across
        any given LAS image.  This is useful in preparing scientific
        illustrations.
        The procedure works with any single-banded image (e.g., byte,
        short, float...).

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    5/97   Orion Lawlor

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   las_ramp will create an output LAS ramp.				    *
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
#define maxImgs 20
	int x,y;
	float /**outbuf,*/*inbuf;
	char *outfile,*infile;
	FILE *outF,*inF;
	struct DDR outDDR,inDDR;
	int topX,topY,height,wid;
	double min,max;
	if (argc<7)
	{
	  printf("\nUsage:\n"
	  "las_ramp  <in img> <out img> <topleft line,sample> <ht> <wth> <min><max>\n"
	   "\n  <in img>                Input single-banded LAS image."
	  "\n  <out img>               Output LAS image."
   	"\n  <topleft line, sample>  Top left corner of image."
	"\n  <ht>                    height."
	"\n  <wth>                   width."
	"\n  <min> <max>             Top to Bottom OR Right to Left (See man page)"
	  "\n\nlas_ramp will create an output LAS ramp.\n"
	  "Version %.2f, ASF SAR TOOLS\n\n",VERSION);
	  exit(1);
	}
	infile=argv[1];
	outfile=argv[2];
	topY=atoi(argv[3]);
	topX=atoi(argv[4]);
	height=atoi(argv[5]);
	wid=atoi(argv[6]);
	sscanf(argv[7],"%lf",&min);
	sscanf(argv[8],"%lf",&max);
	c_getddr(infile,&inDDR);
	inF=fopenImage(infile,"rb");
	inbuf=(float *)MALLOC(sizeof(float)*inDDR.ns);
	outF=fopenImage(outfile,"wb");
	outDDR=inDDR;
	c_putddr(outfile,&outDDR);
	
	/*outbuf=(float *)MALLOC(sizeof(float)*outDDR.ns);*/
	for (y=0;y<topY;y++)
	{
		getFloatLine(inF,&inDDR,y,inbuf);
		putFloatLine(outF,&outDDR,y,inbuf);
	}
	for (y=topY;y<topY+height;y++)
	{
		getFloatLine(inF,&inDDR,y,inbuf);
		if (wid>height)
		 /*Horizontal color bar*/
		  for (x=topX;x<topX+wid;x++)
			inbuf[x]=((float)x-topX)/(wid-1)*(max-min)+min;
		else /*Vertical color bar*/
		  for (x=topX;x<topX+wid;x++)
			inbuf[x]=((float)y-topY)/(height-1)*(max-min)+min;
		putFloatLine(outF,&outDDR,y,inbuf);
	}
	for (y=topY+height;y<outDDR.nl;y++)
	{
		getFloatLine(inF,&inDDR,y,inbuf);
		putFloatLine(outF,&outDDR,y,inbuf);
	}
	return 0;
}
