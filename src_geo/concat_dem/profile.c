/******************************************************************************
NAME: profile extract a slice through several DEMS, and plot the result.

SYNOPSIS:  profile <projY> <output img> <input1> [<input2>[..]]

DESCRIPTION:  
	profile extracts a slice through several images
	at the specified projection coordinate.
	Inputs are LAS images, with extensions.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    6/98   O. Lawlor	Create concatenated DEMs for IGARS

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   profile extract a slice through several DEMS, and plot the result.      *
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
#include "worgen.h"
#include "ddr.h"
#include "caplib.h"
#include "dem.h"

#define PIX_SIZE 3.0 /*Size of vertical pixels (meters): 
	since horizonatal pixels are 30m, this is
	a 10x height-stretched image.*/
#define HEIGHT 2000.0 /*Distance spanned by vertical pixels, in meters.*/

int toY(double z,int nl)
{
	int ret=nl-1-z/PIX_SIZE;
	if (ret<0)
		ret=0;
	if (ret>=nl)
		ret=nl-1;
	return ret;
}

const float VERSION=1.0;

int main(int argc,char *argv[])
{
	#define maxImgs 100
/*Input files:*/
	float *inBuf[maxImgs],*inErr[maxImgs];
	dem *in[maxImgs];
/*Output files: output (heights) and byte (index of written pixel).*/
	float *byteBuf;
	char *outfile,bytefile[255];
	FILE *byteF;
	struct DDR /*outDDR,*/byteDDR;
/*General Variables:*/
	int i,x,y,ns,nl;
	extents outExt;
	double pStartX,pStartY,pDistX,pDistY;
	double sliceY;
	int nInputs,optInd=1;
	
/*Check CLA's.*/
	if (argc<4)
	{
		printf("\nUsage:\n"
		"\tprofile <projY> <output img> <input1> [<input2>[..]]\n"
		"\n"
		"profile extracts a slice through several images\n"
		"at the specified projection coordinate.\n"
		"Inputs are LAS images, with extensions.\n"
		"\nVersion %.2f, ASF SAR TOOLS\n\n",VERSION);
		exit(1);
	}
	sliceY=atof(argv[optInd++]);
	outfile=argv[optInd++];
	nInputs=argc-optInd;
/*Open input files, accumulating the extent they span.*/
	outExt=nullExtents();
	for (i=0;i<nInputs;i++)
	{
		in[i]=open_dem(argv[optInd+i]);
		if (i!=nInputs-1)
			outExt=sumExtents(outExt,in[i]->height->ext);
	}

/*Create output file.*/
	strcpy(bytefile,outfile);
	byteF=FOPEN(bytefile,"wb");
	outExt.v.min=sliceY;
	outExt.v.max=sliceY+1000.0;
	create_outDDR(outExt,&(in[0]->height->ddr),
		&pStartX,&pStartY,&pDistX,&pDistY,&byteDDR);
	nl=byteDDR.nl=(int)(HEIGHT/PIX_SIZE);
	ns=byteDDR.ns;
	byteDDR.dtype=EBYTE;
	byteDDR.nbands=1;
	c_putddr(bytefile,&byteDDR);
	

/*Create buffers.*/
	byteBuf=(float *)MALLOC(sizeof(float)*ns*nl);
	for (i=0;i<nInputs;i++)
	{
		inBuf[i]=(float *)MALLOC(sizeof(float)*ns);
		inErr[i]=(float *)MALLOC(sizeof(float)*ns);
	}
	
/*Do Work:*/
	printf("Reading lines.\n");
	for (i=0;i<nInputs;i++)
		getLineByProj(pStartX,sliceY,ns,
				in[i],inBuf[i],inErr[i]);
	
	printf("Zeroing buffer.\n");
	for (y=0;y<nl;y++)
		for (x=0;x<ns;x++)
			byteBuf[y*ns+x]=0;
	
	printf("Drawing lines.\n");
	for (i=0;i<nInputs;i++)
		for (x=1;x<ns;x++)
		if (inBuf[i][x]!=0.0)
		{
			float prevZ=inBuf[i][x-1];
			float currZ=inBuf[i][x];
			float nextZ=inBuf[i][x-1];
			int curY=toY(currZ,nl);
			int minY,maxY;
			/*int errY=inErr[i][x];*/
			if (prevZ!=0.0)
			{
				maxY=curY,minY=(toY(prevZ,nl));
				if (minY>maxY)
					maxY=minY,minY=curY;/*Swap min and max.*/
				for (y=minY;y<=maxY;y++)
					byteBuf[y*ns+x]=i+1;
			}
			if (nextZ!=0.0)
			{
				maxY=curY,minY=(toY(nextZ,nl));
				if (minY>maxY)
					maxY=minY,minY=curY;/*Swap min and max.*/
				for (y=minY;y<=maxY;y++)
					byteBuf[y*ns+x]=i+1;
			}
		#if 0
			/*Outline error estimate in light color, being gentle to existing pixels.*/
			if (errY!=50 && /*USGS Error.*/
			    errY!=100 ) /*Default unknown SAR Error.*/
			{
				errY/=PIX_SIZE;
				for (y=curY-errY;y<curY+errY;y++)
					if (byteBuf[y*ns+x]==0)/*Don't mash existing pix.*/
						byteBuf[y*ns+x]=100;
			}
		#endif
		}
	
	printf("Writing out buffer.\n");
	for (y=0;y<nl;y++)
		putFloatLine(byteF,&byteDDR,y,&byteBuf[y*ns]);
	
	return 0;
}
