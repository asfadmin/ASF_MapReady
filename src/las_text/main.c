/******************************************************************************
NAME: las_text

SYNOPSIS:

    las_text <in> <out> <txt> [<sz>]  [<txtclr>] [<bgclr>] [<centr ln,samp>]

DESCRIPTION:

	Writes the given text string on top of a LAS image.
        This is useful in preparing scientific illustrations.
        The procedure works with any single-banded image (e.g., byte, short,
        float...).

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:  DATE:   AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	   ??/??   O. Lawlor
    1.1     2/02   P. Denny     Update command line parsing

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   las_text will write the given text on the given las image. 		    *
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
#include <ctype.h>
#include "ddr.h"

#include "characters.tbl"

#define VERSION 1.1

void usage(char *name);

int main(int argc,char *argv[])
{
	float *inbuf;
	char *outfile,*infile,*writeStr;
	FILE *outF,*inF;
	struct DDR outDDR,inDDR;
	int x,y;
	int topY=-1,leftX=-1;
	int bottomY,rightX;
	int textHeight=CHAR_HEIGHT,textWidth=CHAR_WIDTH,scale=1,size=CHAR_HEIGHT;
	float textColor=0.0,bgColor=255.0;
	int stringWidth;
	int nChar;
	extern int optind;            /* argv index of the next argument */
	extern char *optarg;          /* current argv[] */
	int c;                        /* option letter from getopt() */

/*Parse CLA's.*/
	while ((c=getopt(argc,argv,"z:t:b:l:s:")) != EOF)
	{
	   switch (c) {
	     case 'z':
		size=atoi(optarg)*CHAR_HEIGHT;
		scale=(size+CHAR_HEIGHT/2)/CHAR_HEIGHT;
		textHeight=scale*CHAR_HEIGHT;
		textWidth=scale*CHAR_WIDTH;
		break;
	     case 't':
		textColor=atof(optarg);
		break;			
	     case 'b':
		bgColor=atof(optarg);
		break;
	     case 'l':
		topY=atoi(optarg);
		break;
	     case 's':
		leftX=atoi(optarg);
		break;
	     default:
		usage(argv[0]);
		break;	
	   }
	}

	if ((argc-optind) != 3)
	{
		if ((argc-optind) > 3)
			printf("\nToo many arguments.\n");
		if (((argc-optind) < 3) && (argc!=1))
			printf("\nToo few arguments.\n");
		if (fLog) FCLOSE(fLog);
		usage(argv[0]);
	}
	infile=argv[optind];
	outfile=argv[optind+1];
	writeStr=argv[optind+2];
	nChar=strlen(writeStr);
	
/*Open output files.*/
	inF=fopenImage(infile,"rb");
	outF=fopenImage(outfile,"wb");

/*Read and copy over DDR.*/
	c_getddr(infile,&inDDR);
	outDDR=inDDR;
	c_putddr(outfile,&outDDR);
	
/*Set output size.*/
	stringWidth=nChar*textWidth;
	if (topY==-1) topY=outDDR.nl/2;
	if (leftX==-1) leftX=outDDR.ns/2;
	
	topY-=textHeight/2;
	leftX-=stringWidth/2;
	rightX=leftX+stringWidth;
	bottomY=topY+textHeight;

	if (topY<0)
	{
		bottomY += -topY;
		topY=0;
	}
	if (leftX<0)
	{
		rightX += -leftX;
		leftX=0;
	}
	if (rightX>outDDR.ns)
	{
		leftX -= rightX-outDDR.ns;
		rightX=outDDR.ns;
	}
	if (bottomY>outDDR.nl)
	{
		topY -= bottomY-outDDR.nl;
		bottomY=outDDR.nl;
	}

	if (topY < scale) scale=topY;

/*Allocate buffers.*/
	inbuf=(float*)MALLOC(sizeof(float)*inDDR.ns);

/*Display status.*/	
	printf("Writing the phrase '%s' to image '%s', \n"
		"with %dx%d-pixel characters, in color %f/background %f,\n"
		"starting at line %d and sample %d.\n",
		writeStr,outfile,textHeight,textWidth,textColor,bgColor,
		topY,leftX);

/*Copy over top portion.*/
	for (y=0;y<topY-scale;y++)
	{
		getFloatLine(inF,&inDDR,y,inbuf);
		putFloatLine(outF,&outDDR,y,inbuf);
	}

/*Write one line of background.*/
	for (y=topY-scale;y<topY;y++)
	{
		getFloatLine(inF,&inDDR,y,inbuf);
		for (x=leftX;x<rightX;x++)
			inbuf[x]=bgColor;
		putFloatLine(outF,&outDDR,y,inbuf);
	}
	
/*Write text on middle portion.*/
	for (y=topY;y<bottomY;y++)
	{
		int inX,inY;
		getFloatLine(inF,&inDDR,y,inbuf);
		
		inY=(y-topY)*CHAR_HEIGHT/textHeight;
		for (x=leftX;x<rightX;x++)
		{
			int charNo;
			inX=(x-leftX)*CHAR_WIDTH/textWidth;
			charNo=inX/CHAR_WIDTH;
			if (letterHasLoc(writeStr[charNo],inX-charNo*CHAR_WIDTH,inY))
				inbuf[x]=textColor;
			else
				inbuf[x]=bgColor;
		}
		
		putFloatLine(outF,&outDDR,y,inbuf);
	}

/*Copy over bottom portion.*/
	for (y=bottomY;y<outDDR.nl;y++)
	{
		getFloatLine(inF,&inDDR,y,inbuf);
		putFloatLine(outF,&outDDR,y,inbuf);
	}
	return 0;
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s  [-z <sz>] [-t <txtclr>] [-b <bgclr>] [-l <line>] [-s <sample>]\n"
	"             <in> <out> <txt>\n"
	"\n"
	"REQUIRED:\n"
	"   <in>    Input LAS (single-band) image.\n"
	"   <out>   Output image.\n"
	"   <txt>   Desired text to be written on image.\n"
	"\n"
	"OPTIONS:\n"
	"   -z <sz>      Size of text (pixels by mult 9).\n"
	"   -t <txtclr>  Numeric value for text color.  Default is 0 (black).\n"
	"   -b <bgclr>   Background color of text.  Default is 255 (white).\n"
	"   -l <line>    Center line (y dir) of text.  Default center of image.\n"
	"   -s <sample>  Center sample (x dir) of text .  Default center of image.\n"
	"\n"
	"DESCRIPTION:\n"
	"  %s will write the given text on the given las image.\n"
	"\n"
	"Version %.2f, ASF SAR TOOLS\n"
	"\n",name,name,VERSION);
 exit(1);
}
