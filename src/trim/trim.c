/******************************************************************************
NAME: trim

SYNOPSIS: 

    trim [-log <file>] [-h <trim height>] [-w <trim width>]
	 <in> <out> <new top line> <new left sample> 

DESCRIPTION:
	Trim will let you change the size of any LAS 6.0 single-band image.
It should work (but hasn't yet been tested!) on int data.
It does work on byte, short, float, and complex data.

	You specify the input and output filenames (WITH extension), and 
a top left coordinate.  Trim will create a new image in outfile which
starts at these coordinates.  Optionally, you can also specify the new height 
and width of the output image (if not specified, trim will use the old size
minus the top left coordinates-- i.e. minus the part that is trimmed off).

	Trim relies on DDR information to do the trimming, and maintains DDR
information through to the output.

	Trim can be used anytime you want to work with a smaller image, but
it also can be used to remove the CEOS header from an image (albeit the process
for doing so is a bit convoluted): since the RADARSAT-era CEOS header is one line
on top of the image, plus 192 bytes per line, we can just trim it off.  E.g.
if we have a byte CEOS image "bob.D" we wish to turn into a LAS image "bill.img", type

	trim bob.D bill.img 1 192

	Trim will generate bill.img and bill.ddr.  Note that for this to work, you
need a file called "bob.ddr" to describe bob.D's dimensions and data type-- but 
CEOS files don't ship with such a file.  This is the purpose of the makeddr program,
which should also be available from ASF.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    6/97   Orion Lawlor Needed to trim various image files
    				 for faster interferometry.
    1.1	    5/98   Orion Lawlor Caplib/bug fix.
    1.11    7/01   Rudi Gens	Added logfile switch
    1.3     3/02   Pat Denny    Updated Command line parsing

HARDWARE/SOFTWARE LIMITATIONS: none

SEE ALSO:
	makeddr(1), trim_sic(1)

BUGS: none known

******************************************************************************/
/****************************************************************************
*								            *
*   Trim: lets you trim/add any # of samples to any LAS 6.0 image file      *
*         (even complex .cpx) while maintaining the DDR.		    *
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
#include "las.h"

#define VERSION 1.3
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

void usage(char *name);

int main(int argc, char *argv[])
{
  struct DDR inDDR,outDDR;
  int isComplex;
  long long pixelSize, offset;
  long long x,y,startX,startY,endX=-1,endY=-1,
      	    inMaxX,inMaxY,outMaxX,outMaxY,
      	    lastReadY,firstReadX,numInX;
  FILE *in,*out;
  char *buffer;
  char *infile,*outfile;

  logflag=0;
  currArg=1;      /* from cla.h in asf.h, points to current argv string */
  system("date");
  printf("Program: trim\n\n");  

/* Parse command line args */
  while (currArg < (argc-4))
  {
    char *key=argv[currArg++];
    if (strmatch(key,"-w")) {
      CHECK_ARG(1) /*one integer argument: width */
      endX=atoi(GET_ARG(1));
    } 
    else if (strmatch(key,"-h")) {
      CHECK_ARG(1) /*one integer argument: height */
      endY=atoi(GET_ARG(1));
    } 
    else if (strmatch(key,"-log")) {
      CHECK_ARG(1) /*one string argument: logfile name */
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag=1;
      StartWatchLog(fLog);
      printLog("Program: trim\n\n");
    }
    else {printf("*****Unrecognized option keyword:  %s\n",argv[currArg-1]);usage(argv[0]);}
  }
  if ((argc-currArg) < 4) {printf("Insufficient arguments.\n"); usage(argv[0]);}

 /*Compute filenames*/
  infile=argv[currArg];
  isComplex=(findExt(infile)&&(0==strcmp(findExt(infile),".cpx")));
  outfile=argv[currArg+1];
  in=fopenImage(infile,"rb");
  out=fopenImage(outfile,"wb");

  c_getddr(infile,&inDDR);
  pixelSize=inDDR.dtype;
  if (pixelSize==3) pixelSize=4;
  if (pixelSize==5) pixelSize=8;
  if (isComplex)    pixelSize*=2;
	
  inMaxX=inDDR.ns; inMaxY=inDDR.nl;
  startX=atoi(argv[currArg+3]);
  startY=atoi(argv[currArg+2]);

  endX = (endX!=-1) ? endX+startX : inMaxX;
  endY = (endY!=-1) ? endY+startY : inMaxY;

  outMaxX=endX-startX;
  outMaxY=endY-startY;

/*Write out our outDDR*/
  outDDR=inDDR;
  outDDR.nl=outMaxY;
  outDDR.ns=outMaxX;
  outDDR.master_line+=startY*outDDR.line_inc;
  outDDR.master_sample+=startX*outDDR.sample_inc;
  /*Some sort of conditional on the validity of the corner coordinates would be nice here.*/
  {
  	double bX, mX, bY, mY;
  	bY=inDDR.upleft[0];
  	bX=inDDR.upleft[1];
  	mY=(inDDR.loleft[0]-inDDR.upleft[0])/(inDDR.nl-1);
  	mX=(inDDR.upright[1]-inDDR.upleft[1])/(inDDR.ns-1);
  	
  	outDDR.upleft[0]=bY+mY*startY;
  	outDDR.upright[0]=bY+mY*startY;
  	outDDR.loleft[0]=bY+mY*(startY+outMaxY);
  	outDDR.loright[0]=bY+mY*(startY+outMaxY);
  	outDDR.upleft[1]=bX+mX*startX;
  	outDDR.upright[1]=bX+mX*(startX+outMaxX);
  	outDDR.loleft[1]=bX+mX*startX;
  	outDDR.loright[1]=bX+mX*(startX+outMaxX);
  }
  
  c_putddr(outfile,&outDDR);
    
/*If everything's OK, then allocate a buffer big enough for one line of output data.*/
  buffer= (char *)MALLOC(pixelSize*(outMaxX));
  
/*Let the user know what's going on.
  printf("%s is now copying pixels from %s to %s.\n",argv[0],argv[1],argv[2]);*/
  
/*If necessary, fill the top of the output with zeros, by loading up a buffer and writing.*/
  for (x=0;x<outMaxX*pixelSize;x++)
  	buffer[x]=0;
  for (y=0;y<-startY && y<outMaxY;y++)
  {
  	FWRITE(buffer,pixelSize,outMaxX,out);
  	if (y==0) printf("   Filling zeros at beginning of output image\n");
  }

/*Do some calculations on where we should read.*/
  firstReadX=MAX(0,-startX);
  numInX=MIN(MIN(outMaxX,inMaxX-(firstReadX+startX)),outMaxX-firstReadX);
  lastReadY=MIN(outMaxY,inMaxY-startY);
  offset=0;

  for (;y<lastReadY;y++)
  {
  	int inputY=y+startY,
  		inputX=firstReadX+startX,
  		outputX=firstReadX;

	offset=pixelSize*(inputY*inMaxX+inputX);

  	if (y==lastReadY) printf("   Writing output image\n");

	FSEEK64(in,offset,SEEK_SET);

  	FREAD(buffer+outputX*pixelSize,pixelSize,numInX,in);
  	FWRITE(buffer,pixelSize,outMaxX,out); 
  }
/*Reset buffer to zeros and fill remaining pixels.*/
  for (x=0;x<outMaxX*pixelSize;x++)
  	buffer[x]=0;
  for (;y<outMaxY;y++)
  {
  	FWRITE(buffer,pixelSize,outMaxX,out);
  	if (y==outMaxY) printf("   Filled zeros after writing output image\n");
  }

  printf("   Wrote %lld lines of data\n\n", outMaxY);
  if (logflag) {
    sprintf(logbuf, "   Wrote %lld lines of data\n\n", outMaxY);
    printLog(logbuf);
  }
    
/*Now close the files & free memory 'cause we're done.*/
  FREE(buffer);
  FCLOSE(in);
  FCLOSE(out);
  return(0);
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] [-h <trim_height>] [-w <trim_width>]\n"
	"        <in_file> <out_file> <new_top_line> <new_left_sample>\n",name);
 printf("\n"
      	"REQUIRED ARGUMENTS:\n"
      	"   <in_file>         Input file\n"
      	"   <out_file>        File to be output\n"
      	"   <new_top_line>    Line in input that will be top of output\n"
      	"   <new_left_sample> Sample in input that will be left side of output\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -log <file>      Option to have output written to a log <file>.\n"
      	"   -h <trim_height> Height of output file.\n"
      	"   -w <trim_width>  Width of output file.\n");
 printf("\n"
      	"DESCRIPTION:\n"
      	"   Allows you to trim/add any number of samples to any\n"
	"   LAS 6.0 image file (even complex .cpx) while maintaining the DDR.\n");
 printf("\n"
      	"Version %.2f, ASF SAR TOOLS\n"
      	"\n",VERSION);
 exit(1);
}
