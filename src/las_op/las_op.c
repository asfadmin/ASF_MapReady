/******************************************************************************
NAME: las_op

SYNOPSIS:  las_op [-log <file>] <out.ext> "exp" <inA.ext> [<inB.ext> [...]]

DESCRIPTION:  

	Las_op calculates an output image based on some mathematical
	function of the input image (or images).  That is, if the expression is
	"2*a", the output image's pixel values will be twice the input image's
	pixel values.
        Works with any type of input data-- byte, short, long, or float.


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

ROGRAM HISTORY:
    VERS:    DATE:   AUTHOR:
    -----------------------------------------------------------------
     1.0     11/97   O. Lawlor   Initial Development.
     1.01     7/01   R. Gens     Added logfile switch
     1.1      2/02   P. Denny    Updated command line parsing

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   Las_op will create an output LAS image based upon the mathematical	    *
*          expression you give it. 					    *
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
#include "expression.h"

#define MAXIMGS 20


#define VERSION 1.1

void usage(char *name);
void printLog(char *); /* from log.h */

int main(int argc,char **argv)
{
	int i,x,y;
	char *expr,*cookie;
	float *outbuf,*inbuf[MAXIMGS];
	char *outfile,*infile[MAXIMGS];
	FILE *outF,*inF[MAXIMGS];
	struct DDR outDDR,*inDDR[MAXIMGS];
	int nInputs;
	extern int optind;            /* argv index of the next argument */
	extern char *optarg;          /* current argv[] */
	int c;                        /* option letter from getopt() */

	fLog=NULL;
	logflag=0;

        system("date");
	printf("\n");

	while ((c=getopt(argc,argv,"l:")) != EOF)
	{
	   switch (c) {
	     case 'l':/* -log <filename>, get logfile; this is sorta hacked */
		if (0==strncmp(optarg,"og",2)) 
		{
			strcpy(logFile,argv[optind++]);
			fLog = FOPEN(logFile, "a");
			StartWatchLog(fLog);
			logflag=1;
			printLog("\n");
		}
		else usage(argv[0]);
		break;
	     default:
		if (fLog) FCLOSE(fLog);
 		usage(argv[0]);
		break;	
	   }
	}
	if ((argc-optind) < 3)
	{
		printf("Missing <out.ext>, \"exp\", or <inA.ext>.\n\n");
		if (fLog) FCLOSE(fLog);
		usage(argv[0]);
	}

	outfile = argv[optind++];
	expr = argv[optind++];
	
	nInputs = argc-optind;

	for (i=0;i<nInputs;i++)
	{
		infile[i]=argv[optind+i];
		inDDR[i]=(struct DDR *)MALLOC(sizeof(struct DDR));
		c_getddr(infile[i],inDDR[i]);
		inF[i]=fopenImage(infile[i],"rb");
		if (i!=0)
		{/*Make sure everyone has at least the same size as the first image.*/
			if ((inDDR[i]->nl<inDDR[0]->nl)||
				(inDDR[i]->ns<inDDR[0]->ns))
			{
				fprintf(stderr,"The images must all be as least as \n"
						"big as the first input image.\n");
				exit(1);
			}
		}
		inbuf[i]=(float *)MALLOC(sizeof(float)*inDDR[i]->ns);
	}
	outF=fopenImage(outfile,"wb");
	outDDR=*(inDDR[0]);
	c_putddr(outfile,&outDDR);
	
	outbuf=(float *)MALLOC(sizeof(float)*outDDR.ns);
	cookie=expression2cookie(expr,nInputs);
	if (NULL==cookie) exit(1);
	for (y=0;y<outDDR.nl;y++)
	{
		double variables[26];
		variables['y'-'a']=y;
		for (i=0;i<nInputs;i++)
			getFloatLine(inF[i],inDDR[i],y,inbuf[i]);
		for (x=0;x<outDDR.ns;x++)
		{
			variables['x'-'a']=x;
			for (i=0;i<nInputs;i++)
				variables[i]=inbuf[i][x];
			outbuf[x]=evaluate(cookie,variables);
		}
		putFloatLine(outF,&outDDR,y,outbuf);
		if ((y%100)==0) {
			printf(" Now Processing Line %d\r",y);
			fflush(NULL);
		}
	}
	printf("Processed %d Lines.          \n",y);
	return 0;
}

void usage(char *name)
{
 printf("Usage:\n"
	"%s [-log <file>] <out.ext> \"exp\" <inA.ext> [<inB.ext> [..]]\n"
	"\n"
	"   [-log <file>]    Option to have output written to a log file.\n"
	"   <out.ext>        LAS 6.0 image with extension to be created.\n"
	"   <exp>            Expression.\n"
	"   <inA.ext>        First input image with extension.\n"
	"   [<inB.ext>]      Optional second input image with extension.\n"
	"   [...]            Optional additional images with extension.\n"
	"\n"
	"Las_op will create an output LAS image based upon the\n"
	"mathematical expression you give it.\n"
	"For example, this command line creates an image one.img,\n"
	"whose pixels are equal to two.img plus one tenth of three.img:\n"
	"las_op one.img 'a+b/10' two.img three.img\n"
	"\nVersion %.2f, ASF SAR TOOLS\n\n",name,VERSION);
 exit(1);
}
