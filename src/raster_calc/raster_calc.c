/******************************************************************************
*                                                                             *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/
/******************************************************************************
NAME: raster_calc

SYNOPSIS:  raster_calc [-log <file>] <out.ext> "exp" <inA.ext> [<inB.ext> [...]]

DESCRIPTION:  
	Calculates an output image based on some mathematical function of the 
	input image (or images).  That is, if the expression is "2*a", the 
	output image's pixel values will be twice the input image's pixel 
	values. Works with any type of input data-- byte, short, long, or float.


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
     1.2      2/04   P. Denny    Changed name from las_op to raster_calc
                                  Changed from the GPL to the BSD License
                                  
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/

#include "asf.h"
#include "asf_meta.h"
#include "expression.h"

#define MAXIMGS 20


#define VERSION 1.2

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
 printf("\n"
 	"USAGE:\n"
	"   %s [-log <file>] <out.ext> \"exp\" <inA.ext> [<inB.ext> [...]]\n",
	name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <out.ext>        ASF tools format image with extension to be created.\n"
	"   <exp>            Expression.\n"
	"   <inA.ext>        First input image with extension.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   [<inB.ext>]      Optional second input image with extension.\n"
	"   [...]            Optional additional images with extension.\n"
	"   [-log <file>]    Option to have output written to a log file.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Creates an output ASF tools format image based upon the\n"
	"   mathematical expression you give it.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
