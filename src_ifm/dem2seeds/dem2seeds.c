/******************************************************************************
NAME: dem2seeds -- search a slant-range DEM for good seed points.

SYNOPSIS:  dem2seeds [-b] [-log <file>] <inDEM> <inAmp> <outSeeds>

DESCRIPTION:
	This program searches the specified DEM for seed points.  The seed
	points are selected on the basis of their flat surroundings and
	well-distributed-ness.  These seed point locations are useful for 
	refining the interferometric baseline.  We define good seed points
	as those having minimal local slope.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    2/98   O. Lawlor	Automate Seed point generation for tandem_ifm
    1.1	    6/98   O. Lawlor	Changed definition of seed point file.
    1.11    7/01   R. Gens	Added logfile switch
    1.3     4/02   P. Denny     Standardized commandline parsing & usage()

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   dem2seeds -- This program searches a slant-range LAS 6.0 DEM for good   *
*		 seed point locations.					    *
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
#include "asf_meta.h"

#define size 4
#define gridRes 100
#define boxRes 15
#define boxFrac ((int)(boxRes*boxRes*0.7))
#define VERSION 1.3

typedef struct {
	int x,y;
	float height;
	float slopeErr;
} seedRec;
seedRec *seeds[gridRes*gridRes];
int numSeeds=0;
seedRec *boxes[boxRes*boxRes];
int numBoxes=0;


int sortDirection=1;/* 1== ascending sort order.
		-1 == descending sort order.*/

int qsorter(const seedRec **a,const seedRec **b);
int qsorter(const seedRec **a,const seedRec **b)
{
	if (a[0]->slopeErr-b[0]->slopeErr<0)
		return -1*sortDirection;
	if (a[0]->slopeErr-b[0]->slopeErr>0)
		return 1*sortDirection;
	return 0;
}

int main(int argc, char **argv)
{
	int xCount,yCount,x,y,seedNo;
	float *prev,*line,*aftr;
	char *fin,*fout;
	FILE *fpin,*fpout;
	struct DDR ddrin,ddrAmp;

	logflag=0;

/* parse command line */
	currArg=1; /*from cla.h in asf.h*/
	while (currArg < (argc-3)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-log")) {
			CHECK_ARG(1);
			strcpy(logFile, GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			StartWatchLog(fLog);
			printLog("Program: dem2seeds\n\n");
 			logflag=1;
		}
		else if (strmatch(key,"-b")) {
			sortDirection=-1;
		}
		else {printf("\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < 3) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	fin  = argv[currArg];
	fout = argv[currArg+2];

	system("date");
	printf("Program: dem2seeds\n\n");

/*Open files and allocate memory.*/
	c_getddr(fin,&ddrin);
	c_getddr(argv[currArg+1],&ddrAmp);
	
	fpout=FOPEN(fout,"w");
	fpin=fopenImage(fin,"rb");
	line=(float *)MALLOC(sizeof(float)*ddrin.ns);
	prev=(float *)MALLOC(sizeof(float)*ddrin.ns);
	aftr=(float *)MALLOC(sizeof(float)*ddrin.ns);
/*Loop through a gridRes x gridRes grid, creating a list of potential seed points & their errors.*/
	printf("   Searching DEM for potential seed point locations...\n\n");
	for (yCount=0;yCount<gridRes;yCount++)
	{
		y=size+((float)(yCount))/(gridRes-1)*(ddrin.nl-1-2*size);
/*		printf("Searching line %i...\n",y);*/
		getFloatLine(fpin,&ddrin,y,line);
		getFloatLine(fpin,&ddrin,y-size,prev);
		getFloatLine(fpin,&ddrin,y+size,aftr);
		for (xCount=0;xCount<gridRes;xCount++)
		{
			float cur,slopeErr;
			x=size+((float)(xCount))/(gridRes-1)*(ddrin.ns-1-2*size);
			cur=line[x];
			if (cur>0.0) /*If the point is valid, add it to the seed list.*/
			{
				seedRec *seed=MALLOC(sizeof(seedRec));
				slopeErr=
(fabs(prev[x-size]-cur)+fabs(prev[x-size/2]-cur)+fabs(prev[x]-cur)+fabs(prev[x+size/2]-cur)+fabs(prev[x+size]-cur)+
 fabs(line[x-size]-cur)+fabs(line[x-size/2]-cur)+                 +fabs(line[x+size/2]-cur)+fabs(line[x+size]-cur)+
 fabs(aftr[x-size]-cur)+fabs(aftr[x-size/2]-cur)+fabs(aftr[x]-cur)+fabs(aftr[x+size/2]-cur)+fabs(aftr[x+size]-cur));
 				seed->x=x;
 				seed->y=y;
 				seed->height=cur;
 				seed->slopeErr=slopeErr;
				seeds[numSeeds++]=seed;
			}
		}
	}
/*Now sort that list of potential seed points.*/
	printf("   Potential seed points: %i\n",numSeeds);
	if (logflag) {
	  sprintf(logbuf,"   Potential seed points: %i\n",numSeeds);
	  printLog(logbuf);
	}
	qsort(seeds,numSeeds,sizeof(seedRec *),(int (*)(const void *,const void *))qsorter);
/*Now file that list of seed points into the boxes, starting at the top of the list.
	printf("Sifting the better seed points into boxes.\n");*/
	/*Set the boxes initially to all empty.*/
	for (y=0;y<boxRes;y++)
		for (x=0;x<boxRes;x++)
			boxes[y*boxRes+x]=NULL;
	seedNo=0;
	while (seedNo<numSeeds&&numBoxes<boxFrac)
	{
		seedRec *seed=seeds[seedNo];
	/*Try putting this seed point into it's corresponding box--
	  if the box is empty, then this point is a good one (output it).
	  if the box is already full, then discard this point. */
		int seedX=seed->x*boxRes/ddrin.ns;
		int seedY=seed->y*boxRes/ddrin.nl;
		if (boxes[seedY*boxRes+seedX]==NULL)
		{
			boxes[seedY*boxRes+seedX]=seed;
			fprintf(fpout,"%i %i %f\n",
				(int)(seed->x*ddrAmp.sample_inc+ddrAmp.master_sample-1),
				(int)(seed->y*ddrAmp.line_inc+ddrAmp.master_line-1),
				seed->height);
			numBoxes++;
		}
		seedNo++;
	}
	printf("   Final number of seed points: %i\n\n", seedNo);
	printf("   Seed point distribution:\n\n");
	if (logflag) {
	  sprintf(logbuf,"   Final number of seed points: %i\n\n   Seed point distribution:\n\n", seedNo);
	  printLog(logbuf);
	}
	for (y=0;y<boxRes;y++)
	{
		char tmp[5];

		printf("   ");
		if (logflag) sprintf(logbuf,"   ");
		for (x=0;x<boxRes;x++) {
			printf("%c",boxes[y*boxRes+x]?'X':' ');
			if (logflag) {
			  sprintf(tmp,"%c",boxes[y*boxRes+x]?'X':' ');
			  strcat(logbuf,tmp);
			}
		}
		printf("\n");
		if (logflag) {
		  printLog(logbuf);
		  printLog("\n");
		}
	}
	printf("\n");
	if (logflag) printLog("\n");
/*	printf("Dem2seeds complete!\n");*/
	return(0);
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-b] [-log <file>] <inDEM> <inAmp> <outSeeds>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   inDEM     The input DEM image.\n"
	"   inAmp     The input SAR amplitude image.  The seed points\n"
	"               are written relative to this image.\n"
	"   outSeeds  Output list of seed points (x, y, height) found in DEM.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"    -b    seed points will be selected on the\n"
	"            basis of *maximal* local slope\n"
	"    -log  Option to have output written to a log <file>.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   %s searches a slant-range LAS 6.0 DEM for good\n"
	"   seed point locations.\n",name);
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n", VERSION);
 exit(1);
}
