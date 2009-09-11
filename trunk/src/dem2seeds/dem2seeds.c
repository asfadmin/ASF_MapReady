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

int main(int argc, char **argv)
{
  int xCount,yCount,x,y,seedNo,line_count,sample_count;
  float *prev,*line,*aftr;
  char *fin,*fout;
  FILE *fpin,*fpout;
  meta_parameters *metaIn, *metaAmp;
  
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
  metaIn = meta_read(fin);
  line_count = metaIn->general->line_count;
  sample_count = metaIn->general->sample_count;
  metaAmp = meta_read(argv[currArg+1]);
  
  fpout=FOPEN(fout,"w");
  fpin=fopenImage(fin,"rb");
  line=(float *)MALLOC(sizeof(float)*sample_count);
  prev=(float *)MALLOC(sizeof(float)*sample_count);
  aftr=(float *)MALLOC(sizeof(float)*sample_count);
  /*Loop through a gridRes x gridRes grid, creating a list of potential seed points 
    & their errors.*/
  printf("   Searching DEM for potential seed point locations...\n\n");
  for (yCount=0;yCount<gridRes;yCount++)
    {
      y=size+((float)(yCount))/(gridRes-1)*(line_count-1-2*size);
      get_float_line(fpin, metaIn, y, line);
      get_float_line(fpin, metaIn, y-size, prev);
      get_float_line(fpin, metaIn, y+size, aftr);
      for (xCount=0;xCount<gridRes;xCount++)
	{
	  float cur,slopeErr;
	  x=size+((float)(xCount))/(gridRes-1)*(sample_count-1-2*size);
	  cur=line[x];
	  if (cur>0.0) /*If the point is valid, add it to the seed list.*/
	    {
	      seedRec *seed=MALLOC(sizeof(seedRec));
	      slopeErr=
		(fabs(prev[x-size]-cur)+fabs(prev[x-size/2]-cur)+fabs(prev[x]-cur)+
		 fabs(prev[x+size/2]-cur)+fabs(prev[x+size]-cur)+
		 fabs(line[x-size]-cur)+fabs(line[x-size/2]-cur)+                 
		 fabs(line[x+size/2]-cur)+fabs(line[x+size]-cur)+
		 fabs(aftr[x-size]-cur)+fabs(aftr[x-size/2]-cur)+
		 fabs(aftr[x]-cur)+fabs(aftr[x+size/2]-cur)+fabs(aftr[x+size]-cur));
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
      int seedX=seed->x*boxRes/sample_count;
      int seedY=seed->y*boxRes/line_count;
      if (boxes[seedY*boxRes+seedX]==NULL)
	{
	  boxes[seedY*boxRes+seedX]=seed;
	  fprintf(fpout,"%i %i %f\n",
		  (int)(seed->x*metaAmp->sar->sample_increment +
			metaAmp->general->start_sample),
		  (int)(seed->y*metaAmp->sar->line_increment +
			metaAmp->general->start_line),
		  seed->height);
	  numBoxes++;
	}
      seedNo++;
    }
  printf("   Final number of seed points: %i\n\n", seedNo);
  printf("   Seed point distribution:\n\n");
  if (logflag) {
    sprintf(logbuf,"   Final number of seed points: %i\n\n   "
	    "Seed point distribution:\n\n", seedNo);
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
 
  return(0);
}

