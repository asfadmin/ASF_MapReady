/******************************************************************************
NAME: fit_Line

SYNOPSIS: fit_line <in> <out> [-log <file>] [-quiet]

DESCRIPTION:

	fit_line is used during CCSD interferometry to clean up
    the output of fico.  It's normally called by register_ccsd.

EXTERNAL ASSOCIATES:none

FILE REFERENCES:none

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    1/97   T. Logan	To collate output of fico
    2.0     6/97   O. Lawlor	Total rewrite, allow reverse correlation, 
    				weighting of input points (+more general).
    2.1     10/97   O. Lawlor	Check for not enough points in fit.
    2.2     5/98    O. Lawlor	Updated for new fico.
    2.21    7/01    R. Gens     Added logfile and quiet switch

HARDWARE/SOFTWARE LIMITATIONS: none

ALGORITHM DESCRIPTION:
	(1) Read in source files, tossing out points which do not correlate the
		same backwards and forwards (SNR->point's weight).
	(2) Find a least-squares linear mapping of the coordiate to the offset.
	(3) Compute the deviation between each point and that mapping.
	(4) Remove the point with the greatest deviation.
	(5) Repeat (2-4) until there are two points left.
	(6) Output the mapping which resulted in the lowest total error.

ALGORITHM REFERENCES:

BUGS: none known

FILE FORMATS:
Input and Output:
	<in forward>: a correlation point file from fico <img 1> <img 2> .. <ficoOut>
	<in backward>: a correlation point file from fico <img 2> <img 1> .. <ficoBW> <ficoOut>
	<out>: a least-squares regression line, in calc_deltas compatible format:
		<x scale delta> <x offset> \n <y scale delta> <y offset>

******************************************************************************/
/****************************************************************************
*								            *
*   Fit_line takes, as an input, correlation points from fico and produces  *
*   as output correlation coefficients for use by calc_deltas. 		    *
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


float version=2.21;
FILE *pointOutput=NULL;

void usage(char *name);

/*******************Least Squares Computation********************
System for performing a weighted linear first-order least-squares fit of
  data items.  The code finds the coefficents a and b which make the equation
  y=a*x+b
  best fit the given data.  First you call lsc_init with an existing lsc record,
  then add each point with lsc_add_point, then when done call lsc_get_coeffs.
  
Details:
  Minimizes Sum of w*(a*x+b-y)^2 by taking partials with respect to a and b, then
  setting them equal to zero.  c,d,e,f, and g are running sums for the sigma terms.*/
  
typedef struct {
	double c,d,e,f,g;
	double outA,outB;
	double error;
} least_squares_computation; /* a.k.a. lsc*/

void lsc_init(least_squares_computation *l)
{
	l->c=l->d=l->e=l->f=l->g=0.0;/*Start sums at zero.*/
}

void lsc_add_point(least_squares_computation *l,float x,float y,float w)
{/*x is the input, y is the output, w is the weight of this point (w==0 -> ignore) */
	l->c+=w*x*x;
	l->d+=w*x;
	l->e+=w*x*y;
	l->f+=w*y;
	l->g+=w;
}
void lsc_set_coeffs(least_squares_computation *l)
{
	double h=l->c*l->g-l->d*l->d;
	l->outA=(l->e*l->g-l->d*l->f)/h;
	l->outB=(l->c*l->f-l->e*l->d)/h;
	/*Now the data best fits the line  y=outA*x+outB  */
}


/***************************fit_line*****************************
fit_line is a set of structures and procedures which
  allows one to easily do two-dimentional least squares fitting
  of a function to a set of weighted points (eqns. as in lsc, above).

To use them, call:
fit_init 	once with a preallocated fit_line.  It fills in
		  all the fields.
fit_add_point 	with each data point, a struct of type point.
fit_print 	to calculate and display the least-squares fit.
fit_refine_and_remove, repeatedly if necessary.
	
*******************************************************************/
typedef struct {
	float x1,x2,y1,y2,w;
	int *next;
} point;

typedef struct {
	point *pts;
	int numPoints;
	least_squares_computation x,y;

	double minError;
	int minNumPoints;
	char minCoeffs[250];
	FILE *coeffList;
} fit_line;

void fit_init(fit_line *f)
{
	f->numPoints=0;
	f->pts=NULL;
	lsc_init(&f->x);
	lsc_init(&f->y);
	f->x.error=-1;
	f->y.error=-1;
	f->minError=1000000000000.0;
	f->minNumPoints=-1;
	f->coeffList=NULL;
	sprintf(f->minCoeffs,"Error in fit_line2: never called 'fit_refine_and_remove'!\n");
}
void fit_add_point(fit_line *f,point *p)
{
	point *oldHead=f->pts;
	f->pts=p;
	p->next=(int *)oldHead;
	f->numPoints++;
}
void fit_calc_data(fit_line *f)
{
	point *pt=f->pts;
	lsc_init(&f->x);
	lsc_init(&f->y);
	while (pt!=NULL)
	{
		lsc_add_point(&f->x,pt->x1,pt->x2,pt->w);
		lsc_add_point(&f->y,pt->y1,pt->y2,pt->w);
		pt=(point *)pt->next;
	}
	lsc_set_coeffs(&f->x);
	lsc_set_coeffs(&f->y);
}
void fit_refine_and_remove(fit_line *f)
{
	double thisError;
	char tempCoeffs[255];
	point *pt=f->pts,*prev=NULL;
	point *maxPt=NULL,*maxPrev=NULL;
	float maxDist=-1,xa,xb,ya,yb;
	/*calculate coefficents*/
	fit_calc_data(f);
	xa=f->x.outA,xb=f->x.outB,ya=f->y.outA,yb=f->y.outB;
	f->x.error=0;
	f->y.error=0;
	/*identify the point which is the maximum distance from conformance*/
	while (pt)
	{
		register float dx=pt->x2-(pt->x1*xa+xb);
		register float dy=pt->y2-(pt->y1*ya+yb);
		register float dist;
		dx*=dx;dy*=dy;
		f->x.error+=dx;
		f->y.error+=dy;
		dist=dx+dy;
		if (dist>maxDist)
		{
			maxDist=dist;
			maxPt=pt;
			maxPrev=prev;
		}
		prev=pt;
		pt=(point *)pt->next;
	}
	f->x.error=sqrt(f->x.error/(f->numPoints-2));
	f->y.error=sqrt(f->y.error/(f->numPoints-2));
	thisError=f->x.error+f->y.error;
	sprintf(tempCoeffs,"%20.16f %20.16f %20.16f %20.16f %d\n",-f->x.outA,-f->x.outB
					,-f->y.outA,-f->y.outB,f->numPoints);
	if (f->coeffList)
		fputs(tempCoeffs,f->coeffList);
	if (thisError<f->minError)
	{	
		
		strcpy(f->minCoeffs,tempCoeffs);
		f->minNumPoints=f->numPoints;
		f->minError=thisError;
	}
	/*remove that worst point*/
	if (maxPt)
	{
		if (maxPrev) 
			maxPrev->next=maxPt->next;
		else
			f->pts=(point *)maxPt->next;
		free(maxPt);
		f->numPoints--;
	}
}
void fit_print(fit_line *f)
{
	fit_calc_data(f);
	system("date");
        printf("Program: fit_line\n\n");
	printf("   Fit_line with %i points:\n",f->numPoints);
	printf("   <img 2>.x=<img 1>.x+%20.16f*<img 1>.x+%20.16f\n",-f->x.outA,-f->x.outB);
	printf("   <img 2>.y=<img 1>.y+%20.16f*<img 1>.x+%20.16f\n\n",-f->y.outA,-f->y.outB);
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: fit_line\n\n");
	  sprintf(logbuf,"   Fit_line with %i points:\n",f->numPoints);
	  printLog(logbuf);
	  sprintf(logbuf,"   <img 2>.x=<img 1>.x+%20.16f*<img 1>.x+%20.16f\n",-f->x.outA,-f->x.outB);
	  printLog(logbuf);
	  sprintf(logbuf,"   <img 2>.y=<img 1>.y+%20.16f*<img 1>.x+%20.16f\n\n",-f->y.outA,-f->y.outB);
	  printLog(logbuf);
	}
}
void fit_outputPoints(fit_line *f)
{
	if (pointOutput)
	{
		int n=1;
		point *pt=f->pts;
		while (pt)
		{
			fprintf(pointOutput,"%5i %8.5f %8.5f %12.10f %12.10f\n",n++,pt->x1,pt->y1,pt->x2,pt->y2);
			pt=(point *)pt->next;
		}
	}
}
int main(int argc,char **argv)
{
	char lineForward[255];
	FILE *inForward,*out;
	int i, totalPoints=0;
	fit_line f;
	if (argc<3) usage(argv[0]);
	fit_init(&f);
        for (i=3; i<argc; i++) {
	  /*Process extra argument: either c (coefficents) or p (points)*/
	  if (argv[3][0]=='c')
	    f.coeffList=FOPEN(argv[4],"w");
	  if (argv[3][0]=='p')
	    pointOutput=FOPEN(argv[4],"w");
          if(strncmp(argv[i],"-log", 4)==0) {
            sscanf(argv[i+1], "%s", logFile);
            logflag=1;
            fLog = FOPEN(logFile, "a");
          }
          if(strncmp(argv[i],"-quiet", 6)==0) quietflag=1;
        }
	inForward=FOPEN(argv[1],"r");
	out=FOPEN(argv[2],"w");

/*add points to fitline*/
	while (NULL!=(fgets(lineForward,255,inForward)))
	{
		float outX,outY,inX,inY;
		point *pt=(point *)MALLOC(sizeof(point));
		pt->w=1.0;
		sscanf(lineForward,"%f%f%f%f%f",&outX,&outY,&inX,&inY,&pt->w);
		pt->x1=inX;
		pt->x2=outX-inX;
		pt->y1=inX;
		pt->y2=outY-inY;
		fit_add_point(&f,pt);/*Now we pass the point on to the linear regressor (fitline).*/
		totalPoints++;
	}
/*output least-squares results*/
	fit_print(&f);
	fit_outputPoints(&f);
	while(f.numPoints>totalPoints*0.8&&totalPoints>3)
	{
		fit_refine_and_remove(&f);
		/*if ((f.numPoints<100)||(f.numPoints%50==0))
		{
			f.numPoints++;
			fit_print(&f);
			f.numPoints--;
		}*/
		
	}
	if (!quietflag) printf("   Fit_line: Starting with %i forward- and reverse-correlated\n   \
		points, we then threw away all but %i of those points, based on a linear\n   \
		regression, to produce the following coefficents:\n", totalPoints,f.minNumPoints);
	printf("   x linear, x offset, y linear, y offset:\n   %s\n",f.minCoeffs);
	if (logflag) {
	  sprintf(logbuf,"   x linear, x offset, y linear, y offset:\n   %s\n",f.minCoeffs);
	  printLog(logbuf);
	}
	fprintf(out,"%s",f.minCoeffs);
	return 0;
}

void usage(char *name)
{
		printf("\n\
Usage: %s <in> <out> [-log <file>] [-quiet]\n\
\n\t<in> input: forward correlation point file (from fico)\n\
\t<out> output: correlation coefficients, derived from a\n\
\t      least-squares fit, with point elimination.\n\
\t-log <file> allows the output to be written to a log file\n\
\t-quiet suppresses the output to the essential\n\
\nFit_line takes, as an input, correlation points from fico and produces\n\
as output correlation coefficients for use by calc_deltas. \n\
\nVersion %4.2f, ASF SAR Tools\n\n",name,version);
		exit(1);
}
