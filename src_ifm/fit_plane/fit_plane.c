/******************************************************************************
NAME: fit_plane

SYNOPSIS: fit_plane [-k <keepFraction] [-log <file>] <in> <out>

DESCRIPTION:
    	fit_plane is used during complex interferometry to clean up
    the output of fico.
    	
    	fit_plane performs a weighted planar least-squares regression 
    on the output of fico to generate an 
    output file in remap-compatible format.  This allows for any 
    combination of translate, scale, rotate, and shear transformations.

OPTIONS:
    	<in> an fico offset point file
    	<out> a matrix file in remap compatible format:
    		This file matches the <image 1> to <image 2>, as
	img1.x=img2.x*<first number>+img2.y*<second number>+<third number>
	img1.y=img2.x*<fourth number>+img2.y*<fifth number>+<sixth number>
    		
EXTERNAL ASSOCIATES:none

FILE REFERENCES:none

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0     6/97   O. Lawlor	Modified from my fit_line, to allow
    				 more generality in remap than
    				 calc_deltas and asp would allow.
     "      7/97   D.Corbett	updated version number
    1.1    10/97   O. Lawlor	Added check for enough correlation points.
    1.2     5/98   O. Lawlor	Updated input for new fico.
    1.21    7/01   R. Gens	Added logfile switch
    1.5    12/03   P. Denny	Bring commandline parsing & usage up to
				 our current standard

HARDWARE/SOFTWARE LIMITATIONS: none

ALGORITHM DESCRIPTION:
	(1) Read in source files, mapping fico's SNR->point's weight.
	(2) Find a least-squares planar mapping of the coordiate to the offset.
	(3) Compute the deviation between each point and that mapping.
	(4) Remove the point with the greatest deviation.
	(5) Repeat (2-4) until there are keep_fraction points left.

ALGORITHM REFERENCES:

BUGS: none known

FILE FORMATS:
Input and Output:
	<in>: a correlation point file from fico <img 1> <img 2> .. <ficoOut>
	<out>: a least-squares regression matrix, in remap-compatible format.
		This will be a matrix to map img2 to line up with img1.

******************************************************************************/
/****************************************************************************
*								            *
*   fit_plane performs a weighted planar least-squares regression	    *
*   on the output of fico to generate an output file in remap-compatible    *
*   format.								    *
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

#define VERSION 1.5


FILE *pointOutput=NULL;

void usage(char *name);

/*******************Least Squares Computation********************
System for performing a weighted planar first-order least-squares fit of
  data items.  The code finds the coefficents a, b, and c which make the equation
  out=a*in.x+b*in.y+c
  best fit the given data.  First you call lsc_init with an existing lsc record,
  then add each point with lsc_add_point, then when done call lsc_get_coeffs.
  
Details:
  Minimizes Sum of w*(a*x+b*y+c-out)^2 by taking partials with respect to a, b, and c, then
  setting them equal to zero.  The matrix is a 3 row by 4 column augmented matrix,
  and is equal to:
|-                                            -|  
|  a*Sum(x*x)  b*Sum(x*y)  c*Sum(x)  Sum(o*x)  |
|                                              |
|  a*Sum(x*y)  b*Sum(y*y)  c*Sum(y)  Sum(o*y)  |
|                                              |
|   a*Sum(x)    b*Sum(y)   c*Sum(1)   Sum(o)   |
|-                                            -|
  */
  
typedef struct {
#define m l->matrix
	double matrix[4][3];
	double outA,outB,outC;
	double error;
} least_squares_computation; /* a.k.a. lsc*/

void lsc_init(least_squares_computation *l)
{
	int i,j;
	for (i=0;i<3;i++)
		for (j=0;j<4;j++)
			m[j][i]=0.0;
	l->outA=l->outB=l->outC=0;
}
double d_abs(double x)
{
	if (x<0) return -x;
	else return x;
}
void lsc_add_point(least_squares_computation *l,float x,float y,float o,float w)
{/*x is an input, y is an input, o is the output, and w is the weight of this point (w==0  => ignore) */
	m[0][0]+=x*x*w; m[1][0]+=x*y*w; m[2][0]+=x*w; m[3][0]+=o*x*w; 
	m[0][1]+=x*y*w; m[1][1]+=y*y*w; m[2][1]+=y*w; m[3][1]+=o*y*w; 
	m[0][2]+=x*w;   m[1][2]+=y*w;   m[2][2]+=w;   m[3][2]+=o*w; 
}
void lsc_set_coeffs(least_squares_computation *l)
{/*We have points-- we just need to set the output coeficients now.
   We solve our 4x3 matrix by Gaussian elimination.*/
	int n=3;/*it can also be called a 3x3 augmented matrix.*/
	int pivotCol,pivotRow=0,x,y;
	double pivotVal,scale,temp;
	for (pivotCol=0;pivotCol<n;pivotCol++)
	{
		/*First, be must pick a pivot.  We pick the pivot value which
		has the largest absolute value.*/
		pivotVal=0.0;
		for (y=pivotCol;y<n;y++)
		{
			if (d_abs(m[pivotCol][y])>pivotVal)
			{
				pivotVal=d_abs(m[pivotCol][y]);
				pivotRow=y;
			}
		}
		/*If we found an acceptable pivot...*/
		if (d_abs(pivotVal)>0.000001)
		{
			if (pivotCol!=pivotRow)
			   /*...then we need to do a row interchange- swap row(pivotRow) and row(pivotCol).*/
				for (x=0;x<=n;x++)
				{
					temp=m[x][pivotCol];
					m[x][pivotCol]=m[x][pivotRow];
					m[x][pivotRow]=temp;
				}
			pivotRow=pivotCol;
			/*Now we scale the pivot row by the 1.0/(the pivot value).*/
			scale=1.0/m[pivotCol][pivotRow];
			for (x=pivotCol;x<=n;x++)
				m[x][pivotRow]*=scale;
			/*Now we subtract the pivot row from each remaining row.*/
			for (y=0;y<n;y++)
				if (y!=pivotRow)
				{
					scale=m[pivotCol][y];
					for (x=pivotCol+1;x<=n;x++)
						m[x][y]-=m[x][pivotRow]*scale;
					m[pivotCol][y]=0;
				}
		}
	}
	l->outA=m[3][0];
	l->outB=m[3][1];
	l->outC=m[3][2];
	/*Now the data best fits the plane  out=outA*x+outB*y+outC  */
}
void lsc_print(least_squares_computation *l)
{
	int i,j;
	for (i=0;i<3;i++)
	{
		for (j=0;j<4;j++)
			printf("   %f ",(float)m[j][i]);
		printf("\n   ");
	}
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
	float x,y,dx,dy,w;
	int *next;
} point;

typedef struct {
	point *pts;
	int numPoints;
	least_squares_computation dx,dy;

	double minError;
	int minNumPoints;
	char minCoeffs[250];
	FILE *coeffList;
} fit_line;
void fit_init(fit_line *f)
{
	f->numPoints=0;
	f->pts=NULL;
	lsc_init(&f->dx);
	lsc_init(&f->dy);
	f->dx.error=-1;
	f->dy.error=-1;
	f->minError=1000000000000.0;
	f->minNumPoints=-1;
	f->coeffList=NULL;
	sprintf(f->minCoeffs,"Error in fit_plane: never called 'fit_refine_and_remove'!\n");
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
	lsc_init(&f->dx);
	lsc_init(&f->dy);
	while (pt!=NULL)
	{
		lsc_add_point(&f->dx,pt->x,pt->y,pt->dx,pt->w);
		lsc_add_point(&f->dy,pt->x,pt->y,pt->dy,pt->w);
		pt=(point *)pt->next;
	}
	lsc_set_coeffs(&f->dx);
	lsc_set_coeffs(&f->dy);
}
void fit_refine_and_remove(fit_line *f)
{
	double thisError;
	char tempCoeffs[255];
	point *pt=f->pts,*prev=NULL;
	point *maxPt=NULL,*maxPrev=NULL;
	float maxDist=-1,xa,xb,xc,ya,yb,yc;
	/*calculate coefficents*/
	fit_calc_data(f);
	xa=f->dx.outA,xb=f->dx.outB,xc=f->dx.outC;
	ya=f->dy.outA,yb=f->dy.outB,yc=f->dy.outC;
	f->dx.error=0;
	f->dy.error=0;
	/*identify the point which is the maximum distance from conformance*/
	while (pt)
	{
		register float dx=pt->dx-(pt->x*xa+pt->y*xb+xc);
		register float dy=pt->dy-(pt->x*ya+pt->y*yb+yc);
		register float dist;
		dx*=dx;dy*=dy;
		f->dx.error+=dx;
		f->dy.error+=dy;
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
	f->dx.error=sqrt(f->dx.error/(f->numPoints-3));/*Since we estimate 3 parameters (A, B, and C), we lose 3 DOF.*/
	f->dy.error=sqrt(f->dy.error/(f->numPoints-3));
	thisError=f->dx.error+f->dy.error;
	sprintf(tempCoeffs,"   %20.16f %20.16f %20.16f \n   %20.16f %20.16f %20.16f %d\n",
		f->dx.outA,f->dx.outB,f->dx.outC,
		f->dy.outA,f->dy.outB,f->dy.outC,f->numPoints);
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
	printf("Program: fit_plane\n");
	printf("\n   Number of grid points: %i\n\n   Coefficients:\n",f->numPoints);
	printf("   <img out>.x=%20.16f*<img in>.x+%20.16f*<img in>.y+%20.16f\n",
			f->dx.outA,f->dx.outB,f->dx.outC);
	printf("   <img out>.y=%20.16f*<img in>.x+%20.16f*<img in>.y+%20.16f\n",
			f->dy.outA,f->dy.outB,f->dy.outC);
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: fit_plane\n");
	  sprintf(logbuf,"\n   Number of grid points: %i\n\n",f->numPoints);
	  printLog(logbuf);
	  sprintf(logbuf,"   <img out>.x=%20.16f*<img in>.x+%20.16f*<img in>.y+%20.16f\n",
				f->dx.outA,f->dx.outB,f->dx.outC);
	  printLog(logbuf);
	  sprintf(logbuf,"   <img out>.y=%20.16f*<img in>.x+%20.16f*<img in>.y+%20.16f\n",
				f->dy.outA,f->dy.outB,f->dy.outC);
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
			fprintf(pointOutput,"%5i %8.5f %8.5f %12.10f %12.10f\n",n++,pt->x,pt->y,pt->dx,pt->dy);
			pt=(point *)pt->next;
		}
	}
}
int main(int argc,char **argv)
{
	char lineForward[255];
	FILE *inForward,*out;
	int totalPoints=0;
	double keepFraction=0.8;
	fit_line f;

	fit_init(&f);

/* Parse command line arguments */
	logflag=quietflag=FALSE;
	while (currArg < (argc-2)) {
	   char *key = argv[currArg++];
	   if (strmatch(key,"-log")) {
	      CHECK_ARG(1);
	      strcpy(logFile,GET_ARG(1));
	      fLog = FOPEN(logFile, "a");
	      logflag=TRUE;
	   }
	   else if (strmatch(key,"-k")) {
	      CHECK_ARG(1);
	      keepFraction = strtod(GET_ARG(1),NULL);
	   }
	/*** Begin hidden arguments: c (coefficents) or p (points) ***/
	   else if (strmatch(key,"-c")) {
	      CHECK_ARG(1);
	      f.coeffList = FOPEN(GET_ARG(1),"w");
	   }
	   else if (strmatch(key,"-p")) {
	      CHECK_ARG(1);
	      pointOutput = FOPEN(GET_ARG(1),"w");
	   }
	/*** End hidden arguments: c (coefficents) or p (points) ***/
 	   else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < 2) {printf("Insufficient arguments.\n"); usage(argv[0]);}

	inForward = FOPEN(argv[currArg++],"r");
	out       = FOPEN(argv[currArg],"w");

/*add points to fitline*/
	while (NULL!=(fgets(lineForward,255,inForward)))
	{
		point *pt=(point *)MALLOC(sizeof(point));
		pt->w=1.0;
		sscanf(lineForward,"%f%f%f%f%f",&pt->dx,&pt->dy,&pt->x,&pt->y,&pt->w);
		fit_add_point(&f,pt);
		totalPoints++;
	}
/*output least-squares results*/
	fit_print(&f);
	fit_outputPoints(&f);
	fit_refine_and_remove(&f);
	while(f.numPoints>totalPoints*keepFraction&&f.numPoints>=6)
	{
		fit_refine_and_remove(&f);
		/*if ((f.numPoints<100)||(f.numPoints%50==0))
		{
			f.numPoints++;
			fit_print(&f);
			f.numPoints--;
		}*/
	}
	printf("\n   Number of points after regression: %i\n\n   Coefficients:\n", f.minNumPoints);
        printf("   <img out>.x=%20.16f*<img in>.x+%20.16f*<img in>.y+%20.16f\n",
                        f.dx.outA,f.dx.outB,f.dx.outC);
        printf("   <img out>.y=%20.16f*<img in>.x+%20.16f*<img in>.y+%20.16f\n\n",
                        f.dy.outA,f.dy.outB,f.dy.outC);
	if (logflag) {
	  sprintf(logbuf,"\n   Number of points after regression: %i\n\n   Coefficients:\n", f.minNumPoints);
	  printLog(logbuf);
          sprintf(logbuf,"   <img out>.x=%20.16f*<img in>.x+%20.16f*<img in>.y+%20.16f\n",
                        f.dx.outA,f.dx.outB,f.dx.outC);
	  printLog(logbuf);
          sprintf(logbuf,"   <img out>.y=%20.16f*<img in>.x+%20.16f*<img in>.y+%20.16f\n\n",
                        f.dy.outA,f.dy.outB,f.dy.outC);
	  printLog(logbuf);
	}
	fprintf(out,"%s",f.minCoeffs);
	return 0;
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-k <keepFraction>] [-log <file>] <in> <out>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <in>   a correlation point file (from fico)\n"
	"   <out>  a remap-compatible matrixFile, derived from a least-squares\n"
	"           fit of the input data, with point elimination.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -k <keepFraction> Specifies some fraction of points to not throw away.\n"
	"                       The default is 0.8 (keep 80%% of the points).\n"
	"   -log <file>       Option to have the output written to a log file.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Takes correlation points from fico as an input and produces correlation\n"
	"   coefficients in <out>. This coefficient matrix can be used with the -matrix\n"
	"   option of remap to map image 2 to line up with image 1.\n");
 printf("\n"
	"Version %4.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}

