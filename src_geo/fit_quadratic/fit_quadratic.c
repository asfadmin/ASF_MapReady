/******************************************************************************
NAME: fit_quadratic

SYNOPSIS: fit_quadratic <fico tie file> <out>

DESCRIPTION:

    fit_quadratic fits a least-squares planar quadratic mapping to the
    given input data.  This is a mapping of the form:
        <out>=A + B*x + C*y + D*x*x + E*x*y + F*y*y

    A mapping is calculated which maps the input points (see below) to
    the output points and back again.  Each mapping is written out.

    In addition, this program lists the fit error it finds for each point.
       		
EXTERNAL ASSOCIATES:none

FILE REFERENCES:none

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0     3/99   O. Lawlor	New geocoding software
    1.1	    7/01   R. Gens	Added log file switch
    1.25    3/02   P. Denny     Updated commandline parsing & usage()

HARDWARE/SOFTWARE LIMITATIONS: 
	Don't run this program directly-- call geocode.

******************************************************************************/
/****************************************************************************
*								            *
*   fit_quadratic -- produces as output quadratic mapping matrix <out>.     *
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
#include "quadratic.h"
#include "matrix.h"

#define VERSION 1.25

FILE *pointOutput=NULL;

void usage(char *name);
double get_term(int termNo,double x,double y);
quadratic_2d find_quadratic(const double *out, const double *x,
                            const double *y, int numPts);

int main(int argc,char **argv)
{
#define MAX_PTS 1000
	double	*outx=(double *)MALLOC(sizeof(double)*MAX_PTS),
		*outy=(double *)MALLOC(sizeof(double)*MAX_PTS),
		*inx=(double *)MALLOC(sizeof(double)*MAX_PTS),
		*iny=(double *)MALLOC(sizeof(double)*MAX_PTS);
	int   pointNo;
	char  lineForward[255];
	FILE *inForward,*out;
	
	logflag=0;	/* from log.h which is in asf.h */
	currArg=1;	/* from cla.h which is in asf.h */

	/* Parse command line args */
	while (currArg < (argc-2))
	{
		char *key=argv[currArg++];
		if (strmatch(key,"-log")) {
			CHECK_ARG(1) /*one string argument: log file */
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			logflag=1;
		}
		else {printf("\n*****Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
	}
	if ((argc-currArg) < 2) {printf("Insufficient arguments.\n"); usage(argv[0]);}

	/* Nab required arguments */
	inForward=FOPEN(argv[currArg],"r");
	out=FOPEN(argv[currArg+1],"w");

	system("date");
	printf("Program: fit_quadratic\n\n");
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: fit_quadratic\n\n");
	}

	/*Read in points*/
	pointNo=0;
	while (NULL!=(fgets(lineForward,255,inForward)))
	{
		if (4!=sscanf(lineForward,"%lf%lf%lf%lf",
			&outx[pointNo],&outy[pointNo],
			&inx[pointNo],&iny[pointNo]))
		{
			sprintf(errbuf, "   ERROR: Couldn't read line %d of"
			" tie point file %s!\n",pointNo+1,argv[1]);
			printErr(errbuf);
		}
		pointNo++;
	}
	printf("   Read %d tie points.\n\n",pointNo);
	if (logflag) {
	  sprintf(logbuf, "   Read %d tie points.\n\n",pointNo);
	  printLog(logbuf);
	}
	FCLOSE(inForward);

	{/*create & write out least-squares results*/
		quadratic_2d fwX,fwY,bwX,bwY;
		fwX=find_quadratic(outx,inx,iny,pointNo);quadratic_write(&fwX,out);
		fwY=find_quadratic(outy,inx,iny,pointNo);quadratic_write(&fwY,out);
		bwX=find_quadratic(inx,outx,outy,pointNo);quadratic_write(&bwX,out);
		bwY=find_quadratic(iny,outx,outy,pointNo);quadratic_write(&bwY,out);
	/*Print out residuals for forward mapping*/
/***		for (i=0;i<pointNo;i++)
 *		{
 *			double dx=outx[i]-quadratic_eval(&fwX,inx[i],iny[i]);
 *			double dy=outy[i]-quadratic_eval(&fwY,inx[i],iny[i]);
 *			double totalErr=sqrt(dx*dx+dy*dy);
 *			printf("Input (%.1f,%.1f) -> output (%.1f, %.1f) Err=%.3f pixels\n",
 *				inx[i],iny[i],outx[i],outy[i],totalErr);
****		} */
		FCLOSE(out);
	}
	return 0;
}



/*Routine internal to find_quadratic:
Return the value of the given term of the quadratic equation.*/
double get_term(int termNo,double x,double y)
{
	switch(termNo)
	{
		case 0:/*A*/return 1;
		case 1:/*B*/return x;
		case 2:/*C*/return y;
		case 3:/*D*/return x*x;
		case 4:/*E*/return x*y;
		case 5:/*F*/return y*y;
		default:/*??*/
			fprintf(stderr,"Unknown term number %d passed to get_term!\n",
				termNo);
			exit(1);
			return 0.0;/*<-- For whining compilers*/
	}
}

/*Fit a quadratic warping function to the given points
in a least-squares fashion*/
quadratic_2d find_quadratic(const double *out, const double *x,
                            const double *y, int numPts)
{
	int nTerms=6;
	matrix *m=matrix_alloc(nTerms,nTerms+1);
	int row,col;
	int i;
	quadratic_2d c;
	/*For each data point, add terms to matrix*/
	for (i=0;i<numPts;i++)
	{
		for (row=0;row<nTerms;row++)
		{
			double partial_Q=get_term(row,x[i],y[i]);
			for (col=0;col<nTerms;col++)
				m->coeff[row][col]+=partial_Q*get_term(col,x[i],y[i]);
			m->coeff[row][nTerms]+=partial_Q*out[i];
		}
	}
	/*Now solve matrix to find coefficients*/
	/*matrix_print(m,"\nLeast-Squares Matrix:\n",stdout);*/
	matrix_solve(m);
	c.A=m->coeff[0][nTerms];c.B=m->coeff[1][nTerms];c.C=m->coeff[2][nTerms];
	c.D=m->coeff[3][nTerms];c.E=m->coeff[4][nTerms];c.F=m->coeff[5][nTerms];
	return c;
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] <in> <out>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <in>  input: a correlation point file (from fico)\n"
	"   <out> output: a remap-compatible quadFile, derived from a\n"
	"        least-squares fit of the input data (without point elimination).\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -log <file> : allows the output to be written to a log file.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Takes, as an input, correlation points from fico and \n"
	"   produces as output quadratic mapping matrix <out>. \n"
	"   This coefficient matrix can be used with the -quad option of remap\n"
	"   to map the input image to line up with the output image.\n");
 printf("\n"
 	"Version %4.2f, ASF SAR TOOLS.\n"
	"\n",VERSION);
 exit(1);
}
