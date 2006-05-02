/******************************************************************************
NAME: fit_poly

SYNOPSIS: fit_poly <fico tie file> <out>

DESCRIPTION:

    fit_poly fits a least-squares planar polynomial mapping to the
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

******************************************************************************/

#include "asf.h"
#include "poly.h"
#include "matrix.h"

static poly_2d *find_poly(int degree,const double *out, const double *x,
			  const double *y, int numPts)
{
	int t;
	poly_2d *c=poly_allocate(degree);
	int nTerms=c->nTerms;
	matrix *m=matrix_alloc(nTerms,nTerms+1);
	int row,col;
	int i;
	/*For each data point, add terms to matrix*/
	for (i=0;i<numPts;i++)
	{
		for (row=0;row<nTerms;row++)
		{
			double partial_Q=poly_term(row,x[i],y[i]);
			for (col=0;col<nTerms;col++)
				m->coeff[row][col]+=partial_Q*poly_term(col,x[i],y[i]);
			m->coeff[row][nTerms]+=partial_Q*out[i];
		}
	}
	/*Now solve matrix to find coefficients*/
	/*matrix_print(m,"\nLeast-Squares Matrix:\n",stdout);*/
	matrix_solve(m);
	for (t=0;t<nTerms;t++)
		c->v[t]=m->coeff[t][nTerms];
	return c;
}

int fit_poly(char * gridFile, int degree, double *maxErr,
	     poly_2d **fwX, poly_2d **fwY, poly_2d **bwX, poly_2d **bwY)
{
#define MAX_PTS 66000
	double	*outx=(double *)MALLOC(sizeof(double)*MAX_PTS),
		*outy=(double *)MALLOC(sizeof(double)*MAX_PTS),
		*inx=(double *)MALLOC(sizeof(double)*MAX_PTS),
		*iny=(double *)MALLOC(sizeof(double)*MAX_PTS);
	int   pointNo;
	char  lineForward[255];
	FILE *inForward;
	
	/* Nab required arguments */
	inForward=FOPEN(gridFile,"r");

	/*Read in points*/
	pointNo=0;
	while (NULL!=(fgets(lineForward,255,inForward)))
	{
		if (4!=sscanf(lineForward,"%lf%lf%lf%lf",
			&outx[pointNo],&outy[pointNo],
			&inx[pointNo],&iny[pointNo]))
		{
			sprintf(errbuf, "   ERROR: Couldn't read line %d of"
			" tie point file %s!\n",pointNo+1,gridFile);
			printErr(errbuf);
		}
		++pointNo;
	}
	FCLOSE(inForward);

	/*create & write out least-squares results*/
	*fwX=find_poly(degree,outx,inx,iny,pointNo);
	*fwY=find_poly(degree,outy,inx,iny,pointNo);
	*bwX=find_poly(degree,inx,outx,outy,pointNo);
	*bwY=find_poly(degree,iny,outx,outy,pointNo);
		
	/*Calculate residuals for forward mapping*/
	{
		int i;
		*maxErr = 0.0;
		for (i=0;i<pointNo;i++)
		{
			double dx=outx[i]-poly_eval(*fwX,inx[i],iny[i]);
			double dy=outy[i]-poly_eval(*fwY,inx[i],iny[i]);
			double totalErr=sqrt(dx*dx+dy*dy);
			if (totalErr>*maxErr) *maxErr=totalErr;
		}
	}
	free(outx);
	free(outy);
	free(inx);
	free(iny);
	return 1;
}
