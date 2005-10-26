#include "asf.h"
#include "asf_proj2slant_range.h"

#define VERSION 1.25

FILE *pointOutput=NULL;

void usage(char *name);
double get_term(int termNo,double x,double y);
poly_2d *find_poly(int degree,const double *out, const double *x,
                            const double *y, int numPts);

void fit_poly(char *inName, int degree, char *outName)
{
#define MAX_PTS 10000
  double	
    *outx=(double *)MALLOC(sizeof(double)*MAX_PTS),
    *outy=(double *)MALLOC(sizeof(double)*MAX_PTS),
    *inx=(double *)MALLOC(sizeof(double)*MAX_PTS),
    *iny=(double *)MALLOC(sizeof(double)*MAX_PTS);
  int   pointNo;
  char  line[255];
  FILE *in, *out;
  
  /* Open input and output files */
  in = FOPEN(inName, "r");
  out = FOPEN(outName, "w");
  
  /*Read in points*/
  pointNo=0;
  while (NULL!=(fgets(line, 255, in)))
    {
      if (4!=sscanf(line,"%lf%lf%lf%lf",
		    &outx[pointNo],&outy[pointNo],
		    &inx[pointNo],&iny[pointNo]))
	{
	  sprintf(errbuf, "   ERROR: Couldn't read line %d of"
		  " tie point file %s!\n",pointNo+1,inName);
	  printErr(errbuf);
	}
      pointNo++;
    }
  FCLOSE(in);
  
  {/*create & write out least-squares results*/
    poly_2d *fwX,*fwY,*bwX,*bwY;
    fwX=find_poly(degree,outx,inx,iny,pointNo);poly_write(fwX,out);
    fwY=find_poly(degree,outy,inx,iny,pointNo);poly_write(fwY,out);
    bwX=find_poly(degree,inx,outx,outy,pointNo);poly_write(bwX,out);
    bwY=find_poly(degree,iny,outx,outy,pointNo);poly_write(bwY,out);
    
    /*Print out residuals for forward mapping*/
    {
      int i;
      double maxErr=0;
      for (i=0;i<pointNo;i++)
	{
	  double dx=outx[i]-poly_eval(fwX,inx[i],iny[i]);
	  double dy=outy[i]-poly_eval(fwY,inx[i],iny[i]);
	  double totalErr=sqrt(dx*dx+dy*dy);
	  if (totalErr>maxErr) maxErr=totalErr;
	}
      printf("   Maximum error of polynomial fit: %.2f pixels\n\n",maxErr);
      if (logflag) {
	sprintf(logbuf, "   Maximum error of polynomial fit: %.2f pixels\n\n",
		maxErr);
	printLog(logbuf);
      }
    }
    FCLOSE(out);
  }
}

/*Fit a poly warping function to the given points
  in a least-squares fashion*/
poly_2d *find_poly(int degree,const double *out, const double *x,
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


/************************************************************
Implementation file for C matrix subroutines. 

Orion Sky Lawlor 3/99
*/

/*A #define to make accessing matrix coefficients easier*/
#define m this->coeff

matrix *matrix_alloc(int rows,int columns)
{
  int row,col;
  matrix *this=(matrix *)MALLOC(sizeof(matrix));
  this->rows=rows;
  this->columns=columns;
  this->coeff=(double **)MALLOC(sizeof(double *)*rows);
  for (row=0;row<rows;row++)
    {
      m[row]=(double *)MALLOC(sizeof(double)*columns);
      for (col=0;col<columns;col++)
	m[row][col]=0.0;/*Initialize to zero*/
    }
  return this;
}

matrix *matrix_dup(const matrix *source)
{
  int r,c;
  matrix *dup=matrix_alloc(source->rows,source->columns);
  for (r=0;r<dup->rows;r++)
    for (c=0;c<dup->columns;c++)
      dup->coeff[r][c]=source->coeff[r][c];
  return dup;
}

void matrix_free(matrix *doomed)
{
  int row;
  for (row=0;row<doomed->rows;row++)
    FREE(doomed->coeff[row]);
  FREE(doomed->coeff);
  doomed->coeff=NULL;
  doomed->rows=-1;
  doomed->columns=-1;
  FREE(doomed);
}

void matrix_print(matrix *this,const char *message,FILE *stream)
{
  int r,c;
  fprintf(stream,"%s (%d rows/%d columns)\n",message,
	  this->rows,this->columns);
  for (r=0;r<this->rows;r++)
    {
      for (c=0;c<this->columns;c++)
	fprintf(stream,"%f\t",m[r][c]);
      fprintf(stream,"\n");
    }
}

/*Row operations: Swap rows A and B.*/
void matrix_rowSwap(matrix *this,int A,int B)
{
  double *row_tmp=m[A];
  m[A]=m[B];
  m[B]=row_tmp;
}

/*Row operations: multiply [row dest] by scale*/
void matrix_rowScale(matrix *this,int dest,double scale)
{
  int col;
  for (col=0;col<this->columns;col++)
    m[dest][col]*=scale;
}

/*Row operations: add scale*[row source] to [row dest]*/
void matrix_rowAddScale(matrix *this,int dest,double scale,int source)
{
  int col;
  for (col=0;col<this->columns;col++)
    m[dest][col]+=scale*m[source][col];
}


/*Solve the given (columns>rows) matrix
by Gaussian Elimination.  Exits with error if
given matrix is singular.*/
void matrix_solve(matrix *this)
{
  int row,pivotRow,pivotCol;
  if (this->columns<=this->rows) {
    sprintf(errbuf, "   ERROR: Matrix passed to matrix_solve doesn't have"
	    " enough columns!\n");
    printErr(errbuf);
  }
  for (pivotCol=0;pivotCol<this->rows;pivotCol++)
    {
      /*Search for a nonzero pivot for this column*/
      pivotRow=pivotCol;/*Start search at pivotCol*/
      while (fabs(m[pivotRow][pivotCol])<0.00000000001)
	{
	  pivotRow++;
	  if (pivotRow>=this->rows) {
	    sprintf(errbuf, "   ERROR: Matrix passed to matrix_solve is singular--\n"
		    "   Cannot find a pivot for column %d!\n",pivotCol);
	    printErr(errbuf);
	  }
	}
      /*Now that we've found a pivot row, swap it into place*/
      matrix_rowSwap(this,pivotRow,pivotCol);
      pivotRow=pivotCol;
      
      /*Scale the pivot row so the pivot has value 1.0*/
      matrix_rowScale(this,pivotRow,1.0/m[pivotRow][pivotCol]);
      
      /*Subtract the pivot from every other row*/
      for (row=0;row<this->rows;row++)
	if (row!=pivotRow)
	  matrix_rowAddScale(this,
			     row,-m[row][pivotCol],pivotRow);
    }
}


/*********************************
Implementation for quadratic 2D function 
utility routines.
Orion Sky Lawlor, 3/99
*/


/*Return the value of the i'th term of this polynomial.
  E.g., term 2 is y; term 4 is x*y, etc.
 Degree  TermNo     Reduced    Value
 0       0          0          1
 1       1 2        0 1        x    y
 2       3 4 5      0 1 2      x^2  x*y    y^2
 3       6 7 8 9    0 1 2 3    x^3  x^2*y  x*y^2  y^3
 ...
*/
double poly_term(int termNo,double x,double y)
{
  int degree=0;
  int reduced=termNo;
  /* Search to find the term's degree */
  while (reduced>degree) {
    degree++;
    reduced-=degree;
  }
  /* TermNo now gives number within row */
  return pow(x,degree-reduced)*pow(y,reduced);
}


/* Allocate a polynomial of this degree */
poly_2d *poly_allocate(int degree)
{
  int d,nTerms;
  poly_2d *ret=(poly_2d *)malloc(sizeof(poly_2d));
  ret->degree=degree;
  nTerms=0;
  for (d=0;d<=degree;d++) nTerms+=1+d;
  ret->nTerms=nTerms;
  ret->v=(double *)malloc(sizeof(double)*nTerms);
  return ret;
}

/* Delete this polynomial */
void poly_delete(poly_2d *c) {
  free(c->v);
  c->v=NULL;
  free(c);
}


/*Evaluate quadratic warp at given location*/
double poly_eval(const poly_2d *c,double x,double y)
{
  double ret=0;
  int t;
  for (t=0;t<c->nTerms;t++)
    ret+=c->v[t]*poly_term(t,x,y);
  return ret;
}

void poly_write(const poly_2d *c,FILE *stream)
{
  int t;
  fprintf(stream,"%d     ",c->degree);
  for (t=0;t<c->nTerms;t++)
    fprintf(stream,"%.18g  ",c->v[t]);
  fprintf(stream,"\n");
}

poly_2d *poly_read(FILE *stream)
{
  poly_2d *c=NULL;
  int degree=-1;
  int t;
  if (1!=fscanf(stream,"%d",&degree)) goto bad;
  c=poly_allocate(degree);
  for (t=0;t<c->nTerms;t++)
    if (1!=fscanf(stream,"%lg",&c->v[t]))
      goto bad;
  return c;
 bad:
  /*Some input error has occured*/
  fprintf(stderr,"Couldn't read a polynomial from the given file\n");
  exit(1);
}
