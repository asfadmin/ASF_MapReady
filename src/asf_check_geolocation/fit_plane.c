#include "asf.h"
#include "asf_check_geolocation.h"

#define VERSION 1.5
#define m l->matrix

static FILE *pointOutput=NULL;

typedef struct {
  double matrix[4][3];
  double outA,outB,outC;
  double error;
} least_squares_computation; /* a.k.a. lsc*/
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
{/*x is an input, y is an input, o is the output, and w is the weight of this point 
   (w==0  => ignore) */
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
	    /*...then we need to do a row interchange- swap row(pivotRow) 
	      and row(pivotCol).*/
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
  f->dx.error=sqrt(f->dx.error/(f->numPoints-3));/*Since we estimate 3 parameters 
						   (A, B, and C), we lose 3 DOF.*/
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
	  fprintf(pointOutput,"%5i %8.5f %8.5f %12.10f %12.10f\n",
		  n++,pt->x,pt->y,pt->dx,pt->dy);
	  pt=(point *)pt->next;
	}
    }
}

void fit_plane(char *inFile, char *outFile)
{
  char lineForward[255];
  FILE *inForward,*out;
  int totalPoints=0;
  double keepFraction=1.0;
  fit_line f;
  
  fit_init(&f);
  
  /* open files */  
  inForward = FOPEN(inFile, "r");
  out       = FOPEN(outFile, "w");
  
  /*add points to fitline*/
  while (NULL!=(fgets(lineForward,255,inForward)))
    {
      point *pt=(point *)MALLOC(sizeof(point));
      pt->w=1.0;
      sscanf(lineForward,"%f%f%f%f",&pt->dx,&pt->dy,&pt->x,&pt->y);
      fit_add_point(&f,pt);
      totalPoints++;
    }

  /*output least-squares results*/
  //fit_print(&f);
  fit_outputPoints(&f);
  fit_refine_and_remove(&f);
  while(f.numPoints>totalPoints*keepFraction&&f.numPoints>=6)
    {
      fit_refine_and_remove(&f);
    }
  if (!quietflag)
    printf("   Number of points after regression in least-square fit: %i\n\n", 
	   f.minNumPoints);
  fprintf(out,"%s",f.minCoeffs);

  FCLOSE(inForward);
  FCLOSE(out);
}

