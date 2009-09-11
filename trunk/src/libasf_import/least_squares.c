#include "asf.h"
#include "least_squares.h"
#include "matrix.h"

/* Routine internal to find_quadratic:
 * Return the value of the given term of the quadratic equation.*/
double get_term(int termNo, double x, double y)
{
  switch(termNo) {
    case 0:/*A*/return 1;
    case 1:/*B*/return x;
    case 2:/*C*/return y;
    case 3:/*D*/return x*x;
    case 4:/*E*/return x*y;
    case 5:/*F*/return y*y;
    case 6:/*G*/return x*x*y;
    case 7:/*H*/return x*y*y;
    case 8:/*I*/return x*x*y*y;
    case 9:/*J*/return x*x*x;
    case 10:/*K*/return y*y*y;
    default:/*??*/
      fprintf(stderr,"Unknown term number %d passed to get_term!\n", termNo);
      exit(EXIT_FAILURE);
  }
}

/* Fit a quadratic warping function to the given points
 * in a least-squares fashion*/
quadratic_2d find_quadratic(const double *out, const double *x,
                            const double *y, int numPts)
{
  int nTerms=11;
  matrix *m=matrix_alloc(nTerms,nTerms+1);
  int row,col;
  int i;
  quadratic_2d c;
  /*For each data point, add terms to matrix*/
  for (i=0;i<numPts;i++) {
    for (row=0;row<nTerms;row++) {
      double partial_Q=get_term(row,x[i],y[i]);
      for (col=0;col<nTerms;col++)
        m->coeff[row][col]+=partial_Q*get_term(col,x[i],y[i]);
      m->coeff[row][nTerms]+=partial_Q*out[i];
    }
  }
  /*Now solve matrix to find coefficients*/
  /*matrix_print(m,"\nLeast-Squares Matrix:\n",stdout);*/
  matrix_solve(m);
  c.A=m->coeff[0][nTerms];  c.B=m->coeff[1][nTerms];  c.C=m->coeff[2][nTerms];
  c.D=m->coeff[3][nTerms];  c.E=m->coeff[4][nTerms];  c.F=m->coeff[5][nTerms];
  c.G=m->coeff[6][nTerms];  c.H=m->coeff[7][nTerms];  c.I=m->coeff[8][nTerms];
  c.J=m->coeff[9][nTerms];  c.K=m->coeff[10][nTerms];
  matrix_free(m);

  return c;
}

void quadratic_write(const quadratic_2d *c,FILE *stream)
{
  fprintf(stream,"%.18f %.18f %.18f %.18f %.18f %.18f %.18f %.18f %.18f %.18f %.18f\n",
          c->A,c->B,c->C,c->D,c->E,c->F,c->G,c->H,c->I,c->J,c->K);
}
