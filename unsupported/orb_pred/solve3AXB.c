/****************************************************************
FUNCTION NAME:  solveAXB	

SYNTAX:  solveAXB(double a[3][3], x, b, m)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    a		double [3][3]	Matrix
    x		double *	vector to solve for
    b		double *	
    m		int
   
DESCRIPTION:
      Solve the Equation AX=B where A is a 3x3 matrix

RETURN VALUE:
       returns X

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
     1.0	M. Shindle	Original Development
****************************************************************/

void solveAXB(a,x,b,m)
double a[3][3];
double *x;
double *b;
int m;
{
  int kk, j, i;
  double det1, det2, detm1, detm2;
  double z[3][3];

  det1 = a[0][0] * a[1][1] * a[2][2] + a[0][1] * a[1][2] * a[2][0];
  det1 += a[0][2] * a[1][0] * a[2][1];
  
  det2 = a[0][2] * a[1][1] * a[2][0] + a[0][0] * a[1][2] * a[2][1];
  det2 += a[0][1] * a[1][0] * a[2][2];
  detm1 = det1 - det2;

  for (kk = 0; kk < m; kk++) {
     for (j = 0; j < m; j++) {
	for (i = 0; i < m; i++) {
           if (j != kk)
             z[i][j] = a[i][j];
	   else
             z[i][j] = b[i];
	}
     }
     det1 = z[0][0] * z[1][1] * z[2][2] + z[0][1] * z[1][2] * z[2][0];
     det1 += z[0][2] * z[1][0] * z[2][1];
     det2 = z[0][2] * z[1][1] * z[2][0] + z[0][0] * z[1][2] * z[2][1];
     det2 += z[0][1] * z[1][0] * z[2][2];
     detm2 = det1 - det2;
     x[kk] = detm2/detm1;
  }

  return;
}
