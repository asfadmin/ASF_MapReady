/****************************************************************
FUNCTION NAME: matrix_multiply

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:
  Matix Multiplication, c = a x b.
  NR convention:  matrix indices run from [1...row][1...col]
    a is r1 rows x c1 columns,  
    b is c1 rows x c2 columns,
    c is r1 rows x c2 columns.

RETURN VALUE:

SPECIAL CONSIDERATIONS:
   Matrix indices use the NR/Matrix convention of row/col indexing. All
   indices start with 1 instead of zero. See above. Use the matrix/vector
   allocation routines to create suitable matricies for this function.

PROGRAM HISTORY:
   1.0 - Mike Shindle - Original creation
****************************************************************/
void 
matrix_multiply(float **a, float **b, float **c, int r1, int c1, int c2)
{
  int i, j, k;
  float sum;
  
  for (i = 1; i <= r1; i++) {
    for (j = 1; j <= c2; j++) {
      for (k = 1, sum = 0.0; k <= c1; k++) {
        sum += a[i][k]*b[k][j];
      }
      c[i][j] = sum;
    }
  }

  return;
}
