/****************************************************************
FUNCTION NAME:  Print functions

SYNTAX:  

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:
	A collection of print routines to display vectors and matricies of
	different types.

RETURN VALUE:
    None.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
   1.0 - Mike Shindle - original development
****************************************************************/
#include "asf.h"
#include "ifm.h"

/* print n-vector in a column */
void 
print_vector(float *a, int nrl, int nrh, char *cmnt)
{
  int i;

  printf("\nvector printout: %s\n", cmnt);
  printf("\n");
  for (i = nrl; i <= nrh; i++){
    printf ("%8.4f \n", a[i]);
  }
  printf ("\n");
  return;
}

/* print m row x n column matrix 'a' to stdout */
void print_matrix(float **a, int nrl, int nrh, int ncl, int nch, char *cmnt)
{
  int i, j;

  printf("\nmatrix printout: %s\n", cmnt);
  for (i = nrl; i <= nrh; i++){
    printf ("\n");
    for (j = ncl; j <= nch; j++) {
      printf ("%8.4f   ", a[i][j]);
    }
  }
  printf("\n\n");
  return;
}

