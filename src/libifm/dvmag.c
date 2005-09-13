/****************************************************************
FUNCTION NAME: d_vmag

SYNTAX: double d_vmag(v,n)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    v		double *	pointer to beginning of vector
    n		int		number of elements in vector

DESCRIPTION:
    Return the magnitude of the vector v with number of elements n.

RETURN VALUE:
   Returns the magnitude of the given vector

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - Original Development - M. Shindle
****************************************************************/
#include "asf.h"

double d_vmag (double *v, int n)
{
  int i;
  double m, *vp;

  for (i = 0, m = 0.0, vp = v; i < n; i++, vp++) 
    m += (*vp)*(*vp); 
  return(sqrt(m));
}
