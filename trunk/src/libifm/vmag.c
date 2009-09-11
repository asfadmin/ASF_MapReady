/****************************************************************
FUNCTION NAME: vmag

SYNTAX: float vmag(v,n)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    v		float *	pointer to beginning of vector
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

float vmag (float *v, int n)
{
  int i;
  float m, *vp;

  for (i = 0, m = 0.0, vp = v; i < n; i++, vp++) 
    m += (*vp)*(*vp); 
  return( (float)(sqrt(m)) );
}
