/****************************************************************
FUNCTION NAME: Cphase

SYNTAX:  float Cphase(a)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    a		Complex		Complex variable to compute phase from

DESCRIPTION:
	Compute the phase from a complex variable.

RETURN VALUE:
	The phase as stored in the complex variable a.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
1.0 - M. Shindle & R. Fatland - Original Development
****************************************************************/

#include "ifm.h"

float Cphase(FComplex a)
{
  float real, imag;

  real = (float)(a.real);
  imag = (float)(a.imag);
  if (real == 0.0 && imag == 0.0) return 0.0;
  return( (float)(atan2(imag, real)) );
}

