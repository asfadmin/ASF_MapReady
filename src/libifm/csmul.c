/****************************************************************
FUNCTION NAME: Csmul

SYNTAX: complexFloat Csmul(s,b);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    s           double          scalar value
    b           Complex         multiplier

DESCRIPTION:
	Multiplies a complex number by a scalar.

RETURN VALUE:
        Returns a complex variable.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0  - Mike Shindle & Rob Fatland. Original development.
****************************************************************/
#include "ifm.h"

complexFloat Csmul(float s, complexFloat b)
{
  complexFloat x;
  x.real = s * b.real;
  x.imag = s * b.imag;
  return x;
}

complexDouble Csmul_d(double s, complexDouble b)
{
  complexDouble x;
  x.real = s * b.real;
  x.imag = s * b.imag;
  return x;
}
