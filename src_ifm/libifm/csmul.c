/****************************************************************
FUNCTION NAME: Csmul

SYNTAX: Complex Csmul(s,b);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    s           double           scalar value
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

FComplex Csmul(float s, FComplex b)
{
  FComplex x;
  x.real = s * b.real;
  x.imag = s * b.imag;
  return x;
}

DComplex Csmul_d(double s, DComplex b)
{
  DComplex x;
  x.real = s * b.real;
  x.imag = s * b.imag;
  return x;
}
