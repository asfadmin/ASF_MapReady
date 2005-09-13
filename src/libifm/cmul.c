/****************************************************************
FUNCTION NAME: Cmul

SYNTAX: Cmul(a,b);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    a           Complex         multiplier
    b           Complex         multiplier

DESCRIPTION:
	Takes two complex variables and multiplies them together.

RETURN VALUE:
        Returns a complex variable.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0  - Mike Shindle & Rob Fatland. Original development.
****************************************************************/
#include "ifm.h"

complexFloat Cmul(complexFloat a, complexFloat b)
{
  complexFloat x;
  x.real = a.real*b.real - a.imag*b.imag;
  x.imag = a.real*b.imag + a.imag*b.real;
  return x;
}

