/****************************************************************
FUNCTION NAME: Cabs()

SYNTAX: float Cabs(a);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    a           Complex         value to take absolute value of.

DESCRIPTION:
    Takes the absolute value of a complex number.

RETURN VALUE:
    Returns a float value.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
   1.0 - Mike Shindle & Rob Fatland. Original Dvelopment.
****************************************************************/

#include "ifm.h"

float Cabs(FComplex a)
{
  double x;
  x = sqrt(a.real*a.real + a.imag*a.imag);
  return((float)x);
}

double Cabs_d(DComplex a)
{
  double x;
  x = sqrt(a.real*a.real + a.imag*a.imag);
  return(x);
}
