/****************************************************************
FUNCTION NAME: Cconj[_d]

SYNTAX:	   Cconj(a);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    a           Complex         Complex number to take conjugate of.

DESCRIPTION:
	Takes the complex conjugate of a number.
        
	Cconj_d is a double version of same function.

RETURN VALUE:
	Returns a Complex variable that is the complex conjugate of the
	specified parameter.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
   1.0 - Mike Shindle & Rob Fatland. Original Development.
****************************************************************/
#include "ifm.h"

complexFloat Cconj(complexFloat a)
{
  complexFloat x;
  x.real = a.real;
  x.imag = -a.imag;
  return x;
}

complexDouble Cconj_d(complexDouble a)
{
  complexDouble x;
  x.real = a.real;
  x.imag = -a.imag;
  return(x);
}

