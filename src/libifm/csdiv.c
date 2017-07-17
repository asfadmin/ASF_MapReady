/****************************************************************
FUNCTION NAME: Csdiv

SYNTAX: complexFloat Csdiv(s,a);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    s		double		scalar to divide complex number by.
    a		complexFloat         complex number

DESCRIPTION:
    Divides a complex number by a scalar.

RETURN VALUE:
    A complex variable that contains the result to the above division.
    The function will Exit() if division by zero occurs.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
    1.0 - Mike Shindle & Rob Fatland - Original Development
****************************************************************/
#include "ifm.h"
#include <stdbool.h>

complexFloat Csdiv(float s, complexFloat a) {
   complexFloat x;

   if (s == 0.0)
     Exit("Csdiv(): scalar divisor == 0.0");
   
   x.real = a.real/s;
   x.imag = a.imag/s;

   return x;
}

