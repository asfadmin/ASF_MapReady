/****************************************************************
NAME: Cadd

SYNOPSIS:
	complexDouble Cadd_d();
	complexFloat  Cadd();

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     9/96	M. Shindle - Original Development
    1.1    10/96        M. Shindle - Added DComplex version of add
    1.2     6/03        P. Denny   - Change FComplex and DComplex data types
                                       to complexFloat and complexDouble

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
#include "ifm.h"


complexDouble Cadd_d(complexDouble a, complexDouble b) {
  complexDouble c;

  c.real = a.real+b.real;
  c.imag = a.imag+b.imag;

  return(c);
}

complexFloat Cadd(complexFloat a, complexFloat b) {
  complexFloat c;

  c.real = a.real+b.real;
  c.imag = a.imag+b.imag;

  return(c);
}
