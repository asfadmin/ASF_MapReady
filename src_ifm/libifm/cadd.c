/****************************************************************
NAME: Cadd

SYNOPSIS:
	SIComplex Cadd_sic();
	DComplex Cadd_d();

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

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
#include "ifm.h"


DComplex Cadd_d(DComplex a, DComplex b) {
  DComplex c;

  c.real = a.real+b.real;
  c.imag = a.imag+b.imag;

  return(c);
}

FComplex Cadd(FComplex a, FComplex b) {
  FComplex c;

  c.real = a.real+b.real;
  c.imag = a.imag+b.imag;

  return(c);
}
