/****************************************************************
NAME: Czero

SYNOPSIS:
	Complex Czero(void);

DESCRIPTION:
        return a complex variable initialized to zero.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     10/95	M. Shindle - Original Development

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
#include "ifm.h"

complexFloat Czero() {
  complexFloat zero;

  zero.real = 0.0;
  zero.imag = 0.0;

  return zero;
}

