/****************************************************************
FUNCTION NAME: c2p

SYNTAX: int c2p(complexFloat *cpx, float *amp, float *phase, int nitems)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:
    Convert an array of complex data to amp & phase values.
    

    
RETURN VALUE:
    True if success, False if failed.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     10/95        M. Shindle & R. Fatland - Original Development
    1.1      8/96        M. Shindle - Converted to a function

BUGS:

****************************************************************/
#include "asf.h"


#include "ifm.h"


int 
c2p(complexFloat *cpx, float *amp, float *phase, int nitems)
{
  int i;

  /*
   * step through each item, making conversion.
   */
  for(i=0; i<nitems; i++)
    if (cpx[i].real != 0.0 || cpx[i].imag != 0.0) {
      amp[i] = sqrt(SQR(cpx[i].real) + SQR(cpx[i].imag));
      phase[i] = atan2(cpx[i].imag, cpx[i].real);
    } else { 
      amp[i] = 0.0; 
      phase[i] = 0.0; 
    }

  return(TRUE);
}

