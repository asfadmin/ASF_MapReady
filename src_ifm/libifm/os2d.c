/****************************************************************
FUNCTION NAME: os2d

SYNTAX: void os2d(v,vo,dim,os);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    v           Complex *	input vector
    vo          Complex *       oversampling vector
    dim         int             size of 2d vector array
    os          int             oversampling factor

DESCRIPTION:
    Generalized 2D complex oversampling. Destroys input vector v.

RETURN VALUE:
    Oversampled vector output.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
   1.0 - Rob Fatland & Mike Shindle - original development
****************************************************************/
#include "asf.h"
#include "ifm.h"

/* Prototypes */
void fft2d (complexFloat *array, int n, int direction);
void zeroPad(complexFloat *vIn, complexFloat *vOut, int dim, int os);

void os2d(complexFloat *v, complexFloat *vo, int dim, int os)
{
  int i;
  int forward_fft =  1;
  int inverse_fft = -1;
  int osize;

  /* printf("os2d(): dim = %d\tos = %d\n", dim, os); */

  /* calculate total size of oversampled array */
  osize = dim*dim*os*os;

  /* printf("os2d(): total size of oversampled array (osize) = %d\n", osize); */

  /* 2-D fft vector 'v' */
  fft2d (v, dim, forward_fft); 

  /* make sure 'vo' = 0.0 */
  for (i = 0; i < osize; i++) 
     *(vo+i) = Czero();

  /* zero pad 'v' into 'vout' */
  zeroPad(v, vo, dim, os);

  /* inverse FFT 'vo' */
  fft2d (vo, dim*os, inverse_fft); 

  return;
}
