/****************************************************************
FUNCTION NAME: oversamp2dCpx

SYNTAX:  void oversamp2dCpx(vin,vout,dim,os);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    vin		Complex * 	input vector
    vout	Complex *	oversample output vector
    dim		int		dimension of input box
    os		int 		oversample factor

DESCRIPTION:
    This function works as a front end to the oversampling function os2d().
    os2d() will modify the incoming vector. This function copies the incoming
    vector and passes that to os2d(). This allows the main program to still
    have a copy of the unmodified input vector.

RETURN VALUE:
   vout which is created by os2d().

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
   1.0 - Mike Shindle & Rob Fatland - original development
****************************************************************/
#include "asf.h"

#include "ifm.h"

void oversamp2dCpx(FComplex *vin, FComplex *vout, int dim, int os)
{
  int     vsize;
  FComplex *vtmp;

  vsize = dim*dim;
  vtmp  = (FComplex *)(MALLOC(vsize*sizeof(FComplex)));
  /* printf("oversamp2dCpx(): allocated memory\n"); */
  
  copyVector(vin, vtmp, COMPLEX, COMPLEX, vsize);
  /* printf("oversamp2dCpx(): copied vector\n"); */

  os2d(vtmp, vout, dim, os);
  /* printf("oversamp2dCpx(): oversampling complete\n"); */

  free (vtmp);
  return;
}
