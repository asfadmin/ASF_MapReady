/****************************************************************
FUNCTION NAME: wks2dAllocateAndInit() and wks2dFree()


SYNTAX:
   Routines to allocate and initialize workspace

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
1.0 - Mike Shindle - Original development
****************************************************************/

#include "imsl.h"
#include "ifm.h"

/* imsl functions */
extern fftci_();

void 
wks2dAllocateAndInit(int n,
		   float **wff1,
		   float **wff2,
		   float **cpy)
{
  int datsize = sizeof(float);

  *wff1 = (float *)calloc(4*n+32,datsize);
  *wff2 = (float *)calloc(4*n+32,datsize);
  *cpy = (float *)calloc(2*n,datsize);
  if (*wff1 == NULL || *wff2 == NULL || *cpy == NULL)
    Exit("Cannot allocate buffer space for fico.");

  fftci_(&n,*wff1);
  fftci_(&n,*wff2);

  return;
}

void
wks2dFree(float *wff1, float *wff2, float *cpy)
{
  free(wff1);
  free(wff2);
  free(cpy);

  return;
}
