/*******************************************************************************
NAME:    c_pxswap

PURPOSE: Swap bytes of an input array

HISTORY:
Programmer       Date   Reason
---------------  -----  --------------------------------------------------------
D.Gordon         05/85  Original development for NEWLAS
J.Reed           11/85  Optimized byte portion of routine.
B.Ailts          12/87  Changed include directory specifications.
                        Use raw 'C' types.
                        Placed bridge routines in a seperate file.
B.Ailts          04/88  Replaced newlas.h with worgen.h
T.Baltzer        09/90  Add c_errmsg call prior to returning E_FAIL
J.Fenno          04/92  Optimized code.  Removed the swap function that was
                        called for each element that was to be swapped.
			Speed up of 30% to 55%

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
         Part of the LAS system.

PROJECT: LAS

*******************************************************************************/
#include "worgen.h"


lasErr FUNCTION c_pxswap(unsigned char *buf, int ns, int size)
{
register unsigned char *bottom;
register unsigned char *top;
register unsigned char temp;
register int count;


if (size == 1)
   return(E_SUCC);
else if ((size % 2) != 0)
   {
   c_errmsg("Odd number of bytes per entity specified",
	    "pxswap-oddnum", LAS_FATAL);
   return(E_FAIL);
   }
else
   {
   count = ns;
   for (; count--; buf += size)
      {
      bottom = buf;
      top    = bottom + size - 1;
      while (bottom < top)
	 {
	 temp = *bottom;
	 *(bottom++) = *top;
	 *(top--) = temp;
	 }
      }
   }
return(E_SUCC);
}
