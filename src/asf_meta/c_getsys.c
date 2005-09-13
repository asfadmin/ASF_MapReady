/******************************************************************************

NAME:	GETSYS

FUNCTION:
	GETSYS retrieves the character data stored in the environment
	variable(logical) DATASYS and returns it to the calling program.

PROGRAM HISTORY:
  Version	Date       Author       Request
  -------	----	   ------       -------
    1.0         10/87       B.Ailts     initial development
    1.1		12/87	    B.Ailts     Change include directory specification
					Replace DESC arguments with char. string
					Place bridge routines in a seperate file
    1.2		04/88	    B.Ailts     Replaced newlas.h with las.h
					Replaced SYSTEM with LASSYS
    1.3		12/90	    B.Ailts     Updated error messages
    1.4          4/91       B.Ailts	Replaced LASSYS with DATASYS
    2.0          6/98		O. Lawlor Added ieee-lil for little-endian IEEE machines.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	
		Run under TAE

PROJECT:	LAS

ALGORITHM:

ALGORITHM REFERENCES:
******************************************************************************/
#include "asf.h"

#include "las.h"
#include "sysdef.h"

FUNCTION char *c_getsys(void)

{
#if defined(lil_ieee)
	return IEEE_LIL;
#elif defined(cray_float)
	return UNICOS;
#else
	/*#if defined(big_ieee)*/
	return IEEE;
#endif 
}

