/*******************************************************************************
NAME				C_UP2LOW

PURPOSE		Lower cases an input string

PROGRAM HISTORY
	PROGRAMMER	DATE	REASON
	----------	----	------
	D.GORDON	5/7/85	Original development for NEWLAS
	D. Hollaren	04/88   changed include spec to las
				removed call to squeeze routine
	B.Ailts		06/88	Changed include spec to worgen.h

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS
	C_UP2LOW must be run under TAE.

PROJECT				LAS

ALGORITHM DESCRIPTION:
   For each character of the string
      If upper case
         Change to lower case

ALGORITHM REFERENCES		none
*******************************************************************************/
#include <ctype.h>

#include "worgen.h"

void FUNCTION c_up2low(register char *buf, register int   *size)
{
register int cnt;
register char *ptr;

cnt = *size;
ptr = buf;
for(; cnt--; ptr++)
   {
   if (isupper(*ptr))
       *ptr = tolower(*ptr);
   }
return;
}
