/*******************************************************************************
NAME				C_LOW2UP

PURPOSE		Upper cases an input string

PROGRAM HISTORY
	PROGRAMMER	DATE	REASON
	----------	----	------
	D.GORDON	5/7/85	Original development for NEWLAS
	D. Hollaren	04/88   changed include spec to las.h
				removed call to squeeze routine
	B.Ailts		06/88	Changed include spec to worgen.h

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS
	C_LOW2UP must be run under TAE.

PROJECT				LAS

ALGORITHM DESCRIPTION:
   For each character of the string
      If lower case
         Change to upper case

ALGORITHM REFERENCES		none
*******************************************************************************/
#include <ctype.h> 
void c_low2up(register char *buf, register int *size);
void c_low2up(register char *buf, register int *size)
{
register int cnt;
register char *ptr;

cnt = *size;
ptr = buf;
for(; cnt--; ptr++)
   {
   if (islower(*ptr))
       *ptr = toupper(*ptr);
   }
return;
}
