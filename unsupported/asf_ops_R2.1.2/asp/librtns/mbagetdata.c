/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbagetdata.c */

#include <aspdecl.h>

/* getdata (Dout, start, length) ---------------------------------------
	This routine retrieves data in the mb array and places it in a
	Masscomp memory array in HLDATA format.
*/
getdata (Dout, start, length)
int Dout[];
int start, length;
{
	register int *i, *iend;
	register unsigned *j;

	i = Dout;
	iend = i + length;
	j = &mb.u[start >> 2];
	while (i < iend) *i++ = *j++;
}
