/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbaputdata.c */

#include <aspdecl.h>

/* putdata (Din, start, length) ----------------------------------------
	This routine transfers a Masscomp data array to the mb array.
*/
putdata (Din, start, length)
int Din[];
int start, length;
{
	int l;
	register int *i, *iend;
	register unsigned *j;

	i = Din;
	iend = i + length;
	j = &mb.u[start >> 2];
	while (i < iend) *j++ = *i++;
	if( start>0x80000 ) {
	    l = ( mb.w[0]<<20 ) | start;
	    asp_write( l, &mb.u[start>>2], length<<2 );
	}
}
