/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbam.c -- move words within the mb array */

#include <aspdecl.h>

/* m (s1, e1, s2, e2) --------------------------------------------------
	This routine moves a string of words from one location
	to another in the mb array.  If the source (s1 to e1) is
	shorter than the destination (s2 to e2), the source is reused
	cyclically.
*/
m (s1, e1, s2, e2)
int s1, e1, s2, e2;
{
	register unsigned short int *s,*d;
	register unsigned *s4,*d4;

	if ((s1 & 2) || (e1 & 2) || (s2 & 2) || (e2 & 2)) {
    /* use 2-byte moves */
	    s = &mb.w[s1 >> 1];
	    d = &mb.w[s2 >> 1];
	    while (d < &mb.w[e2 >> 1]) {
		*d++ = *s++;
		if (s >= &mb.w[e1 >> 1])
		    s = &mb.w[s1 >> 1];
	    }
	}
	else {
    /* use 4-byte moves */
	    s4 = &mb.u[s1 >> 2];
	    d4 = &mb.u[s2 >> 2];
	    while (d4 < &mb.u[e2 >> 2]) {
		*d4++ = *s4++;
		if (s4 >= &mb.u[e1 >> 2])
		    s4 = &mb.u[s1 >> 2];
	    }
	}
}
