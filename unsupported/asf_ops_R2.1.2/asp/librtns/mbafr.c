/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbafr.c */

#include <math.h>
#include <aspdecl.h>

/* fr (start, end, incr, stimno) ---------------------------------------
	This routine places a set of random values in the mb array,
	starting at start, ending at end, setting every incr'th word.
	If stimno = 0, the next sequential set of random values is
	used.  If stimno is positive, the stimno'th set (from the start
	of random number generation) is used.
*/
fr (start, end, incr, stimno)
int start, end, incr, stimno;
{
	static int first = 1;
	static char state[256];
	unsigned short int *i, *iend;
	int n;

	if (incr <= 0) incr = 2;
    /* reset the random number generator to start, if needed */
	if (first || (stimno > 0)) {
	    srandom(1);
	    initstate (1,state,256);
	    first = 0;
	}

    /* spin forward to the correct place in sequence */
	if (stimno > 1) {
	    while (--stimno) {
		n = ((end - start) >> 1) / incr;
		while (n--) random();
	    }
	}

    /* copy the random numbers to the output area */
	iend = &mb.w[end >> 1];
	for (i = &mb.w[start >> 1]; i < iend; i += incr) *i = random();
}
