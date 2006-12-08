#include "asf.h"

/* This is the Complex Variable declaration that FICO will use */
#include "asf_insar.h"

/*cfft1d: Perform FFT, 1 dimensional:
	dir=0 -> init; 
	dir<0 -> forward; 
	dir>0 -> backward*/
void cfft1d(int n, FCMPLX *c, int dir);
