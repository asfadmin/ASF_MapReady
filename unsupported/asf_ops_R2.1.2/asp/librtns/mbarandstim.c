/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbarandstim.c -- random stimulus generator for asp */

#include <math.h>
#include <aspdecl.h>

/* randstim (Data, points, seed) ---------------------------------------
	This routine writes a random data set to the Masscomp array
	'Data', 'points' complex points long.  If 'seed' is non-zero,
	the random number generator is reseeded before the set is
	generated.
*/
randstim (Data, points, seed)
DATA Data[];
int points,seed;
{
    int bucket, i;
    static char state[256];

    if (seed) {
	srandom(seed);
	initstate(seed,state,256);
    }
    for (i = 0; i < points; i++) {
	Data[i].rdat = random();
	Data[i].idat = random();
    }
}
