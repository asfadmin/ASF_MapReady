/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbac.c -- compare words */

#include <aspdecl.h>

/* c (s1, e1, s2, e2) --------------------------------------------------
	This routine compares two parts of the mb array and
	reports any differences.  If the two are the same, the
	routine returns PASS, otherwise it returns FAIL.

	The data from s1 to e1 is used as a circular list, against which
	the data from s2 to e2 is compared.
*/
c (s1, e1, s2, e2)
int s1, e1, s2, e2 ;
{
	int compare_err ;
	unsigned *pn0, *pn1, *pn2, *pn3, *source_p, *dest_p;
	int i,j,add1,m1a,m1b,add2,m2a,m2b;
	int first = 1;
	int n = 0;

    /* adjust compare parameters if needed */
	if (e2 == 0) e2 = s2 + e1 - s1 ;
    
    /* compare the two arrays using 32-bit compares (for speed) */
        pn0 = mb.u + (s1 >> 2);
        pn1 = mb.u + (e1 >> 2);
        pn2 = mb.u + (s2 >> 2);
        pn3 = mb.u + (e2 >> 2);
	compare_err = 0;
	dest_p = pn2;
	while ((!compare_err) && (dest_p < pn3)) {
	    source_p = pn0;
	    while ((!compare_err) && (source_p < pn1))
	      	compare_err = ((*source_p++) != (*dest_p++));
	}

    /* if difference was found, find diffs and print message */
	if (compare_err) {
	    s1 >>= 1;
	    e1 >>= 1;
	    s2 >>= 1;
	    e2 >>= 1;
	    i = s1;
	    for (j = s2; j < e2; j++) {
		if (mb.w[i] != mb.w[j]) {
		    if (first) {
			add1 = j;
			m1a = mb.w[i];
			m1b = mb.w[j];
			first = 0;
		    }
		    add2 = j;
		    m2a = mb.w[i];
		    m2b = mb.w[j];
		    n++;
		}  /* if mb... */
		if (++i >= e1)
		    i = s1;
	    }  /* for j */
	    printf("%d words differ: first=%x,w1=%.4x,w2=%.4x last=%x,w1=%.4x,w2=%.4x\n",
		n, add1 << 1, m1a & 0xffff, m1b & 0xffff,
		   add2 << 1, m2a & 0xffff, m2b & 0xffff);
	}  /* if compare_err */

    /* return FAIL if different, PASS if same */
	return (compare_err ? FAIL : PASS);
}
