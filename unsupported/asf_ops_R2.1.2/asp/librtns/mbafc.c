/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbafc.c */

#include <aspdecl.h>

/* fc (start, end, beg_cnt, cnt_inc, end_cnt, addr_inc) ----------------
	This routine fills the range of words from start to end with a
	counter value.  The value starts at beg_cnt, increments by
	cnt_inc, and recycles at end_cnt.  addr_inc gives the address
	increment in words.  If addr_inc is 0, it is set to 2.
*/
fc (start, end, beg_cnt, cnt_inc, end_cnt, addr_inc)
int start, end, beg_cnt, cnt_inc, end_cnt, addr_inc;
{
	int i, js;

/*
printf("FC: %x %x %x %x %x %x\n",
	start, end, beg_cnt, cnt_inc, end_cnt, addr_inc );
*/
	if (addr_inc == 0) addr_inc = 2;
	js = beg_cnt;
	start >>= 1;
	end >>= 1;
	for (i = start; i < end; i += addr_inc) {
	    mb.w[i] = js;
	    js += cnt_inc;
	    if (cnt_inc > 0) {	/* chk for end (going up) */
		if (js >= end_cnt) js = beg_cnt;
	    } else {		/* chk for end (going down) */
		if (js <= end_cnt) js = beg_cnt;
	    }
	}
}
