/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  mbaloopon.c -- Loop test for BOB */

#include <aspdecl.h>

extern int no_break;		/* 0 = ctrl-C has been typed */

/* loopon (rw, addr, ivalue) -------------------------------------------
	This program loops on a single word read or write from/to the 
	ASP, to provide a constant test signal for hardware debugging.

	rw is one of 'r' (read) or 'w' (write), followed optionally
		by 'b' (bytes)
	addr is the mb array address to read or write
	ivalue is the value to write (for writing)
	       - when reading, ivalue > 0 means do not display.

	To exit the loop, type control-C.
*/
loopon (rw, addr, ivalue)
	char rw[2];
	int addr, ivalue;
{
	int bytes;
	int display, mb_addr;
	short int value, prev_value;

    /* pick up the command arguments */
	rw[0] = tolower(rw[0]);
	bytes = (tolower(rw[1]) == 'b');
	value = ivalue;
	display = (ivalue <= 0);
	mb_addr = addr;
	addr &= 0xfffff;
	addr = addr >> 1;
	set_break ();

    /* perform the read (or write) loop */
	if (bytes) {
	    if (rw[0] == 'r') {
		if (display) {
		    asp_read( mb_addr, mb.w[addr], 2 );
		    prev_value = ~mb.w[addr];
		    while (no_break) {	/* read bytes with display */
			asp_read( mb_addr, &mb.w[addr], 2 );
			value = mb.w[addr];
			if (value != prev_value) {
			    prev_value = value;
			    printf("%.4x\n",value & 0xff);
			}
		    }
		} else {
		    while (no_break){	/* read bytes with no display */
			asp_read( mb_addr, &mb.w[addr], 2 );
			value = mb.w[addr];
		    }
		}
	    } else {
		mb.w[addr] = value;
		while (no_break)	/* write bytes */
		    asp_write( mb_addr, &mb.w[addr], 2 );
	    }
	} else {
	    if (rw[0] == 'r') {
		if (display) {
		    asp_read( mb_addr, &mb.w[addr], 2 );
		    prev_value = ~mb.w[addr];
		    while (no_break) {	/* read words with display */
			asp_read( mb_addr, &mb.w[addr], 2 );
			value = mb.w[addr];
			if (value != prev_value) {
			    prev_value = value;
			    printf("%.4x\n",value & 0xffff);
			}
		    }
		} else {
		    while (no_break){	/* read bytes with no display */
			asp_read( mb_addr, &mb.w[addr], 2 );
			value = mb.w[addr];
		    }
		}
	    } else {
		mb.w[addr] = value;
		while (no_break)	/* write bytes */
		    asp_write( mb_addr, &mb.w[addr], 2 );
	    }
	}
}
