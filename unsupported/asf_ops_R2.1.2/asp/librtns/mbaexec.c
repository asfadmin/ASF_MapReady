/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbaexec.c -- Exec Module routines

   This module contains library routines for Exec Module
   operation using the ASP version.  These routines assume
   that the mb array has already been mapped and that
   clock_speed has been set.

   The following routines are included:

   ex_init() - Initializes the Exec Module
   ex_set_event(count_sync,mask,ptrn,eventval) - Allows registers
	that specify the triggering event to be changed.
   ex_set_delay(delay) - Sets Exec delay which is the
	number of clock periods to continue writing the
	Resp Buffer after a trigger has occurred.
   ex_set_stim_mode(mode) - Set Stim Buffer mode.
   ex_set_resp_mode(mode) - Set Resp Buffer mode.
   ex_set_stim_page(page) - Set Stim Buffer page.
   ex_set_resp_page(page) - Set Resp Buffer page.
   ex_set_sync_page(page) - Set Sync Buffer page.
   ex_set_selftest(status) - Activate or deactivate
	Exec selftest.
   ex_set_reset(reset) - Set reset mode (1 = reset all counters)
   ex_set_zero(zerostim,zeroresp) - Set Zero Stim Out
	or Zero Resp In registers.
   ex_set_wordsout(words) - Set Stim Buffer words out.
   ex_set_codeper(codeper) - Set Stim Buffer # of syncs out.
   ex_set_clock(clockspeed) - Set Clock Speed.
   ex_op(lpow,wordsout) - Set Exec for operation
   ex_load_stim(stim_data,stim_length,stim_addr) -
        Loads stimulus buffer with stim data.
   ex_load_sync(synccode,codeper) - Loads codeper number
	of synccode elements into sync stim memory.
*/

#include <sys/types.h>
#include <aspdecl.h>

#define    RMODE_MEM_READ 0   /* Normal read mode */
#define    RMODE_SYNC_READ 2  /* Normal sync read mode */
#define    RMODE_MEM_WRITE 0  /* Normal write mode */
#define    RMODE_SYNC_WRITE 2 /* Normal sync write mode */
#define    RMODE_RUN_NORM 4   /* Normal run mode */
#define    COUNT_EVENT 1      /* Use exec counter as event */
#define    SYNC_EVENT 0       /* Use sync as event */

static short int    stim_pg, resp_pg;  /* Buffer page addresses */
static short int    sync_pg; /* Sync resp memory page address */

/* ex_init() -----------------------------------------------------------
	This routine initializes the Executive Module developed
        for the ASP.  The module is left in rawreset mode with
	the Stim and Resp Buffers accessed.
*/
ex_init()
{
    int    zeroto, reset, stim_mode,codeper,lpow;
    int    bufstart,wordsout;
    int    zeroti, resp_selftest, resp_mode;
    int    cntrl_selftest, count_sync, maskpow, eventval;
    int    syncmask, syncptrn;
    int   execptrn, delay;

    /* Set up control values for Stimulus Board */
    zeroto = 0;     /* Stim output not zeroed */
    reset = 0x11; /* Reset Exec Module and all counters */
    stim_mode = RMODE_MEM_WRITE;   /* Normal access of Stim Buffer */
    stim_pg = 0;    /* Set Stim Buffer page */
    bufstart = 0;   /* Start stim at addr 0 */
    lpow = 4;       /* Line length of 16 */
    codeper = 0;    /* Synccode repetition of 1 */
    wordsout = 0;   /* Number of words out = 8 */

/* Initialize Stimulus Board */
    setup_ex_stim(zeroto,stim_pg,lpow,reset,stim_mode,codeper,bufstart,wordsout);
    
/* Set up control values for Response Board */
    zeroti = 0;        /* Resp input not zeroed */
    resp_selftest = 0; /* Response Buffer not in selftest mode */
    resp_mode = RMODE_MEM_READ;    /* Normal access of Resp Buffer */
    resp_pg = 0;       /* Set Stim Buffer page */

/* Initialize Response Board */
    setup_ex_resp(zeroti,resp_selftest,resp_pg,resp_mode);

/* Set up control values for Control Board */
    reset = 1;	         /* Reset all counters */
    cntrl_selftest = 0;  /* Take sync input from test bus */
    sync_pg = 0;         /* Set sync Resp memory page */
    count_sync = 1;      /* Use Exec counter pattern for trigger */
    maskpow = 0x0f;      /* Max maskpow */
    execptrn = 0x05;     /* Trigger value = 5 (self test) */ 
    delay = 0x2000;      /* Delay after trigger = 8K */
    eventval = 1;        /* Trigger after 1 event */
    syncmask = 0;        /* Not used in this configuration */
    syncptrn = 0;        /* Not used in this configuration */

/* Initialize Control Board */
    setup_ex_cntrl(reset,cntrl_selftest,sync_pg,count_sync,clock_speed,maskpow,execptrn,delay,eventval,syncmask,syncptrn);

}

/* ex_set_event(count_sync,mask,ptrn,eventval) -------------------------
	This routine allows the registers that specify the
	triggering event to be changed.  count_sync can be
	specified by the defined constants COUNT_EVENT and
	SYNC_EVENT.  mask and ptrn are interpreted according
	to the status of count_sync.  eventval gives the number
	of events to occur before triggering.
*/
ex_set_event(count_sync,mask,ptrn,eventval)
int    count_sync, mask, ptrn, eventval;
{
/*
printf("EX_SET_EVENT: cs=%x(%d) mask=%x ptrn=%x eventval=%x\n",
	count_sync, count_sync, mask, ptrn, eventval );
*/
    asp_read( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 24 );
    if ((eventval & 0xff) < 1) eventval = 1;
    mb.w[RLOC_CNTRL+5] = (ushort) eventval;
    asp_write( (RLOC_CNTRL+5)<<1, &mb.w[RLOC_CNTRL+5], 2 );

    if (count_sync == SYNC_EVENT){	/* Load sync mask registers */

	mb.w[RLOC_CNTRL] &= ~0x0080;
	asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
	mask = (mask & 0xff) << 8;	/* Limit value */
	ptrn &= 0xff;			/* Limit value */
	mb.w[RLOC_CNTRL + 6] = (ushort) ( mask | ptrn );
	asp_write( (RLOC_CNTRL+6)<<1, &mb.w[RLOC_CNTRL+6], 2 );

    } else {				/* Load exec mask registers */

	mask &= 0x1f;   /* Limit value */
	mb.w[RLOC_CNTRL] &= ~0x001f;
	asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
	asp_read( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
	mb.w[RLOC_CNTRL] |= (ushort) (0x0080 | mask);
	asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
	mb.w[RLOC_CNTRL + 1] = (ushort) ( ptrn & 0xffff );
	asp_write( (RLOC_CNTRL+1)<<1, &mb.w[RLOC_CNTRL+1], 2 );
	mb.w[RLOC_CNTRL + 2] = (ushort) (ptrn >> 16);
	asp_write( (RLOC_CNTRL+2)<<1, &mb.w[RLOC_CNTRL+2], 2 );
    }
}

/* ex_set_delay(delay) -------------------------------------------------
	This routine sets the exec delay which is the number
	of clock periods to continue writing the Resp Buffer
	after a trigger has occurred.
*/
ex_set_delay(delay)
int    delay;
{
/*
printf("EX_SET_DELAY: delay=%x(%d)\n", delay, delay );
*/
    mb.w[RLOC_CNTRL + 3] = (ushort) ( delay & 0xffff );
    mb.w[RLOC_CNTRL + 4] = (ushort) ( (delay >> 16)&0xff );
    asp_write( (RLOC_CNTRL+3)<<1, &mb.w[RLOC_CNTRL+3], 4 );
}

/* ex_set_stim_mode(mode) ----------------------------------------------
	This routine sets the Stim Buffer mode.
*/
ex_set_stim_mode(mode)
int    mode;
{
    asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    mode &= 0x7;   /* Limit mode value */
    mb.w[RLOC_STIM] = (ushort) ( (mb.w[RLOC_STIM] & 0xfff8) | mode );
/*
printf("EX_SET_STIM_MODE new STIM[0] value:  %04x\n", 0xffff&mb.w[RLOC_STIM]);
*/
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
}

/* ex_set_resp_mode(mode) ----------------------------------------------
	This routine sets the Resp Buffer mode.
*/
ex_set_resp_mode(mode)
int    mode;
{
    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
    mode &= 0x7;   /* Limit mode value */
    mb.w[RLOC_RESP] = (ushort) ( (mb.w[RLOC_RESP] & 0xfff8) | mode );
    asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
}

/* ex_set_stim_page(page) ----------------------------------------------
	This routine sets the stimulus buffer page.
*/
ex_set_stim_page(page)
int    page;      /* Stimulus Buffer page address */
{
    asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    page &= 0x3f;  /* Limit page value */
    mb.w[RLOC_STIM] = (ushort) ( (mb.w[RLOC_STIM] & 0xc0ff) | (page << 8));
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
}

/* ex_set_resp_page(page) ----------------------------------------------
	This routine sets the response buffer page.
*/
ex_set_resp_page(page)
int    page;      /* Response Buffer page address */
{
    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
    page &= 0x3f;  /* Limit page value */
    mb.w[RLOC_RESP] = (ushort) ( (mb.w[RLOC_RESP]&0xc0ff) | (page<<8) );
    asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
}

/* ex_set_sync_page(page) ----------------------------------------------
	This routine sets the response buffer page.
*/
ex_set_sync_page(page)
int    page;      /* Sync Buffer page address */
{
    asp_read( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
    page &= 0x3f;  /* Limit page value */
    mb.w[RLOC_CNTRL] = (ushort) ( (mb.w[RLOC_CNTRL]&0xc0ff) | (page<<8) );
    asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
}

/* ex_set_selftest(status) ---------------------------------------------
	This routine sets or resets Exec self test mode
	according to status.
*/
ex_set_selftest(status)
int    status;
{
/*
printf("EX_SET_SELFTEST:  STATUS=%d\n", status );
*/
    asp_read( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
    if (status) {
	mb.w[RLOC_CNTRL] &= 0xbfff;  /* Assert selftest */
	mb.w[RLOC_RESP] &= 0xbfff;
    } else {
	mb.w[RLOC_CNTRL] |= 0x4000;  /* Deassert selftest */
	mb.w[RLOC_RESP] |= 0x4000;
    }
    asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
    asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
}

/* ex_set_reset(reset) -------------------------------------------------
	This routine sets the reset mode bit on both the STIM and
	CTRL boards.  This mode bit enables (or disables) RAWRESET'
	(bit 3 on STIM) affecting counters on STIM and CTRL boards.
	For example, if the EXEC is running, trigerring it with
	reset mode bit "off" will merely capture another set of data
	without resynchronizing counters on STIM and CTRL boards.
	It is important for the CTC (or MLC) boards because by non-
	synchronizing we avoid glitching syncs, or having a line with
	the period that differs from surrounding lines.

	reset = 1 - allows counters on STIM and CTRL to be synchronized
		    when RAWRESET' is asserted.  Does not do actual
		    synchronization which is done on the "trigger"
		    command.
        reset = 0 - disallows counters on STIM and CTRL to be synchronized
		    when RAWRESET' is asserted.  It first performs the
		    synchronization (because by the end the trigger command
		    will no longer be able to do it), and then disallows
		    counters, etc.
*/
ex_set_reset(reset)
int reset;
{
        int ireg;

/*
printf("EX_SET_RESET:  RESET=%d\n", reset );
*/
/* Allow synchronization */
    asp_read( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
    mb.w[RLOC_CNTRL] &= 0x7fff;
    asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
    asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    mb.w[RLOC_STIM] &= 0xbfff;
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    if( reset ) return;

/* Synchronize */

    asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    ireg = mb.w[RLOC_STIM] & 0xfff0;
    mb.w[RLOC_STIM] = (ushort)(ireg|0x0004);	/* stim=rawreset & run mode */
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    mb.w[RLOC_STIM] = (ushort)(ireg|0x000C);	/* stim = run mode */
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );

/* Disallow synchronization */ 
    mb.w[RLOC_CNTRL] |= 0x8000;
    mb.w[RLOC_STIM] |= 0x4000;
    asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
}

/* ex_set_zero(zerostim,zeroresp) --------------------------------------
	This routine sets the Zero Stim Out and Zero Resp In
	registers according to zerostim and zeroresp.
*/
ex_set_zero(zerostim,zeroresp)
int    zerostim, zeroresp;
{
    asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    if (zerostim != 0)
	mb.w[RLOC_STIM] &= 0x7fff;   /* Assert Zero Stim Out */
    else
	mb.w[RLOC_STIM] |= 0x8000;   /* De-assert Zero Stim Out */
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );

    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
    if (zeroresp != 0)
	mb.w[RLOC_RESP] &= 0x7fff;   /* Assert Zero Resp In */
    else
	mb.w[RLOC_RESP] |= 0x8000;   /* De-assert Zero Resp In */
    asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
}

/* ex_set_wordsout(words) ----------------------------------------------
	This routine sets the Stim Buffer number of complex words out.
*/
ex_set_wordsout(words)
int	words;
{
/*
printf("EX_SET_WORDSOUT: words=%x(%d)\n", words, words );
*/
    if (words <= 0) words = 8;
    words = (words - 1) & 0xfffff8;
    mb.w[RLOC_STIM + 4] = (ushort) (words & 0xffff);
    mb.w[RLOC_STIM + 5] = (ushort) ((words >> 16) & 0x7f);
    asp_write( (RLOC_STIM+4)<<1, &mb.w[RLOC_STIM+4], 4 );
}

/* ex_set_codeper(codeper) ---------------------------------------------
	This routine sets the Stim Buffer number of syncs to cycle on.
*/
ex_set_codeper(codeper)
int	codeper;
{
    if (codeper < 1) codeper = 1;
    mb.w[RLOC_STIM + 1] = (ushort) ((codeper - 1) & 0x3fff);
/*
printf("EX_SET_CODEPER STIM[1]=%x\n", 0xffff&mb.w[RLOC_STIM+1] );
*/
    asp_write( (RLOC_STIM+1)<<1, &mb.w[RLOC_STIM+1], 2 );
}

/* ex_set_clock(speed) -------------------------------------------------
	This routine sets the system clock speed.
*/
ex_set_clock(speed)
	int speed;
{
    asp_read( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
    mb.w[RLOC_CNTRL] = (mb.w[RLOC_CNTRL] & 0xff9f) | ((speed & 0x3) << 5);
    asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
}

/* ex_op(lpow,wordsout) ------------------------------------------------
	This routines sets the Exec operating parameters that
	are most frequently changed during tests of ASP boards.
*/
ex_op(lpow,wordsout)
short int    lpow;   /* Line length */
int    wordsout;     /* Number of complex words to send from Stim Buf */
{
/*
printf("EX_OP: lpow=%x(%d), wordsout=%x(%d)\n", lpow, lpow, wordsout, wordsout);
*/
    /* Limit wordsout to multiple of 8 */
    if (wordsout <= 0) wordsout = 8;
    wordsout = (wordsout - 1) & 0xfffff8;
    asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 12 );
    mb.w[RLOC_STIM] = (ushort) ( (mb.w[RLOC_STIM] & 0xff0f) | (lpow<<4) );
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    mb.w[RLOC_STIM + 4] = (ushort) (wordsout & 0xffff);
    mb.w[RLOC_STIM + 5] = (ushort) ((wordsout >> 16) & 0x7f);
    asp_write( (RLOC_STIM+4)<<1, &mb.w[RLOC_STIM+4], 4 );
}

/* ex_load_stim(stim_data,stim_length,stim_addr) -----------------------
	This routine loads the stim_data array into the Stimulus
	Buffer starting at stim_addr.  Buffer paging is taken
	care of in the code.  stim_length data word pairs are
	written.
	Modified for R1B to use VME I/O routines instead of memory
	mapped I/O
*/
ex_load_stim(stim_data,stim_length,stim_addr)
DATA stim_data[];	/* Real and Imag Stimulus data */
int stim_length;	/* Number of data words to write, can be greater
				than array size, 23 bit value */
int stim_addr;		/* Stimulus Buffer starting word (4 bytes) in
				which to write data */
{
    int    page, mbaddr;  /* Stim_addr is decoded into two values */
    int    i, j;
    register unsigned char *s;

/*
printf("EX_LOAD_STIM length=%x(%d) addr=%x(%d) stim-data:%x\n",
	stim_length, stim_length, stim_addr, stim_addr, stim_data );
*/
/* Decode stim_addr */
    page = (stim_addr >> 17) & 0x003f;

/* Generate ASP bus address and for 2 LSB's to zero */
    mbaddr = ( (stim_addr & 0x1ffff)<<2 ) | MLOC_EXEC;

    ex_set_stim_mode(RMODE_MEM_WRITE);
    i = stim_length<<2;		/* get byte count from words */
    s = (unsigned char *) stim_data;
    while( i>0 ){
	if( i > PAGE_SIZE + MLOC_EXEC - mbaddr ){	/* if beyond page */
	    j = PAGE_SIZE + MLOC_EXEC - mbaddr;	/* reduce count to page end */
	}else{
	    j = i;				/* use full count */
	}
/*
printf("EX_LOAD_STIM writing %d bytes to %x on page %d from %x\n",
j, mbaddr, page, s );
*/
	ex_set_stim_page(page++);		/* Set page */
	asp_write( (PID_EXEC<<20)|mbaddr, s, j );
	i -= j;					/* decrement byte count */
	s += j;					/* increment source pointer */
	mbaddr = MLOC_EXEC;			/* set to page start */
    }
    ex_set_wordsout(stim_length);
}

/* ex_load_sync(synccode,codeper) -------------------------------------
	This routine loads an array of synccodes into the Stim
	Buffer sync memory and sets the number of synccodes to
	cycle on.
*/
ex_load_sync (synccode,codeper)
char   synccode[];
int    codeper;    /* Number of codes to cycle on */
{
    int    i, mbaddr;

/*
printf("EX_LOAD_SYNC:  codeper=%x(%d)\n", codeper, codeper );
for( i=0; i<codeper; i+=8 ){
    printf("SYNC[%3d]:", i );
    for( mbaddr=i; mbaddr<i+8; mbaddr++ ) printf("  %02x", 0xff&synccode[mbaddr] );
    printf("\n");
}
abob();
*/
    ex_set_stim_mode(RMODE_SYNC_WRITE);	/* Set mode for sync access */
    mbaddr = MLOC_EXEC >>1;
    for (i = 0; i < codeper; i++)	/* Load synccodes into MB */
	mb.w[mbaddr++] = synccode[i];
    asp_write( MEM_EXEC, &mb.w[MLOC_EXEC>>1], codeper<<1 );
    ex_set_codeper (codeper);
}
