/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  setup_avg.c - This module sets up the control registers on
    the Averager Board.  It contains four functions:

    setup_avg       - This function sets the board control
		      bits in register 1.
    set_avg_wrmode  - Sets the 3 write mode control bits
    get_avg_stat    - This function gets the status of the
		      read only registers a bits.
    get_avg_del     - Returns the propagation delay based on
	              the current control register contents.
    get_avg_addr    - Returns the address at which the averager
		      is currently writing.
    get_avg_mem     - Reads Averager Board Multibus memory.
    avg_set_reset   - Sets or Resets Averager

    The calling sequences and command line arguments for these
    functions are explained below.

int setup_avg(enable,front_back,wr_mode,reset,reading,divpow,
		test,segsize,lpow)
    
    The command line arguments are:
    enable    - Each nibble enables (1) or bypasses (0) the
		  corresponding functional block:
		0x1000 = Primary Accumulator
                0x0100 = Memory Accumulator
                0x0010 = Scale function
                0x0001 = Square root function
    front_back- 1 = Bypass data from memory accumulator
		0 = Bypass data from scaler/square root
    wr_mode   - Each nibble sets (1) or resets (0) the corresponding
		  write mode control bit:
		0x100 = Disable writing
		0x010 = Write to only one segment (FREEZEWRITE)
		0x001 = Write to segment B when asserted,
			else write to segment A.
    reset     - 1 = Master reset, configure status registers
		in known state.
    reading   - 1 = Masscomp currently reading segment
    divpow    - Amount for scaler to divide by (3 bits).  Divisor
		is calculated as 2exp(6-divpow).
    test      - 1 = send board output to test bus
    segsize   - The number of averaged points in one segment
		(8 bits)
    lpow      - Line length in power of 2 notation.  Used for
		data propagation delay calculation, but is
		not needed for board registers.
    Propagation delay is returned.

set_avg_wrmode(wr_mode)
    
    The command line arguments are:
    wr_mode   - Each nibble sets (1) or resets (0) the corresponding
		  write mode control bit:
		0x100 = Disable writing
		0x010 = Write to only one segment (FREEZEWRITE)
		0x001 = Write to segment B when asserted,
			else write to segment A.

get_avg_stat(start,ready,overflow)

    start   - Start of scene can be read
    ready   - Segment read since written
    overflow- A segment in the scene was written which was
	      not read (reset on scene start)

get_avg_addr(adrdat)

    The command line arguments are passed as addresses:
    adrdat  - Address to which averager is currently writing
	      (17 bits)

int get_avg_del(lpow)
    Propagation delay is returned, based on the contents of the
    hardware control registers.  lpow must be provided
    since it is not a board control register.

get_avg_mem(seg_store,seg_len)
    Returns the contents of segment storage.  The amount of
    data copied is given by the hardware SEGSIZE register.
    seg_store   - array to contain the returned memory values, up to
		  128Kbytes of data.
    seg_len     - number of words to return

avg_set_reset(reset)
    Sets or Resets Averager.
    reset  - 1 = Reset
	     0 = Set

*/

#include <aspdecl.h>

#define MEMSIZE 0xffff

static short int   cregs[4];

/* get_avg_del(lpow)----------------------------------------------------
	This routine gets the data propagation delay through the 
	Averager Board based on the current register settings and lpow.
*/
int get_avg_del(lpow)
int    lpow;
{
    int    delay;
    int    enablema, front_back, linelength;

    /* Get cregs from board */

    asp_read( (RLOC_AVG+1)<<1, &mb.w[RLOC_AVG+1], 2 );
    cregs[1] = mb.w[RLOC_AVG + 1];

    /* Decode relevant values from cregs */
    enablema = (cregs[1] >> 13) & 0x1;
    front_back = (cregs[1] >> 11) & 0x1;
    linelength = 1 << lpow;

    delay = 4 + ((2 + (8 * linelength)) * enablema) + front_back;
    return(delay);
}

/* setup_avg(enable,front_back,wr_mode,reset,reading,divpow,test,segsize,lpow)-----
	This routine sets the Avg Board control registers.
	The data propagation delay through the board is returned.

	segsize is expressed in points!
*/
int setup_avg(enable,front_back,wr_mode,reset,reading,divpow,test,segsize,lpow)
int   enable, front_back, wr_mode, reset;
int   reading, divpow, test, segsize;
int   lpow;
{
    int   i, trigger;
    int   enablesqrt, enablesc, enablema, enablepa;
    int   diswrite, frzwrite, frzab;

    /* Expand condensed control */
    enablepa   = ((enable & 0xf000) != 0);
    enablema   = ((enable & 0x0f00) != 0);
    enablesc   = ((enable & 0x00f0) != 0);
    enablesqrt = ((enable & 0x000f) != 0);
    diswrite = ((wr_mode & 0xf00) != 0);
    frzwrite = ((wr_mode & 0x0f0) != 0);
    frzab    = ((wr_mode & 0x00f) != 0);

    /* Limit arguments */
    test &= 0x1;
    front_back &= 0x1;
    reset &= 0x1;
    reading &= 0x1;
    divpow &= 0x7;
    if (segsize > 131070) segsize = 131070;

    /* Default applicable cregs[] */
    cregs[1] = 0x00f0;
    cregs[2] = 0xee0e;
    cregs[3] = 0;

    /* Set up the register bits */
    cregs[1] |= ((enablesqrt<<15)&0x8000) | ((enablesc<<14)&0x4000);
    cregs[1] |= ((enablema<<13)&0x2000) | ((enablepa<<12)&0x1000);
    cregs[1] |= ((front_back<<11)&0x0800) | ((!diswrite<<10)&0x0400);
    cregs[1] |= ((!frzwrite<<9)&0x0200) | ((!frzab<<8)&0x0100);
    cregs[2] |= ((!reset<<12)&0x1000) | ((!reading<<8)&0x0100);
    cregs[2] |= ((divpow<<4)&0x0070) | (!test&0x0001);
    cregs[3] = segsize >> 1;

    /* Move the control registers to the board */
    for (i = 1; i < 4; i++){
	mb.w[RLOC_AVG + i] = cregs[i];
	asp_write( (RLOC_AVG+i)<<1, &mb.w[RLOC_AVG+i], 2 );
    }

    /* Set trigger count */
    trigger = get_avg_del(lpow);
    return(trigger);
}

/* set_avg_wrmode(wr_mode) ---------------------------------------------
	This routine sets the 3 write mode bits of the Averager Board.
*/
int set_avg_wrmode(wr_mode)
int  wr_mode;
{
    int   diswrite, frzwrite, frzab;
/*
printf("SETUP_AVG_WRMODE=%x\n", wr_mode );
*/
    diswrite = ((wr_mode & 0xf00) != 0);
    frzwrite = ((wr_mode & 0x0f0) != 0);
    frzab    = ((wr_mode & 0x00f) != 0);

    /* retrieve the current register contents */
    asp_read( (RLOC_AVG+1)<<1, &mb.w[RLOC_AVG+1], 2 );
    cregs[1] = mb.w[RLOC_AVG + 1] & 0xf8ff;
    cregs[1] &= 0xf8ff;

    /* set the new bit values */
    cregs[1] |= ((!diswrite<<10)&0x0400);
    cregs[1] |= ((!frzwrite<<9)&0x0200) | ((!frzab<<8)&0x0100);

    /* write the new register values back to the hardware */
    mb.w[RLOC_AVG + 1] = cregs[1];
    asp_write( (RLOC_AVG+1)<<1, &mb.w[RLOC_AVG+1], 2 );
}

/* get_avg_stat(start,ready,overflow)-----------------------------------
	This routine gets the status of the Averager Board read only 
	register bits.  The arguments must be passed as addresses.

	The variable will be set to 1 if the corresponding bit
	is set to zero.
*/
get_avg_stat(start,ready,overflow)
int   *start, *ready, *overflow;
{
    /* Get registers from board */
    asp_read( (RLOC_AVG+1)<<1, &mb.w[RLOC_AVG+1], 2 );
    cregs[1] = mb.w[RLOC_AVG + 1];

    /* Get values from cregs */
    *start = (cregs[1] & 0x0080) == 0;
    *ready = (cregs[1] & 0x0040) == 0;
    *overflow = (cregs[1] & 0x0020) == 0;
}

/* get_avg_addr(adrdat)-------------------------------------------------
	This routine returns the address at which the Averager Board
	is currently writing.
*/
get_avg_addr(adrdat)
int *adrdat;
{
    /* Get registers from board */
    asp_read( RLOC_AVG<<1, &mb.w[RLOC_AVG], 4 );
    cregs[0] = mb.w[RLOC_AVG];
    cregs[1] = mb.w[RLOC_AVG + 1];
    *adrdat = (cregs[0] & 0xffff) | ((cregs[1] << 16) & 0x10000);
}

/* get_avg_mem(seg_store,seg_len)---------------------------------------
	This routine gets the contents of the Averager Board
	Multibus memory.
*/
get_avg_mem(seg_store,seg_len)
short int   seg_store[];  	/* Segment Memory */
int         seg_len;		/* # of words in segment memory */
{
    int pid ;
/*
printf("GET_AVG_MEM seg_len=%d\n", seg_len );
*/
/* Save current control computer page ID */
    pid = mb.w[RLOC_REP];

/* Set pid for Avg Multibus memories */
    mb.w[RLOC_REP] = PID_AVG;

/* flag that this buffer is being read */
    asp_read( (RLOC_AVG+2)<<1, &mb.w[RLOC_AVG+2], 2 );
    mb.w[RLOC_AVG + 2] &= ~0x0100;
    asp_write( (RLOC_AVG+2)<<1, &mb.w[RLOC_AVG+2], 2 );

    /* Read Avg memory */
    asp_read( MEM_AVG, &mb.w[MLOC_AVG>>1], seg_len*2 );
    bcopy( &mb.w[MLOC_AVG>>1], seg_store, seg_len*2 );

    /* flag that buffer read is completed */
    asp_read( (RLOC_AVG+2)<<1, &mb.w[RLOC_AVG+2], 2 );
    mb.w[RLOC_AVG +2] |= 0x0100;
    asp_write( (RLOC_AVG+2)<<1, &mb.w[RLOC_AVG+2], 2 );
     
    /* Restore pid */
    mb.w[RLOC_REP] = pid;
}

/* avg_set_reset(reset) ------------------------------------------------
	This routine sets the board in reset (reset = 1) or 
	operational (reset = 0) state.
*/
int avg_set_reset (reset)
	int reset;
{
/*
printf("AVG_SET_RESET...\n");
*/
    asp_read( (RLOC_AVG+2)<<1, &mb.w[RLOC_AVG+2], 2 );
	if (reset) mb.w[RLOC_AVG + 2] &= ~0x1000;
	else mb.w[RLOC_AVG + 2] |=  0x1000;
    asp_write( (RLOC_AVG+2)<<1, &mb.w[RLOC_AVG+2], 2 );
}
