/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* setup_iif.c -- This module sets up the registers and
	gets status for the Input Interface Board for the
	Alaska SAR Processor.
	This module consists of xxxx major functions:

        setup_iif_cntrl  - Set up IIF Board register 1
	setup_iif_par    - Set up registers 1 through 10
	get_iif_stat     - Get read only registers
        reset_iif        - Reset IIF
        get_iif_del      - Get propagation delay through board
		  	   based on current register settings
	set_iif_hdr_mode - Set normal or short header save mode
	get_iif_hdr_mode - Get the current header mode setting

	These function and their calling arguments are explained
	below.  All arguments are positive logic.

setup_iif_cntrl(preproc,stest,test,bypmap,enable,hdrmode,partswath)
	This function sets the contents of register 0.
    preproc    - Bypass unprocessed engineering and measurement
                 data to output
    stest      - Repeat scene start sync code when asserted;
		 otherwise generate scene start only once, scene
		 done at end
    test       - Enable test bus interface
    bypmap     - 0 = not assigned
		 1 = Bypass 34-bit data/sync in to Map R/I
		     output
		 2 = Bypass I/Q to Map R/I output
		 3 = No bypass, normal operation
    enable     - Each nibble enables (1) or bypasses (0) the
		 corresponding functional block
		 0x1000 = Enable input buffer (FIFO)
		 0x0100 = Enable Staged FFT
		 0x0010 = Enable the Unscrambler
		 0x0001 = Enable the Forward Multiplexer
    hdrmode    - 1 = Save shortened version of header data
		 0 = Save normal amount of header data
    partswath  - 1 = Process partial swath
		 0 = Process full swath
                 

setup_iif_par(rangelen,framelen,scenelen,rgwlen,rgw1start,rgw2start,
	      segofs,segnum,lpow,invlen)
	This function sets the contents of registers 1 through 10
    rangelen   - Number of complex data points in satellite
		 sampling window (13 bit value)
    framelen   - Number of valid line in frame (13 bit value)
    scenelen   - Number of frames in a scene (8 bit value)
    rgwlen     - Number of lines at first range gate window
		 setting (15 bit value)
    rgw1start  - Offset of first data point in window for first
		 rgwlen lines (13 bit value)
    rgw2start  - Offset of first data point in window after
		 rgwlen lines (13 bit value)
    segofs     - Number of points offset for each segment of
                 the staged fft (13 bit value)
    segnum     - Number of segments in the staged fft
    lpow       - Line length (FFT length) in power of two
		 notation (4 bit value)
    invlen     - Minumum number of invalid lines in frame

        
get_iif_stat(sdone,overrun,validin,resetout)
	This function gets the status of the read only
	fields from register 0
    sdone      - Scene processing done
    overrun    - Input buffer overrun
    validin    - Input data received since reset
    resetout   - Reset complete


iif_set_reset(reset)
	This function sets or resets the reset cycle bit
	in register 0
    reset  - 1 - Reset the board
	     0 - Remove reset bit

get_iif_del()
	Data propagation delay is returned.  cregs must be
	moved to the board before the delay is valid.

setup_iif_memory (iqmap)
	Routine load I/Q map table into the memory of the IIF
	board.

set_iif_hdr_mode (mode) 
	Set normal (mode=0) or short (mode=1) header save mode.


get_iif_hdr_mode (mode)
	Return the current header save mode.
*/

/*----------------------------------------------------------------*/

#include <aspdecl.h>

static short int    cregs[11];

/* get_iif_del()---------------------------------------------
	This routine calculates and returns the data
	propragation delay through the board based on
	the current register settings.  cregs must be
	moved to the board before the delay is valid.
*/
int get_iif_del()
{
    int    delay;
    int    preproc, lpow;
    int    enableuns, enablefft, enablefifo, enablemap;

    /* Get appropriate registers from the board */
    asp_read( RLOC_IIF<<1, &mb.w[RLOC_IIF], 20 );
    cregs[0] = mb.w[RLOC_IIF];
    cregs[9] = mb.w[RLOC_IIF + 9] & 0xf;

    /* Decode relevant values from cregs */
    preproc = (cregs[0] >> 13) & 0x1;
    enableuns = (cregs[0] >> 5) & 0x1;
    enablemap = (cregs[0] >> 3) & 0x3;
    enablefft = (cregs[0] >> 2) & 0x1;
    enablefifo = (cregs[0] >> 1) & 0x1;
    lpow = cregs[9];

    /* Calculate delay */
    if (preproc) delay = 2;
    else {
	if (enablemap == 1)
            delay = 5 + (!enablefifo * 2) + (!enablefft * 6) + (enableuns * (1 << lpow));
        else
	    delay = 4;
    }

    return(delay);
}

/* setup_iif_cntrl(preproc,stest,test,bypmap,enable,hdrmode,partswath) -------
  	This routine sets up the control bits in register 0.
*/
int setup_iif_cntrl(preproc,stest,test,bypmap,enable,hdrmode,partswath)
int    preproc, stest, test;
int    bypmap, enable, hdrmode, partswath;
{
    int    trigger;
    int    enablefifo, enablefft, enableuns, enablefmux;

    /* Expand condensed control bits */
    enablefifo = ((enable & 0xf000) != 0);
    enablefft  = ((enable & 0x0f00) != 0);
    enableuns  = ((enable & 0x00f0) != 0);
    enablefmux = ((enable & 0x000f) != 0);

    /* Limit arguments */
    preproc &= 0x1;
    stest &= 0x1;
    test &= 0x1;
    bypmap &= 0x3;
    hdrmode &= 0x1;
    partswath &= 0x1;

    cregs[0] = 0x4000;   /* Default register */

    /* Set up the register bits */
    cregs[0] |= (!hdrmode << 15);
    cregs[0] |= (!preproc << 13) | (!stest << 11) | (!test << 7);
    cregs[0] |= (enablefmux << 6) | (enableuns << 5) | (bypmap << 3);
    cregs[0] |= (enablefft << 2) | (enablefifo << 1) | (!partswath);

    /* Move the control register to the board */
    mb.w[RLOC_IIF] = cregs[0];
    asp_write( RLOC_IIF<<1, &mb.w[RLOC_IIF], 2 );

    /* Set trigger count */
    trigger = get_iif_del();
    return(trigger);
}

/* setup_iif_par(rangelen,framelen,scenelen,rgwlen,rgw1start,rgw2start,segofs,segnum,lpow,invlen)-----
  	This routine sets up the parameters in registers 1 through 10.
*/
int setup_iif_par(rangelen,framelen,scenelen,rgwlen,rgw1start,rgw2start,segofs,segnum,lpow,invlen)
int    rangelen, framelen, scenelen;
int    rgwlen, rgw1start, rgw2start;
int    segofs, segnum, lpow, invlen;
{
    int    i, trigger;

    /* Assign arguments to registers (limiting them) */
    cregs[1] = (rangelen > 8192) ? 0x1fff : (rangelen & 0x1fff) - 1;
    cregs[2] = (framelen > 4096) ? 0x0fff : (framelen & 0x0fff) - 1;
    cregs[3] = (scenelen >  256) ? 0x00ff : (scenelen & 0x00ff) - 1;
    cregs[4] = rgwlen & 0x7fff;
    cregs[5] = rgw1start & 0x1fff;
    cregs[6] = rgw2start & 0x1fff;
    cregs[7] = segofs & 0x1fff;
    cregs[8] = (segnum > 8) ? 0x7 : (segnum & 0x7) - 1;
    cregs[9] = lpow & 0xf;
    cregs[10] = invlen & 0xffff;

    /* Move registers to board */
    for (i = 1; i < 11; i++) mb.w[RLOC_IIF + i] = cregs[i];
    asp_write( (RLOC_IIF+1)<<1, &mb.w[RLOC_IIF+1], 20 );

    /* Set trigger count */
    trigger = get_iif_del();
    return(trigger);
}

/* get_iif_stat(sdone,overrun,validin,resetout)----------------
  	This routine gets the contents of the read only bits
	in register 0.
*/
get_iif_stat(sdone,overrun,validin,resetout)
int    *sdone, *overrun, *validin, *resetout;
{
    /* Get register 0 from board */
    asp_read( RLOC_IIF<<1, &mb.w[RLOC_IIF], 2 );
    cregs[0] = mb.w[RLOC_IIF];

    /* Assign read only bits to arguments */
    *sdone = !((cregs[0] >> 12) & 0x1);
    *overrun = !((cregs[0] >> 10) & 0x1);
    *validin = !((cregs[0] >> 9) & 0x1);
    *resetout = !((cregs[0] >> 8) & 0x1);
}

/* iif_set_reset(reset)------------------------------------------
	This routine sets and resets the reset cycle bit
	it register 0.  It returns the read only value of the
	bit.
*/
int iif_set_reset(reset)
int reset;
{
    int   i;

    asp_read( RLOC_IIF<<1, &mb.w[RLOC_IIF], 2 );
    cregs[0] = mb.w[RLOC_IIF];

    if (reset) {
	cregs[0] &= 0xfeff;
	mb.w[RLOC_IIF] = cregs[0];  /* Move reg to board */
	asp_write( RLOC_IIF<<1, &mb.w[RLOC_IIF], 2 );
	i = 0;
	do {  /* Wait for reset complete or time out */
	    asp_read( RLOC_IIF<<1, &mb.w[RLOC_IIF], 2 );
	    cregs[0] = mb.w[RLOC_IIF];
	    reset = cregs[0] & 0x0100;
	    i++;
	} while ((reset) & (i < 100));
	if (i >= 100) printf("RESET TIMEOUT!\n");
    } else {
	cregs[0] |= 0x0100;
	mb.w[RLOC_IIF] = cregs[0];
	asp_write( RLOC_IIF<<1, &mb.w[RLOC_IIF], 2 );
    }
}

/* setup_iif_memory (iqmap) -----------------------------------------
    	This function loads the Multibus memories
	in the Input Interface Board.
*/
setup_iif_memory (iqmap)
short int iqmap[8192];   /* Conversion map from I/Q to R/I */
{
    int   k, mloc;

     /* Load board memory */
    mloc = MLOC_IIF_MAP >> 1;
    bcopy( iqmap, &mb.w[mloc], 16384 );
    asp_write( MEM_IIF_MAP, &mb.w[mloc], 16384 );
}

/* set_iif_hdr_mode(mode) ----------------------------------------------
	This routine sets the header save mode to normal (mode=0) or
	short (mode=1).
*/
set_iif_hdr_mode(mode)
int mode;
{
	asp_read( RLOC_IIF<<1, &mb.w[RLOC_IIF], 2 );
	if (mode) mb.w[RLOC_IIF] &= 0x7fff;
	else mb.w[RLOC_IIF] |= 0x8000;
	asp_write( RLOC_IIF<<1, &mb.w[RLOC_IIF], 2 );
}

/* get_iif_hdr_mode(mode) ----------------------------------------------
	This routine returns the current header save mode: 
		1 = short, 0 = normal
*/
get_iif_hdr_mode(mode)
int *mode;
{
	int m;

	asp_read( RLOC_IIF<<1, &mb.w[RLOC_IIF], 2 );
	m = mb.w[RLOC_IIF] & 0x8000;
	*mode = (m == 0);
}

/* set_iif_holdfft(mode) -----------------------------------------------
	This routine sets the HOLDFFT bit to normal mode (mode=0) or
	special reset mode (mode=1).
*/
set_iif_holdfft(mode)
int mode;
{
	asp_read( RLOC_IIF<<1, &mb.w[RLOC_IIF], 2 );
	if (mode) mb.w[RLOC_IIF] &= !0x4000;
	else mb.w[RLOC_IIF] |= 0x4000;
	asp_write( RLOC_IIF<<1, &mb.w[RLOC_IIF], 2 );
}
