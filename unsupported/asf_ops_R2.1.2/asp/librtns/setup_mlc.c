/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  setup_mlc.c - This module sets up the control registers on the
    Multilook Control Board.  It contains the following functions:


    setup_mlc_cntrl - This function sets up bits in registers
		      0 and 6 that are concerned with the
		      MLC initial operating state (reset, 
		      enable, etc.)  The overflow bit in
		      register 5 is also reset.

    setup_mlc_mode  - This function sets operating mode bits
		      in the registers 0 and 6.

    setup_mlc_par - This function sets registers 1 through 4,
		    7 and part of 6.  These are the operating
		    parameters.

    get_mlc_stat  - This function reads the state machine status
		    and the overflow bit in register 5.
    get_mlc_hist  - This function retrieves the histogram data
		    from the MLC histogram memory.
    get_mlc_delay - This function returns the data propagation
		    delay according to the current control
		    register contents.
    mlc_set_fsmc  - This function sets finite state machine control
		    bits.

    The calling sequences and command line arguments for these
    functions are explained below.  All three functions return
    the sync delay through the board based on the current
    control register contents.

int setup_mlc_cntrl(enable,histrst,histrd,reset,state_cntrl,wr_disable)
    
    The setup_mlc_cntrl command line arguments are:
    enable      - At power-on or reset, this bit is set to
		  initialize the lsync output counters.  It
		  is automatically reset when the counters
		  are enabled by the first lsync in.
    histrst     - 1 = Reset histogram
		  0 = Enable histogram
    histrd      - 1 = Read histogram
		  0 = Enable histogram
    reset       - 1 = Reset
    state_cntrl - Finite state machine control bits:
		  0 = Continous operation
		  1 = Hold at wr0, rd1, rd2
		  2 = Hold at wr1, rd2, rd0
		  3 = Hold at wr2, rd0, rd1
    wr_disable  - 1 = Disable write to memory boards
		  0 = Write to memory boards


int setup_mlc_mode(enable,dcrsi_out,test,norm_aspect,four_lk,zerodat,
                   sfbs,rq)

    The setup_mlc_mode command line arguments are:
    enable      - Each nibble enables (1) or bypasses (0) the
		    corresponding functional block:
		  0x100 = Intraline Adder
		  0x010 = Multilook Controller
		  0x001 = Square Root
    dcrsi_out   - 0 = DCRSi upper 16 bit output
		  1 = DCRSi 32 bit output
    test        - 0xf0 = send board data out to test bus
		  0x0f = send DCRSi out to test bus mux if
			 data test not asserted
    norm_aspect - 1 = Normal aspect ratio for MLC
		  0 = Extended aspect ratio for MLC
    four_lk     - 1 = Four look mode
		  0 = One look mode
    zerodat     - Each non-zero nibble enables the corresponding 
		    zero function:
		  0xf0 = Zero data in
		  0x0f = Zero data out
    sfbs        - 1 = Select Frame Block Sync
		  0 = No Frame Block Sync
    rq          - 1 = Accept DCRSi requests for lines
		  0 = Always request lines from MLC


int setup_mlc_par(azrem,azpow,nov,azof,rglen,vfdel,vlpts)
    This routine sets all the numeric parameters for the MLC:
    azrem        - Azimuth remainder (length - offset )
    azpow        - Azimuth Linelength in power of 2
		   notation
    nov          - Non-overlap for Intraline Adder
    azof         - Azimuth offset
    rglen        - Range Linelength (even value)
    vfdel        - Valid Frame Delay
    vlpts        - Valid Points Per Line


setup_mlc_mem (rotate)
    This routine sets up the rotate memory
    rotate       - An array of 1024 16-bit values


int mlc_set_reset (reset)
    reset        - 1 = put the MLC in the reset state.
		 - 0 = put the MLC in the enabled state.
    NOTE: this routine also clears the histogram memory to zero.

int get_mlc_stat(state_status,overflow)
    The arguments for this routine are passed as addresses.
    state_status - Read only register, finite state machine status,
		   4 bits.
		   0000 - reset
		   0101 - pre-state 1
		   0001 - state 1
		   1001 - post-state 1
		   0110 - pre-state 2
		   0010 - state 2
		   1010 - post-state 2
		   0111 - pre-state 3
		   0011 - state 3
		   1011 - post-state 3
		   All other states are invalid.
    overflow     - Indicates Multilook Controller overrun
		   condition.

    get_mlc_hist (hist)

    hist         - A short integer array to hold the histogram values.
		   (256 entries)

    mlc_set_fsmc (fsmc)

    fsmc         - 00 - continuous operation
		   01 - hold at state 1
		   10 - hold at state 2
		   11 - hold at state 3
-----------------------------------------------------------------
*/

#include <aspdecl.h>

extern int alarm;     /* 1 = alarm went off */

static short int   cregs[8];

/* get_mlc_delay() -----------------------------------------------------
    This routine gets the sync delay through the Multilook
    Controller based on  the current control register contents.
    Delay due to Multilook Controller is not included.
*/
int get_mlc_delay()
{
    int    delay;
    int    azpow, rglen;
    int    ilapass, mlcpass;

    /* Get cregs from board */
    asp_read( RLOC_MLC<<1, &mb.w[RLOC_MLC], 10 );
    cregs[0] = mb.w[RLOC_MLC];
    cregs[2] = mb.w[RLOC_MLC + 2];
    cregs[4] = mb.w[RLOC_MLC + 4];

    /* Decode relevant values from cregs[] */
    ilapass = (cregs[0] >> 11) & 0x1;
    mlcpass = (cregs[0] >> 10) & 0x1;
    azpow = (cregs[2] >> 9) & 0xf;
    rglen = cregs[4] & 0x1fff;

    delay = 4 + ((1 << azpow) * !ilapass);
    return(delay);
}

/* setup_mlc_cntrl(enable,histrst,histrd,reset,state_cntrl,wr_disable)-----
   	This routine sets the bits in registers 0 and 6 that are
	concerned with the start up state of the board.  The
	overflow bit in register 5 is cleared.  It returns the
	sync delay.
*/
int setup_mlc_cntrl(enable,histrst,histrd,reset,state_cntrl,wr_disable)
int   enable, histrst, histrd;
int   reset, state_cntrl, wr_disable;
{
    int    trigger;   /* Data propagation delay through board */

    /* Limit arguments */
    enable &= 0x1;
    histrst &= 0x1;
    histrd &= 0x1;
    reset &= 0x1;
    state_cntrl &= 0x3;
    wr_disable &= 0x1;

    /* Clear applicable bits in cregs */
    asp_read( RLOC_MLC<<1, &mb.w[RLOC_MLC], 14 );
    cregs[0] = mb.w[RLOC_MLC    ] & 0x3fc6;
    cregs[6] = mb.w[RLOC_MLC + 6] & 0xffef;

    /* Set up register bits */
    cregs[0] |= wr_disable | (state_cntrl << 3) | (reset << 5);
    cregs[0] |= (!histrst << 14) | (enable << 15);
    cregs[6] |= histrd << 4;

    /* Move registers to the board */
    mb.w[RLOC_MLC    ] = cregs[0];
    asp_write( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
    mb.w[RLOC_MLC + 6] = cregs[6];

    /* Clear overflow bit */
    mb.w[RLOC_MLC + 5] &= 0xfffe;
    asp_write( (RLOC_MLC+5)<<1, &mb.w[RLOC_MLC+5], 4 );

    /* Determine trigger count based on current control registers */
    trigger = get_mlc_delay();

    return(trigger);
}

/* setup_mlc_mode(enable,dcrsi_out,test,norm_aspect,four_lk,zerodat,sfbs,rq)-----
  	This routine sets operating mode bits in registers
	0 and 6.  It returns the sync delay.
*/
int setup_mlc_mode(enable,dcrsi_out,test,norm_aspect,four_lk,zerodat,sfbs,rq)
int   enable, dcrsi_out, test, norm_aspect;
int   four_lk, zerodat, sfbs, rq;
{
    int   trigger;  /* Data propagation delay through board */
    int   zerodi, zerodo, sqrtpass, ilapass, mlcpass;
    int   dcrsi_test, data_test;

    /* Expand condensed arguments */
    ilapass =  ((enable & 0x0f00) != 0);   /* Intraline add enable */
    mlcpass =  ((enable & 0x00f0) != 0);   /* Multilook enable */
    sqrtpass = ((enable & 0x000f) != 0);   /* Square root enable */
    zerodi = ((zerodat & 0xf0) != 0);   /* Zero data in */
    zerodo = ((zerodat & 0x0f) != 0);   /* Zero data out */
    data_test =  ((test & 0xf0) != 0);  /* Data out to test out */
    dcrsi_test = ((test & 0x0f) != 0);  /* DCRSi to test if */
					  /*  not data_test */
    /* Limit arguments */
    dcrsi_out &= 0x1;     /* DCRSi output bits */
    norm_aspect &= 0x1;   /* Aspect ration for MLC */
    four_lk &= 0x1;       /* One or four look mode */
    sfbs &= 0x1;          /* Select frame block sync */
    rq &= 0x1;            /* Always request for test */

    /* Clear applicable cregs bits */
    asp_read( RLOC_MLC<<1, &mb.w[RLOC_MLC], 14 );
    cregs[0] = mb.w[RLOC_MLC    ] & 0xc039;
    cregs[6] = mb.w[RLOC_MLC + 6] & 0xffd7;

    /* Set up the register bits */
    cregs[0] |= (sqrtpass << 13) | (!data_test << 12);
    cregs[0] |= (ilapass << 11) | (mlcpass << 10);
    cregs[0] |= (dcrsi_out << 9) | (!dcrsi_test << 8);
    cregs[0] |= (norm_aspect << 7) | (four_lk << 6);
    cregs[0] |= (!zerodi << 2) | (!zerodo << 1);
    cregs[6] |= (rq << 5) | (sfbs << 3);

    /* Move the control registers to the board */
    mb.w[RLOC_MLC    ] = cregs[0];
    asp_write( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
    mb.w[RLOC_MLC + 6] = cregs[6];
    asp_write( (RLOC_MLC+6)<<1, &mb.w[RLOC_MLC+6], 2 );

    /* Clear overflow bit */
    mb.w[RLOC_MLC + 5] &= 0xfffe;
    asp_write( (RLOC_MLC+5)<<1, &mb.w[RLOC_MLC+5], 2 );

    /* Determine trigger count based on current control registers */
    trigger = get_mlc_delay();
    
    return(trigger);
}

/* setup_mlc_par(azrem,azpow,nov,azof,rglen,vfdel,vlpts) ---------------
    	This routine sets the operating parameters in registers 1
    	through 4, 7 and part of 6.  It also returns the sync delay.
*/
int setup_mlc_par(azrem,azpow,nov,azof,rglen,vfdel,vlpts)
int   azrem, azpow, nov, azof;
int   rglen, vfdel, vlpts;
{
    int   trigger, i;

    /* Clear applicable cregs bits */
    asp_read( (RLOC_MLC+6)<<1, &mb.w[RLOC_MLC+6], 2 );
    cregs[6] = mb.w[RLOC_MLC + 6] & 0xfff8;

    /* Set up cregs[] with values */
    if (azrem > 0x1000) azrem = 0x1000;
    if (azrem < 0) azrem = 0;
    cregs[1] = azrem;
    cregs[2] = ((azpow & 0xf) << 9) | (nov & 0x1ff);
    if (azof > 0x1000) azof = 0x1000;
    if (azof < 0) azof = 0;
    cregs[3] = azof;
    if (rglen > 0x2000) rglen = 0x2000;
    if (rglen < 1) rglen = 1;
    cregs[4] = (rglen & 0xfffe) - 1;
    cregs[6] |= (vfdel - 1) & 0x7;
    cregs[7] = (vlpts - 1) & 0x3ff;

    /* Move the control registers to the board */
    for (i = 1; i < 5; i++) mb.w[RLOC_MLC + i] = cregs[i];
    asp_write( (RLOC_MLC+1)<<1, &mb.w[RLOC_MLC+1], 8 );
    mb.w[RLOC_MLC + 6] = cregs[6];
    mb.w[RLOC_MLC + 7] = cregs[7];
    asp_write( (RLOC_MLC+6)<<1, &mb.w[RLOC_MLC+6], 4 );

    /* Determine trigger count based on current control registers */
    trigger = get_mlc_delay();

    return(trigger);
}

/* setup_mlc_mem (rotate) ----------------------------------------------
	This routine loads the rotate values into the MLC rotate memory.
*/
setup_mlc_mem (rotate)
short int *rotate;
{
	int i;
	int pid;

	bcopy( rotate, &mb.w[MLOC_MLC_ROT>>1], 2048 );
	asp_write( MEM_MLC_ROT, &mb.w[MLOC_MLC_ROT>>1], 2048 );
}

/* mlc_set_reset(reset) ------------------------------------------------
	This routine sets the board in reset (reset = 1) or 
	operational (reset = 0) state.  When changing to the
	operational state, the histogram memory is cleared to zero.
*/
int mlc_set_reset (reset)
int reset;
{
	int save;	/* use of "save" fixes multibus bug */
	int timeout;

	asp_read( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
	if (reset) {
	    cregs[0] = (mb.w[RLOC_MLC] | 0x0020) | 0x8000;
	    mb.w[RLOC_MLC] = cregs[0];
	} else {
	    mb.w[RLOC_MLC] |=  0x0020;	/* reset  = 1 */
	    asp_write( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
	    mb.w[RLOC_MLC] &= ~0x4000;	/* histrst = 0 */
	    asp_write( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
	    mb.w[RLOC_MLC] |=  0xC000;	/* enable = 1, histrst = 1 */
	    asp_write( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
	    timeout = 0;
/*	    set_alarm(3);
	    while (alarm==0 && ((save = mb.w[RLOC_MLC] & 0x8000) != 0)){ */
	    while ( timeout<3000 && ((save = mb.w[RLOC_MLC] & 0x8000) != 0)){
		usleep( 1000 );
		timeout++;
		asp_read( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
	    }
/*	    if (alarm) {	*/
	    if( timeout>=3000 ){
		printf("MLC will not clear enable bit\n");
	    }
	    mb.w[RLOC_MLC] &= ~0x0020;	/* reset  = 0 */
	}
	asp_write( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
}

/* get_mlc_stat(state_status,overflow) ---------------------------------
    	This routine reads the state machine status and the
    	overflow bit in register 6.  It also returns the sync
    	delay based on the current control register contents.
    	The arguments are passed as addresses.
*/
int get_mlc_stat(state_status,overflow)
int    *state_status, *overflow;
{
    int    trigger;

    /* Get contents of register from board */
	asp_read( (RLOC_MLC+5)<<1, &mb.w[RLOC_MLC+5], 2 );
    cregs[5] = mb.w[RLOC_MLC + 5];
    *state_status = (cregs[5] >> 1) & 0xf;
    *overflow = cregs[5] & 0x1;

    trigger = get_mlc_delay();  /* Get sync delay */

    return(trigger);
}

/* get_mlc_hist (hist) -------------------------------------------------
	This routine returns the contents of the histogram memory.
*/
get_mlc_hist (hist)
int *hist;
{
	unsigned short int *i, *iend;

	i = &mb.w[MLOC_MLC_HIST >> 1];
	asp_read( MEM_MLC_HIST, i, 1024 );
	iend = i + 512;
	while (i < iend) *hist++ = (*i++ << 16) + (*i++ & 0xffff);
}

/* mlc_set_fsmc (fsmc) ------------------------------------------------
	This routine sets the finite state machine control bits.
*/
mlc_set_fsmc (fsmc)
int fsmc;
{
	asp_read( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
        cregs[0] = mb.w[RLOC_MLC];
	cregs[0] &= ~(0x3 << 3);
	cregs[0] |= (fsmc & 0x3) << 3;
	mb.w[RLOC_MLC] = cregs[0];
	asp_write( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
}
