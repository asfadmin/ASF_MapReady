/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  setup_ctc.c - This module sets up the control registers on the
    Corner Turn Control Board.  It contains three functions:

    setup_ctc_cntrl - This function sets register 0 which
		      contains the board control bits.  It also
		      clears the overflow bit in register 6.

    setup_ctc_par - This function sets registers 1 through 5 and
		    7 which are the operating parameters.

    ctc_set_reset - This function sets the board in or out of the
		    reset state.

    get_ctc_stat  - This function reads the state machine status
		    and the overflow bit in register 6.

    The calling sequences and command line arguments for these
    functions are explained below.  All three functions return
    the sync delay through the board based on the current
    control register contents.

int setup_ctc_cntrl(zerodat,bypass,test,wr_disable,state_cntrl,olpow,
                allreq,sendframe)
    
    The setup_ctc_cntrl command line arguments are:
    zerodat     - Each nibble represents one zero flag.  The flag is
		    true if its nibble is non-zero.
		  0xf0 = Zero data in
                  0x0f = Zero data out
    enable      - Each nibble specifies to enable (1) or bypass (0)
		    the given functional block:
		  0x100 = Corner Turn
		  0x010 = Unscrambler
	       	  0x001 = Forward Mux
    test        - 1 = Send board output to test bus
    wr_disable  - 1 = Disable write operation
    state_cntrl - Finite state machine control bits:
		  0 = Continous operation
		  1 = Hold at wr0, rd1, rd2
		  2 = Hold at wr1, rd2, rd0
		  3 = Hold at wr2, rd0, rd1
    olpow       - Output line length in power of two notation
    allreq	- 1 = Send data out immediately
		  0 = Send data only when RM request bit is active
    sendframe	  1 = Send frame syncs normally
		  0 = Send frame syncs abnormally

    Note: overflow is automatically set to 0 when the function is 
	  called.  Also, reset and enable are set to 0 and 1,
	  respectively.

int setup_ctc_par(vlines_frame,buf1rdlen,buf2rdlen,use_points,rotate)

    vlines_frame - Number of valid lines output per frame
    buf1rdlen    - First buffer read length (even value)
    buf2rdlen    - Second buffer read length (even value)
    use_points   - Useful points per line sync (even value)
    rotate       - Unscrambler line rotation

int ctc_set_reset(reset)

    reset - 1 = set the board in reset
	    0 = set the board in operational mode

int get_ctc_stat(state_status,overflow)
    The arguments are passed as addresses.
    state_status - Read only register; finite state machine status;
		   Valid states are from state 0 to state 9
    overflow     - Indicates buffer overflow

----------------------------------------------------------------- */

#include <aspdecl.h>

extern int alarm;		/* 1 = timer alarm went off */

static short int   cregs[8];

/*  int get_ctc_delay()-------------------------------------------------
    This routine gets the sync delay through the Courner Turn
    Controller based on  the current control register contents.
    The delay due to corner turn is not included.
*/
int get_ctc_delay()
{
    int    delay;
    int    olpow, upass, ctcpass;

    /* Get cregs from board */
    asp_read( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
    cregs[0] = mb.w[RLOC_CTC];

    /* Decode relevant values from cregs[] */
    upass = (cregs[0] >> 3) & 0x1;
    ctcpass = (cregs[0] >> 2) & 0x1;
    olpow = (cregs[0] >> 9) & 0xf;

    delay= 4 + ((1 << olpow) * !upass);
    return(delay);
}

/* setup_ctc_cntrl(zerodat,enable,test,wr_disable,state_cntrl,olpow,allreq,sendframe)-----
  	This routine sets register 0 which contains the board
	control bits.  It also clears the overflow bit in
	registers 6 and returns the sync delay through the
	board for the present control register contents.
*/
int setup_ctc_cntrl(zerodat,enable,test,wr_disable,state_cntrl,olpow,
		    allreq,sendframe)
int   zerodat,enable,test,wr_disable,state_cntrl, olpow,allreq,sendframe;
{
    int   trigger, rloc, reset, enb;
    int   zerodi, zerodo, ctcpass, upass, fmpass;

    /* Expand condensed control */
    ctcpass = ((enable & 0xf00) != 0);  /* Corner turn enable */
    upass =   ((enable & 0x0f0) != 0);  /* Unscrambler enable */
    fmpass =  ((enable & 0x00f) != 0);  /* Forward Mux enable */
    zerodi = ((zerodat & 0xf0) != 0); /* Zero data in */
    zerodo = ((zerodat & 0x0f) != 0); /* Zero data out */
    olpow &= 0xf;
    state_cntrl &= 0x3;
    wr_disable &= 0x1;
    test &= 0x1;
    sendframe &= 0x1;

    /* force board to reset state */
    reset = 1;
    enb = 0;

    /* initialize registers */
    asp_read( RLOC_CTC<<1, &mb.w[RLOC_CTC], 16 );
    cregs[0] = 0;
    cregs[6] = mb.w[RLOC_CTC + 6];
    cregs[7] = mb.w[RLOC_CTC + 7];

    /* Set up the register bits */
    cregs[0] |= (allreq << 15) | (!reset << 14) | (!enb << 13);
    cregs[0] |= (olpow << 9) | (state_cntrl << 7);
    cregs[0] |= (wr_disable << 6) | (!test << 5);
    cregs[0] |= (fmpass << 4) | (upass << 3) | (ctcpass << 2);
    cregs[0] |= (!zerodo << 1) | (!zerodi);
    
    cregs[6] &= 0xffef;  /* Clear overflow bit */

    cregs[7] = (cregs[7] & 0xdfff) | (sendframe << 13);  

    /* Move the control registers to the board */
    mb.w[RLOC_CTC] = cregs[0];
    asp_write( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
    mb.w[RLOC_CTC + 6] = cregs[6];
    mb.w[RLOC_CTC + 7] = cregs[7];
    asp_write( (RLOC_CTC+6)<<1, &mb.w[RLOC_CTC+6], 4 );

    /* Return trigger count based on current control registers */
    return (get_ctc_delay());
}

/* setup_ctc_par(vlines_frame,buf1rdlen,buf2rdlen,use_points,rotate)-----
    	This routine sets the operating parameters in registers 1
    	through 4 and 7.  It also returns the sync delay.
*/
int setup_ctc_par(vlines_frame,buf1rdlen,buf2rdlen,use_points,rotate)
int   vlines_frame, buf1rdlen, buf2rdlen, use_points, rotate;
{
    /* Set up (and limit) control registers with values */
    if (vlines_frame > 8192) 
	mb.w[RLOC_CTC + 1] = 8192;
    else 
	mb.w[RLOC_CTC + 1] = vlines_frame;
    asp_write( (RLOC_CTC+1)<<1, &mb.w[RLOC_CTC+1], 2 );

    if (buf1rdlen > 4095) 
	mb.w[RLOC_CTC + 2] = 4095;
    else 
	mb.w[RLOC_CTC + 2] = (buf1rdlen & 0x1ffe) - 1;
    asp_write( (RLOC_CTC+2)<<1, &mb.w[RLOC_CTC+2], 2 );

    if (buf2rdlen > 4096) 
	mb.w[RLOC_CTC + 3] = 4096;
    else 
	mb.w[RLOC_CTC + 3] = buf2rdlen & 0x1ffe;
    asp_write( (RLOC_CTC+3)<<1, &mb.w[RLOC_CTC+3], 2 );

    if (use_points > 8192) 
	mb.w[RLOC_CTC + 4] = 8192;
    else 
	mb.w[RLOC_CTC + 4] = (use_points & 0x3ffe);
    asp_write( (RLOC_CTC+4)<<1, &mb.w[RLOC_CTC+4], 2 );

    asp_read( (RLOC_CTC+7)<<1, &mb.w[RLOC_CTC+7], 2 );
    cregs[7] = mb.w[RLOC_CTC + 7];
    mb.w[RLOC_CTC + 7] = (cregs[7] & 0x2000) | rotate;
    asp_write( (RLOC_CTC+7)<<1, &mb.w[RLOC_CTC+7], 2 );

    /* return trigger count based on current control registers */
    return (get_ctc_delay());
}

/* get_ctc_stat(state_status,overflow)-----------------------
    	This routine reads the state machine status and the
    	overflow bit in register 6.  It also returns the sync
    	delay based on the current control register contents.
    	The arguments are passed as addresses.
*/
int get_ctc_stat(state_status,overflow)
int    *state_status, *overflow;
{
    int    trigger, rloc;

    rloc = RLOC_CTC;

    /* Get contents of register from board */
    asp_read( (rloc+6)<<1, &mb.w[rloc+6], 2 );
    cregs[6] = mb.w[rloc + 6];
    *state_status = cregs[6] & 0xf;
    *overflow = (cregs[6] >> 4) & 0x1;

    trigger = get_ctc_delay();  /* Get sync delay */

    return(trigger);
}

/* ctc_set_reset(reset) ------------------------------------------------
	This routine sets the board in reset (reset = 1) or 
	operational (reset = 0) state.
*/
int ctc_set_reset (reset)
int reset;
{
	int save;	/* use of "save" fixes multibus bug */
	int timeout;

	asp_read( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
	if (reset) {
	    cregs[0] = (mb.w[RLOC_CTC] & ~0x4000) | 0x2000;
	    mb.w[RLOC_CTC] = cregs[0];
	} else {
	    cregs[0] = (mb.w[RLOC_CTC] | 0x4000) & ~0x2000;
	    mb.w[RLOC_CTC] &= ~0x4000;	/* reset  = 0 */
	    asp_write( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
	    asp_read( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
	    mb.w[RLOC_CTC] |=  0x2000;	/* enable = 1 */
	    asp_write( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
	    asp_read( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
/*	    set_alarm(3);
	    while (alarm == 0 && ((save = mb.w[RLOC_CTC] & 0x2000) != 0)){ */

	    timeout = 0;
	    while (timeout<3000 && ((save = mb.w[RLOC_CTC] & 0x2000) != 0)){
		timeout++;
		usleep( 1000 );
		asp_read( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
	    }
/*	    if( alarm ){	*/
	    if (timeout>=3000) {
		printf("CTC will not clear enable bit\n");
	    }
	    mb.w[RLOC_CTC] |= 0x4000;	/* reset  = 1 */
	}
	asp_write( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
}

/* ctc_set_fsmc (fsmc) ------------------------------------------------
	This routine sets the finite state machine control bits.
*/
ctc_set_fsmc (fsmc)
int fsmc;
{
	asp_read( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
	cregs[0] = mb.w[RLOC_CTC];
	cregs[0] &= ~(0x3 << 7);
	cregs[0] |= (fsmc & 0x3) << 7;
	mb.w[RLOC_CTC] = cregs[0];
	asp_write( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
}
