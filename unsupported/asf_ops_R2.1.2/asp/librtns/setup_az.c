/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  setup_az.c - This module sets up the control registers and
    multibus memory on the Azint Board.  It contains two
    functions.

    The calling sequences are:

setup_az(enable,format,test,uns_int,b16_32,detshift,lpow,wsel,ptspace)
    
    The command line arguments are:
    enable    - Each nibble enables (1) or bypasses (0) the 
		  corresponding functional block:
                0x100 = Inverse Mux
                0x010 = Unscrambler/Interpolator
		0x001 = Detector
    format    - 1 = Divide output of Interpolator by 2
    test      - 1 = Send board output to test bus
    uns/int   - Each nibble activates (1) or bypasses (0) the
		  corresponding functional block:
		0x10 = Activate Unscrambler
                0x01 = Activate Interpolator
    b16_32    - 1 - 16 bit output
		0 - 32 bit output
    detshift  - Detector output bits select - contains 2 fields:
		0xff00 = Output size: 0 = 16-bit output, non-zero =
		  32-bit output.
		0x001f = output shift (5 bits); 0 means window lsb
		  is aligned with the detector output lsb
    lpow      - Line length in power of 2 notation
    wsel      - Select set of weighting coefficients (5 bits)
    ptspace   - Bit 13 = Interpolator point space of 1
		Bit (12..0) = Fractional point space - must
		be 0 if bit 13 is set
    This routine returns the data propagation through the
    board based on the register contents.

setup_az_mem(xbuf)
    xbuf - Multibus memory containing starting addresses
	   for data lines read out of the Interpolator
	   memory.

-----------------------------------------------------------------
*/

#include <aspdecl.h>

/* setup_az(enable,format,test,uns_int,b16_32,detshift,lpow, --------
	    wsel,ptspace)
	This routine sets up the Azint control registers and
	returns the data propagation delay.
*/
int setup_az(enable,format,test,uns_int,b16_32,detshift,lpow,wsel,ptspace)
int   enable, format, test, uns_int, b16_32, detshift;
int   lpow, wsel, ptspace;
{
    int   i, trigger;
    int   enablemux, enableint, enabledet, uns, azint, incr;
    static short int   cregs[3];

    /* Expand condensed control */
    enabledet = ((enable & 0x000f) != 0);
    enableint = ((enable & 0x00f0) != 0);
    enablemux = ((enable & 0x0f00) != 0);
    azint = ((uns_int & 0x000f) != 0);
    uns = ((uns_int & 0x00f0) != 0);
    detshift = detshift & 0x001f;
    test &= 0x1;
    format &= 0x1;
    lpow &= 0xf;
    wsel &= 0x1f;
    if (ptspace & 0xe000) ptspace = 0x2000;

    /* Default cregs[] */
    cregs[0] = 0x1000;
    cregs[1] = 0xf000;
    cregs[2] = 0;

    /* Set up the register bits */
    cregs[0] |= (enabledet << 15) | (enableint << 14);
    cregs[0] |= (enablemux << 13) | (format << 11);
    cregs[0] |= (!test << 10) | (!uns << 9) | (!azint << 8);
    cregs[0] |= (b16_32 << 7) | (detshift);
    cregs[1] |= (lpow << 8) | (wsel);
    cregs[2] |= ptspace;

    /* Set trigger count */
    trigger = 4 + (!enableint * (1 + (1 << lpow) + (azint * 8)))
	      + (!enabledet * 3);

    /* Move the control registers to the board */
    for (i = 0; i < 3; i++) mb.w[RLOC_AZ + i] = cregs[i];
    asp_write( RLOC_AZ<<1, &mb.w[RLOC_AZ], 6 );
    
    /*
    printf("sync codes = %x\n", gsync);
    printf("cregs = %x %x %x\n\n", cregs[0] & 0xffff,
	    cregs[1] & 0xffff, cregs[2] & 0xffff);
    */

    return(trigger);
}

/* setup_az_mem (xbuf) -------------------------------------------------
    	This function loads the Azint XBuffer with the read address 
	offsets provided in xbuf[].
*/
void setup_az_mem(xbuf)
short int xbuf[XBUFSIZE];  /* Holds read address offsets */
{
    int   i, pid, mloc_xb, n, frac;

    /* Save current control computer page ID */
    pid = mb.w[RLOC_REP];

    /* Set pid for xbuffer */
    mb.w[RLOC_REP] = PID_AZ;
    mloc_xb = MLOC_XBUF >> 1;
    for (i = 0; i < XBUFSIZE; i++) {
	n = (xbuf[i] >> 4) & 0x01ff;    /* integer part */
	frac = xbuf[i] & 0xf;           /* fractional part */
	mb.w[mloc_xb + i] = (frac << 9) | n;
    }
    asp_write( MEM_XBUF, &mb.w[mloc_xb], XBUFSIZE<<1 );

    /* Restore pid */
    mb.w[RLOC_REP] = pid;
}

/* set_az_detshift(shift) ----------------------------------------------
	This routine sets the AZINT board detector shift register to
	the given value.
*/
set_az_detshift(shift)
int shift;
{
	static short int   cregs[3];

	cregs[1] = mb.w[RLOC_AZ + 1];
	cregs[1] &= 0xffe0;
	cregs[1] |= (shift & 0x1f);
	mb.w[RLOC_AZ + 1] = cregs[1];
	asp_write( (RLOC_AZ+1)<<1, &mb.w[RLOC_AZ], 2 );
}
