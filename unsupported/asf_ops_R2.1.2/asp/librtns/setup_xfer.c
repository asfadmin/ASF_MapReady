/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  setup_xfer.c - This module sets up the control registers on
    the Azimuth Transfer Function Board.  It contains four
    functions:

    setup_xfer_cntrl - This function sets the board control
		       bits in register 0.
    setup_xfer_par   - This function sets up board parameters
		       in registers 1 and 2.
    get_xfer_del     - Returns the propagation delay based on
		       the current control register contents.
    setup_xfer_mem   - Loads Azimuth Transfer Function Board
		       Multibus memory.

    The calling sequences and command line arguments for these
    functions are explained below.

int setup_xfer_cntrl(bypass,rosign,four_lk,test,xfer_dat,conj,format,
		     weight_dat,lpow)
    
    The command line arguments are:
    bypass    - Each nibble enables (1) or bypasses (0) the
		  corresponding functional block:
		0x10000 = Spectral Adjust Block
                0x01000 = Weight Multiplier
                0x00100 = MPY Block
                0x00010 = Spectral Shift Block
                0x00001 = Forward Mux
    rosign    - 1 = Sign of beam wavelength (ro) is negative
                0 = Sign of beam wavelength (ro) is positive
    four_lk   - 1 = Four Look input data
		0 = One Look input data
    test      - 1 = Send board output to test bus
    xfer_dat  - 0 = Select data to be conjugated or bypassed
		1 = Select transfer function to be conjugated or
		bypassed
    conj      - 1 = Complex conjugate an input on MPY
    format    - 1 = Interpolator arithmetic format adjust
    weight_dat- 0 = Select data to be bypassed
		1 = Select weighting to be bypassed
    lpow      - Line length in power of 2 notation
    Propagation delay is returned.

int setup_xfer_par(looklen,lookpos)

    The command line arguments are:
    looklen - Number of good points in a look.  Not used
	      for one look case.  (11 bits)
    lookpos - First nonzero point in a look, zero in one
	      look case.  (11 bits)
    Propagation delay is returned.

int get_xfer_del()
    Propagation delay is returned.  cregs must be moved to the
    board before the delay is valid.

void setup_xfer_mem(az_weight,theta,beam,quad,weightstart,lookstart,four_lk,linelen)

    The command line arguments are:
    az_weight   - 8k memory; Azimuth Weight
    theta       - 2k memory; Theta offset
    beam        - 2k memory; Spacecraft beam wavelength
    quad        - 2k memory; Quadratic offset
    weightstart - 2k memory; Starting point for Azimuth Weight
    lookstart   - 2k memory; Starting point for a set of 4 looks
    four_lk     - 1 = Four Look input data
	   	  0 = One Look input data
    linelen     - length of the line for azimuth processing (not the look length!)

-----------------------------------------------------------------
*/

#include <aspdecl.h>

static short int   cregs[3];

/* get_xfer_del() ------------------------------------------------------
	This routine get the data propagation delay through the Azimuth
	Transfer Function Board based on the current register settings.
*/
int get_xfer_del()
{
    int    delay;
    int    bypassadj, bypassazw, bypassmpy, bypassshift;
    int    lpow, linelength;

    /* Get cregs from board */
    asp_read( RLOC_XFER<<1, &mb.w[RLOC_XFER], 2 );
    cregs[0] = mb.w[RLOC_XFER];

    /* Decode relevant values from cregs */
    bypassadj = (cregs[0] >> 15) & 0x1;
    bypassmpy = (cregs[0] >> 14) & 0x1;
    bypassazw = (cregs[0] >> 13) & 0x1;
    bypassshift = (cregs[0] >> 12) & 0x1;
    lpow = cregs[0] & 0xf;
    linelength = 0x1 << lpow;

    delay = 6 + (!bypassadj * linelength);
    delay += (!bypassazw * 2) + (!bypassmpy * 3);
    delay += (!bypassshift * linelength);
    return(delay);
}

/* setup_xfer_cntrl(bypass,rosign,four_lk,test,xfer_dat,conj, ----------
	    format,weight_dat,lpow)
	This routine sets the XFER Board control bits in register 0.
	The data propagation delay through the board is returned.
*/
int setup_xfer_cntrl(bypass,rosign,four_lk,test,xfer_dat,conj,format,
		weight_dat,lpow)
int   bypass, rosign, four_lk, test, xfer_dat;
int   conj, format, weight_dat, lpow;
{
    int   i, trigger;
    int   bypassadj, bypassmpy, bypassazw, bypassshift, bypassmux;

    /* Expand condensed control */
    bypassadj =   ((bypass & 0xf0000) != 0);
    bypassazw =   ((bypass & 0x0f000) != 0);
    bypassmpy =   ((bypass & 0x00f00) != 0);
    bypassshift = ((bypass & 0x000f0) != 0);
    bypassmux =   ((bypass & 0x0000f) != 0);

    /* Limit arguments */
    test &= 0x1;
    rosign &= 0x1;
    four_lk &= 0x1;
    format &= 0x1;
    xfer_dat &= 0x1;
    conj &= 0x1;
    weight_dat &= 0x1;
    lpow &= 0xf;

    /* Clear cregs[] */
    cregs[0] = 0;

    /* Set up the register bits */
    cregs[0] |= (bypassadj << 15) | (bypassmpy << 14);
    cregs[0] |= (bypassazw << 13) | (bypassshift << 12);
    cregs[0] |= (bypassmux << 11) | (rosign << 10) | (!four_lk << 9);
    cregs[0] |= (!test << 8) | (xfer_dat << 7) | (!conj << 6);
    cregs[0] |= (format << 5) | (weight_dat << 4) | lpow;

    /* Move the control registers to the board */
    mb.w[RLOC_XFER] = cregs[0];
    asp_write( RLOC_XFER<<1, &mb.w[RLOC_XFER], 2 );

    /* Set trigger count */
    trigger = get_xfer_del();
    return(trigger);
}

/* setup_xfer_par(looklen,lookpos) -------------------------------------
	This routine setup us board parameters in registers 1 and 2.  
	The data propagation delay through the board is returned.
*/
int setup_xfer_par(looklen,lookpos)
int    looklen,  lookpos;
{
    int    trigger;

    /* Clear applicable cregs */
    cregs[1] = 0;
    cregs[2] = 0;

    /* Set up cregs */
    cregs[1] = (looklen - 1) & 0x1ff;
    cregs[2] = lookpos & 0x7ff;
    
    /* Move cregs to board */
    mb.w[RLOC_XFER + 1] = cregs[1];
    mb.w[RLOC_XFER + 2] = cregs[2];
    asp_write( ( RLOC_XFER+1 )<<1, &mb.w[RLOC_XFER+1], 4 );

    /* Set trigger count */
    trigger = get_xfer_del();
    return(trigger);
}

/* setup_xfer_mem(az_weight,theta,beam,quad,weightstart, ---------------
	    lookstart,four_lk,linelen)
	This routine loads the Azimuth Transfer Function Board
	Multibus memory.
	Completely revised to use the VME I/O routines for R1B
*/
void setup_xfer_mem(az_weight,theta,beam,quad,weightstart,lookstart,four_lk,linelen)
short int az_weight[8192];      /* Azimuth Weight */
short int theta[2048];          /* Theta offset */
short int beam[2048];           /* Spacecraft beam wavelength */
short int quad[2048];           /* Quadratic offset */
short int weightstart[2048];    /* Starting pt for Azimuth Weight */
short int lookstart[2048];      /* Starting pt for a set of 4 looks */
int four_lk;			/* One look (0) or four look mode (1) */
int linelen;                    /* Azimuth processing line length */
{
    int   i, j;
    int   mloc_wt, mloc_theta, mloc_beam, mloc_quad;
    int   mloc_wtst, mloc_lkst;

    /* Set up memory locations */
    mloc_wt = MLOC_XFER>>1;
    mloc_theta = mloc_wt + 16384;
    mloc_beam = mloc_theta + 2048;
    mloc_quad = mloc_beam + 2048;
    mloc_wtst = mloc_quad + 2048;
    mloc_lkst = mloc_wtst + 2048;

    /* Load memories */
    for (j = mloc_wt; j < (mloc_wt + 8192); j += linelen)
	bcopy( az_weight, &mb.w[j], linelen*2 );
    asp_write( MEM_XFER, &mb.w[mloc_wt], 16384 );

    bcopy( beam, &mb.w[mloc_beam], 4096 );
    asp_write( (PID_XFER<<20)|(mloc_beam<<1), &mb.w[mloc_beam], 4096 );

    if (four_lk) {
	bcopy( lookstart, &mb.w[mloc_lkst], 4096 );
	asp_write( (PID_XFER<<20)|(mloc_lkst<<1), &mb.w[mloc_lkst], 4096 );
	asp_write( (PID_XFER<<20)|(mloc_wtst<<1), weightstart, 2 );
	asp_write( (PID_XFER<<20)|(mloc_theta<<1), theta, 2 );
	asp_write( (PID_XFER<<20)|(mloc_quad<<1), quad, 2 );
    } else {
	bcopy( weightstart, &mb.w[mloc_wtst], 4096 );
	bcopy( theta, &mb.w[mloc_theta], 4096 );
	bcopy( quad, &mb.w[mloc_quad], 4096 );
	asp_write( (PID_XFER<<20)|(mloc_wtst<<1), &mb.w[mloc_wtst], 4096 );
	asp_write( (PID_XFER<<20)|(mloc_theta<<1), &mb.w[mloc_theta], 4096 );
	asp_write( (PID_XFER<<20)|(mloc_quad<<1), &mb.w[mloc_quad], 4096 );
	asp_write( (PID_XFER<<20)|(mloc_lkst<<1), lookstart, 2 );
    }
}
