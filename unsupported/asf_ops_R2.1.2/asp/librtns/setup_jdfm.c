/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  setup_jdfm.c - This module sets up the control registers on
    the JERS Deformatter board.  It contains the following functions:

    setup_jdfm       - Set the JERS-1 Deformatter control registers.
    reset_jdfm       - Reset bit error counter and/or arm dropout 
			 detector.  
    setup_jdfm_edi   - Set JDFM External Data Interface control bits
    setup_jdfm_test  - Enable asynchronous testing of the JDFM
    write_edi_data   - Write data to the EDI
    setup_jdfm_mem   - Load JDFM function memories
    get_jdfm_bitslip - Read the JDFM bitslip register value


    The calling sequences and command line arguments for the main
    functions are explained below.

setup_jdfm(enable,outsel,bypfifo,proc_pre,qinv,bshift,maxerr,maxfsdo,offset)
    enable    - Each nibble enables (1) or disables (0) one functional
		block of the board:
		0x1000 = Enable the Format Parser
		0x0100 = Enable the Sync Detector
		0x0010 = Enable Byte Alignment
		0x0001 = Enable the Deinterleaver
    outsel    - Output select (BYPMAP in the hardware):
		0 = Send gain value to output
		1 = Send weight function to output
		2 = Send data in to output
		3 = Map normally
    bypfifo   - 1 = Bypass the deinterleaver FIFO
		0 = Don't bypass the FIFO
    proc_pre  - 1 = Process data
		0 = Preprocessing mode
    qinv      - 1 = Invert the Q channel
		0 = Don't invert
    bshift    - Amount for format parser to shift if
		bypassed (3 bit value)
    maxerr    - Maximum allowable bit errors in a sync code
		(6 bit value)
    maxfsdo   - Maximum number of consecutive frames with
		missing sync codes (8 bit value)
    offset    - Length of first input format in bits

reset_jdfm(becreset,doarm)
    becreset  - 1 = reset bit error counter
                0 = remove reset for bit error counter
    doarm     - 1 = arm dropout detector
                0 = disarm dropout detector

setup_jdfm_edi(edi_enable,jers_gen)
    edi_enable - 1 = Enable the External Data Interface (EDI)
    jers_gen   - 1 = Data written to EDI is in JERS interleaved format
		 0 = Data written to EDI is unpacked

setup_jdfm_test(enable)
    enable - 1 = substitute multibus input for RPI input
	   
write_edi_data(data)
    data   - 16-bit data to write through to the IIF board

setup_jdfm_mem(weight,map)
    weight  - STC weight function memory (8K words)
    map     - STC/PCM map memory (2K words)

get_jdfm_bitslip(bitslip)
    bitslip   - Returned value: total bits difference between expected
		scene length and actual length (signed number).

get_jdfm_status(reqdata,dropout,d_fifo_ovr,becnt)
    All returned values:
    reqdata    - 1 = JDFM is requesting data from the RPI
		 0 = not requesting
    dropout    - 1 = sync dropout has been detected
		 0 = no dropout detected
    d_fifo_ovr - 1 = Deinterleaver FIFO has overflowed
		 0 = no FIFO overflow
    becnt      - Number of bit errors counted (in sync words)

-----------------------------------------------------------------
*/

#include <stdio.h>
#include <aspdecl.h>

static int    syncl,synch,synco;
static int    bits;
static int    sep;
static int    maskl,maskh,masko;
static int    first = 0;

static short int   cregs[15];
static FILE   *fptr, *logptr;

/* setup_jdfm(enable,outsel,bypfifo,proc_pre,qinv,bshift,maxerr,maxfsdo,offset) --
	This routine sets up the registers on the JERS-1 deformatter,
	and disables the EDI.
*/
setup_jdfm(enable,outsel,bypfifo,proc_pre,qinv,bshift,maxerr,maxfsdo,offset)
int enable, outsel, bypfifo, proc_pre, qinv, bshift, maxerr, maxfsdo, offset;
{
	int bypassfp,bypasssd,bypassba,bypassdi;

    /* Limit arguments */
	outsel &= 0x3;
	bypfifo = 0;   /* Never bypass the FIFO */
	proc_pre &= 1;
	qinv &= 1;
	bshift &= 0x7;
	maxerr &= 0x3f;
	maxfsdo &= 0xff;

    /* grab bypass bits */
	bypassfp = !((enable & 0xf000) == 0);
	bypasssd = !((enable & 0x0f00) == 0);
	bypassba = !((enable & 0x00f0) == 0);
	bypassdi = !((enable & 0x000f) == 0);

    /* Clear applicable cregs[] */
	cregs[0] = 0x0320;	/* bec reset off, edienable off, plus 4 spares */
	cregs[1] = 0x8000;      /* disable asynctest            */
	cregs[2] = 0;

    /* Set up the register bits */
	cregs[0] |= (bypassfp << 15) | (bypasssd << 14);
	cregs[0] |= (bypassba << 13) | (bypassdi << 12);
	cregs[0] |= (outsel << 10) | (proc_pre << 4);
	cregs[0] |= (qinv << 3) | bshift;
	cregs[1] |= (maxerr << 8) | maxfsdo;
	cregs[2] |= offset;

    /* Move the control registers to the board */
	mb.w[RLOC_JDFM] = cregs[0];
	mb.w[RLOC_JDFM + 1] = cregs[1];
	mb.w[RLOC_JDFM + 2] = cregs[2];
	asp_write( RLOC_JDFM<<1, &mb.w[RLOC_JDFM], 6 );
}

/* reset_jdfm(becreset,doarm) -----------------------------------------
	This routine resets the JERS-1 deformatter bit error counter
	and/or arms the frame sync dropout detector.
*/
reset_jdfm(becreset,doarm)
int becreset, doarm;
{
	asp_read( RLOC_JDFM<<1, &mb.w[RLOC_JDFM], 2 );
	if (becreset) mb.w[RLOC_JDFM] &= 0xfeff;  /* Set becreset low */
        else mb.w[RLOC_JDFM] |= 0x0100;  /* Set becreset high */

	if (doarm) mb.w[RLOC_JDFM] |= 0x0080;  /* Set doarm high */
        else mb.w[RLOC_JDFM] &= 0xff7f;  /* Set doarm low */
	asp_write( RLOC_JDFM<<1, &mb.w[RLOC_JDFM], 2 );
}

/* setup_jdfm_edi(edi_enable,jers_gen) ---------------------------------
	This routine sets the control bits for the JDFM External
	Data Interface (EDI).
*/
setup_jdfm_edi(edi_enable,jers_gen)
int edi_enable,jers_gen;
{
    /* limit the inputs */
	edi_enable &= 1;
	jers_gen &= 1;

    /* get the current control register contents */
	asp_read( RLOC_JDFM<<1, &mb.w[RLOC_JDFM], 2 );
	cregs[0] = mb.w[RLOC_JDFM] & 0xff9f;

    /* set the new bit values */
	cregs[0] |= (jers_gen << 6) | (edi_enable << 5);

    /* update the register values */
	mb.w[RLOC_JDFM] = cregs[0];
	asp_write( RLOC_JDFM<<1, &mb.w[RLOC_JDFM], 2 );
}

setup_jdfm_test(enable)
int enable;
{
    /* limit the input */
	enable &= 1;

    /* get the current control register contents */
	asp_read( RLOC_JDFM<<1, &mb.w[RLOC_JDFM], 4 );
	cregs[0] = mb.w[RLOC_JDFM];
	cregs[1] = mb.w[RLOC_JDFM + 1] & 0x7fff;

    /* set the new bit value */

    if (enable) {  /* set edienable* inactive */
	cregs[0] |= (1 << 5) | (1 << 6); 
    }
    else {
	cregs[1] |= 0x8000;
    }
    mb.w[RLOC_JDFM] = cregs[0];
    mb.w[RLOC_JDFM + 1] = cregs[1];
	asp_write( RLOC_JDFM<<1, &mb.w[RLOC_JDFM], 4 );
}

/* write_edi_data(data) ------------------------------------------------
	This routine writes data to the External Data Interface.
*/
write_edi_data(data)
int data;
{
	mb.w[RLOC_JDFM + 3] = data;
	asp_write( (RLOC_JDFM+3)<<1, &mb.w[RLOC_JDFM+3], 2 );
}

/* setup_jdfm_mem(weight,map) ------------------------------------------
	This routine loads data to the JDFM function memories.
*/
setup_jdfm_mem(weight,map)
short int *weight,*map;
{
	int i;
	int pidsave;
	short int *mp;

	pidsave = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_JDFM;
    /* load weight memory */
	bcopy( weight, &mb.w[MLOC_JDFM_WT>>1], 16384 );
	asp_write( MEM_JDFM_WT, &mb.w[MLOC_JDFM_WT>>1], 16384 );
	bcopy( map, &mb.w[MLOC_JDFM_MAP>>1], 4096 );
	asp_write( MEM_JDFM_MAP, &mb.w[MLOC_JDFM_MAP>>1], 4096 );
	mb.w[RLOC_REP] = pidsave;
}

/* get_jdfm_bitslip(bitslip) -------------------------------------------
	This routine retrieves the bitslip value from the JDFM board.
*/
get_jdfm_bitslip(bitslip)
long int *bitslip;
{
	asp_read( (RLOC_JDFM+5)<<1, &mb.w[RLOC_JDFM+5], 4 );
	*bitslip = (mb.w[RLOC_JDFM + 5] << 16) | mb.w[RLOC_JDFM + 6];
}

/* get_jdfm_status(reqdata,dropout,d_fifo_ovr,becnt) -------------------
	This routine returns several status items from the JDFM board.
*/
get_jdfm_status(reqdata,dropout,d_fifo_ovr,becnt)
int *reqdata, *dropout, *d_fifo_ovr, *becnt;
{
    /* retrieve register values */
	asp_read( (RLOC_JDFM+3)<<1, &mb.w[RLOC_JDFM+3], 10 );
	cregs[3] = mb.w[RLOC_JDFM + 3];
	cregs[7] = mb.w[RLOC_JDFM + 7];

    /* interpret values */
	*reqdata = ((cregs[7] & 0x8000) == 0);
	*dropout = ((cregs[7] & 0x4000) == 0);
	*d_fifo_ovr = ((cregs[7] & 0x2000) == 0);
	*becnt = cregs[3];
}
