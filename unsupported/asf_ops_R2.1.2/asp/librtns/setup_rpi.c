/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  setup_rpi.c - This module sets up the control registers on
    the Recorder Processor Interface Board (RPI).  It contains
    the following functions:

    setup_rpi       - Set the RPI board control registers.
    setup_rpi_test  - Set the test, output select and bypass bits.
    setup_edfm      - Set the ERS-1 Deformatter control registers.
    reset_rpi       - Toggle reset RPI bit on, then off.
    reset_edfm      - Reset bit error counter and/or arm dropout 
			detector.
    set_rpi_mode    - Set RPI Interface and Control Unit mode for 
		        reading and writing to the DCRSi.
    set_rpi_offset  - Set the block offset register.
    set_rpi_pos     - Set the block offset, bit shift and polarity.
    get_rpi_fifostat- Get the read and write fifo status.
    get_rpi_stat    - Get the rest of the read only status bits.
    get_rpi_fifo    - Read data from the RPI read FIFO.
    write_rpi_fifo  - Write data to the RPI write FIFO.
    play_dis        - Enable play output


    The calling sequences and command line arguments for the main
    functions are explained below.

setup_rpi(format,mode,offset,shift,norm_inv,test,drivesel,rev)
    
    The command line arguments are:
    format    - Satellite Data Formats:
		0 = European ERS
		1 = Japanese ERS
		2 = Canadian RADARSAT
    mode      - 1 = Write to DCRSi from MC5600
		2 = Read from DCRSi to MC5600
		4 = Read from DCRSi to Processor
    offset    - Frame Sync Code Offset, 16 bit value
    shift     - Byte alignment shift, 3 bit value
    norm_inv  - 1 = Data normal
		0 = Data inverted
    test      - 1 = Clear all test and bypass bits
		0 = Leave all test and bypass bits unchanged
    drivesel  - 0 = use DCRSi on the new RPI board
		1 = use SONY on the new RPI board
    rev       - 0 = do not reverse each 8 bits from tape recorder
		1 = reverse each 8 bits from tape recorder

setup_edfm(sbm,fsm,og_obsel,maxfsdo)
    sbm       - Single byte match:  Required number of matching 
		bits per byte of sync code, 4 bit value
    fsm       - Frame sync byte match:  Required number of matching 
		bits in whole sync code, 5 bit value
    og_obsel  - 1 = On-Ground processing
		0 = On-Board processing
    maxfsdo   - Maximum number of consecutive frames with
		missing sync codes, 8 bit value

setup_rpi_test(test,enableba,enablebd,outsel,modsel)
    test      - Selects test (0) or normal (1)
		data as the input to the corresponding module.
                0x1000 = Board output to test bus
		0x0100 = Test B/A
		0x0010 = Test write
		0x0001 = Test read
    enableba  - 1 = Enable byte alignment
    		0 = Bypass byte alignment
    enablebd  - 1 = Enable data in to data out
    		0 = Bypass data in to data out
    outsel    - Selects the source of board output:
		0 = Normal data
		1 = PRN decoder output
		2 = Frame parser byte count bits
		3 = 4 misc. signals from frame parser
		4 = Frame sync code detector output
		5-7 = Unused
    modsel    - Selects module data (1) or test data (0)
		as the output of the corresponding module.
                0x1000000 = SELBCTST
		0x0100000 = SELLDVBYP
		0x0010000 = SELFRMNUMTST
		0x0001000 = SELFMTNUMTST
		0x0000100 = SELPRNBYP
		0x0000010 = SELBEGFRM
		0x0000001 = SELFPBYP

reset_rpi(reset)
    reset     - 1 = reset rpi board
                0 = remove reset

reset_edfm(rstbecntr,doarm)
    rstbecntr - 1 = reset bit error counter
                0 = remove reset for bit error counter
    doarm     - 1 = arm dropout detector
                0 = disarm dropout detector

set_rpi_mode(mode)
    mode      - 0 = Write to DCRSi from MC5600
	      - 1 = Read from DCRSi to MC5600
	      - 2 = Read from DCRSi to Processor

set_rpi_offset(offset)
    offset    - Frame sync code offset, 16 bit value

set_rpi_pos(offset,shift,norm_inv)
    offset    - Frame sync code offset
    shift     - Bit shift within byte
    norm_inv  - Normal (1) or inverted (0) data polarity

get_rpi_fifostat(dataready,rdstat,wrstat)

    The command line arguments are passed as addresses:
    dataready - DCRSi data ready for MC5600
    rdstat    - state of the read buffer:
		0 = empty
		1 = less than half full
		2 = at least half full
		3 = full
    wrstat    - state of the write buffer (same as for rdstat)

get_rpi_stat(doflg,becnt,frmnum,fmtnum)
    doflg     - Frame sync code dropout flag
    becnt     - Bit error count, 16 bit value
    frmnum    - Frame number of first sync code dropout,
		maximum of 28
    fmtnum    - Format number of first sync code dropout,
		16 bit value

int get_rpi_fifo(raw_dat,words)
    raw_dat   - Short integer array to contain data being read.
    words     - Number of 16-bit words to read.
    The number of data actually read is returned.

write_rpi_fifo(out_dat,words)
    out_dat   - Short integer array containing data to write.
    words     - Number of 16-bit words to write.

play_dis (disable)
    disable   - 1 = disable play output
                0 = enbale play output

-----------------------------------------------------------------
*/

#include <stdio.h>
#include <aspdecl.h>
#include <btio.h>
extern int asp_chan;

#define FIFOSIZE 256   /* Size of RPI fifo */
#define BLKSIZE 2178   /* Size of DCRS data block in words */
#define BUFSIZE 4096   /* Size of data buffer to load into DCRS */

static int    syncl,synch,synco;
static int    bits;
static int    sep;
static int    maskl,maskh,masko;
static int    first = 0;

static short int   cregs[15];
static FILE   *fptr, *logptr;
extern int    alarm;          /* Defined in wait_io.c module */

/* setup_rpi(format,mode,offset,shift,norm_inv,test,drivesel,rev) -----------
	This routine sets the Recorder Processor Interface
	Board control registers.
*/
setup_rpi(format,mode,offset,shift,norm_inv,test,drivesel,rev)
int format, mode, offset, shift, norm_inv, test,drivesel,rev;
{
	/* if requested, clear all test and bypass bits */
	if (test) setup_rpi_test(0xffff,1,1,0,0xfffffff);

	/* Limit arguments */
	shift &= 0x7;
	norm_inv &= 0x1;
	if (format < 0 || format > 2) format = 0;
	if (mode != 1 && mode != 2) mode = 4;

	/* Clear applicable cregs[] */
	asp_read( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
	cregs[0] = mb.w[RLOC_RPI] & 0xf841;

	/* Set up the register bits */
	cregs[0] |= (shift << 8) | (norm_inv << 7) | (format << 4);
	cregs[0] |= (mode << 1);
	cregs[3] = offset & 0xffff;
	cregs[4] = ((offset>>16) & 0xff) | ( (drivesel&1)<<8 )
		| ( (rev&1)<<9 );

	/* Move the control registers to the board */
	mb.w[RLOC_RPI] = cregs[0];
	asp_write( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
	mb.w[RLOC_RPI + 3] = cregs[3];
	mb.w[RLOC_RPI + 4] = cregs[4];
	asp_write( (RLOC_RPI+3)<<1, &mb.w[RLOC_RPI+3], 4 );
}

/* setup_rpi_test(test,enableba,enablebd,outsel,modsel)----------------
        This routine sets the test path, enable and output select.
*/
setup_rpi_test(test,enableba,enablebd,outsel,modsel)
int test, enableba, enablebd, outsel, modsel;
{
	int    tst_path, tstb_a, tst_wri, tst_rdi;
	int    bc_tst, ldv_byp, frmnum_tst, fmtnum_tst;
	int    prn_byp, beg_frm, fp_byp;

	/* Limit arguments */
	enableba &= 0x1;
	enablebd &= 0x1;
	outsel &= 0x7;

	/* Default applicable cregs[] */
	asp_read( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
	cregs[0] = mb.w[RLOC_RPI] & 0x7bf;
	cregs[10] = 0xf880;

	/* Decode arguments */
	tst_path = ((test & 0xf000) != 0);
	tstb_a   = ((test & 0x0f00) != 0);
	tst_wri  = ((test & 0x00f0) != 0);
	tst_rdi  = ((test & 0x000f) != 0);
	
	bc_tst     = ((modsel & 0xf000000) != 0);
	ldv_byp    = ((modsel & 0x0f00000) != 0);
	frmnum_tst = ((modsel & 0x00f0000) != 0);
	fmtnum_tst = ((modsel & 0x000f000) != 0);
	prn_byp    = ((modsel & 0x0000f00) != 0);
	beg_frm    = ((modsel & 0x00000f0) != 0);
	fp_byp     = ((modsel & 0x000000f) != 0);

	/* Set up the register bits */
	cregs[0] |= (tst_path << 15) | (tstb_a << 14) | (tst_wri << 13);
	cregs[0] |= (tst_rdi << 12) | (enableba << 11) | (enablebd << 6);
	cregs[10] |= (outsel << 8) | (bc_tst << 6) | (ldv_byp << 5);
	cregs[10] |= (frmnum_tst << 4) | (fmtnum_tst << 3);
	cregs[10] |= (prn_byp << 2) | (beg_frm << 1) | fp_byp;

	/* Move registers to the board */
	mb.w[RLOC_RPI] = cregs[0];
	asp_write( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
	mb.w[RLOC_EDFM + 2] = cregs[10];
	asp_write( (RLOC_EDFM+2)<<1, &mb.w[RLOC_EDFM+2], 2 );
}

/* setup_edfm(sbm,fsm,og_obsel,maxfsdo) --------------------------------
	This routine sets up the registers on the ERS-1 deformatter.
*/
setup_edfm(sbm,fsm,og_obsel,maxfsdo)
int sbm, fsm, og_obsel, maxfsdo;
{
	/* Limit arguments */
	sbm &= 0xf;
	fsm &= 0x1f;
	og_obsel &= 0x1;
	maxfsdo &= 0xff;

	/* Clear applicable cregs[] */
	cregs[0] = 0;
	asp_read( (RLOC_EDFM+1)<<1, &mb.w[RLOC_EDFM+1], 2 );
	cregs[1] = mb.w[RLOC_EDFM + 1] & 0x0b00;

	/* Set up the register bits */
	cregs[0] = (sbm << 8) | fsm;
	cregs[1] |= (og_obsel << 10) | maxfsdo;

	/* Move the control registers to the board */
	mb.w[RLOC_EDFM] = cregs[0];
	mb.w[RLOC_EDFM + 1] = cregs[1];
	asp_write( RLOC_EDFM<<1, &mb.w[RLOC_EDFM], 4 );
}

/* reset_rpi (reset) --------------------------------------------------------
	This routine toggles the RPI reset bit off, on, then off again.
*/
reset_rpi(reset)
int reset;
{
	asp_read( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
	if (reset) mb.w[RLOC_RPI] &= 0xfffe;  /* Set reset bit low */
	else mb.w[RLOC_RPI] |= 0x1;     /* Set reset bit high */
	asp_write( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
}

/* reset_edfm(rstbecntr,doarm) -----------------------------------------
	This routine resets the ERS-1 deformatter bit error counter
	and/or arms the frame sync dropout detector.
*/
reset_edfm(rstbecntr,doarm)
int rstbecntr, doarm;
{
	asp_read( (RLOC_EDFM+1)<<1, &mb.w[RLOC_EDFM+1], 2 );
	if (rstbecntr) mb.w[RLOC_EDFM + 1] &= 0xfdff;  /* Set rstbecntr low */
        else mb.w[RLOC_EDFM + 1] |= 0x0200;  /* Set rstbecntr high */
	if (doarm) mb.w[RLOC_EDFM + 1] |= 0x0100;  /* Set doarm high */
        else mb.w[RLOC_EDFM + 1] &= 0xfeff;  /* Set doarm low */
	asp_write( (RLOC_EDFM+1)<<1, &mb.w[RLOC_EDFM+1], 2 );
}

/* set_rpi_mode(mode) --------------------------------------------------
	This routine sets the RPI Interface and Control Unit
	read/write mode.
*/
set_rpi_mode(mode)
int mode;
{
	/* limit value */
	if (mode != 1 || mode != 2) mode = 4;

/* load value into register */
	asp_read( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
	cregs[0] = mb.w[RLOC_RPI] & 0xfff1;
	mb.w[RLOC_RPI] = cregs[0] | (mode << 1);
	asp_write( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
}

/* set_rpi_offset(offset) ----------------------------------------------
	This routine sets the RPI Interface and Control Unit
	frame sync code offset.
*/
set_rpi_offset(offset)
int offset;
{
	cregs[3] = offset;
	mb.w[RLOC_RPI + 3] = cregs[3];
	asp_write( (RLOC_RPI+3)<<1, &mb.w[RLOC_RPI+3], 2 );
}

/* set_rpi_pos(offset,shift,norm_inv) ----------------------------------
	This routine sets the RPI Interface and Control Unit
	frame sync code offset, bit shift and data polarity.
*/
set_rpi_pos(offset,shift,norm_inv)
int offset,shift,norm_inv;
{
	/* Limit arguments */
	shift &= 0x7;
	norm_inv &= 0x1;

	/* Clear applicable cregs[] */
	asp_read( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
	cregs[0] = mb.w[RLOC_RPI] & 0xf87f;

	/* Set up the register bits */
	cregs[0] |= (shift << 8) | (norm_inv << 7);
	cregs[3] = offset;

	/* Move the control registers to the board */
	mb.w[RLOC_RPI + 0] = cregs[0];
	asp_write( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
	mb.w[RLOC_RPI + 3] = cregs[3];
	asp_write( (RLOC_RPI+3)<<1, &mb.w[RLOC_RPI+3], 2 );
}

/* get_rpi_fifostat(dataready,rdstat,wrstat) ---------------------------
	This routine returns the RPI Board read only fifo status.
	The arguments are passed as addresses.
	rdstat and wrstat return the state of each fifo as:
		0 = empty
		1 = less than half full
		2 = at least half full
		3 = full
*/
get_rpi_fifostat(dataready,rdstat,wrstat)
int *dataready, *rdstat, *wrstat;
{
	int rdbits, wrbits;
	static int trans[8] = { 0, 3, 0, 3, 0, 2, 0, 1 };

	/* Get register from board */
	asp_read( (RLOC_RPI+1)<<1, &mb.w[RLOC_RPI+1], 2 );
	cregs[1] = mb.w[RLOC_RPI + 1];

	/* Get values from cregs */
	*dataready = (cregs[1] >> 6) & 0x1;
	rdbits = (cregs[1] >> 3) & 0x7;
	wrbits = cregs[1] & 0x7;
	*rdstat = trans[rdbits];
	*wrstat = trans[wrbits];
}

/* get_rpi_stat(doflg,becnt,frmnum,fmtnum) -----------------------------
	This routine gets the read only status bits not concerned with 
	the fifo.  The arguments must be passed as addresses.
*/
get_rpi_stat(doflg,becnt,frmnum,fmtnum)
int *doflg, *becnt, *frmnum, *fmtnum;
{
	/* Get values from registers */
	asp_read( (RLOC_EDFM+3)<<1, &mb.w[RLOC_EDFM+3], 8 );
	*doflg = mb.w[RLOC_EDFM + 3] & 0x1;
	*becnt = mb.w[RLOC_EDFM + 4] & 0xffff;
	*frmnum = mb.w[RLOC_EDFM + 5] & 0x1f;
	*fmtnum = mb.w[RLOC_EDFM + 6] & 0xffff;
}

/* get_rpi_fifo(raw_dat,words) -----------------------------------------
	This routine reads data from the RPI Board read FIFO.
*/
int get_rpi_fifo(raw_dat,words)
short int raw_dat[];
int words;		/* number of 16-bit words to read */
{
	int   i;
	int   dataready,rdstat,wrstat;
	bt_param_t bt_par;

/* set byte swap mode, swap bytes on 2-byte transfers */

	set_vme_param( 12, 1 );

/* Read words number of data */

	for (i=0; i<words; i++) asp_read( (RLOC_RPI+2)<<1, &raw_dat[i], 2 );

/* reset the byte swap mode */

	set_vme_param( 12, 0 );

/* check the ending status */

	get_rpi_fifostat(&dataready,&rdstat,&wrstat);
	if (!dataready) i = 0;
	return(i);
}

/* get_rpi_blk(raw_dat) ------------------------------------------------
	This routine gets a block of data from the RPI Board read FIFO.
*/
int get_rpi_blk(raw_dat)
short int raw_dat[BLKSIZE];
{
	return (get_rpi_fifo(raw_dat,BLKSIZE));
}

/* write_rpi_fifo(out_dat,words) ---------------------------------------
	This routine writes data to the RPI Board write FIFO.
*/
write_rpi_fifo(out_dat,words)
short int out_dat[BUFSIZE];
int words;    /* Number of 16-bit words to write */
{
    int   i;

    /* write buffer data */
    for (i = 0; i < words; i++) asp_write( (RLOC_RPI+2)<<1, &out_dat[i], 2 );
/*	mb.w[RLOC_RPI + 2] = out_dat[i];	*/
}

/* play_dis (disable) --------------------------------------------------------
	This routine sets or removes enable play bit for the DCRSi.
*/
play_dis (disable)
int disable;
{
	asp_read( (RLOC_EDFM+1)<<1, &mb.w[RLOC_EDFM+1], 2 );
	if (disable) mb.w[RLOC_EDFM + 1] |= 0x1000;  /* disable */
	else mb.w[RLOC_EDFM + 1] &= 0xefff;  /* enable */
	asp_write( (RLOC_EDFM+1)<<1, &mb.w[RLOC_EDFM+1], 2 );
}
