/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  setup_out.c -             Eugene Chu 8/2/1995 (R1B Version)
    This module sets up the control registers on the OUT Board.

    setup_out_c	- Sets the SCSI register on the OUT board

    setup_out_s	- Sets the control bus register 0

    out_set_reset - Sets or Resets MASTER_RESET on OUT board

    get_out_stat    - This function gets the status of the
		      read only register (reg6)
			bit 4: WR A/B*
			bit 5: OVERFLOW*
			bit 6: READY*
			bit 7: START*

    get_out_addr    - Returns the address at which the averager
		      is currently writing.
    get_out_mem     - Reads OUTBD memory contents

    The calling sequences and command line arguments for these
    functions are explained below.

int setup_out_c(wr_mode,reading)
    
    wr_mode   - values are loaded into bits 1,2,3 of reg0
    reading   - 1 = Segment currently being read through SCSI
   
int setup_out_s	- Sets the control bus register 0

	enable	- Sets the corresponding modes:
			2 = full mode (32 bits in and out)
			1 = Square root (8 bit mode)
			0 = 16 bit real mode
	reset	- 1 = Master reset, configure status registers in known state.
	test	- 1 = send board output to test bus

get_out_stat(start,ready,overflow)

    start   - Start of scene can be read
    ready   - Segment read since written
    overflow- A segment in the scene was written which was
	      not read (reset on scene start)

get_out_addr(adrdat)

    The command line arguments are passed as addresses:
    adrdat  - Address to which averager is currently writing
	      (17 bits)

get_out_mem(seg_store,seg_len)
    Returns the contents of segment storage.  The amount of
    data copied is given by the hardware SEGSIZE register.
    seg_store   - array to contain the returned memory values, up to
		  8 MBytes of data in 2 segments.
    seg_len     - number of 32 bits words to return

------------------------------------------------------------------------ */

#include <aspdecl.h>
static unsigned char cregs[8];

/* setup_out_init() --------------------------------------------
  This routine initializes the two main control registers and the
	Reg0=0x35
  four SCSI control registers on the OUT board
	Reg0=0xff; all controls deasserted
	Reg 1 thru 3 = 0; read address to zero
*/
int setup_out_init()
{
    cregs[0] = 0xee;	/*  deassert all control signals:
				bit 7,6:  unused
				bit 5:	WRITEONCE*, ignored
				bit 4:	READING*
				bit 3:	DISWR*
				bit 2:	FREEZEWR*
				bit 1:	FREEZEA/B*
				bit 0:	RESETREAD*	*/

    cregs[1]=0;		/*  set read address to 0  */
    cregs[2]=0;
    cregs[3]=0;
    sb(0,4,cregs);

/*  The control bus register  */

    mb.w[RLOC_OUT] = 0x35;	/* preset values:
					bit 6-15 spare
					bit 5:  WRITEONCE*
					bit 4:  ALLOWMLCSTOP*
					bit 3:  FULLMODE
					bit 2:  BYPASSSQRT*
					bit 1:  MASTERRESET*
					bit 0:  TEST*		*/
    asp_write( RLOC_OUT<<1, &mb.w[RLOC_OUT], 2 );
    return (0);
}

/* setup_out_c(wr_mode,reading,reset)-----------------------------------
  This routine sets the OUT Board SCSI bus registers.
	wr_mode:
	0x1000 = Enable writeonce		(bit 5)
	0x0100 = Disable write			(bit 3)
	0x0010 = Enable Freeze write		(bit 2)
	0x0001 = Enable Freeze write A/B*	(bit 1)
	reading:  1=set board to SCSI read mode, 0=allow MLC write
	reset:  1=reset read, reset board, load read address
*/
int setup_out_c( wr_mode, reading, reset )
int   wr_mode, reading, reset;
{
    wr_mode = (wr_mode<1)&0x02 | (wr_mode>2)&0x04 | (wr_mode>5)&0x08
		| (wr_mode>7)&0x20;
    wr_mode = (~wr_mode) & 0x2e;
    cregs[0] |= (unsigned char) wr_mode;
    if( reading ) cregs[0] &= 0xef;
    else cregs[0] |= 0x10;
    if( reset ) cregs[0] &= 0xfe;
    else cregs[0] |= 0x1;
    return(sb( 0, 1, cregs ));
}

/* setup_out_s(enable,reset,test)-----------------------------------
  This routine sets the OUT Board control bus registers.
  The data propagation delay through the board is returned.
*/
int setup_out_s(enable,reset,test)
int   enable, reset, test;
{
    int   i, trigger;
    int   enablesqr, fullmode, allowmlc;

    /* Expand condensed control */
    enablesqr = ((enable & 0x1) != 0);
    fullmode = ((enable & 0x2) !=0);
    allowmlc = ((enable & 0x4) !=0);

    /* Limit arguments */
    test &= 0x1;
    reset &= 0x1;
   
    /* Set up the register bits  on the multibus*/
    cregs[1] = 0x20;	/* writeonce* (?) */
    cregs[1] |= (allowmlc<<4) | (enablesqr<<2) | (fullmode<<3) |
		(!reset<<1) | (!test);
    mb.w[RLOC_OUT] = cregs[1];
    asp_write( RLOC_OUT<<1, &mb.w[RLOC_OUT], 2 );

    /* Set trigger count */
    trigger = 6;
    return(trigger);
}

/* get_out_stat(start,ready,overflow)-----------------------------------
	This routine gets the status of the OUT Board SCSI read only 
	register bits.  The arguments must be passed as addresses.

	The variable will be set to 1 if the corresponding bit
	is set to zero.
*/
get_out_stat(start,ready,overflow)
int   *start, *ready, *overflow;
{
    /* Get registers from board */
    rc( 0, 8, cregs );
    *start = (cregs[6]&0x80)==0;
    *ready = (cregs[6]&0x40)==0;
    *overflow = (cregs[6]&0x20)==0;
}

/* get_out_addr(adrdat)-------------------------------------------------
	This routine returns the address at which the OUT Board
	is currently writing.
*/
get_out_addr(adrdat)
int *adrdat;
{
    rc( 4, 4, cregs );
    *adrdat = ( ((int)cregs[2] & 0x0f)<<16 ) | ( ((int)cregs[1])<<8 )
		| (int)cregs[0];
    return;
}

/* get_out_mem(seg_store,seg_len,wr_mode,overflow)---------------------------
	This routine gets the contents of the OUT Board segment memories
	and saves the overflow status.
*/
get_out_mem(seg_store,seg_len, wr_mode, overflow )
DATA seg_store[];  		/* Segment Memory */
int seg_len, wr_mode, *overflow;
{
    int status;

    if( (wr_mode&0xf0)!=0 ){			/*  freezewrite  */
	if( (wr_mode&0xf)!=0 ) status = 2;	/*  freezewrite B  */
	else status = 1;			/*  freezewrite A  */
    } else {
	status = 0;				/*  no freezewrite  */
    }
    if( (wr_mode&0xf000)!=0 ) status |= 4;	/*  writeonce (FUA)  */
    else status &= 0xb;

printf("Calling RDB to read %d bytes, no CONTROL-C please...\n", seg_len<<2 );
    status = rdb( 0, seg_len>>7, seg_store, status, overflow );
printf("Returned from RDB, status=%d\n", status );
    if( status<0 ){
	printf("GET_OUT_MEM error reading segment memory\n");
	*overflow = 1;
    }
    return;
}

/* out_set_reset(reset) ------------------------------------------------
	This routine sets the board in reset (reset = 1) or 
	operational (reset = 0) state.
*/
int out_set_reset (reset)
int reset;
{
	asp_read( RLOC_OUT<<1, &mb.w[RLOC_OUT], 2 );
	if (reset) mb.w[RLOC_OUT] &= ~0x2;
	else mb.w[RLOC_OUT] |=  0x2;
	asp_write( RLOC_OUT<<1, &mb.w[RLOC_OUT], 2 );
}
