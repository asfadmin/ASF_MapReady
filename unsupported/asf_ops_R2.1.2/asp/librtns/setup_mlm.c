/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  setup_mlm - This module sets up the control registers on the
    Multilook Memory Boards in the Multilook Module.  The
    Multilook Controller is set up by another routine.

    The calling sequence and command line arguments are as
    follows:

    setup_mlm(aspect_mode,offm,ramen,rgof,azof)

    aspect_mode - 0 = 2k range x 4k azimuth, 32 bit data
		  1 = 8k range x 1k azimuth, 32 bit data
		  2 = 16k range x 512 azimuth, 32 bit data
		  3 = 8k range x 2k azimuth, 16 bit data

    offm        - 0 = offset mode off
		  1 = offset mode on

    ramen       - 0 = dram read and write disabled
		  1 = dram read and write enabled

    rgof        - Starting offset value for reading in the
		  range axis.

    azof        - Starting offset value for reading in the
		  azimuth axis.
		  
-------------------------------------------------------------
*/

#include <aspdecl.h>

static short int   cregs[3];

/* setup_mlm(aspect_mode,offm,ramen,rgof,azof)-----------------
	This routine sets up the control registers on the
	Multilook Memory boards.
*/
int setup_mlm(aspect_mode,offm,ramen,rgof,azof)
int    aspect_mode, offm;
int    ramen, rgof, azof;
{
    int    i, retval;

    /* Default cregs */
    cregs[0] = 0xffe0;
    cregs[1] = 0;
    cregs[2] = 0;
    
    aspect_mode &= 0x3;  /* Limit argument */

    /* Decode arguments */
    switch (aspect_mode) {
    case 0:
	cregs[0] |= 0x1;
	break;
    case 1:
	break;
    case 2:
	cregs[0] |= (0x1 << 4);
	break;
    case 3:
	cregs[0] |= (0x1 << 2);
	break;
    }

    /* Set up register bits */
    cregs[0] |= (!ramen << 1) | (offm << 3);
    cregs[1] = rgof & 0x3fff;
    cregs[2] = azof & 0x1fff;

    /* Move registers to boards */
    for (i = 0; i < 3; i++) {
	mb.w[RLOC_MLM1 + i] = cregs[i];
	mb.w[RLOC_MLM2 + i] = cregs[i];
	mb.w[RLOC_MLM3 + i] = cregs[i];
    }
    asp_write( RLOC_MLM1<<1, &mb.w[RLOC_MLM1], 6 );
    asp_write( RLOC_MLM2<<1, &mb.w[RLOC_MLM2], 6 );
    asp_write( RLOC_MLM3<<1, &mb.w[RLOC_MLM3], 6 );
    retval = 0;
    return(retval);
}
