/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* st_play.c -- routines to set up the DCRSi and RPI to
		play data from the tape

*/


#include <stdio.h>
#include <procfil.h>
#include <procdec.h>
#include <sony.h>

#define  LENGTH     132   /* length of response vector         */
#define  STATLEN     30   /* length of the status register     */
#define  BLKSIZE   4356   /* length of one DCRSi block         */
#define  E_SIZE    7424   /* length of 1 ers-1 format          */

extern int vbose;

int    drivesel,rev;

/* start_byte_play(block) ----------------------------------------------
	This routine sets up the DCRSi and RPI to play data through
	the RPI raw data register into the MC5600.  The read will
	start at the first byte of the given block number.
*/


start_byte_play (block)
	int     block;
{
	char    rcvd[LENGTH];
	char    cmd[30];
	int     i;
	int	rcdr = IN;
        int     dc_code;

    /* put the RPI board in reset, and DCRS-to-Masscomp mode */
	reset_rpi(1);
drivesel=0;
rev=0;
	setup_rpi(0,2,0,0,1,1,drivesel,rev);
	play_dis(1);

    /* Get the recorder ready to play  */
	dc_send_cmd(rcdr,"EE");		/* enable ECC */
        if (dc_get_prompt(rcdr,2,rcvd,NULL) == FAIL)
             return(FAIL);
       
        if (vbose) printf ("message from EE\n");

        dc_send_cmd(rcdr,"PD");         /* select parallel data */
        if (dc_get_prompt(rcdr,2,rcvd,NULL) == FAIL)
             return(FAIL);
        if (vbose) printf ("message from PD\n");

	dc_clr_stat(rcdr);		/* Clear status */

    /* build and send play command */
	sprintf(cmd,"PL a%d,%d",block,3000000);
	dc_send_cmd(rcdr,cmd);
	printf ("DCRSi finding starting address (%d). . .\n\n", block);
	fflush(stdout);

    /* Wait until the DCRSi has found the address.  Do this by checking
       for a status code C022.  Timeout after 400 seconds            */
	rcvd[0] = '\0';
	for (i = 0; i < 80; i++) {

	    dc_code = dc_get_resp(rcdr,5,rcvd);
	    if ((dc_code == 290) | (dc_code == 261))
/*
            if (dc_get_resp(rcdr,5,rcvd) == FAIL)
*/
		return (FAIL);

	    if (strspn("DS C022", rcvd) == 7) {
		printf("Got it . . .\n");
		if (op_strstr(rcvd,"E290") != NULL)
		    return (FAIL);  
	    /* toggle RPI reset */
		reset_rpi(0);
		play_dis(0);
		return (PASS);
	    }  /* if */
	}  /* for i */

	printf("Timed out \n");
	dc_stop (rcdr);
	return (FAIL);
}


/* start_fmt_play(satellite,block,bit_offset,polarity) ---------------
	This routine sets up the DCRSi and RPI to play data through
	the given satellite deformatter.  The block, offset and shift
	define the start of the first format to be played.  This routine
	calculates the actual block and offset which must be used to
	start playing through to the IIF correctly.  Currently, these
	are:
		actual block = block - 1
		actual offset = offset
		if satellite=0, the block and offset are adjusted back
		one ers-1 format.
	The input arguments are:
	    satellite  : 0 = ERS, 1 = JERS, 2 = RADARSAT
	    block      : Starting cassette tape block
	    bit_offset : Byte offset of start format within block
	    polarity   : 1 = normal, 0 = inverted

	NOTE: The IIF board must be set up and ready before this
	      routine is called.
*/

start_fmt_play (satellite,block,bit_offset,polarity)
	int satellite,block,bit_offset,polarity;
{
	int     act_block,act_offset;
	int     offset,shift;
	int     magic_offset = 269;
	char    rcvd[LENGTH];
	char    cmd[30];
	int     i;
	int	rcdr = IN;
	short int creg;

    /* calculate actual block and offset */
	offset = (bit_offset >> 3);
	shift = bit_offset & 0x7;
	act_block = block - 1;
	act_offset = offset;
    /* special adjustment for ERS-1: back up one format */
	if (satellite == 0) {
	    act_offset -= E_SIZE;
	    while (act_offset < 0) {
		act_offset += BLKSIZE;
		act_block--;
	    }
	}

    /* set up the eventual RPI control reg contents */
	satellite &= 0x0003;
	polarity &= 0x0001;

    /* put the RPI board in a known configuration: mode=0, reset=1 */
	reset_rpi(1);
drivesel=0;
rev=0;
	setup_rpi(satellite,4,act_offset,shift,polarity,1,drivesel,rev);
	play_dis(1);

/*	if (abob()) exit(1);           */

    /* Get the recorder ready to play  */
	dc_send_cmd(rcdr,"EE");		/* enable ECC */
        if (dc_get_prompt(rcdr,2,rcvd,NULL) != FAIL)
             if (vbose) printf ("message from EE\n");

        dc_send_cmd(rcdr,"PD");         /* select parallel data */
        if (dc_get_prompt(rcdr,2,rcvd,NULL) != FAIL)
             if (vbose) printf ("message from PD\n");

	dc_clr_stat(rcdr);		/* Clear status */

    /* build and send play command */
	sprintf(cmd,"PL a%d,%d",act_block,3000000);
	dc_send_cmd(rcdr,cmd);
	printf ("DCRSi finding starting address (%d). . .\n\n", block);
	fflush(stdout);

    /* Wait until the DCRSi has found the address.  Do this by checking
       for a status code C022.  Timeout after 400 seconds            */
	rcvd[0] = '\0';
	for (i = 0; i < 80; i++) {
	    dc_get_resp(rcdr,5,rcvd);
	    if (strspn("DS C022", rcvd) == 7) {
		printf("Got it . . .\n");
		if (op_strstr(rcvd,"E290") != NULL)
		    return (FAIL);  
		return (PASS);
	    }  /* if */
	}  /* for i */

	printf("Timed out \n");
	dc_stop (rcdr);
	return (FAIL);
}

/* sony_start_byte_play(block) ----------------------------------------------
	This routine sets up the SONY and RPI to play data through
	the RPI raw data register into the MC5600.  The read will
	start at the first byte of the given block number.
*/


sony_start_byte_play (block)
	int     block;
{
	int     i;
	int	rcdr = IN;
        int     endblock;


    /* put the RPI board in reset mode */
	reset_rpi(1);
drivesel=1;
rev=0;
        setup_rpi(0,2,0,0,1,1,drivesel,rev);
	play_dis(1);

    /* Get the recorder ready to play  */

/*
        sony_init(rcdr);
	sony_clr_stat(rcdr);		
*/
    /* build and send play command */
        endblock = block + 90909; /*almost the same as 3000000 for DCRSi*/
        if (sony_playback(rcdr, block, endblock) == FAIL) {
          printf("sony_start_byte_play: cann't sony_playback\n");
          return(FAIL);
        }

        printf("Got it ...\n");

/* toggle RPI reset */
        reset_rpi(0);
	play_dis(0);
	return (PASS);

}/*sony_start_byte_play*/



/* sony_start_fmt_play(satellite,block,bit_offset,polarity) ---------
	This routine sets up the SONY and RPI to play data through
	the given satellite deformatter.  The block, offset and shift
	define the start of the first format to be played.  This routine
	calculates the actual block and offset which must be used to
	start playing through to the IIF correctly.  Currently, these
	are:
		actual block = block - 1
		actual offset = offset
		if satellite=0, the block and offset are adjusted back
		one ers-1 format.
	The input arguments are:
	    satellite  : 0 = ERS, 1 = JERS, 2 = RADARSAT
	    block      : Starting cassette tape block
	    bit_offset : Byte offset of start format within block
	    polarity   : 1 = normal, 0 = inverted

	NOTE: The IIF board must be set up and ready before this
	      routine is called.
*/

sony_start_fmt_play (satellite,block,bit_offset,polarity)
	int satellite,block,bit_offset,polarity;
{
	int     act_block,act_offset;
	int     offset,shift;
	int     magic_offset = 269;
	char    rcvd[LENGTH];
	char    cmd[30];
	int     i;
	int	rcdr = IN;
	short int creg;
   
        int endblock;

    /* calculate actual block and offset */

/*
        bit_offset -= 19*8;
*/

/*
bit_offset += 32;
*/


printf("***block=%d,bit_offset=%d\n", block,bit_offset);
	offset = (bit_offset >> 3);
	shift = bit_offset & 0x7;
	act_block = block - 1;
	act_offset = offset ;
    /* special adjustment for ERS-1: back up one format */
	if (satellite == 0) {
	    act_offset -= E_SIZE;
	    while (act_offset < 0) {
		act_offset += ID1_BLOCK_LEN;
		act_block--;
	    }
	}

    /* set up the eventual RPI control reg contents */
	satellite &= 0x0003;
	polarity &= 0x0001;

    /* put the RPI board in a known configuration: mode=0, reset=1 */
	reset_rpi(1);
drivesel=1;
rev=0;

/*
act_offset -= 19;
*/
printf("****act_offset=%d, shift=%d\n", act_offset, shift);
        setup_rpi(satellite,4,act_offset,shift,polarity,1,drivesel,rev);
	play_dis(1);
/*
	if (abob()) exit(1);           
*/
    /* Get the recorder ready to play  */

/*
        sony_init(rcdr);
	dc_clr_stat(rcdr);		
*/
    /* build and send play command */
        endblock = block + 90909; /*almost the same as 3000000 for DCRSi*/
        if (sony_playback(rcdr, block, endblock) == FAIL) {
          printf("sony_start_fmt_play: cann't sony_playback\n");
          return(FAIL);
        }


        printf("Got it ...\n");
	return (PASS);

}/*sony_start_fmt_play*/




/* sony_start_byte_play_REV(block) ----------------------------------------------
	This routine sets up the SONY and RPI to reverse_play data through
	the RPI raw data register into the MC5600.  The read will
	start at the first byte of the given block number.
*/


sony_start_byte_play_REV (block)
	int     block;
{
	int     i;
	int	rcdr = IN;
        int     endblock;


    /* put the RPI board in reset mode */
	reset_rpi(1);
drivesel=1;
rev=1;
        setup_rpi(0,2,0,0,1,1,drivesel,rev);
	play_dis(1);

    /* Get the recorder ready to play  */

/*
        sony_init(rcdr);
	sony_clr_stat(rcdr);		
*/

    /* build and send play command */
        if ((block - 90909) < 0) 
           endblock = 1;
        else 
           endblock = block - 90909; /*almost the same as 3000000 for DCRSi*/
        if (sony_playback_REV(rcdr, block, endblock) == FAIL) {
          printf("sony_start_byte_play_REV: cann't sony_playback_REV\n");
          return(FAIL);
        }

        printf("Got it ...\n");

/* toggle RPI reset */
        reset_rpi(0);
	play_dis(0);
	return (PASS);

}/*sony_start_byte_play_REV*/



/* sony_start_fmt_play_REV(satellite,block,bit_offset,polarity) ---------
	This routine sets up the SONY and RPI to reverse_play data through
	the given satellite deformatter.  The block, offset and shift
	define the start of the first format to be played.  This routine
	calculates the actual block and offset which must be used to
	start playing through to the IIF correctly.  Currently, these
	are:
		actual block = block - 1
		actual offset = offset
		if satellite=0, the block and offset are adjusted back
		one ers-1 format.
	The input arguments are:
	    satellite  : 0 = ERS, 1 = JERS, 2 = RADARSAT
	    block      : Starting cassette tape block
	    bit_offset : Byte offset of start format within block
	    polarity   : 1 = normal, 0 = inverted

	NOTE: The IIF board must be set up and ready before this
	      routine is called.
*/

sony_start_fmt_play_REV (satellite,block,bit_offset,polarity)
	int satellite,block,bit_offset,polarity;
{
	int     act_block,act_offset;
	int     offset,shift;
	int     magic_offset = 269;
	char    rcvd[LENGTH];
	char    cmd[30];
	int     i;
	int	rcdr = IN;
	short int creg;
   
        int endblock;

printf("***block=%d,bit_offset=%d\n", block,bit_offset);

    /* calculate actual block and offset */
	offset = (bit_offset >> 3);
	shift = bit_offset & 0x7;
	act_block = block - 1;
	act_offset = offset;
    /* special adjustment for ERS-1: back up one format */
	if (satellite == 0) {
	    act_offset -= E_SIZE;
	    while (act_offset < 0) {
		act_offset += ID1_BLOCK_LEN;
		act_block--;
	    }
	}

    /* set up the eventual RPI control reg contents */
	satellite &= 0x0003;
	polarity &= 0x0001;

    /* put the RPI board in a known configuration: mode=0, reset=1 */
	reset_rpi(1);
drivesel=1;
rev=1;
printf("****act_offset=%d, shift=%d\n", act_offset, shift);
        setup_rpi(satellite,4,act_offset,shift,polarity,1,drivesel,rev);
	play_dis(1);

/*	if (abob()) exit(1);           */

    /* Get the recorder ready to play  */

/*
        sony_init(rcdr);
	dc_clr_stat(rcdr);	
*/

    /* build and send play command */
/*
        if ((block - 90909) < 0) 
           endblock = 1;
        else 
           endblock = block - 90909; 
*/
        endblock = 1;
        if (sony_playback_REV(rcdr, block, endblock) == FAIL) {
          printf("sony_start_fmt_play_REV: cann't sony_playback_REV\n");
          return(FAIL);
        }


        printf("Got it ...\n");
	return (PASS);

}/*sony_start_fmt_play_REV*/
