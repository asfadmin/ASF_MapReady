/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* setup_nexec.c -- This module sets up the registers and
	handles memory read and write for the new style Executive
	Module developed for the Alaska SAR project.
	This module consists of five major functions:

        setup_ex_stim - Set up Stimulus Board registers
	setup_ex_resp - Set up Response Board registers
        setup_ex_cntrl- Set up Control Board registers
	get_ex_stat   - Read Response and Control Status registers

	These function and their calling arguments are explained
	below.  All arguments are positive logic.

setup_ex_stim(zeroto,pgaddr,lsyncpow,reset,rawmbmode,codeper,
                   bufstart,wordsout)
	This function sets the contents of the 6 registers on the Exec
	Module Stimulus Board.  It returns ...
    zeroto     - Zero data output of Stimulus Buffer
    pgaddr     - Page of memory in Stimulus Buffer to access
    lsyncpow   - Clock cycles between syncs in power of 2 notation
    reset      - Each nibble sets (1) or resets (0) the corresponding
		 reset bit:
		 0x10 = RAWRESET will reset counters
		 0x01 = Reset Exec module (RAWRESET)
    rawmbmode  - 0 = Normal MB access to Buffer momory
		 1 = Block mode MB access to Buffer memory
		 2 = Normal MB access to Sync memory
		 3 = Block mode MB access to Sync memory
		 4 = Run mode, normal
		 5 = Run mode, normal
		 6 = Run mode, extended, collect Real data only
		 7 = Run mode, extended, collect Imag data only
    codeper    - Period of the synccode pattern repetition (14 bits)
    bufstart   - Starting address for the output to be read from the
		 Stimulus buffer in number-1 notation.  Two LSBs are
		 hardwired to zero.  Buffer size = 8 Meg of 32 bit
		 words.
		 Range: 0x0 to 0x007fffff  (23 bits)
    wordsout   - Number of data words output from Stimulus Buffer in
		 number-1 notation  (23 bits).

setup_ex_resp(zeroti,selftest,pgaddr,rawmbmode)

  	This function sets up the control register on the Exec Module
	Response Board.  It is not concerned with the read only
	registers.
    zeroti     - Zero the data input to the Response Buffer
    selftest   - Accept input directly from Stimulus Buffer
    pgaddr     - Page of Response Buffer memory to access
    rawmbmode  - 0x00 = Normal MB access to Buffer memory
		 0x01 = Block mode MB access to Buffer memory
                 0x02 = Normal MB access to Sync memory
		 0x03 = Block mode MB access to Sync memory
		 0x04 = Run mode, normal
		 0x05 = Run mode, normal
		 0x06 = Run mode, extended, collect Real data only
		 0x07 = Run mode, extended, collect Imag data only
		 
setup_ex_cntrl(reset,selftest,pgaddr,count_sync,cksel,maskpow,exptrn,
                    delay,eventval,syncmask,syncptrn)
	This function sets up the contents of the registers on the
        Exec Module Control Board.  It is not concerned with the
	read only registers.
    reset      - 1 = RAWRESET will reset all counters
    selftest   - 1 = Take sync input directly from Stimulus Board
    pgaddr     - Page address in Sunc Response memory to access
    count_sync - 0 = Use Sync pattern to cause an event
		 1 = Use Exec counter pattern to cause an event
    cksel      - 0 = 10 MHz
		 1 = 5 MHz
		 2 = 2.5 MHz
		 3 = External Clock
    maskpow    - Mask pattern for Exec counter in power of 2 notation
		 (5 bits).
    execptrn   - Value of the Exec counter on which to produce an
		 event.  (32 bit value)
    delay      - Number of clock periods to continue writing the
		 Response Buffer after a trigger has occurred.
		 (24 bit value)
    eventval   - Number of events which must occur before trigger
		 is generated.  In number-1 notation and minimum
		 value of 1.  (8 bit value)
    syncmask   - Mask showing which bits of the synccode to compare
		 to produce an event (8 bit value)
    syncptrn   - Pattern of synccodes which will produce an event
                 (8 bit value)

        
get_ex_stat(frzaddr,writetob,eventcnt,execcnt,delaycnt,
                arm,triggered)
	This function gets the status of the read only registers
	and fields from the Exec Module Boards.
    frzaddr    - Address of Response Buffer when trigger occurred
		 (32 bit value)
    writetob   - Status of Exec Counter ????
    eventcnt   - Number of events that have already occurred
		 (8 bit value)
    execcnt    - Value of Exec Counter (32 bit value)
    delaycnt   - Value of Delay Counter (24 bit value)
    arm        - 0 = Response Buffer armed
		 1 = Zero crossing has occurred
    triggered  - 0 = not triggered
		 1 = triggered


*/

/*----------------------------------------------------------------*/

#include <aspdecl.h>

/* setup_ex_stim(zeroto,pgaddr,lsyncpow,reset,rawmbmode, ---------------
	    codeper,bufstart,wordsout)
  	This routine sets up the 6 registers on the Stimulus Board
*/
setup_ex_stim(zeroto,pgaddr,lsyncpow,reset,rawmbmode,codeper,bufstart,wordsout)
int    zeroto, pgaddr, lsyncpow;
int    reset, rawmbmode, codeper, bufstart, wordsout;
{
    int    pid, i;
    int    rawreset, resetctr;
    static short int    cregs[6];  /* Contents of board registers */

/*
printf("SETUP_EX_STIM zeroto=%d pgaddr=%x lsyncpow=%x reset=%d\n",
	zeroto, pgaddr, lsyncpow, reset );
printf("	rawmbmode=%x codeper=%d bufstart=%x wordsout=%d\n",
	rawmbmode, codeper, bufstart, wordsout );
*/
    /* Save current page ID  and set for Exec */
    pid = mb.w[0x0];
    mb.w[0x0] = 0x0;

    /* extract reset bits */
    resetctr = ((reset & 0xf0) != 0);
    rawreset = ((reset & 0x0f) != 0);

    for (i=0; i < 6; i++)  /* Clear registers */
	cregs[i] = 0;
    /* Translate arguments into registers fields */
    cregs[0] = ((!zeroto<<15)&0x8000) | ((!resetctr<<14)&0x4000) |
		((pgaddr<<8)&0x3f00)  | ((lsyncpow<<4)&0x00f0) |
		((!rawreset<<3)&0x0008) | (rawmbmode&0x0007);
    cregs[1] |= codeper;
    cregs[2] |= (bufstart & 0xffff);
    cregs[3] |= ((bufstart >> 16) & 0x7f);
    cregs[4] |= (wordsout & 0xffff);
    cregs[5] |= ((wordsout >> 16) & 0x7f);

    /* Move the register contents to the board */
    for (i=0; i < 6; i++) mb.w[RLOC_STIM + i] = cregs[i];
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 12 );
    
    
    /* Restore page ID */
    mb.w[0x0] = pid;
/*
printf("SETUP_EX_STIM complete\n");
*/
    return (0);
}

/* setup_ex_resp (zeroti,selftest,pgaddr,rawmbmode) --------------------
  	This routine sets up the Response Buffer control register.
	It does not involve the read only registers.
*/
setup_ex_resp(zeroti,selftest,pgaddr,rawmbmode)
int    zeroti, selftest, pgaddr, rawmbmode;
{
    int    pid;    /* Page ID */
    static short int    cregs;  /* Contents of writable registers */

/*
printf("SETUP_EX_RESP zeroti=%d selftest=%d pgaddr=%x rawmbmode=%x\n",
	zeroti, selftest, pgaddr, rawmbmode );
*/
    /* Save current page ID and set for Exec */
    pid = mb.w[0x0];
    mb.w[0x0] = 0x0;

    cregs = 0;  /* Clear contents of control register */

    /* Translate arguments into register fields */
    cregs = ((!zeroti<<15)&0x8000) | ((!selftest<<14)&0x4000) |
		((pgaddr<<8)&0x3f00) | (rawmbmode&0x0007);

    /* Move register contents to board */
    mb.w[RLOC_RESP] = cregs;
    asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );

    /* Restore page ID */

/*
printf("SETUP_EX_RESP complete\n");
*/
    return (0);
}

/* setup_ex_cntrl(reset,selftest,pgaddr,count_sync,cksel,maskpow, ------
		    execptrn,delay,eventval,syncmask,syncptrn)
  	This routine sets up the contents of the registers on the
	Control Board.  It does not involve the read only registers
	or fields.
*/
setup_ex_cntrl(reset,selftest,pgaddr,count_sync,cksel,maskpow,execptrn,
		delay,eventval,syncmask,syncptrn)
int    reset, execptrn, delay;
int    selftest, pgaddr, count_sync, cksel;
int    maskpow, eventval, syncmask, syncptrn;
{
    int    pid;   /* Page ID */
    static short int    cregs[7];  /* Contents of writable registers */
    int    i;

/*
printf("SETUP_EX_CNTRL reset=%d selftest=%d pgaddr=%x count_sync=%d\n",
	reset, selftest, pgaddr, count_sync );
printf("	cksel=%d maskpow=%x execptrn=%x delay=%x eventval=%x\n",
	cksel, maskpow, execptrn, delay, eventval );
printf("	syncmask=%x syncptrn=%x\n", syncmask, syncptrn );
*/

    /* Save current page ID and set for Exec */
    pid = mb.w[0x0];
    mb.w[0x0] = 0x0;

    /* Clear contents of control registers */
    for (i=0; i < 7; i++) cregs[i] = 0;
    /* Translate arguments into register fields */
    cregs[0] = ((!reset<<15)&0x8000) | ((!selftest<<14)&0x4000) |
		((pgaddr<<8)&0x3f00) | ((count_sync<<7)&0x0080) |
		((cksel<<5)&0x0060) | (maskpow&0x001f);
    cregs[1] |= (execptrn & 0xffff);
    cregs[2] |= (execptrn >> 16);
    cregs[3] |= (delay & 0xffff);
    cregs[4] |= ((delay >> 16) & 0xff);
    if (eventval == 0) eventval = 1;
    cregs[5] |= (eventval & 0xff);
    cregs[6] = (syncptrn & 0xff) | ((syncmask << 8) & 0xff00);

    /* Move register contents to board */
    for (i=0; i < 7; i++) mb.w[RLOC_CNTRL + i] = cregs[i];
    asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 14 );

    /* Restore page ID */
    mb.w[0x0] = pid;
/*
printf("SETUP_EX_CNTRL complete\n");
*/
    return (selftest ? 7 : 5);
}

/* get_ex_stat(frzaddr,writetob,eventcnt,execcnt,delaycnt,arm, -------
	    triggered)
  	This routine gets the status of the read only registers
	and fields from the Exec Module Response and Control Boards.
	The Stimulus Board does not have any read only registers.
*/

get_ex_stat(frzaddr,writetob,eventcnt,execcnt,delaycnt,arm,triggered)
long   frzaddr, execcnt, delaycnt;
int    writetob, eventcnt, arm, triggered;
{
    int    pid;   /* page ID */
    int    cregs[2];

    /* Save current page ID and set for Exec */
    pid = mb.w[0x0];
    mb.w[0x0] = 0x0;

    /* Clear long arguments */
    frzaddr = 0;
    execcnt = 0;
    delaycnt = 0;

    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 24 );
    /* Get read only registers from Response Board */
    cregs[0] = mb.w[RLOC_RESP + 1];  /* Get LS Word */
    cregs[1] = mb.w[RLOC_RESP + 2];  /* Get MS Word */
    frzaddr = (cregs[1] << 16) | cregs[0];

    cregs[0] = mb.w[RLOC_RESP + 3];
    writetob = !(cregs[0] & 0x1);

    cregs[0] = mb.w[RLOC_RESP + 4];
    arm = cregs[0] & 0x1;

    /* Get read only registers from Control Board */
    cregs[0] = mb.w[RLOC_CNTRL + 5];
    eventcnt = cregs[0];

    cregs[0] = mb.w[RLOC_CNTRL + 7];  /* Get LS Word */
    cregs[1] = mb.w[RLOC_CNTRL + 8];  /* Get MS Word */
    execcnt = (cregs[1] << 16) | cregs[0];

    cregs[0] = mb.w[RLOC_CNTRL + 9];  /* Get LS Word */
    cregs[1] = mb.w[RLOC_CNTRL + 10];  /* Get MS Word */
    delaycnt = (cregs[1] << 16) | cregs[0];

    cregs[0] = mb.w[RLOC_CNTRL + 11];
    triggered = !(cregs[0] & 0x1);

    /* Restore page ID */
    mb.w[0x0] = pid;
}
