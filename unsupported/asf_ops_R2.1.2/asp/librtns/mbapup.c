/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbapup.c - initialize the ASP registers

     This module initializes the Alaska SAR Processor.
     It is assumed that the mb array has already been mapped.

     The following routines are included in this module:

     bypass_pipe() - Puts all boards in bypass mode; also
		 sets the controls on all the boards in a
		 reasonable state avoiding any special modes.
		 Does not load any function memories.
		 No test taps are enabled.  The EXEC is put
		 in selftest mode.
     pup() - This library routine sets the ASP in a
	     reasonable power-up configuration.  The set-up
	     may be inconsistent for the entire processor
	     (i.e. the registers on the boards may be set
	     for differing line length).  The attempted
	     configuration is a frame of 8K in range by 1K in
	     azimuth in 4 look processing mode.
	     It closely reminds the default values of the
	     standard processing mode.
	     It requires that the system be using the new
	     Executive Module developed for Alaska SAR.
             bypass_pipe() is called to bypass the rest
	     of the pipe.
             pup() routine also uses the ex_init() library
             routine to initialize the Exec Module.

     Processor clock speed is set in pup () through ex_init ().

	Modification log:

	05/06/89	RM(R,I) modified to have 0 in third nibble
	in register 1; also, 1 in STOPSTART/FREE' (bit 7) in register 0.

	06/30/89        IIF setup added.
	10/17/89        IIF setup fixed.
*/

#include <aspdecl.h>

/* bypass_pipe() -------------------------------------------------------
   This routine puts all the ASP boards in bypass mode.
   Also sets the controls on the boards in a reasonable
   state, avoiding any special modes.
*/
bypass_pipe()
{
    int    pid, i;

    /* Save current control computer page ID */
    pid = mb.w[RLOC_REP];
    mb.w[RLOC_REP] = 0x0;  /* Set page ID for page 0 */

			    /* Exec Cage */

/* Exec registers are initialized in ex_init() */

    /* stimulus */
/*  mb.w[RLOC_STIM] = 0x0000;           /* */

    /* response */
/*  mb.w[RLOC_RESP] = 0x0000;           /* */

    /* control */
/*  mb.w[RLOC_CONT] = 0x0000;           /* */

    /* recorder processor interface	*/
 
  mb.w[RLOC_RPI] = 0xf084;		/* bypass all, no test	*/
  asp_write( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );

  mb.w[RLOC_EDFM] = 0x0000;		/* SBM = 0 ; FSM = 0	*/
  mb.w[RLOC_EDFM + 1] = 0xf6ff;		/* OGSEL ,MAXFSDO = 255 */
  mb.w[RLOC_EDFM + 2] = 0xf8ff; 	/* norm data, bypass all,
					   no tests selected	*/
  asp_write( RLOC_EDFM<<1, &mb.w[RLOC_EDFM], 6 );
 
    /* input interface */

  mb.w[RLOC_IIF] = 0xfe89;	/* bypass all, reset, no test, input from RPI,
					long header, full swath */
  mb.w[RLOC_IIF + 1] = 0x15ef;          /* 5,616 points in sampling window */
  mb.w[RLOC_IIF + 2] = 0x03ff;          /* 1,024 lines per frame */
  mb.w[RLOC_IIF + 3] = 0x0015;          /* 22 frames per scene */
  mb.w[RLOC_IIF + 4] = 0x8000;          /* 32K lines (all) at same range gate */
  mb.w[RLOC_IIF + 5] = 0x0000;          /* 0 offset for range lines */
  mb.w[RLOC_IIF + 6] = 0x0000;          /* no offset change */
  mb.w[RLOC_IIF + 7] = 0x0542;          /* 1,346 points offset per segment */
  mb.w[RLOC_IIF + 8] = 0x0003;          /* 4 staged FFT segments */
  mb.w[RLOC_IIF + 9] = 0x000b;          /* 2K FFT per segment */
  mb.w[RLOC_IIF + 10] = 0x0400;         /* 1K invalid lines per frame */
  asp_write( RLOC_IIF<<1, &mb.w[RLOC_IIF], 22 );

			    /* Range Correlator Cage */

    /* The controls are set for staged FFT algorithm with
       8K actual line partitioned into four 2K lines.
       Transform length is 2K, period of the line sync is
       2K. All stages should have scale OFF, but for
       the requirement that bypassed stages must have their
       bypass on.  Also, the default setting for FFT is 8K
       transform; the actual transform length being selected
       by taking appropriate stages out of bypass.          */

    /* range fwd fft #1 */
    mb.w[RLOC_FFT_RF1] = 0xdbca;        /* 8K transform length */
    mb.w[RLOC_FFT_RF1 + 1] = 0x0f0f;    /* bypass all stages, fwd, scale all */
    mb.w[RLOC_FFT_RF1 + 2] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FFT_RF1 + 3] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FFT_RF1<<1, &mb.w[RLOC_FFT_RF1], 8 );

    /* range fwd fft #2 */
    mb.w[RLOC_FFT_RF2] = 0x9876;        /* 8K transform length, back */
    mb.w[RLOC_FFT_RF2 + 1] = 0x0f0f;    /* bypass all stages , fwd, scale all */
    mb.w[RLOC_FFT_RF2 + 2] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FFT_RF2 + 3] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FFT_RF2<<1, &mb.w[RLOC_FFT_RF2], 8 );

    /* range fwd five */
    mb.w[RLOC_FIVE_RF] = 0x543f;        /* 8K transform length, end */
    mb.w[RLOC_FIVE_RF + 1] = 0x07ff;    /* all stages bypassed, fwd, don't scale */
    mb.w[RLOC_FIVE_RF + 2] = 0x0007;    /* clear overflows */
    mb.w[RLOC_FIVE_RF + 3] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FIVE_RF + 4] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FIVE_RF<<1, &mb.w[RLOC_FIVE_RF], 10 );

    /* range fwd umr */
    mb.w[RLOC_UMR_RF] = 0xc2eb;         /* rref, bypass all stages, 2K line, no test */
    mb.w[RLOC_UMR_RF + 1] = 0x0000;     /* don't rotate (0)*/
    asp_write( RLOC_UMR_RF<<1, &mb.w[RLOC_UMR_RF], 4 );

    /* range inv fft #1 */
    mb.w[RLOC_FFT_RI1] = 0xdbca;        /* 8K transform length, front */
    mb.w[RLOC_FFT_RI1 + 1] = 0x0e0f;    /* all stages bypassed, inv, scale all */
    mb.w[RLOC_FFT_RI1 + 2] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FFT_RI1 + 3] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FFT_RI1<<1, &mb.w[RLOC_FFT_RI1], 8 );

    /* range inv fft #2 */
    mb.w[RLOC_FFT_RI2] = 0x9876;        /* 8K transform length, back */
    mb.w[RLOC_FFT_RI2 + 1] = 0x0e0f;    /* bypass all, stages fwd, scale all */
    mb.w[RLOC_FFT_RI2 + 2] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FFT_RI2 + 3] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FFT_RI2<<1, &mb.w[RLOC_FFT_RI2], 8 );

    /* range inv five */
    mb.w[RLOC_FIVE_RI] = 0x543f;        /* 8K transform length, end */
    mb.w[RLOC_FIVE_RI + 1] = 0x0607;    /* all stages bypassed, inv, scale all */
    mb.w[RLOC_FIVE_RI + 2] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FIVE_RI + 3] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FIVE_RI + 4] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FIVE_RI<<1, &mb.w[RLOC_FIVE_RI], 10 );

    /* range inv umr */
    mb.w[RLOC_UMR_RI] = 0x42eb;         /* rcomp, bypass all stages, 2K line, no test */
    mb.w[RLOC_UMR_RI + 1] = 0x0000;     /* don't rotate (0)*/
    asp_write( RLOC_UMR_RI<<1, &mb.w[RLOC_UMR_RI], 4 );

                            /* Corner Turn Cage */

    /* ctc */
    mb.w[RLOC_CTC] = 0xb623;            /* bypass all stages, reset, no test, cont, 2K out line length */
    mb.w[RLOC_CTC + 1] = 0x1334;        /* 4916 valid lines per frame out */
    mb.w[RLOC_CTC + 2] = 0x03ff;        /* 1024 points out from first buffer */
    mb.w[RLOC_CTC + 3] = 0x0400;        /* 1024 points out from second buffer */
    mb.w[RLOC_CTC + 4] = 0x0544;        /* use 1348 points from each input line */
    mb.w[RLOC_CTC + 5] = 0x0001;        /* always valid lines */
    mb.w[RLOC_CTC + 6] = 0x0000;        /* clear overflow */
    mb.w[RLOC_CTC + 7] = 0x0000;        /* sendframe off, no rotation (0) */
    asp_write( RLOC_CTC<<1, &mb.w[RLOC_CTC], 16 );

    /* ctm1 */
    mb.w[RLOC_CTM1] = 0xffe0;           /* 8K X 1K mode, enabled dram, no offset mode */
    mb.w[RLOC_CTM1 + 1] = 0x0000;       /* range offset 0 */
    mb.w[RLOC_CTM1 + 2] = 0x0000;       /* azimuth offset 0 */
    asp_write( RLOC_CTM1<<1, &mb.w[RLOC_CTM1], 6 );

    /* ctm2 */
    mb.w[RLOC_CTM2] = 0xffe0;           /* 8K X 1K mode, enabled dram, no offset mode */
    mb.w[RLOC_CTM2 + 1] = 0x0000;       /* range offset 0 */
    mb.w[RLOC_CTM2 + 2] = 0x0000;       /* azimuth offset 0 */
    asp_write( RLOC_CTM2<<1, &mb.w[RLOC_CTM2], 6 );

    /* ctm3 */
    mb.w[RLOC_CTM3] = 0xffe0;           /* 8K X 1K mode, enabled dram, no offset mode */
    mb.w[RLOC_CTM3 + 1] = 0x0000;       /* range offset 0 */
    mb.w[RLOC_CTM3 + 2] = 0x0000;       /* azimuth offset 0 */
    asp_write( RLOC_CTM3<<1, &mb.w[RLOC_CTM3], 6 );

    /* az fwd fft #1 */

    /* For FFT setup, see comments for Range Correlator.
       Intended transform length is 2K.                  */

    mb.w[RLOC_FFT_AF1] = 0xdbca;        /* 8K transform length, front */
    mb.w[RLOC_FFT_AF1 + 1] = 0x0f0f;    /* all stages bypassed, fwd, scale all */
    mb.w[RLOC_FFT_AF1 + 2] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FFT_AF1 + 3] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FFT_AF1<<1, &mb.w[RLOC_FFT_AF1], 8 );

    /* az fwd fft #2 */
    mb.w[RLOC_FFT_AF2] = 0x9876;        /* 8K transform length, back */
    mb.w[RLOC_FFT_AF2 + 1] = 0x0f0f;    /* bypass all, stages fwd, scale all */
    mb.w[RLOC_FFT_AF2 + 2] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FFT_AF2 + 3] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FFT_AF2<<1, &mb.w[RLOC_FFT_AF2], 8 );

    /* az fwd five */
    mb.w[RLOC_FIVE_AF] = 0x543f;        /* 8K transform length, end */
    mb.w[RLOC_FIVE_AF + 1] = 0x0707;    /* all stages bypassed, fwd, scale all */
    mb.w[RLOC_FIVE_AF + 2] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FIVE_AF + 3] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FIVE_AF + 4] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FIVE_AF<<1, &mb.w[RLOC_FIVE_AF], 10 );

			    /* Azimuth Cage */

    /* rm for real data */
    mb.w[RLOC_RMR] = 0x18eb;            /* bypass all stages, real, 4 look, init oflow, 2K line, no test */ 
    mb.w[RLOC_RMR + 1] = 0xff04;        /* sync coefficients */
    asp_write( RLOC_RMR<<1, &mb.w[RLOC_RMR], 4 );

    /* rm for imag data */
    mb.w[RLOC_RMI] = 0x1ceb;            /* bypass all stages, imag, 4 look, init oflow, 2K line, no test */ 
    mb.w[RLOC_RMI + 1] = 0xff04;        /* sync coefficients */
    asp_write( RLOC_RMI<<1, &mb.w[RLOC_RMI], 4 );

    /* xfer */
    mb.w[RLOC_XFER] = 0x014b;           /* bypass all stages, 2K line */
    mb.w[RLOC_XFER + 1] = 0x01ff;       /* 512 points per look */
    mb.w[RLOC_XFER + 2] = 0x0000;       /* look position is zero */
    asp_write( RLOC_XFER<<1, &mb.w[RLOC_XFER], 6 );

    /* az inv fft #1 */
    mb.w[RLOC_FFT_AI1] = 0xdbca;        /* 8K transform length, front */
    mb.w[RLOC_FFT_AI1 + 1] = 0x0e0f;    /* all stages bypassed, inv, scale all */
    mb.w[RLOC_FFT_AI1 + 2] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FFT_AI1 + 3] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FFT_AI1<<1, &mb.w[RLOC_FFT_AI1], 8 );

    /* az inv fft #2 */
    mb.w[RLOC_FFT_AI2] = 0x9876;        /* 8K transform length, back */
    mb.w[RLOC_FFT_AI2 + 1] = 0x0e0f;    /* all stages bypassed, fwd, scale all */
    mb.w[RLOC_FFT_AI2 + 2] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FFT_AI2 + 3] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FFT_AI2<<1, &mb.w[RLOC_FFT_AI2], 8 );

    /* az inv five */
    mb.w[RLOC_FIVE_AI] = 0x543f;        /* 8K transform length, end */
    mb.w[RLOC_FIVE_AI + 1] = 0x0607;    /* all stages bypassed, inv, scale all */
    mb.w[RLOC_FIVE_AI + 2] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FIVE_AI + 3] = 0x0000;    /* clear overflows */
    mb.w[RLOC_FIVE_AI + 4] = 0x0000;    /* clear overflows */
    asp_write( RLOC_FIVE_AI<<1, &mb.w[RLOC_FIVE_AI], 10 );

    /* azint */
    mb.w[RLOC_AZ] = 0x14f0;             /* bypass all stages, no test, uns and int, shift by 16, 16 bits out */
    mb.w[RLOC_AZ + 1] = 0xf904;         /* 512 points per line (look) */
    mb.w[RLOC_AZ + 2] = 0x2000;         /* point spacing is 1.0 */
    asp_write( RLOC_AZ<<1, &mb.w[RLOC_AZ], 6 );

			    /* Multi-look Cage */

    /* mlc/out */
    mb.w[RLOC_MLC] = 0x9166;            /* bypass all stages, 16 bits out, 4 look mode, extended */
    mb.w[RLOC_MLC + 1] = 0x0600;        /* (2K -512) azimuth remainder */
    mb.w[RLOC_MLC + 2] = 0x17ff;        /* 2K azimuth line length, NOV is 511 points */
    mb.w[RLOC_MLC + 3] = 0x0200;        /* 512 points azimuth offset */
    mb.w[RLOC_MLC + 4] = 0x1fff;        /* 8192 points per range line out */
    mb.w[RLOC_MLC + 5] = 0x0000;        /* clear overflow */
    mb.w[RLOC_MLC + 6] = 0xffe9;        /* hist enable, frame block sync, delay is 2 frames */
    mb.w[RLOC_MLC + 7] = 0x01ff;        /* 512 valid points per look */
    asp_write( RLOC_MLC<<1, &mb.w[RLOC_MLC], 16 );

    /* mlm1 */
    mb.w[RLOC_MLM1] = 0xffe8;           /* 8K X 1K mode, enabled dram, offset mode */
    mb.w[RLOC_MLM1 + 1] = 0x0000;       /* range offset 0 */
    mb.w[RLOC_MLM1 + 2] = 0x0000;       /* azimuth offset 0 */
    asp_write( RLOC_MLM1<<1, &mb.w[RLOC_MLM1], 6 );

    /* mlm2 */
    mb.w[RLOC_MLM2] = 0xffe8;           /* 8K X 1K mode, enabled dram, offset mode */
    mb.w[RLOC_MLM2 + 1] = 0x0000;       /* range offset 0 */
    mb.w[RLOC_MLM2 + 2] = 0x0000;       /* azimuth offset 0 */
    asp_write( RLOC_MLM2<<1, &mb.w[RLOC_MLM2], 6 );

    /* mlm3 */
    mb.w[RLOC_MLM3] = 0xffe8;           /* 8K X 1K mode, enabled dram, offset mode */
    mb.w[RLOC_MLM3 + 1] = 0x0000;       /* range offset 0 */
    mb.w[RLOC_MLM3 + 2] = 0x0000;       /* azimuth offset 0 */
    asp_write( RLOC_MLM3<<1, &mb.w[RLOC_MLM3], 6 );

    /* avg */
/*  mb.w[RLOC_AVG] = 0x0000;            /* read only register */
    mb.w[RLOC_AVG + 1] = 0x0ff0;        /* bypass all stages, front path*/
    mb.w[RLOC_AVG + 2] = 0xef6f;        /* don't divide , no test */
    mb.w[RLOC_AVG + 3] = 0xffff;        /* 128K points in a segment */
    asp_write( RLOC_AVG<<1, &mb.w[RLOC_AVG], 8 );

    /* out */
    mb.w[RLOC_OUT] = 0xa5a5;
    asp_write( RLOC_OUT<<1, &mb.w[RLOC_OUT], 2 );

    /* Restore former page ID */

    mb.w[RLOC_REP] = pid;

    /* Now put EXEC in selftest */

    ex_set_selftest(1);
}

/* pup() ---------------------------------------------------------------
   Sets ASP in power-up configuration.  Requires that ASP
   Exec Module be installed.
*/
pup()
{
int i;

    mb.w[RLOC_CNTRL] = 0x008f;
    asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
    for (i = 0x100; i < 0x700; i+=2 ) mb.w[i>>1] = 0xa5a5;
    asp_write( 0x100, &mb.w[0x80], 384 );
    asp_write( 0x300, &mb.w[0x180], 128 );
    asp_write( 0x400, &mb.w[0x200], 128 );
    asp_write( 0x500, &mb.w[0x280], 128 );
    asp_write( 0x600, &mb.w[0x300], 128 );
    mb.w[RLOC_CNTRL] = 0x008f;
    asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
    ex_init();        /* Initialize Exec Module */
    /* Load the entire used register address space 
       (excluding the EXEC) in case we forgot some registers */
    bypass_pipe();    /* Put all boards in bypass */
    mb.w[RLOC_REP] = 0;     /* Set control computer page ID to zero */
}
