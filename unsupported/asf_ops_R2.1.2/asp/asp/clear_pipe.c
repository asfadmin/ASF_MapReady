/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* clear_pipe () -------------------------------------------------------

   This program clears the ASP pipe by loading by sending zeros down
   the pipe from EXEC.  The main puprose to clear CT and ML memories
   so that it is easier to see data captured in the RESP in the
   four look mode in the presence of invalid frames.

   Also, one can see partial (not fully multilooked) frames
   in the four look mode.

   It uses regular processing parameter file with a fixed name
   "clear.par" located in /usr/local/asp.

   Run initmbx() and pup() before calling subroutine.

*/

#include <stdio.h>
#include <aspdecl.h>
#include <procfil.h>
#include <procdec.h>
#include <procpar.h>

extern RQST_PTR Cur_Rqst;
#define FS (~0x01)
#define VL (~0x02)
#define VF (~0x04)
#define AL (~0x08)
#define SS (~0x10)
#define SYNC_EVENT 0

clear_pipe (sp)
	TAPE_SEG_PTR sp;		/* CV 10/3/97 */
{
	static char filename[80] = "/usr/local/asp/clear.par";
	static char filename1[80] = "/usr/local/asp/clear.tom";
	int i;
	int trigger;
	char syncs[8192];
	int wordsout;
	int linelen = 8192;
	int framelen = 1024;

	printf ("FLUSHING THE PIPE...\n");
        /* open the input file; CV 10/3/97 white bar for ST6 & ST7 */
        if ((sp->aux.beam_seq == 6) || (sp->aux.beam_seq == 7)){
           printf("filename1= %s\n", filename1);
           if (read_proc_param_file(filename1) == FAIL) {
               printf ("Cannot read file %s\n",filename);
               exit(1);
           }
        } else {
           /* open the input file */
           printf("filename= %s\n", filename);
           if (read_proc_param_file(filename) == FAIL) {
               printf ("Cannot read file %s\n",filename);
               exit(1);
           }
        }
                                          
    /* pre-fill sync buffer */
	syncs[0] = SS & FS & VF & VL & AL;
	for (i = 1; i < framelen; i++)
	    syncs[i] = VL & AL;
	ex_load_sync(syncs, framelen);

    /* set up EXEC */
	wordsout = linelen * framelen;
	ex_op(power_of_2(linelen),wordsout);
	ex_set_selftest(1);

	asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
	mb.w[RLOC_STIM] &= 0x7fff;	/* zero STIM output */
	asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );

    /* set up triggering */
	ex_set_delay(wordsout);
	trigger = SS & FS & VF & AL & VL;
	ex_set_event (SYNC_EVENT,0x1f,trigger,1);
	t();

    /* set up registers */
	if (p_load_regs() == FAIL) {
	    printf ("problem encountered with parameter file %s\n",filename);
	    exit (1);
	}

    /* activate test tap */
	asp_read( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
	mb.w[RLOC_MLC] &= ~0x1000;	/* set MLC in test mode */
	asp_write( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );

    /* start processing */
	ex_set_selftest(0);             /* get out of selftest */

	asp_read( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
	mb.w[RLOC_CTC] &= 0xfffd;   	/* zero CTC output */
	asp_write( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );

	ex_set_event (SYNC_EVENT,0x1f,trigger,1);
	ex_set_reset(0);
	ctc_set_reset(1);
	mlc_set_reset(1);
	avg_set_reset(1);
	avg_set_reset(0);
	mlc_set_reset(0);
	sleep(1);
	rm_ovf_reset();
	ctc_set_reset(0);
	t();

	printf ("END OF FLUSH.\n");

	if(strcmp(Cur_Rqst->type, "CSD") == 0) {
	    bypass_pipe();
	    printf("CSD mode, all stages bypassed after clear_pipe\n");
	}
}
