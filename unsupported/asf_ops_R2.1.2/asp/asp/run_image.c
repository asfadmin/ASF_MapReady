/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  run_image.c             3/30/95             Ted Robnett    
*/


#include <stdio.h> 
#include <unistd.h> 
#include <sys/types.h>
#include <aspdecl.h>
#include <procfil.h>
#include <procdec.h>
#include <procpar.h>
#include <scene_file.h>
#include <asp_shm.h>

#define  FS        (~0x01)
#define  VL        (~0x02)
#define  VF        (~0x04)
#define  AL        (~0x08)
#define  SS        (~0x10)
#define  SYNC_EVENT 0

#define  LENGTH     132		/* length of response vector         */
#define  STATLEN     50		/* length of the status register     */

extern int cp_flag;
extern RQST_PTR Cur_Rqst;	/* pointer to current job request */
extern DPP_FILE dpf;		/* df processing parameter file */
extern SCENE_FILE sf;		/* scene file */

enum { LOW_RES, FULL_RES, BOTH_IMAGE };
extern int Res_type;

/* ASP configuration parameters */
extern int vbose;		/* 1 = print progress messages */
extern int first_odd;		/* flag for first of odd or even */
extern int first_ident;		/* flag for dcrs identify */
extern int fst_len;
extern int fmt_tobe;
extern float fmt_ratio;
extern int pre_done;

/*SONY*/
extern char media_type[];
extern char data_direction[];

/* run_image (sp,pp,param_file,image_id) -----------------------
	This program performs the required sequence of events to get the
	image through the processor.  It opens communications with Read
	and Write DCRSis, clears the ASP pipe, loads processing
	parameters, etc.
*/

run_image (sp,pp,param_file,image_id) 
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	char *param_file,*image_id;
{

    char    id[STATLEN],log[104],tape_id[7];
    short int rpi_save,ifdr[4100]; 
    unsigned short int *ahg;
    int     read_rcdr, trigger1, trigger2, i, end_addr;
    int     tcount,temp1,temp2;
    int     use_avg;
    int     outlines = 8192;
    int     pixel_size = 1;
    int	    ahgmax, pid_save, cntr, creg;
    int	    blkno, fine_ltrr, nfmts_fine;
    int	    start_id, nfmts, sync_code, ans;
    int	    dropout, should_end;
    int outb_pid, outb_stat;

/* Initialize local variables */
    read_rcdr = IN;
    read_proc_param_file (param_file);
    use_avg = (ml_segsize > 0);

/* Ready recorders for commands */
/* do input only for the first of a series of images */
/* do this in the calling routine's outer loop */

    printf("first_odd (run_image): %d\n",first_odd);fflush(stdout);
    if (first_odd != 0) {
    	printf ("Initializing READ recorder...\n");
    	if (rcdr_start (read_rcdr) == FAIL)
	    return (FAIL);
    }

    if (first_odd != 0) {
	if ( Cur_Rqst->take_id[0] == 'E' )
/*SONY*/
		start_id = pp->fmt_id - (int)((pp->blk_start-in_startblock)*
						TAPE_BLOCK_LEN/(7424.*fmt_ratio));
	else start_id = pp->fmt_start;

    	fmt_tobe = start_id;
    }
    else { /* skipping formats to move to region start point */
	fine_ltrr = 0;
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) ) 
            blkno = sony_get_addr(IN);
        else
            blkno = dc_get_addr(IN);

    	if (blkno == 0)
            printf(" output block 0, who cares! ");
    	if (vbose)
            printf("...now at block %d\n",blkno);

    /* setup and play 8 frames to the beginning of next region */
    	printf("Let the tape run and run ...\n");
	if ( Cur_Rqst->take_id[0] == 'E' )
/*SONY*/
		start_id = pp->fmt_id - (int)((pp->blk_start-in_startblock)*
						TAPE_BLOCK_LEN/(7424.*fmt_ratio));
	else
		start_id = pp->fmt_start;
	if (vbose) printf("next region start id = %d\n",start_id);
	nfmts = start_id - fmt_tobe;
	if (Cur_Rqst->take_id[0] == 'E' || 
	    Cur_Rqst->take_id[0] == 'R'){
		if (nfmts < 40) {
		printf("nfmt = %d, No tape forword\n", nfmts);
		return(FAIL);
		}
		nfmts = 8 * (int)((nfmts + 4)/8.);
	}
	if (nfmts > 12000) {
	    fine_ltrr = 1;
	    nfmts_fine = nfmts - 12000;
	    nfmts = 12000;
	}
	if ( Cur_Rqst->take_id[0] == 'E' ){
		if (e1_ltrr(sp,pp,nfmts) == FAIL) return(FAIL);
	}
	else if ( Cur_Rqst->take_id[0] == 'R' ){
		if (r1_ltrr(sp,pp,nfmts) == FAIL) return(FAIL);
	}
	else if (j1_ltrr(sp,pp,nfmts) == FAIL) return(FAIL);

    /* do fine tune LTRR if necessary */
	if (fine_ltrr == 1) {
    	    printf("Let the tape run and run ... again\n");
	    if ( Cur_Rqst->take_id[0] == 'E' ) {
	    	if (e1_ltrr(sp,pp,nfmts_fine) == FAIL) return(FAIL);
	    }
	    if ( Cur_Rqst->take_id[0] == 'R' ) {
	    	if (r1_ltrr(sp,pp,nfmts_fine) == FAIL) return(FAIL);
	    }
	    else if (j1_ltrr(sp,pp,nfmts_fine) == FAIL) return(FAIL);
	}
    }

/*SONY*/
        if( strcmp( media_type, "DCRSI" ) ) 
            blkno = sony_get_addr(IN);
        else
            blkno = dc_get_addr(IN);

    printf ("at blk after LTRR = %d\n", blkno);

    /* adjust fmt_tobe if necessary */

  if (strcmp( data_direction, "REVERSE" ) ) {
    if (abs(in_startblock-blkno) < 200) {
	start_id -= in_startblock - blkno;
	printf("move blk addr back by %d\n", in_startblock-blkno);
    }
  }

    if ( Cur_Rqst->take_id[0] == 'E' ||
	 Cur_Rqst->take_id[0] == 'R' )
	fmt_tobe = start_id + in_scenelen * in_vlines;
    else 
    	fmt_tobe = 8 * (int)((start_id+in_scenelen*in_vlines+4)/8.);
    if (vbose){
	printf("start_id %d in_scenelen %d in_vlines %d\n",
		start_id, in_scenelen, in_vlines);
	printf("format number after run_image = %d\n",fmt_tobe);
    }

/* Initialize RPI board for reading.
   Note that the RPI board must be armed before the DCRSi is
   started because it is looking for a block sync
*/

    clear_pipe (sp);

/*    if ((sp->aux.beam_seq == 6) || (sp->aux.beam_seq == 7)){
        clear_pipe ();   CV 5/1/97 to fix the white band 
        if (vbose) printf("clear_pipe again\n");
    }
*/
    if (!strcmp(Cur_Rqst->type,"CSD")) pup();
    if(first_odd != 0) reset_rpi (1);

    if ((Cur_Rqst->take_id[0]=='E') ||  /* E-ERS-1 */
	(Cur_Rqst->take_id[0]=='R') ||  /* RADARSAT*/
	(Cur_Rqst->take_id[0]=='S')) {  /* SEASAT  */
      	reset_edfm (1, 1);
      	reset_edfm (0, 0);
    }
    else if (Cur_Rqst->take_id[0]=='J') { /* J-ERS-1 */
      	    reset_jdfm (1, 1);
      	    reset_jdfm (0, 0);
    }
    else {
	printf("VUU TEST 1\n");
	return(FAIL);
    }

/* set up registers */
    read_proc_param_file (param_file);
    if ( Cur_Rqst->take_id[0] == 'J' ){
    	if (first_odd == 2) {
                if (!strcmp( data_direction, "REVERSE" ) )
		  in_startblock = pp->blk_end;
                else
		  in_startblock = pp->blk_start;
		in_blkoff = pp->bit_off / 8;
		in_shift = pp->bit_off % 8;
    	}
    }

    if (p_load_regs () == FAIL) {
	printf ("problem encountered with parameter file\n");
	return (FAIL);
    }

/* setup for the EXEC */
    ex_set_selftest (0);
    ex_set_delay (8192 * 1000);
    trigger1 = SS & FS & VF & AL & VL;	/* scene start */
    trigger2 = FS & VF & AL & VL;	/* valid frame */
    tcount = in_scenelen - ml_frdelay - 1;
    if (tcount <= 1)
	ex_set_event (SYNC_EVENT, 0x1f, trigger1, 1);
    else
	ex_set_event (SYNC_EVENT, 0x0f, trigger2, tcount);
    ex_set_reset (0);

/* activate MLC test tap */
    asp_read( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
    mb.w[RLOC_MLC] &= ~0x1000;
    asp_write( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );

/* reset all the boards */

    play_dis (1);
    iif_set_reset (1);
    ctc_set_reset (1);
    mlc_set_reset (1);
    avg_set_reset (1);

/* reset outboard */

    if ( Res_type != LOW_RES ){
	    if (strcmp(Cur_Rqst->type,"CPX") == 0) {
	    		setup_out_s( 2, 1, 1 );
	    } else if (strcmp(Cur_Rqst->type,"CSD") == 0)
			setup_out_s( 0, 1, 1 );
	    else 
			setup_out_s( 1, 1, 1 );
	
	    out_set_reset(0);
    }

    clear_fft_oflo_regs ("rf");
    clear_fft_oflo_regs ("ri");
    clear_fft_oflo_regs ("af");
    clear_fft_oflo_regs ("ai");

/* check recorder identities */
/* do input only for the very first time play DCRSi ??? */
    if (first_ident == 1) {
      if ( Cur_Rqst->take_id[0] == 'E' ||
	   Cur_Rqst->take_id[0] == 'R' ) first_ident = 0;
      else if (strcmp(Cur_Rqst->type,"STD") == 0) first_ident = 0;

/*SONY*/
      if (!strcmp( media_type, "DCRSI" ) ) {
        if (dc_identify (read_rcdr, id) == FAIL) {
          if (dc_identify (read_rcdr, id) == FAIL) {
	    printf ("dc_identify for READ recorder failed second time\n");
	    return (FAIL);
	  }
	else
	    printf ("dc_identify: FAIL cleared on second try... Proceeding...\n");
        }
      }
      else {
             if (sony_identify(read_rcdr) == FAIL) {
                printf("sony_identify for READ recorder failed\n");
                return(FAIL);
             }
           }

      if ((Cur_Rqst->take_id[0] == 'J') && (pre_done == 1)) {
	printf("Surrender!\n");
	pre_done = 0;
	return(FAIL);
      }
      pre_done = 0;
    }  /* if first ident */

/* Capture full-res images
*/
    if ( Res_type != LOW_RES ){
       	if ( (outb_pid = p_get_full_image( image_id )) == FAIL ){
		printf("Cannot prepare to capture full-res data (%d)\n",
			outb_pid );
		return(FAIL);
    	}
    }

/* release board resets */
/* extra delay after MLC reset is crucial */
    iif_set_reset (0);
    avg_set_reset (0);
    set_avg_wrmode (0x001);
    mlc_set_reset (0);
    sleep(1);
    rm_ovf_reset ();
    ctc_set_reset (0);

    /* if (abob()) exit(1); */
/* Turn off ALLOWMLCSTOP at OUT board if low_res is on, MC 8/8/95 */
    if (use_avg && (Res_type != FULL_RES )) {
	printf("ALLOWMLCSTOP is OFF\n");
    	asp_read( RLOC_OUT<<1, &mb.w[RLOC_OUT], 2 );
    	mb.w[RLOC_OUT] |= 0x10;
    	asp_write( RLOC_OUT<<1, &mb.w[RLOC_OUT], 2 );
    }

/* gentlemen, start your recorders */
/*  
    this code probably needs to be moved or completely modified
    but for now leave it here until the problem is figured out.
    The first pass should start the drives, but after that the
    start address needs to be specified to the iif.
*/
    if (first_odd != 0) {
	/* first_odd = 0; */
/*	reset_rpi(0); */
    	printf ("  Starting input recorder\n");
	/* sleep(10); /* Ming */
    	if(play_record_go (read_rcdr,1,log)== FAIL)
          return (FAIL);
    	printf ("Waiting for recorder status . . .\n");
/*SONY*/
        if (!strcmp( media_type, "DCRSI" ) )
    	  if (check_command (read_rcdr,1) == FAIL)
	     return (FAIL);
	if ( Cur_Rqst->take_id[0] == 'J' ){
	    if (first_odd == 2 || first_ident == 1) {
		first_ident = 0;
		/* j1_Ted_fix(); comment out by CV 4/2/96 */
	    } /* end of first_odd if */
	} /* end of J if */
    }   /* if first in pass odd or even */

/* trigger and wait for process completion */
    printf ("Before tarm\n");
    tarm ();
    if (first_odd != 0) {
	reset_rpi(0);
    	first_odd = 0;
    }
    clear_fft_oflo_regs ("rf");
    clear_fft_oflo_regs ("ri");
    clear_fft_oflo_regs ("af");
    clear_fft_oflo_regs ("ai");
    play_dis (0);		/* Enable play output */

    if (use_avg && (Res_type != FULL_RES )) {
	printf("capturing averaged image...\n");
	printf("pr_avglines = %d, ml_linelen = %d\n", pr_avglines, ml_linelen);
	ans = p_get_avg_image(pr_avglines, (ml_linelen+7)/8, "avg_image");
	if (ans == -1) {
	    printf("failed in get_avg_image\n");
	    return(FAIL);
	}
	printf("...averaged image captured\n");
	play_dis (1);
    }
    else {
	printf ("Before tdone\n");
	tdone ();
	play_dis (1);
    }
/*SONY*/
    if (!strcmp( media_type, "DCRSI" ) )
       blkno = dc_get_addr(IN);
    else
       blkno = sony_get_addr(IN);

    printf ("at blk after run_image = %d\n", blkno);

/* Check status of image capturer
*/
    if ( Res_type != LOW_RES ){
	while ( shm_flags->get_outb == 1 ) usleep(1000);
	outb_stat = shm_flags->get_outb;
	asp_delete_shm();
	if ( outb_stat == FAIL ) return(FAIL);
    }
	
/* check dropout bit */

    if ( Cur_Rqst->take_id[0] == 'E' ){
	asp_read( (RLOC_EDFM+3)<<1, &mb.w[RLOC_EDFM+3], 2 );
	dropout = mb.w[RLOC_EDFM + 3] & 0x1;
        if (dropout == 1) {
    	   printf("dropout detected in RUN_IMAGE\n");
	   /* if (abob()) exit(1); */
    	   return(FAIL);
        }
    }
    else if ( Cur_Rqst->take_id[0] == 'J' ){
	asp_read( (RLOC_JDFM+7)<<1, &mb.w[RLOC_JDFM+7], 2 );
	dropout = (mb.w[RLOC_JDFM+7]>>14) & 0x1;
        if (dropout == 0) {
    	   printf("dropout detected in RUN_IMAGE\n");
	   /* if (abob()) exit(1); */
    	   return(FAIL);
        }
    }
    else if ( Cur_Rqst->take_id[0] == 'R' ){
	asp_read( (RLOC_RDFM+14)<<1, &mb.w[RLOC_RDFM+14], 2 );
	dropout = mb.w[RLOC_RDFM+14] & 0x100;
        if (dropout == 0) {
    	   printf("dropout detected in RUN_IMAGE\n");
	   /* if (abob()) exit(1); */
    	   return(FAIL);
        }
    }
    else
	return(FAIL);

/* save bit error rate and input histogram */

    if ( Cur_Rqst->take_id[0] == 'E' )
	ans = e1_get_hwdata(sp,pp,1);
    else if (  Cur_Rqst->take_id[0] == 'J' )
	ans = j1_get_hwdata(sp,pp,1);
    else if (  Cur_Rqst->take_id[0] == 'R' )
	ans = r1_get_hwdata(sp,pp,1);
    if (ans == FAIL) {
        printf("failed in get_hwdata\n");
        return(FAIL);
    }

/* pull histogram out from MLM, 7/18/91, MC */
    pid_save = mb.w[RLOC_REP];
    mb.w[RLOC_REP] = PID_MLC;

    asp_read( (RLOC_MLC+6)<<1, &mb.w[RLOC_MLC + 6], 2 );
    creg = mb.w[RLOC_MLC + 6];
    creg |= 0x0010;
    mb.w[RLOC_MLC + 6] = creg;
    asp_write( (RLOC_MLC+6)<<1, &mb.w[RLOC_MLC + 6], 2 );

    ahg = &mb.w[MLOC_MLC_HIST >> 1];
    ahgmax = 256;
    asp_read( MEM_MLC_HIST, ahg, ahgmax*4 );	/*  Modified by EYC 6/24/96 */

    for (cntr = 0; cntr < ahgmax; cntr++) {
	temp1 = *ahg++;
	temp2 = *ahg++;
	sf.i_hstgrm[cntr] = (temp1 <<16) + (temp2 &0xffff);
/*      printf("cntr = %d, histogram = %d\n", cntr, sf.i_hstgrm[cntr]); */
    }
    creg &= 0xffef;
    mb.w[RLOC_MLC + 6] = creg;
    asp_write( (RLOC_MLC+6)<<1, &mb.w[RLOC_MLC + 6], 2 );
    mb.w[RLOC_REP] = pid_save;

/* do end-of-image things and stop recorders */
/* do this only if the last image in a series and move this
   code to proc.c at the end of the loop for "odd" */
/* need to re-think how quick look lo-res images are saved to
   the DCRSi tapes. */
/*
    dc_stop (read_rcdr);
    dc_comm_close (read_rcdr);
*/
    sleep(3); /* Ming's Test, 2-25-94 */
    return (PASS);
}


/* rcdr_start (rcdr) ---------------------------------------------------
	Routine initalizes communications with the recorder.
*/

rcdr_start (rcdr)
    int rcdr;
{
    char resp[LENGTH];

/*SONY*/
    if (!strcmp( media_type, "DCRSI" ) ) {
      if (dc_comm_init (rcdr) == FAIL) {	 /* Initialize communications */
         printf ("dc_comm_init failed to execute correctly\n");
	 if (dc_reset (rcdr, resp) == FAIL) { /* Try to reset recorder */
	    printf ("dc_reset failed to execute correctly\n");
	    printf ("check power on the recorder\n");
	    return (FAIL);
	 }
      }
    }
    else {
           if (sony_comm_init (rcdr) == FAIL) {
              printf ("sony_comm_init failed to execute correctly\n");
              return(FAIL);
           }
           if (sony_init (rcdr) == FAIL) {
              printf ("sony_init failed to execute correctly\n");
              return(FAIL);
           }
         }

    return (PASS);
}


/* play_record_go (rcdr, play_record, log) -----------------------------
	This routine configures DCRSi recorder (rcdr) to start reading
	(play_record = 1), or writing (play_record = 0).
*/

int play_record_go (rcdr, play_record, log)
    int    rcdr, play_record;
    char  *log;
{
    char    rcvd[LENGTH];
    char    cmd[LENGTH];
    char    addr[LENGTH];
    int     i = 0;
    int     startad, nblocks;
    int     blank = 7000;	/* If reading, the tape begins at block
				   - 1, but if writing, the tape
				   is advanced 7000 blocks */


/* Assign enough play/record space */
    if (play_record) {
	startad = in_startblock - 1;
/*SONY*/
        if (!strcmp( media_type, "DCRSI" ) )
	  nblocks = 2000000;
        else {
          startad += 1;
          nblocks = 2000000/33;
        }
    }
    else {
	startad = ml_startblock - blank;
/*SONY*/
        if (!strcmp( media_type, "DCRSI" ) )
	  nblocks = 60000;
        else {
          startad += 1;
          nblocks = 60000/33;
        }
    }

/* Get the recorder ready to record or play */
/*SONY*/
   if (!strcmp( media_type, "DCRSI" ) ) {
    dc_send_cmd(rcdr,"EE");
    if (dc_get_prompt(rcdr,2,rcvd,NULL) != FAIL)
        if (vbose) printf ("message from EE\n");

    dc_send_cmd(rcdr,"PD");
    if (dc_get_prompt(rcdr,2,rcvd,NULL) != FAIL)
        if (vbose) printf ("message from PD\n");

/* comment out by CV 3/26/96: RE: only uses if write out to tape 
    dc_send_cmd(rcdr,"RE");
    if (dc_get_prompt(rcdr,2,rcvd,NULL) != FAIL)
        if (vbose) printf ("message from RE\n");
*/
    dc_clr_stat(rcdr);          /* Clear status */
    if (play_record)		/* Generate command */
	sprintf(cmd,"PL a%d,%d",startad,nblocks);
    else {
	dc_set_log(rcdr,log);
	sprintf(cmd,"RC a%d,%d",startad,nblocks);
    }

    dc_send_cmd(rcdr,cmd);
    printf ("DCRSi finding starting address(%d) . . .\n\n", startad);
   }
   else
       {
        printf ("SONY finding starting address(%d) . . .\n\n", startad);

        if (!strcmp( data_direction, "REVERSE" ) ){
         if (sony_playback_REV(rcdr, startad, startad-nblocks) == FAIL) {
            printf("SONY fail to playback_REV\n");
            return(FAIL);
         }
        }
        else {
         if (sony_playback(rcdr, startad, startad+nblocks) == FAIL) {
            printf("SONY fail to playback\n");
            return(FAIL);
         }
        }  

       }

}



/* check_command ( rcdr,play_record) -----------------------------------
	This routine checks whether the recorder (rcdr) has executed
	the command to go to the play (play_record = 1) or record
	(play_record = 0) address.
*/

check_command (rcdr, play_record)
    int rcdr, play_record;
{
    int j;
    char code[8];
    char rcvd[LENGTH];

/*  Wait until the DCRSi has found the address.  Do this by checking
    for a status code C022 (play) or C033 (record), timeout after 400
    seconds */

    if (play_record)
	strcpy (code, "DS C022");
    else
	strcpy (code, "DS C033");
    if (dc_get_resp(rcdr,2,rcvd) == 290){
	printf("FAIL in check_command\n");
	return (FAIL);
    }
    j = 0;
    while ((strspn (code, rcvd) != 7) && (j < 80)) {
         if (dc_get_resp(rcdr,5,rcvd) == 290) {
		printf("Fail1 in check_command\n");
	 	return (FAIL);
	 }
	 j++;
    }
    if (strspn (code, rcvd) != 7) {
	 printf("Recorder timed out . . . %d\n", play_record);
	 dc_stop (rcdr);
	 return(FAIL);
    }
    else
	 printf("Got it . . . %d\n", play_record);
    if (op_strstr(rcvd,"E290") != NULL)
	return(FAIL);  
    return(PASS);
}
