/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* j1_scan.c -- scan the data take, locating preambles, postambles,
		preprocessing regions and recording gaps.
*/

#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include <stdlib.h>
#include <aspdecl.h>
#include <procfil.h>
#include <procdec.h>

#define  FS         (~0x01)
#define  VL         (~0x02)
#define  VF         (~0x04)
#define  AL         (~0x08)
#define  SS         (~0x10)
#define  SYNC_EVENT 0
#define  COUNT_EVENT 1
#define  HK_LEN 23		/* No. of bytes per HK data set */
#define  ID_MINIMUM  10000	/* minimum # of formats to allow tape
				   id test to be performed */
#define VALID_PIXELS 6144	/* valid pixels/line after rg cmp, who say 4608 ??? */
#define BYTES_FORMAT 6144	/* bytes per format ??? */
/* system configuration parameters */
extern int vbose;		/* 1 = print debugging messages */
extern int sw_noise_meas;	/* 1 = perform noise measurement */
extern int sw_rep_meas;		/* 1 = perform replica measurement */
extern RQST_PTR Cur_Rqst;	/* pointer to current job request */
extern SV sv1,sv2;		/* current request's statevectors */
extern TC_FILE tcf;		/* current request's time cor. elem. */
extern TAPE_SEG_PTR seg_list;	/* tape segment list */
extern SP_FILE spf;		/* sensor parameters file */
extern EM_FILE emf;		/* Earth model file */
extern DPP_FILE dpf;		/* df processing parameter file */
extern int ptype_sw;		/* 1 = using new pre/postamble codes */
				/* 0 = using old pre/postamble codes */
extern char host[16];

static int end_block;		/* ending block # of take */
float prf;			/* pulse rep freq */
int fmt_len;			/* avg fmt length(bit) for its prf */
int fst_len;			/* 1st fmt length after srch-sync */
int fst_len24;			/* 1st fmt length after srch-sync minus 24 */
extern int post_fud;		/* before postamble found */
static int tape_ok = 0;		/* 1 = tape id checks out OK */
int stc_old, agc_old, lgfmt_old, slip_old;
GMT last_end;
int good_becnt = 0;
extern int do_scan;
 
/*SONY*/
extern media_type[];
extern char data_direction[];

extern int Write_CP_Error_Status;

/* j1_scan_take() ------------------------------------------------------
	This routine scans an JERS-1 formatted data take, breaking it up
	into segments bounded by preambles, postambles and recording
	gaps.  Within each segment preprocessing regions are located
	and verified.  The tape segment and preprocessing region data
	table is created, containing information found during this
	process.  This table will guide the operation of preprocessing
	pass two.
*/

j1_scan_take()
{
	TAPE_SEG_PTR sp,sp2;
	int start_block, cur_block, block_skip;
	int block,offset,norm_inv,format;
	int nblks,segment,ans,sar_fmt,try_pre;
	int out_of_sync = 1;
	int gap_type = 0;
	int seg_fmt, short_segs, NOPREAM;
	double time_diff;
	double get_gmt_diff();

/* DCRSi or SONY ? */

        int J1_MIN_TAKE_LEN, BYTES_BLOCK, BLOCKSKIP;

        BYTES_BLOCK = TAPE_BLOCK_LEN;  /* 144304 for SONY, 4356 for DCRSi*/
        if( strcmp( media_type, "DCRSI" ) ) {
           J1_MIN_TAKE_LEN = 150000/33;
           BLOCKSKIP = 10000/33;
           block_skip = dpf.blkskip/33;
        }
        else {
                J1_MIN_TAKE_LEN = 150000;
                BLOCKSKIP = 10000;
                block_skip = dpf.blkskip;
             }

    /* clear any previous tape segment and preproc. block data */
	seg_fmt = 26000;
    	format = ID_MINIMUM + 1;
	short_segs = 0;
	NOPREAM = 0;
	segment = 0;
	p_clear_seg_list();
	tape_ok = 0;
    /* initialize loop variables */
	if (Cur_Rqst->start_blk<50) Cur_Rqst->start_blk=50; /* Ming 3-8-95 */
	start_block = Cur_Rqst->start_blk;
	end_block = Cur_Rqst->end_blk;
	cur_block = start_block;
	printf("processing take from %d to %d\n",start_block,
		end_block);

    /* outer loop: tape segment loop */
	fflush(stdout);
	while (cur_block < end_block - J1_MIN_TAKE_LEN) {
	/* attempt to sync up on the current location */
	/* NOTE: if still in sync, the variables format, cur_block,
		 offset and norm_inv contain values loaded during the
		 previous iteration of this loop */
	    start_block = cur_block; 
	    lgfmt_old = 0;
	    slip_old = 0;

	    while (out_of_sync && cur_block < end_block - J1_MIN_TAKE_LEN) {
/* SONY */
                if (!strcmp( media_type, "DCRSI" ) )
                  nblks = (gap_type == 2) ? 20 : 10;
                else
                     nblks = 3;

		if (seg_fmt < ID_MINIMUM && ++short_segs > 2) {
		   cur_block += BLOCKSKIP;
		   short_segs = 0;
		}
		if (j1_srch_sync(3,2,nblks,&cur_block,&offset,
			&norm_inv,&format,&prf,&fst_len) == PASS) {
		    printf("found sync at block = %d, prf = %g\n",
				cur_block,prf);
		    /* check if fmt of next segment is < fmt end of previous CV 2/24/98 */
		    if (segment != 0) { 
			if (format < sp->fmt_end) {
			    out_of_sync = 1;
			    printf("last fmt_end = %d, new format=%d\n",
				sp->fmt_end, format);
			    return(PASS);
			}
		    }
		    out_of_sync = 0;
		    block_skip = BLOCKSKIP;
		    if ((int)prf == 1505) fmt_len = 39844;
		    if ((int)prf == 1530) fmt_len = 39212;
		    if ((int)prf == 1555) fmt_len = 38580;
		    if ((int)prf == 1581) fmt_len = 37948;
		    if ((int)prf == 1606) fmt_len = 37358;
		    if ((int)prf == 1646) fmt_len = 37648; /* for seasat */
		    fst_len = fmt_len + 1; /* Ming TEST ??? */
		    fst_len24 = fst_len - 24;
		    printf("fst_len = %d, fmt_len = %d\n", fst_len, fmt_len);
		}
		else {
                    if (Write_CP_Error_Status == -2)
                     return(FAIL);

		    start_block = cur_block;
		    cur_block += block_skip;
		    printf("sync not found, cur_block=%d, blkskip=%d\n",
		       cur_block,block_skip);
		}
		fflush(stdout);
	    }  /* while out_of_sync */

	/* if out of sync, the take is done */
	    if (out_of_sync)
		break;

	if (NOPREAM == 0 && seg_fmt > 10000) {
	/* create a new tape segment block if last segment was not
	   a short one (<10000 formats) or if a no-preamble 
	   situation has occured. */ 
	    if (vbose)
		printf("creating new tape segment block\n");
	    if ((sp = (TAPE_SEG_PTR) malloc(sizeof(TAPE_SEG))) == NULL) {
		printf("Out of memory in preproc pass 1\n");
		return (ABORT);
	    }
	    segment++;
	    if (seg_list == NULL) {
		seg_list = sp;
		sp->prenum = 0;
		sp->postnum = 0;
		sp->pre = NULL;
		sp->post = NULL;
	    }
	    else {
		sp2->nxt_seg = sp;
		*sp = *sp2; /* put in some init value every new sp */
	    }
    	} /* end if NOPREAM == 0  */

	    sp->ppb_list = NULL;
	    sp->nxt_seg = NULL;
	    sp->fmt_start = format;
	    sp->fmt_end = format;
	    sp->fmt_id = 0;
	    sp->end_id = 0;
	    sp->blk_start = cur_block;
	    sp->bit_off = offset;
	    sp->polarity = norm_inv;
	    sp->gap_type = gap_type;
	    sp->win_count = 0;
	    sp->agc_count = 0;
	    block = cur_block;

	/* process the preamble, if any */
	    try_pre = 0;
	    while (try_pre < 6) {
	    	sar_fmt = -1;
	    	ans = j1_proc_preamble(sp,block,offset,&sar_fmt);
		if (ans == PASS) break;
		printf("Fail number %d in proc_preamble\n", ++try_pre);
                if ((try_pre == 6) && (ans == FAIL)){
                   if (gap_type == 2) {
                      printf("Cannot locate preamble again; using previous one \n");
                      sp->pre = sp2->pre;
/*                    printf("Some good segment at beginning, create them\n");
                      return(PASS);
*/
                   } else {
                      if (good_becnt)
                        printf("TRY CHANGE START_TIME in JOB FILE & RE-RUN\n");
                      else
                        printf("DATA too noisy or so many small fragment\n");
                     return(FAIL);
                   }
                }
                if (try_pre < 3)
		   j1_get_fmt_loc(1,sp,sp->fmt_start+10000,&fst_len,&block,&offset);
                else {
                   if (try_pre == 3){
		     j1_get_fmt_loc(1,sp,sp->fmt_start+30000,&fst_len,&block,&offset);
                      sp->fmt_start += 20000;
                   }
                   else
		     j1_get_fmt_loc(1,sp,sp->fmt_start+10000,&fst_len,&block,&offset);
                }
		sp->fmt_start += 10000;
		sp->blk_start = block;
		sp->bit_off = offset;
		fflush(stdout);
	    }
	/* check if time go backword */
	    if (segment != 1 && ans != FAIL) {
	    	time_diff = get_gmt_diff(&sp->start,&last_end);
	    	if (time_diff < 0.0) {
		  printf("WARNING:segment starting time is bad\n");
		  printf("%d:%d:%d:%d:%f\n", sp->start.yr,sp->start.day,
		  sp->start.hr,sp->start.min,sp->start.second);
                  sar_fmt = -1;         /* CV 12/4/97 */
                  out_of_sync = 1;      /* CV 12/4/97 */
		  if (!strcmp( media_type, "DCRSI" ))
                     block += 10000;       /* CV 12/4/97 */
		  else
                     block += 10000/33;       /* CV 12/4/97 */
                             
		/*  return(FAIL); */
		}
	    }
/*	    if (j1_proc_preamble(sp,block,offset,&sar_fmt) == FAIL) { */
	    if (ans == FAIL) {
		if (gap_type == 2) {
		    printf("\nCannot locate preamble again; using previous one\n");
		    sp->pre = sp2->pre;
		}
		else {
		    sar_fmt = -1;
		    if (j1_fake_preamble(sp) == FAIL) {
			printf("fatal error occured in preamble!\n");
			return(FAIL);
		    }
		    printf("No preamble ..., fake preamble!\n");
		}
	    }
	    if (sar_fmt != -1) {
                if (j1_get_fmt_loc(1,sp,sar_fmt,&fst_len,&block,&offset) == FAIL) {
                    block = sp->blk_start;
                    printf("set block = %d\n", sp->blk_start);
                }
/* sp starting block don't have to equal OBS start
		sp->blk_start = block;
		sp->bit_off = offset;
*/
	    }
	    cur_block = block;

	  while (out_of_sync == 0) {
	/* scan the take, locating a sync loss, postamble, or the
	   end of the take */
	    p_rw_pream_file(1,"preambles");
	    if (j1_scan_segment(sp,&cur_block,&offset) == ABORT)
		return (FAIL);
	    if (vbose)
		printf("...scan done, cur_block = %d\n",cur_block);
	    gap_type = sp->gap_type;
	    out_of_sync = (gap_type == 2);
	    seg_fmt = sp->fmt_end - sp->fmt_start;
	    get_fmt_time(sp,sp->fmt_start,&sp->start);
	    if (vbose)
	      printf("end of scan_take loop, cur_block=%d\n",cur_block);
	    sp2 = sp;
	    if (post_fud == 1) {
		cur_block += BLOCKSKIP;
		out_of_sync = 1;
		if (vbose)
	          printf("start of next segment scan, block=%d\n", cur_block);
	    }
	    fflush(stdout);
	   } /* while out_of_sync == 0 */
	   fflush(stdout);
	}  /* while cur_block */
	return (PASS);
}

/* j1_fake_preamble(sp) ------------------------------------------
	This routine attempts to fake a preamble at the format
	location given in the tape segment sp. 
*/

j1_fake_preamble(sp)
	TAPE_SEG_PTR sp;
{
	PREAM_FILE_PTR pre;
	double time_diff;
	double get_gmt_diff();

	if (vbose){
	    printf("faking preamble data block\n");
	    printf("block = %d, offset = %d, fmt_start = %d\n",
      			sp->blk_start, sp->bit_off, sp->fmt_start);
	}

/* do not fake preamble, just return FAIL */
	if (1 == 1) return(FAIL);

	p_create_preamble(&pre);
	sp->pre = pre;
	sp->prenum = pre->prenum;
      /* save preamble data */
	strcpy(pre->sat,"J1");
	strcpy(pre->dcrs_id,Cur_Rqst->tape_id);
	strcpy(pre->takeid,Cur_Rqst->take_id);
	strcpy(pre->sen_mode, "OGRC");
	pre->prf = prf;
	pre->fmt_start = sp->fmt_start;
	pre->rec_gain = 0;
	pre->cal_att = 0;
	pre->blk_start = sp->blk_start;
	pre->bit_off = sp->bit_off;
	pre->polarity = sp->polarity;
	pre->repp_dton = 0.0;

	/* without calling E. Chu's program */
	sp->start = sv1.gmt;
	sp->pre->time_ref_fmt = sp->fmt_start;
	sp->pre->time_ref_gmt = sp->start;

    /* compare start time of seg with state vector time */
	time_diff = get_gmt_diff(&sp->start,&sv1.gmt);
	if (time_diff < 0.0) {
	   printf("start time less than state vector time\n");
	   return (FAIL);
	   }
	if (time_diff > 6060.0) {
	   printf("start time more than 101 min+state vector time\n");
	   return (FAIL);
	   }
	return (PASS);
}


/* j1_proc_preamble(sp,iblock,ioffset,sar_fmt) ---------------------------
	This routine attempts to process a preamble at the format
	location given in the tape segment sp.  If that location is
	the start of a preamble, the preamble processing is performed,
	a preamble record is created, and the routine returns PASS.
	If not, the format number of the first format is loaded into
	sp, and the routine returns FAIL.
*/

j1_proc_preamble(sp,iblock,ioffset,sar_fmt)
	TAPE_SEG_PTR sp;
	int iblock,ioffset,*sar_fmt;
{
/*#define NPFMTS 1000  TEST Ming input 64 by 1000 */
#define NPFMTS 512 /* TEST Ming input 64 by 1000 */
	PREAM_FILE_PTR pre;
	HK_DATA hk;
	int sync_code;
	int block,offset;
	int doflg,biterr,frmnum,fmtnum;
	int ref_count,start_id;
	GMT ref_time;
	double time_diff;
	double get_gmt_diff();
	char hk_buf,pcm_buf,hk_file[32],pcm_file[32];
	int becnt, lgformat;

/* SONY */

        int  BYTES_BLOCK;

        BYTES_BLOCK = TAPE_BLOCK_LEN;  /* 144304 for SONY, 4356 for DCRSi*/

	printf("Attempting to locate preamble\n");
    /* set up processor to capture the preamble and chirp */
	reset_rpi(1);
	reset_jdfm(1,1);
	reset_jdfm(0,0);

	setup_jdfm(0x1110,2,0,0,0,0,dpf.fsm,dpf.maxfsdo,fst_len24);
	setup_iif_cntrl(0,0,1,3,0x1100,1,0);
	setup_iif_par(VALID_PIXELS,NPFMTS,64,64*NPFMTS,0,0,0,1,13,0);
	ex_set_selftest(0);
	ex_set_delay(8192 * 64 * NPFMTS);
	sync_code = FS & VF & AL & VL;
	ex_set_event(SYNC_EVENT,0x0f,sync_code,64);
	ex_set_reset(0);
	play_dis(1);
	iif_set_reset(1);
	iif_set_reset(0);
	block = iblock;
	offset = ioffset;
    /* start the DCRS at the right location */
	if (offset < 32) {
	    block -= 1;
	    offset += BYTES_BLOCK * 8;
	}
/*SONY*/
        if (!strcmp( media_type, "DCRSI" ) ) {
	   if (vbose)
	     printf("starting DCRSi play at %d, offset %d, polarity %d\n",
		block,offset,sp->polarity);
	   start_fmt_play(1,block,offset-32,sp->polarity);
        }
        else {
	       if (vbose)
	         printf("starting SONY play at %d, offset %d, polarity %d\n",
		     block,offset,sp->polarity);
	       sony_start_fmt_play(1,block,offset-184,sp->polarity);
             }
	
/*	if (abob()) exit(1); */

    /* arm and trigger, also save the averger memory */
	j1_clear_hdr_mem(0);
	tarm();
	sleep(3);
/*	j1_Ted_fix();  comment out on 3/25/96 due to hang */
	reset_rpi(0);
	play_dis(0);
/*	p_get_avg_image(128, 128, NULL); comment out for test ??? */
	tjdone();
	
/* if (abob()) exit(1); */

    /* at this point, the data should be captured */
    /* see if we're still in sync */
	asp_read( (RLOC_JDFM+7)<<1, &mb.w[RLOC_JDFM+7], 2 );
	doflg = (mb.w[RLOC_JDFM + 7] >> 14) & 0x1;
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
           sony_stop(IN);
        else
           dc_stop(IN);

        asp_read( (RLOC_JDFM+3)<<1, &mb.w[RLOC_JDFM+3], 2 );
        becnt = mb.w[RLOC_JDFM+3];
        asp_read( (RLOC_JDFM+4)<<1, &mb.w[RLOC_JDFM+4], 2 );
        lgformat = mb.w[RLOC_JDFM+4];
        if (vbose) printf("becnt(hex)=%x, lgformat(hex)=%x\n", becnt, lgformat);

	if (doflg == 0) {
	/* not to try a second time */
	    printf("Drop out in proc_preamble! Do not trust PCM\n");
/*	    return(FAIL); */
	}

    /* check for good header data */
	printf("After tjdone\n");
	if (j1_get_hk_data(sp,1,&hk) == FAIL) {
	    printf("FAIL get HK\n");
	    return(FAIL);
	}
        printf("First format found at %d\n", hk.format);
        /* CV 12/4/97 check if format of next preamble is < format of
        previous preamble */
        if (hk.format < sp->fmt_start) {
           sp->fmt_start = hk.format;
           if (vbose) printf("fmt_start=%d\n", sp->fmt_start);
        }
	printf("First format found at %d\n", hk.format);
    /* process replica captured in response buffer if detect CAL mode */
	if (j1_get_sar_fmt(sp,sar_fmt) == FAIL)
	    printf("no sar_fmt found, sar_fmt = %d\n", *sar_fmt);
	else { 
	    printf("perform j1_repmeas\n");
	    j1_repmeas(sp,block,offset);
	}

    /* call E. Chu's header decoder and calibration routines  */
	ref_count = tcf.bt;
	ref_time  = tcf.gmt;
	strcpy( hk_file, "HK.TST" );
	strcpy( pcm_file, "PCM.TST" );
	j1_hdr(ref_count,&ref_time,hk_file,pcm_file,&start_id);
/*
	j1_cal(rep_file,cal_agc,cal_att,abs,hk_file);
*/
/*	if (abob()) exit();*/
    /* compare start time of seg with state vector time */
/* ??? should we get sp->start from EChus j1_hdr */
	sp->start = ref_time;
        printf("fmt_start=%d, prf=%g\n",sp->fmt_start, prf);
	add_seconds(&ref_time, -1.*(sp->fmt_start/prf));
        printf("ref_time=%d:%d:%d:%d:%f\n",
	ref_time.yr,ref_time.day,ref_time.hr,ref_time.min,ref_time.second);
	printf("%g second\n", sp->fmt_start/prf);
	time_diff = get_gmt_diff(&ref_time,&Cur_Rqst->start);
	printf("diff of ref time and job start time: %g second\n", time_diff);
	if (fabs(time_diff) > 20.0) {
	   printf("*** QC ***: decoded BAD start time!\n");
           if (becnt == 0) {
               good_becnt = 1;
               printf("TIME IN JOB FILE SHOULD BE %d:%d:%d:%d:%f\n",
		ref_time.yr,ref_time.day,ref_time.hr,ref_time.min,ref_time.second);
           } else
               good_becnt = 0;
	   return (FAIL);
	}
	time_diff = get_gmt_diff(&sp->start,&sv1.gmt);
	if (time_diff < 0.0) {
	   printf("start time less than state vector time\n");
	   return (FAIL);
	   }
	if (time_diff > 6060.0) {
	   printf("start time more than 101 min+state vector time\n");
	   return (FAIL);
	   }

	p_create_preamble(&pre);
	sp->pre = pre;
	sp->prenum = pre->prenum;
      /* save preamble data */
	strcpy(pre->sat,"J1");
	strcpy(pre->dcrs_id,Cur_Rqst->tape_id);
	strcpy(pre->takeid,Cur_Rqst->take_id);
	strcpy(pre->sen_mode, "OGRC");
	pre->prf = prf;
	pre->fmt_start = sp->fmt_start;
	pre->rec_gain = 0;
	pre->cal_att = 0;
	pre->blk_start = sp->blk_start;
	pre->bit_off = sp->bit_off;
	pre->polarity = sp->polarity;
	pre->repp_dton = 0.0;
	pre->time_ref_fmt = hk.format; /* To be fixed ??? */
	pre->time_ref_gmt = sp->start;

	return (PASS);
}



/* j1_repmeas(sp,block,offset) ---------------------------------------------------
	This routine measures the characteristics of 8 replicas.
*/

j1_repmeas(sp,block,offset)
	TAPE_SEG_PTR sp;
	int block,offset;
{
#undef NPFMTS
#define NPFMTS 300
	short int tf[2048][2],iif_tbl[32768];
	unsigned char replica[2048];
	char filename[80];
	int sync_code,doflg,r,dcbias,n3sig,reperror;
	int len = 594;
	float spikes[2048],peakoffset,iqrephase;
	float gain[2],bias[2];
	float peak,x3db,pslr,islr;
	int istat = 0;
	int file_mj;
	
	printf("  measuring replica characteristics\n");
	if (sw_rep_meas == 0) {
	    printf("......bypassed\n");
	    iqrephase = 90.0;
	    return;
	}
    /* get the range transfer function */
	p_get_mem_file(tf,4096,DEF_PATH,"rf_ra_file","");

	printf("Start playing DCRS for j1_repmeas\n");
    /* set up processor to capture the preamble and chirp */
	reset_rpi(1);
	reset_jdfm(1,1);
	reset_jdfm(0,0);

/* For Mings test only: setup IIF table for reading CAL mode data */
/*
	strcat(strcpy(filename,DEF_PATH),"params.cal");
	if (read_proc_param_file(filename) == FAIL) {
	    printf("...unable to read params.cal for preamble\n");
	    return (FAIL);
	}
	if (p_load_regs() == FAIL) {
	    printf("...register load failed\n");
	    return (FAIL);
	}
*/
	if (sp->polarity == 1)
	    strcat(strcpy(filename,DEF_PATH),"input_trans.n");
	else
	    strcat(strcpy(filename,DEF_PATH),"input_trans.i");
	if (p_get_mem_file (iif_tbl, 8192, filename, "") == FAIL)
	    return (FAIL);
	setup_iif_memory (iif_tbl);
	setup_jdfm(0x1111,2,0,1,1,0,dpf.fsm,dpf.maxfsdo,fst_len24);
	setup_iif_cntrl(0,0,1,3,0x1100,1,0);
	setup_iif_par(VALID_PIXELS,NPFMTS,1,NPFMTS,0,0,0,1,13,0);
	ex_set_selftest(0);
	ex_set_delay(8192 * 2 * NPFMTS);
	sync_code = SS & FS & VF & AL & VL;
	ex_set_event(SYNC_EVENT,0x1f,sync_code,1);
	ex_set_reset(0);
	play_dis(1);
	iif_set_reset(1);
	iif_set_reset(0);
    /* start the DCRS at the right location */
/*SONY*/
        if (!strcmp( media_type, "DCRSI" ) ){
	  if (vbose)
	    printf("starting DCRSi play at %d, offset %d, polarity %d\n",
		block,offset,sp->polarity);
	  start_fmt_play(1,block,offset-32,sp->polarity);
        }
        else {
	       if (vbose)
	          printf("starting SONY play at %d, offset %d, polarity %d\n",
		      block,offset,sp->polarity);
	       sony_start_fmt_play(1,block,offset-184,sp->polarity);
             }
	
/*	if (abob()) exit(1); */

    /* arm and trigger */
	tarm();
	sleep(3);
/*	j1_Ted_fix();  comment out on 3/25/96 due to hang */
	reset_rpi(0);
	play_dis(0);
	tjdone();
	
    /* at this point, the data should be captured */
    /* see if we're still in sync */
	asp_read( (RLOC_JDFM+7)<<1, &mb.w[RLOC_JDFM+7], 2 );
	doflg = (mb.w[RLOC_JDFM + 7] >> 14) & 0x1;
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
           sony_stop(IN);
        else
           dc_stop(IN);

	if (doflg == 0) {
	/* not to try a second time */
	    printf("Drop out in j1_repmeas!\n");
	    return(FAIL);
	}

    /* read and process 8 replicas, 594 points each ??? */
	for (r = 1; r < 9; r++) {
	    j1_get_replica(0,r,594,1,replica);

	    if (vbose && r==1) {
		if ((file_mj = creat("replica_mj", 0666)) == FAIL) {
			printf (" \n Can't open %s", "replica_mj");
			return(FAIL);
		}
		if (write(file_mj, replica, 1188) != 1188) {
		    printf( "Error in writing replica file" );
		    return(FAIL);
		}
		close(file_mj);
		printf("Finish writing replica_mj\n");
	    }

	    /* NOTE: the following is a FORTRAN subroutine call */
	    dcbias = 28;  /* J-ERS-1 specific offset */
	    repmeas_(replica,tf,&len,gain,bias,&dcbias,&iqrephase,
		spikes,&n3sig,&peak,&peakoffset,&x3db,&pslr,&islr,&istat);
	    if (vbose)
		printf("...peak power = %g\n",peak);
	    if (peakoffset > spf.xlocation)
		reperror |= 1;
	    if (x3db > spf.x3db)
		reperror |= 2;
	    if (pslr > spf.xpslr)
		reperror |= 4;
	    if (islr < spf.xislr)
		reperror |= 8;
	/*
	    noise(replica,len,&power,&cstat);
	    tpower += power;
	*/
	}
}


/* j1_get_replica(x,y,nx,ny,Data) ------------------------------------
	This routine retrieves one replica line from the response
	buffer, takes 8 MSBits from both I and Q (for repmeas.f),
	and loads it into the byte array Data.
	For JERS1, range line length after IIF is 8192.
*/

j1_get_replica(x,y,nx,ny,Data)
	int x,y,nx,ny;
	unsigned char *Data;
{
	int i, rgo, linelen;
	int maddr = 0x10000;
	register unsigned short int *m, *mend;
	unsigned char *d;

	d = Data;
	linelen = 4 * nx;
	for (i = y; i < y + ny; i++) {
	    rgo = i * 8192 * 4 + x * 4;
	    mr (rgo, rgo + linelen, maddr, maddr + linelen);
	    m = &mb.w[maddr >> 1];
	    mend = m + nx * 2;
	    while (m < mend) {
/*		*d++ = ((*m++) >> 8) & 0xff; */
		*d++ = ((*m++) >> 10) & 0x3f;
	    }
	    maddr += linelen;
	}
}


/* j1_scan_segment(sp,block,offset) -----------------------------
	This routine scans the given tape segment from start to end,
	looking for:  1) loss of sync;  2) a postamble.  Either one
	signals end of segment, and the location and reason are
	returned.  If a postamble is found, it is processed.
	    Also, the routine checks for data window position changes,
	and records any found in the global table dwp.
*/

j1_scan_segment(sp,block,offset)
	TAPE_SEG_PTR sp;
	int *block,*offset;
{
#define NFMTS 20000
	HK_DATA hk;
	short int fmtsav[104 * 10];
	int not_at_end,gap_type,sync_code;
	int i,j,k,pid,head_start,ans,drivesel,rev;
	int doflg,biterr,frmnum,fmtnum,nfmts,nframes,tfmts;
	int first = 1;
	int fmt_count,bit_slip,scan_bit,lgfmt;
	int first_format,last_format,sar_end;

	char q[80];

/*SONY */

        int BYTES_BLOCK;

        BYTES_BLOCK = TAPE_BLOCK_LEN;  /* 144304 for SONY, 4356 for DCRSi*/
    /* set up processor to scan the segment */
/*
drivesel=0;
rev=0;
      setup_rpi(format,mode,offset,shift,norm_inv,test,drivesel, rev)    */
/*      setup_rpi(1,0,9,0,1,1,drivesel, rev);   */
	reset_rpi(1);
	reset_jdfm(1,1);
  	reset_jdfm(0,0);  

	setup_jdfm(0x1110,2,0,0,0,0,dpf.fsm,dpf.maxfsdo,fst_len24);
	setup_iif_cntrl(0,0,1,2,0x1100,1,0);
	nframes = (NFMTS + 999) / 1000;
	nfmts = NFMTS / nframes;
	tfmts = nfmts * nframes;
	if (vbose)
	    printf("...requesting %d frames of %d formats\n",
		    nframes,nfmts);
	setup_iif_par(VALID_PIXELS,nfmts,nframes,tfmts,0,0,0,1,13,0);
	ex_set_selftest(0); /* No need if execute preamble properly ??? */
	ex_set_delay(nfmts * 8192);
	sync_code = FS & VF & AL & VL;
      	ex_set_event(SYNC_EVENT,0x0f,sync_code,nframes);
	ex_set_reset(0);
	play_dis(1);
	iif_set_reset(1);
	iif_set_reset(0);
    /* start the DCRS at the right location */
	if (*offset < 32) {
	    *block -= 1;
	    *offset += BYTES_BLOCK * 8;
	}
/*SONY*/
        if (!strcmp( media_type, "DCRSI" ) ) {
	  if (vbose)
	    printf("starting DCRSi play at %d, offset %d, polarity %d\n",
		*block,*offset,sp->polarity);
/*     	  start_fmt_play(1,*block,*offset,sp->polarity);   */
	  start_fmt_play(1,*block,*offset - 32,sp->polarity);  /* Ted likes this better */
        }
        else {
	      if (vbose)
	         printf("starting SONY play at %d, offset %d, polarity %d\n",
		      *block,*offset,sp->polarity);
	      sony_start_fmt_play(1,*block,*offset - 184,sp->polarity);  /* Ted likes this better */
             }

/*	if (abob()) exit(1); */

    /* top of scan loop */
	not_at_end = 1;
	k = 0;
	while (not_at_end) {
	    if (vbose)
		printf("scanning segment, block %d...\n",*block);

	    iif_set_reset(1);
	    iif_set_reset(0);
	    j1_clear_hdr_mem(0);
	/* arm and trigger */
	    tarm();
	    if (first) {
		reset_rpi(0);
		head_start = 20;
		sleep(3);
/*		j1_Ted_fix(); */

	    }
/*  	    else {
		j = (MLOC_IIF_HEAD >> 1) - (104 * 10);
		for (i = 0; i < 104 * 10; i++) mb.w[j + i] = fmtsav[i];
		i = ( PID_IIF_HEAD<<20 ) | (j<<1);
		asp_write( i, &mb.w[j], (104*10)<<1 );
	    }
comment out 2/27/96 CV */

	    play_dis(0);
	    tjdone();
   	    play_dis(1);
/*	    if (abob()) exit(1); */
/*	    play_dis(1);  */

	/* at this point, the data should be captured */
	    asp_read( (RLOC_JDFM+4)<<1, &mb.w[RLOC_JDFM+4], 2 );
	    lgfmt = ((int)mb.w[RLOC_JDFM + 4]) & 0xffff; /* ??? use CR */
	    fmt_count = lgfmt - lgfmt_old - 1;
	    if (lgfmt < lgfmt_old) fmt_count += 65536;
	    lgfmt_old = lgfmt;
	    if (vbose)
		printf("...%d formats scanned\n",fmt_count);
	    gap_type = 0;
	/* display start and end format numbers */
	    if (j1_get_hk_data(sp,1,&hk) == FAIL) {
		printf("NO HK ...\n");
/*		if (abob()) exit(1); */
	    }
	    if (first) {
		first = 0;
	    	first_format = hk.format;
	    }
	    else
		first_format = last_format + 1;
	    if (k < 40) {	/* mod 6/96 to take care out of array */
	        sp->ppfmt[k] = first_format;
	        sp->ppblk[k] = *block;
	        sp->ppoff[k] = *offset;
	        k++;
	    }
	    if (sp->fmt_start == -1) sp->fmt_start = first_format;
	    if (vbose)
		printf("   first format is %d\n",first_format);
	    last_format = first_format + fmt_count - 1;
	    if (vbose)
		printf("    last format is %d\n",last_format);
/*	    j1_get_fmt_loc(1,sp,last_format,&fst_len,block,offset); ??? */

	    asp_read( (RLOC_JDFM+5)<<1, &mb.w[RLOC_JDFM+5], 4 );
	    bit_slip = ((mb.w[RLOC_JDFM+5] << 16) & 0xffff0000) |
				(mb.w[RLOC_JDFM+6] & 0xffff);
	    if (vbose) printf("BITSLIP = %d\n", bit_slip);
	    scan_bit = fmt_count*fst_len + (bit_slip - slip_old);
	    if (vbose) printf("NEW BITSLIP = %d\n", bit_slip - slip_old);
	    slip_old = bit_slip;
	    *block += scan_bit / (BYTES_BLOCK * 8);
	    *offset += scan_bit % (BYTES_BLOCK * 8);
	    if (*offset >= BYTES_BLOCK * 8) {
		*offset -= BYTES_BLOCK * 8;
		*block += 1;
	    }
	    if (vbose)
		printf("scan %d bits ...now at block %d, offset %d\n",scan_bit,*block,*offset);

	/* see if we're still in sync */
	    asp_read( (RLOC_JDFM+7)<<1, &mb.w[RLOC_JDFM+7], 2 );
	    doflg = (mb.w[RLOC_JDFM + 7] >> 14) & 0x1;
            printf("doflg=%d, %x, %d,%d\n", doflg, doflg,tfmts, fmt_count);
            /*if (doflg == 0 || (tfmts - fmt_count > (tfmts / 10))) {*/
            if ((doflg == 0) || (fmt_count == -1)){
		printf("lost sync at or before block %d\n",*block);
		not_at_end = 0;
		gap_type = (*block > end_block - 1000) ? 0 : 2;
	    }
	/* check for data window position change */
	    j1_find_hk_change(sp,last_format);
	/* check for postamble */
	    if (sp->wdwp[sp->win_count - 1] == 10) {
		sar_end = sp->wfmt[sp->win_count - 1];
		printf("STC = 0, possible postamble at %d!\n", sar_end);
		sp->win_count -= 1; /* kick out wdwp = 10 */
		last_format = sar_end;
/*		dc_stop(IN); for j1_get_fmt_loc in proc_postamble */
/*
	    	if (sar_end < last_format && sar_end > 0) {
	            if (j1_proc_postamble(sp,sar_end) == PASS) {
		    	printf("found postamble\n");
		    	post_fud = 1;
		    	if (gap_type == 2) {
		            if (*block >= end_block - MIN_TAKE_LEN)
				sp->gap_type = 0;
		    	    else
				sp->gap_type = 2;
			}
		    	else
		    	    sp->gap_type = 1;

                       if( strcmp( media_type, "DCRSI" ) )
                          sony_stop(IN);
                       else
                          dc_stop(IN);

		    	sp->pptotal = k;
		    	return (PASS);
		    }
	    	}
*/
	    }
	/* postamble not found */
	    if (gap_type == 2 || not_at_end == 0) {
		sp->gap_type = 2;
		sp->fmt_end = last_format;
		if(last_format - first_format > NFMTS){
		  printf("Difference between first and last format is too large\n");
		  sp->fmt_end = (int)((float)((*block - sp->blk_start)*
			    BYTES_BLOCK/BYTES_FORMAT)) + sp->fmt_start;
		  printf("Updated the last format to %d\n",
			      sp->fmt_end);
		}
		get_fmt_time(sp,sp->fmt_end,&sp->end);
/*	        j1_get_fmt_loc(1,sp,last_format,&fst_len,block,offset); ??? */
		last_end = sp->end;
		sp->blk_end = *block;
/*SONY*/
                if( strcmp( media_type, "DCRSI" ) )
                  sony_stop(IN);
                else
                  dc_stop(IN);

		sp->pptotal = k;
		return (PASS);
	    }
	  /* save tail end of this scan -- postamble may start there */
	    i = (sp->fmt_end + 1) * 20 + MLOC_IIF_HEAD; /* ??? */
	    pid = mb.w[RLOC_REP];
	    mb.w[RLOC_REP] = PID_IIF_HEAD;
/*	    getdata(fmtsav,i,104*5); ??? */
	    mb.w[RLOC_REP] = pid;
	    fflush(stdout);
	}  /* while not_at_end */
	printf("j1_scan_segment could not find end of take\n");
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
           sony_stop(IN);
        else
           dc_stop(IN);

	return (FAIL);
}



/* j1_proc_postamble(sp,fmt) -------------------------------------
	This routine attempts to process a postamble at the format
	location given.  If that location is the start of a postamble, 
	the postamble processing is performed, a postamble record is 
	created, and the routine returns PASS, otherwise FAIL.
*/

j1_proc_postamble(sp,fmt)
	TAPE_SEG_PTR sp;
	int fmt;
{
#undef NFMTS
#define NFMTS 120 /* How many do we need ??? */
	POSTAM_FILE_PTR post;
	int sync_code,block,offset;
	int fmt_end;
	int ref_count,start_id;
	GMT ref_time;
	char filename[80];
	char hk_buf,pcm_buf,hk_file,pcm_file;

	printf("Attempting to locate postamble at %d\n", fmt);
	j1_get_fmt_loc(1,sp,fmt,&fst_len,&block,&offset);

    /* set up processor to capture the postamble and chirp */
	reset_rpi(1);
	reset_jdfm(1,1);
	reset_jdfm(0,0);
/*
	strcat(strcpy(filename,DEF_PATH),"params.cal");
	if (read_proc_param_file(filename) == FAIL) {
	    printf("...unable to read params.cal for postamble\n");
	    return (FAIL);
	}
*/
	setup_jdfm(0x1110,2,0,0,0,0,dpf.fsm,dpf.maxfsdo,fst_len24);
	setup_iif_cntrl(0,0,1,3,0x1100,1,0);
	setup_iif_par(VALID_PIXELS,NFMTS,1,NFMTS,0,0,0,1,13,0);
	ex_set_selftest(0);
	ex_set_delay(8192 * 2 * NFMTS);
	sync_code = SS & FS & VF & AL & VL;
	ex_set_event(SYNC_EVENT,0x1f,sync_code,1);
	ex_set_reset(0);
	play_dis(1);
	iif_set_reset(1);
	iif_set_reset(0);
    /* start the DCRS at the right location */
/*SONY*/
        if (strcmp( media_type, "DCRSI" ) ) {
	  if (vbose)
	    printf("starting DCRSi play at %d, offset %d, polarity %d\n",
		block,offset,sp->polarity);
	  start_fmt_play(1,block,offset-32,sp->polarity);
        }
        else {
	       if (vbose)
	         printf("starting SONY play at %d, offset %d, polarity %d\n",
		     block,offset,sp->polarity);
	       sony_start_fmt_play(1,block,offset-184,sp->polarity);
             }
    /* arm and trigger, also save the averger memory */
	j1_clear_hdr_mem(0);
	tarm();
	sleep(3);
/*	j1_Ted_fix(); comment on 3/25/96 due to hang */
	reset_rpi(0);
	play_dis(0);
/*	p_get_avg_image(128, 128, NULL); comment out for test */
	tjdone();

    /* at this point, the data should be captured */
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
           sony_stop(IN);
        else
           dc_stop(IN);

    /* see if we got enough data */
	fmt_end = mb.w[0x8000]; /* ??? use CR */
	if (fmt_end < 103) {
	    printf("...lost sync\n");
	    return (FAIL);
	}

/* Do we need E. Chu's header decoder and calibration S/W here ??? */
/*
	j1_hdr(&ref_count,&ref_time,&hk_file,&pcm_file,&start_id);
	j1_cal(rep_file,cal_agc,cal_att,abs,hk_file);
*/
	return (PASS);
}



/* j1_get_fmt_loc(f_dcrsi,sp,fmt,fst_len,block,offset) -------------------------
	This routine calculates approximate block address and do re-sync 
	for exact block as well as offset of the given format number, fmt,
	based on the reference addrss shown in sp.
	Here, we calculate "distance" based on the average format length.
	Do re-sync, get the new block, offset, and fst_len.  Hope ...
	the HW can take care of this!
	
	Do we need fst_len from j1_get_fmt_loc ???
*/

j1_get_fmt_loc(f_dcrsi,sp,fmt,fst_len,block,offset)
	TAPE_SEG_PTR sp;
	int f_dcrsi,fmt,*fst_len,*block,*offset;
{
	int fmt_diff, norm_inv, format;
	float total_bit, prf;
/*SONY*/
        int BYTES_BLOCK;

        BYTES_BLOCK = TAPE_BLOCK_LEN;  /* 144304 for SONY, 4356 for DCRSi*/

	fmt_diff = fmt - sp->fmt_start;
	total_bit = (float)fmt_diff * fmt_len + sp->bit_off;
	total_bit -= 6.; /* just in case there is an extra short line */
/*SONY*/
        if ((!strcmp( data_direction, "REVERSE" ) ) && (strcmp(media_type, "DCRSI")))
	  *block = sp->blk_start - (int)(total_bit / (BYTES_BLOCK * 8.));
        else
	  *block = sp->blk_start + (int)(total_bit / (BYTES_BLOCK * 8.));

	if (f_dcrsi) {
	   /* offset calculated above will be overwrite. */
	   if (vbose) printf("get format location ...\n");
	   printf("Try at block %d\n", *block);
	   if (do_scan) {
              if ((*block > Cur_Rqst->end_blk) || 
		  (*block < Cur_Rqst->start_blk)){
                printf("block %d is greater than the end block\n",*block);
                return(FAIL);
              }
	   }
/*SONY*/
           if (!strcmp( media_type, "DCRSI" ) ) {
	     while (j1_srch_sync(3,2,10,block,offset,
			&norm_inv,&format,&prf,fst_len) != PASS) {
	       *block += 1000;
	       printf("Can not re-sync ... Try again at block %d\n", *block);
	       fflush(stdout);
	     }
           }
           else {
	          while (j1_srch_sync(3,2,1,block,offset,
			&norm_inv,&format,&prf,fst_len) != PASS) {
                      if (!strcmp( data_direction, "REVERSE" ))
	                *block -= 3;
                      else
	                *block += 3;
	              printf("Can not re-sync ... Try again at block %d\n", *block);
	              fflush(stdout);
                  }
           }
	   fmt = format; /* fix broken river; CV 1/14/98 */
	   sp->polarity = norm_inv; /* overwrite polarity 8/22/96 */
           if (*offset < 32) {      /* CV 8/19/97 avoid < 32 bit */
               *offset += fmt_len;
               if (vbose) printf("offset=%d, fmt_len=%d\n", *offset, fmt_len);
	   }
	}
	else {
	    *offset = 0; /* fake offset 3/19/96 */
	    *fst_len = fmt_len;
	}
	fst_len24 = *fst_len - 24;
	return(PASS);
}

	    

/* j1_get_hk_data(sp,index,hk) --------------------------------------------
	This routine pull out one set of HK data from the IIF memory 
	specified by "index". "hk" is the pointer to a structure to
	hold this specific set of HK data.
*/

j1_get_hk_data(sp,index,hk)
	HK_PTR hk;
	int index;
	TAPE_SEG_PTR sp;
{
	int i, pid_save, temp1, temp2, polarity;
	int even, even8, agc12, agc_mode, agc, junk;

	even = (index + 1) % 2;
	even8 = even * 8;
	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HEAD;
	i = ((index-1) * HK_LEN) / 2 + (MLOC_IIF_HEAD >> 1);
/*	polarity = sp->polarity; */
	polarity = 1; /* assume RPI norm-inv takes care of bit inverse */
	
	asp_read( (PID_IIF_HEAD<<20) | (i<<1), &mb.w[i], 24 );

	/* pull out prf information */
	if (((((mb.w[i] >> (14-even8)) & 0x3) != 2) && (polarity == 1)) ||
	    ((((mb.w[i] >> (14-even8)) & 0x3) != 1) && (polarity == 0))) {
		if (vbose) printf("PRF is off !?\n");
		hk->prf = 0.;
		return(FAIL);
	}
	else {
		temp1 = ((mb.w[i] >> (8-even8)) & 0xff);
/*		hk->prf = j1_get_prf(temp1, 24, polarity); */
		junk = j1_get_prf(temp1, 24, polarity); /* To be fixed by Ming */
	}

	/* pull out sar information */
	asp_read( (PID_IIF_HEAD<<20)|((i+even)<<1), &mb.w[i+even], 2 );
	if (((((mb.w[i+even] >> (6+even8)) & 0x3) != 2) && (polarity == 1)) ||
	    ((((mb.w[i+even] >> (6+even8)) & 0x3) != 1) && (polarity == 0))) {
		printf("Neither CAL nor OBS mode !?\n");
		return(FAIL);
	}
        if ((((mb.w[i+even]>>(4+even8)) & 0x3) != 2) &&
			((mb.w[i+even]>>(4+even8)) & 0x3) != 1) {
		printf("Suspicious CAL/OBS mode bits ... \n");
		return(FAIL);
        }
	if (((((mb.w[i+even]>>(4+even8)) & 0x3) == 2) && (polarity == 1)) ||
	    ((((mb.w[i+even]>>(4+even8)) & 0x3) == 1) && (polarity == 0)))
	    hk->sar = 1;
	else
	    hk->sar = 0;
	
	/* pull out 8 bits STC data */
	temp1 = 0x5555; /* interleaved zero */
	if (even == 0)
	    temp2 = (mb.w[i+2] & 0xffff) | 0x55550000;
	else
	    temp2 = ((mb.w[i+2] << 8) & 0xff00) | ((mb.w[i+3] >> 8) & 0xff)
			| 0x55550000;
	hk->stc = j1_get_fmt(temp1, temp2, 16, polarity);
	if (hk->stc == -1) return(FAIL);
	hk->stc = ((hk->stc >> 3) + 1) * 10; /* use only 5 bits, Ming, 5/23 */
/*	if (vbose) printf("STC in micro second = %d\n", hk->stc); */

	/* pull out 12 bits AGC data */
	temp1 = 0x5555; /* interleaved zero */
	if (even == 0)
	    temp2 = ((mb.w[i+3] << 8) & 0xffff00) | ((mb.w[i+4] >> 8) & 0xff) 
			| 0x55000000;
	else
	    temp2 = ((mb.w[i+3] << 16) & 0xff0000) | (mb.w[i+4] & 0xffff) 
			| 0x55000000;
	agc12 = j1_get_fmt(temp1, temp2, 16, polarity);
	if (agc12 == -1) return(FAIL);
	agc_mode = agc12 >> 11;
	hk->agc = (agc12 >> 5) & 0x1f; /* use only 5 bits, Ming, 6/3 */
/*	printf("AGC mode = %d, AGC = %d\n", agc_mode, agc); */

	/* pull out STP data */

	/* pull out format number */
	if (even == 0) {
	    temp1 = ((mb.w[i+8] << 8) & 0xff00) | ((mb.w[i+9] >> 8) & 0xff);
	    temp2 = (((int)mb.w[i+9] << 24) & 0xff000000) | 
			(((int)mb.w[i+10] << 8) & 0xffff00) |
			((mb.w[i+11] >> 8) & 0xff);
	}
	else {
	    temp1 = mb.w[i+9] & 0xffff;
	    temp2 = ((mb.w[i+10] << 16) & 0xffff0000) | (mb.w[i+11] & 0xffff);
	}
	hk->format = j1_get_fmt(temp1, temp2, 16, polarity);
	if (hk->format == -1) return(FAIL);
	if (vbose) printf("*** HK %d ***, fmt number = %d\n", index, hk->format);
	mb.w[RLOC_REP] = pid_save;
	return(PASS);
}



/* j1_get_sar_fmt(sp,sar_fmt) ---------------------------------------------
	This routine search through the IIF memory to find if there
	is a SAR on/off transient.  If a SAR on/off transient is found,
	return the format number.
	Ming suspect that around CAL/OBS transient we have a few PRF OFF???
	Currently, we allow 5 bad HK sets for transient.
*/

j1_get_sar_fmt(sp,sar_fmt)
	int *sar_fmt;
	TAPE_SEG_PTR sp;
{
	HK_DATA lo,hi;
	int i,j,cal_end_fmt;

	printf("Checking for CAL/OBS change\n");
	if (j1_get_hk_data(sp,1,&lo) == FAIL) {
	    printf("0 set of HK, no CAL/OBS checked!\n");
	    return(FAIL);
	}
    /* see if there is any CAL/OBS change, assume max is 200 sets of HK */
    /* Ming 12/19/91 */
	if (lo.sar == 1) {
	    printf("already in OBS mode!\n");
	    return(FAIL);
	}
	i = 0;
	cal_end_fmt = lo.format;
	for (j = 2; j < 200; j++) {
	    if (j1_get_hk_data(sp,j,&hi) == FAIL) {
	    	if (vbose) printf("HK number %d is bad\n", j);
		i++;
		if (i > 5) {
		    printf("only %d set of HK\n", j-i);
		    printf("ref cal mode format: %d\n", cal_end_fmt);
		    *sar_fmt = cal_end_fmt + 15000;
		    return(FAIL);
		}
	    }
	    else {
	    	if (hi.sar != lo.sar) {
		    printf("CAL/OBS changed at %d HK, fmt = %d\n",
					j,hi.format);
		    *sar_fmt = hi.format;
		    break;
		}
		cal_end_fmt = hi.format;
	    }
	}
	return;
}
	
/* j1_find_hk_change(sp,fmt_end) --------------------------------------
	This routine locates all AGC changes and no more than one STC
	change in the IIF HK memory after each scan.  
	fmt_end is the last format for this scan.
*/

j1_find_hk_change(sp,fmt_end)
	TAPE_SEG_PTR sp;
	int fmt_end;
{
	HK_DATA lo,hi;
	int stc_change, j;

	printf("Checking for STC/AGC change\n");
	if (j1_get_hk_data(sp,1,&lo) == FAIL) {
	    printf("0 set of HK, no HK checked!\n");
	    return;
	}
    /* if tables are empty, fill in first entry */
	if (sp->win_count == 0 || sp->agc_count == 0) {
	    sp->wfmt[0] = lo.format;
	    sp->wdwp[0] = lo.stc;
	    sp->win_count = 1;
	    stc_old = lo.stc;
	    sp->afmt[0] = lo.format;
	    sp->agc[0] = lo.agc;
	    sp->agc_count = 1;
	    agc_old = lo.agc;
	}
	else {
	    if (lo.stc != stc_old) {
	   	printf("   STC changed at the 1st HK, OLD = %d, NEW = %d\n",
				stc_old, lo.stc);
              /* CV 12/4/97 add the if statement to check the window
		 format is < fmt_end & win position must be different of 10 */
              if ((lo.format < fmt_end) && ((abs(stc_old - lo.stc) == 10))){
	        if (sp->win_count < 20) {
  	    	  sp->wdwp[sp->win_count] = lo.stc;
	    	  sp->wfmt[sp->win_count++] = lo.format;
		  stc_old = lo.stc;
		  if (lo.stc == 10) return;
	        }
	      }
	    }
	    if (lo.agc != agc_old) {
	     if (sp->agc_count < 80){
	      if (abs(lo.agc-agc_old) == 1) {
	   	printf("   AGC changed at the 1st HK, OLD = %d, NEW = %d\n",
				agc_old, lo.agc);
  	    	sp->agc[sp->agc_count] = lo.agc;
	    	sp->afmt[sp->agc_count++] = lo.format;
		agc_old = lo.agc;
	      }
	      else printf("AGC bit error,  BAD = %d\n", lo.agc);
	     }
	    }
	}
    /* see if there is any HK change, assume max is 20 sets of HK */
    /* Ming 12/16/91 */
	stc_change = 0;
	for (j = 2; j < 20; j++) {
	    if (j1_get_hk_data(sp,j,&hi) == FAIL) {
	    	if (vbose) printf("only %d set of HK\n", j-1);
		return;
	    }
	    else {
	    	if (hi.format > fmt_end) return;
		else {
		    if (hi.stc != stc_old && stc_change == 0) {
			printf("   STC changed! OLD = %d, NEW = %d\n", stc_old, hi.stc);
		      if (sp->win_count < 20) {
		  	sp->wdwp[sp->win_count] = hi.stc;
			sp->wfmt[sp->win_count++] = hi.format;
			stc_old = hi.stc;
			stc_change = 1;
			if (lo.stc == 10) break;
		      }
		    }
		    if (hi.agc != agc_old) {
		     if (sp->agc_count < 80) {
	      	      if (abs(hi.agc-agc_old) == 1) {
			printf("   AGC changed! OLD = %d, NEW = %d\n", agc_old, hi.agc);
		  	sp->agc[sp->agc_count] = hi.agc;
			sp->afmt[sp->agc_count++] = hi.format;
			agc_old = hi.agc;
		      }
	      	      else printf("AGC bit error, BAD = %d\n", hi.agc);
		     }
		    }
		}
	    }
	}
	return;
}
j1_Ted_fix()
{
        int dc_timeout;
        char rcvd[132];

/*   This is Ted's high quality fix to the drop out problem */

/*        if (!strcmp(host,"smart")){ */
                printf("Welcome to Ted's fix\n"); fflush(stdout);
                dc_timeout = 0;
                dc_get_resp (IN, 5, rcvd);
                while ((strspn("DS C022", rcvd) != 7) && (dc_timeout < 40))
                {
                         dc_get_resp(IN,5,rcvd);
                         dc_timeout++;
                }
                printf("You are now leaving Ted's fix\n"); fflush(stdout);
/*        } */

/*   end Ted's high quality fix           */
}
