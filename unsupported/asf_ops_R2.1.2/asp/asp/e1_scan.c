/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* e1_scan.c -- scan the data take, locating preambles, postambles,
		preprocessing regions and recording gaps.

   CLW 12/19/95 - Initialize agc_count to zero
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
#define  HDR_LEN2 (20 >> 1)	/* header bytes per format (short mode) */
#define  ID_MINIMUM  10000	/* minimum # of formats to allow tape
				   id test to be performed */
#define VALID_PIXELS 5616	/* valid pixels/line after rg cmp */
#define BYTES_FORMAT 7424	/* bytes per ami format line */
#define FMT_RATIO    1767.9	/* ratio of format to prf */

/* system configuration parameters */
extern int vbose;		/* 1 = print debugging messages */
extern int sw_noise_meas;	/* 1 = perform noise measurement */
extern int sw_rep_meas;		/* 1 = perform replica measurement */
extern RQST_PTR Cur_Rqst;	/* pointer to current job request */
extern SV sv1,sv2;		/* current request's statevectors */
extern TAPE_SEG_PTR seg_list;	/* tape segment list */
extern SP_FILE spf;		/* sensor parameters file */
extern EM_FILE emf;		/* Earth model file */
extern DPP_FILE dpf;		/* df processing parameter file */
extern float fmt_ratio;		/* ratio of IDHT fmts to AMI fmts */
extern int ptype_sw;		/* 1 = using new pre/postamble codes */
				/* 0 = using old pre/postamble codes */
extern int num_dwp_base;
extern int dwp[20];

extern int Write_CP_Error_Status;

/*SONY*/
extern char media_type[];

AUX_DATA aux;
static int end_block;		/* ending block # of take */

/* format table */
/* these declarations do not seem to be used anywhere so ... */
/*#define FSTART 0x60000		/* start of format table in mb.b */
/*int nfindex = 0;		/* # of format index entries */
/*int nxt_ftent = FSTART;		/* next available format table entry */
/*struct {
/*	int id;			/* ami format number */
/*	int fmt;		/* idht format number */
/*	int loc;		/* table location */
/*} findex[50];		/* format table index */

int window_old = -1;		/* window pos history, -1=first time */
int dwp_base = 0;
int post_fud = 0;		/* before postamble found */
int f_dropout = 0;			/* 1 = force dropout */
int bad_preamble = 0;			/* 1 = dropout or bad AUX in proc_preamble */
static int tape_ok = 0;		/* 1 = tape id checks out OK */

/* e1_scan_take() ------------------------------------------------------
	This routine scans an ERS-1 formatted data take, breaking it up
	into segments bounded by preambles, postambles and recording
	gaps.  Within each segment preprocessing regions are located
	and verified.  The tape segment and preprocessing region data
	table is created, containing information found during this
	process.  This table will guide the operation of preprocessing
	pass two.
*/

e1_scan_take()
{
	TAPE_SEG_PTR sp,sp2;
	PP_BLOCK_PTR pp,pp2;
	int start_block, cur_block, block_skip;
	int block,offset,norm_inv,format,iformat,ami_fmt,pri;
	int nblks,segment,ans;
	int nsegs = 0;
	int nregions = 0;
	int out_of_sync = 1;
	int gap_type = 0;
	int ami_formats, short_segs, NOPREAM;
	char q[100];
        int Badpass = 0;


/* DCRSi or SONY ? */

        int MIN_TAKE_LEN, BYTES_BLOCK, BLOCKSKIP;

        BYTES_BLOCK = TAPE_BLOCK_LEN;  /* 144304 for SONY, 4356 for DCRSi*/ 
        if( strcmp( media_type, "DCRSI" ) ) {  
           MIN_TAKE_LEN = 50000/33;
           BLOCKSKIP = 10000/33;
           block_skip = dpf.blkskip/33; 
        }
        else {
                MIN_TAKE_LEN = 50000;
                BLOCKSKIP = 10000;
                block_skip = dpf.blkskip; 
             }

    /* clear any previous tape segment and preproc. block data */
    	ami_formats = 26000;
	short_segs = 0;
	NOPREAM = 0;
	p_clear_seg_list();
/*	nfindex = 0; */
/*	nxt_ftent = FSTART; */
	tape_ok = 0;
    /* initialize loop variables */
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) ) 
	  if (Cur_Rqst->start_blk<2) Cur_Rqst->start_blk=2;
        else
	  if (Cur_Rqst->start_blk<50) Cur_Rqst->start_blk=50;/* Ming 3-8-95 */
	start_block = Cur_Rqst->start_blk;
	end_block = Cur_Rqst->end_blk;
/*	block_skip = dpf.blkskip; */
	cur_block = start_block;
	printf("processing take from %d to %d\n",start_block,
		end_block);

    /* outer loop: tape segment loop */
	if (cur_block > (end_block - MIN_TAKE_LEN)) 
	  printf("The take is too short. Min required is %d.\n",
		MIN_TAKE_LEN);
	fflush(stdout);
	while (cur_block < (end_block - MIN_TAKE_LEN)) {
	/* attempt to sync up on the current location */
	/* NOTE: if still in sync, the variables format, cur_block,
		 offset and norm_inv contain values loaded during the
		 previous iteration of this loop */
	    start_block = cur_block; 

	    while (out_of_sync && cur_block < end_block - MIN_TAKE_LEN) {
/* SONY */
                if (!strcmp( media_type, "DCRSI" ) ) 
		  nblks = (gap_type == 2) ? 20 : 10;
                else
                     nblks = 3;

		if (ami_formats < ID_MINIMUM && ++short_segs > 2) {
		   cur_block += BLOCKSKIP;
		   short_segs = 0;
		}
		if (e1_srch_sync(16,2,nblks,&cur_block,&offset,
			&norm_inv,&iformat,&ami_fmt,&pri) == PASS) {
		    printf("found sync at block = %d\n", cur_block);
		    out_of_sync = 0;
		    format = iformat & 0xffff;
		    block_skip = BLOCKSKIP;
		}
		else {
                    if (Write_CP_Error_Status == -2)
                     return(FAIL);

		    start_block = cur_block;
		    cur_block += block_skip;
		    printf("sync not found, cur_block=%d, blkskip=%d\n",
		       cur_block,block_skip);
		}
/*SONY*/        
                if( strcmp( media_type, "DCRSI" ) )
                  sony_stop(IN);
                else
                  dc_stop(IN);

		fflush(stdout);
	    }  /* while out_of_sync */

	/* if out of sync, the take is done */
	    if (out_of_sync)
		break;

    	/* calculate format ratio */
	    fmt_ratio = FMT_RATIO * (dpf.b_prf * (pri + dpf.a_prf));

	if (NOPREAM == 0 && ami_formats > 10000) {
	/* create a new tape segment block if last segment was not
	   a short one (<10000 formats) or if a no-preamble 
	   situation has occured. */ 
	    if (vbose)
		printf("creating tape segment block\n");
	    if ((sp = (TAPE_SEG_PTR) malloc(sizeof(TAPE_SEG))) == NULL) {
		printf("Out of memory in preproc pass 1\n");
		return (ABORT);
	    }
	    if (seg_list == NULL) {
		seg_list = sp;
		sp->prenum = 0;
		sp->postnum = 0;
		sp->pre = NULL;
		sp->post = NULL;
	    }
	    else {
		sp2->nxt_seg = sp;
		*sp = *sp2;
                Badpass = 1;
	    }
    	} /* end if NOPREAM == 0  */

	    sp->ppb_list = NULL;
	    sp->nxt_seg = NULL;
	    sp->fmt_start = format;
	    sp->fmt_end = format;
	    sp->fmt_id = ami_fmt;
	    sp->end_id = 0;
	    sp->blk_start = cur_block;
	    sp->bit_off = offset;
	    sp->polarity = norm_inv;
	    sp->gap_type = gap_type;
	    sp->win_count = 0;
	    sp->agc_count = 0;	/* clw initialize this value to 0 */
	    block = cur_block;
/*
	    iformat = 0;
	    e1_get_fmt_loc(-1,iformat,sp,&block,&offset);
	    printf("block = %d, offset = %d\n", block, offset); 
*/
	/* process the preamble, if any */
	    fflush(stdout);
	    while (e1_proc_preamble(sp,block,offset) == FAIL) {
/*
		if (gap_type == 2) {
		    printf("\nCannot locate preamble!  Let's fake it\n");
		    if (e1_fake_preamble(sp) == FAIL) 
			return (FAIL);
		    cur_block = sp->blk_start;
		    break;
		}
*/
		if (sp->fmt_id == -2) {
		    printf("\nCannot proceed\n");
		    return (FAIL);
		}
		if (sp->fmt_id == -1) {
		    printf("\nCannot locate preamble - Dropout or Bad AUX!\n");

/*
                    free(sp);
                    seg_list = NULL;
*/
                    if (Badpass == 0)
                      p_clear_seg_list();
                    out_of_sync = 1;
		    bad_preamble = 1;
		    cur_block += block_skip;
		    break;
		}
		if (gap_type == 2 || bad_preamble == 1) {
		    printf("\nCannot locate preamble!  Let's fake it\n");
		    if (e1_fake_preamble(sp) == FAIL) {
                        if (Badpass == 0)
                           p_clear_seg_list();
		    	out_of_sync = 1;
		    	cur_block += block_skip;
		    	break;
		    }
		    cur_block = sp->blk_start;
		    break;
		}
		iformat = 0;
		e1_get_fmt_loc(-1,iformat,sp,&block,&offset);
		if (block == sp->blk_start) {
		    printf("\nCannot locate preamble\n");
		    return (FAIL);
		}
		if (block <= (Cur_Rqst->start_blk - 5)) {
		  if (NOPREAM) {
		    printf("NO PREAMBLE! start proc at = %d\n", block);
		    if (e1_fake_preamble(sp) == FAIL) {
                        if (Badpass == 0)
                           p_clear_seg_list();
		    	out_of_sync = 1;
		    	cur_block += block_skip;
		    	break;
		    }
		    cur_block = sp->blk_start;
		    NOPREAM = 0;
		    break;
		  }
		  else {
                    if (Badpass == 0)
                      p_clear_seg_list();
		    NOPREAM = 1;
		    out_of_sync = 1;
		    cur_block = Cur_Rqst->start_blk;
		    break;
		  }
		}
		cur_block = block;
	    }

	  fflush(stdout);
	  while (out_of_sync == 0) {
	/* scan the take, locating a sync loss, postamble, or the
	   end of the take */
	    p_rw_pream_file(1,"preambles");
	    if (e1_scan_segment(sp,&cur_block,&offset) == ABORT)
		return (FAIL);
	    if (vbose)
		printf("...scan done, cur_block = %d\n",cur_block);
	    gap_type = sp->gap_type;
	    out_of_sync = 1;
	    ami_formats = sp->end_id - sp->fmt_id;
	    get_fmt_time(sp,sp->fmt_id,&sp->start);
	    if (vbose)
	      printf("end of scan_take loop, cur_block=%d\n",cur_block);
	    sp2 = sp;
	    if (post_fud == 1) {
		cur_block += BLOCKSKIP;
		out_of_sync = 1;
/*SONY*/
                if( strcmp( media_type, "DCRSI" ) )   
	           block_skip = dpf.blkskip/33;
                else
	           block_skip = dpf.blkskip;
                 
		if (vbose)
	          printf("start of next segment scan, block=%d\n", cur_block);
	    }
	    fflush(stdout);
	   } /* while out_of_sync == 0 */
	   fflush(stdout);
	}  /* while cur_block */

        bad_preamble = 0;
	return (PASS);
}

/* e1_fake_preamble(sp) ------------------------------------------
	This routine attempts to fake a preamble at the format
	location given in the tape segment sp. 
*/

e1_fake_preamble(sp)
	TAPE_SEG_PTR sp;
{
	PREAM_FILE_PTR pre;
	double time_diff;
	double get_gmt_diff();

	if (vbose){
	    printf("faking preamble data block\n");
	    printf("block = %d, offset = %d, fmt_id = %d, fmt_start = %d\n",
		      sp->blk_start, sp->bit_off, sp->fmt_id, sp->fmt_start);
	}
	p_create_preamble(&pre);
	sp->pre = pre;
	sp->prenum = pre->prenum;
	stime_to_gmt(aux.time,&sp->start);

      /* save preamble data */
	memset(pre->sat,'\0',sizeof(pre->sat));
	strncpy(pre->sat,Cur_Rqst->take_id,2);
	strcpy(pre->dcrs_id,Cur_Rqst->tape_id);
	strcpy(pre->takeid,Cur_Rqst->take_id);
	strcpy(pre->sen_mode,(aux.obrc) ? "OBRC" : "OGRC");
	pre->prf = 1.0 / (dpf.b_prf * (aux.pri + dpf.a_prf));
	pre->fmt_start = sp->fmt_start;
	pre->rec_gain = aux.rec_gain;
	pre->cal_att = aux.cal_att;
	pre->fmt_id = sp->fmt_id;
	pre->blk_start = sp->blk_start;
	pre->bit_off = sp->bit_off;
	pre->polarity = sp->polarity;
	pre->repp_dton = 0.0;

        pre->noise_dton = 0.0;
        pre->calp_dton = 0.0;

    /* calculate reference time */
	e1_refine_time(sp);
	get_fmt_time(sp,sp->fmt_id,&sp->start);
	printf("start time = %d:%d:%d:%d:%g\n",sp->start.yr,
	  sp->start.day,sp->start.hr,sp->start.min,sp->start.second);

    /* compare start time of seg with state vector time */
	time_diff = get_gmt_diff(&sp->start,&sv1.gmt);
	if (time_diff < -60.0) {
	   printf("start time 60 seconds before state vector time\n");
	   return (FAIL);
	}
    /* compare start time of seg with end time plus 60 seconds */
	if (strcmp(Cur_Rqst->type,"QLK") == 0) {
	  if (time_diff > 6060.0) {
	     printf("start time more than 100 min after SV time\n");
	     return (FAIL);
	  }
	}
	else{
	  time_diff = get_gmt_diff(&sp->start,&Cur_Rqst->end);
	  if (time_diff > 60.0) {
	     printf("start time more than 1 min after end time\n");
	     return (FAIL);
	  }
	}
	return (PASS);
}


/* e1_proc_preamble(sp,iblock,ioffset) ---------------------------
	This routine attempts to process a preamble at the format
	location given in the tape segment sp.  If that location is
	the start of a preamble, the preamble processing is performed,
	a preamble record is created, and the routine returns PASS.
	If not, the format number of the first format is loaded into
	sp, and the routine returns FAIL.
*/

e1_proc_preamble(sp,iblock,ioffset)
	TAPE_SEG_PTR sp;
	int iblock,ioffset;
{
#define NPFMTS 300
	PREAM_FILE_PTR pre;
	int sync_code;
	int i,j,k,kn,misses,missmax,bestindex;
	int block,offset;
	int doflg,biterr,frmnum,fmtnum,pfmts;
	int ttbl[NPFMTS], ftbl[NPFMTS];
	double time_diff;
	double get_gmt_diff();
	float prf, e1_calmeas(),e1_noisemeas();
    /* preamble format types table -- 2 ints per entry: */
    /*        count, type                               */
	static int ptypes[] =  { 3,2, 4,3, 4,4, 20,5, 4,4, 20,5,
				 4,4, 20,5, 4,4, 20,5, 4,8, 10,9 };
	static int ptypes2[] = { 3,1, 4,1, 4,4, 20,5, 4,4, 20,5,
				 4,4, 20,5, 4,4, 20,5, 4,8, 10,9 };
	static int plen = sizeof(ptypes) / sizeof(int);

/* DCRSi or SONY ? */

        int MIN_TAKE_LEN, BYTES_BLOCK, BLOCKSKIP;

        BYTES_BLOCK = TAPE_BLOCK_LEN;  /* 144304 for SONY, 4356 for DCRSi*/
        if( strcmp( media_type, "DCRSI" ) ) {
           MIN_TAKE_LEN = 50000/33;
           BLOCKSKIP = 10000/33;
        }
        else {
                MIN_TAKE_LEN = 50000;
                BLOCKSKIP = 10000;
             }

	printf("Attempting to locate preamble\n");
    /* set up processor to capture the preamble */
	reset_rpi(1);
	reset_edfm(1,1);
	reset_edfm(0,0);
	setup_edfm(dpf.sbm,dpf.fsm,1,dpf.maxfsdo);
	setup_iif_cntrl(0,0,1,2,0x1100,0,0);
	setup_iif_par(VALID_PIXELS,NPFMTS,1,NPFMTS,0,0,0,1,13,0);
	ex_set_selftest(0);
	ex_set_delay(8192 * 2 * NPFMTS);
	sync_code = SS & FS & VF & AL & VL;
	ex_set_event(SYNC_EVENT,0x1f,sync_code,1);
	ex_set_reset(0);
	play_dis(1);
	iif_set_reset(1);
	iif_set_reset(0);
	block = iblock;
	offset = ioffset;

    /* start the DCRS at the right location */
	if (vbose)
/*SONY*/
          if( strcmp( media_type, "DCRSI" ) )
	    printf("starting SONY play at %d, offset %d, polarity %d\n",
		block,offset,sp->polarity);
          else
	    printf("starting DCRSi play at %d, offset %d, polarity %d\n",
		block,offset,sp->polarity);
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) ) 
	  sony_start_fmt_play(0,block,offset-152,sp->polarity);
        else 
	  start_fmt_play(0,block,offset,sp->polarity);
    /* arm and trigger */
	e1_clear_hdr_mem(0);
	tarm();
	reset_rpi(0);
	play_dis(0);
	tdone();
    /* at this point, the data should be captured */
    /* see if we're still in sync */
	get_rpi_stat(&doflg, &biterr, &frmnum, &fmtnum);
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
	   sony_stop(IN);
        else
           dc_stop(IN);

	if (doflg) {
	/* try a second time */
	    reset_edfm(1,1);
	    reset_edfm(0,0);
	    play_dis(1);
	    iif_set_reset(1);
	    iif_set_reset(0);
	/* move forward to next format */
	    offset += BYTES_FORMAT * 8;
	    while (offset >= BYTES_BLOCK * 8) {
		offset -= BYTES_BLOCK * 8;
		block++;
	    }
	/* start the DCRS at the right location */
	    if (vbose) {
              printf("...second attempt\n");
              if (!strcmp( media_type, "DCRSI" ) )
		printf("...starting DCRSi play at %d, offset %d, polarity %d\n",
		    block,offset,sp->polarity);
              else
		printf("...starting SONY play at %d, offset %d, polarity %d\n",
		    block,offset,sp->polarity);

	    }
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
          sony_start_fmt_play(0,block,offset-152,sp->polarity);
        else
          start_fmt_play(0,block,offset,sp->polarity);

	/* arm and trigger */
	    e1_clear_hdr_mem(0);
	    tarm();
	    reset_rpi(0);
	    play_dis(0);
	    tdone();
	/* at this point, the data should be captured */
	/* see if we're still in sync */
	    get_rpi_stat(&doflg, &biterr, &frmnum, &fmtnum);
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
	   sony_stop(IN);
        else
           dc_stop(IN);

	    if (doflg) {
		printf("...lost sync at frame %d, format %d\n",
			    frmnum,fmtnum);
		sp->fmt_id = -1;
		return (FAIL);
	    }
	}
    /* check for good header data */
	if (e1_get_good_aux_data(0,&aux,10) == FAIL) {
	    printf("...no good format headers found\n");
	    sp->fmt_id = -1;
	    return (FAIL);
	}
	sp->fmt_id = aux.format;
	sp->fmt_start = aux.ifmt;
	sp->blk_start = block;
	sp->bit_off = offset;
    /* check to see if this is a preamble */
      /* calculate number of formats in the preamble */
	pfmts = 0;
	for (k = 0; k < plen; k += 2)
	    pfmts += ptypes[k];
      /* get format types and numbers for all captured formats */
	misses = 0;
	for (i = 0; i < NPFMTS; i++) {
	    e1_get_fmt_type (i, &ttbl[i], &ftbl[i] );
	    if (ttbl[i] == -1)
		misses++;
	}
	if (misses > NPFMTS - pfmts) {
	    printf("...not enough valid formats found\n");
	    return (FAIL);
	}
      /* compare format types with those in the preamble type table */
	missmax = 5;
	for (i = 0; i < NPFMTS - pfmts; i++) {
	    j = i;
	    misses = 0;
	    for (k = 0; k < plen && misses < missmax; k++) {
		kn = ptypes[k++];
		while (kn--)
		    misses += ttbl[j++] != ((ptype_sw) ? ptypes2[k] : ptypes[k]);
		kn++;
	    }  /* while k */
	    if (misses < missmax) {
		missmax = misses;
		bestindex = i;
	    }
	    if (missmax == 0)
		break;
	}  /* for i */
	if (missmax > 3) {	/* too many misses - no preamble */
	    if (vbose)
	    	printf(" aux.ifmt=%d, aux.ami=%d\n", aux.ifmt, aux.format);
	    e1_get_fmt_loc(aux.ifmt,aux.format,sp,&block,&offset);
	    sp->fmt_id = aux.format;
	    sp->fmt_start = aux.ifmt;
	    sp->blk_start = block;
	    sp->bit_off = offset;
	    printf("...not a valid preamble\n");
	    return (FAIL);
	}
    /* we have a recognizeable preamble */
	printf("...preamble found at hdr format %d\n",bestindex);
	bad_preamble = 0;
	post_fud = 0;
	if (vbose)
	    printf("creating preamble data block\n");
	p_create_preamble(&pre);
	sp->pre = pre;
	sp->prenum = pre->prenum;
      /* get aux data for start format of preamble */
	e1_get_good_aux_data(bestindex,&aux,NPFMTS);
	e1_get_fmt_loc(aux.ifmt-1,aux.format-1,sp,&block,&offset);
	sp->fmt_start = aux.ifmt-1;
	sp->fmt_id = aux.format-1;
	sp->blk_start = block;
	sp->bit_off = offset;
	e1_get_good_aux_data(bestindex+7,&aux,NPFMTS);
	stime_to_gmt(aux.time,&sp->start);
      /* save preamble data */
	strcpy(pre->sat,"E1");
	strcpy(pre->dcrs_id,Cur_Rqst->tape_id);
	strcpy(pre->takeid,Cur_Rqst->take_id);
	strcpy(pre->sen_mode,(aux.obrc) ? "OBRC" : "OGRC");
	pre->prf = 1.0 / (dpf.b_prf * (aux.pri + dpf.a_prf));
	pre->fmt_start = sp->fmt_start;
	pre->rec_gain = aux.rec_gain;
	pre->cal_att = aux.cal_att;
	pre->fmt_id = sp->fmt_id;
	pre->blk_start = sp->blk_start;
	pre->bit_off = sp->bit_off;
	pre->polarity = sp->polarity;
	pre->repp_dton = 0.0;

    /* measure noise power */
	pre->noise_dton = e1_noisemeas(bestindex+1);
    /* measure drift calibration pulses */
	pre->calp_dton = e1_calmeas(bestindex+7);
    /* calculate reference time */
	e1_refine_time(sp);
	get_fmt_time(sp,sp->fmt_id,&sp->start);
	printf("start time = %d:%d:%d:%d:%g\n",sp->start.yr,
	  sp->start.day,sp->start.hr,sp->start.min,sp->start.second);

    /* compare start time of seg with state vector time */
	time_diff = get_gmt_diff(&sp->start,&sv1.gmt);
	if (time_diff < -60.0) {
	   printf("start time less than state vector time\n");
	   sp->fmt_id = -2;
	   return (FAIL);
	}
    /* compare start time of seg with end time plus 60 seconds */
	if (strcmp(Cur_Rqst->type,"QLK") == 0) {
	  if (time_diff > 6060.0) {
	     printf("start time more than 100 min after SV time\n");
	     sp->fmt_id = -2;
	     return (FAIL);
	  }
	}
	else{
	  time_diff = get_gmt_diff(&sp->start,&Cur_Rqst->end);
	  if (time_diff > 60.0) {
	     printf("start time more than 1 min after end time\n");
	     sp->fmt_id = -2;
	     return (FAIL);
	  }
	}
	return (PASS);
}


/* float e1_calmeas(fmt) -----------------------------------------------
	This routine measures the peak power of the four drift
	calibration pulses which are part of the preamble or postamble.
	fmt = the format number of the start of the pre(post)amble in
	header memory.
*/

float e1_calmeas(fmt)
	int fmt;
{
	short int tf[2048][2];
	unsigned char replica[2048];
	unsigned char r2[2048];
	int f,dcbias,n3sig,istat;
	int len = 702;
	float power,gain[2],bias[2],spikes[2048],peakoffset;
	float peak,x3db,pslr,xislr,iqrephase;
	float tpower = 0.0;
	int count = 0;
	int diff = 0;

	printf("Measuring calibration pulse characteristics\n");
	if (sw_rep_meas == 0) {
	    printf("...bypassed\n");
	    return(1.0);
	}
    /* get the range transfer function */
	p_get_mem_file(tf,4096,DEF_PATH,"rf_ra_file");
    /* read and process 4 drift calibration pulses */
	for (f = fmt; f < fmt + 96; f += 24) {
	    e1_get_replica(f,replica,OFF);
	    /* NOTE: repmeas_ is a FORTRAN subroutine call */
	    istat = 0;
	    dcbias = 32;  /* E-ERS-1 specific offset */
	    repmeas_(replica,tf,&len,gain,bias,&dcbias,&iqrephase,
		spikes,&n3sig,&peak,&peakoffset,&x3db,&pslr,&xislr,
		&istat);
	    if (istat != PASS)
		printf("   repmeas had a problem -- see file fort.6 for details\n");
	    tpower += peak;
	    if (vbose)
		printf("...replica %d power=%g\n",count,peak);
	    count++;
	}
	printf("...calc completed, tpower=%g\n",tpower);
	return (tpower);
}


/* float e1_noisemeas(fmt) ---------------------------------------------
	This routine measures the average noise power in the 6 good
	noise lines at the start of the preamble/postamble.
*/

float e1_noisemeas(fmt)
	int fmt;
{
#define SYMAX 4096
	char syncs[SYMAX];
	int i,j,k,len,spacing,loc,count;
	unsigned int uval;
	float power,val,avg_i,avg_q,sum_i,sum_q;
	float tpower = 0.0;

	printf("Measuring noise power\n");

	if (sw_noise_meas == 0) {
	    printf("...bypassed\n");
	    return (1.0);
	}
	len = SYMAX;
	spacing = 8192 * 4;
	ex_get_spaced_syncs(0,0x1000000,syncs,&len,spacing);
	if (vbose)
	    printf("  found %d syncs\n",len);
    /* find the first line of noise */
	loc = 0;
	j = 0;
    /* end of dump */
	for (i = 0; i < len && j < fmt; i++) {
	    if ((syncs[i] & ~VL) == 0)
		j++;
	    loc += spacing;
	}
	if (j < fmt) {
	    printf("e1_noisemeas could not find noise formats\n");
	    return (0.0);
	}
    /* retrieve each noise line and calculate its total power */
	count = 0;
	if (vbose)
	    printf("   using data lines ");
	fflush(stdout);
	while (i < len && j < fmt+6) {
	    if ((syncs[i] & (~VL & 0xff)) == 0) {
		if (vbose)
		    printf("%d ",i);
		j++;
		mr(loc,loc+spacing,0x10000,0x10000+spacing);
		sum_i = 0.0;
		sum_q = 0.0;
		for (k = (0x10000 >> 1); 
			k < ((0x10000 + VALID_PIXELS * 4) >> 1); k+=2) {
		    sum_i += (mb.w[k] & 0xffff) >> 11;
		    sum_q += (mb.w[k+1] & 0xffff) >> 11;
		}
		avg_i = sum_i/(float)VALID_PIXELS;
		avg_q = sum_q/(float)VALID_PIXELS;
		printf("in noise_meas:  mean(i)=%f, mean(q)=%f\n",
			avg_i,avg_q);
		sum_i = 0.0;
		sum_q = 0.0;
		for (k = (0x10000 >> 1); 
			k < ((0x10000 + VALID_PIXELS * 4) >> 1); k+=2) {
		    val = ((mb.w[k] & 0xffff) >> 11) - avg_i;
		    sum_i += val*val; 
		    val = ((mb.w[k+1] & 0xffff) >> 11) - avg_q;
		    sum_q += val*val; 
		}
		power = sum_i + sum_q;
	printf("in noise_meas:  sum(i-mean)**2=%f, sum(q-mean)**2=%f\n",
			sum_i, sum_q);
	printf("in noise_meas:  sum(i&q-mean)**2=%f,valid pixels=%d\n",
			power, VALID_PIXELS);

		if (power < 20000.0) {
		  tpower += power / (float) VALID_PIXELS;
		  count++;
		}
		else printf("in noise_meas:  too high, rejected\n");
	    }  /* if */
	    loc += spacing;
	    i++;
	    fflush(stdout);
	}  /* while i */
	if (vbose)
	    printf("\n");
	if (count>0) tpower /= (float)(count);
	printf("...%d noise lines used, tpower=%g\n",count,tpower);
	return (tpower);
}


/* e1_scan_segment(sp,block,offset) -----------------------------
	This routine scans the given tape segment from start to end,
	looking for:  1) loss of sync;  2) a postamble.  Either one
	signals end of segment, and the location and reason are
	returned.  If a postamble is found, it is processed.
	    Also, the routine checks for data window position changes,
	and records any found in the global table dwp.
*/

e1_scan_segment(sp,block,offset)
	TAPE_SEG_PTR sp;
	int *block,*offset;
{
#define NFMTS 24000
	short int fmtsav[104 * 10];
	int not_at_end,gap_type,blkno,ifmt,type,sync_code,fmt_end;
	int i,j,k,kn,misses,missmax,bestindex,pid,head_start,ans;
	int doflg,biterr,frmnum,fmtnum,nfmts,nframes,tfmts;
	int first = 1;
	int ami_format, fmt_scan;
	int blkno_old;
	int first_format,last_format;

	char q[80];

/* DCRSi or SONY ? */

        int MIN_TAKE_LEN, BYTES_BLOCK, BLOCKSKIP;

        BYTES_BLOCK = TAPE_BLOCK_LEN;  /* 144304 for SONY, 4356 for
DCRSi*/
        if( strcmp( media_type, "DCRSI" ) ) {
           MIN_TAKE_LEN = 50000/33;
           BLOCKSKIP = 10000/33;
        }
        else {
                MIN_TAKE_LEN = 50000;
                BLOCKSKIP = 10000;
             }

    /* set up processor to scan the segment */
	reset_rpi(1);
	reset_edfm(1,1);
	reset_edfm(0,0);
	setup_edfm(dpf.sbm,dpf.fsm,1,dpf.maxfsdo);
	setup_iif_cntrl(0,0,1,2,0x1100,1,0);
	nframes = (NFMTS + 999) / 1000;
	nfmts = NFMTS / nframes;
	tfmts = nfmts * nframes;
	if (vbose)
	    printf("...requesting %d frames of %d formats\n",
		    nframes,nfmts);
	setup_iif_par(VALID_PIXELS,nfmts,nframes,tfmts,0,0,0,1,13,0);
	ex_set_delay(nfmts * 8192);
	sync_code = FS & VF & AL & VL;
	ex_set_event(SYNC_EVENT,0x0f,sync_code,nframes);
	ex_set_reset(0);
	play_dis(1);
	iif_set_reset(1);
	iif_set_reset(0);
    /* start the DCRS at the right location */
	*block = sp->blk_start;
	*offset = sp->bit_off;
	if (vbose)
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
	  printf("starting SONY play at %d, offset %d, polarity %d\n",
		*block,*offset,sp->polarity);
        else
           printf("starting DCRSi play at %d, offset %d, polarity %d\n",
                *block,*offset,sp->polarity);
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
          sony_start_fmt_play(0,*block,*offset-152,sp->polarity);
        else
          start_fmt_play(0,*block,*offset,sp->polarity);

    /* top of scan loop */
	not_at_end = 1;
	blkno = *block;
	while (not_at_end) {
	    if (vbose)
		printf("scanning segment, block %d...\n",blkno);
	    iif_set_reset(1);
	    iif_set_reset(0);
	    e1_clear_hdr_mem(1);
	/* arm and trigger */
	    tarm();
	    if (first) {
		first = 0;
		reset_rpi(0);
		head_start = 20;
	    }
	    else {
		j = (MLOC_IIF_HEAD >> 1) - (104 * 10);
		for (i = 0; i < 104 * 10; i++) mb.w[j + i] = fmtsav[i];
		i = (PID_IIF_HEAD<<20) | (j<<1);
	/* Do not save the tail end register, VME can not handle
	this. The chance postamble occurs at tail end should be small*/
	/*	asp_write( i, &mb.w[j], (104*10)<<1 );*/
		head_start = -104;
	    }
	    play_dis(0);
	    tddone();
	    play_dis(1);
	/* at this point, the data should be captured */
	    blkno_old = blkno;
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) ){
/*
	    blkno = sony_get_addr(IN);
*/
        }
        else 
	    blkno = dc_get_addr(IN);

	    fmt_end = e1_find_last_hdr();
	    if (vbose)
		printf("...%d formats scanned\n",fmt_end+1);
	    gap_type = 0;
	/* display start and end format numbers */
	    if (e1_get_good_aux_data(0,&aux,fmt_end) == FAIL) {
	       /* check to see if format agree with expected */
		first_format = (int)(
			  ( (float)(blkno_old - sp->blk_start)*
			    (float)(BYTES_BLOCK)/(float)(BYTES_FORMAT) )
			       /fmt_ratio) + sp->fmt_id;
		if (abs(first_format - aux.format) > 5) {
		  printf("bad format in scan: expected= %d, got = %d\n",
			 first_format,aux.format);
		 /* return with what or do what here? */
/*SONY*/
                if( strcmp( media_type, "DCRSI" ) )
                   sony_stop(IN);
                else
                   dc_stop(IN);

		  sp ->gap_type = 2;
		  sp->fmt_end = -1;
		  sp->end_id  = first_format;
		  get_fmt_time(sp,sp->end_id,&sp->end);
		  first_format += fmt_end;
		  e1_get_fmt_loc(-1,first_format,sp,block,offset);
		  sp->blk_end = blkno_old;
		  return (PASS);
		}
	    }
	    else first_format = aux.format;
	    if (sp->fmt_id == -1) sp->fmt_id = aux.format;
	    if (vbose) {
		printf("   first format is %d\n",aux.format);
	    }
	    e1_get_good_aux_data(fmt_end,&aux,fmt_end);
	    last_format = aux.format;
	    if (vbose)
		printf("    last format is %d\n",aux.format);
	    
	    if (blkno == 0) {
		printf(" DCRS output block 0, change it! ");
		blkno = blkno_old + NFMTS*BYTES_FORMAT/BYTES_BLOCK;
	    }
	    if (vbose)
		printf("...now at block %d\n",blkno);

	/* see if we're still in sync */
	    get_rpi_stat(&doflg, &biterr, &frmnum, &fmtnum);
	    if (f_dropout == 1) {
		printf("force dropout activated!\n");
		doflg = 1;
		f_dropout = 0;
	    }
	    if (doflg || (tfmts - fmt_end > (tfmts / 10))) {
		printf("lost sync at or before block %d\n",blkno);
		printf("doflg = %d, fmt_end = %d\n", doflg, fmt_end);
		/* blkno = blkno_old; */
		not_at_end = 0;
		fmt_scan = fmt_end;
	    }
	    blkno_old = blkno;
	/* check for data window position change */
	    e1_find_dwp_change(sp,fmt_end);
	/* check for postamble (checks for 3 "no data" formats
				within 4 consecutive formats) */
	    j = 0;
	    k = 0;
	    for (i = head_start; (i < fmt_end - 103) && (j < 3); i++) {
/* Ming temp workaround, because can not handle head_start equal -104,
   and this part is really slow. MC 6/12/95 */
if (1 == 1) {
    printf("No Postamble checking\n");
    break;
}
		e1_get_fmt_type(i,&type,&ifmt);
		if (type == ((ptype_sw) ? 10 : 1)) {
		    if (k == 0) {
			k = 4;
			j = 0;
		    }
		    j++;
		}
		if (k)
		    k--;
		else
		    j = 0;
	    /* if a postamble was found, process it */
		if (j >= 3) {
		    if (e1_proc_postamble(sp,i) == PASS) {
			printf("found postamble\n");
			post_fud = 1;
			if (blkno >= end_block - MIN_TAKE_LEN)
			    sp->gap_type = 0;
			else
			    sp->gap_type = 1;
			ami_format = sp->post->fmt_id + 104;
			e1_get_fmt_loc(-1,ami_format,sp,block,offset);
			sp->blk_end = *block;
			if (vbose)
			    printf("...at format %d, block %d\n",
				ami_format,*block);
/*SONY*/
                        if( strcmp( media_type, "DCRSI" ) )
                           sony_stop(IN);
                        else
                           dc_stop(IN);

			return (PASS);
		    }  /* if e1... */
		}  /* if j */
	    }  /* for i */
	  /* postamble not found */
	    if (not_at_end == 0) {
		sp->gap_type = 2;
		e1_get_good_aux_data(fmt_end,&aux,fmt_end);
		sp->fmt_end = aux.ifmt;
		sp->end_id = aux.format;
		if(last_format - first_format > NFMTS){
		  printf("Difference between first and last format is too large\n");
		  sp->end_id = (int)(( (float)(blkno-sp->blk_start)*
			    (float)(BYTES_BLOCK)/(float)(BYTES_FORMAT) )
			       /fmt_ratio) + sp->fmt_id;
		  printf("blkno=%d, sp->blk_start=%d, sp->fmt_id=%d\n",
				blkno, sp->blk_start, sp->fmt_id);
		  printf("old equation: sp->end_id = %d\n", sp->end_id);
/*		  sp->end_id = first_format + fmt_scan;;	*/
/*		  printf("new equation updated the last format to %d\n",
/*			      sp->end_id);  old equation corrected   */
		}
		get_fmt_time(sp,sp->end_id,&sp->end);
		e1_get_fmt_loc(aux.ifmt,aux.format,sp,block,offset);

/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
	    blkno = sony_get_addr(IN);

		*block = blkno; /* disregard result from last line */

		sp->blk_end = *block;
/*SONY*/
                if( strcmp( media_type, "DCRSI" ) )
                  sony_stop(IN);
                else
                  dc_stop(IN);

		return (PASS);
	    }
	  /* save tail end of this scan -- postamble may start there */
	    i = (fmt_end + 1) * 20 + MLOC_IIF_HEAD;
	    pid = mb.w[RLOC_REP];
	    mb.w[RLOC_REP] = PID_IIF_HEAD;
	    getdata(fmtsav,i,104*5);
	    mb.w[RLOC_REP] = pid;
	    fflush(stdout);
	}  /* while not_at_end */
	printf("e1_scan_segment could not find end of take\n");
/*SONY*/
            if( strcmp( media_type, "DCRSI" ) )
              sony_stop(IN);
            else
              dc_stop(IN);

	return (FAIL);
}



/* e1_proc_postamble(sp,fmt) -------------------------------------------
	This routine attempts to process a postamble at the format
	location given.  If that location is the start of a postamble, 
	the postamble processing is performed, a postamble record is 
	created, and the routine returns PASS, otherwise FAIL.
*/

e1_proc_postamble(sp,fmt)
	TAPE_SEG_PTR sp;
	int fmt;
{
#undef NFMTS
#define NFMTS 120
	POSTAM_FILE_PTR post;
	int sync_code,block,offset;
	int i,j,k,kn,misses,missmax,bestindex;
	int doflg,biterr,frmnum,fmtnum,pfmts,fmt_end;
	int ttbl[NFMTS], ftbl[NFMTS];
	float prf, e1_calmeas(),e1_noisemeas();
    /* postamble format types table -- 2 ints per entry: */
    /*        count, type                               */
	static int ptypes[] =  { 4,2, 4,3, 4,6, 20,7, 4,6, 20,7,
				 4,6, 20,7, 4,6, 20,7 };
	static int ptypes2[] = { 4,1, 4,1, 4,4, 20,5, 4,4, 20,5,
				 4,4, 20,5, 4,4, 20,5 };
	static int plen = sizeof(ptypes) / sizeof(int);


	printf("Attempting to locate postamble\n");
    /* check to see if this is a postamble */
      /* get format types and numbers for all captured formats */
	for (i = 0; i < NFMTS; i++)
	    e1_get_fmt_type (i + fmt, &ttbl[i], &ftbl[i] );
      /* calculate number of formats in the postamble */
	pfmts = 0;
	for (k = 0; k < plen; k += 2)
	    pfmts += ptypes[k];
      /* compare format types with those in the postamble type table */
	missmax = 5;
	for (i = 0; i < NFMTS - pfmts; i++) {
	    j = i;
	    misses = 0;
	    for (k = 0; k < plen && misses < missmax; k++) {
		kn = ptypes[k++];
		while (kn--)
		    misses += ttbl[j++] != ((ptype_sw) ? ptypes2[k] : ptypes[k]);
		kn++;
	    }  /* while k */
	    if (misses < missmax) {
		missmax = misses;
		bestindex = i;
	    }
	    if (missmax == 0)
		break;
	}  /* for i */
	if (missmax > 3) {	/* too many misses - no postamble */
	    printf("could not find postamble\n");
	    return (FAIL);
	}
    /* we have a recognizeable postamble */
	bestindex += fmt;
	printf("...postamble found at hdr format %d\n",bestindex);
	p_create_postamble(&post);
	sp->post = post;
	sp->postnum = post->postnum;
      /* get aux data for start format of postamble */
	e1_get_good_aux_data(bestindex,&aux,bestindex+10);
	e1_get_fmt_loc(aux.ifmt,aux.format,sp,&block,&offset);
	sp->fmt_end = aux.ifmt;
	sp->end_id = aux.format;
	sp->blk_end = block;
	get_fmt_time(sp,sp->end_id,&sp->end);
      /* save postamble data */
	strcpy(post->takeid,Cur_Rqst->take_id);
	post->rec_gain = aux.rec_gain;
	post->cal_att = aux.cal_att;
	post->fmt_start = aux.ifmt;
	post->fmt_id = aux.format;
	post->blk_start = block;
	post->bit_off = offset;
	post->polarity = sp->polarity;
	post->repp_dtoff = 0.0;
    /* set up processor to capture the postamble */
	reset_rpi(1);
	reset_edfm(1,1);
	reset_edfm(0,0);
	setup_edfm(dpf.sbm,dpf.fsm,1,dpf.maxfsdo);
	setup_iif_cntrl(0,0,1,2,0x1100,0,0);
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
        if( strcmp( media_type, "DCRSI" ) ) {
	  if (vbose)
	    printf("starting SONY play at %d, offset %d, polarity %d\n",
		block,offset,sp->polarity);
	  sony_start_fmt_play(0,block,offset-152,sp->polarity);
        }
        else {
	  if (vbose)
	    printf("starting DCRSi play at %d, offset %d, polarity %d\n",
		block,offset,sp->polarity);
	  start_fmt_play(0,block,offset,sp->polarity);
        }

    /* arm and trigger */
	e1_clear_hdr_mem(0);
	tarm();
	reset_rpi(0);
	play_dis(0);
	tdone();
    /* at this point, the data should be captured */
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
           sony_stop(IN);
        else
           dc_stop(IN);

    /* see if we got enough data */
	fmt_end = e1_find_last_hdr();
	if (fmt_end < 103) {
	    printf("...lost sync\n");
	    return (FAIL);
	}

    /* measure noise power */
	post->noise_dtoff = e1_noisemeas(2);
    /* measure drift calibration pulses */
	post->calp_dtoff = e1_calmeas(8);
	post->calp_dtoff = sp->pre->calp_dton;

	return (PASS);
}


/* e1_find_dwp_change(sp,fmt_end) --------------------------------------
	This routine locates a data window position change in the range
	of formats contained in the IIF header memory.  fmt_end gives
	the last valid format in header memory.
*/

e1_find_dwp_change(sp,fmt_end)
	TAPE_SEG_PTR sp;
	int fmt_end;
{
	AUX_DATA lo,hi;
	int i,inc,dwp_value,found_dwp_base;

	printf("Checking for data window position change\n");
	e1_get_good_aux_data(0,&lo,fmt_end);
    /* if window table is empty, fill in first entry */
	if (sp->win_count == 0) {
          found_dwp_base = 0;
          dwp_value = 1;
          while ((!found_dwp_base) && (dwp_value <num_dwp_base+1)){
              if ( ((dwp[dwp_value] - lo.window_start) >= 0) &&
                   (((dwp[dwp_value] - lo.window_start) % 22) == 0) ) {
                dwp_base = dwp[dwp_value];
                found_dwp_base = 1;
                printf("dwp_base = %d\n", dwp[dwp_value]);
	    	sp->wfmt[0] = lo.format;
	    	sp->wdwp[0] = lo.window_start;
	    	sp->win_count = 1;
	    	window_old = lo.window_start;
	      }
	      else dwp_value += 1;
	  }
          if ((found_dwp_base == 0) && (dwp_value > num_dwp_base)){
             printf("Can not find the match dwp_base in dwp.base ");
	     printf("file\n");
             printf("lo.window_start = %d\n", lo.window_start);
             return(FAIL);
          }
				
	  /* need to find a valid value for dwp but how? */
	  /* search from first through the last until a  */
	  /* good one is found?                          */
	  /*else printf("...the very first dwp in the segment is bad.\n");*/
	}
	else {
	    if (lo.window_start != window_old) {
		printf("...window position changed at boundary\n");

	       if ( ((dwp_base - lo.window_start) >= 0) && 
	            (((dwp_base - lo.window_start) % 22) == 0) ) {
		  sp->wdwp[sp->win_count] = lo.window_start;
		  window_old = lo.window_start;
		  sp->wfmt[sp->win_count++] = lo.format;
		  printf("...change found at format %d\n",lo.format);
		}
		else { /* the lo.window_start is a bad value */
		  printf("...the boundary dwp is bad: %d\n",
			 lo.window_start);
/*		if (abob()) exit(1);  */
	        }
	    }
	}
    /* see if there is any window change any where else */
	while (fmt_end > 0) {
	    e1_get_good_aux_data(fmt_end,&hi,fmt_end);
	    if (window_old == hi.window_start)
		return;
	    if ( hi.window_start > 1500 || 
	 	 abs(hi.window_start - window_old) >= 50 ||
		 (dwp_base - hi.window_start) < 0 ||
		 ((dwp_base - hi.window_start) % 22) != 0 ) {
		printf("...Invalid window position change = %d at %d\n", 
			hi.window_start, fmt_end);
		fmt_end--;
	    }
	    else break; /* must be a good window change */
	    fflush(stdout);
	}
    /* there is a change, so find it (binary search) */
	if (vbose)
	    printf("...change sensed, searching for it\n");
	sp->wdwp[sp->win_count] = hi.window_start;
	i = inc = 1 << (power_of_2(fmt_end) - 1);
	while (inc) {
	    if (i > fmt_end)
		i = fmt_end;
	    e1_get_good_aux_data(i,&hi,fmt_end);
	    inc /= 2;
	    if (hi.window_start == window_old)
		i += inc;
	    else
		i -= inc;
	}
	window_old = sp->wdwp[sp->win_count];
	if (hi.window_start == window_old)
	    e1_get_good_aux_data(i+1,&hi,fmt_end);
	sp->wfmt[sp->win_count++] = hi.format;
	printf("...change found at format %d\n",hi.format);
}


/* e1_refine_time(sp) --------------------------------------------------
	This routine improves the accuracy of the time associated with
	each input format.  
	    ERS-1 samples its on-board clock only once every 4 formats.
	The clock ticks 256 times per second, or about once every 6.4 
	to 6.7 formats (depending on the PRF).  However, the tick edge 
	is accurate to less than a millisecond.  This routine finds
	three successive clock transitions, and uses their spacing to
	determine the clock tick edge position to less than one format.
	This allows the time for each format to be calculated with
	about 3 times better accuracy.
*/

e1_refine_time(sp)
	TAPE_SEG_PTR sp;
{
	unsigned int time[7],format[7];
	int i,t0,t1,t2,fmt;
	float r,ctr;

    /* get seven new clock samples (every 4th format) */
	for (fmt = 8, i = 0; fmt < 36; fmt += 4) {
	    e1_get_good_aux_data(fmt,&aux,100);
	    time[i] = aux.time;
	    format[i++] = aux.format;
	}
    /* find the transitions: t0 = first transition index */
    /*			     t1 = 1 if 2nd transition is 8 fmts away */
    /*			     t2 = 1 if 3rd transition is 16 fmts away */
	t0 = (time[1] == time[0]) ? 2 : 1;
	t1 = (time[t0] == time[t0 + 1]);
	t2 = (t1 && (time[t0 + 2] == time[t0 + 3]));
    /* r = the number of formats per tick */
	r = sp->pre->prf / 256.0;
    /* calculate the offset to the center of the region containing the
       clock tick edge */
	if (t2)
	    ctr = 10.0 - r;
	else {
	    if (t1)
		ctr = (24.0 - 3.0 * r) / 2.0;
	    else
		ctr = (8.0 - r) / 2.0;
	}
    /* calculate the refined time for one format, and store in global
       variables */
	sp->pre->time_ref_fmt = format[t0] - 8.0 + ctr;
	stime_to_gmt(time[t0],&sp->pre->time_ref_gmt);
	printf("...refine time, satellite time = %d (%x)\n",
		time[t0],time[t0]);
}
