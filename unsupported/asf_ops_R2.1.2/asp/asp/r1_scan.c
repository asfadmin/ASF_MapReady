/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* r1_scan.c -- scan the data take, locating preambles, postambles,
		preprocessing regions and recording gaps.

   CLW 11/9/95 - save Radarsat aux data for each segment into seglist
   CLW 8/25/95   comment out the op_answer
   CLW 8/15/95   initialize postambles to NULL once it is created
		 check for array range for window and agc changes
		 recalculate the actual end block of a segment
   CLW 5/26/95 - Use aspdecl.h instead of aspdecl_temp.h
                 extern int f_dropout & window_old
                 remove cos2 function
		 modify get_gmt to r1_get_gmt 
		 modify for VME interface
		 remove checking for pp_regions0 file
		 remove writing pp_regions0 and make pp_regions1
   CV 3/18/97 -  replace rec to 1 in setup_rdfm() & in start_fmt_play()
*/

#include <stdio.h>
#include <math.h>
#include <procdec.h>
#include <procfil.h>
#include <aspdecl.h>
#include <ephem.h>

#define  FS         (~0x01)
#define  VL         (~0x02)
#define  VF         (~0x04)
#define  AL         (~0x08)
#define  SS         (~0x10)
#define  SYNC_EVENT 0
#define  COUNT_EVENT 1
#define  ID_MINIMUM  10000	/* minimum # of formats to allow tape
  				   id test to be performed */
#define FMT_RATIO 1767.9

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
extern int f_dropout;		/* 1 = force dropout */
extern int window_old;		/* window pos history, -1=first time */
extern RSAT_EPHEM_DATA ephem;   /* ephem data */
/*SONY*/
extern char media_type[];
extern char data_direction[];

extern int Write_CP_Error_Status;

int agc_old = -1;		/* agc pos history -1=first time */
int ifmt_old;
int old_beam_seq;

RSAT_AUX_DATA aux;
static int end_block;		/* ending block # of take */

int bytes_format;		/* # of bytes per format exclude rep */
int bytes_fmt_rep;		/* # of bytes per format include rep */
int first_time;			/* time of first format */
int del_time;			/* delta time between first and sec
				format */
int rec;
int echo_byte, rep_byte;
int VALID_PIXELS;				
bad_preamble;			/* 1 = dropout or bad AUX in proc_preamble */
static int tape_ok = 0;		/* 1 = tape id checks out OK */
GMT last_end;
int try_pre;
int Badpass = 0;

/* r1_scan_take() ------------------------------------------------------
	This routine scans an RSAT formatted data take, breaking it up
	into segments bounded by preambles and recording
	gaps. 
	The tape segment is created, containing information found  
	during this process. 
*/

r1_scan_take()
{
	TAPE_SEG_PTR sp,sp2;
	PP_BLOCK_PTR pp,pp2;
	double get_gmt_diff();
	int start_block, cur_block, block_skip;
	int block,offset,norm_inv,format,iformat,ami_fmt;
	int nblks,segment,ans;
	int nsegs = 0;
	int nregions = 0;
	int out_of_sync = 1;
	int quit = 0 ;
	int gap_type = 0;
	int r_formats, short_segs, NOPREAM;
	char q[100];
	double prf;
	int itime, format_num;
	int first_seg = 1;
	double time_diff;

/* DCRSi or SONY ? */

        int MIN_TAKE_LEN, BYTES_BLOCK, BLOCKSKIP;

        BYTES_BLOCK = TAPE_BLOCK_LEN;  /* 144304 for SONY, 4356 for DCRSi*/
printf("r1_scan.c : BYTES_BLOCK = %d\n", BYTES_BLOCK);
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


    /* clear any previous tape segment */
    	r_formats = 26000;
	short_segs = 0;
	NOPREAM = 0;
	p_clear_seg_list();
	tape_ok = 0;
    /* initialize loop variables */
	start_block = Cur_Rqst->start_blk;
	end_block = Cur_Rqst->end_blk;
	cur_block = start_block;

	printf("processing take from %d to %d\n",start_block,
		end_block);

/* ??? TBF check pass 1 results here. if done, assign r1_cal_pixel, and
	   r1_cal_samp here */

    /* outer loop: tape segment loop */
	if (cur_block > (end_block - MIN_TAKE_LEN)) 
	  printf("The take is too short. Min required is %d.\n",
		MIN_TAKE_LEN);
	while (cur_block < (end_block - MIN_TAKE_LEN)) {
	/* attempt to sync up on the current location */
	/* NOTE: if still in sync, the variables format, cur_block,
		 offset and norm_inv contain values loaded during the
		 previous iteration of this loop */
	    start_block = cur_block; 

	    while (out_of_sync && cur_block < end_block - MIN_TAKE_LEN) {
		/*nblks = (gap_type == 2) ? 20 : 10;*/
/*SONY*/
                if (!strcmp( media_type, "DCRSI" ) )
                  nblks = 20;
                else
                  nblks = 3;

		if (r_formats < ID_MINIMUM && ++short_segs > 2) {
		   cur_block += BLOCKSKIP;
		   short_segs = 0;
		}
		if (r1_srch_sync(3,2,nblks,&cur_block,&offset,
			&norm_inv,&itime,&prf) == PASS) {
		    printf("found sync at block = %d offset %d\n", 
				cur_block, offset);
		    out_of_sync = 0;
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

	     }  /* while out_of_sync */

	/* if out of sync, the take is done */
	     if (out_of_sync)
		break;

	     if (NOPREAM == 0 && r_formats > 10000) {
	/* create a new tape segment block if last segment was not
	   a short one (<10000 formats) or if a no-preamble 
	   situation has occured. */ 
	        if (vbose)
		    printf("creating tape segment block\n");
	        if ((sp = (TAPE_SEG_PTR) malloc(sizeof(TAPE_SEG))) == NULL) {
			printf("Out of memory in preproc pass 1\n");
			printf ("ERROR creating tape segment block\n");	
			exit (-1);
			/* return (ABORT); */
	        }
	    	if (seg_list == NULL) {
			seg_list = sp;
			sp->prenum = 0;
			sp->postnum = 0;
			sp->pre = NULL;
			sp->post = NULL; /* added back by clw */
	        }
	        else {
			sp2->nxt_seg = sp;
			*sp = *sp2;
                        Badpass = 1;
	        }
    	     } /* end if NOPREAM == 0  */

	/* start the format counter */
	     if (first_seg) {
		format = 0;
		first_seg = 0;
	     }
	     else
	         format = r1_get_fmt(itime);
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
	     while (try_pre < 3) {
		/* ??? need VALID_PIXEL befor proc_preamble */
	    	ans = r1_proc_preamble(sp,block,offset);
		if (ans == PASS) {
                        try_pre = 0;
                        break;
		}
		printf("PREAMBLE:fail number %d in proc_preamble\n", 
			++try_pre);
                if (ans == FAIL) {
                  if (sp->fmt_id == -1) {
                    printf("Cannot locate preamble - Dropout or Bad ");
                    printf("AUX\n");
                    out_of_sync = 1;
                    block += 10000;
/*
                    block += block_skip;
*/
	            cur_block = block;
                    if (Badpass == 0) {
                      p_clear_seg_list();
                      first_seg = 1;
                    }

                    break;
                  }
		  else {
		    printf("fatal error occured in preamble!\n");
		    return(FAIL);
		  }
	        }
		r1_get_fmt_loc(0,sp,sp->fmt_start+10000,&block,&offset);
		sp->fmt_start += 10000;
		sp->blk_start = block;
		sp->bit_off = offset;
	     

	        cur_block = block;

          } /*try_pre < 3*/

	/* scan the take, locating a sync loss, postamble, or the
	   end of the take */
	     while (out_of_sync == 0) {
	    	p_rw_pream_file(1,"preambles");
	    	if (r1_scan_segment(sp,&cur_block,&offset) == ABORT) {
		   printf ("Error in r1_scan_segment\n");
		   /*exit (-1); */
		    return (FAIL);
		}
	    	if (vbose)
		   printf("...scan done, cur_block = %d\n",cur_block);
	    	gap_type = sp->gap_type;
	    	out_of_sync = 1;
	    	if (vbose)
	      	   printf("end of scan_take loop, cur_block=%d\n",cur_block);
	    	sp2 = sp;
	        sp->aux = aux;
	     } /* while out_of_sync == 0 */
	}  /* while cur_block */
	return(ans);
}


/* r1_proc_preamble(sp,iblock,ioffset) ---------------------------
	This routine attempts to process a preamble at the format
	location given in the tape segment sp.  If that location is
	the start of a preamble, the preamble processing is performed,
	a preamble record is created, and the routine returns PASS.
	If not, the format number of the first format is loaded into
	sp, and the routine returns FAIL.
*/

r1_proc_preamble(sp,iblock,ioffset)
	TAPE_SEG_PTR sp;
	int iblock,ioffset;
{
#define NPFMTS 300
	PREAM_FILE_PTR pre;
	int sync_code,itime;
	int i,j,k;
	int block,offset, first_format;
	int doflg,biterr,frmnum,fmtnum,pfmts;
	int fracount,frasynerr,badfracnt,docnt,forcnt;
	int crcerrcnt,fratoterr,bitslip;
	double time_diff;
	double get_gmt_diff();
	int r1_get_fmt();
	double prf_period;
	int format, fmt_time;

	printf("Attempting to locate preamble\n");
    /* set up processor to capture the preamble */
	reset_rpi(1);
	reset_edfm(1,1);
	reset_edfm(0,0);
	/* CV replace rec to 1: data was reversed even recorded dump 3/18/97 */
	setup_rdfm(0x111,1,0,0,1,0,1,0,0,dpf.sbm,dpf.sbm,dpf.maxfsdo);
	setup_rdfm_scs(0,rep_byte,echo_byte,0,0,0,0,0,0);
	setup_iif_cntrl(0,0,1,2,0x1100,0,0);
	setup_iif_par(VALID_PIXELS,NPFMTS,1,NPFMTS,0,0,0,1,13,0);
	ex_set_selftest(0);
	ex_set_delay(8192 * 2 * NPFMTS);
	sync_code = FS & VF & AL & VL;
	/*
	sync_code = SS & FS & VF & AL & VL;
	ex_set_event(SYNC_EVENT,0x1f,sync_code,1);
	*/
	ex_set_event(SYNC_EVENT,0x0f,sync_code,1);
	ex_set_reset(0);
	play_dis(1);
	iif_set_reset(1);
	iif_set_reset(0);
	block = iblock;
	offset = ioffset;
    /* start the DCRS at the right location 
	if (vbose)*/
/*SONY*/
            if (!strcmp( media_type, "DCRSI" ) ) {
              printf("starting DCRSi play at %d, offset %d, polarity %d\n",
                block,offset,sp->polarity);
	      start_fmt_play(2,block,offset-32,1); /* CV 3/18/97 replace rec to 1 */
            }
            else {
                printf("starting SONY play at %d, offset %d, polarity %d\n",
                  block,offset,sp->polarity);
                sony_start_fmt_play(2,block,offset-184,1);
                 }
/* 	if (abob()) exit(1); */
    /* arm and trigger */
	r1_clear_hdr_mem(0);
/*if (abob()) exit(1);*/
	tarm();
	reset_rpi(0);
	play_dis(0);
	tdone();
    /* at this point, the data should be captured */
    /* see if we're still in sync */

	get_rdfm_status(&doflg,&fracount,&frasynerr,&badfracnt,&docnt,&forcnt,&crcerrcnt,&fratoterr,&bitslip); 

/*SONY*/
       if( strcmp( media_type, "DCRSI" ) )
          sony_stop(IN);
       else
          dc_stop(IN);

	if (doflg) {
		/* not to try a second time */
	    printf (" lost sync \n");
	    sp->fmt_id = -1;
	    return (FAIL);
	}

        if (r1_cal_samp(0,&aux,sp,&prf_period,&fmt_time)==FAIL){
	   printf("FAIL in r1_cal_samp\n");
	   sp->fmt_id = -1;
	   return (FAIL);
	}
        if (vbose) printf ("fmt_time = %x; ", fmt_time);
        if (sp->fmt_start != 0){
           format = r1_get_fmt(fmt_time);
           sp->fmt_start = format;
           printf("format = %d\n", format);
        }
        if (r1_get_ephem(0) == FAIL)
                printf("Error in getting ephem data\n");
	bad_preamble = 0;
	sp->blk_start = block;
	sp->bit_off = offset;
	sp->fmt_id = 0; 
	sp->end_id = 0; 
	r1_get_gmt(&aux,&sp->start);

	printf ("creating preamble data block\n");
	p_create_preamble(&pre);
	sp->pre = pre;
	sp->prenum = pre->prenum;
	sp->postnum = 0;
     /* save preamble data */
	strcpy(pre->sat,"RSAT");
	strcpy(pre->dcrs_id,Cur_Rqst->tape_id);
	strcpy(pre->takeid,Cur_Rqst->take_id);
	strcpy(pre->sen_mode,"NONE");
	pre->prf = prf_period;
	pre->fmt_start = sp->fmt_start;
	pre->rec_gain = 0; 
	pre->cal_att = aux.CALN_att;
        pre->noise_dton = 0.0;
        pre->calp_dton = 0.0;
        pre->repp_dton = 0.0;
	pre->fmt_id = sp->fmt_id;
	pre->blk_start = sp->blk_start;
	pre->bit_off = sp->bit_off;
	pre->polarity = sp->polarity;
	pre->time_ref_fmt = sp->fmt_start;
	r1_get_gmt(&aux,&sp->pre->time_ref_gmt);

       /* if (r1_get_ephem(0) == FAIL)
                printf("Error in getting ephem data\n");
     compare start time of seg with state vector time 
	time_diff = get_gmt_diff(&sp->start,&sv1.gmt);
	if (time_diff < 0.0) {
	   printf("start time less than state vector time\n");
	   sp->fmt_id = -2;
	   return (FAIL);
	}
	else{
	  if (time_diff > 60.0) {
	     printf("start time more than 1 min after end time\n");
	     sp->fmt_id = -2;
	     return (FAIL);
	  }
	}*/
	return (PASS);
}


/* r1_scan_segment(sp,block,offset) -----------------------------
	This routine scans the given tape segment from start to end,
	looking for loss of sync. The signal end of segment, the
	location and reason are returned.
	    Also, the routine checks for data window position changes,
	and records any found in the global table dwp.
*/

r1_scan_segment(sp,block,offset)
	TAPE_SEG_PTR sp;
	int *block,*offset;
{
#define NFMTS 22000 /* Ming change, because not enough header memory */


	short int fmtsav[104 * 10];
	int not_at_end,gap_type,blkno,ifmt,type,sync_code,fmt_end;
	int i,j,k,kn,pid,head_start,ans;
	int doflg,biterr,frmnum,fmtnum,nfmts,nframes,tfmts;
	int fracount,frasynerr,badfracnt,docnt,forcnt;
	int crcerrcnt,fratoterr,bitslip;
	int first = 1;
	int blkno_old, total_fmt;
	int first_format,last_format;
	int itime, seg, valid_fmt;

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

	printf("Performing pass 1 scan\n");
    /* set up processor to scan the segment */
	reset_rpi(1);
	reset_edfm(1,1);
	reset_edfm(0,0);
	/* if (abob()) exit(1); */
	/* CV replace rec to 1: data was reversed even recorded dump 3/18/97 */
	setup_rdfm(0x111,1,0,0,1,0,1,1,0,dpf.sbm,dpf.sbm,dpf.maxfsdo);
	setup_rdfm_scs(0,rep_byte,echo_byte,0,0,0,0,0,0);
	/* if (abob()) exit(1); */
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
/*SONY*/
          if (!strcmp( media_type, "DCRSI" ) ){
            if (vbose)
              printf("starting DCRSi play at %d, offset %d, polarity %d\n",
                *block,*offset,sp->polarity);
	    start_fmt_play(2,*block,*offset-32,1); /* CV 3/18/97 replace rec to 1 */
           }
           else {
                if (vbose)
                  printf("starting SONY play at %d, offset %d, polarity %d\n",
                    *block,*offset,sp->polarity);
                sony_start_fmt_play(2,*block,*offset-184,1);

                }


    /* top of scan loop */
	not_at_end = 1;
	total_fmt = 0;
	seg = 0;
	blkno = *block;
	while (not_at_end) {
	    if (vbose)
		printf("scanning segment, block %d...\n",blkno);
	    iif_set_reset(1);
	    iif_set_reset(0);
	    r1_clear_hdr_mem(1);
/* if (abob()) exit(1); */
	/* arm and trigger */
	    tarm();
	    if (first) {
		/*first = 0;*/
		reset_rpi(0);
		head_start = 20;
	    }
	    else {
		j = (MLOC_IIF_HEAD >> 1) - (104 * 10);
		for (i = 0; i < 104 * 10; i++)
		    mb.w[j + i] = fmtsav[i];
		i = (PID_IIF_HEAD<<20) | (j<<1);
printf("i %d j %d\n",i,j);
/*
		asp_write( i, &mb.w[j], (104*10)<<1 );
*/
		head_start = -104;
	    }
	    play_dis(0);
	    tddone();
	    play_dis(1);
	/* at this point, the data should be captured */
	    blkno_old = blkno;
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) ) {
/*
            blkno = sony_get_addr(IN);
*/
        }
        else
            blkno = dc_get_addr(IN);

	    fmt_end = r1_find_last_hdr();
	    if (vbose)
		printf("...%d formats scanned\n",fmt_end+1);
	    gap_type = 0;
	    if (first) {
	        total_fmt = total_fmt + fmt_end;
		first = 0;
	    } else total_fmt = total_fmt + fmt_end + 1;
	    seg = seg + 1;
	/* display start and end format numbers */
	    if (r1_get_aux_data(0,&aux) == FAIL) {
		printf("bad aux data in scan!\n");
		sp->gap_type = 2;
		first_format += fmt_end;
		sp->blk_end = blkno_old;
		return (PASS);
	    }
	    else { 
		first_format = 0;
	    	/*first_format = r1_get_fmt(itime);*/
	    }
	    if (vbose) {
		printf("   first format is %d\n",first_format);
	    }
	    r1_get_aux_data(fmt_end,&aux);
	    last_format = fmt_end;
	   /* last_format = r1_get_fmt(itime);*/
	    if (vbose)
		printf("    last format is %d\n",last_format);
	    printf("total_fmt = %d, seg= %d\n", total_fmt, seg);
	    
	    if (blkno == 0) {
		printf(" DCRSi output block 0, change it! ");
		/* ??? TBF */
		blkno = blkno_old + NFMTS*bytes_format/BYTES_BLOCK;
	    }

	/* see if we're still in sync */
	    get_rdfm_status(&doflg,&fracount,&frasynerr,&badfracnt,&docnt,&forcnt,&crcerrcnt,&fratoterr,&bitslip); 
	    if (f_dropout == 1) {
		printf("force dropout activated!\n");
		doflg = 1;
		f_dropout = 0;
	    }
	    printf("dropout(doflg) = %d, forcnt = %d\n", doflg, forcnt);
	    if (doflg || (tfmts - fmt_end > (tfmts / 10))) {
		printf("lost sync at or before block %d\n",blkno);
		printf("doflg = %d, fmt_end = %d\n", doflg, fmt_end);
		/* blkno = blkno_old; */
		not_at_end = 0;
	    }
/* if (abob()) exit(1); */
	    blkno_old = blkno;
	/* check for data window position change */
	    if (r1_find_agc_dwp_change(sp,fmt_end,&total_fmt,&valid_fmt)==
		ABORT){
		fmt_end = valid_fmt;
		if (vbose) printf("valid_fmt=%d,fmt_end=%d\n",valid_fmt,fmt_end);
		not_at_end = 0;
		total_fmt = total_fmt - (fmt_end-valid_fmt);
		if (vbose) printf("total_fmt=%d\n",total_fmt);
	    }
/*if (abob()) exit(1); */
	/* check for end of segment */
	    if (gap_type == 2 || not_at_end == 0) {
		sp->gap_type = 2;
                sp->fmt_end = total_fmt + sp->fmt_start;
		r1_get_aux_data(fmt_end,&aux);
		printf("aux.days=%d\n",aux.time.days);
		r1_get_gmt(&aux,&sp->end);
		last_end = sp->end;
		*block = blkno;
		sp->blk_end = *block;

/* Recalculate actually end blk of the segment, clw */
		sp->blk_end = blkno_old + fmt_end*bytes_format/BYTES_BLOCK;
		*block = sp->blk_end; /* returned for scanning of next seg */

/*SONY, for scan job*/
        if( strcmp( media_type, "DCRSI" ) ) {
            blkno = sony_get_addr(IN);
                if (blkno == -1) blkno = sony_get_addr(IN);
                if (blkno == -1) {
                  return(FAIL);
                }     
		*block = blkno;
		sp->blk_end = *block;
        }
/*SONY*/
                if( strcmp( media_type, "DCRSI" ) )
                   sony_stop(IN);
                else
                   dc_stop(IN);

		return (PASS);
	    }
	/* save tail end of this scan -- postamble may start there */
	    i = (sp->fmt_end + 1) * 20 + MLOC_IIF_HEAD; /* ??? */
	    pid = mb.w[RLOC_REP];
	    mb.w[RLOC_REP] = PID_IIF_HEAD;
	    mb.w[RLOC_REP] = pid;
	}  /* while not_at_end */
	printf("r1_scan_segment could not find end of take\n");
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
           sony_stop(IN);
        else
           dc_stop(IN);

	return (FAIL);
}

/* r1_find_agc_dwp_change(sp,fmt_end,total_fmt)--------------------------
	This routine locates a window and AGC position change in the range
	of formats contained in the IIF header memory.  fmt_end gives
	the last valid format in header memory.

	NOTE: the max of dwp array is 20 and agc array is 80. Need to       
	expand more if real data has a lot dwp and agc change. Test RSAT
	data shows there are ~1300 agc change.
*/

r1_find_agc_dwp_change(sp,fmt_end,total_fmt,valid_fmt)
	TAPE_SEG_PTR sp;
	int fmt_end, *total_fmt, *valid_fmt;
{
	RSAT_AUX_DATA lo,hi;
	int i,j,k,fmt_no;
	int ifmt, count_fmt,n_diff_fmt;
	int agc_change[1000], win_change[100];
	int inc, no_agc_per_seg = 0;

	*valid_fmt = 0;
	printf("Checking for data window and AGC change\n");
	r1_get_aux_data(0,&lo);

    /* if window table is empty, fill in first entry */
	j = 0;
	k = 0;
	if (sp->agc_count == 0){
            sp->afmt[0] = *total_fmt - fmt_end + sp->fmt_start;
	    sp->agc[0] = lo.AGC_set;
	    sp->agc_count = 1;
	    agc_old= lo.AGC_set;
	    if (vbose) printf("Ming first, j=%d, fmt=%d\n",j, sp->afmt[j]);

            sp->wfmt[0] = *total_fmt - fmt_end + sp->fmt_start;
	    sp->wdwp[0] = lo.window_start;
	    sp->win_count = 1;
	    ifmt_old = 0;
	    window_old= lo.window_start;
	    no_agc_per_seg += 1;

	    old_beam_seq = lo.beam_seq;
	    if (vbose) printf("old_beam_seq=%d\n",old_beam_seq);
	}
	else {
	    if (lo.AGC_set != agc_old) {
		printf("...AGC position changed at boundary ");
		if ( sp->agc_count < 80 ) { /* clw */
		  sp->agc[sp->agc_count] = lo.AGC_set;
		  agc_old = lo.AGC_set;
                  ifmt = *total_fmt - fmt_end + sp->fmt_start;
		  sp->afmt[sp->agc_count++] = ifmt; 
	    	  no_agc_per_seg += 1;
		  printf("at fmt=%d, agc=%d\n",ifmt,lo.AGC_set);
		}
	    }
	    if (lo.window_start != window_old)
	    {
	  	printf("...Window position changed at boundary, ");
		if ( sp->win_count < 20 ) { /* clw */
	   	  sp->wdwp[sp->win_count] = lo.window_start;
	   	  window_old = lo.window_start;
                  ifmt = *total_fmt - fmt_end + sp->fmt_start;
	   	  sp->wfmt[sp->win_count++] = ifmt; 
		  printf("at fmt=%d, win.start=%d\n",ifmt,
			window_old);
		}
	    }
	}

/* Get the first two AGC change per 22k format*/
	count_fmt = fmt_end - 1;
        fmt_no = fmt_end - count_fmt;
        while ((fmt_no > 0) && (fmt_no <= fmt_end) &&
               (no_agc_per_seg < 3) && (sp->agc_count < 81)) {
	   if(r1_get_aux_data(fmt_no, &hi)== FAIL){
		fmt_no = fmt_no +1;
		printf("fail in r1_get_aux_data in fmt %d\n",fmt_no);
	   } else {
	  	if (sp->agc_count < 80) {
		    if (hi.AGC_set != agc_old){
                        sp->agc[sp->agc_count]= hi.AGC_set;
			ifmt=fmt_no+(*total_fmt-fmt_end)+sp->fmt_start;
                        sp->afmt[sp->agc_count++] = ifmt;
			agc_old = hi.AGC_set;
			no_agc_per_seg += 1;
	      		printf("AGC change at fmt=%d,AGC_set=%d\n", 
			     fmt_no, hi.AGC_set);   
		    }	
		 } /* if agc_count */
		 fmt_no = fmt_no +1;
	    } /* if r1_get_aux_data */
	} /* while */

	k = fmt_end;
	if (r1_get_aux_data(k,&hi) == PASS) { 
	   if (hi.beam_seq == old_beam_seq){
	      if (hi.window_start == window_old)
		 return;
	   } else {
	      printf("beam_seq=%d,old_beam=%d,fmt_end=%d\n",
			hi.beam_seq,old_beam_seq,fmt_end);
              i = inc = 1 << (power_of_2(fmt_end) - 1);
              while (inc) {
       	      	if (i > fmt_end)
      	       		i = fmt_end;
       	      	if (r1_get_aux_data(i,&hi)== FAIL){
			printf("sync code is wrong\n");
			r1_get_aux_data(i+1,&hi);
	      	}
       	      	inc /= 2;
	      	if (vbose) printf("inc=%d, i=%d, ",inc,i);
       	      	if (hi.beam_seq == old_beam_seq)
               		i += inc;
       	      	else
       	       		i -= inc;
             }
	     if (hi.beam_seq == old_beam_seq) *valid_fmt = i;
	     else *valid_fmt = i-1;
	     if (vbose) printf("fmt=%d,hi.b_seq=%d\n",i,hi.beam_seq);
	     return(-2);	
	   }
	}
	
        if(vbose) {
	   printf(" There is wdwp change at last fmt %d, window_start %d\n",
			k, hi.window_start);
           printf("...change sensed, searching for it\n");
        }
        sp->wdwp[sp->win_count] = hi.window_start;
        i = inc = 1 << (power_of_2(fmt_end) - 1);
        while (inc) {
       	   if (i > fmt_end)
      	       i = fmt_end;
       	   if (r1_get_aux_data(i,&hi)== FAIL){
		printf("sync code is wrong\n");
		r1_get_aux_data(i+1,&hi);
	   }
       	   inc /= 2;
	   if (vbose) printf("inc=%d, i=%d, ",inc,i);
       	   if (hi.window_start == window_old)
               i += inc;
       	   else
       	       i -= inc;
        } 
        window_old = sp->wdwp[sp->win_count];
        if (vbose)printf("hi.win_start=%d, win_old=%d  ",
			hi.window_start,window_old);
      	if (hi.window_start == window_old){
	   r1_get_aux_data(i-1,&hi);
	   if (hi.window_start == window_old){
  	   	ifmt = i-1 + (*total_fmt-fmt_end)+sp->fmt_start;
           	printf("....change found at fmt %d\n",i-1);
	   } else {
  	   	ifmt = i + (*total_fmt-fmt_end)+sp->fmt_start;
           	printf("......change found at fmt %d\n",i);
	   }
	} else {
           r1_get_aux_data(i+1,&hi);
  	   ifmt = i+1 + (*total_fmt-fmt_end)+sp->fmt_start;
           printf("...change found at fmt %d\n",i+1);
	}
        sp->wfmt[sp->win_count++] = ifmt;
}



/* r1_make_pprs(sp) ----------------------------------------------------
	This routine creates preprocessing data blocks for each
	preprocessing region required in the given segment.
*/

r1_make_pprs(sp)
	TAPE_SEG_PTR sp;
{
	PP_BLOCK_PTR pp,pp2;
	int w,format,dformats,dformats2,savefmt,dmin,istat;
	double time,dtime,prf;
	double get_gmt_diff();
	int count = 0;
	int end_sw = 0;
	int first = 1;
	int last = 0;
	int first_format, last_format;
	GMT start_gmt;
	double a[3],b[3],c[3],d[3],e[3];
	double r3p[3];
	float fd3p[3],fr3p[3];
	int tfmts;
	int block, offset;

	prf = sp->pre->prf;
	rec = sp->polarity;
	if (vbose)
	    printf("prf=%g\n",prf);
	if ((sp->fmt_end - sp->fmt_start) < 26000) {
	    printf("...not enough data to process\n");
	    return (FAIL);
	}

	format = sp->fmt_start + 12;
	printf("format = %d\n", format);
	while (format < sp->fmt_end) {
	    if ((pp = (PP_BLOCK_PTR) malloc(sizeof(PP_BLOCK))) == NULL) {
		printf("Out of memory in r1_make_pprs\n");
		return (FAIL);
	    }
	    p_init_pp_block(pp);
	    if (sp->ppb_list == NULL)
		sp->ppb_list = pp;
	    else
		pp2->nxt_ppb = pp;
	    pp2 = pp;
	    pp->nxt_ppb = NULL;
	    pp->fmt_start = format;
	    r1_get_fmt_loc(0,sp,format,&pp->blk_start,&pp->bit_off);
	    pp->polarity = sp->polarity;
	    pp->quality = -1;
	/* calculate the first and last format within a region */
	    first_format = format - 8*prf;
	    last_format  = format + 12*prf;
	    if (first) {
	       first_format = sp->fmt_start;
	       last_format  = first_format + 20*prf;
	    }
	    if (last) {
	       last_format  = sp->fmt_end;
	       first_format = last_format - 20*prf;
	    }
	if (vbose) printf("first format = %d, last format = %d\n", 
		  first_format, last_format);
	/* get first window position */
	    for (w = 0; (w < sp->win_count-1) && 
		  (first_format > sp->wfmt[w+1]); w++)
		;
	     printf("window position - w = %d, sp->wdwp[w] = %d\n",
			     w,sp->wdwp[w]);
	    if (vbose)
	      printf("window position - w = %d, sp->wdwp[w] = %d\n",
			     w,sp->wdwp[w]);
	    pp->dly1 = sp->wdwp[w];
	    pp->dly2 = sp->wdwp[w];
	    pp->dly_chg = 10000000;
	/* get window change, if any */
	    if ((++w < sp->win_count) && (last_format >= sp->wfmt[w])) {
		pp->dly2 = sp->wdwp[w];
		pp->dly_chg = sp->wfmt[w];
	    	printf("window change - wdwp = %d, wfmt = %d\n",
		         sp->wdwp[w], sp->wfmt[w]);
	        if (vbose)
	    	    printf("window change - wdwp = %d, wfmt = %d\n",
		         sp->wdwp[w], sp->wfmt[w]);
	    }
	/* propagate statevector to this region start */
	    if (vbose)
		printf("...propagating state vector\n");
	    get_fmt_time(sp,format,&pp->sv.gmt);
	    time = get_gmt_diff(&pp->sv.gmt,&sv1.gmt);
	    printf ("time = %g\n", time);
	    if (stv_prop(time,pp)==FAIL) return(FAIL);
	    printf("pass stv_prop\n");

	/* calculate spacecraft attitude */
	    get_attitude(&pp->sv,&pp->att);
	    printf("pass get_attitude\n");
	    get_range(sp,pp);
	    printf(" pass get_range\n");


	/* get initial doppler parameters */
	    istat = 0;
	    printf("before call get_dops subroutine\n");
	    if (get_dops(sp,pp) != PASS){
		printf("FAIL in get_dops\n");
		return (FAIL);
	    } else printf("pass get_dops\n");
	    if (vbose)
		printf("...swath speed = %g\n",pp->swth_speed);

	if (first) {
	    first = 0;
	    if ((format = r1_locate_ppr(sp,pp)) == FAIL)
		return (FAIL);
	    if (format < sp->fmt_start || format > (sp->fmt_end - 2056)) {
		printf("...image not within this segment\n");
		sp->ppb_list = NULL;
		return (PASS);
		}
	/* if not doing standard processing, use format number
	   to determine the correct data window position     */
	/* in all cases go ahead and determine correct dwp -dtc */
	/*    if (strcmp(Cur_Rqst->type,"STD") != 0   		*/
	/*      && strcmp(Cur_Rqst->type,"QLK") != 0) {		
	      first_format = format - 8*prf;
	      last_format  = format + 12*prf;*/

	      first_format = format;
	      last_format = format + 29000;

	/* get first window position */
	      for (w = 0; (w < sp->win_count-1) && 
		  (first_format > sp->wfmt[w+1]); w++)
		;
	      if (vbose)
	        printf("window position - w = %d, sp->wdwp[w] = %d\n",
			     w,sp->wdwp[w]);
	      pp->dly1 = sp->wdwp[w];
	      pp->dly2 = sp->wdwp[w];
	      pp->dly_chg = 10000000;
	/* get window change, if any */
	      if ((++w < sp->win_count) && (last_format >= sp->wfmt[w])) {
		pp->dly2 = sp->wdwp[w];
		pp->dly_chg = sp->wfmt[w];
	        if (vbose)
	  	  printf("window change - wdwp = %d, wfmt = %d\n",
		     sp->wdwp[w], sp->wfmt[w]);
	      }

	/* FROM JERS, re-sync the starting location if not STD or QLK job */
	    if (strcmp(Cur_Rqst->type,"STD") != 0 &&
		    strcmp(Cur_Rqst->type,"QLK") != 0) {
		    pp->fmt_start = format;
		    r1_get_fmt_loc(0,sp,format,&block,&offset);
		   pp->blk_start = block;
												   pp->bit_off = offset;
											   }
	/* FROM ERS update the region location 
	    pp->fmt_start = format;
	    r1_get_fmt_loc(0,sp,format,&pp->blk_start,&pp->bit_off);*/


	/* propagate statevector to this region start */
	    get_fmt_time(sp,format,&pp->sv.gmt);
	    time = get_gmt_diff(&pp->sv.gmt,&sv1.gmt);
	    printf("second time %g\n", time);
	    if (stv_prop(time,pp)==FAIL) return(FAIL);
	/* calculate spacecraft attitude */
	    get_attitude(&pp->sv,&pp->att);
	    get_range(sp,pp);


	/* get initial doppler parameters */
	    istat = 0;
	    if (get_dops(sp,pp) != PASS)
		return (FAIL);
	  } /* end if first */

    	/* call corn_loc with estimated start time, to get
           rough lat and long at scene center */
	    start_gmt = pp->sv.gmt;
	    add_seconds(&start_gmt, 3.0);
	    add_seconds(&start_gmt, -51200.0/pp->swth_speed);
	    tfmts = (102400.0 / pp->swth_speed) * prf;
	    istat = 0;
	    r3p[0] = pp->r_close;
	    r3p[1] = pp->r_mid;
	    r3p[2] = pp->r_far;
	    fd3p[0] = 0;
	    fd3p[1] = 0;
	    fd3p[2] = 0;
	    fr3p[0] = -2200.0;
	    fr3p[1] = -2200.0;
	    fr3p[2] = -2200.0;
	    get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,0,0.0,
		         a,b,c,d,e,&istat);
	    pp->lat_rough = c[0];
	    pp->lon_rough = c[1];

	    count++;
	    printf("...creating region %d at format %d\n",count,format);
	    if (vbose)
		printf("...creating region %d at format %d\n",count,format);
	/* if not doing standard processing, only make 1 ppr */
	    if (strcmp(Cur_Rqst->type,"STD") != 0
	    	&& strcmp(Cur_Rqst->type,"QLK") != 0)
		break;
	/* calculate location of next preprocessing region */
	    dtime = 89200.0 / pp->swth_speed;
	    dformats = (int) (dtime * prf + 0.5);
	    dformats2 = dformats / 2;
	    format += dformats;
	  /* check for near end of segment */
	    if (format > sp->fmt_end - (dformats2 + 2056))
		end_sw++;
	    switch (end_sw) {
		case 0:		    /* all but last region */
		    break;
		case 1:		    /* last region */
		    format = sp->fmt_end - (dformats2 + 2056);
		    last = 1;
		    break;
		default:	    /* beyond end of take */
		    format += 1000000;
		    break;
	    }
	}  /* while */
	printf("...created %d regions\n",count);
	return (PASS);
}


/* r1_locate_ppr(sp,pp) ------------------------------------------------
	This routine calculates the location of the first preprocessing
	region.
*/

r1_locate_ppr(sp,pp)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
{
	GMT start_gmt;
	double latr,lonr,lata,lona,latd,lond,r_e;
	double a[3],b[3],c[3],d[3],e[3];
	double r3p[3];
	double time,distance,deg_to_rad;
	double cosrd,cosra,cosda,sinra,sinda;
	double pi = 3.141592653589793;
	double prf;
	float fd3p[3],fr3p[3];
	int format,istat,tfmts,nfmts,maxfmt;
	/*char *malloc();*/
	double cos2();

	deg_to_rad = pi / 180.0;
	format = sp->fmt_start + 12;
    /* if standard request, start at front of take */
	if (strcmp(Cur_Rqst->type,"STD") == 0) {
	    format += (int) ((44800.0 / pp->swth_speed) 
				* sp->pre->prf + 0.5);
	    return (format);
	}
    /* if quick look request, ask user to select area of interest */
    /*   The question has already been asked and need not be
	 asked again. (done in select_segment call           */
	if (strcmp(Cur_Rqst->type,"QLK") == 0) {
	    format += (int) ((44800.0 / pp->swth_speed) 
				* sp->pre->prf + 0.5);
	    return (format);
	}
    /* if time was given, use it to calculate start */
	prf = sp->pre->prf;
	if (Cur_Rqst->targ.yr) {
	if (vbose) printf("use time for format start: year= %d\n",
		Cur_Rqst->targ.yr);
	    get_time_fmt(sp,&Cur_Rqst->targ,&format);
	    if (vbose) printf("passed get_time_fmt, format=%d\n",
		&format);
	    return (format);
	}

    /* call corn_loc with estimated start time, to get
       earth radius at scene center */
       if(vbose) 
	printf("by lat and long: swathspeed=%g\n",pp->swth_speed);
	start_gmt = pp->sv.gmt;
	time = 400000.0 / pp->swth_speed;
	tfmts = time * prf;
	istat = 0;
	r3p[0] = pp->r_close;
	r3p[1] = pp->r_mid;
	r3p[2] = pp->r_far;
	fd3p[0] = 0;
	fd3p[1] = 0;
	fd3p[2] = 0;
	fr3p[0] = -2200.0;
	fr3p[1] = -2200.0;
	fr3p[2] = -2200.0;
	get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,0,0.0,
		     a,b,c,d,e,&istat);
    /* calculate the track distance to the new lat/lon */
	latr = deg_to_rad * Cur_Rqst->lat;
	lonr = deg_to_rad * Cur_Rqst->lon;
	lata = deg_to_rad * a[0];
	lona = deg_to_rad * a[1];
	latd = deg_to_rad * d[0];
	lond = deg_to_rad * d[1];
	r_e = c[2];
	cosra = cos2(latr,lonr,lata,lona);
	cosrd = cos2(latr,lonr,latd,lond);
	cosda = cos2(latd,lond,lata,lona);
	sinra = sin(acos(cosra));
	sinda = sin(acos(cosda));
	distance = r_e * acos(cosra) * (cosrd - cosra * cosda)
				     / (sinra * sinda);
	if (distance > 0.0) 
	   printf("...distance is negative =%g",distance);
    /* calculate the format number of the requested location */
	time = distance / pp->swth_speed;
	nfmts = time * prf;
	format += nfmts;
	/* add this line CV */
	format -= (int) ((51200.0 / pp->swth_speed)
				*sp->pre->prf + 0.5);
    /* remove the test for going beyond end of datatake
	maxfmt = sp->fmt_end;
	maxfmt -= 2056;
	if (format > maxfmt)
	    format = maxfmt;
    */
	return (format);
}



/* r1_get_fmt.c -- This routine find the format number by using time in
		aux data 

*/

int r1_get_fmt(itime)
	int itime;
{
	int fmt;

	fmt = (itime - first_time) / del_time;
	return(fmt);
}

/* r1_get_fmt_loc.c -- This routine finds the position (block, offset)
		       of input format.	
*/


r1_get_fmt_loc(f_dcrsi,sp,fmt,block,offset)
	TAPE_SEG_PTR sp;
	int f_dcrsi, fmt, *block, *offset;
{
	int rec1, norm_inv, itime;
	double total_bit, m_bytes, total_bytes, prf;
	
/* DCRSi or SONY ? */

        int MIN_TAKE_LEN, BYTES_BLOCK, BLOCKSKIP;

        BYTES_BLOCK = TAPE_BLOCK_LEN;  /* 144304 for SONY, 4356 for DCRSi*/
printf("r1_get_fmt_locat:BYTES_BLOCK=%d\n",BYTES_BLOCK);
        if( strcmp( media_type, "DCRSI" ) ) {
           MIN_TAKE_LEN = 50000/33;
           BLOCKSKIP = 10000/33;
        }
        else {
                MIN_TAKE_LEN = 50000;
                BLOCKSKIP = 10000;
             }

	rec1 = sp->polarity;
	if (rec1 == 1 ) m_bytes = 13125105; /* change from 13125000 */ 
	else {
	     if (rec1 == 0) m_bytes = 10625025;/* CV for MC 6/17/97 */
	     else printf("r1_get_fmt_loc: incorrect rec=%d\n",rec);
	}
	if (vbose) {
	     printf("rec=%d, mbytes=%f, fmt= %d\n",rec1, m_bytes, fmt);
	     printf("fmt_start = %d; blk_start = %d; bit_off=%d\n",
		   sp->fmt_start, sp->blk_start, sp->bit_off);
	}

	total_bytes = m_bytes / sp->pre->prf * (fmt-sp->fmt_start);
	total_bytes = 323. * trunc(total_bytes/323.); /* one frame is 323 bytes */
	total_bit = total_bytes * 8.0 + sp->bit_off;

/*SONY*/
        if ((!strcmp( data_direction, "REVERSE" ) ) && (strcmp(media_type, "DCRSI")))
          *block = sp->blk_end - trunc(total_bit/(BYTES_BLOCK * 8)); 
        else 
          *block = sp->blk_start + trunc(total_bit/(BYTES_BLOCK * 8)); 

	*offset =  trunc( fmod(total_bit,(double)(BYTES_BLOCK * 8)) );

	if (vbose) {
	   printf("total_bytes= %f, total_bit= %f, block= %d, offset= %d\n",
		total_bytes, total_bit, *block, *offset);
	}
	if (*offset < 32) *offset += 323*8;  /* CV 4/24/97 avoid < 32 bit */

/* For now, all flag f_dcrsi = 0 */ 
        if (f_dcrsi) {
/*SONY*/
                if (!strcmp( media_type, "DCRSI" ) ) {
                  while (r1_srch_sync(3,2,20,&block,&offset,
                                &norm_inv,&itime,&prf) != PASS) {
                      if (!strcmp( data_direction, "REVERSE" ))
                        *block -= 100;
                      else
                        *block += 100;
                      printf("Can not re-sync ... Try again at block %d\n",
                                *block);
                  }
                }
                else {
                      while (r1_srch_sync(3,2,1,&block,&offset,
                                &norm_inv,&itime,&prf) != PASS) {
                         *block += 3;
                         printf("Can not re-sync ... Try again at block %d\n",
                                *block);
                       }
                     }

                printf("found sync at block = %d offset %d\n",
                                *block, *offset);
        }
/*
	total_bytes = (int) ((m_bytes/sp->pre->prf) * (fmt+1));
	total_bit = total_bytes * 8 + sp->bit_off;
	*block = (int) (sp->blk_start + (total_bit/(BYTES_BLOCK*8))); 
	*offset = ((long) (total_bit)) % (BYTES_BLOCK * 8);
*/
}


r1_get_gmt(aux,gmt)
	RSAT_AUX_PTR aux;
	GMT *gmt;
{
	int remain, remain_hr, total_sec;
	int day_in_raw;
	float frac_sec;

	/* 3/10/97 */
	frac_sec = (aux->time.msecs + aux->time.microsecs/1024) * 0.001;	
	total_sec = (aux->time.days * 24 * 3600) + aux->time.seconds;
	gmt->yr = Cur_Rqst->start.yr;	  /* use the year in job file */
/*	gmt->yr = ephem.year;	   use the year in ephem data */
	gmt->day = (total_sec / (24*3600)) - (365-263); 

/* ASP V.5.29 */

        if (gmt->day > (366+365+365+365+366))
          gmt->day -= (366+365+365+365+366);
        else if (gmt->day > (366+365+365+365))
               gmt->day -= (366+365+365+365);
             else if (gmt->day > (366+365+365))
                    gmt->day -= (366+365+365);
                  else if (gmt->day > (366+365))
                         gmt->day -= (366+365);
                       else if (gmt->day > 366)
                              gmt->day -= 366;

	if (vbose) printf("gmt.yr=%d, day=%d, \n", 
		gmt->yr,gmt->day);

	remain = total_sec % (24*3600);
	gmt->hr = remain / 3600;
	remain_hr = remain % 3600;
	gmt->min = remain_hr / 60;
	gmt->second = (remain_hr % 60) + frac_sec;
}


