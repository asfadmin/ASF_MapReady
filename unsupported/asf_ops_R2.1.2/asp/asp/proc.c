/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* proc.c -- processing routines for ASP */
/* Modified for throughput upgrade, 7/23/92, MC */
/*  EYC 2-23-96 - corrected field 3 of data record prefix (11, not 10)*/

#include <stdio.h>
#include <syslog.h>
#include <unistd.h>
#include <malloc.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <aspdecl.h>
#include <procfil.h>
#include <procdec.h>
#include <scene_file.h>
#include <i_fdr.h>
#include <asp_msg.h>
#include <asp_shm.h>

#define CEOS_PREFIX_SIZE 192
#define IMG_RECLEN 8192
#define CSD_IMG_RECLEN 11264
#define STD_IMG_LINES 8192
#define CPX_IMG_LINES 12800
#define CSD_IMG_LINES 26624
#define MAX_COPY 20

extern int skip_segment;
extern char ASP_VERSION[8];
extern int asphw;
extern int cp_flag;
extern char FULL_IMG_PATH[PATHLEN];
enum { LOW_RES, FULL_RES, BOTH_IMAGE };
extern int Res_type;
extern int ucs_product;

extern TAPE_SEG_PTR seg_list;	/* tape segment list */
extern DPP_FILE dpf;		/* default proc parameter file */
extern SP_FILE spf;		/* sensor parameters file */
extern SCENE_FILE sf;		/* scene file */
extern RQST_PTR Cur_Rqst;	/* pointer to current job request */

extern int toupper_sw;		/* 1 = convert lower case to upper */
extern char token_separator;	/* separator for last token returned */
extern int rec, echo_byte, rep_byte;

/* ASP configuration parameters */
extern int vbose;		/* 1 = print progress messages */
extern int sw_clutter_lock;	/* 1 = perform 2DFFT */
extern int sw_rep_meas;         /* 1 = perform repmeas */
extern int sw_pp_only;		/* 1 = preprocessing only */
/*
extern int alarm;		/* 1 = timer alarm went off */
extern int fmt_len;		/* avg fmt length(bit) for its prf */
extern int fst_len;		/* length of first input format, from scan */

extern double gain_value[900];	/* gain values from pvs_cal.odl */
extern char frame_mode[];

/* -- parameter data -- */
/* following variables are derived from the data in parameter file
   or are reserved for special modes.
*/

/* system data */
int sys_ckspeed;	/* system clock frequency */

/* input data */
int in_bypass;		/* 1 = bypass input interface */
char in_source[80];	/* input source filename / DCRSi */
int in_format;		/* sensor: 0=ERS(OGRC), 1=JERS, 2=RADARSAT, 3=ERS(OBRC) */
int in_startblock;	/* starting block if input is from DCRSi */
int in_startline;	/* starting line if input is from the file */
int in_startpoint;	/* starting point in the line if input is from the file */
int in_blkoff;		/* sync code offset in first block */
int in_shift;		/* byte alignment shift */
int in_norminv;		/* 1=normal, 0=inverted bit values */
char in_trantbl[80];	/* input translate table filename */
int in_linelen;		/* input line length */
int in_outlen;		/* output line length (to range cor.) */
int in_chirp;		/* chirp length */
int in_vlines;		/* # of valid lines per frame */
int in_ilines;		/* # of invalid lines per frame */
int in_scenelen;	/* # of frames in the scene */
int in_offch;		/* line # where offset changes */
int in_offset1;		/* offset used before change */
int in_offset2;		/* offset used after change */
int in_swstart;		/* start of swath within a range line */
int in_lenoff;		/* length offset */
char in_jdfm_tbl[80];	/* JDFM table filename */
char in_stc_wgt[80];	/* STC weights filename */

/* range correlator data */
     /* from parameter file */
int rc_bypass;		/* 1 = bypass range correlator */
int rc_fscale;		/* forward fft scaling flags */
int rc_iscale;		/* inverse fft scaling flags */
char rc_reffun[80];	/* reference function filename */
int rc_refrot;		/* ref. function rotation */
char rc_rcfun[80];	/* radiom.comp. function filename */
     /* special modes */
int rc_refform = 0;		/* 1=divide ref.func. output by 2 */
int rc_refconj = 0;		/* 1=conjugate an input to ref.funct. */
int rc_refrdat = 0;		/* 1=conjugate ref.funct, 0=data */
int rc_rcform = 0;		/* 1=divide r.c.func. output by 2 */
int rc_rcconj = 0;		/* 1=conjugate an input to r.c.funct. */
int rc_rcrdat = 0;		/* 1=conjugate r.c.funct., 0=data */
int rc_rcrot = 0;		/* radiom.comp. function rotation */

/* corner turn data */
     /* from parameter file */
int ct_bypass;		/* 1 = bypass corner turn */
int ct_outlen;		/* output line length */
int ct_vlpf;		/* valid lines per frame */
int ct_rdlen1;		/* frame 1 read length */
int ct_rdlen2;		/* frame 2 read length */
int ct_aspect;		/* aspect ratio */
     /* special modes */
int ct_unsrot = 0;		/* unscrambler rotation */

/* azimuth forward fft data */
int af_bypass;		/* 1 = bypass azimuth forward fft */
int af_scale;		/* scaling flags */

/* azimuth processing data */
     /* from parameter file */
int ap_bypass;		/* 1 = bypass azimuth processing */
int ap_looks;		/* 1 or 4 look data */
int ap_rmwt;		/* range migration weighting function */
int ap_rmlines;		/* number of range migration output lines */
char ap_rmpath[80];	/* range migration funct. filename */
int ap_looklen;		/* valid points per look */
int ap_lookpos;		/* first non-zero point in look */
char ap_xfun[80];	/* xfer function filename */
int ap_scale;		/* fft scaling flags */
int ap_azwt;		/* azimuth interp. weighting function */
int ap_detshift;	/* detector bit shift (-1 = bypass detector) */
int ap_ptspace;		/* az. interp. point spacing */
char ap_azstart[80];	/* azint line start array filename */
     /* special modes */
int ap_rmform = 0;		/* 1 = divide r.m. output by 2 */
int ap_xform = 0;		/* 1 = divide xfer output by 2 */
int ap_xconj = 0;		/* 1 = conjugate an input to xfer */
int ap_xtran = 0;		/* 1 = conj. xfer funct., 0 = data */
int ap_azform = 0;		/* 1=divide azint output by 2 */

/* multi-look data */
int ml_bypass;		/* 1 = bypass multi-look processing */
char ml_output[80];	/* output destination filename / DCRSi */
int ml_startblock;	/* starting block if output is to DCRSi */
int ml_outbits;		/* output 16 or 32 bits */
int ml_frdelay;		/* number of frames to delay */
int ml_froff;		/* azimuth offset between frames */
int ml_azlen;		/* azimuth line length */
int ml_linelen;		/* multi-look memory output line length */
char ml_rotfun[80];	/* rotation function filename */
int ml_vppl;		/* valid points per look */
int ml_lkoff;		/* look offset */
int ml_aspect;		/* aspect ratio */
int ml_segsize;		/* # of points to use in avg buffers */
int ml_divisor;		/* divisor for averaging */


/* variables used by multiple modules that are not
   derived directly from parameter file.
   all of these variables have prefix pr_ .
*/

int pr_nlines;		 /* number of valid lines in the scene */
int pr_tlines;		 /* total number of lines in the scene (valid + invalid) */
int pr_segments;         /* number of segments in staged FFT */
int pr_valpoints;        /* number of fully correlated points in range line */
int pr_rglen;		 /* number of range points to be processed */
int pr_actline;		 /* length of the actual range line */
int pr_look;             /* 1 for 4 look case; 0 for 1 look case */
int pr_aifftlen;         /* azimuth inverse FFT length */
int pr_avglines;	 /* number of averaged lines */


/* register override data */
typedef struct regov {
	struct regov *nxt_regov; /* pointer to next override */
	int addr;		/* register address */
	int value;		/* register value */
} REGOV, *ROPTR;

ROPTR regov_list = NULL;	/* register overflow data list */

static int param_io_sw = 0;	/* 0 = read parameter file */
				/* 1 = write parameter file */
static FILE *pfp;		/* output parameter file pointer */
int first_odd;			/* flag for first of odd or even */
int first_ident;		/* flag to identify dcrs */
int pre_done;			/* 1 = do not repeat preprocessing */	
int do_binary;			/* 1 = rebuild binary files when pre_done = 1 */
int do_repmeas;                 /* 1 = perform repmeas */
int detshift_old;               /* pass set_gain result to even image */
float prf;

/*SONY*/
extern char media_type[];
extern char data_direction[];

/* proc(rp) -----------------------------------------------------
	This routine performs processing on the job request pointed
	to by rp.
*/

proc(rp)
	RQST_PTR rp;
{
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp, pp2;
	int ans,odd,cnt;
	char image_id[12];
	int seg = 0, region;
	int first_scene = 1;
	int first_ident =1;
	int seg_skip = 0;
	int segment = 0;
	int do_preproc;
	char string[132];
	int status;

    /* check job status */
	if (!cp_flag && (rp->status > 2)) {
	    printf ("This job has already been processed, wish to re-process?\n");
	    if (op_answer("-- say no to exit") == FAIL) return (PASS);
	}
	asp_msg(LOG_DEBUG,"Preparing processing...");

    /* read the preprocessing result files for this job */

	chdir(JOB_PATH);
	if (pre_done == 1) {
	    strcpy( string, "pp_regions" );
	}
	else strcpy( string, "pp_regions1" );

	p_rw_pp_file(0,string,1);
	p_rw_pream_file(0,"preambles");
	p_rw_postam_file(0,"postambles");
	p_link_pambles();

    /* Assume we will process the data in two passes, first the
       odd number regions, then the even number regions.  
       This does not account for the last three regions that
       might overlap highly.                             */

/* 3/30, MC
	1. do two-pass from segment to segment.
	2. slip proper formats between regions (in run_image).
	3. make sure coming back for 2nd pass is proper.
*/
    
      if (!cp_flag && pre_done && (rp->status != 2) &&
	  (Cur_Rqst->id[9] != 'P') &&
	  (strcmp(Cur_Rqst->type, "STD") == 0 ||
	   strcmp(Cur_Rqst->type, "QLK") == 0 )) {
        for (sp = seg_list; sp != NULL; sp = sp->nxt_seg)
	    segment++;
	skip_segment = 1;
        display_seg_list(segment);
	skip_segment = 0;
	printf("Enter number of segment(s) to skip for re-processing:");
	scanf( "%d", &seg_skip );
      }

      ans = PASS;
      first_ident = 1;
      seg = 0;
      for (sp = seg_list; sp != NULL; sp = sp->nxt_seg) {

	if (seg < seg_skip){
	    seg++;
	    continue;
	}
	seg++;
	asp_msg(LOG_DEBUG,"-- Segment %d --",seg); 
        prf = (float) sp->pre->prf;     /*  save for data record prefix*/
        if ( Cur_Rqst->take_id[0] == 'J' ) {
            if ((int)sp->pre->prf == 1505) fmt_len = 39844;
            else if ((int)sp->pre->prf == 1530) fmt_len = 39212;
            else if ((int)sp->pre->prf == 1555) fmt_len = 38580;
            else if ((int)sp->pre->prf == 1581) fmt_len = 37948;
            else if ((int)sp->pre->prf == 1606) fmt_len = 37358;
            else if ((int)sp->pre->prf == 1646) fmt_len = 37648; /* seasat */
            fst_len = fmt_len + 1;
            if (vbose) 
		printf("use avg length %d for j1_get_fmt_loc\n", fmt_len);
        }
      	for (odd=0; odd < 2; odd++) {
	    if (odd && sw_pp_only) {
		if (vbose) printf("ONE PASS for preprocessing only!\n");
		continue;
	    }
	    first_odd = 1;  /* in run_image to start tapes */
	    asp_msg(LOG_DEBUG,"-- Start Pass %d --",odd+1);
	    for (pp = sp->ppb_list, cnt = 0, region = 1; 
			ans==PASS && pp!=NULL;
			pp = pp->nxt_ppb, cnt++, region++) {
		if ( Cur_Rqst->take_id[0] == 'E' ||
			Cur_Rqst->take_id[0] == 'R' )
                   do_repmeas = sw_rep_meas&&(region==0||pp->nxt_ppb==NULL);
		do_preproc = ((cnt%2) == odd);
		if ( (rp->status == 2) && do_preproc ) continue;

    	    /* create and change to image directory */

		if (pp->img_name[0] == 'I')
		    strcpy(image_id,pp->img_name);
		else if ( cp_flag ){
		    sprintf(image_id,"I%9.9s",Cur_Rqst->site);
		    image_id[10] = '\0';
		}
		else
		    get_image_id(Cur_Rqst->type,image_id);
		chdir(JOB_PATH);
		if (odd == 0) op_mkdir(image_id);
		chdir(image_id);
		strcpy(pp->img_name,image_id);

		if ( do_preproc ){
		    if ( odd ){
		       asp_msg(LOG_DEBUG,
				"Start saving preprocess for Image %s ... ",
				 pp->img_name);
		    }
		    else {
		       asp_msg(LOG_DEBUG,
				"Start preprocess for Image %s ... ",
				pp->img_name);
		    }
		}
		else {
		    asp_msg(LOG_DEBUG,
			"Start avg_image for Image %s ... ",pp->img_name);
		}

		if ( do_preproc || sw_pp_only ) {

	    	  /* 2DFFT preprocessing */

		  if ( Cur_Rqst->id[9] != 'P' ){

			if ( chk_2dfft(pp->img_name) == FAIL ){
			   chdir(JOB_PATH);
			   continue;
			}
		        if (proc_2dfft(sp,pp,region) == FAIL ){
                           asp_msg(LOG_ERR,asp_messages[ERROR_IN_2DFFT]);
			   return(FAIL);
			}
			if (vbose) printf("after proc_2dfft\n");
                  	if (sw_pp_only) {
                           if (vbose) printf("LTRR ...\n");
                           proc_ltrr(sp,pp,12000);
                  	}
		  	/* copy Doppler parameters to next region in 1st pass */
		  	if (odd==0 && sw_clutter_lock==1) {
			   if ((pp2 = pp->nxt_ppb) != NULL) {
				pp2->fda = pp->fda;
				pp2->fdb = pp->fdb;
				pp2->fdc = pp->fdc;
				if ( Cur_Rqst->take_id[0] == 'E' ){
					pp2->pro_gain = pp->pro_gain;
					pp2->det_sc = detshift_old;
				}
			   }
                           if ((ans = proc_hwdata(sp,pp,0)) == FAIL) {
                                asp_msg(LOG_DEBUG,"failed in get_hwdata");
                                return(FAIL);
                           }
		  	} /* sw_clutter_lock */
		  } /* end if non-PBK */
		  status = update_pp(sp,pp,odd,seg,region);
		  if (status == FAIL) {
                        asp_msg(LOG_ERR,asp_messages[ERROR_IN_REGION]);
			return (FAIL);
		  }
		}
		else { /* of if do_preproc */

	    	   /* process the image */

		   if ( chk_image(pp->img_name) == FAIL ){
			chdir(JOB_PATH);
			continue;
		   }
		   if (rp->status != 2) { /* do not touch pp if preproc */
			status = update_pp(sp,pp,odd,seg,region);
		  	if (status == FAIL){
                           asp_msg(LOG_ERR,asp_messages[ERROR_IN_REGION]);
			   return (FAIL);
			}
		   }
		   if(first_scene == 1) {
		  	p_rw_scene_file(0,"scene_file");
		  	if(strcmp(sf.asp_ver,ASP_VERSION) != 0) {
		     	   first_scene = 0;
		     	   printf("\nThe software version during preproc does");
		     	   printf(" not match current version.\n");
		     	   printf("   preproc = %s\n",sf.asp_ver);
		     	   printf("   current = %s\n",ASP_VERSION);
		     	   strcpy(sf.asp_ver,ASP_VERSION);
		  	}
		   }
                   if ( proc_image(sp,pp,region,image_id) == FAIL ){ 
                        asp_msg(LOG_ERR,asp_messages[ERROR_IN_IMAGE]);
			return(FAIL);
		   }
		   if ( !strcmp(Cur_Rqst->type,"RPR") ){
			if ( Res_type == LOW_RES )
			   ans = sl_to_gl("avg_image", 1024, 1024, 1.0 );
			else if ( Res_type == FULL_RES )
			   ans = sl_to_gl("full_image", 8192, 8192, 1.0 );
			else {
			   ans = sl_to_gl("avg_image", 1024, 1024, 1.0 );
			   ans = sl_to_gl("full_image", 8192, 8192, 1.0 );
			}
			if ( ans == FAIL ) return(FAIL);
		   }
		}  /* if do_preproc  */

	    /* write ceos in 2nd pass */
		if (odd) {
                   read_proc_param_file("params.proc");
                   p_rw_scene_file(0,"scene_file");
                   if(strcmp(sf.asp_ver,ASP_VERSION) != 0)
                       strcpy(sf.asp_ver,ASP_VERSION);
		   write_ceos(sp,pp,"ceos_leader");
		}
	    }  /* for pp */
	    if (vbose) printf("-- Finish Pass %d --\n",odd+1);
	    if ( asphw ){
/*SONY*/
                if( strcmp( media_type, "DCRSI" ) ){
                  sony_stop(IN);
            	  sony_comm_close (IN);
                }
                else {
                       dc_stop(IN);
            	       dc_comm_close (IN);
                      }
	    }
	}  /* for odd */
	if (vbose) printf("-- Finish Segment %d --\n",seg);
      }  /* for sp */

      return (ans);
}
/* read_proc_param_file (filename) -------------------------------------
	This routine reads the processing parameters file and builds
	the processing parameters data base in memory.
	The routine returns PASS or FAIL.
*/

read_proc_param_file (filename)
	char *filename;
{
	char t[100],p[100];
	int i, n, tmp, byp, head_id;
	ROPTR rp, rp2;
	int ans = PASS;
	static int ctpixels[4] = { 2048, 8192, 16384, 8192 };
	char tmp_name[100];

    /* open the parameter file */
	if (open_file (filename,"") == 0) {
	    printf("cannot open %s\n", filename);
	    return (FAIL);
	}
    /* clean out any old register overrides */
	for (rp = regov_list; rp != NULL; rp = rp2) {
	    rp2 = rp->nxt_regov;
	    free(rp);
	}
	regov_list = NULL;
    /* set parser and token routine parameters */
	param_io_sw = 0;
	set_separators("=");
	toupper_sw = 1;
    /* read the file */
	for (next_token(t); strcmp (t,"END."); next_token(t)) {
	    head_id = get_head_id(t);
	    if (vbose)
		printf("header: %s, id=%d\n",t,head_id);
	    byp = 0;
	    if (token_separator != '\n') {
		next_token(t);
		byp = (strcmp (t,"BYPASS") == 0);
		if (vbose)
		    printf ("checking bypass, = %d\n",byp);
	    }
	    if (head_id == 0) {
		printf("unrecognizable heading: %s\n",t);
		ans = FAIL;
		flush_line();
	    }
	    else {
		switch (head_id) {
		    case 2: in_bypass = byp;  break;
		    case 3: rc_bypass = byp;  break;
		    case 4: ct_bypass = byp;  break;
		    case 5: af_bypass = byp;  break;
		    case 6: ap_bypass = byp;  break;
		    case 7: ml_bypass = byp;  break;
		    default: break;
		}
		for (next_token(p); strcmp (p,";"); next_token(p)) {
		    /*if(vbose)  printf("  param: %s\n",p);*/
		    switch (head_id) {
			case 1:	/* SYSTEM */
			    if (p_get_sy (p) == FAIL)
				ans = FAIL;
			    break;
			case 2:	/* INPUT */
			    if (p_get_in (p) == FAIL)
				ans = FAIL;
			    break;
			case 3:	/* RANGE_CORRELATOR */
			    if (p_get_rc (p) == FAIL)
				ans = FAIL;
			    break;
			case 4:	/* CORNER_TURN */
			    if (p_get_ct (p) == FAIL)
				ans = FAIL;
			    break;
			case 5:	/* AZIMUTH_FORWARD_FFT */
			    if (p_get_af (p) == FAIL)
				ans = FAIL;
			    break;
			case 6:	/* AZIMUTH_PROCESSING */
			    if (p_get_ap (p) == FAIL)
				ans = FAIL;
			    break;
			case 7:	/* MULTI_LOOK */
			    if (p_get_ml (p) == FAIL)
				ans = FAIL;
			    break;
			case 8:	/* REGISTER_OVERRIDES */
				printf("goto case8\n");
			    rp = (ROPTR) malloc(sizeof(REGOV));
			    if (rp == NULL) {
				printf("out of memory in reg override\n");
				ans = FAIL;
			    }
			    sscanf(p,"%x",&rp->addr);
			    next_token (t);
			    sscanf(t,"%x",&rp->value);
			    rp->nxt_regov = regov_list;
			    regov_list = rp;
			    flush_line();
			    break;
			default:
			    break;
		    }  /* switch */
		}  /* for p != ";" */
	    }  /* else */
	}  /* for t != "END." */

    /* calculate common parameters */
	if (ans != FAIL) {
	    pr_nlines = in_vlines * in_scenelen;
	    pr_tlines = pr_nlines + in_ilines * in_scenelen;
	    strcat(strcpy(tmp_name,DEF_PATH),"params.2dfft");
    /* Overwrite in_chirp for RSAT 2dfft 3/19/96 */
	    if ((Cur_Rqst->take_id[0]== 'R') && 
		(!strcmp(filename,tmp_name))) {
	    	pr_valpoints = in_outlen;
	    } 
	    else 
	        pr_valpoints = in_outlen - in_chirp;
	    pr_rglen = in_linelen - in_chirp;
	    pr_segments = (pr_rglen - 1) / pr_valpoints + 1;
	    if (ct_bypass == 0) {
		if (pr_segments > (tmp = ctpixels[ct_aspect] / pr_valpoints))
		    pr_segments = tmp;
		if (pr_rglen > (tmp = pr_segments * pr_valpoints))
		    pr_rglen = tmp;
	    }
    /* Overwrite IIF register for RSAT CPX job main proc 3/19/96 */
	    if ((Cur_Rqst->take_id[0]== 'R') && 
		(!strcmp(Cur_Rqst->type,"CPX")) &&
		(strcmp(filename,tmp_name))) {
		pr_segments = 2;
		pr_valpoints = 1024;
		pr_rglen = 2048;
	    }
	    pr_actline = in_outlen * pr_segments;
	    pr_look = (ap_looks == 4);
	    pr_aifftlen = (pr_look) ? ct_outlen / 4 : ct_outlen;
	    pr_avglines = ((in_scenelen - ml_frdelay + 1 - 2) * ml_froff ) / 8; 
	}

	close_file();
	return (ans);
}


/* write_proc_param_file (filename) ------------------------------------
	This routine writes the current processing parameters to disk
	in the format of a processing parameters file.
*/

write_proc_param_file (filename)
	char *filename;
{
	ROPTR rp;

	if ((pfp = fopen(filename,"w")) == NULL) {
	    printf ("cannot open output parameter file %s\n",filename);
	    return (FAIL);
	}
	if (vbose)
	    printf ("writing output parameter file\n");
	param_io_sw = 1;
	if (vbose)
	    printf("system, ");
	fflush(stdout);
	p_get_sy (NULL);
	if (vbose)
	    printf("input, ");
	fflush(stdout);
	p_get_in (NULL);
	if (vbose)
	    printf("range, ");
	fflush(stdout);
	p_get_rc (NULL);
	if (vbose)
	    printf("ctc, ");
	fflush(stdout);
	p_get_ct (NULL);
	if (vbose)
	    printf("az_fft, ");
	fflush(stdout);
	p_get_af (NULL);
	if (vbose)
	    printf("az_proc, ");
	fflush(stdout);
	p_get_ap (NULL);
	if (vbose)
	    printf("mlook\n");
	fflush(stdout);
	p_get_ml (NULL);
	if ((rp = regov_list) != NULL) {
	    fprintf(pfp,"REGISTER_OVERRIDES\n");
	    while (rp != NULL) {
		fprintf(pfp,"  %.3X = %X\n",rp->addr,rp->value);
		rp = rp->nxt_regov;
	    }
	    fprintf(pfp,"  ;\n");
	}
	fprintf(pfp,"END.");
	fclose (pfp);
	return (PASS);
}


/* get_head_id (t) -----------------------------------------------------
	This routine determines which heading word (if any) is in t,
	and returns the corresponding heading id code.  If the code is
	not recognized, the routine returns 0.
*/

get_head_id (t)
	char *t;
{
	if (strcmp(t,"SYSTEM") == 0)
	    return (1);
	if (strcmp(t,"INPUT") == 0)
	    return (2);
	if (strcmp(t,"RANGE_CORRELATOR") == 0)
	    return (3);
	if (strcmp(t,"CORNER_TURN") == 0)
	    return (4);
	if (strcmp(t,"AZIMUTH_FORWARD_FFT") == 0)
	    return (5);
	if (strcmp(t,"AZIMUTH_PROCESSING") == 0)
	    return (6);
	if (strcmp(t,"MULTI_LOOK") == 0)
	    return (7);
	if (strcmp(t,"REGISTER_OVERRIDES") == 0)
	    return (8);
	return (0);
}


/* p_get_sy (p) --------------------------------------------------------
	This routine translates and stores the parameters for the
	SYSTEM section of the parameter file, or alternately writes
	those parameters to the output parameter file.
*/

p_get_sy (p)
	char *p;
{
	char t[100];
	static char *ptab[] = { "CLOCK_SPEED",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, param_id;
	static char *clk_tbl[4] = { "10", "5", "2.5", "EXTERNAL" };
	int ans = PASS;

    /* if io switch=1, write parameters to output file */
	if (param_io_sw) {
	    fprintf(pfp,"SYSTEM\n");
	    fprintf(pfp,"  %s = %s\n",ptab[0],clk_tbl[sys_ckspeed]);
	    fprintf(pfp,"  ;\n");
	    return (0);
	}

    /* retrieve one parameter */
	param_id = 0;
	for (i = 0; i < ptablen; i++) {
	    if (strcmp(p,ptab[i]) == 0) {
		param_id = i + 1;
		break;
	    }
	}
	switch (param_id) {
	    case 1:		/* CLOCK_SPEED */
		next_token (t);
		sys_ckspeed = 0;
		for (i = 0; i < 3; i++)
		    if (strcmp(t,clk_tbl[i]) == 0)
			sys_ckspeed = i;
		break;
	    default:		/* unrecognizable parameter */
		printf("unrecognizable parameter: %s\n",p);
		ans = FAIL;
	}  /* switch */

	flush_line();
	return (ans);
}



/* p_get_in (p) --------------------------------------------------------
	This routine translates and stores the parameters for the
	INPUT section of the parameter file.
*/

p_get_in (p)
	char *p;
{
	char t[100];
	static char *ptab[] = { "SOURCE",
				 "FORMAT",
				 "INPUT_START_BLOCK",
				 "START_LINE",
				 "START_POINT",
				 "SYNC_OFFSET",
				 "INVERTED_BITS",
				 "TRANSLATE_TABLE",
				 "LINE_LENGTH",
				 "OUTPUT_LINE_LENGTH",
				 "CHIRP_LENGTH",
				 "VALID_LINE_RATIO",
				 "FRAMES_PER_SCENE",
				 "OFFSET_CHANGE_LINE",
				 "OFFSET1",
				 "OFFSET2",
				 "SWATH_START",
				 "LENOFF",
				 "JDFM_TBL",
				 "STC_WGT",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, param_id;
	static char *fmt_tbl[] = { "ERS", "JERS", "RADARSAT" };
	int ans = PASS;

    /* if io switch=1, write parameters to output file */
	if (param_io_sw) {
	    fprintf(pfp,"INPUT\n");
	    if (in_bypass) {
		fprintf(pfp," = BYPASS\n  ;\n");
		return (0);
	    }
	    fprintf(pfp,"  %s = %s\n",ptab[0],in_source);
	    fprintf(pfp,"  %s = %s\n",ptab[1],fmt_tbl[in_format]);
	    fprintf(pfp,"  %s = %d\n",ptab[2],in_startblock);
	    fprintf(pfp,"  %s = %d\n",ptab[3],in_startline);
	    fprintf(pfp,"  %s = %d\n",ptab[4],in_startpoint);
	    fprintf(pfp,"  %s = %d.%d\n",ptab[5],in_blkoff,in_shift);
	    fprintf(pfp,"  %s = %s\n",ptab[6],in_norminv ? "N" : "I");
	    fprintf(pfp,"  %s = %s\n",ptab[7],in_trantbl);
	    fprintf(pfp,"  %s = %d\n",ptab[8],in_linelen);
	    fprintf(pfp,"  %s = %d\n",ptab[9],in_outlen);
	    fprintf(pfp,"  %s = %d\n",ptab[10],in_chirp);
	    fprintf(pfp,"  %s = %d/%d\n",ptab[11],in_vlines,in_ilines);
	    fprintf(pfp,"  %s = %d\n",ptab[12],in_scenelen);
	    fprintf(pfp,"  %s = %d\n",ptab[13],in_offch);
	    fprintf(pfp,"  %s = %d\n",ptab[14],in_offset1);
	    fprintf(pfp,"  %s = %d\n",ptab[15],in_offset2);
	    fprintf(pfp,"  %s = %d\n",ptab[16],in_swstart);

	    if ( Cur_Rqst->take_id[0] == 'J') {
	    fprintf(pfp,"  %s = %d\n",ptab[17],in_lenoff);
	    fprintf(pfp,"  %s = %s\n",ptab[18],in_jdfm_tbl);
	    fprintf(pfp,"  %s = %s\n",ptab[19],in_stc_wgt);
	    }

	    fprintf(pfp,"  ;\n");
	    return (0);
	}
/*
	strcpy(in_jdfm_tbl,"BYPASS");
	strcpy(in_stc_wgt,"BYPASS");
*/
    /* retrieve one parameter */
	param_id = 0;
	for (i = 0; i < ptablen; i++) {
	    if (strcmp(p,ptab[i]) == 0) {
		param_id = i + 1;
		break;
	    }
	}
	switch (param_id) {
	    case 1:		/* SOURCE */
		toupper_sw = 0;
		next_token (in_source);
		toupper_sw = 1;
		break;
	    case 2:		/* FORMAT */
		next_token (t);
		in_format = 0;
		for (i = 0; i < 3; i++)
		    if (strcmp(t,fmt_tbl[i]) == 0) {
			in_format = i;
			break;
		    }
		if (i == 3) {
		    printf(" INPUT FORMAT unrecognizable\n");
		    ans = FAIL;
		}
		break;
	    case 3:		/* INPUT_START_BLOCK */
		get_int (&in_startblock);
		break;
	    case 4:		/* START_LINE */
		get_int (&in_startline);
		break;
	    case 5:		/* START_POINT */
		get_int (&in_startpoint);
		break;
	    case 6:		/* SYNC_OFFSET */
		next_token (t);
		i = strlen(t);
		if (t[i-2] == '.') {
		    in_shift = atoi(&t[i-1]);
		    t[i-2] = '\0';
		}
		else
		    in_shift = 0;
		in_blkoff = atoi(t);
		break;
	    case 7:		/* INVERTED_BITS */
		next_token (t);
		in_norminv = (t[0] == 'N');
		break;
	    case 8:		/* TRANSLATE_TABLE */
		toupper_sw = 0;
		next_token (in_trantbl);
		toupper_sw = 1;
		break;
	    case 9:		/* LINE_LENGTH */
		get_int (&in_linelen);
		break;
	    case 10:		/* OUTPUT_LINE_LENGTH */
		get_int (&in_outlen);
		break;
	    case 11:		/* CHIRP_LENGTH */
		get_int (&in_chirp);
		break;
	    case 12:		/* VALID_LINE_RATIO */
		next_token (t);
		sscanf(t,"%d%c%d",&in_vlines,t,&in_ilines);
		break;
	    case 13:		/* FRAMES_PER_SCENE */
		get_int (&in_scenelen);
		break;
	    case 14:		/* OFFSET_CHANGE_LINE */
		get_int (&in_offch);
		break;
	    case 15:		/* OFFSET1 */
		get_int (&in_offset1);
		break;
	    case 16:		/* OFFSET2 */
		get_int (&in_offset2);
		break;
	    case 17:		/* SWATH START*/
		get_int (&in_swstart);
		break;
	    case 18:		/* LENOFF */
		get_int (&in_lenoff);
		break;
	    case 19:		/* JDFM_TBL */
		toupper_sw = 0;
		next_token (in_jdfm_tbl);
		toupper_sw = 1;
		break;
	    case 20:		/* STC_WGT */
		toupper_sw = 0;
		next_token (in_stc_wgt);
		toupper_sw = 1;
		break;
	    default:		/* unrecognizable parameter */
		printf("unrecognizable parameter: %s\n",p);
		ans = FAIL;
	}  /* switch */

	flush_line();
	return (ans);
}


/* p_get_rc (p) --------------------------------------------------------
	This routine translates and stores the parameters for the
	RANGE_CORRELATOR section of the parameter file.
*/

p_get_rc (p)
	char *p;
{
	char t[100];
	static char *ptab[] = { "FORWARD_SCALING",
				"INVERSE_SCALING",
				"REF_FUNCTION",
				"REF_ROTATE",
				"RC_FUNCTION",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, param_id;
	int ans = PASS;

    /* if io switch=1, write parameters to output file */
	if (param_io_sw) {
	    fprintf(pfp,"RANGE_CORRELATOR");
	    if (rc_bypass) {
		fprintf(pfp," = BYPASS\n  ;\n");
		return (0);
	    }
	    fprintf(pfp,"\n");
	    p_put_mask(rc_fscale,t);
	    fprintf(pfp,"  %s = %s\n",ptab[0],t);
	    p_put_mask(rc_iscale,t);
	    fprintf(pfp,"  %s = %s\n",ptab[1],t);
	    fprintf(pfp,"  %s = %s\n",ptab[2],rc_reffun);
	    fprintf(pfp,"  %s = %d\n",ptab[3],rc_refrot);
	    fprintf(pfp,"  %s = %s\n",ptab[4],rc_rcfun);
	    fprintf(pfp,"  ;\n");
	    return (0);
	}

    /* retrieve one parameter */
	param_id = 0;
	for (i = 0; i < ptablen; i++) {
	    if (strcmp(p,ptab[i]) == 0) {
		param_id = i + 1;
		break;
	    }
	}
	switch (param_id) {
	    case 1:		/* FORWARD_SCALING */
		p_get_mask (&rc_fscale);
		break;
	    case 2:		/* INVERSE_SCALING */
		p_get_mask (&rc_iscale);
		break;
	    case 3:		/* REF_FUNCTION */
		toupper_sw = 0;
		next_token (rc_reffun);
		toupper_sw = 1;
		break;
	    case 4:		/* REF_ROTATE */
		get_int (&rc_refrot);
		break;
	    case 5:		/* RC_FUNCTION */
		toupper_sw = 0;
		next_token (rc_rcfun);
		toupper_sw = 1;
		break;
	    default:		/* unrecognizable parameter */
		printf("unrecognizable parameter: %s\n",p);
		ans = FAIL;
	}  /* switch */

	flush_line();
	return (ans);
}


/* p_get_ct (p) --------------------------------------------------------
	This routine translates and stores the parameters for the
	CORNER_TURN section of the parameter file.
*/

p_get_ct (p)
	char *p;
{
	char t[100];
	static char *ptab[] = { "OUTPUT_LINE_LENGTH",
				"VALID_LINES_PER_FRAME",
				"FRAME1_READ_LENGTH",
				"FRAME2_READ_LENGTH",
				"ASPECT_MODE",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, param_id;
	int ans = PASS;

    /* if io switch=1, write parameters to output file */
	if (param_io_sw) {
	    fprintf(pfp,"CORNER_TURN");
	    if (ct_bypass) {
		fprintf(pfp," = BYPASS\n  ;\n");
		return (0);
	    }
	    fprintf(pfp,"\n");
	    fprintf(pfp,"  %s = %d\n",ptab[0],ct_outlen);
	    fprintf(pfp,"  %s = %d\n",ptab[1],ct_vlpf);
	    fprintf(pfp,"  %s = %d\n",ptab[2],ct_rdlen1);
	    fprintf(pfp,"  %s = %d\n",ptab[3],ct_rdlen2);
	    p_get_aspect (t,&ct_aspect);
	    fprintf(pfp,"  %s = %s\n",ptab[4],t);
	    fprintf(pfp,"  ;\n");
	    return (0);
	}

    /* retrieve one parameter */
	param_id = 0;
	for (i = 0; i < ptablen; i++) {
	    if (strcmp(p,ptab[i]) == 0) {
		param_id = i + 1;
		break;
	    }
	}
	switch (param_id) {
	    case 1:		/* OUTPUT_LINE_LENGTH */
		get_int (&ct_outlen);
		break;
	    case 2:		/* VALID_LINES_PER_FRAME */
		get_int (&ct_vlpf);
		break;
	    case 3:		/* FRAME1_READ_LENGTH */
		get_int (&ct_rdlen1);
		break;
	    case 4:		/* FRAME2_READ_LENGTH */
		get_int (&ct_rdlen2);
		break;
	    case 5:		/* ASPECT_MODE */
		next_token (t);
		p_get_aspect (t,&ct_aspect);
		break;
	    default:		/* unrecognizable parameter */
		printf("unrecognizable parameter: %s\n",p);
		ans = FAIL;
	}  /* switch */

	flush_line();
	return (ans);
}


/* p_get_af (p) --------------------------------------------------------
	This routine translates and stores the parameters for the
	AZIMUTH_FORWARD_FFT section of the parameter file.
*/

p_get_af (p)
	char *p;
{
	char t[100];
	static char *ptab[] = { "SCALING",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, param_id;
	int ans = PASS;

    /* if io switch=1, write parameters to output file */
	if (param_io_sw) {
	    fprintf(pfp,"AZIMUTH_FORWARD_FFT");
	    if (af_bypass) {
		fprintf(pfp," = BYPASS\n  ;\n");
		return (0);
	    }
	    fprintf(pfp,"\n");
	    p_put_mask(af_scale,t);
	    fprintf(pfp,"  %s = %s\n",ptab[0],t);
	    fprintf(pfp,"  ;\n");
	    return (0);
	}

    /* retrieve one parameter */
	param_id = 0;
	for (i = 0; i < ptablen; i++) {
	    if (strcmp(p,ptab[i]) == 0) {
		param_id = i + 1;
		break;
	    }
	}
	switch (param_id) {
	    case 1:		/* SCALING */
		p_get_mask (&af_scale);
		break;
	    default:		/* unrecognizable parameter */
		printf("unrecognizable parameter: %s\n",p);
		ans = FAIL;
	}  /* switch */

	flush_line();
	return (ans);
}


/* p_get_ap (p) --------------------------------------------------------
	This routine translates and stores the parameters for the
	AZIMUTH_PROCESSING section of the parameter file.
*/

p_get_ap (p)
	char *p;
{
	char t[100];
	static char *ptab[] = { "LOOKS",
				"RM_WEIGHTING",
				"RM_LINES",
				"RM_PATH",
				"LOOK_LENGTH",
				"LOOK_POSITION",
				"XFER_FUNCTION",
				"SCALING",
				"AZINT_WEIGHTING",
				"DETECTOR_SHIFT",
				"POINT_SPACING",
				"AZINT_START",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, param_id;
	int ans = PASS;
	float f;
	double atof();

    /* if io switch=1, write parameters to output file */
	if (param_io_sw) {
	    fprintf(pfp,"AZIMUTH_PROCESSING");
	    if (ap_bypass) {
		fprintf(pfp," = BYPASS\n  ;\n");
		return (0);
	    }
	    fprintf(pfp,"\n");
	    fprintf(pfp,"  %s = %d\n",ptab[0],ap_looks);
	    fprintf(pfp,"  %s = %d\n",ptab[1],ap_rmwt);
	    fprintf(pfp,"  %s = %d\n",ptab[2],ap_rmlines);
	    fprintf(pfp,"  %s = %s\n",ptab[3],ap_rmpath);
	    fprintf(pfp,"  %s = %d\n",ptab[4],ap_looklen);
	    fprintf(pfp,"  %s = %d\n",ptab[5],ap_lookpos);
	    fprintf(pfp,"  %s = %s\n",ptab[6],ap_xfun);
	    p_put_mask(ap_scale,t);
	    fprintf(pfp,"  %s = %s\n",ptab[7],t);
	    fprintf(pfp,"  %s = %d\n",ptab[8],ap_azwt);
	    if (ap_detshift == -1)
		fprintf(pfp,"  %s = BYPASS\n",ptab[9]);
	    else{
		if (Cur_Rqst->take_id[0] == 'J' ||
		    Cur_Rqst->take_id[0] == 'R' )
		fprintf(pfp,"  %s = %d\n",ptab[9],ap_detshift);
		else
		fprintf(pfp,"  %s = %d\n",ptab[9],(ap_detshift -1));
	    }
	    fprintf(pfp,"  %s = %f\n",ptab[10],ap_ptspace / 8192.0);
	    fprintf(pfp,"  %s = %s\n",ptab[11],ap_azstart);
	    fprintf(pfp,"  ;\n");
	    return (0);
	}

    /* retrieve one parameter */
	param_id = 0;
	for (i = 0; i < ptablen; i++) {
	    if (strcmp(p,ptab[i]) == 0) {
		param_id = i + 1;
		break;
	    }
	}
	switch (param_id) {
	    case 1:		/* LOOKS */
		get_int (&ap_looks);
		break;
	    case 2:		/* RM_WEIGHTING */
		get_int (&ap_rmwt);
		break;
	    case 3:		/* RM_LINES */
		get_int (&ap_rmlines);
		break;
	    case 4:		/* RM_PATH */
		toupper_sw = 0;
		next_token (ap_rmpath);
		toupper_sw = 1;
		break;
	    case 5:		/* LOOK_LENGTH */
		get_int (&ap_looklen);
		break;
	    case 6:		/* LOOK_POSITION */
		get_int (&ap_lookpos);
		break;
	    case 7:		/* XFER_FUNCTION */
		toupper_sw = 0;
		next_token (ap_xfun);
		toupper_sw = 1;
		break;
	    case 8:		/* SCALING */
		p_get_mask (&ap_scale);
		break;
	    case 9:		/* AZINT_WEIGHTING */
		get_int (&ap_azwt);
		break;
	    case 10:		/* DETECTOR_SHIFT */
		next_token(t);
		if (strcmp(t,"BYPASS") == 0)
		    ap_detshift = -1;
		else
		    ap_detshift = atoi(t);
		break;
	    case 11:		/* POINT_SPACING */
		get_float (&f);
		if (f > 1.0) {
		    f = 1.0;
		    printf("point spacing too large, set to 1.0\n");
		}
		ap_ptspace = f * 8192.0;
		break;
	    case 12:		/* AZINT_START */
		toupper_sw = 0;
		next_token (ap_azstart);
		toupper_sw = 1;
		break;
	    default:		/* unrecognizable parameter */
		printf("unrecognizable parameter: %s\n",p);
		ans = FAIL;
	}  /* switch */

	flush_line();
	return (ans);
}


/* p_get_ml (p) --------------------------------------------------------
	This routine translates and stores the parameters for the
	MULTI_LOOK section of the parameter file.
*/

p_get_ml (p)
	char *p;
{
	char t[100];
	static char *ptab[] = { "ROTATE_FUNCTION",
				"VALID_POINTS_PER_LOOK",
				"LOOK_OFFSET",
				"OUTPUT",
				"OUTPUT_START_BLOCK",
				"OUTPUT_BITS",
				"OUTPUT_FRAME_DELAY",
				"FRAME_OFFSET",
				"AZIMUTH_LINE_LENGTH",
				"OUTPUT_LENGTH",
				"ASPECT_MODE",
				"AVG_SEGSIZE",
				"AVG_DIVISOR",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, param_id;
	int ans = PASS;

    /* if io switch=1, write parameters to output file */
	if (param_io_sw) {
	    fprintf(pfp,"MULTI_LOOK");
	    if (ml_bypass) {
		fprintf(pfp," = BYPASS\n  ;\n");
		return (0);
	    }
	    fprintf(pfp,"\n");
	    fprintf(pfp,"  %s = %s\n",ptab[0],ml_rotfun);
	    fprintf(pfp,"  %s = %d\n",ptab[1],ml_vppl);
	    fprintf(pfp,"  %s = %d\n",ptab[2],ml_lkoff);
	    fprintf(pfp,"  %s = %s\n",ptab[3],ml_output);
	    fprintf(pfp,"  %s = %d\n",ptab[4],ml_startblock);
	    fprintf(pfp,"  %s = %d\n",ptab[5],ml_outbits ? 32 : 16);
	    fprintf(pfp,"  %s = %d\n",ptab[6],ml_frdelay);
	    fprintf(pfp,"  %s = %d\n",ptab[7],ml_froff);
	    fprintf(pfp,"  %s = %d\n",ptab[8],ml_azlen);
	    fprintf(pfp,"  %s = %d\n",ptab[9],ml_linelen);
	    p_get_aspect (t,&ml_aspect);
	    fprintf(pfp,"  %s = %s\n",ptab[10],t);
	    fprintf(pfp,"  %s = %d\n",ptab[11],ml_segsize);
	    fprintf(pfp,"  %s = %d\n",ptab[12],ml_divisor);
	    fprintf(pfp,"  ;\n");
	    return (0);
	}

    /* retrieve one parameter */
	param_id = 0;
	for (i = 0; i < ptablen; i++) {
	    if (strcmp(p,ptab[i]) == 0) {
		param_id = i + 1;
		break;
	    }
	}
	switch (param_id) {
	    case 1:		/* ROTATE_FUNCTION */
		toupper_sw = 0;
		next_token (ml_rotfun);
		toupper_sw = 1;
		break;
	    case 2:		/* VALID_POINTS_PER_LOOK */
		get_int (&ml_vppl);
		break;
	    case 3:		/* LOOK_OFFSET */
		get_int (&ml_lkoff);
		break;
	    case 4:		/* OUTPUT */
		toupper_sw = 0;
		next_token (ml_output);
		toupper_sw = 1;
		break;
	    case 5:		/* OUTPUT_START_BLOCK */
		get_int (&ml_startblock);
		break;
	    case 6:		/* OUTPUT_BITS */
		get_int (&i);
		ml_outbits = (i == 32);
		break;
	    case 7:		/* OUTPUT_FRAME_DELAY */
		get_int (&ml_frdelay);
		break;
	    case 8:		/* FRAME_OFFSET */
		get_int (&ml_froff);
		break;
	    case 9:		/* AZIMUTH_LINE_LENGTH */
		get_int (&ml_azlen);
		break;
	    case 10:		/* OUTPUT_LENGTH */
		get_int (&ml_linelen);
		break;
	    case 11:		/* ASPECT_MODE */
		next_token (t);
		p_get_aspect (t,&ml_aspect);
		break;
	    case 12:		/* AVG_SEGSIZE */
		get_int (&ml_segsize);
		break;
	    case 13:		/* AVG_DIVISOR */
		get_int (&ml_divisor);
		break;
	    default:		/* unrecognizable parameter */
		printf("unrecognizable parameter: %s\n",p);
		ans = FAIL;
	}  /* switch */

	flush_line();
	return (ans);
}


/* p_get_mask (mask) ---------------------------------------------------
	This routine reads the next token, expecting a binary string in
	ASCII.  The string is converted into a 32-bit binary value and
	returned in mask.
*/

p_get_mask (mask)
	int *mask;
{
	char *s, t[80];

	next_token(t);
	*mask = 0;
	for (s = t; *s; s++)
	    *mask = (*mask << 1) | (*s == '1');
}


/* p_put_mask (val,t) --------------------------------------------------
	This routine formats the given value into an ascii-coded binary
	string in t.
*/

p_put_mask (val, t)
	int val;
	char t[];
{
	int i, m;

	for (i = 0, m = 1 << 30; m; m >>= 1)
	    if (i || (m & val))
		t[i++] = (m & val) ? '1' : '0';
	if (i == 0)
	    t[i++] = '0';
	t[i] = '\0';
}


/* p_get_aspect (t,aspect) ---------------------------------------------
	This routine reads the next token, and decodes it as one of 4
	possible aspect ratios.
*/

p_get_aspect (t,aspect)
	char *t;
	int *aspect;
{
	static char *tbl[] = { "2KX4K", "8KX1K", "16KX512", "8KX2K" };
	int i;

	if (param_io_sw) {
	    i = *aspect;
	    if (i < 0 || i > 4)
		i = 1;
	    strcpy(t,tbl[i]);
	}
	else {
	    for (i = 0; i < 4; i++)
		if (strcmp(t,tbl[i]) == 0) {
		    *aspect = i;
		    return;
		}
	    printf ("Unknown aspect ratio: forced to 8KX1K\n");
	    *aspect = 1;
	}
}


/* print_proc_param_file () --------------------------------------------
	This routine prints the currently stored processing parameters.
*/

print_proc_param_file ()
{
	ROPTR rp;

	printf ("SYSTEM: %d\n",sys_ckspeed);
	printf ("INPUT: %s\n",in_bypass);
	printf ("       %s\n",in_source);
	printf ("       %d\n",in_format);
	printf ("       %d\n",in_startblock);
	printf ("       %d\n",in_startline);
	printf ("       %d\n",in_startpoint);
	printf ("       %d.%d\n",in_blkoff,in_shift);
	printf ("       %d\n",in_norminv);
	printf ("       %s\n",in_trantbl);
	printf ("       %d\n",in_linelen);
	printf ("       %d\n",in_outlen);
	printf ("       %d\n",in_chirp);
	printf ("       %d/%d\n",in_vlines,in_ilines);
	printf ("       %d\n",in_scenelen);
	printf ("       %d\n",in_offch);
	printf ("       %d\n",in_offset1);
	printf ("       %d\n",in_offset2);
	printf ("       %d\n",in_swstart);
	if (Cur_Rqst->take_id[0] == 'J') {
	printf ("       %d\n",in_lenoff);
	printf ("       %s\n",in_jdfm_tbl);
	printf ("       %s\n",in_stc_wgt);
	}
	printf ("RANGE_COR: %d\n",rc_bypass);
	printf ("           %.4x\n",rc_fscale);
	printf ("           %.4x\n",rc_iscale);
	printf ("           %s\n",rc_reffun);
	printf ("           %d\n",rc_refrot);
	printf ("           %s\n",rc_rcfun);
	printf ("    special modes:\n");
	printf ("           %d%d%d\n",rc_refform,rc_refconj,rc_refrdat);
	printf ("           %d%d%d\n",rc_rcform,rc_rcconj,rc_rcrdat);
	printf ("           %d\n",rc_rcrot);
	printf ("CORNER_TURN: %d\n",ct_bypass);
	printf ("             %d\n",ct_outlen);
	printf ("             %d\n",ct_vlpf);
	printf ("             %d\n",ct_rdlen1);
	printf ("             %d\n",ct_rdlen2);
	printf ("             %d\n",ct_aspect);
	printf ("    special modes:\n");
	printf ("             %d\n",ct_unsrot);
	printf ("AZ_FWD_FFT: %d\n",af_bypass);
	printf ("            %.4x\n",af_scale);
	printf ("AZ_PROCESSING: %d\n",ap_bypass);
	printf ("               %d\n",ap_looks);
	printf ("               %d\n",ap_rmwt);
	printf ("               %d\n",ap_rmlines);
	printf ("               %s\n",ap_rmpath);
	printf ("               %d\n",ap_looklen);
	printf ("               %d\n",ap_lookpos);
	printf ("               %s\n",ap_xfun);
	printf ("               %.4x\n",ap_scale);
	printf ("               %d\n",ap_azwt);
	printf ("               %d\n",ap_detshift);
	printf ("               %d = %f\n",ap_ptspace,
					ap_ptspace/8192.0);
	printf ("               %s\n",ap_azstart);
	printf ("    special modes:\n");
	printf ("               %d\n",ap_rmform);
	printf ("               %d%d%d\n",ap_xform,ap_xconj,ap_xtran);
	printf ("               %d\n",ap_azform);
	printf ("MULTI_LOOK: %d\n",ml_bypass);
	printf ("            %s\n",ml_output);
	printf ("            %d\n",ml_startblock);
	printf ("            %d\n",ml_outbits);
	printf ("            %d\n",ml_frdelay);
	printf ("            %d\n",ml_froff);
	printf ("            %d\n",ml_azlen);
	printf ("            %d\n",ml_linelen);
	printf ("            %s\n",ml_rotfun);
	printf ("            %d\n",ml_vppl);
	printf ("            %d\n",ml_lkoff);
	printf ("            %d\n",ml_aspect);
	printf ("            %d\n",ml_segsize);
	printf ("            %d\n",ml_divisor);
	if (regov_list != NULL) {
	    printf ("REGISTER_OVERRIDES:\n");
	    for (rp = regov_list; rp != NULL; rp = rp->nxt_regov)
		printf ("    %.3x = %.4x\n",rp->addr,rp->value);
	}
}
/* p_load_regs() -------------------------------------------------------
	This routine loads the ASP hardware registers with the values
	specified by the parameter file variables.
	The routine returns PASS or FAIL.
*/

p_load_regs()
{
	ROPTR rp;
	int apow,ipow,istages,ibyp,ibyp2,ilook,ishift,rosign,ishake,ireq;
	int i,j,k,l,m,n,creg;
	int ilen, ioff1, ioff2, ipart;
	int tempshift,tempoff; /* Temp changes by Matt 10/19/92 */
	short int Data[32768], Data2[8192], Data3[4096], Data4[4096];
	short int Data5[4096], Data6[4096];
	int blkno,drivesel,rev;
	int agc_table;
	char string[132];

    /* disable ALLOWMLCSTOP 7-12-94 */

	asp_read( RLOC_OUT<<1, &mb.w[RLOC_OUT], 2 );
	creg = mb.w[RLOC_OUT];
	creg |= 0x0010;
	mb.w[RLOC_OUT] = creg;
	asp_write( RLOC_OUT<<1, &mb.w[RLOC_OUT], 2 );

	printf("loading ASP registers\n");
    /* exec cage */
	if (vbose)
	    printf ("setting up input configuration\n");
	fflush(stdout);
	ex_set_clock (sys_ckspeed);
/*SONY*/
	if ((strcmp (in_source,"DCRSI") == 0) || (strcmp (in_source,"SONY") == 0)) {
	    if (vbose) {
		printf ("  using tape input\n");
		printf ("  frame synchronizer\n");
		fflush(stdout);
	    }	

	    if (in_format == 1 || in_format == 2) {  /* for JERS or RSAT */
		if (first_odd != 0)	{	/* MRC* 10/19/92 */
		    tempshift = in_shift;
		    tempoff = in_blkoff - 4;
		}
	    }
	    else {
		tempshift = in_shift;
		tempoff = in_blkoff;
	    }
/*SONY*/
           if (!strcmp( media_type, "DCRSI" ) ) {
              drivesel=0;
              rev=0;
           }
           else {

                  tempoff -= 19;


                  drivesel=1; /*SONY*/
                  if (!strcmp( data_direction, "REVERSE" ) )
                    rev=1;
                  else
                    rev=0;
                }

	    setup_rpi (in_format,4,tempoff,tempshift,in_norminv,1,drivesel,rev);
	    if ( in_format == 1 ) /* for JERS */
	   	setup_jdfm(0x1111,3,0,1,1,0,dpf.fsm,dpf.maxfsdo,in_lenoff-24);
	    else if ( in_format == 2 ) { /* for RSAT */
                /* CV 3/18/97 setup_rpi() replace in_norminv = 1*/
                setup_rpi (in_format,4,tempoff,tempshift,1,1,drivesel,rev);
                rec = 1;
                rep_byte = in_chirp;
                echo_byte = in_linelen; /* Ming Test */
		setup_rdfm(0x111,rec,1,1,1,0,1,1,0,dpf.sbm,dpf.sbm,dpf.maxfsdo);
                play_dis(1);
                setup_rdfm_scs(0,rep_byte,echo_byte,0,0,0,0,0,0);
                play_dis(1);
                if (p_get_mem_file (Data2, 1024, "rdfm_tbl", "") == FAIL){
		   agc_table = r1_select_agc_table();
		   if (agc_table == FAIL) return (FAIL);
		   else {
		       sprintf(string,"rdfm_tbl.%d",agc_table);
		       if (vbose) printf("string=%s\n", string);
		   }
                   if (p_get_mem_file (Data2, 1024, DEF_PATH, string) == FAIL)
                       return (FAIL);
		}
                setup_rdfm_agcmem(Data2, Data2);
            /*    if (abob()) exit(1); */
	    }
	    else { /* for ERS */
                setup_edfm (dpf.sbm,dpf.fsm,1,dpf.maxfsdo-2);
                play_dis(1);
            }

            if ( in_format == 1 &&
                 (strcmp(in_stc_wgt,"BYPASS")) != 0 &&
                 (strcmp(in_jdfm_tbl,"BYPASS") != 0) ) {
              if (p_get_mem_file (Data, 8192, in_stc_wgt, "") == FAIL)
                    return (FAIL);
              if (p_get_mem_file (Data2, 2048, in_jdfm_tbl, "") == FAIL)
                    return (FAIL);
              setup_jdfm_mem (Data,Data2);
            }
	}
	else {
	    if (vbose) {
		printf ("  using file input\n");
		printf ("  loading input data to stimulus buffer...\n");
		fflush(stdout);
	    }
	    if (first_odd != 0) {
		if (vbose) printf("setup_rpi_test for clear_pipe\n");
		setup_rpi_test (0xffff,0,0,0,0xffffff);
	    }
	    else {
	    	if (vbose) printf ("clear_pipe without setup_rpi_test\n");
		asp_read( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
	    	creg = mb.w[RLOC_RPI];
	    	creg &= 0xffbf;
	    	mb.w[RLOC_RPI] = creg;
		asp_write( RLOC_RPI<<1, &mb.w[RLOC_RPI], 2 );
	    }
	}
	fflush(stdout);

    play_dis(1); /* Ming */

/*SONY*/
        if( strcmp( media_type, "DCRSI" ) ) 
            blkno = sony_get_addr(IN);
        else
            blkno = dc_get_addr(IN);

    printf ("at blk while loading reg. = %d\n", blkno);

    /* input interface */
	if (in_bypass == 0) {
	    if (vbose)
		printf("setting up input interface\n");
	    if (strcmp (in_source,"DCRSI") == 0)
		ibyp = 0xffff;
	    else 
		ibyp = 0x00ff;  /* bypass FIFO and FFT */
	    ibyp2 = 2;
	    if (strcmp(in_trantbl,"BYPASS") && strcmp(in_trantbl,"INTERNAL")) {
		ibyp2 = 3;
		if (p_get_mem_file (Data, 8192, in_trantbl, "") == FAIL)
		    return (FAIL);
		setup_iif_memory (Data);
	    }
	    ipart = (in_swstart != 0) ? 1 : 0;
	    setup_iif_cntrl (0, 0, 0, ibyp2, ibyp,0,ipart);
	    ipow = power_of_2(in_outlen);
	    ilen = in_linelen + in_swstart;
	    ioff1 = (8192 - in_swstart + in_offset1) & 0x1fff;
	    ioff2 = (8192 - in_swstart + in_offset2) & 0x1fff;
	    setup_iif_par (ilen,in_vlines,in_scenelen,in_offch,
	           ioff1,ioff2,pr_valpoints,pr_segments,ipow,in_ilines);
	}
	fflush(stdout);

    /* range correlator cage */
	if (rc_bypass == 0) {
	    if (vbose)
		printf("setting up range cage\n");

	    if (vbose)
		printf ("  forward fft\n");
	    ipow = power_of_2(in_outlen);
	    istages = 0x1fff >> (13 - ipow);
	    setup_fft("rf",istages,1,ipow,~rc_fscale);

	    if (vbose)
		printf ("  inverse fft\n");
	    setup_fft("ri",istages,0,ipow,~rc_iscale);

	    if (vbose)
		printf ("  forward umr\n");
	    if (strcmp(rc_reffun,"BYPASS")) {
		setup_umr("rf",0x1111,rc_refrot,1,0,rc_refform,
			   rc_refconj,rc_refrdat,1,ipow);
		if (strcmp(rc_reffun,"INTERNAL")) {
		    if (p_get_mem_file (Data, 16384, rc_reffun, "") == FAIL)
			return (FAIL);
		    setup_rref_memory ("rf",Data,1,1,0,ipow,ipow);
		}
	    }
	    else {
		setup_umr("rf",0x1101,rc_refrot,1,0,rc_refform,
			   rc_refconj,rc_refrdat,1,ipow);
	    }

	    if (vbose)
		printf ("  inverse umr\n");
	    if ((!strcmp(rc_rcfun,"BYPASS")) || (ucs_product == 1)){
		setup_umr("ri",0x1100,rc_rcrot,0,0,rc_rcform,
			   rc_rcconj,rc_rcrdat,0,ipow);
		printf("BYPASS rc_rcfun function\n");
	    } else {
		setup_umr("ri",0x1110,rc_rcrot,0,0,rc_rcform,
			   rc_rcconj,rc_rcrdat,0,ipow);
		if (strcmp(rc_rcfun,"INTERNAL")) {
		    if (p_get_mem_file (Data, 16384, rc_rcfun, "") == FAIL)
			return (FAIL);
		    setup_rc_memory ("ri",Data,ipow,pr_valpoints,pr_rglen,pr_segments);
		}
	    }
	}
	fflush(stdout);

    /* corner turn cage */
	ishake = (ap_bypass || (strcmp(ap_rmpath,"BYPASS") == 0));
	if (ct_bypass == 0) {
	    if (vbose)
		printf ("setting up corner turn cage\n");

	    if (vbose)
		printf ("  corner turn controller\n");
	    ipow = power_of_2(ct_outlen);
	    ibyp = (af_bypass) ? 0x100 : 0x111 ;
	    setup_ctc_cntrl (0,ibyp,0,0,0,ipow,ishake,0);
	    setup_ctc_par (ct_vlpf,ct_rdlen1,ct_rdlen2,pr_valpoints,
			   ct_unsrot);

	    if (vbose)
		printf ("  corner turn memory\n");
	    setup_ctm (ct_aspect,0,1,0,0);
	}
	fflush(stdout);
	if (af_bypass == 0) {
	    if (vbose)
		printf ("  azimuth forward fft\n");
	    ipow = power_of_2(ct_outlen);
	    istages = 0x1fff >> (13 - ipow);
	    setup_fft("af",istages,1,ipow,~af_scale);
	}
	fflush(stdout);

    /* azimuth cage */
	if (ap_bypass == 0) {
	    if (vbose)
		printf ("setting up azimuth cage\n");

	    if (vbose)
		printf("  range migration boards\n");
	    ibyp = (af_bypass) ? 0x0001 : 0x1111;
	    if (strcmp(ap_rmpath,"BYPASS") == 0)
		ibyp &= 0xff0f;
	    ipow = power_of_2(ct_outlen);
	    setup_rm (ibyp,0,pr_look,ap_rmform,~ishake,ipow,ap_rmwt);
	    if ((ibyp & 0x00f0) && strcmp(ap_rmpath,"INTERNAL")) {
		if (p_get_mem_file (Data , 8192, ap_rmpath, ".crs") == FAIL)
		    return (FAIL);
		if (p_get_mem_file (Data2, 8192, ap_rmpath, ".fin") == FAIL)
		    return (FAIL);
		if (p_get_mem_file (Data3, 2048, ap_rmpath, ".nkl") == FAIL)
		    return (FAIL);
		if (p_get_mem_file (Data4, 2048, ap_rmpath, ".2d") == FAIL)
		    return (FAIL);
		if (p_get_mem_file (Data5, 2048, ap_rmpath, ".a") == FAIL)
		    return (FAIL);
		setup_rm_mem (Data, Data2, Data3, Data4, Data5, ap_rmlines);
	    }

	    if (vbose)
		printf ("  azimuth transfer function\n");
	    if (strcmp(ap_xfun,"BYPASS") && strcmp(ap_xfun,"INTERNAL")) {
		if (p_get_mem_file (Data , 8192, ap_xfun, ".aw") == FAIL)
		    return (FAIL);
		if (p_get_mem_file (Data2, 2048, ap_xfun, ".th") == FAIL)
		    return (FAIL);
		if (p_get_mem_file (Data3, 2048, ap_xfun, ".bm") == FAIL)
		    return (FAIL);
		if (p_get_mem_file (Data4, 2048, ap_xfun, ".qd") == FAIL)
		    return (FAIL);
		if (p_get_mem_file (Data5, 2048, ap_xfun, ".wt") == FAIL)
		    return (FAIL);
		if (p_get_mem_file (Data6, 2048, ap_xfun, ".ls") == FAIL)
		    return (FAIL);
	    }
	    ipow = power_of_2(ct_outlen);
	    ibyp = pr_look ? 0x11111 : 0x01111;
	    if (strcmp(ap_xfun,"BYPASS") == 0)
		ibyp &= 0x0000f;
	    rosign = (Data3[0] >> 15) & 0x0001;
	    setup_xfer_cntrl (ibyp,rosign,pr_look,0,ap_xtran,ap_xconj,
				ap_xform,0,ipow);
	    if (!pr_look)
		ap_lookpos = 0;
	    setup_xfer_par (ap_looklen, ap_lookpos);
	    if (strcmp(ap_xfun,"BYPASS") && strcmp(ap_xfun,"INTERNAL"))
		setup_xfer_mem (Data,Data2,Data3,Data4,Data5,Data6,pr_look,ct_outlen);

	    if (vbose)
		printf ("  azimuth inverse fft\n");
	    ipow = power_of_2(pr_aifftlen);
	    istages = 0x1fff >> (13 - ipow);
	    setup_fft ("ai",istages,0,ipow,~ap_scale);

	    if (vbose)
		printf ("  azimuth interpolator\n");
	    ibyp = (ap_detshift == -1) ? 0x110 : 0x111;
	    ishift = (ap_detshift == -1) ? 0 : ap_detshift;
	    ibyp2 = (ap_looks == 4) ? 0x11 : 0x10 ;
	    if (strcmp(ap_azstart,"BYPASS") == 0)
		ibyp2 = 0x10;
	    setup_az (ibyp,ap_azform,0,ibyp2,1,ishift,ipow,ap_azwt,ap_ptspace);
	    if (strcmp(ap_azstart,"BYPASS") && strcmp(ap_azstart,"INTERNAL")) {
		if (p_get_mem_file (Data2, XBUFSIZE, ap_azstart, "") == FAIL)
		    return (FAIL);
		setup_az_mem (Data2);
	    }
	}
	fflush(stdout);

    /* multilook cage */
	if (ml_bypass == 0) {
	    if (vbose)
		printf ("setting up multilook cage\n");

	    if (vbose)
		printf ("  multilook controller\n");
	    setup_mlc_cntrl (1,0,0,1,0,0);
	    ireq = (strcmp (ml_output,"DCRSI") == 0) ? 1 : 0;
	    j = (ml_aspect != 3);
	    i = (ml_segsize == 0) ? 0xf0 : 0x00;

	    ibyp = 0x010;
	    if (pr_look)  /* activate square root for 4 looks */
		ibyp |= 0x001;
	    if ((strcmp(ml_rotfun,"BYPASS") != 0) && (pr_look))
		ibyp |= 0x100;
	    setup_mlc_mode (ibyp,ml_outbits,i,j,pr_look,0,0,ireq);
	    i = ml_azlen - ml_froff;
	    ipow = power_of_2(ml_azlen);
	    setup_mlc_par (i,ipow,ml_lkoff,ml_froff,ml_linelen,
			   ml_frdelay,ml_vppl);
	    if (strcmp(ml_rotfun,"BYPASS") && strcmp(ml_rotfun,"INTERNAL")) {
		if (p_get_mem_file (Data, 1024, ml_rotfun, "") == FAIL)
		    return (FAIL);
		setup_mlc_mem (Data);
            }

	    if (vbose)
		printf ("  multilook memory\n");
	    if (pr_look)
		setup_mlm (ml_aspect,1,1,0,ml_froff);
            else {   /* no azimuth offset in one look */
		setup_mlm (ml_aspect,0,1,0,0);
}

	    if (ml_segsize) {
		if (vbose)
		    printf ("  averager board\n");
		apow = 6 - power_of_2(ml_divisor);
		if (apow < 0)
		    apow = 0;
		setup_avg (0x1111,0,0,1,0,apow,0,ml_segsize,ipow);
	    }
	}
	fflush(stdout);

    /* register overrides */
	i = 0;
	for (rp = regov_list; rp != NULL; rp = rp->nxt_regov) {
	    mb.w[rp->addr >> 1] = rp->value;
	    asp_write( rp->addr, &mb.w[rp->addr>>1], 2 );
	    i++;
	}
	if (i)
	    if (vbose)
		printf ("%d register overrides written\n",i);
	fflush(stdout);
	return (PASS);
}


/* power_of_2 (n) ------------------------------------------------------
	This routine returns the exponent of 2 which makes 2**exp equal
	to or just higher than n.
*/

power_of_2 (n)
	int n;
{
	register int p = 0;
	register int i = 1;

	if (n < 0)
	    n = -n;
	while (i < n) {
	    p++;
	    i <<= 1;
	}
	return (p);
}


/* p_get_avg_image (nlines, len, filename) -----------------------------
	This routine retrieves the developing 8x8 averaged image being
	produced by the averager board, and stores it in a dedicated
	memory array.  Then the routine copies the image to disk,
	inverting the image and adding the imagery options file
	descriptor record to the front.

	len is the line length, expressed in data points (single bytes).

	If 'filename' is NULL, the data is copied to the mb array
	starting at 0x8000, for 0x38000 bytes.
*/

int p_get_avg_image (nlines, len, filename)
	int nlines, len;
	char *filename;
{
	short int *b, *p;
	int i, r, o, imgsize, imgend, bufsize, addr, addrend, ab;
	int memsize;
	unsigned char *ifdr;
	I_FDR_FILE  *ip;
	FILE *fp, *fopen();
	int seq_number, zp;
	int count, creg, n, total, expect, hist[256];
	u_char *pix;
	int no_count = 0;

	asp_read( (RLOC_AVG+3)<<1, &mb.w[RLOC_AVG+3], 2 );
	bufsize = mb.w[RLOC_AVG + 3] & 0xffff;  /* expressed in words (two points) */
	imgsize = nlines * len;        /* expressed in points (bytes) */
    /* get a dedicated memory array to hold the image */
	memsize = imgsize;
	printf("memsize = %d, len = %d, nline = %d\n", memsize,len,nlines);
	fflush(stdout);
	if ((b = malloc(memsize)) == NULL) {
	    printf ("cannot allocate enough memory for lo_res image\n");
	    return (1);
	}
    /* retrieve the image from the ASP AVG board buffers */
	addrend = imgsize % (bufsize * 2);
	imgsize >>= 1;   /* convert to 16 bit words */
	imgend = imgsize - bufsize + 1;
	ab = 1;
	if (vbose)
	    printf("...capturing %d words\n",imgsize);
	    fflush(stdout);
    /* look for scene start */
	count = 0;
	i = 0;
	while (count<200000 && i == 0) {
	    asp_read( (RLOC_AVG+1)<<1, &mb.w[RLOC_AVG+1], 2 );
	    creg = mb.w[RLOC_AVG + 1];
    	    i = (creg & 0x0080) == 0;
	    if ((count%10000)==0) {
		printf("from asp_read creg=%x,i=%d\n",creg,i);
                no_count += 1;  /* CV 9/27/97 return instead of continue */
                if (no_count == 5) {
                   no_count = 0;
                   return(FAIL);
                }
	    }   
/*
	    get_avg_stat(&i,&r,&o);
*/
	    usleep( 100 );
	    count++;
	}
	printf("out of while, i=%d, count=%d\n",i,count);
	if (i == 0) {
	    printf("timeout in look for scene start\n");
	    /* if (abob()) exit(1); */
	}
    /* look for buffer ready and read buffer */
	printf("start getting output ...\n");
	for (p = b; p < b + imgend; p += bufsize) {
	    count = 0;
	    r = 0;
	    while (count<15000 && r == 0) {
	    	get_avg_stat(&i,&r,&o);
		usleep( 1000 );
		count++;
	    }
	    printf("out of while, r=%d, count=%d\n",r,count);
	    if (r == 0) {
		printf("timeout in look for buf ready\n");
		/* if (abob()) exit(1); */
	    }
            if (o) printf ("Overflow in averager!\n");
	    get_avg_mem(p,bufsize);
	    if (vbose){
		printf("...got %d words\n",bufsize);
		fflush(stdout);
	    }
	    ab = !ab;
	}
    /* retrieve partial buffer at end of image */
	if (addrend) {
	    for (addr = 0; addr < addrend; get_avg_addr(&addr))
		;
	    avg_set_reset(1);
	    set_avg_wrmode(ab | 0x010);
	    get_avg_mem(p,(addrend + 1) / 2);
	    if (vbose)
		printf("...got %d words in last buffer\n",
			(addrend + 1) / 2);
	}				/* end getting avg buffer */
	set_avg_wrmode(0x001);		/* unfreeze buffers */

  /* if no filename, copy data to the mb array */
printf("saving to local memory imagesize=%d\n", imgsize);
	if (filename[0] == '\0') {
	    bcopy( b, &mb.w[0x8000],
		(imgsize*2>0x40000-0x8000) ? (0x40000-0x8000) : imgsize*2 );
	    free (b);
	    return (0);
	}

/* truncate file to 1024 by 1024 */
	if( nlines>1024 ) nlines = 1024;
	imgsize = nlines*len/2;

/* calculate zero pixel area */
	r = ap_rmlines / 8;
	if( r==1024 ) r = 1023;
	if( strcmp(filename, "avg_image")==0 && len>r )
		zp = r;
	else
		zp = 0;

    /* Generate histogram for low-res image */

	bzero( (char *) hist, sizeof( hist ) );
	pix = (u_char *) b;
	for( i=0; i<nlines; i++ ){
	    if( zp>0 ) bzero( pix+zp, len-zp );
	    for( n=0; n<r; n++ ) hist[ pix[n] ]++;
	    pix += len;
	}
	for ( i=0, total=0; i<256; i++ ) total += hist[i];
	expect = total - ((sf.nl+7)/8)*r;
	if ( expect < 0 )
		printf("histogram total count is lower than expected: %d\n", 
			total);
	else if ( expect > 0 )
		hist[0] -= expect;
	if ( hist[0] < 0 ) hist[0] = 0;
	p_write_mem_file(hist,512,"hstgrm",".lr","Low-res histogram");

  /* write the image to disk */
	printf("Writing file %s...\n",filename);
	if ((fp = fopen (filename,"w")) == NULL) {
	    printf ("cannot open lo-res output file %s\n",filename);
	    free (b);
	    return (1);
	}
	ifdr = (unsigned char *) malloc(len+CEOS_PREFIX_SIZE);
	ip = (I_FDR_FILE *) ifdr;
	i_fdr_cmp(ifdr,len);
    /* write the imagery options file descriptor record */
	fwrite(ifdr,1,len+CEOS_PREFIX_SIZE,fp);

	i_prefix_cmp( ifdr );	/* prefill prefix area */
	seq_number = 1;
	if (spf.side == 'R')	/* reverse record order for right-looking */
	    for (p = b + imgsize - len/2; p >= b; p -= len/2){
                i = ( htonl( seq_number ) );
                bcopy( &i, &ifdr[12], 4 );
		ip->seq_number = htonl( ++seq_number );
		fwrite (ifdr,1,CEOS_PREFIX_SIZE,fp);
		fwrite (p,1,len,fp);
	    }
	else{			/* straight record order for left-looking */
	    for (p = b; p < b+imgsize; p += len/2){
                i = ( htonl( seq_number ) );
                bcopy( &i, &ifdr[12], 4 );
                ip->seq_number = htonl( ++seq_number );
		fwrite (ifdr,1,CEOS_PREFIX_SIZE,fp);
		fwrite (p,1,len,fp);
	    }
	}
	fclose (fp);
	free(ifdr);
	free (b);
	return (0);
}

/* p_chk_img_qual (filename) -------------------------------------------
	This routine retrieves the histogram and overflow data from the
	ASP hardware at the completion of processing, writes that data
	to a disk file, and then checks the data for any problems.  If
	there were any problems, they are reported on the screen.
*/

p_chk_img_qual (filename)
	char *filename;
{
	FILE *fp;
	static char *nm[4] = {"rf", "ri", "af", "ai"};
	static char *NM[4] = {"RF", "RI", "AF", "AI"};
	static char *name[4] = {"range forward",
				"range inverse",
				"azimuth forward",
				"azimuth inverse"
				};
	int i,j;
	int oflo[4][13];
	int fftbyp[4];
	int error = 0;
	int ctcoflo, rmoflo, mlcoflo, avgoflo;
	int hist[256];

    /* overflow counters on all fft modules */
	fftbyp[0] = fftbyp[1] = rc_bypass;
	fftbyp[2] = af_bypass;
	fftbyp[3] = ap_bypass;
	for (i = 0; i < 4; i++) {
	    if (fftbyp[i] == 0) {
		get_fft_oflo_regs(nm[i],oflo[i]);
		for (j = 0; j < 13; j++)
		    if (oflo[i][j]) {
			printf("%s fft overflowed on stage %d\n",name[i],j+1);
			error = 1;
		}
	    }
	}
    /* corner turn memory buffer overflow */
	if (ct_bypass == 0) {
	    get_ctc_stat(&i,&ctcoflo);
	    if (ctcoflo) {
		printf ("buffer overflow on corner turn memory\n");
		error = 1;
	    }
	}
    /* range migration buffer overflow */
	if (ap_bypass == 0) {
	    get_rm_stat(&rmoflo,&i,&j);
	    if (rmoflo) {
		printf ("buffer overflow on range migration memory\n");
		error = 1;
	    }
	}
    /* multi-look memory buffer overflow */
	if (ml_bypass == 0) {
	    get_mlc_stat(&i,&mlcoflo);
	    if (mlcoflo) {
		printf ("buffer overflow on multi-look memory\n");
		error = 1;
	    }
	}
    /* averager buffer overflow */
	if (ml_bypass == 0) {
	    get_avg_stat (&i,&i,&i,&avgoflo);
	    if (avgoflo) {
		printf ("buffer overflow on averager\n");
		error = 1;
	    }
	}
    /* retrieve histogram */
	get_mlc_hist(hist);
    /* write the error/histogram data to disk */
	if ((fp = fopen(filename,"w")) == NULL) {
	    printf ("cannot open output file %s\n");
	    return;
	}
	fprintf (fp,"ERRORS=%c\n",error ? 'Y' : 'N');
	for (i = 0; i < 4; i++) {
	    fprintf (fp,"%s_OVERFLOW=",NM[i]);
	    for (j = 0; j < 12; j++)
		fprintf (fp,"%d,",oflo[i][j]);
	    fprintf (fp,"%d\n",oflo[i][13]);
	}
	fprintf (fp, "CTC_OVERFLOW=%c\n",ctcoflo ? 'Y' : 'N');
	fprintf (fp, "RM_OVERFLOW=%c\n",rmoflo ? 'Y' : 'N');
	fprintf (fp, "MLC_OVERFLOW=%c\n",mlcoflo ? 'Y' : 'N');
	fprintf (fp, "AVG_OVERFLOW=%c\n",avgoflo ? 'Y' : 'N');
	fprintf (fp, "$$512\n");
	fwrite (hist, sizeof(int), 256, fp);
	fclose (fp);
}
/* build_image_files (sp, pp, seg, region) -------
*/
build_image_files (sp, pp, seg, region)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	int seg, region;
{
	int ans,i;

/*	if (Cur_Rqst->take_id[0] == 'R'){

	ans = AspReadCalFile(); 
	if (ans == NULL) 
	    printf("Unable to read cal_file, ans=%d \n", ans);
	for (i=0; i<30; i++)
		printf("gain_value[%d]=%g\n",i,gain_value[i]);*/

	switch ( Cur_Rqst->take_id[0] ){
	case 'E':
		return(e1_build_image_files( sp, pp, seg, region ));
	case 'J':
		return(j1_build_image_files( sp, pp, seg, region ));
	case 'R':
		return(r1_build_image_files( sp, pp, seg, region ));
	default:
		return(FAIL);
	}
}
/* update_pp (sp, pp, odd, seg, region) ----------------------------------------
*/
int update_pp(sp, pp, odd, seg, region )
        TAPE_SEG_PTR sp;
        PP_BLOCK_PTR pp;
	int odd, seg, region;
{
	int status = PASS;

	if (pre_done == 0) {
		if (odd == 0) 
			status = build_image_files(sp,pp,seg,region);
		if (status == FAIL) {
	    		asp_msg(LOG_DEBUG,"Error in preprocessing");
			return (FAIL);
		}
   		chdir(JOB_PATH);
		p_rw_pp_file(1,"pp_regions",0);
		chdir(pp->img_name);
	}
	else {
		if (odd == 0 && do_binary == 1) {
			status = build_image_files(sp,pp,seg,region);
		  	if (status == FAIL) {
	    			asp_msg(LOG_DEBUG,"Error in preprocessing");
				return (FAIL);
			}
		}
	}
}
/* chk_2dfft (img_id) ---------------------------------------------
*/
int chk_2dfft(char *img_id)
{
	int status = PASS;
	FILE *op;

	if (!cp_flag && pre_done) {
		if ( (op = fopen("avg_2dfft","r")) != NULL){
		   printf("\nPreprocessing has been done for %s\n",img_id);
		   status = op_answer("Do you wish to repeat"); 
		}
		fclose(op);
	}
	return(status);
}
/* chk_image (img_id) ---------------------------------------------
*/
int chk_image(char *img_id)
{
	int status = PASS;
	FILE *op;

	if (!cp_flag && pre_done) {
		if ( (op = fopen("avg_image","r")) != NULL){
		   printf("\navg_image has been generated for %s\n",img_id);
		   status = op_answer("Do you wish to repeat"); 
		}
		fclose(op);
	}
	return(status);
}
/* proc_2dfft (sp, pp, region) ---------------------------------------------
*/
int proc_2dfft(sp, pp, region )
        TAPE_SEG_PTR sp;
        PP_BLOCK_PTR pp;
	int region;
{
	int status = PASS;
	int block, offset;

	report_asp_status(ASP_PREPROC);
	if ( asphw ) {
		AspDisableKill();
	        switch ( Cur_Rqst->take_id[0] ){
	        case 'E':
	                status = e1_2dfft(sp,pp);
			break;
	        case 'J':
	                status = j1_2dfft(sp,pp);
			break;
	        case 'R':
	                status = r1_2dfft(sp,pp,region);
			break;
	        default:
	                status = FAIL;
			break;
	        }
		AspEnableKill();
	}
        if (status != PASS) {
                printf("Error in 2DFFT, region %d is bad\n", region);
/*SONY*/
                if ( asphw ) {
                  if( strcmp( media_type, "DCRSI" ) )
                      sony_stop(IN);
                  else
                      dc_stop(IN);
                }

                printf("Try to re-sync ... \n");
                first_odd = 2;
                if ( Cur_Rqst->take_id[0] == 'J' ){
			j1_get_fmt_loc(1,sp,pp->fmt_start,&fst_len,
                                                &block,&offset);
			/* overwrite polarity 8/22/96 */
			pp->polarity = sp->polarity; 
                        pp->blk_start = block;
                        pp->bit_off = offset;
                }
		if ( asphw ) {
			AspDisableKill();
		        switch ( Cur_Rqst->take_id[0] ){
		        case 'E':
		                status = e1_2dfft(sp,pp);
				break;
		        case 'J':
		                status = j1_2dfft(sp,pp);
				break;
		        case 'R':
		                status = r1_2dfft(sp,pp,region);
				break;
		        default:
		                status = FAIL;
				break;
		        }
			AspEnableKill();
		}
                if (status != PASS) {
                        printf("*** QC ***:DOUBLE FAULT in 2DFFT\n");
   			chdir(JOB_PATH);
			p_rw_pp_file(1,"pp_regions",0);
			chdir(pp->img_name);
                }
        }
	return(status);
}
/* proc_ltrr (sp, pp, nfmts) ---------------------------------------------
*/
int proc_ltrr(sp, pp, nfmts)
        TAPE_SEG_PTR sp;
        PP_BLOCK_PTR pp;
        int nfmts;
{
	int status = PASS;

	if ( !asphw ) return(status);
	AspDisableKill();
        switch ( Cur_Rqst->take_id[0] ){
        case 'E':
                status = e1_ltrr(sp,pp,nfmts);
		break;
        case 'J':
                status = j1_ltrr(sp,pp,nfmts);
		break;
        case 'R':
                status = r1_ltrr(sp,pp,nfmts);
		break;
        default:
                status = FAIL;
		break;
        }
	AspEnableKill();
	return(status);
}
/* proc_hwdata (sp, pp, in_mode) ---------------------------------------------
*/
int proc_hwdata(sp, pp, in_mode)
        TAPE_SEG_PTR sp;
        PP_BLOCK_PTR pp;
        int in_mode;
{
	int status = PASS;

	if (!asphw) return(status);

	AspDisableKill();
        switch ( Cur_Rqst->take_id[0] ){
        case 'E':
                status = e1_get_hwdata(sp,pp,in_mode);
		break;
        case 'J':
                status = j1_get_hwdata(sp,pp,in_mode);
		break;
        case 'R':
                status = r1_get_hwdata(sp,pp,in_mode);
		break;
        default:
                status = FAIL;
		break;
        }
	AspEnableKill();
	return(status);
}
/* proc_image (sp, pp, region, image_id) -------------------------------
*/
int proc_image(sp, pp, region, image_id )
        TAPE_SEG_PTR sp;
        PP_BLOCK_PTR pp;
	int region; 
	char *image_id;
{
	int status = PASS;
	int block, offset;

	report_asp_status(ASP_PROC);
        if ( asphw ) {
		AspDisableKill();
		status = run_image(sp,pp,"params.proc",image_id);
		AspEnableKill();
	}
        if (status != PASS) {
            printf("ERROR in run_image, region %d is bad\n", region);
/*SONY*/
            if ( asphw ) {
                if( strcmp( media_type, "DCRSI" ) )
                  sony_stop(IN);
                else
                  dc_stop(IN);
            }
            printf("Try to re-sync ... \n");
            first_odd = 2;
            if ( Cur_Rqst->take_id[0] == 'J' ){
			j1_get_fmt_loc(1,sp,pp->fmt_start,&fst_len,
                                                &block,&offset);
                        pp->blk_start = block;
                        pp->bit_off = offset;
            }
            if ( asphw ) {
		AspDisableKill();
                status = run_image(sp,pp,"params.proc",pp->img_name);
		AspEnableKill();
	    }
            if (status != PASS) {
                printf("** QC **: DOUBLE FAULT in run_image\n");
            }
        }
	if ( asphw ) p_rw_scene_file(1,"scene_file");
	return(status);
}
/* p_get_full_image 
*/
p_get_full_image( image_id )
	char *image_id;
{
	int i, status;
	char string[132], outfname[132], rec_type;
	int img_lines, img_reclen;
	int pid, ofd;
	unsigned char *ifdr;

	printf("Capture image id %s\n", image_id );
	chdir( JOB_PATH );
        op_mkdir( image_id );
        chdir ( image_id );
        if ( !strcmp(Cur_Rqst->type,"CPX") ){
             img_reclen = -IMG_RECLEN;
	     img_lines = CPX_IMG_LINES;
             sprintf(string,"%sCPX",FULL_IMG_PATH);
	     rec_type = 11;
        }
        else if ( !strcmp(Cur_Rqst->type,"CSD") ){
             img_reclen = CSD_IMG_RECLEN;
             img_lines = CSD_IMG_LINES;
             sprintf(string,"%sCSD",FULL_IMG_PATH);
	     rec_type = 10;
        }
        else {
             if (!strcmp(frame_mode,"ANTARCTIC"))
                img_reclen = IMG_RECLEN;
             else
                img_reclen = -IMG_RECLEN;
	     img_lines = STD_IMG_LINES;
             sprintf(string,"%sRPR",FULL_IMG_PATH);
	     rec_type = 11;
        }
        strcpy( outfname, "full_image" );
        if ( access( outfname, F_OK ) == FAIL ){
             	for ( i = 1; i <= MAX_COPY; i++ ){
                   sprintf(outfname,"%s/%d",string,i);
                   if ( (status=access( outfname, F_OK )) == PASS )
                               break;
             	}
        }
        if ( status == FAIL ){
		asp_msg(LOG_ERR,asp_messages[OUT_OF_FULL_IMAGE]);
	     	return( FAIL );
        }
        if ( rename( outfname, "full_image" ) == FAIL) {
		printf("Cannot rename %s to full_image\n", string);
		return(FAIL);
	}

	/* Write ceos header */

	ifdr = (unsigned char *) malloc(abs(img_reclen)+CEOS_PREFIX_SIZE);
	i_fdr_cmp(ifdr,abs(img_reclen));
	ofd = open( "full_image", O_WRONLY, 0644 );
	if ( ofd == FAIL ) {
/*		printf("Error in openning file full_image\n");*/
		asp_msg(LOG_ERR,asp_messages[UNOPEN_FULL_IMAGE]);
	     	return( FAIL );
        }
	write( ofd, ifdr, abs(img_reclen)+CEOS_PREFIX_SIZE );
	close( ofd );
        i_prefix_cmp( ifdr );
/*
	ifdr[4] = 50;
	ifdr[5] = 10;
	ifdr[6] = 18;
	ifdr[7] = 20;
*/
	if ( asp_get_shm() == FAIL ) return(FAIL);
	shm_flags->get_outb = 1;
	pid = fork();
	if ( pid == 0 ) {
        	status = get_outb("full_image", img_lines, img_reclen,
				CEOS_PREFIX_SIZE, ifdr );
		shm_flags->get_outb = status;
		printf("Finish capturing image (%s) data.\n",image_id);
printf("P_GET_FULL_IMAGE exiting, SHM_FLAGS->GET_OUTB=%d\n",
shm_flags->get_outb );
		exit(status); 
	}
	free(ifdr);
	return( pid );

} /* p_get_full_image */
