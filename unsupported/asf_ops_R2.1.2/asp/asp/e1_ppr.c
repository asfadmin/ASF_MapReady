/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* e1_proc_ppr.c -- ERS-1 process a preprocessing region
		    Using 2DFFT for Doppler measurement.
		    There is no azimuth scaling.
   Ming Chen, 10/23/92.
*/

#include <aspdecl.h>
#include <procfil.h>
#include <procdec.h>
#include <procpar.h>
#include <scene_file.h>
#include <stdio.h>
#include <math.h>

/* ERS-1 header data format definitions */
#define HDR_LEN (92 >> 1) /* # of bytes of header stored per format */
#define HDR_LEN2 (20 >> 1) /* bytes of header per format (short mode) */
#define AUX_LOC (2 >> 1)  /* start location of auxiliary data */
#define AUX_LEN (18 >> 1)  /* length of auxiliary data */
#define REP_LOC (20 >> 1) /* start location of replica/drift data */
#define REP_LEN (72 >> 1)  /* length of replica/drift data */
#define REP_LAST (24 >> 1)  /* length of replica/drift last line */

/* sync code bits */
#define FS 0x01
#define VL 0x02
#define VF 0x04
#define AL 0x08
#define SS 0x10

#define SYNC_EVENT 0
#define COUNT_EVENT 1
#define STATLEN 50		/* length of the status register */

/* hardware definitions */
#define MEND_EXEC 0xfffff  /* end of Exec response buffer page */
#define RMODE_MEM_READ 0   /* exec response buffer read mode */

extern alarm;			/* 1 = timer alarm has gone off */
extern f_dropout;		/* 1 = force dropout */

extern char ASP_VERSION[8];	/* software version number */

extern first_odd;		/* 1 = first region in a pass */
extern first_ident;		/* 1 = first time touch DCRSi */
extern do_repmeas;		/* 1 = perform repmeas */

/* data from current job request */
extern RQST_PTR Cur_Rqst;	/* pointer to current job request */
extern SV sv1,sv2;		/* state vectors */

/* Parameter and data files */
extern SP_FILE spf;		/* sensor parameters file */
extern DPP_FILE dpf;		/* default processing parameters file */
extern EM_FILE emf;		/* earth model file */
extern SCENE_FILE sf;		/* scene file */

extern TAPE_SEG_PTR seg_list;	/* tape segment list */

float fmt_ratio = 1.0;		/* actual formats / valid formats */

/* ASP configuration parameters */
extern int vbose;		/* 1 = print progress messages */
extern int sw_dcrsi_in;		/* 1 = use input dcrsi */
extern int sw_noise_meas;	/* 1 = perform noise measurement */
extern int sw_rep_meas;		/* 1 = perform replica measure */
extern int sw_snr_meas;		/* 1 = perform SNR measurement */
extern int sw_clutter_lock;	/* 1 = perform clutter lock */
extern int sw_ambig;		/* 1 = perform ambiguity determ. */
extern int sw_auto_focus;	/* 1 = perform auto-focus */
extern int sw_auto_scale;	/* 1 = adjust fft scaling
					automatically */
extern int sw_pp_only;		/* 1 = preprocssing only */

extern int pre_dwp;

enum { LOW_RES, FULL_RES, BOTH_IMAGE };
extern int Res_type; 

int first_region = 1;	/* 1 = first pp region in data take */
int repoff = 8;		/* offset (0 to 23) to replica start */

double r3p[3];		/* 3 ranges for corn_loc routine */
float fd3p[3];		/* 3 corresponding fd values */
float fr3p[3];		/* 3 corresponding fr values */
float fdg[3],fdotg[3];		/* ground Doppler coef */

int pulse_delay;		/* # of prf lines delay to first 
					output pixel */
int frdelay_sw = 0;	/* 1 = clamp frame delay to 8 */

int wdchk = 1;			/* counter for window change */
int fmt_tobe;			/* variable to trace format number */
int dropout;                    /* 1 = dropout occurred in play */

extern int detshift_old;	/* pass set_gain result to even image */
extern int pre_done;
extern int cp_flag;
extern float gaintbl[900];

/*SONY*/
extern char media_type[];
extern char data_direction[];

double get_gmt_diff();


/* ======================= for 2 pass ==============================*/

/* e1_2dfft(sp,pp,region) -------------------------------------------
	This routine determines the Doppler frequency by 2DFFT
*/

e1_2dfft(sp,pp,region)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	int region;
{
#undef NFMTS
#define NFMTS 512
	int sync_code, i, j, k, l, rpt, rpt3, rpt23;
	char id[STATLEN], filename[80];
	char *strcpy();
	int first = 1;
	int backup = 0;
	int dop[3][128], rng[1024], p_noise[3], p_sig[3];
	int read_rcdr;
	int del_j, cut_j, energy, half, init_center;
	int noise_1, noise_2, sig_1, sig_2, estat[3], ans;
	float percent, mhz_bin, snr[3], dop_f[3], tempfd[3], newfd[3];
	float etot, dop_avg[3];
	int file_mj;
	int nfmts, clock_gain, blkno;
	short int data_2d[49152];

    	read_rcdr = IN;

    /* if preprocessing only, do fine tune LTRR */
        if (sw_pp_only == 1 && first_odd == 0) {
    	    if (vbose) printf("LTRR ... again\n");
	    nfmts = pp->fmt_id - fmt_tobe - 12000;
	    if (nfmts < 40) {
	        printf("nfmt = %d, No tape forword\n", nfmts);
	        return(FAIL);
	    }
	    nfmts = 8 * (int)((nfmts+4)/8.);
	    if (e1_ltrr(sp,pp,nfmts) == FAIL) return(FAIL);
	    fmt_tobe = pp->fmt_id;
	}

	if (do_repmeas == 1) {
	    if (vbose) printf("Perform replica measurement!\n");
  	    if (first_odd != 0) {
	    	printf ("Initializing READ recorder...\n");
	    	if (rcdr_start (read_rcdr) == FAIL)
		    return (FAIL);
	    }
	    if (first_odd != 0) {
	    	reset_rpi(1);
	    	fmt_tobe = pp->fmt_id;
	    }

	    reset_edfm(1,1);
	    reset_edfm(0,0);

	/* setup and process 312 lines thru the range fwd fft */
	    strcat(strcpy(filename,DEF_PATH),"params.sigms");
	    if (read_proc_param_file(filename) == FAIL) {
		printf("...unable to read params.sigms file\n");
		return (FAIL);
	    }
	    /* to overwrite BAD setup in params.sigms */
	    in_norminv = pp->polarity;
	    rc_fscale = pp->fftsc_raf;
	    in_blkoff = pp->bit_off / 8 ;
	    in_shift = pp->bit_off % 8;
	    if (p_load_regs() == FAIL) {
		printf("...register load failed\n");
		return (FAIL);
	    }
	    setup_umr("rf",0x1100,0,1,1,0,0,0,0,11);
	    setup_edfm(dpf.sbm,dpf.fsm,1,dpf.maxfsdo);
	    ex_set_selftest(0);
	    ex_set_delay(8192 * 2 * 312);
	    sync_code = ~(SS | FS | VF | AL | VL) & 0xff;
	    ex_set_event(SYNC_EVENT,0x1f,sync_code,1);
	    ex_set_reset(0);
	    play_dis(1);
	    iif_set_reset(1);
	    iif_set_reset(0);

	/* check recorder identities */
	/* do input only for the very first time play DCRSi ??? */
    	    if (first_ident == 1) {
      	        first_ident = 0;
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
    	    }  /* if first ident */

	    clear_fft_oflo_regs ("rf"); /* Clear FFT overflow flags */

	/* if first region, start the DCRS at the right location */
	    if (first_odd != 0) {
	    	/* if (vbose) */
/*SONY*/
                    if (!strcmp( media_type, "DCRSI" ) ){ 
	    	      printf("starting DCRSi at %d, offset %d, polarity %d\n",
			    pp->blk_start,pp->bit_off,pp->polarity);
	    	      start_fmt_play(0,pp->blk_start,pp->bit_off,pp->polarity);
                    }
                    else {
	    	         printf("starting SONY at %d, offset %d, polarity %d\n",
			    pp->blk_start,pp->bit_off,pp->polarity);
                         if (!strcmp( data_direction, "REVERSE" ) )
	    	          sony_start_fmt_play_REV(0,pp->blk_start,pp->bit_off-152,pp->polarity);
                         else
	    	          sony_start_fmt_play(0,pp->blk_start,pp->bit_off-152,pp->polarity);
                         }

		pre_done = 0;
	    	reset_rpi(0);
		first_odd = 0;
	    }

	/* arm and trigger */
	    e1_clear_hdr_mem(0);
	    tarm();
	    play_dis(0);
	    tdone();
	    play_dis(1);

	/* at this point, the data should be captured */
	    fmt_tobe += 312; /* we play 312 formats for repmeas */
	    if (vbose) {
		printf("\n...in repmeas after trigger, fmt = %d\n", fmt_tobe);
	    }

/*SONY*/
            if (!strcmp( media_type, "DCRSI" ) ) 
               blkno = dc_get_addr(IN);
            else
               blkno = sony_get_addr(IN);

    printf ("at blk after repmeas = %d\n", blkno);

	/* check dropout bit */
	    asp_read( (RLOC_EDFM+3)<<1, &mb.w[RLOC_EDFM+3], 2 );
	    if ((mb.w[RLOC_EDFM+3] & 1) == 1) {
		printf("dropout detected in sig_meas\n");
		return(FAIL);
	    }

	    if (e1_dbl_chk_aux_data(sp,pp) == FAIL) {
		printf("...failed to find good aux data in repmeas\n");
	    	pp->iqrephase = 90.0;
	    }
	    else
	    	e1_repmeas(sp,pp);

	} /* complete rep_meas */

	else
	    pp->iqrephase = 90.0;

	if (sw_clutter_lock == 0) {
	    printf("2DFFT Bypassed ...\n");
	    return (PASS);
	}
	else {
	    if (vbose) printf("Welcome to 2DFFT processing!\n");

  	    if (first_odd != 0) {
	    	printf ("Initializing READ recorder...\n");
	    	if (rcdr_start (read_rcdr) == FAIL)
		    return (FAIL);
	    }
	    if (first_odd != 0) {
	    	reset_rpi(1);
	    	fmt_tobe = pp->fmt_id;
	    }

	    reset_edfm(1,1);
	    reset_edfm(0,0);

	/* setup and process 4*512 lines thru the range fwd fft */

	    strcat(strcpy(filename,DEF_PATH),"params.2dfft");
	    if (read_proc_param_file(filename) == FAIL) {
		printf("...unable to read params.2dfft file\n");
		return (FAIL);
	    }
	    /* to overwrite BAD setup in params.2dfft */
	    in_norminv = pp->polarity;
	    rc_fscale = pp->fftsc_raf;
	    in_blkoff = pp->bit_off / 8 ;
	    in_shift = pp->bit_off % 8;
	    if (p_load_regs() == FAIL) {
		printf("...register load failed\n");
		return (FAIL);
	    }
	/* setup 2DFFT.  It is massy, however, 1-lk or 4-lk maybe not important */
	    setup_umr("rf",0x1100,0,1,0,0,0,0,0,11);
	    setup_fft("ri",0,1,11,0);
	    setup_umr("ri",0x0000,0,0,0,0,0,0,0,11);
	    setup_rm(0x1101,0,0,0,1,10,14);
	    setup_xfer_cntrl(0x0,1,1,0,0,0,0,0,10);
	    setup_fft("ai",0,1,8,0);
	    setup_az(0x1,0,1,0,1,ap_detshift,10,0,0);

	    ex_set_selftest(0);
	    ex_set_delay(8192 * 2 * NFMTS);
	    sync_code = ~(SS | FS | VF | AL | VL) & 0xff;
	    ex_set_event(SYNC_EVENT,0x1f,sync_code,1);
	    ex_set_reset(0);
	    play_dis(1);

 	    /* reset all the boards */
	    iif_set_reset (1);		/* reset IIF */
	    ctc_set_reset (1);
	    mlc_set_reset (1);
	    avg_set_reset (1);

	/* check recorder identities */
	/* do input only for the very first time play DCRSi ??? */
    	    if (first_ident == 1) {
      	        first_ident = 0;
                if (!strcmp( media_type, "DCRSI" ) ){
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

    	    }  /* if first ident */

	    clear_fft_oflo_regs ("rf"); /* Clear FFT overflow flags */
	    clear_fft_oflo_regs ("af");

	    iif_set_reset (0);		/* release IIF reset */
	    avg_set_reset (0);
	    set_avg_wrmode (0x001);
	    mlc_set_reset (0);
	    rm_ovf_reset ();
	    ctc_set_reset (0);

	/* if first region, start the DCRS at the right location */
	    if (first_odd != 0) {
	    	/* if (vbose) */
/*SONY*/
                    if (!strcmp( media_type, "DCRSI" ) ) {
	    	      printf("starting DCRSi at %d, offset %d, polarity %d\n",
			    in_blkoff,in_shift,pp->polarity);
	    	      start_fmt_play(0,pp->blk_start,pp->bit_off,pp->polarity);
                    }
                    else {
	    	       printf("starting SONY at %d, offset %d, polarity %d\n",
			    in_blkoff,in_shift,pp->polarity);
                       if (!strcmp( data_direction, "REVERSE" ) )
	    	        sony_start_fmt_play_REV(0,pp->blk_start,pp->bit_off-152,pp->polarity);
                       else
	    	        sony_start_fmt_play(0,pp->blk_start,pp->bit_off-152,pp->polarity);
                         }

	    	reset_rpi(0);
		pre_done = 0;
		first_odd = 0;
	    }

	/* arm and trigger */
	    e1_clear_hdr_mem(0);
	    tarm();
	    play_dis(0);
	    /* writing to mb array */
	    ans = p_get_avg_image (pr_avglines, (ml_linelen + 7)/8, "");
	    if (ans == -1) {
		printf("failed in get_avg_image\n");
		return(FAIL);
	    }
	    printf ("Before tdone\n");
	    tdone ();
	    play_dis(1);

	/* at this point, the data should be captured */
	    fmt_tobe += 4*512; /* we play 2048 formats for 2dfft */
	    if (vbose) {
		printf("\n...in 2DFFT after trigger, fmt = %d\n", fmt_tobe);
	    }

/*SONY*/
            if (!strcmp( media_type, "DCRSI" ) )
               blkno = dc_get_addr(IN);
            else
               blkno = sony_get_addr(IN);

    printf ("at blk after 2DFFT = %d\n", blkno);

	/* check dropout bit */
	    asp_read( (RLOC_EDFM+3)<<1, &mb.w[RLOC_EDFM+3], 2 );
	    if ((mb.w[RLOC_EDFM+3] & 1) == 1) {
		printf("dropout detected in 2DFFT\n");
		return(FAIL);
	    }

	/* save data captured in mb array to a file */
	    if ((file_mj = creat("avg_2dfft", 0666)) == FAIL) {
		printf("\n Can't open %s", "avg_2dfft");
		return(FAIL);
	    }
	    for (i = 0; i < 0xaf80; i++) data_2d[i] = mb.w[i+0x8000];
	    if (write(file_mj, data_2d, 89856) != 89856) {
		printf("Error in writing avg_2dfft\n");
		return(FAIL);
	    }
	    close(file_mj);
	    printf("avg_2dfft saved\n");

	    if (e1_dbl_chk_aux_data(sp,pp) == FAIL) {
		printf("...failed to find good aux data in 2dfft\n");
		printf("...no 2DFFT calculation\n");
		return(PASS);
	    }
	}  /* complete 2DFFT */

    /* perform compress in both directions for data in mb array */
	rpt = ((ml_linelen + 7) / 8) >> 1; /* ml_linelen is 5616 */
	rpt3 = rpt / 3;
	rpt23 = 2 * rpt3;
	for (i = 0; i < rpt*2; i++) rng[i] = 0;
	for (j = 0; j < pr_avglines; j++) {
	    dop[0][j] = 0;
	    dop[1][j] = 0;
	    dop[2][j] = 0;
	}
  	for (j = 0; j < pr_avglines; j++) { 
	    for (k = 0; k < rpt3; k++) {
		i = k + j*rpt + 0x8000;
		dop[0][j] += (mb.w[i]&0xff) + ((mb.w[i]>>8)&0xff);
		dop[1][j] += (mb.w[i+rpt3]&0xff) + ((mb.w[i+rpt3]>>8)&0xff);
		dop[2][j] += (mb.w[i+rpt23]&0xff) + ((mb.w[i+rpt23]>>8)&0xff);
		rng[2*k] += (mb.w[i] >> 8) & 0xff;
		rng[2*k+1] += mb.w[i] & 0xff;
		rng[2*(k+rpt3)] += (mb.w[i+rpt3] >> 8) & 0xff;
		rng[2*(k+rpt3)+1] += mb.w[i+rpt3] & 0xff;
		rng[2*(k+rpt23)] += (mb.w[i+rpt23] >> 8) & 0xff;
		rng[2*(k+rpt23)+1] += mb.w[i+rpt23] & 0xff;
	    }		
	    dop[0][j] /= rpt23;
	    dop[1][j] /= rpt23;
	    dop[2][j] /= rpt23;
	}
	for (k = 0; k < rpt*2; k++) {
	    rng[k] /= pr_avglines;
/*	    printf("k = %d, rng = %d\n", k, rng[k]); */
	}
/*
	for (j = 0; j < pr_avglines; j++) {
	    printf("j = %d, dop1 = %d, dop2 = %d, dop3 = %d\n",
			j, dop[0][j], dop[1][j], dop[2][j]);
	}
*/
    /* save 128 points range spectrum to a file, and calc etot */
	etot = 0.;
	for (k = 0; k < rpt3; k++) {
	    data_2d[k] = (short int)(rng[2*k] + rng[2*k+1] +
				     rng[2*(k+rpt3)] + rng[2*(k+rpt3)+1]+
				     rng[2*(k+rpt23)] + rng[2*(k+rpt23)+1])/6;
	    etot += data_2d[k];
	}
	p_write_mem_file(data_2d,128,"rng_spec","","range spectra from 2dfft");
	printf("rng_spec saved\n");

    /* calculate total desired processor gain */
/*
	if (region % 2 != 0) {
	    clock_gain = dpf.pro_gain - 10*log10(etot/dpf.gain_ref);
	    printf("etot = %g, clock_gain = %d\n", etot, clock_gain);
	    if (vbose)
	        printf("clock_gain = %d\n", clock_gain );
	    if (clock_gain < -3 && first_region == 1)
	        printf("1st image is too bright, NO set_gain\n");
	    else
	        set_gain(pp, clock_gain);
	    detshift_old = ap_detshift;
	}
*/

    /* perform signal measurement */
	printf("\n... in SNR measurement\n");
	mhz_bin = spf.csr / 1000000. / rpt23;	
	noise_1 = (int)(((dpf.fcnoise - dpf.bwnoise/2) / mhz_bin) + 0.5);
	noise_2 = (int)(((dpf.fcnoise + dpf.bwnoise/2) / mhz_bin) + 0.5);
	sig_1 = (int)(((dpf.fcsig - dpf.bwsig/2) / mhz_bin) + 0.5);
	sig_2 = (int)(((dpf.fcsig + dpf.bwsig/2) / mhz_bin) + 0.5);
	/* Ming assume noise located around spectrum center */
	sig_1 += rpt23;
	for (i = 0; i < 3; i++) {
	    p_noise[i] = 0;
	    p_sig[i] = 0;
	}
	for (i = 0; i < rpt23; i++) {
	    if (i < noise_2 && i >= noise_1) {
	    	p_noise[0] += rng[i];
	    	p_noise[1] += rng[rpt23+i];
	    	p_noise[2] += rng[2*rpt23+i];
	    }
	    if (i < sig_2 || i >= sig_1) {
		p_sig[0] += rng[i];
		p_sig[1] += rng[rpt23+i];
		p_sig[2] += rng[2*rpt23+i];
	    }
	}
	for (i = 0; i < 3; i++) {
	    if (p_sig[i] == 0 || p_noise[i] == 0) {
		printf("warming: ZERO POWER in range spectrum!\n");
		pp->snr = 0.;
		return(PASS);
	    }
	    else
		snr[i] = 20. * log10((float)p_sig[i] / (12. * p_noise[i]));
	}
	pp->snr = (snr[0]+snr[1]+snr[2]) / 3.0;
	printf("3 sub-SNRs in DB are: %g, %g, %g\n", snr[0],snr[1],snr[2]);
	printf("averaged SNR in DB is: %g\n", pp->snr);

    /* perform Doppler estimate, 3 swathes, 3 iterations each */
	printf("\n... in Doppler measurement\n");
	for (i = 0; i < 3; i++) {
	  init_center = pr_avglines / 2;
	  for (l = 0; l < 3; l++) {
	    del_j = (int)(pr_avglines / pow(2.,l+1.));
	    cut_j = init_center - del_j;
	    energy = 0;
	    percent = 100.;
	    for (k = cut_j; k < init_center+del_j; k++) {
		j = k;
		if (k < 0) j = k + pr_avglines; 
		if (k >= pr_avglines) j = k - pr_avglines; 
		energy += dop[i][j];
	    }
	    while (del_j > 0) {
	    	half = 0;
	        cut_j += (percent > 50.) ? del_j : -del_j;
	        for (k = cut_j; k < cut_j + (int)(pr_avglines/pow(2.,l+1.)); k++) {
		    j = k;
		    if (k < 0) j = k + pr_avglines; 
		    if (k >= pr_avglines) j = k - pr_avglines; 
		    half += dop[i][j];
		}
	    	percent = 100. * ((float)half) / energy;
/*
		printf("energy = %d, half = %d\n", energy, half);
		printf("center=%d,del=%d,percent=%g\n", cut_j,del_j,percent);
*/
	    	if (percent > 49.9 && percent < 50.1)
		    break;
	    	else
	    	    del_j /= 2;
	    }
            if (l == 0) { /* check ambiguity CV for MC 5/13/97 */
                j = cut_j + pr_avglines/2;
                j -= ((cut_j + pr_avglines/2) >= pr_avglines) ? pr_avglines : 0;
                if (dop[i][cut_j] < dop[i][j]){
                   cut_j = j;
                   printf("WARNING: adjust doppler \n");
                }
            }
	    init_center = cut_j;
	    dop_f[l] = sp->pre->prf * ((float)cut_j) / pr_avglines;
/*	    dop_f[l] = sp->pre->prf * (1. - ((float)cut_j) / pr_avglines); */
	    printf("i = %d,l = %d, center = %d, Doppler = %g\n", i,l, cut_j, dop_f[l]);
	    if ((dop_f[l]-pp->fda) > sp->pre->prf/2.) {
		dop_f[l] -= sp->pre->prf;
	    	printf("Adjusted Doppler = %g\n", dop_f[l]);
	    }
	    if ((dop_f[l]-pp->fda) < -sp->pre->prf/2.) {
		dop_f[l] += sp->pre->prf;
	    	printf("Adjusted Doppler = %g\n", dop_f[l]);
	    }
            /* Try to fix the high doppler in s-pole for E1 & E2, CV 8/27/97 */
            if ((!strncmp(Cur_Rqst->take_id,"E1",2)) &&
                 ( Cur_Rqst->lat < -65.0 ) && (dop_f[l] > 0.0)) {
                dop_f[l] -= sp->pre->prf;
                if (vbose) printf("E1 high doppler in s-pole fix \n");
            }
            if ((!strncmp(Cur_Rqst->take_id,"E2",2)) &&
                 ( Cur_Rqst->lat < -65.0 ) && (dop_f[l] < 0.0)) {
                dop_f[l] += sp->pre->prf;
                if (vbose) printf("E2 high doppler in s-pole fix \n");
            }
	  }
	  estat[i] = 0;
	  dop_avg[i] = (dop_f[0] + dop_f[1] + dop_f[2]) / 3.;
	  printf("i = %d, Doppler = %g\n", i, dop_avg[i]);
	}
    /* Adjusting the Doppler params */
	if (vbose)
	printf("original fd(a,b,c): %g %g %g\n",pp->fda,pp->fdb,
	    pp->fdc);
    /* use linear adjustment only */
	linear_qr(dop_avg,estat,3,tempfd);
	printf("Pass linear fit\n");
	tempfd[2] = 0.0;
/* leave it here only for R1a compatability, MC 6-4-96 */
	adjust_fds(tempfd,newfd);

	pp->fda = tempfd[0];
	pp->fdb = tempfd[1];
	pp->fdc = tempfd[2];
	if (vbose)
	printf("adjusted fd(a,b,c): %g %g %g\n",pp->fda,pp->fdb,
	    pp->fdc);

	return (PASS);
}

/* e1_ltrr(sp,pp,nfmts) --------------------------------------------
	This routine forword DCRSi nfmts formats.
*/
e1_ltrr(sp,pp,nfmts)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	int nfmts;
{
	AUX_DATA aux;
	int blkno, fmt_end, nframes, tfmts, sync_code;

    	setup_iif_cntrl(0,0,1,2,0x1100,1,0);
	nframes = 8;
	nfmts /= nframes;
    	tfmts = nfmts * nframes;
        if (vbose)
            printf("...requesting %d frames of %d formats\n",
	           nframes,nfmts);
	setup_iif_par(5616,nfmts,nframes,tfmts,0,0,0,1,13,0);
	ex_set_selftest(0); /* do we need this ??? */
	ex_set_delay(nfmts * 8192);
	sync_code = FS & VF & AL & VL;
	ex_set_event(SYNC_EVENT,0x0f,sync_code,nframes);
	ex_set_reset(0);
	play_dis(1);
	iif_set_reset(1);
	iif_set_reset(0);

    /* arm and trigger */
	e1_clear_hdr_mem(1);
	tarm();
	play_dis(0);
	printf ("Before tdone\n");
	tdone ();
	play_dis(1);

    /* check dropout bit */
	asp_read( (RLOC_EDFM+3)<<1, &mb.w[RLOC_EDFM+3], 2 );
	if ((mb.w[RLOC_EDFM+3] & 0x1) == 1) {
	    printf("dropout detected in LTRR\n");
	    return(FAIL);
	}

    /* at this point, the data should be captured */
  	if (vbose)
	    printf("\n...in LTRR after trigger\n");

    /* retrieve and check auxiliary data values */
	if (e1_dbl_chk_aux_data(sp,pp) == FAIL) {
	    printf("...failed to find good aux data\n");
	    return(FAIL);
	}
	else {
/*SONY*/
            if (!strcmp( media_type, "DCRSI" ) )
               blkno = dc_get_addr(IN);
            else
               blkno = sony_get_addr(IN);

	    fmt_end = e1_find_last_hdr();
	    if (f_dropout == 1) {
		f_dropout = 0;
		return(FAIL);
	    }
	    if (vbose) 
	        printf("...%d formats played to skip\n",fmt_end+1);
	    e1_get_good_aux_data(0,&aux,fmt_end);
	    if (vbose) 
	        printf("   first format is %d\n",aux.format);
	    e1_get_good_aux_data(fmt_end,&aux,fmt_end);
	    if (vbose) 
	        printf("    last format is %d\n",aux.format);
	    if (blkno == 0)
	        printf("output block 0, who cares! ");
	    if (vbose)
	        printf("...now at block %d\n",blkno);
	}
}
/* ==================== same for both 2pass & 3pass ==================*/

/* e1_chk_aux_data(sp,pp) ----------------------------------------------
	This routine extracts the auxiliary data from the ERS-1 header,
	and checks the data against acceptable values.  If the data is
	OK, the routine uses the data to calculate the actual start of
	the region, and returns PASS.  If not, the routine tries the
	next format's header, and so on up to 50 headers.  If no good
	values are found, the routine returns FAIL.
	    The auxiliary data is part of the data stored by the IIF 
	in its header memory.
*/

e1_chk_aux_data(sp,pp)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
{
	AUX_DATA aux;
	int status = FAIL;
	int fmt;

	for (fmt = 0; (fmt < 50) && (status == FAIL); fmt++) {
	    if (e1_get_good_aux_data(fmt,&aux,fmt+10) == FAIL)
		continue;
	    if ((aux.task & 0xf0) == 0xA0) {
		status = PASS;
		if (pp->quality == -1) {
		    stime_to_gmt(aux.time,&pp->sv.gmt);
		    pp->quality = 0;
		    pp->fmt_id = aux.format;
		    pp->fmt_start = aux.ifmt;
		    e1_get_fmt_loc(aux.ifmt,aux.format,sp,
			&pp->blk_start,&pp->bit_off);
		}
		break;
	    }  /* if aux.task */
	}  /* for fmt */
	return (status);
}

/* e1_dbl_chk_aux_data(sp,pp) --------------------------------------
	This routine extracts the auxiliary data from the ERS-1 header,
	and checks the data against acceptable values.  If the data is
	OK, the routine uses the data to calculate the actual start of
	the region, and returns PASS.  If not, the routine tries the
	next format's header, and so on up to 50 headers.  If no good
	values are found, the routine returns FAIL.
	    The auxiliary data is part of the data stored by the IIF 
	in its header memory.
	    This is a special mod to e1_chk_aux_data which will not
	update the block address in the pp structure.
*/

e1_dbl_chk_aux_data(sp,pp)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
{
	AUX_DATA aux;
	int status = FAIL;
	int fmt;

	for (fmt = 0; (fmt < 50) && (status == FAIL); fmt++) {
	    if (e1_get_good_aux_data(fmt,&aux,fmt+10) == FAIL)
		continue;
	    if ((aux.task & 0xf0) == 0xA0) {
		status = PASS;
		if (pp->quality == -1) {
		    pp->quality = 0;
		}
		break;
	    }  /* if aux.task */
	}  /* for fmt */
	return (status);
}


/* e1_clear_hdr_mem(mode) ----------------------------------------------
	This routine clears the IIF header memory to 0x55, to permit
	the e1_find_last_hdr routine to locate the last recorded header.
	mode = 1 means short header mode, = 0 means normal mode.
*/

e1_clear_hdr_mem(mode)
	int mode;
{
	int i, igo, iend, pid_save, hdr_len, mloc;

	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HEAD;
	hdr_len = (mode) ? HDR_LEN2 : HDR_LEN;
	igo = (MLOC_IIF_HEAD + 2) >> 1;
	mloc = MEM_IIF_HEAD;
	iend = igo + (0x80000 >> 1);
	asp_read( mloc, &mb.w[MLOC_IIF_HEAD>>1], 0x80000 );
	for (i = igo; i < iend; i += hdr_len) mb.w[i] = 0x5555;
	asp_write( mloc, &mb.w[MLOC_IIF_HEAD>>1], 0x80000 );
	mb.w[RLOC_REP] = pid_save;
}


/* e1_find_last_hdr() --------------------------------------------------
	This routine finds the last recorded header in header memory.
	Call e1_clear_hdr_mem before beginning to record, and wait
	until processing is complete before calling this routine.
	The routine returns the header number relative to the start
	of header memory, or FAIL if none is found.

	M. Chen, 3/18/93 -
	add check statement to prevent new echo starts after small
	(i.e. less than maxmiss) break or even no break.  Here it is
	assumed 1st HDR is always good HDR.
*/

e1_find_last_hdr()
{
	int i, igo, iend, pid_save, hdr_mode, hdr_len, code;
	int misses,isave;
	int fsave,flast;
	int maxmiss = 3;

	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HEAD;
	get_iif_hdr_mode(&hdr_mode);
	hdr_len = (hdr_mode) ? HDR_LEN2 : HDR_LEN;
	igo = (MLOC_IIF_HEAD + 2) >> 1;
	iend = igo + (0x80000 >> 1);
	misses = 0;
	asp_read( MEM_IIF_HEAD, &mb.w[MLOC_IIF_HEAD>>1], 0x80000 );
	for (i = igo; i < iend; i += hdr_len) {
	    code = (mb.w[i] >> 8) & 0xff;
	    if (bit_dif(code,0xAA,2) > 2) {
		if (misses == 0) {
		    isave = i;
		}
		if (++misses > maxmiss) {
		    i = isave;
		    break;
		}
	    }
            else {
		if (misses) {
		    flast = (mb.w[i-1] - misses - 1) & 0xffff;
		    if (abs(fsave-flast) > 2) {
			printf("Break, force dropout if scan 1!\n");
			printf("fsave = %d, flast = %d\n", fsave,flast);
			f_dropout = 1;
			i = isave;
			break;
		    }
		}
		else {
		    if (i > igo && mb.w[i+4] == 0 && mb.w[i+5] == 0) {
			printf("Back to 0, force dropout if scan 1!\n");
			f_dropout = 1;
			break;
		    }
		    fsave = mb.w[i-1] & 0xffff;
		}
		misses = 0;
	    }
	}
	mb.w[RLOC_REP] = pid_save;
	if (i > igo)
	    return (((i - igo) / hdr_len) - 1);
	return (FAIL);
}


/* e1_get_aux_data(fmt,ap) ---------------------------------------------
	This routine extracts the IDHT format number and auxiliary 
	data from one header captured in the IIF header memory.
	The IIF captures header data for each processed format.
	'fmt' gives the relative format number (starting from 0) of 
	the format whose header data is desired.  'ap' is a pointer
	to a structure to be loaded with the extracted data.
	If the aux data format code is valid, the routine returns 
	PASS, otherwise FAIL.
*/

e1_get_aux_data(fmt,ap)
	int fmt;
	AUX_PTR ap;
{
	int i, pid_save, fcode, hdr_mode, hdr_len, mloc;

	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HEAD;
	get_iif_hdr_mode(&hdr_mode);
	hdr_len = (hdr_mode) ? HDR_LEN2 : HDR_LEN;
	i = fmt * hdr_len + (MLOC_IIF_HEAD >> 1);
	mloc = fmt*hdr_len*2 + MEM_IIF_HEAD;
if(mloc < 0x780000) {
    printf("Ming mloc1 = %X, fmt = %d\n", mloc, fmt);
    printf("mloc is below VME memory baseline\n");
    return(FAIL);
}    
	asp_read( mloc, &mb.w[i], 2 );
	ap->ifmt = mb.w[i] & 0xffff;
	i += AUX_LOC;
	mloc += AUX_LOC<<1;
	asp_read( mloc, &mb.w[i], 18 );
	fcode = ((mb.w[i] >> 8) & 0xff);
	if (bit_dif(fcode,0xAA,2) > 2)	/* SAR data */
	    return (FAIL);
	ap->obrc = (mb.w[i] >> 7) & 0x1;
	ap->time = (mb.w[i+1] << 16) | (mb.w[i+2] & 0xffff);
	ap->task = (mb.w[i+3] >> 8) & 0xff;
	ap->sample = mb.w[i+3] & 0xff;
	ap->format = ((mb.w[i+4] & 0xff) << 16) | (mb.w[i+5] & 0xffff);
	ap->window_start = mb.w[i+6] & 0xffff;
	ap->pri = mb.w[i+7] & 0xffff;
	ap->cal_att = (mb.w[i+8] >> 8) & 0x1f;
	ap->rec_gain = (mb.w[i+8] >> 1) & 0x1f;
	mb.w[RLOC_REP] = pid_save;
	return (PASS);
}


/* e1_get_good_aux_data(fmt,ap,end) ------------------------------------
	This routine gets reliable values for the aux data pertaining
	to the given format number in header memory.  The routine gets
	several values of each datum, and "votes" which one is the best.
		fmt = header number of interest
		ap = pointer to aux data block to be filled
		end = last valid header number
*/

e1_get_good_aux_data(fmt,ap,end)
	int fmt,end;
	AUX_PTR ap;
{
	AUX_DATA aux;
	int ifmtid,obrcid,timeid,taskid,sampleid,formatid;
	int wsid,priid,calid,gainid;
	int i,igo,iend,igo2,iend2,idif;
	int nreps = 5;
	int npass = 0;
	int status = PASS;

	idif = end - fmt;
	end++;
	igo = fmt - nreps / 2;
	if (end - igo < nreps)
	    igo = end - nreps;
	if (igo < 0)
	    igo = 0;
	if ((iend = igo + nreps) > end)
	    iend = end;
	if (idif > (fmt - igo))
	    idif = fmt - igo;
	igo2 = fmt - idif;
	iend2 = fmt + idif + 1;
	ifmtid = init_best_val(nreps);
	obrcid = init_best_val(nreps);
	timeid = init_best_val(nreps);
	taskid = init_best_val(nreps);
	sampleid = init_best_val(nreps);
	formatid = init_best_val(nreps);
	wsid = init_best_val(nreps);
	priid = init_best_val(nreps);
	calid = init_best_val(nreps);
	gainid = init_best_val(nreps);
	for (i = igo; i < iend; i++) {
	    if (e1_get_aux_data(i,&aux) != PASS)
		continue;
	    npass++;
	    if (i == fmt)
		*ap = aux;
	    save_best_val(obrcid,aux.obrc);
	    save_best_seq_val(formatid,aux.format,0);
	    save_best_val(priid,aux.pri);
	    save_best_val(calid,aux.cal_att);
	    save_best_val(gainid,aux.rec_gain);
	    if ((i >= igo2) && (i < iend2)) {
		save_best_seq_val(ifmtid,aux.ifmt,0x10000);
		save_best_val(timeid,aux.time);
		save_best_val(taskid,aux.task);
		save_best_val(sampleid,aux.sample);
		save_best_val(wsid,aux.window_start);
	    }
	}
	if (npass < iend2-igo2)
	    status = FAIL;
	idif = fmt - igo;
	if ((i = get_best_val(ifmtid)) != FAIL)
	    ap->ifmt = (i + idif) & 0xffff;
	else status = FAIL;
	if ((i = get_best_val(obrcid)) != FAIL)
	    ap->obrc = i;
	else status = FAIL;
	if ((i = get_best_val(timeid)) != FAIL)
	    ap->time = i;
	else status = FAIL;
	if ((i = get_best_val(taskid)) != FAIL)
	    ap->task = i;
	else status = FAIL;
	if ((i = get_best_val(sampleid)) != FAIL)
	    ap->sample = i;
	else status = FAIL;
	if ((i = get_best_val(formatid)) != FAIL)
	    ap->format = i + idif;
	else status = FAIL;
	if ((i = get_best_val(wsid)) != FAIL)
	    ap->window_start = i;
	else status = FAIL;
	if ((i = get_best_val(priid)) != FAIL)
	    ap->pri = i;
	else status = FAIL;
	if ((i = get_best_val(calid)) != FAIL)
	    ap->cal_att = i;
	else status = FAIL;
	if ((i = get_best_val(gainid)) != FAIL)
	    ap->rec_gain = i;
	else status = FAIL;

	return (status);
}


/* e1_get_fmt_status(fmt,echostat,repstat,ifmt) ------------------------
	This routine determines the status of the format 'fmt' in the
	IIF header memory (relative to 0 at the start of the header
	memory), and returns it in echostat and repstat:
	    echostat = 0 means invalid echo/noise data
	    	       1 means valid echo data (1st 4 formats)
		       2 means valid echo data (after 1st 4 formats)
		       3 means valid noise data (1st 4 formats)
		       4 means valid noise data (after 1st 4 formats)
	    repstat = 0 means invalid calibration/replica data
		      1 means valid replica data (1st 4 formats)
		      2 means valid replica data (after 1st 4 formats)
		      3 means valid calibration data (1st 4 formats)
		      4 means valid cal. data (after 1st 4 formats)
	    ifmt = the returned image format number.
	The routine takes into account the auxiliary data format code,
	the activity task code and the sample flags.  Where 
	disagreements between the various codes are found, the data 
	status is returned as invalid, and the routine returns FAIL. 
	If no disagreements are found, the routine returns PASS.
*/

e1_get_fmt_status(fmt,echostat,repstat,ifmt)
	int fmt;
	int *echostat,*repstat,*ifmt;
{
	AUX_DATA aux;
	int samp, val;

    /* assume data will be invalid */
	*echostat = 0;
	*repstat = 0;

    /* pick up data from IIF header memory */
	if (e1_get_aux_data(fmt,&aux) == FAIL)
	    return (FAIL);
	val =   aux.sample & 0x00c0;	  /* valid data flags */
	samp =  aux.sample & 0x003f;	  /* sample flags */
	*ifmt = aux.format;

    /* check auxiliary data fields */
	switch (aux.task) {		/* ensure task code is valid */
	    case 0x44:
		return (PASS);
	    case 0x88:
	    case 0x99:
	    case 0xA9:
	    case 0xAA:
	    case 0xA0:
		break;
	    default:
		return (FAIL);
	}  /* switch aux.task */
	if (val & 0x80) {	/* check echo/noise data */
	    switch (aux.task & 0xf0) {
		case 0xa0:		/* echo data */
		    *echostat = (samp & 0x08) ? 1 : 2;
		    break;
		case 0x80:		/* noise data */
		    *echostat = (samp & 0x20) ? 3 : 4;
		    break;
		default:
		    break;
	    }  /* switch */
	}  /* if val & 0x80 */
	if (val & 0x40) {	/* check calibration/replica data */
	    switch (aux.task & 0x0f) {
		case 0x09:		/* calibration */
		    *repstat = 3;
		    break;
		case 0x0A:		/* replica */
		    *repstat = 1;
		    break;
	    }  /* switch */
	    if (*repstat)
		*repstat += (samp & 0x10) == 0;
	}  /* if val & 0x40 */
}


/* e1_get_fmt_type(fmt,type,ifmt) --------------------------------------
	This routine determines what type of format 'fmt' is, and
	returns a number indicating that type:
		0 = IDHT zero format
		1 = "no data" format
		2 = first 4 noise formats
		3 = rest of noise formats
		4 = first 4 echo/calibration formats
		5 = rest of echo/calibration formats
		6 = first 4 no echo/cal formats
		7 = rest of no echo/cal formats
		8 = first 4 echo/replica formats
		9 = rest of echo/replica formats
	       10 = echo/nothing format
		-1 = unidentifiable format
*/

e1_get_fmt_type(fmt,type,ifmt)
	int fmt;
	int *type,*ifmt;
{
	static int tbl[5][5] = {
		 1,10,10, 2, 3,
		-1, 8, 8,-1,-1,
		-1, 9, 9,-1,-1,
		 6, 4, 4,-1,-1,
		 7, 5, 5,-1,-1,
		};
	int e,r;

	if (e1_get_fmt_status(fmt,&e,&r,ifmt) == FAIL)
	    *type = -1;
	else {
	    if (*ifmt == -1)
		*type = 0;
	    else
		*type = tbl[r][e];
	}
}



/* e1_get_hwdata(sp,pp) ------------------------------------------------
	This routine retrieves the bit error rate and input data
	histograms from the hardware, saves them for later, and does
	some quality checking on them.  If the quality checks pass, the
	routine returns PASS, otherwise, FAIL.
*/

e1_get_hwdata(sp,pp)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
{
	float errcount, bits;
	int i,j,pid_save;
	int itotal = 0;
	int qtotal = 0;
	int count = 32;
	float hilev,lolev,normal,xfit[32];

    /* retrieve the bit error rate */
	asp_read( (RLOC_EDFM+4)<<1, &mb.w[RLOC_EDFM+4], 2 );
	errcount = 0xffff & mb.w[RLOC_EDFM + 4];
	fmt_ratio = 1767.9 / sp->pre->prf;
	bits = 29.0 * 24.0 * 5432.0 * fmt_ratio;
	pp->ber = errcount / bits;
	printf("...bit error rate = %g\n",pp->ber);

    /* retrieve the histograms */
	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HIST;
	j = 0;
	asp_read( MEM_IIF_HIST, &mb.w[MLOC_IIF_HIST>>1], 512 );
	for (i = MLOC_IIF_HIST >> 1; i < (MLOC_IIF_HIST >> 1) + 256;
		i += 8) {
	    pp->ihist_cor[j] = (mb.w[i  ] << 16) + (mb.w[i+1] & 0xffff);
	    pp->qhist_cor[j] = (mb.w[i+2] << 16) + (mb.w[i+3] & 0xffff);
	    itotal += pp->ihist_cor[j];
	    qtotal += pp->qhist_cor[j];
	    j++;
	}
	if (vbose) {
	    printf("I HISTOGRAM:");
	    for (i = 0; i < count; i++) {
		if ((i % 8) == 0) printf("\n");
		printf(" %6.6d",pp->ihist_cor[i]);
	    }
	    printf("\nQ HISTOGRAM:");
	    for (i = 0; i < count; i++) {
		if ((i % 8) == 0) printf("\n");
		printf(" %6.6d",pp->qhist_cor[i]);
	    }
	    printf("\n");
/*		if (abob()) exit(1); */
	}
	mb.w[RLOC_REP] = pid_save;
    /* make quality measurements */
	rawchk_(pp->ihist_cor,&count,&itotal,
		&pp->imean,&pp->istdv,&hilev,&lolev,&normal,xfit);
	rawchk_(pp->qhist_cor,&count,&qtotal,
		&pp->qmean,&pp->qstdv,&hilev,&lolev,&normal,xfit);
	if (vbose)
	    printf("...done with get_hwdata\n");
	return (PASS);
}


/* e1_repmeas(sp,pp) ---------------------------------------------------
	This routine measures the characteristics of the first 8
	replicas in the preprocessing region pointed to by pp, which
	is in the tape segment pointed to by sp.
*/

e1_repmeas(sp,pp)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
{
	short int tf[2048][2];
	unsigned char replica[2048];
	int i,r,rgo,dcbias,n3sig;
	int len;
	float power,spikes[2048],peakoffset;
	float gain[2],bias[2];
	float peak,x3db,pslr,islr;
	float tpower = 0;
	int istat = 0;
	int nreps = 1;
	int bypass = 0;
	char cstat;
	int file_mj;

	if (vbose) {
	if ((file_mj = creat("replica_mj", 0666)) == FAIL) {
			printf (" \n Can't open %s", "replica_mj");
			return(FAIL);
	}
	}

	printf("  measuring replica characteristics\n");
	if (sw_rep_meas == 0) {
	    printf("......bypassed\n");
	    pp->iqrephase = 90.0;
	    return;
	}
    /* get the range transfer function */
	p_get_mem_file(tf,4096,DEF_PATH,"rf_ra_file","");
    /* read and process 8 replicas */
	rgo = (pp->fmt_id < 104) ? 4 : 0;
	len = in_chirp;
	for (r = rgo; r < rgo + nreps; r++) {
	    e1_get_replica(r * 24,replica,ON);
	
	if (vbose) {
	if (write(file_mj, replica, 1536) != 1536) {
		printf( "Error in writing replica file" );
		return(FAIL);
	}
	close(file_mj);
	}

	/* debugging stuff: copy replica to 0x10000 */
        
	    for (i = 0; i < 2*len; i++) mb.w[0x8000 + i] = replica[i];

	    if (vbose) {
		printf("...replica copy at 10000\n");
/*		if (abob()) exit (1);  */
		}
	    
	    /* NOTE: the following is a FORTRAN subroutine call */
	    dcbias = 32;  /* E-ERS-1 specific offset */
	    repmeas_(replica,tf,&len,gain,bias,&dcbias,&pp->iqrephase,
		spikes,&n3sig,&peak,&peakoffset,&x3db,&pslr,&islr,&istat);
	    if (abs((dpf.peak_ref/peak) - 1.0) > 0.07)
		printf("...replica peak = %g\n",(peak/dpf.peak_ref));
	    pp->peak = peak;

	    if (vbose)
		printf("...peak power = %g\n",peak);
            if (vbose)
		printf("...iq_phase = %g\n",pp->iqrephase);
	    if (peakoffset > spf.xlocation)
		pp->reperror |= 1;
	    if (x3db > spf.x3db)
		pp->reperror |= 2;
	    if (pslr > spf.xpslr)
		pp->reperror |= 4;
	    if (islr < spf.xislr)
		pp->reperror |= 8;
	    pp->islr = islr;
	    pp->pslr = pslr;
	/*
	    noise(replica,len,&power,&cstat);
	    tpower += power;
	*/
	}
}


/* e1_get_replica(fmt,data,find_start) ---------------------------------
	This routine extracts one chirp replica (or drift calibration
	pulse) from the replica fields of the header data, and stores
	it in the array 'data'.  If find_start = ON, the routine finds
	the first replica which starts at format number fmt or beyond.
	8/9/91: modified according to new spec from ESA about I/Q
*/

e1_get_replica(fmt,data,find_start)
	int fmt,find_start;
	unsigned char data[];
{
	AUX_DATA aux;
	int f,i,j,k,jgo,jend,pid_save,end, mloc;

    /* find the replica start location */
	f = fmt;
	if (find_start == ON) {
	    end = e1_find_last_hdr();
	    e1_get_good_aux_data(fmt,&aux,end);
	    f = repoff - (aux.format % 24);
	    if ( f < 0)
		f += 24;
	    f += fmt;
	}
	if (vbose)
	    printf("  ...get_replica: returning replica starting at format %d\n",f);

    /* extract the replica from 22 consecutive formats */
	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HEAD;
	k = 0;
	for (i = 0; i < 22; i++) {
	    jgo = (f * HDR_LEN) + (MLOC_IIF_HEAD >> 1) + REP_LOC;
	    mloc = (PID_IIF_HEAD<<20) | (jgo<<1);
	    jend = jgo + ((i == 21) ? REP_LAST : REP_LEN);
	    asp_read( mloc, &mb.w[jgo], (jend-jgo)<<1 );
	    for (j = jgo; j < jend; j++) {
		data[k++] = (mb.w[j] >> 6) & 0x3f;
		data[k++] =  mb.w[j]       & 0x3f;
	    }
	    f++;
	}
	mb.w[RLOC_REP] = pid_save;
	return (PASS);
}
/* setup_ap(sp,pp,looks,write_to_disk,slrange,deskew, ---------------
	    start_bin,rads,skew_adj)
	This routine calculates RM, XFER, AZINT and MLC function values,
	and IIF, CTC, XFER, AZINT, MLC and MLM register parameter values
	from the processing parameters given in pp and sp, and loads
	them into the function memories.  If 'write_to_disk' is ON, the
	tables are instead written to disk in the current directory.
	If 'slrange' is ON, the routine calculates slant range values;
	otherwise ground range values are calculated.  If 'deskew' is
	ON, the image is deskewed.  'start_bin' only applies to 1-look
	requests, in which case it specifies the starting ground range
	pixel.  'rads' is an array of 4 earth radii, used only when
	slrange = OFF.  The radii and skew_adj are used to calculate a 
	more precise earth surface for slant range to ground range 
	conversion.
	The global variables r3p, fd3p, fr3p are set by this routine for
	use later on in get_corn_loc.  Also, several of the processing
	parameter variables are set for use by p_load_regs and/or
	write_proc_param_file.
*/

setup_ap(sp,pp,looks,write_to_disk,slrange,deskew,start_bin,rads,skew_adj)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	int looks,write_to_disk,slrange,deskew,start_bin;
	double rads[4],skew_adj;
{
	short int d1[8192], d2[8192], d3[8192];
	short int d4[8192], d5[8192], d6[8192];
	short int d7[8192], d8[8192];
	static char rname[] = "rmpath";
	static char xname[] = "xfer";
	static char aname[] = "azint_start";
	static char mname[] = "ila_rotate";
	float prf,xlambda,pbwx,err_offset,skew,fzero2nul;
	float h, fudge_fctr;
	double r_sc,r_earth,azsp,r_close,r_far,range_len;
	double r_ad,r_be,r_av,r_close2,r_far2,r_sc2;
	double theta,theta1,theta2,phi1,phi2;
	int roffset,rmlines,istat,vlpts,nov,azoff,bufrdlen,ptspace;
	int rmcoffset;
	int min_delay,temp;
	int flag_azwgt = 0xff;
	short int lookpos,looklen;
	double ceil();

	printf("...setting up azimuth processor\n");
    /* generate and load range migration function files */
        h = pp->h_az;           /* CV 4/03/97 used in df_proc_params */
	prf = sp->pre->prf;
	xlambda = spf.xlambda;
	if (slrange == OFF) {
	/* calculate special earth radii for ground range conversion */
	    r_sc = (pp->sv.pos.x * pp->sv.pos.x)
		 + (pp->sv.pos.y * pp->sv.pos.y)
		 + (pp->sv.pos.z * pp->sv.pos.z);
	    r_sc = sqrt(r_sc) * 1000.0;
	    r_earth = pp->r_e * 1000.0;
	/* adjust elliptical earth to a sphere */
	    r_close = pp->r_close;
	    r_far = pp->r_far;
	    r_ad = (rads[0] + rads[2]) / 2.0;
	    r_be = (rads[1] + rads[3]) / 2.0;
	    r_av = (r_ad + r_be) / 2.0;
	    r_close2 = pp->r_close * pp->r_close;
	    r_far2 = pp->r_far * pp->r_far;
	    r_sc2 = r_sc * r_sc;
	    theta1 = acos((r_close2 + r_sc2 - (r_ad * r_ad)) 
			  / (2.0 * r_close * r_sc));
	    theta2 = acos((r_far2 + r_sc2 - (r_be * r_be)) 
			  / (2.0 * r_far * r_sc));
	    theta = theta2 - theta1;
	    range_len = sqrt(r_close2 + r_far2
			     - (2.0 * r_close * r_far * cos(theta)));
	    phi1 = asin(r_close * sin(theta) / range_len);
	    phi2 = acos(range_len / (2.0 * r_av));
	    r_sc = sqrt(r_far2 + (r_av * r_av)
			- (2.0 * r_far * r_av * cos(phi1 + phi2)));
	    r_earth = r_av;
	}
	istat = 0;
	/* change the calls to rm1l, rm4l, and rm4lslr to  use
	   'pr_rglen' rather than 'ct_vlpf' (which comes from the
	   default 'params.proc' file). AR1658.  */
	if (vbose) printf("pr_rglen= %d \n",pr_rglen);

	if (looks == 1) {
	    rm1l(
		/* inputs (should be 9 of 'em):  */
		    ct_outlen,start_bin,
		    pr_rglen,prf,xlambda,
		    spf.srspace,&pp->fda,&pp->fdota,pp->r_close,
		/* outputs (should be 11 of 'em):  */
		    d1,d2,d3,d4,d5,&roffset,&rmlines,
		    r3p,fd3p,fr3p,&istat
	    );
	    rmcoffset = roffset + start_bin;
	}  /* if 1 look */
	else {    /* 4 looks */
	    if (slrange) {
		rm4lslr(
		    /* inputs (should be 8 of 'em):  */
			ct_outlen,
		    	pr_rglen,
			prf,xlambda,spf.srspace,
			&pp->fda,&pp->fdota,pp->r_close,
		    /* outputs (should be 11 of 'em):  */
			d1,d2,d3,d4,d5,&roffset,&rmlines,
			r3p,fd3p,fr3p,&istat
		);
	    }
	    else {
		rm4l(
		    /* inputs (should be 11 of 'em):  */
			ct_outlen,
			pr_rglen,
			prf,xlambda,spf.srspace,
			&pp->fda,&pp->fdota,spf.grspace,
			r_sc,r_earth,pp->r_close,
		    /* outputs (should be 13 of 'em):  */
			d1,d2,d3,d4,d5,&roffset,fdg,fdotg,&rmlines,
			r3p,fd3p,fr3p,&istat
		);
	    }
	}  /* else 4 looks */

	if (istat) {
	    printf("...error %d in range migration function generation\n",
		istat);
	}
	ap_rmlines = rmlines;
	if (write_to_disk == ON) {
	    p_write_mem_file(d1,8192,rname,".crs","coarse path offset");
	    p_write_mem_file(d2,8192,rname,".fin","fine path offset");
	    p_write_mem_file(d3,2048,rname,".nkl","path counter init");
	    p_write_mem_file(d4,2048,rname,".2d","2d");
	    p_write_mem_file(d5,2048,rname,".a","path eccentricity");
	    strcpy(ap_rmpath,rname);
	}
	else {
	    setup_rm_mem(d1,d2,d3,d4,d5,ap_rmlines);
	}

    /* generate and load azimuth transfer function files */
	azsp = 4.0 * pp->swth_speed / prf;
	istat = 0;

	if (looks == 1) {
	    az1l(
		/* inputs (should be 6):  */
		    ct_outlen,rmcoffset,prf,spf.pbw,&pp->fda,&pp->fdota,
		/* outputs (should be a zillion (actually, 18)): */
		    d1,d2,d3,d4,d5,d6,&lookpos,&looklen,/* XFER stuff */
		    d7,&ptspace,		     /* AZINT stuff */
		    d8,&vlpts,&nov,&azoff,	     /* MLC/MLM stuff */
		    &bufrdlen,			     /* IIF/CTC */
		    &pbwx,&err_offset,&istat  	     /* misc. stuff */
	    );
	    skew = 0.0;
	}  /* if 1 look */
	else {   /* 4 looks */
	    if (slrange) { 
		az4l_pre(
		    /* inputs (should be 6):  */
			ct_outlen,roffset,prf,spf.pbw,&pp->fda,&pp->fdota,
		    /* outputs (should be a zillion (actually, 17)): */
			d1,d2,d3,d4,d5,d6,&lookpos,&looklen,/* XFER stuff */
			d7,&ptspace,		     /* AZINT stuff */
			d8,&vlpts,&nov,&azoff,	     /* MLC/MLM stuff */
			&bufrdlen,			     /* IIF/CTC */
			&pbwx,&istat	      	     /* misc. stuff */
		);
		skew = 0.0;
	    }
	    else {
		fzero2nul = (2.0 * pp->swth_speed) / 10.0;
		/* fudge_fctr = 206.0;  change from 0 to 206 2/14/96 */
		if (!strncmp(Cur_Rqst->take_id,"E",1))
		     fudge_fctr = 0.0;
		else if (!strncmp(Cur_Rqst->take_id,"J",1))
		     fudge_fctr = 250.0;
		else if (!strncmp(Cur_Rqst->take_id,"R",1))
		     fudge_fctr = 206.0;
		else {
		     printf("Job is neither ERS,JERS,nor RSAT, set ");
		     printf("fudge_fctr = 0.0\n");
		     fudge_fctr = 0.0;
		}
	        if (vbose) printf("FUDGE %f\n", fudge_fctr);
		az4l(
		    /* inputs (should be 14):  */
			ct_outlen,roffset,flag_azwgt,deskew,prf,spf.pbw,
			fzero2nul,h,azsp,spf.grspace,fdg,fdotg,skew_adj,
			fudge_fctr,
		    /* outputs (should be a zillion (actually, 19)): */
			d1,d2,d3,d4,d5,d6,&lookpos,&looklen,/* XFER stuff */
			d7,&ptspace,		    /* AZINT stuff */
			d8,&vlpts,&nov,&azoff,	    /* MLC/MLM stuff */
			&bufrdlen,			    /* IIF/CTC */
			&pbwx,&err_offset,&skew,&istat  /* misc. stuff */
		);
	    }
	}  /* else 4 looks */

	if (istat) {
	    printf("...error %d in azimuth function generation\n",istat);
	    if (istat > 1)
		return (FAIL);
	}
	if (strcmp(Cur_Rqst->type,"CSD") != 0) {
	    ap_lookpos = lookpos;
	    ap_looklen = looklen;
	    ap_ptspace = ptspace;
	    ml_vppl = vlpts;
	    ml_lkoff = nov;
	    ml_froff = azoff;
	    min_delay = (int) ((prf * pbwx) / 
			(fabs(pp->fdota) * bufrdlen))
	    		+ 2;
	    ml_frdelay = (int) ceil(fabs(skew) / (double) azoff)
			+ min_delay;

/* validate the frame delay, M. Chen, 3/16/93 */
	    ml_frdelay += 1;

	    in_vlines = ct_rdlen1 = ct_rdlen2 = bufrdlen;
	    in_ilines = ct_outlen - in_vlines;
	    if (strcmp(Cur_Rqst->type,"CPX") == 0) {
		ml_frdelay = 2;
		in_swstart = start_bin;
		in_swstart = 512*(int)((float)(in_swstart)/512.+0.5);
                if (!strncmp(Cur_Rqst->take_id,"R",1)) /* MC 3/5/96 */
		    in_ilines = 50000;
		else {
		    /*increase to 40000 from 20000 due to hang of JERS CPX
		    in_ilines = 3*in_vlines+20000;*/
		    in_ilines = 3*in_vlines+40000;
		}
	    } else if ( Res_type != LOW_RES ){
		in_ilines = 3*in_ilines;
	    }
	}
		/* np_delay should have 9 inputs */
	temp = (ml_frdelay > 8) ? 8 : ml_frdelay;
	pulse_delay = np_delay(looks,temp,bufrdlen,prf,pbwx,
				pp->fdota,skew,azsp,spf.grspace);
	pp->bw_az4_act = pbwx;
	if (write_to_disk == ON) {
	    p_write_mem_file(d1,8192,xname,".aw","azimuth weight");
	    p_write_mem_file(d2,2048,xname,".bm","beam wavelength");
	    p_write_mem_file(d3,2048,xname,".ls","look start");
	    p_write_mem_file(d4,2048,xname,".qd","quadratic offset");
	    p_write_mem_file(d5,2048,xname,".th","theta offset");
	    p_write_mem_file(d6,2048,xname,".wt","weight start");
	    p_write_mem_file(d7,8192,aname,"","azint first good point");
	    p_write_mem_file(d8,1024,mname,"","multilook rotate");
	    strcpy(ap_xfun,xname);
	}
	else {
	    setup_xfer_mem (d1,d5,d2,d4,d6,d3,1,ct_outlen);
	    setup_az_mem (d7);
	    setup_mlc_mem (d8);
	}
	return (PASS);
}
/* e1_build_image_files(sp,pp,seg,region) ------------------------------
	This routine creates the image directory and processing files
	for one image, centered on preprocessing region pp.
*/

e1_build_image_files(sp,pp,seg,region)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	int seg,region;
{
	char image_id[20],short_id[20];
	char filename[80];
	char cmd[100];
	char *pname = "params.proc";
	double time,time2,dsec,prf,pri,a[3],b[3],c[3],d[3],e[3];
	double dist,dist2,range,latm,lonm,latr,lonr;
	double pi = 3.141592653589793;
	double deg_to_rad = pi / 180.0;
	double cos2(), sqrt();
	double ad,be,ae,bd,ff,dd,xx,yy,zz,svec[6],rads[4];
	double sw_adj,skew_adj,fd_adj;
	GMT start_gmt,save_gmt;
	int stfmt,endfmt,minfmt,maxfmt,nfmts,tfmts;
	int nframes,offset,deskew,looks,start_bin;
	int mo,day,sec,mills,targ_range,f_size;
	int ngains=0,i;
	double deltat = 0.1;
	float dtheta = 0.1;
	int ifirst = -38;
	int images = 0;
	int fft_change = 0;
	float r_e,f,sclmcv,r_cls_1,r_cls_2,x[16384];
	int npts,istat,l1,l2,l3,l4;
	short int d1[16384];
        float gamma;
	int roll_tilt;

	if (!cp_flag) {
	    printf("CP flag is off\n");
	    sprintf(filename,"%sgaintbl",DEF_PATH);
	    ngains = GetCalFile( filename );
	    if ( ngains == FAIL ){
		printf("e1_build_image_files: Can't find %s (use default)\n", 
			filename);
	    }  	    
	}
	else {
            ngains = AspReadCalFile();   /* reading the CPF file */
	    if (ngains == FAIL){
            	printf("Error in reading CPF file, ngains=%d \n", ngains);
		return(FAIL);
	
                /* in case ASP does not receive the right CPF file from PVS,then
                read gaintbl file in the default dir. It should be return FAIL
                if there is error in CPF file in the future
                                                                 
		printf("Reading file gaintbl in default directory\n");
	        sprintf(filename,"%sgaintbl",DEF_PATH);
	        ngains = GetCalFile( filename );
	        if ( ngains == FAIL ){
		   printf("e1_build_image_files: Can't find %s (use default)\n", 			filename);
		} */  	    
	    }
        } 

	if (vbose) printf("ngains=%d\n", ngains);                                                     	
       for (i=0; i<ngains; i++)
                printf("gaintbl[%d]=%11.7f\n",i,gaintbl[i]);

	strcpy(image_id,pp->img_name); /* for scene_file */

    /* move range spectra record to image directory */
/*
	sprintf(cmd,"mv ../rng_spctra.%d%.2d rng_spectra",
		seg,region);
	system(cmd);
*/

    /* initialize the processing parameters */
	sprintf(filename,"%s%s",DEF_PATH,pname);
	read_proc_param_file(filename);

    /* add in fft scaling and detector shift values from pass 2 & 3 */  
       if (Cur_Rqst->id[9]=='P') {

    /* compare fft scaling in pp file with df_proc_params file */
           if ((pp->fftsc_raf == dpf.fftsc_raf) &&
                (pp->fftsc_rai == dpf.fftsc_rai) &&
                (pp->fftsc_az4f == dpf.fftsc_az4f) &&
                (pp->fftsc_az4i == dpf.fftsc_az4i))
                ap_detshift = 6;
           else {
                fft_change = 1;
                ap_detshift = pp->det_sc;
           }
        } else
                ap_detshift = 6;
	rc_fscale = pp->fftsc_raf;
	rc_iscale = pp->fftsc_rai;
	af_scale = pp->fftsc_az4f;
	ap_scale = pp->fftsc_az4i;
	/*ap_detshift = pp->det_sc;*/

	if (strcmp(Cur_Rqst->type, "CPX") == 0) 
		ap_detshift = -1;

    /* calculate start format and # of frames */
	minfmt = sp->fmt_id + 3;
	if (minfmt < 11)
	    minfmt = 11;
	if (sp->post == NULL || sp->gap_type == 2)
	    maxfmt = sp->end_id;
	else
	    maxfmt = sp->post->fmt_id - 8;
	dist = ((strcmp(Cur_Rqst->type,"STD") == 0) ||
		(strcmp(Cur_Rqst->type,"QLK") == 0)) ?
		44800.0 : 51200.0;
	dist2 = 102400.0;
	if (strcmp(Cur_Rqst->type,"CPX") == 0) {
	    dist /= 2.0;
	    dist2 = 51200.0;
	}
	prf = sp->pre->prf;
	time = dist / pp->swth_speed;
	time2 = dist2 / pp->swth_speed;
	stfmt = pp->fmt_id - (time * prf + 0.5);
	if (stfmt < minfmt)
	    stfmt = minfmt;
	tfmts = nfmts = time2 * prf + 0.5;

    /* determine setup parameters */
	deskew = (strcmp(Cur_Rqst->type,"STD") == 0) || 
		 (strcmp(Cur_Rqst->type,"QLK") == 0) || 
		 (strcmp(Cur_Rqst->deskew,"NOT") != 0);
	looks = 4;
	if (strcmp(Cur_Rqst->type,"CPX") == 0) {
	    looks = 1;
	    deskew = 0;
	}

    /* call corn_loc with estimated start time, to get
       earth radius at scene center */
	pri = 1.0 / prf;
	dsec = (pp->fmt_id - stfmt) * pri;
	start_gmt = pp->sv.gmt;
	add_seconds(&start_gmt,-dsec);
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
	fd_adj = 0.0;
	get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,deskew,fd_adj,
		     a,b,c,d,e,&istat);
	pp->r_e = c[2] / 1000.0;
	rads[0] = a[2];
	rads[1] = b[2];
	rads[2] = d[2];
	rads[3] = e[2];
    /* refine the swath speed and scene length */
	get_abcd_dist(a,b,d,e,&ad,&be,&ae,&bd);
	sw_adj = (ad + be) / (2.0 * dist2);
	pp->swth_speed *= sw_adj;
	time = dist / pp->swth_speed;
	time2 = dist2 / pp->swth_speed;
	stfmt = pp->fmt_id - (time * prf + 0.5);
	if (stfmt < minfmt)
	    stfmt = minfmt;
	tfmts = nfmts = time2 * prf + 0.5;
	skew_adj = (ae - bd) / sqrt(2.0);
    /* if CPX request by lat/lon, calculate range offset */
	start_bin = 0;
	if (strcmp(Cur_Rqst->type,"CPX") == 0) {
	    if (Cur_Rqst->targ.yr)
		targ_range = Cur_Rqst->targ_rg;
	    else {
		latr = Cur_Rqst->lat * deg_to_rad;
		lonr = Cur_Rqst->lon * deg_to_rad;
		latm = ((a[0] + d[0]) / 2.0) * deg_to_rad;
		lonm = ((a[1] + d[1]) / 2.0) * deg_to_rad;
		targ_range = (int) (pp->r_e * acos(cos2(latm,lonm,latr,lonr)));
	    }
	    start_bin = (targ_range - 20) * 49;
	    if (start_bin < 0)
		start_bin = 0;
	    if (start_bin > 2866)
		start_bin = 2866;
	}

    /* calculate and write the azimuth cage setups */
	setup_ap(sp,pp,looks,ON,OFF,deskew,start_bin,rads,skew_adj);

    /* calculate the fd adjustment factor */
	if (looks == 4)
	    fd_adj = (skew_adj * ap_rmlines) / (pp->swth_speed * 8192.0);
	else
	    fd_adj = 0.0;
	if ((strcmp(Cur_Rqst->type,"RPR") == 0) && deskew == 0)
	    fd_adj = 0.0;

    /* validate the frame delay */

	/*Tom's Fix 12-17-92*/
/*
	ml_frdelay += 1;
*/
	if (strcmp(Cur_Rqst->type,"CSD") == 0 || strcmp(Cur_Rqst->type,"CPX") == 0)
		ml_frdelay = 2; /* Ming for Tom, 10/20 */
	
	if (ml_frdelay > 8) {
	    if (frdelay_sw == 0) {
		printf("\n\07Multilook frame delay = %d\n",ml_frdelay);
		printf("    (max is 8)\n");
/* comment out for R1a by clw 10-11-94 
		if (op_answer("Do you want to abort processing?")
			== PASS)
*/
		    return (ABORT);
	    }
	    frdelay_sw = 1;
	    printf("Frame delay was %d, forced to 8\n",ml_frdelay);
	    ml_frdelay = 8;
	}
    /* calculate the real start format and number of formats */
    /*	nframes = ml_frdelay + 1 + (nfmts + in_vlines - 1) / in_vlines; */
	nframes = ml_frdelay + 1 + (8192 / ml_froff) + 1;
	if ((strcmp(Cur_Rqst->type,"CSD") == 0) ||
	    (strcmp(Cur_Rqst->type,"CPX") == 0))
	    nframes = in_scenelen;
	nfmts = nframes * in_vlines;

	if ((strcmp(Cur_Rqst->type,"STD") != 0)
		&& (strcmp(Cur_Rqst->type,"QLK") != 0)) {
	    stfmt -= pulse_delay;
	    if (stfmt < minfmt)
		stfmt = minfmt;
	}
	endfmt = stfmt + nfmts;
	if (endfmt > maxfmt) {
	    endfmt = maxfmt;
	    stfmt = endfmt - nfmts;
	}
    /* if not enough formats, refuse to make an image */
	if (stfmt < minfmt) {
	    printf("...not enough data to make a whole image\n");
	/* delete image directory and reference to it */
	    chdir(JOB_PATH);
	    sprintf(cmd,"rm -r %s\n",image_id);
	    system(cmd);
	    pp->img_name[0] = '\0';
	    return (FAIL);
	}

    /* write the radiometric compensation vector */
	l1 = 1; l2 = 1; l3 = 2; l4 = 2;
	rdminit_(&l1,&l2,&l3,&l4);
	r_e = (float) emf.r_e * 1000.0;
	f = (float) emf.f;
	svec[0] = pp->sv.pos.x * 1000.0;
	svec[1] = pp->sv.pos.y * 1000.0;
	svec[2] = pp->sv.pos.z * 1000.0;
	svec[3] = pp->sv.vel.x * 1000.0;
	svec[4] = pp->sv.vel.y * 1000.0;
	svec[5] = pp->sv.vel.z * 1000.0;
	npts = pr_rglen;
	range = pp->r_close;
	if (strcmp(Cur_Rqst->type,"CPX") == 0)
	    range += in_swstart * spf.srspace;
        gamma = spf.gamma_nom;
        roll_tilt = 0;
        if (sv1.rev >= 3779 && sv1.rev <= 3865) {
            roll_tilt = 1;
            gamma = 30.0;
        }
        if (sv1.rev >= 15384 && sv1.rev <= 15516) { /* CV add 3/26/97 */
           printf ("Special angle 21.3 applied.\n");
            gamma = 21.3;
        }
	sclmcv = 2.9E13;  /* E-ERS-1 specific scale factor */
	rdmtric_(&istat,
		 &svec[0],&svec[3],&pp->att,&gamma,
		 &range,&spf.srspace,&pp->fdota,
		 gaintbl,&ngains,&ifirst,&dtheta,&r_e,&f,&npts,
		 d1,&sclmcv,x);
	if (vbose) printf("scaling factor = %g\n", sclmcv);
	p_write_mem_file(d1,npts,"rc_funct","","radiometric comp.");

    /* hardcoded signal meas. values to prevent table generating */
	pp->iqrephase = 90.0;
	pp->imean = pp->qmean = 15.5;
	pp->istdv = pp->qstdv = 3.0;
	p_make_lookup_tbl(pp,d1);
	p_write_mem_file(d1,8192,"input_trans","","input lookup tbl.");

    /* copy the range transfer function file */
	if (strcmp(Cur_Rqst->type,"CSD") != 0) {
	    sprintf(cmd,"cp %srf_ra_file .",DEF_PATH);
	    system(cmd);
	}

    /* build the scene file */
	p_init_scene_file();
	strcpy(sf.sw_id,"ASP");
	strcpy(sf.file_name,image_id);
    /* add fudge factor equal to 1.2 x frame size */	
	dsec = (pp->fmt_id - stfmt - pulse_delay + 1.2*in_vlines) * pri;
	start_gmt = pp->sv.gmt;
	add_seconds(&start_gmt,-dsec);
    /* add the delay time due to zero doppler line */
	dsec = - ((double)fd3p[0])/fabs((double)fr3p[0]);
	add_seconds(&start_gmt,- dsec);
	istat = 0;
	get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,deskew,fd_adj,
		     a,b,c,d,e,&istat);
    /* if RPR or CPX by LAT_LON, fine tune the center */
	if ((strcmp(Cur_Rqst->type, "RPR") == 0 ||
	     strcmp(Cur_Rqst->type, "CPX") == 0 ) &&
	     (Cur_Rqst->targ.yr == 0)) {
	    if (vbose) printf("before tune, lat=%g, lon=%g\n",c[0],c[1]);
	    if (vbose) printf("before tune, stfmt=%d\n",stfmt);
	    latr = Cur_Rqst->lat * deg_to_rad;
	    lonr = c[1] * deg_to_rad; /* based on LAT only */
	    latm = c[0] * deg_to_rad;
	    lonm = c[1] * deg_to_rad;
	    targ_range = (int) (pp->r_e * acos(cos2(latm,lonm,latr,lonr)));
	    if ((latr > latm && pp->sv.vel.z > 0.) ||
	        (latr < latm && pp->sv.vel.z < 0.)) targ_range *= -1;
	    dsec = targ_range*1000. / pp->swth_speed;
	    if (vbose) printf("center off by %d km, or %g sec\n",targ_range,dsec);
	    add_seconds(&start_gmt,- dsec);
	    stfmt -= dsec * prf;
	    if (stfmt < minfmt) {
		stfmt = minfmt;
		printf("force stfmt = %d, no scene center warranty\n", minfmt);
		return(FAIL);
	    }
	    istat = 0;
	    get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,deskew,
		fd_adj,a,b,c,d,e,&istat);
	    if (vbose) printf("after tune, lat=%g, lon=%g\n",c[0],c[1]);
	    if (vbose) printf("after tune, stfmt=%d\n",stfmt);

            /* fix the 1.5 km off anomaly CV */
            if (strcmp(Cur_Rqst->type, "RPR") == 0){
	     if (vbose)printf("pre_dwp=%d,wfmt=%d\n",pre_dwp,sp->wfmt[pre_dwp]);
             if (stfmt < sp->wfmt[pre_dwp]) {
               stfmt = pp->fmt_id - (8*prf);
               if (vbose) printf("before tune, lat=%g, lon=%g\n",
                        c[0],c[1]);
               if (vbose) printf("before tune, stfmt=%d\n",stfmt);
               latr = Cur_Rqst->lat * deg_to_rad;
               lonr = c[1] * deg_to_rad;
               latm = c[0] * deg_to_rad;
               lonm = c[1] * deg_to_rad;
               targ_range=(int) (pp->r_e *
				acos(cos2(latm,lonm,latr,lonr)));
	       if (vbose) printf("latr=%g,lonr=%g,latm=%g,lonm=%g,t_range=%d\n",
                        latr,lonr,latm,lonm,targ_range);
               if ((latr > latm && pp->sv.vel.z > 0.) ||
                  (latr < latm && pp->sv.vel.z < 0.)){
                         targ_range *= -1;
                        printf("targ_range=%d\n", targ_range);
                }
               dsec = targ_range*1000. / pp->swth_speed;
               if (vbose) printf("center off by %d km, or %g sec\n",
                        targ_range,dsec);
               add_seconds(&start_gmt,- dsec);
               stfmt -= dsec * prf;
               if (stfmt < minfmt) {
                  stfmt = minfmt;
                  printf("force stfmt = %d, no scene center warranty\n",
                      minfmt);
                  return(FAIL);
               }
               istat = 0;
               get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,deskew,
                   fd_adj,a,b,c,d,e,&istat);
               if (vbose) printf("after tune, lat=%g,lon=%g\n",c[0],c[1]);
               if (vbose) printf("after tune, stfmt=%d\n",stfmt);
             }
            }
	}	        
	sf.lat_scene_ctr = c[0];
	sf.lon_scene_ctr = c[1];
	pp->lat_rough = c[0];
	pp->lon_rough = c[1];

    /* checking for window position change */
	r_cls_1 = r_cls_2 = 0.;
	/*
	f_size = in_vlines * (in_scenelen-2);
	*/
	f_size = 28000;
	if (strcmp(Cur_Rqst->type,"CPX") == 0) 
	    f_size = 15360;
	if (region == 1) wdchk = 1;

	while (stfmt > sp->wfmt[wdchk] && wdchk < sp->win_count) {
	    wdchk++;
	    if(vbose)
	    printf(" Jumping input ..., stfmt = %d, wdchk = %d\n",
		    stfmt, wdchk);
	}
	if(stfmt + f_size > sp->wfmt[wdchk] && 
			wdchk < sp->win_count){
	    if(vbose)
		printf(" Window change ..., stfmt = %d, wdchk = %d\n",
			stfmt, wdchk);
	    r_cls_1 = (float)(pp->r_close) - 2.99792458E8 /2. * 
		      ((float)(sp->wdwp[wdchk-1]) * dpf.b_prf - dpf.a_dwp + 
                      (9. + roll_tilt)/prf);
	    r_cls_2 = (float)(pp->r_close) - 2.99792458E8 /2. *
		      ((float)(sp->wdwp[wdchk]) * dpf.b_prf - dpf.a_dwp + 
                      (9. + roll_tilt)/prf);
	    if(vbose) {
	    printf(" ... Window position changed\n");
	    printf(" r_cls_1 = %g, r_cls_2 = %g\n", r_cls_1, r_cls_2);
	    printf(" pp->r_close = %g, wdwp = %d, b_prf = %g, prf=%g\n",
			pp->r_close, sp->wdwp[wdchk], dpf.b_prf, prf);
	    }
	    if(r_cls_1 >= r_cls_2) {
	        in_offset1 = 0;
	        in_offset2 = (int)((float)(sp->wdwp[wdchk] - 
			sp->wdwp[wdchk-1]) * (dpf.b_prf) * (spf.csr) + 0.5);
		sf.wndw_pos = (sp->wdwp[wdchk]*dpf.b_dwp - dpf.a_dwp) * 1000000.;
	    }
	    else {
	        in_offset1 = (int)((float)(sp->wdwp[wdchk-1] -
			sp->wdwp[wdchk]) * (dpf.b_prf) * (spf.csr) + 0.5);
		in_offset2 = 0;
		sf.wndw_pos = (sp->wdwp[wdchk-1]*dpf.b_dwp - dpf.a_dwp) * 1000000.;
	    }
	    /*
	    if(in_offset1 < 0) in_offset1 += 8192;  
	    if(in_offset2 < 0) in_offset2 += 8192;  
	    */
	    if(in_offset1 < 0 || in_offset2 < 0) {
		printf(" ERROR!!! Negative offset in IIF\n");
		printf(" offset1 = %d, offset2 = %d\n",
				in_offset1, in_offset2);
	    }
	    in_offch = sp->wfmt[wdchk] - stfmt;
	    if ((wdchk+1) < sp->win_count) wdchk++;
	  }
	else {
	    in_offset1 = (int)((sp->wdwp[wdchk-1]*dpf.b_prf - dpf.a_dwp) * 
                         spf.csr - ( (float)(pp->r_close)/(2.99792458E8/2.) - 
                         (9. + roll_tilt)/prf) * spf.csr + 0.5);
	    if (in_offset1 < 0) { 
		printf("ERROR!!! Negative offset - no window change\n");
		printf("r_close = %g, wdchk-1 = %d, wdwp[wdchk-1] = %d, offset = %d\n",
		pp->r_close, wdchk-1, sp->wdwp[wdchk-1], in_offset1);
		in_offset1 = -in_offset1;
	    }
	    in_offset2 = in_offset1;
	    in_offch = 0;  /* dummy */

	/* add the win_pos in the scene_file 6/21/96 */	
	    if (wdchk >= sp->win_count) {
	       sf.wndw_pos = (sp->wdwp[sp->win_count-1]*dpf.b_dwp - dpf.a_dwp) * 1000000.;
	    } else 
	       sf.wndw_pos = (sp->wdwp[wdchk]*dpf.b_dwp - dpf.a_dwp) * 1000000.;
  	}  

	printf("wndw_pos=%g\n",sf.wndw_pos);
	sf.re_image_ctr = c[2] / 1000.0;
	if (spf.side == 'R') {  /* right looking radar:   */
	    sf.lat_a = d[0];	/* image will be inverted */
	    sf.lon_a = d[1];	/* by the post-processor  */
	    sf.lat_b = e[0];
	    sf.lon_b = e[1];
	    sf.lat_d = a[0];
	    sf.lon_d = a[1];
	    sf.lat_e = b[0];
	    sf.lon_e = b[1];
	}
	else {			/* left looking radar:        */
	    sf.lat_a = a[0];	/* image will not be inverted */
	    sf.lon_a = a[1];	/* by the post-processor      */
	    sf.lat_b = b[0];
	    sf.lon_b = b[1];
	    sf.lat_d = d[0];
	    sf.lon_d = d[1];
	    sf.lat_e = e[0];
	    sf.lon_e = e[1];
	}
	dsec = get_gmt_diff(&start_gmt,&pp->sv.gmt);
	newtprop_(&pp->sv.pos,&dsec,&deltat,&sf.x_begin);
	dsec = (dist2 / (2.0 * pp->swth_speed));
	newtprop_(&sf.x_begin,&dsec,&deltat,&sf.x_mid);
	newtprop_(&sf.x_mid,&dsec,&deltat,&sf.x_end);
	add_seconds(&start_gmt,dsec);
	sec = start_gmt.second;
	mills = (start_gmt.second - sec) * 1000.0;
	sprintf(sf.tme_scene_ctr,"%.4d%.3d:%.2d:%.2d:%.2d.%.3d",
		start_gmt.yr,start_gmt.day,start_gmt.hr,
		start_gmt.min,sec,mills);
	ff = 1.0 / emf.f;
	xx = sf.x_mid * sf.x_mid;
	yy = sf.y_mid * sf.y_mid;
	zz = sf.z_mid * sf.z_mid;
	dd = (xx + yy) / (xx + yy + zz);
	sf.re_nadir = emf.r_e * (1 - ff) / sqrt(1 - (ff * (2-ff)) * dd);
	if (strcmp(Cur_Rqst->type,"STD") == 0) {
	    sf.np = 8192;
	    sf.nl = 8192;
	} else if (strcmp(Cur_Rqst->type,"RPR") == 0) {
	    sf.np = 8192;
	    sf.nl = 8192;
	} else if (strcmp(Cur_Rqst->type,"QLK") == 0) {
	    sf.np = 8192;
	    sf.nl = 8192;
	} else if (strcmp(Cur_Rqst->type,"CPX") == 0) {
	    sf.np = 2048;
	    sf.nl = 12800;
	} else if (strcmp(Cur_Rqst->type,"CSD") == 0) {
	    sf.np = 5632;
	    sf.nl = 26624;
	}
	sf.pnum_scene_ctr = sf.np / 2;
	sf.lnum_scene_ctr = sf.nl / 2;

	memset(sf.sp_id,'\0',sizeof(sf.sp_id));
	if (!strncmp(Cur_Rqst->take_id,"E1",2)){
		strcpy(sf.sp_id,"ERS-1"); 
	} else if (!strncmp(Cur_Rqst->take_id,"E2",2)){
		strcpy(sf.sp_id,"ERS-2"); 
	}

	strcpy(sf.asp_ver,ASP_VERSION);
	strcpy(sf.proc_type,"SF");
	if (strcmp(Cur_Rqst->type,"QLK") == 0)
	    strcpy(sf.proc_type,"QF");
	strcpy(sf.image_id,&image_id[1]);
	sf.r_near = r3p[0] / 1000.0;
	sf.r_mid = r3p[1] / 1000.0;
	sf.r_far = r3p[2] / 1000.0;
	sf.fdga = fdg[0];
	sf.fdgb = fdg[1];
	sf.fdgc = fdg[2];
	sf.fdotga = fdotg[0];
	sf.fdotgb = fdotg[1];
	sf.fdotgc = fdotg[2];
	if ( (strcmp(Cur_Rqst->type,"CSD") == 0) ||
	     (strcmp(Cur_Rqst->type,"CPX") == 0) ||
	     ((strcmp(Cur_Rqst->type,"RPR") == 0) &&
	      (Cur_Rqst->gnd_slnt_rg[0] == 'S')) ) {
		sf.fdga = pp->fda;
		sf.fdgb = pp->fdb;
		sf.fdgc = pp->fdc;
		sf.fdotga = pp->fdota;
		sf.fdotgb = pp->fdotb;
		sf.fdotgc = pp->fdotc;
	}
    /* write the scene file */
	p_rw_scene_file(1,"scene_file");
	images++;

    /* finish up and write processing parameters file */
    	if (strcmp(Cur_Rqst->type,"RPR") == 0) {
		ap_detshift -= (Cur_Rqst->proc_gain - pp->pro_gain)/3;
		pp->pro_gain += Cur_Rqst->proc_gain - pp->pro_gain;
	}
        if (Cur_Rqst->id[9]=='P') {
                if (fft_change == 0)
                        ap_detshift -= pp->pro_gain/3;
        }
	in_scenelen = nframes;
	e1_get_fmt_loc(-1,stfmt,sp,&in_startblock,&offset);
	in_blkoff = offset / 8;
	in_shift = offset % 8;
	in_norminv = sp->polarity;

	write_proc_param_file(pname);

	return (PASS);
}
