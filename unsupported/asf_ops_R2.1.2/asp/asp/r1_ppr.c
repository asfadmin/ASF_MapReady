/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* r1_proc_ppr.c -- RSAT process a preprocessing region
		    Using 2DFFT for Doppler measurement.
		    There is no azimuth scaling.
   CV 3/18/96  - replace polarity to 1 in start_fmt_play() 
   MC 05/16/96 - Add doppler_measure for ambiguity determination
   CV 01/23/96 - comment out the calculation total desired processing gain 
   CLW 12/14/95 - Add r1_get_aux_data to fill in sp->aux for frame job
   CLW 12/06/95 - Modify r1_repmeas & r1_get_replica for replica extraction
		  Add definition AUX_SYNC_CODE
   CLW 11/30/95 - Modify rc_reffun in params.proc respect to
			different RADARSAT mode (ST1..ST7)
   CLW 11/14/95 - modify r1_get_ephem to fill DEC/ALPHA compatible binary
			format
   CLW 11/9/95 - add asp_read to r1_get_ephem subroutine
   CLW 11/8/95 - correct start_fmt_play to pass satellite RADARSAT
   CLW 8/25/95 - comment out op_answer
   CLW 8/1/95  - remove r1_setup_ap (it's identical to setup_ap in e1_ppr.c
   CLW 6/27/95 - modify sf.sp_id to RSAT-1
   CLW 5/26/95 - extern int wdchk
		 remove function linear_qr that exists in e1_ppr.c
		 remove r1_build_proc_files & r1_put_gmt
		 modify anywhere with EDFM to RDFM address location
		 modify for VME interface
   Ming Chen, 10/3/94.
   
*/

#include <aspdecl.h>
#include <procdec.h>
#include <procfil.h>
#include <procpar.h>
#include <scene_file.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ephem.h>
#include <netinet/in.h>

/* RSAT-1 header data format definitions ??? TBF*/
#define HDR_LEN (50 >> 1) /* # of bytes of header stored per format */
#define HDR_LEN2 (22 >> 1) /* bytes of header per format (short mode) */
#define AUX_LOC (1 >> 1)  /* start location of auxiliary data */
#define AUX_LEN (50 >> 1)  /* length of auxiliary data */
#define AUX_SYNC_CODE 0x352ef853
#define REP_LOC (20 >> 1) /* start location of replica/drift data */
#define REP_LEN (72 >> 1)  /* length of replica/drift data */
#define REP_LAST (24 >> 1)  /* length of replica/drift last line */
#define PERMS 0644

/*#define HDR_LEN (100 >> 1)
#define AUX_LOC (2 >> 1)  
#define AUX_LEN (100 >> 1) */

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
extern TAPE_SEG_PTR seg_list;

/* Parameter and data files */
extern SP_FILE spf;		/* sensor parameters file */
extern DPP_FILE dpf;		/* default processing parameters file */
extern EM_FILE emf;		/* earth model file */
extern SCENE_FILE sf;		/* scene file */

extern TAPE_SEG_PTR seg_list;	/* tape segment list */

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
extern int wdchk;		/* counter for window change */
float fmt_ratio;

/* Locally global data */
static int first_region = 1;	/* 1 = first pp region in data take */
static int repoff = 8;		/* offset (0 to 23) to replica start */

double r3p[3];			/* 3 ranges for corn_loc routine */
float fd3p[3];			/* 3 corresponding fd values */
float fr3p[3];			/* 3 corresponding fr values */
/* static double r3p[3];		/* 3 ranges for corn_loc routine */
/* static float fd3p[3];		/* 3 corresponding fd values */
/* static float fr3p[3];		/* 3 corresponding fr values */
float fdg[3],fdotg[3];		/* ground Doppler coef */

extern int pulse_delay;		/* # of prf lines delay to first 
					output pixel */
static int frdelay_sw = 0;	/* 1 = clamp frame delay to 8 */

int fmt_tobe;			/* variable to trace format number */

extern int detshift_old;	/* pass set_gain result to even image */
extern int VALID_PIXELS;
extern double rsat_echo_delay[];  /* clw 1/31/96 cv 2/14/96 */ 
extern int st_fmt;
extern int cp_flag;
extern float gaintbl[900];
extern int max_agc;

/*SONY*/
extern char media_type[];
extern char data_direction[];

double get_gmt_diff();
RSAT_EPHEM_DATA ephem;


/* r1_2dfft(sp,pp,region) -------------------------------------------
	This routine determines the Doppler frequency by 2DFFT
*/

r1_2dfft(sp,pp,region)
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
	char rcvd[30];
	int dc_timeout;
	int file_mj;
	int nfmts, clock_gain, blkno;
	short int data_2d[49152];
        int flag_linear;

    	read_rcdr = IN;

    /* if preprocessing only, do fine tune LTRR */
        if (sw_pp_only == 1 && first_odd == 0) {
    	    if (vbose) printf("LTRR ... again\n");
	    nfmts = pp->fmt_start - fmt_tobe - 12000;
	    if (nfmts < 40) {
	        printf("nfmt = %d, No tape forword\n", nfmts);
	        return(FAIL);
	    }
	    nfmts = 8 * (int)((nfmts+4)/8.);
	    if (r1_ltrr(sp,pp,nfmts) == FAIL) return(FAIL);
	    fmt_tobe = pp->fmt_start;
	}

	if (do_repmeas == 1) {
	    if (vbose) printf("Perform replica measurement!\n");
  	    if (first_odd == 1) {
	    	printf ("Initializing READ recorder...\n");
	    	if (rcdr_start (read_rcdr) == FAIL)
		    return (FAIL);
	    	reset_rpi(1);
	    	fmt_tobe = pp->fmt_start;
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
/* M. Chen, 12-5-95, Is IIF setup for replica extraction? */
/*	    setup_rdfm(dpf.sbm,dpf.fsm,1,dpf.maxfsdo); */
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
/*SONY*/
                  if (!strcmp( media_type, "DCRSI" ) ){
                    if (vbose)
                      printf("starting DCRSi at %d, offset %d, polarity %d\n",
                            pp->blk_start,pp->bit_off,pp->polarity);
	    	    start_fmt_play(2,pp->blk_start,pp->bit_off-32,1); /*CV 3/18/97*/
                  }
                  else {
                      if (vbose)
                        printf("starting SONY at %d, offset %d, polarity %d\n",
                            pp->blk_start,pp->bit_off,pp->polarity);
                      if (!strcmp( data_direction, "REVERSE" ) ){
			printf("blk_start=%d\n",pp->blk_start);
                        sony_start_fmt_play_REV(2,pp->blk_start,pp->bit_off-184, 1);
                      } else
                        sony_start_fmt_play(2,pp->blk_start,pp->bit_off-184,1);
                       }

                reset_rpi(0);
                first_odd = 0;
            }
               

	/* arm and trigger */
	    r1_clear_hdr_mem(0);
	    tarm();
	    play_dis(0);
	    tdone();
	    play_dis(1);

	/* at this point, the data should be captured */
	    fmt_tobe += 312; /* we play 312 formats for repmeas */
	    if (vbose) {
		printf("\n...in repmeas after trigger, fmt = %d\n", fmt_tobe);
	    }
	/* check dropout bit */
	    asp_read( (RLOC_RDFM+14)<<1, &mb.w[RLOC_RDFM+14], 2 );
	    if ((mb.w[RLOC_RDFM+14] & 0x100) == 0) {
		printf("dropout detected in sig_meas\n");
		return(FAIL);
	    }

    	/* perform replica measure */
	    r1_repmeas(sp,pp);

	} /* complete rep_meas */

	else
	    pp->iqrephase = 90.0;

	doppler_measure(sp, pp);

	if (sw_clutter_lock == 0) {
	    printf("2DFFT Bypassed ...\n");
	    return (PASS);
	}
	else {
	    if (vbose) printf("Welcome to 2DFFT processing!\n");

  	    if (first_odd == 1) {
	    	printf ("Initializing READ recorder...\n");
	    	if (rcdr_start (read_rcdr) == FAIL)
		    return (FAIL);
	    }
	    if (first_odd != 0) {
	    	reset_rpi(1);
	    	fmt_tobe = pp->fmt_start;
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
            in_chirp = (sp->aux.beam_seq < 3) ? 821 : 575;
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
	    clear_fft_oflo_regs ("af");

	    iif_set_reset (0);		/* release IIF reset */
	    avg_set_reset (0);
	    set_avg_wrmode (0x001);
	    mlc_set_reset (0);
	    rm_ovf_reset ();
	    ctc_set_reset (0);

	/* if first region, start the DCRS at the right location */
	    if (first_odd != 0) {
/*SONY*/
                  if (!strcmp( media_type, "DCRSI" ) ) {
                    if (vbose)
                      printf("starting DCRSi at %d, offset %d, polarity %d\n",
                            in_blkoff,in_shift,pp->polarity);
	    	    start_fmt_play(2,pp->blk_start,pp->bit_off-32,1); /*CV 3/18/97*/
                  }
                  else {
                        if (vbose)
                        if (!strcmp( data_direction, "REVERSE" ) ){
                         sony_start_fmt_play_REV(2,pp->blk_start,pp->bit_off-184
,1);
                        } else

                         sony_start_fmt_play(2,pp->blk_start,pp->bit_off-184,1);
                       }

                reset_rpi(0);
                first_odd = 0;
	    }

	/* arm and trigger */
	    r1_clear_hdr_mem(0);
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
                                             
	printf("at blk after 2DFFT = %d\n", blkno);

	/* check dropout bit */
	    asp_read( (RLOC_RDFM+14)<<1, &mb.w[RLOC_RDFM+14], 2 );
	    if ((mb.w[RLOC_RDFM+14] & 0x100) == 0) {
		printf("dropout detected in 2DFFT\n");
		return(FAIL);
	    }

	/* save data captured in mb array to a file */
	    if ((file_mj = creat("avg_2dfft", 0666)) == FAIL) {
		printf("\n Can't open %s", "avg_2dfft");
		return(FAIL);
	    }
	    for (i = 0; i < 0xc000; i++) 
		data_2d[i] = mb.w[i+0x8000];
	    if (write(file_mj, data_2d, 98304) != 98304) {
		printf("Error in writing avg_2dfft\n");
		return(FAIL);
	    }
	    close(file_mj);
	    printf("avg_2dfft saved\n");

	}  /* complete 2DFFT */

    /* perform compress in both directions for data in mb array */
	rpt = ((ml_linelen + 7) / 8) >> 1; /* ml_linelen is 6144 */
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
		printf("warning: ZERO POWER in range spectrum!\n");
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
	/* Modify "if" to "while" CV 4/26/96 to fix 2dfft problem*/
	    while ((dop_f[l]-pp->fda) > sp->pre->prf/2.) {
		dop_f[l] -= sp->pre->prf;
	    	printf("Adjusted Doppler = %g\n", dop_f[l]);
	    }
	    while ((dop_f[l]-pp->fda) < -sp->pre->prf/2.) { 
		dop_f[l] += sp->pre->prf;
	    	printf("Adjusted Doppler = %g\n", dop_f[l]);
	    }
	/* Taking care of out of boundary slope jobs, MC 6/12/96 */
	    if (i>0) { 
	      if (pp->fdb > 0.3) { /* MC 3/19/97 */
		if ((dop_f[l] - dop_avg[i-1]) < 0.) 
		  dop_f[l] += sp->pre->prf;
	      }
	      else if (pp->fdb < -0.3) { /* MC 3/19/97 */		
                if ((dop_f[l] - dop_avg[i-1]) > 0.)
                  dop_f[l] -= sp->pre->prf;
              }
	      else { /* Do not trust predicted slope */
	        if ((dop_f[l] - dop_avg[i-1]) > sp->pre->prf/2.) {
		    dop_f[l] -= sp->pre->prf;
	    	    printf("Special adjusted Doppler = %g\n", dop_f[l]);
		}
		if ((dop_f[l] - dop_avg[i-1]) < -sp->pre->prf/2.) {
		    dop_f[l] += sp->pre->prf;
	    	    printf("Special adjusted Doppler = %g\n", dop_f[l]);
		}
	      }
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
	qua_fit(dop_avg,estat,3,tempfd);
	printf("Pass quadratic fit\n");
        /* CV for MC to fix the RSAT cross-track doppler not monotonic 5/21/97*/
        flag_linear = 0;
        if ((tempfd[0] * tempfd[1]) < 0.0 ){
            flag_linear = 1;
            linear_qr(dop_avg,estat,3,tempfd);
            printf("Pass linear fit\n");
        }
/* VUU 3/7/97 */
	if (pp->fda <0.0) {
          if (tempfd[0] > (pp->fda + 0.1 * sp->pre->prf))
		tempfd[0] -= sp->pre->prf;
	}
	else {
          if (tempfd[0] < (pp->fda - 0.1 * sp->pre->prf))
		tempfd[0] += sp->pre->prf;
	}
        /* CV for MC to fix the RSAT cross-track doppler not monotonic 5/21/97*/
        if (flag_linear == 1){
            pp->fda = tempfd[0];
            pp->fdb = tempfd[1];
            pp->fdc = 0.0;
        } else {
            pp->fda = tempfd[0];
            pp->fdb = tempfd[1];
            pp->fdc = tempfd[2];
        }
	if (vbose)
	printf("adjusted fd(a,b,c): %g %g %g\n",pp->fda,pp->fdb,
	    pp->fdc);

	return (PASS);
}


/* qua_fit(f_vec,e_vec,num_smpl,f_coff)  -----------------

Using 3 estimated Doppler values to calaulated 3 coefficients.

*/

qua_fit(f_vec,e_vec,num_smpl,f_coff)
        float *f_vec, *f_coff;
        int   *e_vec, num_smpl;

{
        double   a[3][3], b[3][3], del_a;

        if (num_smpl != 3) {
            printf("we only handle num_smpl = 3 situation\n");
            return(FAIL);
        }
        a[0][0] = 1.;
        a[0][1] = 1024.;
        a[0][2] = 1048576.;
        a[1][0] = 1.;
        a[1][1] = 3072.;
        a[1][2] = 9437184.;
        a[2][0] = 1.;
        a[2][1] = 5120.;
        a[2][2] = 26214400.;
        del_a = a[0][0]*a[1][1]*a[2][2]+a[0][2]*a[1][0]*a[2][1]
               +a[0][1]*a[1][2]*a[2][0]-a[0][2]*a[1][1]*a[2][0]
               -a[0][0]*a[1][2]*a[2][1]-a[0][1]*a[1][0]*a[2][2];
    /* B is inverse of A */
        b[0][0] = (a[1][1]*a[2][2]-a[1][2]*a[2][1])/del_a;
        b[1][0] = (a[1][2]*a[2][0]-a[1][0]*a[2][2])/del_a;
        b[2][0] = (a[1][0]*a[2][1]-a[1][1]*a[2][0])/del_a;
        b[0][1] = (a[0][2]*a[2][1]-a[0][1]*a[2][2])/del_a;
        b[1][1] = (a[0][0]*a[2][2]-a[0][2]*a[2][0])/del_a;
        b[2][1] = (a[0][1]*a[2][0]-a[0][0]*a[2][1])/del_a;
        b[0][2] = (a[0][1]*a[1][2]-a[0][2]*a[1][1])/del_a;
        b[1][2] = (a[0][2]*a[1][0]-a[0][0]*a[1][2])/del_a;
        b[2][2] = (a[0][0]*a[1][1]-a[0][1]*a[1][0])/del_a;

        f_coff[0] = b[0][0]*f_vec[0]+b[0][1]*f_vec[1]+b[0][2]*f_vec[2];
        f_coff[1] = b[1][0]*f_vec[0]+b[1][1]*f_vec[1]+b[1][2]*f_vec[2];
        f_coff[2] = b[2][0]*f_vec[0]+b[2][1]*f_vec[1]+b[2][2]*f_vec[2];
}


/* r1_adjust_fds(fdin,fdout) -------------------------------------------
        The least_sq routine returns a1,b1,c1 for 0 <= x <= 18,
        with x incrementing by 1.  We need to convert these to
        a,b,c for 128 <= x <= 4736, with x incrementing by 256.
        The formulae are:
	    a = a1 - (b1 / 2) + (c1 / 4)
	    b = (b1 - c1) / 256
	    c = c1 / 65536
*/

r1_adjust_fds(fdin,fdout)
	float fdin[3],fdout[3];
{
	    fdout[0] = fdin[0] - (fdin[1] / 2.0) + (fdin[2] / 4.0);
	    fdout[1] = (fdin[1] - fdin[2]) / 256.0;
	    fdout[2] = fdin[2] / 65536.0;
}

/* doppler_measure(sp,pp) -------------------------------------------
        This routine performs the Doppler measure functions of pre-
        processing, for the preprocessing region pointed
        to by pp, which is in the tape region pointed to by sp.
*/

doppler_measure(sp,pp)
        TAPE_SEG_PTR sp;
        PP_BLOCK_PTR pp;
{
        char filename[PATHLEN];
        short int rlut[4096], ilut[4096];
        int sync_code;
        char *strcpy();
        short int d1[8192];

        if (sw_ambig == 0 && sw_auto_focus == 0) {
            printf("Doppler measure bypassed\n");
            return (PASS);
        }

	clear_pipe();

     /* setup and process 2K lines thru to the corner turn memory */
        strcat(strcpy(filename,DEF_PATH),"params.dopms");
        if (read_proc_param_file(filename) == FAIL) {
            printf("...unable to read params.dopms file\n");
            return (FAIL); }
        in_norminv = pp->polarity;
        in_blkoff = pp->bit_off / 8 ;
        in_shift = pp->bit_off % 8;
        in_chirp = (sp->aux.beam_seq < 3) ? 821 : 575;
        rc_fscale = pp->fftsc_raf;
        rc_iscale = pp->fftsc_rai;
        if (p_load_regs() == FAIL) {
            printf("...register load failed\n");
            return (FAIL);
        }
/*
        p_make_lookup_tbl(pp,d1);
        setup_iif_memory(d1);
*/
        setup_ctc_cntrl(0,0x111,1,0,3,11,1,0);
        ex_set_selftest(0);
        ex_set_delay(8192 * 1024 - 16);
        sync_code = ~(FS | VF) & 0xff;
        ex_set_event(SYNC_EVENT,~sync_code,sync_code,1);
        ex_set_reset(0);
        play_dis(1);
        iif_set_reset(1);
        ctc_set_reset(1);
        iif_set_reset(0);
        ctc_set_reset(0);
        if (vbose) printf("\n...in Doppler measure before trigger\n");
    /* start the DCRS at the right location */
/*SONY*/
          if (!strcmp( media_type, "DCRSI" ) ){
            if (vbose)
              printf("starting DCRSi play at %d, offset %d, polarity %d\n",
                pp->blk_start,pp->bit_off,pp->polarity);
            start_fmt_play(2,pp->blk_start,pp->bit_off-32,pp->polarity);
         }
          else {
               if (vbose)
                 printf("starting SONY play at %d, offset %d, polarity %d\n",
                    pp->blk_start,pp->bit_off,pp->polarity);
               if (!strcmp( data_direction, "REVERSE" ) )

                sony_start_fmt_play_REV(2,pp->blk_start,pp->bit_off-184,pp->polarity);
               else

                sony_start_fmt_play(2,pp->blk_start,pp->bit_off-184,pp->polarity);
               }
                    
    /* arm and trigger */
        r1_clear_hdr_mem(0);
        tarm();
        reset_rpi(0);
        play_dis(0);
        tdone();
        play_dis(1);

    /* at this point, the data should be captured */
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
           sony_stop(IN);
        else
           dc_stop(IN);
                         
        printf("...%d formats locked into CTM\n",r1_find_last_hdr());
        if (vbose) printf("\n...in Doppler measure after trigger\n");

    /* perform PRF ambiguity determination */
        if (sw_ambig == 1){
        calc_prf(sp,pp);
        }

    /* perform auto-focus
        auto_focus(sp,pp);
        return (PASS);
    */
}


/* calc_prf(sp,pp) --------------------------------------------------
        This routine calculates the PRF ambiguity and adjusts the
        Doppler parameters accordingly.
*/

calc_prf(sp,pp)
        TAPE_SEG_PTR sp;
        PP_BLOCK_PTR pp;
{
        float L1[256][256], L2[256][256];
        float *l1, *l2;
        double d,lvd;
        int n = 1;

        printf("Calculating prf ambiguity...\n");
        if (sw_ambig == 0) {
            printf("...bypassed\n");
            return;
        }
    /* calculate the spacing in pixels between prf's */
        lvd = spf.xlambda / (2.0 * pp->vrel);
        lvd *= lvd;
        spf.pbw = 8.0 * spf.srspace / (3.0 * pp->r_mid * sp->pre->prf *
lvd);
        d = 3.0 * pp->r_mid * spf.pbw/4.0 * sp->pre->prf * lvd;
        d = d / spf.srspace;
        if (vbose){
        printf(" ...... lvd = %g\n", lvd);
        printf(" ...... r_mid = %g\n", pp->r_mid);
        printf(" ...... pbw_ambiquity = %g\n", spf.pbw/4.0 );
        printf(" ...... prf = %g\n", sp->pre->prf);
        printf(" ...... srspace = %g\n", spf.srspace);
        printf(" ...... d = %g\n", d);
        }
    /* top of PRF ambiguity determination loop */
        while (n) {
        /* set up processor and process one frame */
            if (setup_ap(sp,pp,4,OFF,ON,OFF,0,0,0) != PASS)
                return (FAIL);
            ml_froff = 1024;
            ml_lkoff = 256;
            ml_frdelay = 2;
            ml_segsize = 0;
            p_load_regs();
            mlc_set_reset(1);
            avg_set_reset(1);
        /* activate MLC test tap */
            asp_read( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
            mb.w[RLOC_MLC] &= ~0x1000;
            asp_write( RLOC_MLC<<1, &mb.w[RLOC_MLC], 2 );
        /* fix CTC mode bits, etc. */
            asp_read( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
      	    mb.w[RLOC_CTC] |= 0x0020;         /* turn test bit off */
      	    mb.w[RLOC_CTC] &= ~0x8000;        /* turn ALLREQ off */
            asp_write( RLOC_CTC<<1, &mb.w[RLOC_CTC], 2 );
            asp_read((RLOC_CTC+7)<<1, &mb.w[RLOC_CTC+7], 2 );
      	    mb.w[RLOC_CTC+7] |= 0x2000;       /* turn CONTOUT on */
            asp_write((RLOC_CTC+7)<<1, &mb.w[RLOC_CTC+7], 2 );

            if (vbose) {
                printf("\n...in calc_prf before trigger\n");
            }
            avg_set_reset(0);
            mlc_set_reset(0);
            rm_ovf_reset();

            tarm();
            tdone();
        /* retrieve data from captured image in response buffer */
            get_resp(2432,0,256,256,L1);
            if (vbose) {
                printf("\n...in calc_prf after trigger\n");
                printf("  ...correlation array 1 at 0x10000\n");
            }
            get_resp(2432,768,256,256,L2);
            if (vbose) printf("  ...correlation array 2 at 0x10000\n");
        /* find cross-correlation peak position between the two sets */
            n = xcor_rng(L1,L2,d);
            printf("...ambiguity = %d\n",n);
            printf("...fda adjustment = %g\n",n * sp->pre->prf);
        /* adjust Doppler params, if needed */
            pp->fda += n * sp->pre->prf;
        }  /* while n */
}


/* get_resp(x,y,nx,ny,Data) ----------------------------------------
        This routine retrieves a rectangular section of data from the
        response buffer, and loads it into the floating point array
        Data.
*/

get_resp(x,y,nx,ny,Data)
        int x,y,nx,ny;
        float *Data;
{
        int i, rgo, linelen;
        int maddr = 0x10000;
        unsigned short int *m, *mend;
        float *d;

        d = Data;
        linelen = 4 * nx;
        for (i = y; i < y + ny; i++) {
            rgo = i * 4864 * 4 + x * 4;
            mr (rgo, rgo + linelen, maddr, maddr + linelen);
            m = &mb.w[maddr >> 1];
            mend = m + nx * 2;
            while (m < mend) {
                *d++ = (float) ((*m++) & 0xffff);
                m++;
            }
            maddr += linelen;
        }
}


/* xcor_rng(L1,L2,d) ------------------------------------------------
        This routine performs a cross-correlation between the two
        arrays L1 and L2, and returns the peak location as a signed
        integer offset of L1 with respect to L2.  d is a float giving
        the unit size of the offset in pixels.  Thus the returned
        offset is the actual offset in pixels divided by d, and then
        rounded to the nearest integer.
*/

xcor_rng(L1,L2,d)
        float *L1,*L2,d;
{
        float peak,cor;
        int i,n,ans;
        float *l1,*l2,*l1end,*l2end;

        l2end = L2 + (256 * 256);
        peak = 0.0;
    /* perform lags for offsets -2, -1, 0, +1, +2 */
        for (n = -2; n < 3; n++) {
        /* calculate pixel location of this offset */
            i = (abs(n) * d) + 0.5;
            if (n > 0)
                i = -i;
            cor = 0.0;
        /* perform one lag */
            for (l1 = (L1 + 4 + i), l2 = (L2 + 4); l2 < l2end;
                        l1 += 8, l2 += 8) {
                l1end = l1 + 248;
                while (l1 < l1end)
                    cor += (*l1++) * (*l2++);
            }  /* for l1 */
        /* see if this is a new maximum */
            if (vbose)
                printf("... lag %d: pixel off: %d energy: %g\n",n,i,cor);
            if (cor > peak) {
                peak = cor;
                ans = n;
            }  /* if cor */
        }  /* for n */
        return (ans);
}


/* auto_focus(sp,pp) ------------------------------------------------
        This routine performs the auto-focus function on ERS-1 data,
        to check and possibly correct the fm rate Doppler parameter.
*/

auto_focus(sp,pp)
        TAPE_SEG_PTR sp;
        PP_BLOCK_PTR pp;
{
        float L1[256][256], L2[256][256];
        float *l1, *l2;
        float adj,fr,ntd;
        int n, n2;
        int start = 2560;
        int try = 0;

        printf("Performing auto-focus\n");
        if (vbose)
             printf(" ...... fdota,fdotb,fdotc = %g, %g, %g\n",
                 pp->fdota,pp->fdotb,pp->fdotc);
        if (sw_auto_focus == 0) {
            printf("...bypassed\n");
            return (PASS);
        }
        n = 1;
        while (n) {
            if (vbose)
                printf("...iteration %d\n",++try);
        /* compute the adjustment factor */
            fr = pp->fdota
              + (pp->fdotb * start)
              + (pp->fdotc * start * start);
            ntd = (sp->pre->prf * spf.pbw/ 4.) / fr;
            adj = 4.0 / (3.0 * ntd);
            if (vbose){
                printf(" ...... fr = %g\n", fr);
                printf(" ...... prf = %g\n", sp->pre->prf);
                printf(" ...... pbw_autofocus = %g\n", spf.pbw/4.0);
                printf(" ...... ntd = %g\n", ntd);
                printf(" ...... adj = %g\n", adj);
                }
        /* set up processor and process one frame */
            if (setup_ap(sp,pp,4,OFF,ON,OFF,0,0,0) != PASS)
                return (FAIL);
            ml_froff = 1024;
            ml_lkoff = 256;
            ml_frdelay = 2;
            ml_segsize = 0;
            p_load_regs();
            mb.w[RLOC_MLC] &= ~0x1000;  /* set MLC test tap on */
            mlc_set_reset(1);

            if (vbose) {
                printf("\n...in auto-focus before trigger\n");
/*              if (abob()) exit(1); */
                }
            mlc_set_reset(0);
            rm_ovf_reset();
            tarm();
            tdone();

            if (vbose) printf("\n...in auto-focus after trigger\n");

        /* retrieve data from captured image in response buffer */
            get_resp(start-128,0,256,256,L1);
            get_resp(start-128,768,256,256,L2);
        /* find cross-correlation peak position between the two sets */
            n = xcor_az(L1,L2);
            if (n == 0) {
                printf("...no auto-focus adjustment needed");
                if (try > 1)
                    printf(" this iteration");
                printf("\n");
                return(PASS);
            }

        /* an offset was found -- check a second set */
        /* retrieve data from captured image */
            get_resp(start+128,0,256,256,L1);
            get_resp(start+128,768,256,256,L2);
        /* find cross-correlation peak position between the two sets */
            n2 = xcor_az(L1,L2);
            if (n2 != n) {
                printf("...auto-focus: 2 areas give differing results:\n");
                printf("n = %d, n2 = %d\n",n,n2);
                printf("...adjustment will not be made\n");
                return(PASS);
            }
        /* adjust Doppler params, if needed */
            printf("Operator NOTE: fm rate adjusted by auto-focus\n");
            printf("...old fdota = %g,",pp->fdota);
            printf(" new = %g, n = %d\n",
                pp->fdota += n * pp->fdota * adj,n);
        }  /* while n */
        return (PASS);
}


/* xcor_az(L1,L2) ---------------------------------------------------
        This routine performs a cross-correlation in azumuth between the
       two arrays L1 and L2, and returns the peak location as a signed
        integer offset of L1 with respect to L2.  The returned offset
        is the actual offset in pixels.
*/

xcor_az(L1,L2)
        float *L1,*L2;
{
        float peak,cor;
        int n,ans;
        float *l1,*l2,*l1end,*l2end;

        l2end = L2 + (256 * 256);
        peak = 0.0;
    /* perform lags for offsets -2, -1, 0, +1, +2 */
        for (n = -2; n < 3; n++) {
        /* calculate line location of this offset */
            l1 = L1 + (256 * (4 + n));
            l1end = l1 + (248 * 256);
            cor = 0.0;
        /* perform one lag */
            l2 = L2 + (4 * 256);
            while (l1 < l1end)
                cor += (*l1++) * (*l2++);
        /* see if this is a new maximum */
            if (cor > peak) {
                peak = cor;
                ans = n;
            }  /* if cor */
        if (vbose)
            printf(" ...... n = %d cor = %g\n", n, cor);
        }  /* for n */
        return (ans);
}


/* r1_clear_hdr_mem(mode) ----------------------------------------------
	This routine clears the IIF header memory to 0xcad107ad, to permit
	the r1_find_last_hdr routine to locate the last recorded header.
	mode = 1 means short header mode, = 0 means normal mode.
*/

r1_clear_hdr_mem(mode)
	int mode;
{
	int i, j, igo, iend, pid_save, hdr_len, mloc;
	int f_dropout;

	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HEAD;
	hdr_len = (mode) ? HDR_LEN2 : HDR_LEN;
	igo = MLOC_IIF_HEAD >> 1;
	/*igo = (MLOC_IIF_HEAD + 2) >> 1;*/
	mloc = MEM_IIF_HEAD;
	iend = igo + (0x80000 >> 1);
	asp_read( mloc, &mb.w[MLOC_IIF_HEAD>>1], 0x80000 );
	if (mode == 1) {
	     j = 0;
	     for (i = igo; i < iend; i += hdr_len){
	     	if ((j%2) == 1) i += 1; 
	     	mb.w[i] = 0x5555;
	     	mb.w[i+1] = 0x5555;
	        j+=1;
	     }
	} else {  	 
	     for (i = igo; i < iend; i += hdr_len)
	     	mb.w[i] = 0x5555;
	}
	asp_write( mloc, &mb.w[MLOC_IIF_HEAD>>1], 0x80000 );
	mb.w[RLOC_REP] = pid_save;
}

/* r1_find_last_hdr() --------------------------------------------------
	This routine finds the last recorded header in header memory.
	Call r1_clear_hdr_mem before beginning to record, and wait
	until processing is complete before calling this routine.
	The routine returns the header number relative to the start
	of header memory, or FAIL if none is found.
*/

r1_find_last_hdr()
{
	int i, j,igo, iend, pid_save, hdr_mode, hdr_len, code;
	int misses,isave,temp;
	int fsave,flast, f_dropout;
	int maxmiss = 6;
	int value, value1, sync_pos;

	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HEAD;
	get_iif_hdr_mode(&hdr_mode);
	hdr_len = (hdr_mode) ? HDR_LEN2 : HDR_LEN;
	igo = MLOC_IIF_HEAD >> 1;
	/*igo = (MLOC_IIF_HEAD + 2) >> 1;*/
	iend = igo + (0x80000 >> 1);
	misses = 0;
	j = 0;
	asp_read( MEM_IIF_HEAD, &mb.w[MLOC_IIF_HEAD>>1], 0x80000 );
	for (i = igo; i < iend; i += hdr_len) {
	    j +=1;
	    if (i == igo) 
	       code = ((mb.w[i] & 0xffff) << 16) | (mb.w[i+1] & 0xffff);
	    else {
	     sync_pos = mb.w[i] & 0x00ff;
	     if (sync_pos == 0x0035) {
	       value= ((mb.w[i] << 8) & 0xff00) | ((mb.w[i+1] >> 8) & 0x00ff); 
	       value1= ((mb.w[i+1] << 8) & 0xff00) | 
			((mb.w[i+2] >> 8) & 0x00ff); 
	       code = ((value & 0xffff) << 16) | (value1 & 0xffff); 
		/*printf("%d,code=%x, ", j,code);*/
	     } else {
	       i = i+1;
	       code = ((mb.w[i] & 0xffff) << 16) | (mb.w[i+1] & 0xffff);
		/*printf("%d, code=%x, ", j,code);*/
	     }	       
	    }
	    if (bit_dif(code,AUX_SYNC_CODE,2) > 2) {
		if (misses == 0) {
		    isave = i;
		    if (vbose) printf("isave=%d\n",isave);
		}
		if (++misses > maxmiss) {
		    i = isave;
		    if (vbose) printf("misses=%d,isave=%d\n",misses,isave);
		    break;
		}
	    }
	}
	mb.w[RLOC_REP] = pid_save;
	if (i > igo) {
	    temp = (((i - igo) / 23) *2) - 1;
	    return ((((i - igo) / 23) *2) - 1);
	}
	return (FAIL);
}


/* r1_get_aux_data(fmt,ap) ---------------------------------------------

	This routine extracts the auxiliary data from 
	data from one header captured in the IIF header memory.
	The IIF captures header data for each processed format.
	'fmt' gives the relative format number (starting from 0) of 
	the format whose header data is desired.  'ap' is a pointer
	to a structure to be loaded with the extracted data.
	If the aux data format code is valid, the routine returns 
	PASS, otherwise FAIL.
*/

r1_get_aux_data(fmt,ap)
	int fmt;
	RSAT_AUX_PTR ap;
{
	int i, j,value, pid_save, fcode, hdr_mode, hdr_len, mloc;
	int val_temp[4];
	double val1_temp[4];
	int temp_b1, temp_b2, ttemp, add_i;
	int sync_pos, value1, value2;
	double cal_temp();

	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HEAD;
	get_iif_hdr_mode(&hdr_mode);
	hdr_len = (hdr_mode) ? HDR_LEN2 : HDR_LEN;
	if ((fmt > 0 ) && (hdr_mode == 1)) add_i = fmt / 2;
	else add_i = 0;
	i = (fmt * hdr_len) + add_i + (MLOC_IIF_HEAD >> 1);
/*
printf("fmt %d hdr_len %d add_i %d\n", fmt,hdr_len,add_i);
*/
	mloc = fmt*hdr_len*2 + add_i*2 + MEM_IIF_HEAD;
	asp_read( mloc, &mb.w[i], 6 );
/*
printf("%x %x %x\n",mb.w[i],mb.w[i+1],mb.w[i+2]);
*/
	if (hdr_mode == 1) {
	   sync_pos = mb.w[i] & 0x00ff;
	   if (sync_pos == 0x0035) {
	      value2= ((mb.w[i] << 8) & 0xff00) | ((mb.w[i+1] >> 8) & 0x00ff); 
	      value1= ((mb.w[i+1] << 8) & 0xff00) | 
			((mb.w[i+2] >> 8) & 0x00ff); 
	      fcode = ((value2 & 0xffff) << 16) | (value1 & 0xffff); 
	   } else {
	      fcode = ((mb.w[i] & 0xffff) << 16) | (mb.w[i+1] & 0xffff);
	   }
	} else 
	   fcode = ((mb.w[i] & 0xffff) << 16) | (mb.w[i+1] & 0xffff);

	i += AUX_LOC;
	mloc += AUX_LOC<<1;
	asp_read( mloc, &mb.w[i], 50 );
	if (bit_dif(fcode,AUX_SYNC_CODE,2) > 2){
	    printf("fcode (FAIL) = %x\n", fcode);
	    return (FAIL);
	}
	ap->sync = fcode;

	if (hdr_mode == 1) {
	     if (sync_pos == 0x0035){ 
/*                if (vbose) printf("sync_pos=0035, ");*/
		ap->rep_AGC = (mb.w[i+2] >> 2) & 0x003f;
		ap->CALN_att = (mb.w[i+3] >> 8) & 0x00ff;
		ap->waveform = mb.w[i+3] & 0x00f0;
		ap->beam_seq = mb.w[i+4] & 0x00ff; 
		ap->ADC_samp = (mb.w[i+5] >> 12) & 0x0003;
		ap->PRF =(((mb.w[i+5] << 5) & 0x1fe0) | 
			((mb.w[i+6] >> 11) & 0x001f));
		ap->window_start = (((mb.w[i+6] << 4) & 0x0ff0) |
			((mb.w[i+7] >> 12) & 0x000f));
		ap->win_duration = (((mb.w[i+7] << 4) & 0x0ff0) |
			((mb.w[i+8] >> 12) & 0x000f));
		temp_b1 = ((mb.w[i+9] << 8) & 0xff00) |
			  ((mb.w[i+10] >> 8) & 0x00ff);
	 	temp_b2 = ((mb.w[i+10] << 8) & 0xff00) |
			  ((mb.w[i+11] >> 8) & 0x00ff); 
		ap->time_all = (temp_b1 << 16) | temp_b2; 
		ap->time.days = ((mb.w[i+8] << 3) & 0x07f8) |
				((mb.w[i+9] >> 13) & 0x0007);
/*if(vbose)printf("time.days=%x,mb.w[%d+8]=%x,mb.w[%d+9]=%x\n,",
                ap->time.days,i,mb.w[i+8],i,mb.w[i+9]);*/
	 	temp_b1 = mb.w[i+9] & 0x1fff;
		temp_b2 = mb.w[i+10] & 0xf000;
		ttemp = (temp_b1 << 16) | temp_b2;
		ap->time.seconds = (ttemp >> 12) & 0x0001ffff; 
		ap->time.msecs = (mb.w[i+10] >> 2) & 0x03ff;
		ap->time.microsecs = ((mb.w[i+10] << 8) & 0x0300) |
			     ((mb.w[i+11] >> 8) & 0x00ff);
		ap->AGC_set = mb.w[i+11] & 0x003f;

	    } else {
/*                if (vbose)printf("sync_pos=35, ");*/
		ap->rep_AGC = (mb.w[i+2] >> 10) & 0x003f;
		ap->CALN_att = mb.w[i+2] & 0x00ff;
		ap->waveform = (mb.w[i+3] >> 12) & 0x000f;
		ap->beam_seq = ((mb.w[i+3] << 8) & 0xff00) | 
				((mb.w[i+4] >> 8) & 0x00ff);
		ap->ADC_samp = mb.w[i+4] & 0x0030;
		ap->PRF = (mb.w[i+5] >> 3) & 0x1fff;  
		ap->window_start = (mb.w[i+6] >> 4) & 0x0fff; 
		ap->win_duration = (mb.w[i+7] >> 4) & 0x0fff; 
		ap->time_all =  ((mb.w[i+9] & 0xffff) << 16) |
			(mb.w[i+10] & 0xffff); 
		ap->time.days = (mb.w[i+8] >> 5) & 0x07ff; 
/* if(vbose)printf("time.days=%x,mb.w[%d+8]=%x\n,",
                ap->time.days,i,mb.w[i+8]); */
		temp_b1 = mb.w[i+8] & 0x001f; 
		temp_b2 = mb.w[i+9] & 0xfff0;
		ttemp = (temp_b1 << 16) | temp_b2;
		ap->time.seconds = (ttemp >> 4) & 0x0001ffff;

		ap->time.msecs = ((mb.w[i+9] & 0x000f) << 6) | 
				((mb.w[i+10] >> 10) & 0x003f);
		ap->time.microsecs = mb.w[i+10] & 0x03ff; 
		ap->AGC_set = (mb.w[i+11] >> 8) & 0x003f;
	   }
	} else {
/*                if (vbose)printf("hdr_mode=0, "); */
		ap->img_ref_id = ((mb.w[i+2] & 0xffff) << 16) | 
				(mb.w[i+3] & 0xffff);

/* state the status of payload */ 
		value = mb.w[i+4] & 0xffff;
		payload_status(ap,value);

		ap->rep_AGC = (mb.w[i+5] >> 10) & 0x003f;
		ap->CALN_att = mb.w[i+5] & 0x00ff;
		ap->waveform = (mb.w[i+6] >> 12) & 0x000f;

/* calculate the temperature using the equation in Speci */
		val_temp[0] = (mb.w[i+7] >> 8) & 0x00ff;
		val_temp[1] = mb.w[i+7] & 0x00ff;
		val_temp[2] = (mb.w[i+8] >> 8) & 0x00ff;
		val_temp[3] = mb.w[i+8] & 0x00ff;
		for (j=0; j<4; j++){
			val1_temp[j] = cal_temp(val_temp[j]);
			/*printf("val1_temp[%d] = %8.3f\n", j, val1_temp[j]);*/
		}
      
		ap->temp.rec_lna_temp = val1_temp[0];
		ap->temp.rec_sub_temp = val1_temp[1];
		ap->temp.rec_proc_temp = val1_temp[2];
		ap->temp.cal_sub_temp = val1_temp[3];

		ap->beam_seq = mb.w[i+9] & 0xffff;
		ap->ephemeris = mb.w[i+10] & 0xffff;
		ap->no_beams = (mb.w[i+11] >> 14) & 0x0003;
		ap->ADC_samp = (mb.w[i+11] >> 12) & 0x0003;
		ap->pulse1 = mb.w[i+11] &  0x00ff;
		ap->pulse2 = (mb.w[i+12] >> 8) & 0x00ff;
		ap->PRF =(((mb.w[i+12] << 5) & 0x1fe0) | 
			((mb.w[i+13] >> 11) & 0x001f));
		ap->beam_sel = (mb.w[i+13] >> 9 ) & 0x0003;
		ap->window_start = (((mb.w[i+13] << 4) & 0x0ff0) |
			((mb.w[i+14] >> 12) & 0x000f));
		ap->win_duration = (((mb.w[i+14] << 4) & 0x0ff0) |
			((mb.w[i+15] >> 12) & 0x000f));
		temp_b1 = ((mb.w[i+22] << 8) & 0xff00) |
			  ((mb.w[i+23] >> 8) & 0x00ff);
	 	temp_b2 = ((mb.w[i+23] << 8) & 0xff00) |
			  ((mb.w[i+24] >> 8) & 0x00ff); 
		ap->time_all = ((temp_b1 & 0xffff) << 16) | (temp_b2 & 0xffff);
                if (vbose) printf("time_all = %x\n", ap->time_all);
		ap->time.days = ((mb.w[i+21] << 3) & 0x07f8) |
				((mb.w[i+22] >> 13) & 0x0007);
/* if(vbose)printf("time.days=%x,mb.w[%d+21]=%x,mb.w[%d+21]=%x\n",
                ap->time.days,i,mb.w[i+21],i,mb.w[i+22]); */
	 	temp_b1 = mb.w[i+22] & 0x1fff;
		temp_b2 = mb.w[i+23] & 0xf000;
		ttemp = ((temp_b1 << 16) | temp_b2);
		ap->time.seconds = (ttemp >> 12) & 0x0001ffff; 
		ap->time.msecs = (mb.w[i+23] >> 2) & 0x03ff;
		ap->time.microsecs = ((mb.w[i+23] << 8) & 0x0300) |
			     ((mb.w[i+24] >> 8) & 0x00ff);
		ap->rep_pre = (mb.w[i+24] >> 6) & 0x0001;
		ap->AGC_set = mb.w[i+24] & 0x003f;
		
/*		printf("ap->beam_sel= %d = %x\n", ap->beam_sel, ap->beam_sel);
		printf("ap->temp.rec_lna_temp= %8.4f\n", ap->temp.rec_lna_temp);
		printf("ap->temp.rec_sub_temp= %8.4f\n", ap->temp.rec_sub_temp);
		printf("ap->temp.rec_proc_temp= %8.4f\n", ap->temp.rec_proc_temp);
		printf("ap->temp.cal_sub_temp= %8.4f\n", ap->temp.cal_sub_temp);
		printf("ap->no_beams= %d\n", ap->no_beams);*/
	}

	mb.w[RLOC_REP] = pid_save;

	/*printf("ap->sync= %x\n", ap->sync);
	printf("ap->rep_AGC= %d = %x\n", ap->rep_AGC, ap->rep_AGC);
	printf("ap->CALN_att= %d = %x\n", ap->CALN_att, ap->CALN_att);
	printf("ap->pulse_wave= %d = %x\n", ap->waveform, ap->waveform);
	printf("ap->beam_seq= %d = %x\n", ap->beam_seq, ap->beam_seq);
	printf("ap->ADC_samp= %d = %x\n", ap->ADC_samp, ap->ADC_samp);
	printf("ap->PRF= %d = %x\n", ap->PRF, ap->PRF);
	printf("ap->window_start= %d = %x\n", ap->window_start, ap->window_start);
	printf("ap->win_duration= %d = %x\n", ap->win_duration, ap->win_duration);
	printf("ap->time_all= %d = %x\n", ap->time_all, ap->time_all);
	printf("ap->time.days= %d\n", ap->time.days);
	printf("ap->time.seconds= %d\n", ap->time.seconds);
	printf("ap->time.msecs= %d\n", ap->time.msecs);
	printf("ap->time.microsecs= %d\n", ap->time.microsecs);
	printf("ap->AGC_set= %d\n", ap->AGC_set);*/

	return (PASS);
}


/* r1_get_hwdata(sp,pp) ------------------------------------------------
	This routine retrieves the bit error rate and input data
	histograms from the hardware, saves them for later, and does
	some quality checking on them.  If the quality checks pass, the
	routine returns PASS, otherwise, FAIL.
*/

r1_get_hwdata(sp,pp)
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
/* ??? TBD - Ming will fix this
	asp_read( (RLOC_EDFM+4)<<1, &mb.w[RLOC_EDFM+4], 2 );
	errcount = mb.w[RLOC_EDFM + 4];
	fmt_ratio = 1767.9 / sp->pre->prf;
	bits = 29.0 * 24.0 * 5432.0 * fmt_ratio;
	pp->ber = errcount / bits;
	printf("...bit error rate = %g\n",pp->ber);
*/
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
	/*	if (abob()) exit(1); */
	}
	mb.w[RLOC_REP] = pid_save;
    /* make quality measurements */
/* Ming 1/30/96 temporarily comments out  2/14/96
	rawchk_(pp->ihist_cor,&count,&itotal,
		&pp->imean,&pp->istdv,&hilev,&lolev,&normal,xfit);
	rawchk_(pp->qhist_cor,&count,&qtotal,
		&pp->qmean,&pp->qstdv,&hilev,&lolev,&normal,xfit);
*/
	if (vbose)
	    printf("...done with get_hwdata\n");
	return (PASS);
}


/* r1_repmeas(sp,pp) ---------------------------------------------------
	This routine measures the characteristics of the first 8
	replicas in the preprocessing region pointed to by pp, which
	is in the tape segment pointed to by sp.
*/

r1_repmeas(sp,pp)
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

    /* read replica data into array replica */

if ( abob() ) exit(1);	
	asp_read( MEM_IIF_HEAD, &mb.w[MLOC_IIF_HEAD>>1], 0x80000 );
	if ( r1_get_aux_data(0,&sp->aux) == FAIL ) return(FAIL);
	if ( r1_get_replica(replica) == FAIL ) return(FAIL);
	if (vbose) {
	   if (write(file_mj, replica, dpf.nchirp) != dpf.nchirp) {
		printf( "Error in writing replica file" );
		return(FAIL);
	   }
	   close(file_mj);
	}

	len = dpf.nchirp;

	/* debugging stuff: copy replica to 0x10000 */
        
	    for (i = 0; i < 2*len; i++)
		mb.w[0x8000 + i] = replica[i];
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


/* r1_get_replica(data) ---------------------------------
	This routine extracts one chirp replica from IIF memory.
	It first skips 50 bytes of aux data then read dpf.nchirp bytes
	of data. It return data buffer.
*/

r1_get_replica(data)
	unsigned char *data;
{
	int i = (MLOC_IIF_HEAD>>1), j = 0;

	while ( ( mb.w[i] != 0x352E ) && (j < AUX_LEN) ){
		if (vbose) printf("mb.w[%X] %X\n",i,mb.w[i]);
		i++, j++;
	}
	if ( mb.w[i] != 0x352E )
		printf("r1_get_replica: Unable to search aux sync\n");
	else printf("r1_get_replica: found it\n");
	memcpy( data, &mb.w[i], dpf.nchirp );
	return (PASS);
}
/* r1_build_image_files(sp,pp,seg,region) ------------------------------
	This routine creates the image directory and processing files
	for one image, centered on preprocessing region pp.
*/

r1_build_image_files(sp,pp,seg,region)
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
	int ngains = 0;
	double deltat = 0.1;
	float dtheta = 0.1;
	int ifirst;
	int images = 0;
	float r_e,f,sclmcv,r_cls_1,r_cls_2,x[16384];
	int npts,istat,l1,l2,l3,l4;
	short int d1[16384];
        float gamma;
	int roll_tilt;
	int i,cnt;

/* For RSAT, ASP read the the CPF to get the CAL_STATUS, CAL_COMMENT,
etc..., but ignore the gain_vec. If CPF is ready to use with correct
info of gain_vec & other parameters, the read the CPF file only*/ 

/* delete the comment on 3/13/97 */

	if (!cp_flag) { 
            sprintf(filename,"%sgaintbl.%d",DEF_PATH,sp->aux.beam_seq);
            ngains = GetCalFile( filename );
	    if (ngains == FAIL ){
                printf("r1_build_image_files: Can't find %s (use default\n",
		     filename);
	    }
  	}
	else {
            ngains = AspReadCalFile(); 
            if (ngains == NULL) {
                printf("Unable to read cal_file, ngains=%d \n", ngains);
                return(FAIL); 
            }
            if (ngains == FAIL){
                printf("Error in reading cal_file, ngains=%d \n", ngains);
                return(FAIL); 
            }
/*    	    if (sp->aux.beam_seq != 2) {  apply 1.0 gaintbl 3/26/97 
        	sprintf(filename,"%sgaintbl.%d",DEF_PATH,sp->aux.beam_seq);
        	ngains = GetCalFile( filename );
        	if (ngains == FAIL) {
                   printf("Error in reading cal_file,ngains=%d \n", ngains);
		   return(FAIL);
		} 
	    } comment out CV 5/16/97 */
	}
/* comment out on 3/7/96 
        ngains = AspReadCalFile(); 
        sprintf(filename,"%sgaintbl.%d",DEF_PATH,sp->aux.beam_seq);
        ngains = GetCalFile( filename );
        if (ngains == FAIL)
                printf("Error in reading cal_file, ngains=%d \n", ngains);
*/
	ifirst = (int)((ngains/2) - ngains);
	if (vbose) printf("ifirst=%d,ngains=%d\n",ifirst,ngains);
/*	for (i=0;i<ngains;i++) printf("%d=%11.7f\n",i, gaintbl[i]); */

	strcpy(image_id,pp->img_name); /* for scene_file */

    /* initialize the processing parameters */
	sprintf(filename,"%s%s",DEF_PATH,pname);
	read_proc_param_file(filename);
    /* Overwrite line length parameters based on beam no., MC 6-26-96 */
	if (!strcmp(Cur_Rqst->type,"RPR")) {
	    in_chirp = (sp->aux.beam_seq < 3) ? 821 : 575;
	    in_linelen = (sp->aux.beam_seq < 3) ? 7036 : 7336;
	    pr_valpoints = in_outlen - in_chirp;
	    pr_rglen = in_linelen - in_chirp;
	    pr_segments = (pr_rglen - 1) / pr_valpoints + 1;
	}

    /* add in fft scaling and detector shift values from pass 2 & 3 */  
	rc_fscale = pp->fftsc_raf;
	rc_iscale = pp->fftsc_rai;
	af_scale = pp->fftsc_az4f;
	ap_scale = pp->fftsc_az4i;
	ap_detshift = pp->det_sc;

	if (strcmp(Cur_Rqst->type, "CPX") == 0) 
		ap_detshift = -1;

    /* calculate start format and # of frames */
	minfmt = sp->fmt_start;
	maxfmt = sp->fmt_end;

/*	minfmt = sp->fmt_start + 3;
	if (minfmt < 11)
	    minfmt = 11;
	if (sp->gap_type == 2)  add by CV 
	    maxfmt = sp->end_id;
	if (sp->post == NULL || sp->gap_type == 2)
	    maxfmt = sp->end_id;
	else
	    maxfmt = sp->post->fmt_start - 8;  comment out by CV */


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
	stfmt = pp->fmt_start - (time * prf + 0.5);
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
	dsec = (pp->fmt_start - stfmt) * pri;
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
	get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,deskew,0.0,
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
	stfmt = pp->fmt_start - (time * prf + 0.5);
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

    /* Simulate Tom's fudge factor in j1_ppr for location skew, 5/13/96 
        skew_adj += 1000.; */

    /* calculate and write the azimuth cage setups */
    /* Add slnt_rg, however need mod in setup_ap to get slant image 6-5-96 */
	if ((Cur_Rqst->gnd_slnt_rg[0] == 'S') && looks != 1)
 	    setup_ap(sp,pp,4,ON,ON,OFF,0,0,0);
	else
 	    setup_ap(sp,pp,looks,ON,OFF,deskew,start_bin,rads,skew_adj);

    /* remove fudge factor for location skew, 5/13/96 
        skew_adj -= 1000.; */

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
/* comment out by clw 8/25/95
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
	    printf("stfmt=%d,endfmt=%d,nfmts=%d,minfmt=%d\n",
			stfmt,endfmt,nfmts,minfmt);
	}
    /* if not enough formats, refuse to make an image */
	printf("stfmt=%d,endfmt=%d,nfmts=%d,minfmt=%d\n",
			stfmt,endfmt,nfmts,minfmt);
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
	sclmcv = 2.9E13;   /* RSAT specific scale factor TBF */
	rdmtric_(&istat,
		 &svec[0],&svec[3],&pp->att,&gamma,
		 &range,&spf.srspace,&pp->fdota,
		 gaintbl,&ngains,&ifirst,&dtheta,&r_e,&f,&npts,
		 d1,&sclmcv,x);
	if (vbose) {
	    printf("istat=%d, npts=%d\n", istat, npts);
	    printf("scaling factor = %g\n", sclmcv);
	}
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

    /* add fudge factor equal to ??? x frame size */	
	if (sp->aux.beam_seq == 2)
   	    dsec = (pp->fmt_start - stfmt - pulse_delay + .94*in_vlines) * pri;
	else if (sp->aux.beam_seq == 6)
   	    dsec = (pp->fmt_start - stfmt - pulse_delay + .73*in_vlines) * pri;
	else if (sp->aux.beam_seq == 3)
   	    dsec = (pp->fmt_start - stfmt - pulse_delay + .86*in_vlines) * pri;
	else if (sp->aux.beam_seq == 4)
   	    dsec = (pp->fmt_start - stfmt - pulse_delay + .87*in_vlines) * pri;
	else if (sp->aux.beam_seq == 7)
   	    dsec = (pp->fmt_start - stfmt - pulse_delay + .76*in_vlines) * pri;
	else if (sp->aux.beam_seq == 5)
   	    dsec = (pp->fmt_start - stfmt - pulse_delay + .864*in_vlines) * pri;
	else if (sp->aux.beam_seq == 1)
   	    dsec = (pp->fmt_start - stfmt - pulse_delay + .96*in_vlines) * pri;
	else 
    	    dsec = (pp->fmt_start - stfmt - pulse_delay)  * pri; 
	start_gmt = pp->sv.gmt;
	add_seconds(&start_gmt,-dsec);

    /* add the delay time due to zero doppler line */
	if (fd3p[0] < 0.0)  /* VUU for MC 3/5/97 */
	     dsec = - ((double)fd3p[2])/fabs((double)fr3p[2]);
	else
	     dsec = - ((double)fd3p[0])/fabs((double)fr3p[0]);
/*	dsec = - ((double)fd3p[0])/fabs((double)fr3p[0]); old */
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

	}	        
	sf.lat_scene_ctr = c[0];
	sf.lon_scene_ctr = c[1];
	pp->lat_rough = c[0];
	pp->lon_rough = c[1];

	r_cls_1 = r_cls_2 = 0.;
	/*
	f_size = in_vlines * (in_scenelen-2);
	*/
	f_size = 28000;
	if (strcmp(Cur_Rqst->type,"CPX") == 0) 
	    f_size = 15360;
	if (region == 1) wdchk = 1;

	/* Start from fmt contains replica 4/2/96 */  
	stfmt = sp->fmt_start + ((stfmt-sp->fmt_start)/8 * 8);
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
                      (rsat_echo_delay[sp->aux.beam_seq] + roll_tilt)/prf);
	    r_cls_2 = (float)(pp->r_close) - 2.99792458E8 /2. *
		      ((float)(sp->wdwp[wdchk]) * dpf.b_prf - dpf.a_dwp + 
                      (rsat_echo_delay[sp->aux.beam_seq] + roll_tilt)/prf);
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
                         (rsat_echo_delay[sp->aux.beam_seq] + roll_tilt)/prf)
                         * spf.csr + 0.5); /* clw 1/31/96 */
	    if (in_offset1 < 0) { 
		printf("ERROR!!! Negative offset - no window change\n");
		printf("r_close = %g, wdchk-1 = %d, wdwp[wdchk-1] = %d, offset = %d\n",
		pp->r_close, wdchk-1, sp->wdwp[wdchk-1], in_offset1);
		in_offset1 = -in_offset1;
	    }
	    in_offset2 = in_offset1;
	    in_offch = 0;  /* dummy */

	/* Add the win_pos in the scene_file 6/21/96 CV */
	    if (wdchk >= sp->win_count) {
		sf.wndw_pos = (sp->wdwp[sp->win_count-1]*dpf.b_dwp - dpf.a_dwp)
 			* 1000000.;
	    } else 
		sf.wndw_pos = (sp->wdwp[wdchk]*dpf.b_dwp - dpf.a_dwp)*1000000.;
	}   

	if (vbose) printf("wndw=%g\n",sf.wndw_pos);

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
	}
	if (strcmp(Cur_Rqst->type,"RPR") == 0) {
	    sf.np = 8192;
	    sf.nl = 8192;
	}
	if (strcmp(Cur_Rqst->type,"QLK") == 0) {
	    sf.np = 8192;
	    sf.nl = 8192;
	}
	if (strcmp(Cur_Rqst->type,"CPX") == 0) {
	    sf.np = 2048;
	    sf.nl = 12800;
	}
	if (strcmp(Cur_Rqst->type,"CSD") == 0) {
	    sf.np = 5632;
	    sf.nl = 26624;
	}
	sf.pnum_scene_ctr = sf.np / 2;
	sf.lnum_scene_ctr = sf.nl / 2;
	strcpy(sf.sp_id,"RADARSAT-1");
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
/*		ap_detshift -= Cur_Rqst->proc_gain/3; */
		ap_detshift -= (Cur_Rqst->proc_gain - pp->pro_gain)/3;
		pp->pro_gain += Cur_Rqst->proc_gain - pp->pro_gain;
	}
        /* Add 2 new rdmf_tbl, adjust the detshift CV 8/26/97 */
	if (Cur_Rqst->proc_gain != 0){
           if (max_agc < 14)
                ap_detshift += 2;
           if (max_agc > 25)
                ap_detshift -= 2;
           if (vbose) printf("VUU TEST ap_detshift =%d\n", ap_detshift);
	} else {
           if (max_agc < 14)
                pp->pro_gain += 6;
           if (max_agc > 25)
                pp->pro_gain -= 6;
           if (vbose) printf("VUU TEST proc_gain =%d\n", pp->pro_gain);
	}	
	in_scenelen = nframes;
	r1_get_fmt_loc(0,sp,stfmt,&in_startblock,&offset);
	in_blkoff = offset / 8;
	in_shift = offset % 8;
	in_norminv = sp->polarity;

	sprintf(rc_reffun,"%srf_ra_file.%d",DEF_PATH,sp->aux.beam_seq);
	write_proc_param_file(pname);

	return (PASS);
}

/* r1_ltrr(sp,pp,nfmts) --------------------------------------------
	This routine forword DCRSi nfmts formats.
*/
r1_ltrr(sp,pp,nfmts)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	int nfmts;
{
	RSAT_AUX_DATA aux;
	int blkno, fmt_end, nframes, tfmts, sync_code;

    	setup_iif_cntrl(0,0,1,2,0x1100,1,0);
	nframes = 8;
	nfmts /= nframes;
    	tfmts = nfmts * nframes;
        if (vbose)
            printf("...requesting %d frames of %d formats\n",
	           nframes,nfmts);
	setup_iif_par(VALID_PIXELS,nfmts,nframes,tfmts,0,0,0,1,13,0);
	ex_set_selftest(0); /* do we need this ??? */
	ex_set_delay(nfmts * 8192);
	sync_code = FS & VF & AL & VL;
	ex_set_event(SYNC_EVENT,0x0f,sync_code,nframes);
	ex_set_reset(0);
	play_dis(1);
	iif_set_reset(1);
	iif_set_reset(0);

    /* arm and trigger */
	r1_clear_hdr_mem(1);
	tarm();
	play_dis(0);
	printf ("Before tdone\n");
	tdone ();
	play_dis(1);

    /* check dropout bit */
	asp_read( (RLOC_RDFM+14)<<1, &mb.w[RLOC_RDFM+14], 2 );
	if ((mb.w[RLOC_RDFM+14] & 0x100) == 0) {
	    printf("dropout detected in LTRR\n");
	    return(FAIL);
	}

    /* at this point, the data should be captured */
  	if (vbose)
	    printf("\n...in LTRR after trigger\n");

    /* retrieve and check auxiliary data values */

/*SONY*/
        if (!strcmp( media_type, "DCRSI" ) )
           blkno = dc_get_addr(IN);
        else
           blkno = sony_get_addr(IN);
                                    
	fmt_end = r1_find_last_hdr();
	if (f_dropout == 1) {
	    f_dropout = 0;
	    return(FAIL);
	}
	if (vbose) 
	    printf("...%d formats played to skip\n",fmt_end+1);
/* ??? TBF
	r1_get_aux_data(0,&aux);
	if (vbose) 
	    printf("   first format is %d\n",aux.format);
	r1_get_aux_data(fmt_end,&aux);
	if (vbose) 
	    printf("    last format is %d\n",aux.format);
*/
	if (blkno == 0)
	    printf(" DCRS output block 0, who cares! ");
	if (vbose)
	    printf("...now at block %d\n",blkno);
}

/* r1_get_ephem() ----------------------------------------------------
	This routine get 55 words of ephemeris in IIF memory 
*/

r1_get_ephem(fmt)
	int fmt;
{	
	int i, j,value, pid_save, fcode, hdr_mode, hdr_len;
	int ephem_dat[100], fd2, nbyte;
	int write_data, even;
	int not_found_ecode = 1;
	union uid
	{
		int i[ 2 ];
		double d;
		char b[ 9 ];
	};

	union uid u1;
	union uif {
		int i;
		float f;
	} u2;

	if ((fd2 = creat( "ephem.dat", PERMS)) == -1){
		printf ("Error creating output file ephe.dat");
		return (FAIL);
	}

	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HEAD;
	get_iif_hdr_mode(&hdr_mode);
	if (hdr_mode == 1){
		printf("short mode: Does not save all 50 bytes AUX in"); 
		printf(" IIF memory\n");
		return(FAIL);
	}
	else hdr_len = HDR_LEN;
	asp_read( MEM_IIF_HEAD, &mb.w[MLOC_IIF_HEAD>>1], 0x80000 );
	while (not_found_ecode) {
	   i = (fmt * hdr_len) + (MLOC_IIF_HEAD >> 1);
	   i += AUX_LOC;
           fcode = ((mb.w[i] & 0xffff) << 16) | (mb.w[i+1] & 0xffff);
           if (bit_dif(fcode,AUX_SYNC_CODE,2) > 2){
	        /*if (vbose) printf("fcode (FAIL) = %x\n", fcode);*/
		return (FAIL);
	   }
	   ephem_dat[1] = mb.w[i+10] & 0xffff;
           if (bit_dif(ephem_dat[1],0xccaa,2) > 2){
	       /* if (vbose) printf("ecode (FAIL) = %x\n", ephem_dat[1]);*/
		fmt += 1;
	   } else not_found_ecode = 0;
	}
	/* if (vbose)
	 printf("fmt=%d,hdr_len=%d,i=%d,ephem=%x\n",fmt,hdr_len,i,ephem_dat[1]);
	*/
	for (j=2; j<56; j++) {
	   i = ((fmt + j - 1) * hdr_len) + (MLOC_IIF_HEAD >> 1);
	   i += AUX_LOC;
	   /* if (vbose)
	     printf("fmt= %d, hdr_len = %d, i=%d, ", j, hdr_len, i);*/
	   ephem_dat[j] = mb.w[i+10] & 0xffff;
	   /*if (vbose)
	     printf("ephem = %x\n", ephem_dat[j]);*/
	   even = j % 2;
	   if ((even == 0) | (j == 55)) {
		if (j == 55)
		    write_data = ((ephem_dat[j] & 0xffff) << 16) |
				(ephem_dat[j-1] & 0xffff);
		else 
		    write_data = ((ephem_dat[j-1] & 0xffff) << 16) |
				(ephem_dat[j] & 0xffff);
	   	nbyte = write(fd2,&write_data,4);
	        if (nbyte == 0) {
			printf("r1_get_ephe: %d\n", nbyte);
			return(FAIL);
	   	}
	    }
	}
	close(fd2);
	mb.w[RLOC_REP] = pid_save;

/* clw corrects ephem_dat indices, one less */

        ephem.ephe_sync = ntohl(((ephem_dat[1] & 0xffff) << 16) | 0xf0f0);
  	ephem.rev_num = ntohl((ephem_dat[3] & 0xffff) << 16) |
    			(ephem_dat[2] & 0xffff);
    	ephem.year = ephem_dat[4];
        ephem.day = ephem_dat[5];
    	ephem.hour = ephem_dat[6];
	ephem.min = ephem_dat[7];
	ephem.sec = ephem_dat[8];
	ephem.milsec = ephem_dat[9];

/* clw swaps u1.i[1] & u1.i[0] for double precision */

	u1.i[1] = (ephem_dat[13] << 16) | (ephem_dat[12]);
	u1.i[0] = (ephem_dat[11] << 16) | (ephem_dat[10]);
	ephem.greenw_ang = u1.d; 
	u1.i[1] = (ephem_dat[17] << 16) | (ephem_dat[16]);
	u1.i[0] = (ephem_dat[15] << 16) | (ephem_dat[14]);
	ephem.semi_major_axis = u1.d; 
	u1.i[1] = (ephem_dat[21] << 16) | (ephem_dat[20]);
	u1.i[0] = (ephem_dat[19] << 16) | (ephem_dat[18]);
	ephem.inclination = u1.d; 
	u1.i[1] = (ephem_dat[25] << 16) | (ephem_dat[24]);
	u1.i[0] = (ephem_dat[23] << 16) | (ephem_dat[22]);
	ephem.eccen = u1.d; 
	u1.i[1] = (ephem_dat[29] << 16) | (ephem_dat[28]);
	u1.i[0] = (ephem_dat[27] << 16) | (ephem_dat[26]);
	ephem.arg_percen = u1.d; 
	u1.i[1] = (ephem_dat[33] << 16) | (ephem_dat[32]);
	u1.i[0] = (ephem_dat[31] << 16) | (ephem_dat[30]);
	ephem.long_node = u1.d;
	u1.i[1] = (ephem_dat[37] << 16) | (ephem_dat[36]);
	u1.i[0] = (ephem_dat[35] << 16) | (ephem_dat[34]);
	ephem.mean_ano = u1.d; 
	u1.i[1] = (ephem_dat[41] << 16) | (ephem_dat[40]);
	u1.i[0] = (ephem_dat[39] << 16) | (ephem_dat[38]);
	ephem.drag_coef = u1.d; 

/* clw uses u2 for single precision */

	u2.i = (ephem_dat[43] << 16) | (ephem_dat[42]);
        ephem.roll_angle = u2.f;
	u2.i = (ephem_dat[45] << 16) | (ephem_dat[44]);
        ephem.pitch_angle = u2.f; 
	u2.i = (ephem_dat[47] << 16) | (ephem_dat[46]);
        ephem.yaw_angle = u2.f; 
	ephem.crc = ephem_dat[54] & 0xffff;

	if (vbose) {
	  printf("ephe_sync = %x\n", ephem.ephe_sync);
	  printf("rev_num = %d\n", ephem.rev_num);
	  printf("date = %d:%d:%d:%d:%d:%d\n", ephem.year, ephem.day,
		ephem.hour, ephem.min, ephem.sec, ephem.milsec);
	  printf("greenw_angle = %g \n", ephem.greenw_ang);
	  printf("semi_major_axis = %g \n", ephem.semi_major_axis);
	  printf("inclination = %g \n", ephem.inclination);
	  printf("eccen= %g \n", ephem.eccen);
	  printf("arg_percen = %g \n", ephem.arg_percen);
	  printf("long_node = %g \n", ephem.long_node);
	  printf("mean_ano= %g \n", ephem.mean_ano);
	  printf("drag_coef = %g \n", ephem.drag_coef);
	  printf("roll_angle= %g \n", ephem.roll_angle);
	  printf("pitch_angle= %g \n", ephem.pitch_angle);
	  printf("yaw_angle= %g \n", ephem.yaw_angle);
	  printf("crc= %d \n", ephem.crc);
	}
}
/* r1_select_agc_table() ----------------------------------------------------
	This routine find the agc_table for RSAT base on the start_format and
	agc_fmt 
*/

int r1_select_agc_table()
{	
	int agc_num, w, agc_table, average;
	int agc1, agc2;
	TAPE_SEG_PTR sp;

        /* Add 2 new rdfm_tbl, CV 8/26/97 */
        if (max_agc > 25)
                agc_table = 31;
        if ((max_agc > 13) && (max_agc < 26))
                agc_table = 25;
        if (max_agc < 14)
                agc_table = 19;
        return(agc_table);

/*	agc1 = agc2 = 0;
	for (sp = seg_list; (sp != NULL) && (st_fmt > sp->fmt_end);
		sp = sp->nxt_seg)
	    ;
	if (st_fmt <= sp->afmt[0]){
	   agc_num = sp->agc[0];
	   if (vbose) printf("st_fmt less = %d\n", st_fmt);
	}
	else if (st_fmt > sp->afmt[sp->agc_count-1]){
	   agc_num = sp->agc[sp->agc_count-1];
	   if (vbose) printf("st_fmt greater = %d\n", st_fmt);
	}
	else {
	    for (w = 0; (w < sp->agc_count-1) && (st_fmt > sp->afmt[w+1]);
		  w++) 
		;
	    average = (int) ((sp->afmt[w] + sp->afmt[w+1])/2);
	    printf("afmt[%d]=%d,afmt[%d]=%d,average = %d\n", 
		w,sp->afmt[w],w+1,sp->afmt[w+1],average);
	    if (st_fmt > average) agc_num = sp->agc[w+1];
	    else agc_num = sp->agc[w];
	    if (vbose) printf("st_fmt in the range = %d\n", st_fmt);
	}
	if (agc_num > 4) {
	    agc1 = agc_num - 4;
	    agc2 = agc_num + 4;
	}
	if ((agc1 >= 0) && (agc2 <= 15)) agc_table = 1;
	else if ((agc1 >= 8) && (agc2 <= 23)) agc_table = 2;
	else if ((agc1 >= 16) && (agc2 <= 31)) agc_table = 3;
	else if ((agc1 >= 24) && (agc2 <= 43)) agc_table = 4;
	else {
	    printf("The max of agc num in table is 39; agc_num= %d\n",
		 	agc_num);
	    return (-1);
	}
	if (vbose) {
	    printf("st_fmt=%d, agc_num=%d, w=%d, agc_table=%d\n", st_fmt,
		agc_num,w, agc_table);
	}
	return(agc_table);
*/
}
	
