/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* j1_proc_ppr.c -- JERS-1 process a preprocessing region
		    Using 2DFFT for Doppler measurement.
		    There is no azimuth scaling.
   M. Chen, 6/4/92.
*/

#include <aspdecl.h>
#include <procfil.h>
#include <procdec.h>
#include <procpar.h>
#include <scene_file.h>
#include <stdio.h>
#include <math.h>

/* sync code bits */
#define FS 0x01
#define VL 0x02
#define VF 0x04
#define AL 0x08
#define SS 0x10

#define SYNC_EVENT 0
#define COUNT_EVENT 1
#define HK_LEN 23
#define STATLEN 50		/* length of the status register */

/* hardware definitions */
#define MEND_EXEC 0xfffff  /* end of Exec response buffer page */
#define RMODE_MEM_READ 0   /* exec response buffer read mode */

extern char ASP_VERSION[8];
extern alarm;			/* 1 = timer alarm has gone off */
extern first_odd;		/* 1 = first region in a pass */
extern first_ident;

/* data from current job request */
extern RQST_PTR Cur_Rqst;	/* pointer to current job request */

/* Parameter and data files */
extern SP_FILE spf;		/* sensor parameters file */
extern DPP_FILE dpf;		/* default processing parameters file */
extern EM_FILE emf;		/* earth model file */
extern SCENE_FILE sf;		/* scene file */

extern TAPE_SEG_PTR seg_list;	/* tape segment list */

/* ASP configuration parameters */
extern int vbose;		/* 1 = print progress messages */
extern int sw_dcrsi_in;		/* 1 = use input dcrsi */
extern int sw_auto_scale;	/* 1 = adjust fft scaling automatically */
extern int sw_clutter_lock;	/* 1 = perform 2DFFT */
extern int fst_len;		/* length of first input format, from scan */
extern int in_lenoff;		/* fst_len to be passed to write_proc_params */
extern char in_jdfm_tbl[80];
extern int pre_done;
extern int sw_pp_only;		/* 1 = preprocessing only */

extern double r3p[3];           /* 3 ranges for corn_loc routine */
extern float fd3p[3];           /* 3 corresponding fd values */
extern float fr3p[3];           /* 3 corresponding fr values */
extern float fdg[3],fdotg[3];          /* ground Doppler coef */

extern int frdelay_sw;      /* 1 = clamp frame delay to 8 */
extern int pulse_delay;             /* # of prf lines delay to first
                                        output pixel */
extern int cp_flag;
extern float gaintbl[900];

int stchk = 1;                  /* counter for STC change */
int agchk = 1;                  /* counter for AGC change */

int fmt_tobe;			/* variable to trace format number */

double get_gmt_diff();

/*SONY*/
extern char media_type[];
extern char data_direction[];

/* j1_2dfft(sp,pp) -------------------------------------------
	This routine determines the Doppler frequency by 2DFFT
*/

j1_2dfft(sp,pp)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
{
#undef NFMTS
#define NFMTS 512
	AUX_DATA aux;
	int sync_code, i, j, k, l, rpt, rpt3, rpt23;
	char id[STATLEN], filename[80];
	char *strcpy();
	int first = 1;
	int backup = 0;
	int good_data = 0;
	int dop[3][128], rng[1024], p_noise[3], p_sig[3];
	int read_rcdr;
	int del_j, cut_j, energy, half, init_center;
	int fmt_end, blkno, nframes, nfmts, tfmts;
	int noise_1, noise_2, sig_1, sig_2, estat[3], ans;
	float percent, mhz_bin, snr[3], dop_f[3], tempfd[3], newfd[3];
	float dop_avg[3];
	int file_mj;
	short int data_2d[49152];

/*SONY*/
        int BYTES_BLOCK;

        BYTES_BLOCK = TAPE_BLOCK_LEN;  /* 144304 for SONY, 4356 for DCRSi*/

    	read_rcdr = IN;

    /* if preprocessing only, do fine tune LTRR */
        if (sw_pp_only == 1 && first_odd == 0) {
    	    if (vbose) printf("LTRR ... again\n");
	    nfmts = pp->fmt_start - fmt_tobe - 12000;
	    nfmts = 8 * (int)((nfmts+4)/8.);
	    if (j1_ltrr(sp,pp,nfmts) == FAIL) return(FAIL);
	    fmt_tobe = pp->fmt_start;
	}

	if (sw_clutter_lock == 0) {
	    printf("2DFFT Bypassed ...\n");
	    return (PASS);
	}

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

	reset_jdfm(1,1);
	reset_jdfm(0,0);

	if (vbose) printf("Finish reset of jdfm.\n");
	fflush(stdout);
	while (!good_data) {
	/* setup and process 4*512 lines thru the range fwd fft */
	    strcat(strcpy(filename,DEF_PATH),"params.2dfft");
	    if (read_proc_param_file(filename) == FAIL) {
		printf("...unable to read params.2dfft file\n");
		return (FAIL);
	    }
	    /* to overwrite BAD setup in params.2dfft */
	    in_norminv = pp->polarity;
	    rc_fscale = pp->fftsc_raf;
	    in_lenoff = fst_len;
	    in_blkoff = pp->bit_off / 8 ;
	    in_shift = pp->bit_off % 8;
	    if ( vbose ){
	    	printf("fst_len: %d\n",fst_len); fflush(stdout);
	    }
	    if (p_load_regs() == FAIL) {
		printf("...register load failed\n");
		fflush(stdout);
		return (FAIL);
	    }
/*
	    printf("before setup for 2dfft\n");
	    if (abob()) exit(1);
*/
	/* setup 2DFFT.  It is massy, however, 1-lk or 4-lk maybe not important */
/*	    setup_jdfm(0x1111,3,0,1,1,0,dpf.fsm,dpf.maxfsdo,fst_len-24); */
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
		if (strcmp(Cur_Rqst->type,"STD") == 0) first_ident = 0;
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
                if (pp->bit_off < 32) {
                    pp->blk_start -= 1;
                    pp->bit_off += BYTES_BLOCK * 8;
                }



/*SONY*/
                if (!strcmp( media_type, "DCRSI" ) ) {
	    	  if (vbose)
	    	    printf("starting DCRSi at %d, offset %d, polarity %d\n",
                            pp->blk_start,pp->bit_off,pp->polarity);
			fflush(stdout);
	    	  start_fmt_play(1,pp->blk_start,pp->bit_off-32,pp->polarity);
                }
                else {
	    	      if (vbose)
	    	        printf("starting SONY at %d, offset %d, polarity %d\n",
                            pp->blk_start,pp->bit_off,pp->polarity);
	              fflush(stdout);
                      if (!strcmp( data_direction, "REVERSE" ) )
	    	       sony_start_fmt_play_REV(1,pp->blk_start,pp->bit_off-184,pp->polarity);
                      else
	    	       sony_start_fmt_play(1,pp->blk_start,pp->bit_off-184,pp->polarity);
                     }
		
		if (pre_done == 1) {
		    printf("Surrender!\n");
		    pre_done = 0;
		    return(FAIL);
		}
	    if (first_odd == 2 || first_ident == 1) {
		first_ident = 0;
/*		j1_Ted_fix(); comment out 8/22/96 */
	    }
	    	reset_rpi(0);
		first_odd = 0;
	    }
/*
	    printf("Before play for 2DFFT\n");
	    if (abob()) exit(1);
	    if (abob()) exit(1);
*/
	/* arm and trigger */
	    j1_clear_hdr_mem(0);
	    tarm();
	    play_dis(0);
	    /* writing to mb array */
	    ans = p_get_avg_image (pr_avglines, (ml_linelen + 7)/8, "");
/*	    p_get_avg_image (pr_avglines, (ml_linelen + 7)/8, "avg_junk"); */
	    if (ans == -1) {
		printf("failed in get_avg_image\n");
/*		if (abob()) exit(1); */
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

	/* check dropout bit */
	    asp_read( (RLOC_JDFM+7)<<1, &mb.w[RLOC_JDFM+7], 2 );
	    if (((mb.w[RLOC_JDFM+7] >> 14) & 0x1) == 0) {
		printf("dropout detected in 2DFFT\n");
/*		if (abob()) exit (1); */
		return(FAIL);
	    }

	/* save data captured in mb array to a file */
	    if ((file_mj = creat("avg_2dfft", 0666)) == FAIL) {
		printf("\n Can't open %s", "avg_2dfft");
		return(FAIL);
	    }
	    for (i = 0; i < 0xc000; i++) data_2d[i] = mb.w[i+0x8000];
	    if (write(file_mj, data_2d, 98304) != 98304) {
		printf("Error in writing avg_2dfft\n");
		return(FAIL);
	    }
	    close(file_mj);
	    printf("avg_2dfft saved\n");
/*	    if (abob()) exit(1); */

	    good_data = 1;
	    fflush(stdout);
	}  /* while !good_data */

    /* perform compress in both directions for data in mb array */
	rpt = ((ml_linelen + 7) / 8) >> 1; /* ml_linelen is 3*2048 */
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
    /* save 128 points range spectrum to a file */
/* ---	if ((file_mj = creat("rng_spec", 0666)) == FAIL) {
	    printf("\n Can't open %s", "rng_spec");
	    if (abob()) exit(1);
	} --- */
	for (k = 0; k < rpt3; k++)
	    data_2d[k] = (short int)(rng[2*k] + rng[2*k+1] +
				     rng[2*(k+rpt3)] + rng[2*(k+rpt3)+1]+
				     rng[2*(k+rpt23)] + rng[2*(k+rpt23)+1])/6;
/* ---	if (write(file_mj, data_2d, 256) != 256) {
	    printf("Error in writing rng_spec\n");
	    if (abob()) exit(1);
	}
	close(file_mj); --- */
	p_write_mem_file(data_2d,128,"rng_spec","","range spectra from 2dfft");
	printf("rng_spec saved\n");

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

	if (fabs(pp->fda-tempfd[0]) > 100.) {
	    printf("big diff between measured and orig dops, use 100\n");
	    printf("orig = %g, measured = %g\n", pp->fda, tempfd[0]);
	    if (pp->fda > tempfd[0])
		tempfd[0] = pp->fda - 100;
	    else
		tempfd[0] = pp->fda + 100;
	}
	pp->fda = tempfd[0];
	pp->fdb = tempfd[1];
	pp->fdc = tempfd[2];
	if (vbose)
	printf("adjusted fd(a,b,c): %g %g %g\n",pp->fda,pp->fdb,
	    pp->fdc);

	return (PASS);
}


/* j1_clear_hdr_mem(mode) ----------------------------------------------
	This routine clears the IIF header memory to 0x55, to permit
	the j1_find_last_hdr routine to locate the last recorded header.
	mode = 1 means ??? mode, = 0 means ??? mode.
*/

j1_clear_hdr_mem(mode)
	int mode;
{
	int i, igo, iend, pid_save, mloc;

	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HEAD;
	igo = (MLOC_IIF_HEAD) >> 1;
	iend = igo + (0x80000 >> 1);
	mloc = MEM_IIF_HEAD;
	asp_read( mloc, &mb.w[MLOC_IIF_HEAD>>1], 0x80000 );
	for (i = igo; i < iend; i++) {
	    mb.w[i] = 0x5555;
	}
	asp_write( mloc, &mb.w[MLOC_IIF_HEAD>>1], 0x80000 );
	mb.w[RLOC_REP] = pid_save;
}


/* j1_get_hwdata(sp,pp,in_mode) --------------------------------------
	This routine retrieves the bit error rate and input data
	histograms from the hardware, saves them for later, and does
	some quality checking on them.  If the quality checks pass, the
	routine returns PASS, otherwise, FAIL.
	in_mode = 0 if 2DFFT operation. in_mode = 1 if run_image.
*/

j1_get_hwdata(sp,pp,in_mode)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	int in_mode;
{
	float bits;
	int errcount,i,j,pid_save;
	int itotal = 0;
	int qtotal = 0;
	int count = 64;
	float hilev,lolev,normal,xfit[64];

    /* retrieve the bit error rate */
	asp_read( (RLOC_JDFM+3)<<1, &mb.w[RLOC_JDFM+3], 2 );
	errcount = 0xffff & mb.w[RLOC_JDFM + 3];
	if (in_mode == 0)
	    bits = 60.0 * 2048.0; /* 2K formats for 2DFFT */
	else
	    bits = 60.0 * in_scenelen * in_vlines; /* run_image */
	pp->ber = errcount / bits;
	printf("error count = %d, bit error rate = %g\n",errcount, pp->ber);
    /* retrieve the histograms */
	pid_save = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_IIF_HIST;
	j = 32;
	asp_read( MEM_IIF_HIST, &mb.w[MLOC_IIF_HIST>>1], 512 );
	for (i = MLOC_IIF_HIST >> 1; i < (MLOC_IIF_HIST >> 1) + 256;
		i += 4) {
	    pp->ihist_cor[j] = (mb.w[i  ] << 16) + (mb.w[i+1] & 0xffff);
	    pp->qhist_cor[j] = (mb.w[i+2] << 16) + (mb.w[i+3] & 0xffff);
	    itotal += pp->ihist_cor[j];
	    qtotal += pp->qhist_cor[j];
	    j++;
	    j = j%64;
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
/*	    if (abob()) exit(1); */
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

/* j1_ltrr(sp,pp,nfmts) -------------------------------------
	This routine forword DCRSi nfmts formats.
*/
j1_ltrr(sp,pp,nfmts)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	int nfmts;
{
	int blkno, fmt_end, nframes, tfmts, sync_code, lgfmt;

    	setup_iif_cntrl(0,0,1,3,0x1100,1,0);
	nframes = 8;
	nfmts /= nframes;
    	tfmts = nfmts * nframes;
        if (vbose)
            printf("...requesting %d frames of %d formats\n",
	           nframes,nfmts);
	setup_iif_par(6144,nfmts,nframes,tfmts,0,0,0,1,13,0);
	ex_set_selftest(0);
	ex_set_delay(nfmts * 8192);
	sync_code = FS & VF & AL & VL;
	ex_set_event(SYNC_EVENT,0x0f,sync_code,nframes);
	ex_set_reset(0);
	play_dis(1);
	iif_set_reset(1);
	iif_set_reset(0);

    /* arm and trigger */
	j1_clear_hdr_mem(0);
	tarm();
	play_dis(0);
	printf ("Before tdone\n");
	tdone ();
	play_dis(1);

    /* check dropout bit */
	asp_read( (RLOC_JDFM+7)<<1, &mb.w[RLOC_JDFM+7], 2 );
	if (((mb.w[RLOC_JDFM+7]>>14) & 0x1) == 0) {
	    printf("dropout detected in LTRR\n");
	    return(FAIL);
	}

    /* at this point, the data should be captured */
  	if (vbose)
	    printf("\n...in LTRR after trigger\n");
	asp_read( (RLOC_JDFM+4)<<1, &mb.w[RLOC_JDFM+4], 2 );
	lgfmt = ((int)mb.w[RLOC_JDFM + 4]) & 0xffff; /* ??? use CR */
	if (vbose)
	    printf("... format counter = %d\n",lgfmt);
/*SONY*/
        if (!strcmp( media_type, "DCRSI" ) )
           blkno = dc_get_addr(IN);
        else
           blkno = sony_get_addr(IN);

	if (blkno == 0) {
	    printf(" DCRS output block 0, who cares! ");
	}
	if (vbose)
	    printf("...now at block %d\n",blkno);
}


/* linear_qr(f_vec,e_vec,num_smpl,f_coff)  -----------------
This is a "least squares fit to a polynomial of the first degree"
routine.  It attempts to fit the  linear curve y = a + bx
to the 'n' data points supplied, using a subset of 'num_smpl' data
points evenly spaced through the data.

The results of this routine are the coefficients a and b of the
linear equation that best fits these points.

*/

linear_qr(f_vec,e_vec,num_smpl,f_coff)
	float     *f_vec, *f_coff;
	int  *e_vec, num_smpl;

{
	int  i, j, k;
	double	 a[3][3], s, c, d;

	if (num_smpl != 3) {
	    printf("we only handle num_smpl = 3 situation\n");
	    return(FAIL);
    	}
	a[0][0] = 1024.;
	a[0][1] = 1.;
	a[0][2] = f_vec[0];
	a[1][0] = 3072.;
	a[1][1] = 1.;
	a[1][2] = f_vec[1];
	a[2][0] = 5120.;
	a[2][1] = 1.;
	a[2][2] = f_vec[2];
	for(i=0; i<num_smpl-1; i++) {
	    for (j=i+1; j<num_smpl; j++) {
		d = sqrt(a[i][i]*a[i][i] + a[j][i]*a[j][i]); 
		c = a[i][i]/d;
		s = a[j][i]/d;
		a[i][i] = d;
		a[j][i] = 0.;
		    for (k=i+1; k<3; k++) {
			a[i][k] = c*a[i][k] + s*a[j][k];
			a[j][k] = c*a[j][k] - s*a[i][k];
		    }
	    }
	}
	f_coff[0] = a[1][2]/a[1][1];
	if (a[0][0] == 0.) 
	    f_coff[1] = 0.;
	else
	    f_coff[1] = (a[0][2]-f_coff[0]*a[0][1])/a[0][0];
}

/* adjust_fds(fdin,fdout) -------------------------------------------
        The least_sq routine returns a1,b1,c1 for 0 <= x <= 18,
        with x incrementing by 1.  We need to convert these to
        a,b,c for 128 <= x <= 4736, with x incrementing by 256.
        The formulae are:
	    a = a1 - (b1 / 2) + (c1 / 4)
	    b = (b1 - c1) / 256
	    c = c1 / 65536
*/

adjust_fds(fdin,fdout)
	float fdin[3],fdout[3];
{
	    fdout[0] = fdin[0] - (fdin[1] / 2.0) + (fdin[2] / 4.0);
	    fdout[1] = (fdin[1] - fdin[2]) / 256.0;
	    fdout[2] = fdin[2] / 65536.0;
}
/* j1_build_image_files(sp,pp,seg,region) ------------------------------
	This routine creates the image directory and processing files
	for one image, centered on preprocessing region pp.
*/

j1_build_image_files(sp,pp,seg,region)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	int seg,region;
{
	char image_id[20],short_id[20];
	char filename[80];
	char sdef_path[132];
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
	int ngains=0;
	double deltat = 0.1;
	float dtheta = 0.1;
	int ifirst = -30;
	int temp;
	int images = 0;
	float r_e,f,sclmcv,r_cls_1,r_cls_2,x[16384];
	short int d1[16384];
	int npts,istat,l1,l2,l3,l4,block;
	int i, offset_change =0, temp_stchk=0;


	if (!cp_flag) { 
            sprintf(filename,"%sgaintbl",DEF_PATH);
            ngains =  GetCalFile( filename ); 
            if (ngains == FAIL ){
                printf("j1_build_image_files: Can't find %s (use default)\n",
                        filename);
            }
	}
	else {
            ngains = AspReadCalFile();  /* reading the CPF file */
            if (ngains == FAIL){
                printf("Error in reading cal_file, ngains=%d \n", ngains);
		return(FAIL);
	        /* in case ASP does not receive the right CPF file from PVS,then
		read gaintbl file in the default dir. It should be return FAIL 
		if there is error in CPF file in the future

                printf("Reading file gaintbl in default directory\n");
                sprintf(filename,"%sgaintbl",DEF_PATH);
                ngains = GetCalFile( filename );
                if ( ngains == FAIL ){
                   printf("e1_build_image_files: Can't find %s (use default)\n",
                        filename);
                } */
            }
        }

	if (vbose) printf("ngains=%d\n",ngains);
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
	in_lenoff = fst_len; /* from scan if no re-sync */
        if (sp->polarity == 1)
            strcat(strcpy(in_trantbl,DEF_PATH),"in_lut_1to1");
        else
            strcat(strcpy(in_trantbl,DEF_PATH),"in_lut_1to1.i");
	
    /* add in fft scaling and detector shift values from pass 2 & 3 */  
	rc_fscale = pp->fftsc_raf;
	rc_iscale = pp->fftsc_rai;
	af_scale = pp->fftsc_az4f;
	ap_scale = pp->fftsc_az4i;
	ap_detshift = 2;

	if (strcmp(Cur_Rqst->type, "CPX") == 0) 
		ap_detshift = -1;

    /* calculate start format and # of frames */
	minfmt = sp->wfmt[0];  /* change by MC 2/13/97 */
	maxfmt = sp->fmt_end;
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
/* ???
	if (strcmp(Cur_Rqst->type, "STD") == 0) 
		stfmt = pp->fmt_start;
	else
		stfmt = pp->fmt_start - (time*prf + 0.5);
*/
/*	stfmt = pp->fmt_start;  comment out CV 3/1/96 */

	stfmt = pp->fmt_start - (time*prf + 0.5);
	if (vbose) printf("stfmt in CV =%d, ", stfmt);
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
	dsec = (double)(pp->fmt_start - stfmt) * pri;
	start_gmt = pp->sv.gmt;
	add_seconds(&start_gmt,-dsec);
	istat = 0;
	r3p[0] = pp->r_close;
	r3p[1] = pp->r_mid;
	r3p[2] = pp->r_far;
	fd3p[0] = 0;
	fd3p[1] = 0;
	fd3p[2] = 0;
	fr3p[0] = -660.0;
	fr3p[1] = -660.0;
	fr3p[2] = -660.0;
	get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,deskew,0.0,
		     a,b,c,d,e,&istat);
	pp->r_e = c[2] / 1000.0;
	rads[0] = a[2];
	rads[1] = b[2];
	rads[2] = d[2];
	rads[3] = e[2];
    /* refine the swath speed and scene length */
    /* do not recalculate stfmt */
	get_abcd_dist(a,b,d,e,&ad,&be,&ae,&bd);
	sw_adj = (ad + be) / (2.0 * dist2);
	pp->swth_speed *= sw_adj;
	time = dist / pp->swth_speed;
	time2 = dist2 / pp->swth_speed;
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

    /* Tom's fudge factor for location skew, 11/16/91 */
	skew_adj += 750.;

    /* calculate and write the azimuth cage setups */
	setup_ap(sp,pp,looks,ON,OFF,deskew,start_bin,rads,skew_adj);

    /* remove Tom's fudge factor for location skew, 11/16/91 */
	skew_adj -= 750.;

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
		if ( op_answer("Do you want to abort processing?") == PASS)
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

/* NOT to do this for JERS-1, Ming, 10/27/92
	if ((strcmp(Cur_Rqst->type,"STD") != 0)
		&& (strcmp(Cur_Rqst->type,"QLK") != 0)) {
	    stfmt -= pulse_delay;
	    if (stfmt < minfmt)
		stfmt = minfmt;
	}
*/
	endfmt = stfmt + nfmts;
	if (vbose) printf("stfmt=%d, nfmts=%d,minfmt=%d, maxfmt=%d\n",stfmt,nfmts,minfmt,maxfmt);
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
/* comment out for test ???
*/
	sclmcv = 1.2E13;   /* J-ERS-1 specific scaling factor */
	rdmtric_(&istat,
		 &svec[0],&svec[3],&pp->att,&spf.gamma_nom,
		 &range,&spf.srspace,&pp->fdota,
		 gaintbl,&ngains,&ifirst,&dtheta,&r_e,&f,&npts,
		 d1,&sclmcv,x);

	if (vbose) printf(" **** sclmcv = %g ****\n", sclmcv);

	p_write_mem_file(d1,npts,"rc_funct","","radiometric comp.");

	p_make_lookup_tbl(pp,d1);
	p_write_mem_file(d1,8192,"input_trans","","input lookup tbl.");

/* NOT TO copy the range transfer function file */
/* add debug to find out possibly why this does not work */
	if (strcmp(Cur_Rqst->type,"CSD") != 0) {
	    sprintf(cmd,"cp %srf_ra_file .",DEF_PATH);
	    temp = system(cmd);
/*	    printf("DBG: status after cp rf_ra_file = %d\n",temp); */
	}

    /* build the scene file */
	p_init_scene_file();
	strcpy(sf.sw_id,"ASP");
	strcpy(sf.file_name,image_id);
    /* add no fudge factor equal to frame size */	
/*	dsec = (pp->fmt_start - stfmt - pulse_delay + in_vlines) * pri; */
	dsec = (double)(pp->fmt_start - stfmt - pulse_delay) * pri;
	start_gmt = pp->sv.gmt;
	add_seconds(&start_gmt,-dsec);
    /* add the delay time due to zero doppler line */
	dsec = - ((double)fd3p[0])/fabs((double)fr3p[0]);

    /* Tom's fudge factor for absolute location error, 11/16/92 */
	dsec += .85;

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

    /* checking for window position change */
	r_cls_1 = r_cls_2 = 0.;
	/*
	f_size = in_vlines * (nframes-2);
	*/
	f_size = 28000;
	if (strcmp(Cur_Rqst->type,"CPX") == 0)
	    f_size = 15360;
	if (region == 1) stchk = 1;

	while (stfmt > sp->wfmt[stchk] && stchk < sp->win_count) {
	    stchk++;
	    if(vbose)
	    printf(" Jumping input for STC..., stfmt = %d, stchk = %d\n",
		    stfmt, stchk);
	    fflush(stdout);
	}
	if(stfmt + f_size > sp->wfmt[stchk] && stchk < sp->win_count) {
	    if(vbose)
		printf(" Window change ..., stfmt = %d, stchk = %d\n",
			stfmt, stchk);
	    r_cls_1 = (float)(pp->r_close) - 2.99792458E8 /2. * 
		      ((float)(sp->wdwp[stchk-1]) * dpf.b_dwp - dpf.a_dwp+7./prf);
	    r_cls_2 = (float)(pp->r_close) - 2.99792458E8 /2. *
		      ((float)(sp->wdwp[stchk]) * dpf.b_dwp - dpf.a_dwp+7./prf);
	    if(vbose) {
	    printf(" ... Window position changed\n");
	    printf(" r_cls_1 = %g, r_cls_2 = %g\n", r_cls_1, r_cls_2);
	    printf(" pp->r_close = %g, wdwp = %d, b_dwp = %g, prf=%g\n",
			pp->r_close, sp->wdwp[stchk], dpf.b_dwp, prf);
	    }
	    if(r_cls_1 >= r_cls_2) {
	        in_offset1 = 0;
	        in_offset2 = (int)((float)(sp->wdwp[stchk] - 
			sp->wdwp[stchk-1]) * (dpf.b_dwp) * (spf.csr) + 0.5);
                sf.wndw_pos = (sp->wdwp[stchk]*dpf.b_dwp - dpf.a_dwp) *
				1000000.;
	    }
	    else {
	        in_offset1 = (int)((float)(sp->wdwp[stchk-1] -
			sp->wdwp[stchk]) * (dpf.b_dwp) * (spf.csr) + 0.5);
		in_offset2 = 0;
                sf.wndw_pos = (sp->wdwp[stchk-1]*dpf.b_dwp - dpf.a_dwp) *
				1000000.;
	    }
	    if(in_offset1 < 0 || in_offset2 < 0) {
		printf(" ERROR!!! Negative offset in IIF\n");
		printf(" offset1 = %d, offset2 = %d\n",
				in_offset1, in_offset2);
	    }
	    in_offch = sp->wfmt[stchk] - stfmt;
	    offset_change = 1;	/* CV 1/14/98 */
	    temp_stchk = stchk;	/* CV 1/14/98 */
	    if((stchk+1) < sp->win_count) stchk++;
	  }
	else {
	    in_offset1 = (int)((sp->wdwp[stchk-1]*dpf.b_dwp - dpf.a_dwp)*spf.csr -
    	      ((float)(pp->r_close)/(2.99792458E8/2.)-7./prf)*spf.csr + 0.5);
	    if (in_offset1 < 0) { 
		printf("ERROR!!! Negative offset - no window change\n");
		printf("r_close = %g, stchk-1 = %d, wdwp[stchk-1] = %d, offset = %d\n",
		pp->r_close, stchk-1, sp->wdwp[stchk-1], in_offset1);
		in_offset1 = -in_offset1;
	    }
	    in_offset2 = in_offset1;
	    in_offch = 0;  /* dummy */

	/* Add the win_pos in the scene_file 6/21/96 CV */
	    if (stchk >= sp->win_count) {
                sf.wndw_pos = (sp->wdwp[sp->win_count-1]*dpf.b_dwp - dpf.a_dwp)*
				1000000.;
	    } else
                sf.wndw_pos = (sp->wdwp[stchk]*dpf.b_dwp - dpf.a_dwp)*1000000.;
  	}  

	if (vbose) printf("wndw_pos=%g\n",sf.wndw_pos);
    /* use different jdfm_tbl for region with agc = 0. 2/3/93 */
	if (region == 1) agchk = 1;
	while (stfmt >= sp->afmt[agchk] && agchk < sp->agc_count) {
	    agchk++;
	    if(vbose)
	    printf(" Jumping input for AGC..., stfmt = %d, agchk = %d\n",
		    stfmt, agchk);
	    fflush(stdout);
	}
	if((stfmt + f_size < sp->afmt[agchk] || agchk == sp->agc_count) 
		&& sp->agc[agchk-1] == 0) {
	    if ((strcmp(Cur_Rqst->type,"CPX") != 0) && 
		(Cur_Rqst->id[9] != 'P')) {
	    	ap_detshift -= 1;
	    	if (ap_detshift < 0) {
		    printf("ERROR in detect shift\n");
		    return(FAIL);
	    	}
	    	pp->pro_gain = 3;
	    }
	    if (vbose) printf("using 0.8 db per STC step1\n");
	    sprintf(in_jdfm_tbl,"%sjdfm_tbl.8",DEF_PATH);
	}
	else
	    sprintf(in_jdfm_tbl,"%sjdfm_tbl.6",DEF_PATH);
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
/*	sf.wndw_pos = (sp->wdwp[stchk-1]*dpf.b_dwp - dpf.a_dwp) * 1000000.;
	if (r_cls_1 > r_cls_2)
	sf.wndw_pos = (sp->wdwp[stchk]*dpf.b_dwp - dpf.a_dwp) * 1000000.;
*/
	strcpy(sf.sp_id,"JERS-1"); 
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
    	if (Cur_Rqst->id[9]=='P') 
		ap_detshift -= pp->pro_gain/3; 

	in_scenelen = nframes;
 
    /* if RPR or CPX, checking change of starting format */
	if ((strcmp(Cur_Rqst->type, "RPR") == 0 ||
	     strcmp(Cur_Rqst->type, "CPX") == 0 ) &&
	     (stfmt != pp->fmt_start)) {
		if (vbose) printf("starting fmt changed, new fmt is %d\n", stfmt);
		j1_get_fmt_loc(1,sp,stfmt,&fst_len,&pp->blk_start,&pp->bit_off);
		pp->fmt_start = stfmt;
		if (offset_change) { /* broken river; CV 1/14/98*/
		    printf("before in_offch is %d\n", in_offch);
	            in_offch = sp->wfmt[temp_stchk] - stfmt; 
		    printf("after in_offch is %d\n", in_offch);
		    offset_change = 0;
		} 
		if (vbose) printf("block=%d, offset=%d, fmt_start=%d\n", 
			pp->blk_start, pp->bit_off, pp->fmt_start);
	}

	in_startblock = pp->blk_start;
	in_blkoff = pp->bit_off / 8;
	in_shift = pp->bit_off % 8;
	in_norminv = sp->polarity;
	
	write_proc_param_file(pname);

	return (PASS);
}
