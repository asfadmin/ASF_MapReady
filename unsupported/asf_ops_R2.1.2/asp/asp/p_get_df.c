/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* p_get_dfiles.c -- routines to read the various default files */

#include <stdio.h>
#include <procfil.h>
#include <procdec.h>
#include <scene_file.h>

extern int toupper_sw;		/* 1 = convert input to upper case */

extern TAPE_SEG_PTR seg_list;	/* tape segment list */
extern TAPE_SEG_PTR save_seg_list; /* copy of tape segment list */

extern PREAM_FILE_PTR pream_list; /* preamble file list */
extern POSTAM_FILE_PTR postam_list; /* postamble file list */
extern int pream_max;		/* preamble max file # */
extern int postam_max;		/* postamble max file # */

extern RQST_PTR Cur_Rqst;	/* current job request record */

extern DPP_FILE dpf;		/* default processing params file */
extern SP_FILE  spf;		/* sensor parameters file */
extern EM_FILE  emf;		/* Earth model file */
extern SCENE_FILE sf;		/* scene file */

extern double stv[1200];	/* one-minute statevectors array */
extern int nstv;		/* number of statevectors */
extern GMT stv_gmt;		/* time of the first statevector */

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
extern int sw_pp_only;		/* 1 = preproc only */
extern int cp_flag;

int dwp[20];
int num_dwp_base;

/* p_get_version (filename, version)  -------------------------

routine to retrieve the ASP version number from the log file
which should be in the default directory (where config.asp is
kept) and the file name ought to be something like "logfile.E1"
or "logfile.J1".  It will retieve the first token which can be
separated with a blank.  This token should be of the
form MJ.MINR where MJ is the major version number and MINR is
a minor version number. MJ should be two characters and MINR 
should be 4 characters in length so that the whole thing is 
only 7 characters long.

*/

p_get_version (filename, version)
char *filename, *version;
{
	char t[80];
	int n;
	int ans = FAIL;

	if (open_file(filename,"") == 0) {
	  printf("error on input of file %s\n",filename);
	  return (ans);
	}

	set_separators(" ");
	next_token(t);
	/* check length is 7 */
	if (strlen(t) != 7) {
	  printf("error in length of version number:\n");
	  printf("   should be 7 but is %i\n",strlen(t));
	  printf("   string = //%s//\n",t);
	  return(ans);
	}

	close_file();
	strcpy(version,t);
	ans = PASS;
	return(ans);
}


/* p_get_cfg_file (filename) -------------------------------------------
	This routine reads the ASP configuration file, and stores the
	data in global cofiguration parameter variables.
*/

p_get_cfg_file (filename)
	char *filename;
{
	char t[100];
	static char *ptab[] = { "INPUT_DCRSI", 
				"NOISE_MEASURE",
				"REPLICA_MEASURE",
				"SNR_MEASURE",
				"CLUTTER_LOCK",
				"AMBIGUITY_DETERMINATION",
				"AUTO_FOCUS",
				"VERBOSE",
				"AUTO_SCALE",
				"PP_ONLY",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int fields_in = 0;
	int i, n, ibit;
	int ans = PASS;

    /* open the file */
	if (open_file(filename,"") == 0)
	    return (PASS);
    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;

    /* read the file & store the parameters */
	for (n = next_token(t); n > 0 && strcmp(t,"END.") != 0;
		n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++) {
		if (strcmp(t,ptab[i]) == 0)
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* INPUT_DCRSI */
		    next_token (t);
		    sw_dcrsi_in = (t[0] == 'Y');
		    break;
		case 1:		/* NOISE_MEASURE */
		    next_token (t);
		    sw_noise_meas = (t[0] == 'Y');
		    break;
		case 2:		/* REPLICA_MEASURE */
		    next_token (t);
		    sw_rep_meas = (t[0] == 'Y');
		    break;
		case 3:		/* SNR_MEASURE */
		    next_token (t);
		    sw_snr_meas = (t[0] == 'Y');
		    break;
		case 4:		/* CLUTTER_LOCK */
		    next_token (t);
		    sw_clutter_lock = (t[0] == 'Y');
		    break;
		case 5:		/* AMBIGUITY_DETERMINATION */
		    next_token (t);
		    sw_ambig = (t[0] == 'Y');
		    break;
		case 6:		/* AUTO_FOCUS */
		    next_token (t);
		    sw_auto_focus = (t[0] == 'Y');
		    break;
		case 7:	/* VERBOSE */
		    next_token (t);
		    vbose = (t[0] == 'Y');
		    break;
		case 8:	/* AUTO_SCALE */
		    next_token (t);
		    sw_auto_scale = (t[0] == 'Y');
		    break;
		case 9:	/* PP_ONLY */
		    next_token (t);
		    sw_pp_only = (t[0] == 'Y');
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter: %s\n",t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for */
	close_file();
	return (ans);
}


/* p_get_dpp_file (filename,sp) --------------------------------------
	This routine reads the default processing parameters file, and
	stores the data in the structure pointed to by sp.
*/

p_get_dpp_file (filename,sp)
	char *filename;
	DPP_FILE_PTR sp;
{
	char t[100];
	static char *ptab[] = { "SAT",
				"NS_RA",
				"NFFT_RA",
				"FFTSC_RAF",
				"FFTSC_RAI",
				"NCHIRP",
				"NFFT_AZ4",
				"NFFT_AZ1",
				"BW_AZ4",
				"FFTSC_AZ4F",
				"FFTSC_AZ1F",
				"FFTSC_AZ4I",
				"FFTSC_AZ1I",
				"DET_SC",
				"H_AZ",
				"A_PRF",
				"B_PRF",
				"A_DWP",
				"B_DWP",
				"FCNOISE",
				"BWNOISE",
				"FCSIG",
				"BWSIG",
				"PRO_GAIN",
				"CLOCK_ITER",
				"FDERROR_MAX",
				"FDAERROR_MAX",
				"FDBERROR_MAX",
				"FDC_MAX",
				"GAIN_REF",
				"LMEAN_TOL",
				"LSTDV_DIFF_MAX",
				"BLK_SKIP",
				"SCALE_THRESHOLD",
				"SBM",
				"FSM",
				"MAXFSDO",
				"NOISE_FACTOR_A1",
				"LINEAR_FACTOR_A2",
				"OFFSET_FACTOR_A3",
				"PEAK_REF",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, n, ibit, ibyte, fields_exp[2], fields_in[2];
	int ans = PASS;

    /* open the file */
	if (open_file(filename,"") == 0) {
	    printf("cannot open %s\n",filename);
	    return (FAIL);
	}
    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp[0] = 0x7fffffff;
	fields_exp[1] = 0x7fffffff >> (62 - ptablen);
	fields_in[0] = fields_in[1] = 0;

    /* read the file & store the parameters */
	for (n = next_token(t); n > 0; n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++) {
		if (strcmp(t,ptab[i]) == 0)
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* SAT */
		    next_token (t);
/*
		    strncpy (sp->sat,t,2);
*/
		    strncpy (sp->sat,Cur_Rqst->take_id,2);
		    sp->sat[2] = '\0';
		    break;
		case 1:		/* NS_RA */
		    get_int (&sp->ns_ra);
		    break;
		case 2:		/* NFFT_RA */
		    get_int (&sp->nfft_ra);
		    break;
		case 3:		/* FFTSC_RAF */
		    get_binary (&sp->fftsc_raf);
		    break;
		case 4:		/* FFTSC_RAI */
		    get_binary (&sp->fftsc_rai);
		    break;
		case 5:		/* NCHIRP */
		    get_int (&sp->nchirp);
		    break;
		case 6:		/* NFFT_AZ4 */
		    get_int (&sp->nfft_az4);
		    break;
		case 7:		/*  NFFT_AZ1 */
		    get_int (&sp->nfft_az1);
		    break;
		case 8:		/* BW_AZ4 */
		    get_float (&sp->bw_az4);
		    break;
		case 9:		/* FFTSC_AZ4F */
		    get_binary (&sp->fftsc_az4f);
		    break;
		case 10:	/* FFTSC_AZ1F */
		    get_binary (&sp->fftsc_az1f);
		    break;
		case 11:	/* FFTSC_AZ4I */
		    get_binary (&sp->fftsc_az4i);
		    break;
		case 12:	/* FFTSC_AZ1I */
		    get_binary (&sp->fftsc_az1i);
		    break;
		case 13:	/* DET_SC */
		    get_int (&sp->det_sc);
		    break;
		case 14:	/* H_AZ */
		    get_float (&sp->h_az);
		    break;
		case 15:	/* A_PRF */
		    get_float (&sp->a_prf);
		    break;
		case 16:	/* B_PRF */
		    get_float (&sp->b_prf);
		    break;
		case 17:	/* A_DWP */
		    get_float (&sp->a_dwp);
		    break;
		case 18:	/* B_DWP */
		    get_float (&sp->b_dwp);
		    break;
		case 19:	/* FCNOISE */
		    get_float (&sp->fcnoise);
		    break;
		case 20:	/* BWNOISE */
		    get_float (&sp->bwnoise);
		    break;
		case 21:	/* FCSIG */
		    get_float (&sp->fcsig);
		    break;
		case 22:	/* BWSIG */
		    get_float (&sp->bwsig);
		    break;
		case 23:	/* PRO_GAIN */
		    get_int (&sp->pro_gain);
		    break;
		case 24:	/* CLOCK_ITER */
		    get_int (&sp->clock_iter);
		    break;
		case 25:	/* FDERROR_MAX */
		    get_float (&sp->fderror_max);
		    break;
		case 26:	/* FDAERROR_MAX */
		    get_float (&sp->fdaerror_max);
		    break;
		case 27:	/* FDBERROR_MAX */
		    get_float (&sp->fdberror_max);
		    break;
		case 28:	/* FDC_MAX */
		    get_float (&sp->fdc_max);
		    break;
		case 29:	/* GAIN_REF */
		    get_float (&sp->gain_ref);
		    break;
		case 30:	/* LMEAN_TOL */
		    get_float (&sp->lmean_tol);
		    break;
		case 31:	/* LSTDV_DIFF_MAX */
		    get_float (&sp->lstdv_diff_max);
		    break;
		case 32:	/* BLK_SKIP */
		    get_int (&sp->blkskip);
		    break;
		case 33:	/* SCALE_THRESHOLD */
		    get_int (&sp->scale_threshold);
		    break;
		case 34:	/* SBM */
		    get_int (&sp->sbm);
		    break;
		case 35:	/* FSM */
		    get_int (&sp->fsm);
		    break;
		case 36:	/* MAXFSDO */
		    get_int (&sp->maxfsdo);
		    break;
		case 37:	/* NOISE_FACTOR_A1 */
		    get_float (&sp->noise_fctr);
		    break;
		case 38:	/* LINEAR_FACTOR_A2 */
		    get_float (&sp->linear_fctr);
		    break;
		case 39:	/* OFFSET_FACTOR_A3 */
		    get_float (&sp->offset_fctr);
		    break;
		case 40:	/* PEAK_REF */
		    get_float (&sp->peak_ref);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter %s in %s file\n",
				t,filename);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << (i % 31);
		ibyte = i / 31;
		if (fields_in[ibyte] & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in[ibyte] |= ibit;
	    }
	}  /* for n */
    /* check for missing parameters */
	if ((fields_in[0] != fields_exp[0]) || 
			(fields_in[1] != fields_exp[1])) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << (i % 31);
		ibyte = i / 31;
		if ((fields_in[ibyte] & ibit) == 0)
		    printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    ans = FAIL;
	}
	close_file();
	return (ans);
}


/* p_get_sp_file (filename,sp) -----------------------------------------
	This routine reads the sensor parameters file, and stores the 
	data in the structure pointed to by sp.
*/

p_get_sp_file (filename,sp)
	char *filename;
	SP_FILE_PTR sp;
{
	char t[100];
	static char *ptab[] = { "SAT",
				"XLAMBDA",
				"BEAM_EL",
				"BEAM_AZ",
				"ANT_LEN",
				"SIDE",
				"GAMMA_NOM",
				"CSR",
				"INCL",
				"ECC",
				"BW_RA",
				"TAU",
				"NBITS",
				"EATT_ROLL",
				"EATT_YAW",
				"EATT_PITCH",
				"EATTRT_ROLL",
				"EATTRT_YAW",
				"EATTRT_PITCH",
				"XLOCATION",
				"X3DB",
				"XPSLR",
				"XISLR",
				"PBW",
				"DBW",
				"SRSPACE",
				"GRSPACE",
				"MINSNR",
				"EST_NOISE_FLOOR",
				"NOISE",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, n, ibit, fields_exp, fields_in;
	int ans = PASS;

    /* open the file */
	if (open_file(filename,"") == 0) {
	    printf("cannot open %s\n",filename);
	    return (FAIL);
	}
    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp = 0x7fffffff >> (31 - ptablen);
	fields_in = 0;

    /* read the file & store the parameters */
	for (n = next_token(t); n > 0; n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++) {
		if (strcmp(t,ptab[i]) == 0)
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* SAT */
		    next_token (t);
/*
		    strncpy (sp->sat,t,2);
*/
		    strncpy (sp->sat,Cur_Rqst->take_id,2);
		    sp->sat[2] = '\0';
		    break;
		case 1:		/* XLAMBDA */
		    get_double (&sp->xlambda);
		    break;
		case 2:		/* BEAM_EL */
		    get_float (&sp->beam_el);
		    break;
		case 3:		/* BEAM_AZ */
		    get_float (&sp->beam_az);
		    break;
		case 4:		/* ANT_LEN */
		    get_float (&sp->ant_len);
		    break;
		case 5:		/* SIDE */
		    next_token (t);
		    sp->side = t[0];
		    break;
		case 6:		/* GAMMA_NOM */
		    get_float (&sp->gamma_nom);
		    break;
		case 7:		/* CSR */
		    get_double (&sp->csr);
		    break;
		case 8:		/* INCL */
		    get_float (&sp->incl);
		    break;
		case 9:		/* ECC */
		    get_float (&sp->ecc);
		    break;
		case 10:	/* BW_RA */
		    get_float (&sp->bw_ra);
		    break;
		case 11:	/* TAU */
		    get_float (&sp->tau);
		    break;
		case 12:	/* NBITS */
		    get_int (&sp->nbits);
		    break;
		case 13:	/* EATT_ROLL */
		    get_float (&sp->eatt.roll);
		    break;
		case 14:	/* EATT_YAW) */
		    get_float (&sp->eatt.yaw);
		    break;
		case 15:	/* EATT_PITCH */
		    get_float (&sp->eatt.pitch);
		    break;
		case 16:	/* EATTRT_ROLL */
		    get_float (&sp->eattrt.roll);
		    break;
		case 17:	/* EATTRT_YAW */
		    get_float (&sp->eattrt.yaw);
		    break;
		case 18:	/* EATTRT_PITCH */
		    get_float (&sp->eattrt.pitch);
		    break;
		case 19:	/* XLOCATION */
		    get_float (&sp->xlocation);
		    break;
		case 20:	/* X3DB */
		    get_float (&sp->x3db);
		    break;
		case 21:	/* XPSLR */
		    get_float (&sp->xpslr);
		    break;
		case 22:	/* XISLR */
		    get_float (&sp->xislr);
		    break;
		case 23:	/* PBW */
		    get_float (&sp->pbw);
		    break;
		case 24:	/* DBW */
		    get_float (&sp->dbw);
		    break;
		case 25:	/* SRSPACE */
		    get_double (&sp->srspace);
		    break;
		case 26:	/* GRSPACE */
		    get_double (&sp->grspace);
		    break;
		case 27:	/* MINSNR */
		    get_float (&sp->minsnr);
		    break;
		case 28:	/* EST_NOISE_FLOOR */ 
		    get_float (&sp->est_noise_floor);
		    break;
		case 29:	/* NOISE */ 
		    get_float (&sp->noise);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter: %s\n",t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for */
    /* check for missing parameters */
	if (fields_in != fields_exp) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << i;
		if ((fields_in & ibit) == 0)
		    printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    ans = FAIL;
	}
	close_file();
	return (ans);
}


/* p_get_rtf_file (filename,sp) ----------------------------------------
	This routine reads the range transfer function file, and
	stores the data in the structure pointed to by sp.
*/

p_get_rtf_file (filename,sp)
	char *filename;
	RTF_FILE_PTR sp;
{
	char t[100];
	static char *ptab[] = { "NPTS", 
				"TAU",
				"CSR",
				"A",
				"B",
				"C",
				"POWMAX",
				"AVGPOW",
				"H",
				"POWLOSS",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, n, ibit, fields_exp, fields_in;
	int ans = PASS;

    /* open the file */
	if (open_file(filename,"") == 0) {
	    printf("cannot open %s\n",filename);
	    return (FAIL);
	}
    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp = 0x7fffffff >> (31 - ptablen);
	fields_in = 0;

    /* read the file & store the parameters */
	for (n = next_token(t); n > 0 && t[0] != '$'; n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++) {
		if (strcmp(t,ptab[i]) == 0)
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* NPTS */
		    get_int (&sp->npts);
		    break;
		case 1:		/* TAU */
		    get_float (&sp->tau);
		    break;
		case 2:		/* CSR */
		    get_double (&sp->csr);
		    break;
		case 3:		/* A */
		    get_float (&sp->a);
		    break;
		case 4:		/* B */
		    get_float (&sp->b);
		    break;
		case 5:		/* C */
		    get_float (&sp->c);
		    break;
		case 6:		/* POWMAX */
		    get_float (&sp->powmax);
		    break;
		case 7:		/* AVGPOW */
		    get_float (&sp->avgpow);
		    break;
		case 8:		/* H */
		    get_float (&sp->h);
		    break;
		case 9:		/* POWLOSS */
		    get_float (&sp->powloss);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter: %s\n",t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for */
    /* check for missing parameters */
	if (fields_in != fields_exp) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << i;
		if ((fields_in & ibit) == 0)
		    printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    ans = FAIL;
	}
	close_file();
	return (ans);
}


/* p_get_em_file (filename,sp) -----------------------------------------
	This routine reads the earth model file, and stores the data 
	in the structure pointed to by sp.
*/

p_get_em_file (filename,sp)
	char *filename;
	EM_FILE_PTR sp;
{
	char t[100];
	static char *ptab[] = { "EARTH_MOD",
				"EMOD_SRC",
				"R_E",
				"F",
				"R_POLE",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, n, ibit, fields_exp, fields_in;
	int ans = PASS;

    /* open the file */
	if (open_file(filename,"") == 0) {
	    printf("cannot open %s\n",filename);
	    return (FAIL);
	}
    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp = 0x7fffffff >> (31 - ptablen);
	fields_in = 0;

    /* read the file & store the parameters */
	for (n = next_token(t); n > 0; n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++) {
		if (strcmp(t,ptab[i]) == 0)
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* EARTH_MOD */
		    toupper_sw = 0;
		    next_token (t);
		    toupper_sw = 1;
		    strncpy (sp->earth_mod,t,24);
		    sp->earth_mod[24] = '\0';
		    break;
		case 1:		/* EMOD_SRC */
		    toupper_sw = 0;
		    next_token (t);
		    toupper_sw = 1;
		    strncpy (sp->emod_src,t,24);
		    sp->emod_src[24] = '\0';
		    break;
		case 2:		/* R_E */
		    get_double (&sp->r_e);
		    break;
		case 3:		/* F */
		    get_double (&sp->f);
		    break;
		case 4:		/* R_POLE */
		    get_double (&sp->r_pole);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter: %s\n",t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for */
    /* check for missing parameters */
	if (fields_in != fields_exp) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << i;
		if ((fields_in & ibit) == 0)
		    printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    ans = FAIL;
	}
	close_file();
	return (ans);
}


/* p_get_aep_file (filename,sp) ----------------------------------------
	This routine reads the antenna elevation pattern file, and 
	stores the data in the structure pointed to by sp.
*/

p_get_aep_file (filename,sp)
	char *filename;
	AEP_FILE_PTR sp;
{
	char t[100];
	static char *ptab[] = { "NAME",
				"SRC",
				"ENTRIES",
				"STEP",
				"BEG",
				"END",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, n, ibit, fields_exp, fields_in;
	int ans = PASS;

    /* open the file */
	if (open_file(filename,"") == 0) {
	    printf("cannot open %s\n",filename);
	    return (FAIL);
	}
    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp = 0x7fffffff >> (31 - ptablen);
	fields_in = 0;

    /* read the file & store the parameters */
	for (n = next_token(t); n > 0 && t[0] != '$'; n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++) {
		if (strcmp(t,ptab[i]) == 0)
		    break;
		
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* NAME */
		    toupper_sw = 0;
		    next_token (t);
		    toupper_sw = 1;
		    strncpy (sp->name,t,24);
		    sp->name[24] = '\0';
		    break;
		case 1:		/* SRC */
		    toupper_sw = 0;
		    next_token (t);
		    toupper_sw = 1;
		    strncpy (sp->src,t,24);
		    sp->src[24] = '\0';
		    break;
		case 2:		/* ENTRIES */
		    get_int (&sp->entries);
		    break;
		case 3:		/* STEP */
		    get_float (&sp->step);
		    break;
		case 4:		/* BEG */
		    get_float (&sp->beg);
		    break;
		case 5:		/* END */
		    get_float (&sp->end);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter: %s\n",t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for */
    /* check for missing parameters */
	if (fields_in != fields_exp) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << i;
		if ((fields_in & ibit) == 0)
		    printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    ans = FAIL;
	}
	close_file();
	return (ans);
}


/* p_get_tc_file (filename,sp) -----------------------------------------
	This routine reads the time correlation file, and stores the
	data in the structure pointed to by sp.
*/

p_get_tc_file (filename,sp)
	char *filename;
	TC_FILE_PTR sp;
{
	char t[100];
	static char *ptab[] = { "REV",
				"GMT",
				"STIME",
				"DELTA",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, j, n, nn, vparam_id, ibit, fields_exp, fields_in;
	int ans = PASS;
	char *malloc();

    /* open the file */
	if (open_file(filename,"") == 0) {
	    printf("cannot open %s\n",filename);
	    return (FAIL);
	}
    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp = 0x7fffffff >> (31 - ptablen);
	fields_in = 0;

    /* read the file & store the parameters */
	for (n = next_token(t); (n > 0) && (strcmp(t,"END.")); 
		n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++)
		if (strcmp(t,ptab[i]) == 0) {
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* REV */
		    get_int(&sp->rev);
		    break;
		case 1:		/* GMT */
		    set_separators("=:");
		    get_int(&sp->gmt.yr);
		    get_int(&sp->gmt.day);
		    get_int(&sp->gmt.hr);
		    get_int(&sp->gmt.min);
		    get_float(&sp->gmt.second);
		    set_separators("=");
		    break;
		case 2:		/* STIME */
		    get_int(&sp->bt);
		    break;
		case 3:		/* DELTA */
		    get_int(&sp->delta);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter: %s\n",t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for */
    /* check for missing parameters */
	if (fields_in != fields_exp) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << i;
		if ((fields_in & ibit) == 0)
		    printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    ans = FAIL;
	}
	close_file();
	return (ans);
}


/* p_rw_sv_file (iomode,filename) --------------------------------------
	This routine reads (iomode = 0) or writes (iomode = 1) the 
	one-minute statevector file.  The data is stored in memory in
	the stv[] array.
*/

p_rw_sv_file (iomode,filename)
	int iomode;
	char *filename;
{
	char t[100];
	static char *ptab[] = { "START_GMT",
				"STATEVECTOR",
				"X_POS",
				"Y_POS",
				"Z_POS",
				"X_VEL",
				"Y_VEL",
				"Z_VEL",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, j, n, s, vparam_id, ibit, fields_exp, fields_in;
	FILE *op;
	int ans = PASS;
	SV_PTR vp, vp2;
	char *malloc();

    /* if iomode=1 (writing), write the output block */
	if (iomode) {
	    if ((op = fopen(filename,"w")) == NULL) {
		printf ("Cannot open output file %s\n",filename);
		return (FAIL);
	    }
	    fprintf(op,"%s = %d:%3.3d:%2.2d:%2.2d:%g\n",ptab[0],
			stv_gmt.yr, stv_gmt.day, stv_gmt.hr,
			stv_gmt.min, stv_gmt.second);
	    for (i = 0; i < nstv; i++) {
		fprintf(op,"%s = %d\n",ptab[1],i+1);
		fprintf(op,"  %s = %.12g\n",ptab[2],stv[6*i  ]);
		fprintf(op,"  %s = %.12g\n",ptab[3],stv[6*i+1]);
		fprintf(op,"  %s = %.12g\n",ptab[4],stv[6*i+2]);
		fprintf(op,"  %s = %.12g\n",ptab[5],stv[6*i+3]);
		fprintf(op,"  %s = %.12g\n",ptab[6],stv[6*i+4]);
		fprintf(op,"  %s = %.12g\n",ptab[7],stv[6*i+5]);
	    }
	    fprintf(op,"END.\n");
	    fclose(op);
	    return (PASS);
	}

    /* reading (iomode=0) */
    /* open the file */
	if (open_file(filename,"") == 0) {
	    return (FAIL);
	}
    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp = 0x7fffffff >> (31 - ptablen);
	fields_in = 0;

    /* initialize the statevector table */
	nstv = 0;

    /* read the file & store the parameters */
	for (n = next_token(t); (n > 0) && (strcmp(t,"END.")); 
		n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++)
		if (strcmp(t,ptab[i]) == 0) {
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* START_GMT */
		    set_separators("=:");
		    get_int(&stv_gmt.yr);
		    get_int(&stv_gmt.day);
		    get_int(&stv_gmt.hr);
		    get_int(&stv_gmt.min);
		    get_float(&stv_gmt.second);
		    set_separators("=");
		    break;
		case 1:		/* STATEVECTOR */
		    if (nstv) {
		    /* check for missing parameters */
			if (fields_in != fields_exp) {
			    for (i = 0; i < ptablen; i++) {
				ibit = 1 << i;
				if ((fields_in & ibit) == 0)
				    printf ("missing parameter %s in %s file\n",
						ptab[i],filename);
			    }  /* for i */
			    ans = FAIL;
			}
		    }
		    fields_in = 1;
		    s = nstv++;
		    break;
		case 2:		/* X_POS */
		    get_double(&stv[s*6  ]);
		    break;
		case 3:		/* Y_POS */
		    get_double(&stv[s*6+1]);
		    break;
		case 4:		/* Z_POS */
		    get_double(&stv[s*6+2]);
		    break;
		case 5:		/* X_VEL */
		    get_double(&stv[s*6+3]);
		    break;
		case 6:		/* Y_VEL */
		    get_double(&stv[s*6+4]);
		    break;
		case 7:		/* Z_VEL */
		    get_double(&stv[s*6+5]);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter: %s\n",t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for */
	close_file();
	return (ans);
}


/* p_get_init_dops(pp) -------------------------------------------------
	This routine loads the default initial doppler parameters
	into the given preprocessing region, if the default file
	exists.  If not, the previous doppler values are left intact.
*/

p_get_init_dops(pp)
	PP_BLOCK_PTR pp;
{
	char t[100];
	char filename[80];
	static char *ptab[] = { "FDA",
				"FDB",
				"FDC",
				"FDOTA",
				"FDOTB",
				"FDOTC",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, j, n, nn, vparam_id, ibit, fields_exp, fields_in;
	float fda,fdb,fdc,fdota,fdotb,fdotc;
	int ans = PASS;
	SV_PTR vp, vp2;
	char *malloc();

    /* open the file */
	sprintf(filename,"%sdops.%5.5s",DEF_PATH,&Cur_Rqst->take_id[5]);
	if (open_file(filename,"") == 0) {
	    return (FAIL);
	}
    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp = 0x7fffffff >> (31 - ptablen);
	fields_in = 0;

    /* read the file & store the parameters */
	for (next_token(t); strcmp(t,";") != 0; next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++)
		if (strcmp(t,ptab[i]) == 0) {
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* FDA */
		    get_float (&fda);
		    break;
		case 1:		/* FDB */
		    get_float (&fdb);
		    break;
		case 2:		/* FDC */
		    get_float (&fdc);
		    break;
		case 3:		/* FDOTA */
		    get_float (&fdota);
		    break;
		case 4:		/* FDOTB */
		    get_float (&fdotb);
		    break;
		case 5:		/* FDOTC */
		    get_float (&fdotc);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter %s in %s file\n",
				t,filename);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    printf("  duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for n */
    /* check for missing parameters */
	if (fields_in != fields_exp) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << i;
		if ((fields_in & ibit) == 0)
		    printf ("  missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    ans = FAIL;
	}  /* if fields_in */
	close_file();
	if (ans == PASS) {
	    pp->fda = fda;
	    pp->fdb = fdb;
	    pp->fdc = fdc;
	    pp->fdota = fdota;
	    pp->fdotb = fdotb;
	    pp->fdotc = fdotc;
	}
	return (ans);
}


/* p_init_pp_block(pp) -------------------------------------------------
	This routine initializes the preprocessing block pp.  The 
	default processing parameters file is used to initialize 
	as much as possible; the rest is set to zero.
*/

p_init_pp_block(pp)
	PP_BLOCK_PTR pp;
{
	PP_BLOCK_PTR ppsave;
    /* start with zeroes everywhere */
	ppsave = pp->nxt_ppb;
	zero_bytes(pp,sizeof(PP_BLOCK));
	pp->nxt_ppb = ppsave;
    /* get what we can from the default processing parameters file */
	pp->fftsc_raf = dpf.fftsc_raf;
	pp->fftsc_rai = dpf.fftsc_rai;
	pp->bw_az4_act = dpf.bw_az4;
	pp->fftsc_az4f = dpf.fftsc_az4f;
	pp->fftsc_az1f = dpf.fftsc_az1f;
	pp->fftsc_az4i = dpf.fftsc_az4i;
	pp->fftsc_az1i = dpf.fftsc_az1i;
	pp->det_sc = dpf.det_sc;
	pp->h_az = dpf.h_az;
	pp->pro_gain = dpf.pro_gain;
    /* get earth model stuff */
	pp->r_e = emf.r_e;
	pp->f = emf.f;
    /* set a few things to initial guesses */
	pp->polarity = 1;
	pp->dly_chg = 1000000;
	strcpy(pp->mcv_file,"BYPASS");
	strcpy(pp->img_name,"*NONE*");
}


/* p_rw_sv (iomode,sp,op) ----------------------------------------------
	This routine reads (iomode = 0) or writes (iomode > 0) the 
	statevector part of a file.  If writing, iomode gives the
	number of spaces to indent.
*/

p_rw_sv (iomode,sp,op)
	int iomode;
	SV_PTR sp;
	FILE *op;
{
	char t[100];
	static char *ptab[] = { "REV",
				"POS",
				"VEL",
				"GMT",
				"STIME",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, n, ibit, fields_exp, fields_in;
	int ans = PASS;
	char c[30];

    /* iomode = 1 means we are writing the data */
	if (iomode) {
	    for (i = 0; i < iomode; i++)
		c[i] = ' ';
	    c[i] = '\0';
	    fprintf(op,"%s%s = %d\n",c,ptab[0],sp->rev);
	    fprintf(op,"%s%s = %.12g %.12g %.12g\n",c,ptab[1],
			sp->pos.x,sp->pos.y,sp->pos.z);
	    fprintf(op,"%s%s = %.12g %.12g %.12g\n",c,ptab[2],
			sp->vel.x,sp->vel.y,sp->vel.z);
	    fprintf(op,"%s%s = %d:%3.3d:%2.2d:%2.2d:%g\n",c,ptab[3],
			sp->gmt.yr,sp->gmt.day,sp->gmt.hr,
			sp->gmt.min,sp->gmt.second);
	    fprintf(op,"%s%s = %d\n",c,ptab[4],sp->time);
	    fprintf(op,"%s;\n",c+1);
	    return (PASS);
	}

    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp = 0x7fffffff >> (31 - ptablen);
	fields_in = 0;

    /* initialize the structure */
	sp->nxt_sv = NULL;

    /* read the statevector parameters */
	for (n = next_token(t); n > 0 && t[0] != ';';
		    n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++) {
		if (strcmp(t,ptab[i]) == 0)
		    break;
	    }
	/* read and store the parameter values */
	    switch (i) {
		case 0:		/* REV */
		    get_int(&sp->rev);
		    break;
		case 1:		/* POS */
		    set_separators("= ");
		    get_double(&sp->pos.x);
		    get_double(&sp->pos.y);
		    get_double(&sp->pos.z);
		    set_separators("=");
		    break;
		case 2:		/* VEL */
		    set_separators("= ");
		    get_double(&sp->vel.x);
		    get_double(&sp->vel.y);
		    get_double(&sp->vel.z);
		    set_separators("=");
		    break;
		case 3:		/* GMT */
		    set_separators("=:");
		    get_int(&sp->gmt.yr);
		    get_int(&sp->gmt.day);
		    get_int(&sp->gmt.hr);
		    get_int(&sp->gmt.min);
		    get_float(&sp->gmt.second);
		    set_separators("=");
		    break;
		case 4:		/* STIME */
		    get_int(&sp->time);
		    break;
		default:		/* unknown param */
		    printf("unrecognizeable parameter: %s\n",t);
		    ans = FAIL;
	    }  /* switch */
	    flush_line();
	}  /* for n */
	return (ans);
}


/* p_rw_pp_file (iomode, filename, opt) --------------------------------
	This routine reads (iomode = 0) or writes (iomode = 1) the 
	preprocessing data file.  If opt=0, the "cannot open file"
	error message is suppressed.
*/

p_rw_pp_file (iomode, filename, opt)
	int iomode,opt;
	char *filename;
{
	TAPE_SEG_PTR sp, sp2;
	PP_BLOCK_PTR pp, pp2;
	char t[100];
	static char *ptab[] = { "TAPE_SEGMENT",
				"PP_REGION",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i;
	int ans = PASS;
	char *malloc();
	FILE *op;

    /* if iomode=1 (writing), write the output file */
	if (iomode) {
	if (vbose) printf("Writing %s file...\n",filename);
	/* open the output file */
	    if ((op = fopen(filename,"w")) == NULL) {
		printf ("Cannot open output file %s\n",filename);
		return (FAIL);
	    }
	    for (sp = seg_list; sp != NULL; sp = sp->nxt_seg) {
		fprintf(op,"%s\n",ptab[0]);
		p_rw_seg_block(1,sp,filename,op);
		for (pp = sp->ppb_list; pp != NULL; pp = pp->nxt_ppb) {
		    fprintf(op,"%s\n",ptab[1]);
		    p_rw_pp_block(1,pp,filename,op);
		}  /* for pp */
	    }  /* for sp */
	    fprintf(op,"END.\n");
	    fclose(op);
	    return (PASS);
	}  /* if iomode */

    /* iomode=0 (reading) */

    /* clear out any old preprocessing data */
	p_clear_seg_list();
	sp2 = NULL;

    /* open the file */
	if (open_file(filename,"") == 0) {
	    if (opt)
		printf("cannot open %s\n",filename);
	    return (FAIL);
	}
    /* set up parser parameters */
	set_separators(" ");
	toupper_sw = 1;

    /* read the file & store the parameters */
	if (vbose) printf("Reading %s file...\n",filename);
	for (next_token(t); strcmp(t,"END.") != 0; next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++){
		if (strcmp(t,ptab[i]) == 0) 
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* TAPE_SEGMENT */
		    flush_line();
		    sp = (TAPE_SEG_PTR) malloc(sizeof(TAPE_SEG));
		    sp->agc_count = 0;
		    sp->aux.beam_seq = 0;
		    if (sp == NULL) {
			printf("out of memory in p_rw_pp_file\n");
			return (FAIL);
		    }
		    if (p_rw_seg_block(0,sp,filename,op) == FAIL)
			ans = FAIL;
		    if (sp2 == NULL)
			seg_list = sp;
		    else
			sp2->nxt_seg = sp;
		    sp2 = sp;
		    sp->nxt_seg = NULL;
		    pp2 = sp->ppb_list = NULL;
		    break;
		case 1:		/* PP_REGION */
		    flush_line(); 
		    pp = (PP_BLOCK_PTR) malloc(sizeof(PP_BLOCK));
		    if (sp == NULL) {
			printf("out of memory in p_rw_pp_file\n");
			return (FAIL);
		    }
		    if (p_rw_pp_block(0,pp,filename,op) == FAIL)
			ans = FAIL;
		    if (sp2 == NULL) {
			printf("%s before first %s in file %s\n",
				ptab[1],ptab[0],filename);
			ans = FAIL;
			break;
		    }
		    if (pp2 == NULL)
			sp2->ppb_list = pp;
		    else
			pp2->nxt_ppb = pp;
		    pp2 = pp;
		    pp->nxt_ppb = NULL;
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter in %s: %s\n",
				filename,t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	}  /* for next_token */
	close_file();
	return (ans);
}


/* p_clear_seg_list() --------------------------------------------------
	This routine clears the tape segment list, releasing all
	dynamically allocated blocks back to the system.
*/

p_clear_seg_list()
{
	TAPE_SEG_PTR sp, sp2;
	PP_BLOCK_PTR pp, pp2;
	void free();

	for (sp = seg_list; sp != NULL; sp = sp2) {
	    sp2 = sp->nxt_seg;
	    for (pp = sp->ppb_list; pp != NULL; pp = pp2) {
		pp2 = pp->nxt_ppb;
		free (pp);
	    }  /* for pp */
	    free (sp);
	}  /* for sp */
	if (save_seg_list != NULL) {
	    seg_list = save_seg_list;
	    save_seg_list = NULL;
	    p_clear_seg_list();
	}
	seg_list = NULL;
	p_clear_pream_list();
	p_clear_postam_list();
}



/* p_rw_seg_block (iomode,sp,filename,op) ------------------------------
	This routine reads (iomode = 0) or writes (iomode = 1) one 
	tape segment data block.

   2/3/92, Ming add 4 more variables in SP to read/write region OFFSET etc.
   6/3/92, Ming add 3 more variables in SP to read/write AGC.
*/

p_rw_seg_block (iomode,sp,filename,op)
	int iomode;
	TAPE_SEG_PTR sp;
	char *filename;
	FILE *op;
{
	char t[100];
	static char *ptab[] = { "PREAMBLE",
				"POSTAMBLE",
				"FMT_START",
				"FMT_ID",
				"FMT_END",
				"END_ID",
				"BLK_START",
				"BIT_OFF",
				"POLARITY",
				"GAP_TYPE",
				"BLK_END",
				"GMT_START",
				"GMT_END",
				"WINDOW_COUNT",
				"WFORMATS",
				"WPOSITIONS",
				"AGC_COUNT",
				"AGC_FORMATS",
				"AGC_POSITIONS",
				"PPTOTAL",
				"PPFMT",
				"PPBLK",
				"PPOFF",
				"BEAM_SEQ",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, j, ibit, fields_exp, fields_in;
	int ans = PASS;

    /* if iomode=1 (writing), write the output block */
	if (iomode) {
	    fprintf(op," %s = %d\n",ptab[0],sp->prenum);
	    fprintf(op," %s = %d\n",ptab[1],sp->postnum);
	    fprintf(op," %s = %d\n",ptab[2],sp->fmt_start);
	    fprintf(op," %s = %d\n",ptab[3],sp->fmt_id);
	    fprintf(op," %s = %d\n",ptab[4],sp->fmt_end);
	    fprintf(op," %s = %d\n",ptab[5],sp->end_id);
	    fprintf(op," %s = %d\n",ptab[6],sp->blk_start);
	    fprintf(op," %s = %d\n",ptab[7],sp->bit_off);
	    fprintf(op," %s = %d\n",ptab[8],sp->polarity);
	    fprintf(op," %s = %d\n",ptab[9],sp->gap_type);
	    fprintf(op," %s = %d\n",ptab[10],sp->blk_end);
	    fprintf(op," %s = %d:%3.3d:%2.2d:%2.2d:%g\n",ptab[11],
			sp->start.yr,sp->start.day,sp->start.hr,
			sp->start.min,sp->start.second);
	    fprintf(op," %s = %d:%3.3d:%2.2d:%2.2d:%g\n",ptab[12],
			sp->end.yr,sp->end.day,sp->end.hr,
			sp->end.min,sp->end.second);
	    fprintf(op," %s = %d\n",ptab[13],sp->win_count);
	    fprintf(op," %s =",ptab[14]);
	    for (i = 0; i < sp->win_count; i++)
		fprintf(op," %7d",sp->wfmt[i]);
	    fprintf(op,"\n");
	    fprintf(op," %s =",ptab[15]);
	    for (i = 0; i < sp->win_count; i++)
		fprintf(op," %7d",sp->wdwp[i]);
	  if ( Cur_Rqst->take_id[0] == 'J' || 
		Cur_Rqst->take_id[0] == 'R' ) {
	    fprintf(op,"\n");
	    fprintf(op," %s = %d\n",ptab[16],sp->agc_count);
	    fprintf(op," %s   =",ptab[17]);
	    for (i = 0; i < sp->agc_count; i++)
		fprintf(op," %7d",sp->afmt[i]);
	    fprintf(op,"\n");
	    fprintf(op," %s =",ptab[18]);
	    for (i = 0; i < sp->agc_count; i++)
		fprintf(op," %7d",sp->agc[i]);
	    fprintf(op,"\n");
	    fprintf(op," %s = %d\n",ptab[23],sp->aux.beam_seq);
	  } /* if 'J' or 'R' */
	  if ( Cur_Rqst->take_id[0] == 'J' ){ 
	   if (cp_flag) {        /* fill in 0 in pp_regions1 */
	    fprintf(op," %s = %d\n",ptab[19],0);
	    fprintf(op," %s =",ptab[20]);
	    fprintf(op," %8d\n",0);
	    fprintf(op," %s =",ptab[21]);
	    fprintf(op," %8d\n",0);
	    fprintf(op," %s =",ptab[22]);
	    fprintf(op," %8d",0);
	   }
	   else {   		/* get from pp_regions0 */
	    fprintf(op," %s = %d\n",ptab[19],sp->pptotal);
	    fprintf(op," %s =",ptab[20]);
	    for (i = 0; i < sp->pptotal; i++)
		fprintf(op," %8d",sp->ppfmt[i]);
	    fprintf(op,"\n");
	    fprintf(op," %s =",ptab[21]);
	    for (i = 0; i < sp->pptotal; i++)
		fprintf(op," %8d",sp->ppblk[i]);
	    fprintf(op,"\n");
	    fprintf(op," %s =",ptab[22]);
	    for (i = 0; i < sp->pptotal; i++)
		fprintf(op," %8d",sp->ppoff[i]);
	   }
	  } /* if 'J' */

/* hardcode to 0 in pp_regions1 for JERS-1 when interface with CP 3/1/96 CV

	  if ( Cur_Rqst->take_id[0] == 'J' ){ 
	    fprintf(op," %s = %d\n",ptab[19],sp->pptotal);
	    fprintf(op," %s =",ptab[20]);
	    for (i = 0; i < sp->pptotal; i++)
		fprintf(op," %8d",sp->ppfmt[i]);
	    fprintf(op,"\n");
	    fprintf(op," %s =",ptab[21]);
	    for (i = 0; i < sp->pptotal; i++)
		fprintf(op," %8d",sp->ppblk[i]);
	    fprintf(op,"\n");
	    fprintf(op," %s =",ptab[22]);
	    for (i = 0; i < sp->pptotal; i++)
		fprintf(op," %8d",sp->ppoff[i]);
	  } 
*/
	    fprintf(op,"\n ;\n");
	    return (PASS);
	}  /* if iomode */

    /* iomode = 0 (reading) */

    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp = 0x7fffffff >> (31 - ptablen);
	fields_in = 0;

    /* read the file & store the parameters */
	for (next_token(t); strcmp(t,";") != 0; next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++)
		if (strcmp(t,ptab[i]) == 0) {
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* PREAMBLE */
		    get_int (&sp->prenum);
		    break;
		case 1:		/* POSTAMBLE */
		    get_int (&sp->postnum);
		    break;
		case 2:		/* FMT_START */
		    get_int (&sp->fmt_start);
		    break;
		case 3:		/* FMT_ID */
		    get_int (&sp->fmt_id);
		    break;
		case 4:		/* FMT_END */
		    get_int (&sp->fmt_end);
		    break;
		case 5:		/* END_ID */
		    get_int (&sp->end_id);
		    break;
		case 6:		/* BLK_START */
		    get_int (&sp->blk_start);
		    break;
		case 7:		/* BIT_OFF */
		    get_int (&sp->bit_off);
		    break;
		case 8:		/* POLARITY */
		    get_int (&sp->polarity);
		    break;
		case 9:		/* GAP_TYPE */
		    get_int (&sp->gap_type);
		    break;
		case 10:	/* BLK_END */
		    get_int(&sp->blk_end);
		    break;
		case 11:	/* GMT_START */
		    set_separators("=:");
		    get_int(&sp->start.yr);
		    get_int(&sp->start.day);
		    get_int(&sp->start.hr);
		    get_int(&sp->start.min);
		    get_float(&sp->start.second);
		    set_separators("=");
		    break;
		case 12:	/* GMT_END */
		    set_separators("=:");
		    get_int(&sp->end.yr);
		    get_int(&sp->end.day);
		    get_int(&sp->end.hr);
		    get_int(&sp->end.min);
		    get_float(&sp->end.second);
		    set_separators("=");
		    break;
		case 13:	/* WINDOW_COUNT */
		    get_int(&sp->win_count);
		    break;
		case 14:	/* WFORMATS */
		    set_separators("= ");
		    for (j = 0; j < sp->win_count; j++)
			get_int(sp->wfmt+j);
		    set_separators("=");
		    break;
		case 15:	/* WPOSITIONS */
		    set_separators("= ");
		    for (j = 0; j < sp->win_count; j++)
			get_int(sp->wdwp+j);
		    set_separators("=");
		    break;
		case 16:	/* AGC_COUNT */
		    get_int(&sp->agc_count);
		    break;
		case 17:	/* AGC_FORMATS */
		    set_separators("= ");
		    for (j = 0; j < sp->agc_count; j++)
			get_int(sp->afmt+j);
		    set_separators("=");
		    break;
		case 18:	/* AGC_POSITIONS */
		    set_separators("= ");
		    for (j = 0; j < sp->agc_count; j++)
			get_int(sp->agc+j);
		    set_separators("=");
		    break;
		case 19:	/* PPTOTAL */
		    get_int(&sp->pptotal);
		    break;
		case 20:	/* PPFMT */
		    set_separators("= ");
		    for (j = 0; j < sp->pptotal; j++)
			get_int(sp->ppfmt+j);
		    set_separators("=");
		    break;
		case 21:	/* PPBLK */
		    set_separators("= ");
		    for (j = 0; j < sp->pptotal; j++)
			get_int(sp->ppblk+j);
		    set_separators("=");
		    break;
		case 22:	/* PPOFF */
		    set_separators("= ");
		    for (j = 0; j < sp->pptotal; j++)
			get_int(sp->ppoff+j);
		    set_separators("=");
		    break;
		case 23:	/* BEAM_SEQ */
		    get_int(&sp->aux.beam_seq);
		    break;
		default:		/* unrecognizeable parameter */
		    if (vbose) printf("unrecognizeable parameter in %s: %s\n",
				filename,t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for */
    /* check for missing parameters */
	if (fields_in != fields_exp) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << i;
		if ((fields_in & ibit) == 0)
		    if (vbose) printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    /*
	    ans = FAIL;
	    */
	}
	return (ans);
}



/* p_rw_pp_block (iomode,sp,filename,op) -------------------------------
	This routine reads (iomode = 0) or writes (iomode = 1) one
	preprocessing region data block.
*/

p_rw_pp_block (iomode,sp,filename,op)
	int iomode;
	PP_BLOCK_PTR sp;
	char *filename;
	FILE *op;
{
	char t[100];
	static char *ptab[] = { "FMT_START",
				"FMT_ID",
				"BLK_START",
				"BIT_OFF",
				"POLARITY",
				"R_CLOSE",
				"R_MID",
				"R_FAR",
				"LOC_MID",
				"LOC_FAR",
				"STATEVECTOR",
				"ATT",
				"AGC_POS",

				"SNR",
				"BER",
				"PSLR",
				"ISLR",
				"IMEAN",
				"ISTDV",
				"IPROB",
				"QMEAN",
				"QSTDV",
				"QPROB",
				"IMEAN_COR",
				"ISTDV_COR",
				"IPROB_COR",
				"QMEAN_COR",
				"QSTDV_COR",
				"QPROB_COR",
				"IHIST_COR",
				"QHIST_COR",
				"IQREPHASE",
				"REPERROR",

				"FFTSC_RAF",
				"FFTSC_RAI",
				"BW_AZ4_ACT",
				"FFTSC_AZ4F",
				"FFTSC_AZ1F",
				"FFTSC_AZ4I",
				"FFTSC_AZ1I",
				"DET_SC",
				"H_AZ",

				"R_E",
				"F",
				"MCV_FILE",
				"MCV_MAX",
				"PRO_GAIN",
				"SAT_SPEED",
				"SWTH_SPEED",
				"TARG_SPEED",
				"VREL",
				"LAT_ROUGH",
				"LON_ROUGH",
				"FDA",
				"FDB",
				"FDC",
				"FDOTA",
				"FDOTB",
				"FDOTC",
				"QUALITY",
				"IMG_NAME",
				"PEAK",
				"NOISE",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, j, n, ibit, ibyte, fields_exp[3], fields_in[3];
	int ans = PASS;

    /* if iomode=1 (writing), write the output block */
	if (iomode) {
	    fprintf(op," %s = %d\n",ptab[0],sp->fmt_start);
	    fprintf(op," %s = %d\n",ptab[1],sp->fmt_id);
	    fprintf(op," %s = %d\n",ptab[2],sp->blk_start);
	    fprintf(op," %s = %d\n",ptab[3],sp->bit_off);
	    fprintf(op," %s = %d\n",ptab[4],sp->polarity);
	    fprintf(op," %s = %.12g\n",ptab[5],sp->r_close);
	    fprintf(op," %s = %.12g\n",ptab[6],sp->r_mid);
	    fprintf(op," %s = %.12g\n",ptab[7],sp->r_far);
	    fprintf(op," %s = %d\n",ptab[8],sp->loc_mid);
	    fprintf(op," %s = %d\n",ptab[9],sp->loc_far);
	    fprintf(op," %s\n",ptab[10]);
	    p_rw_sv(2,&sp->sv,op);
	    fprintf(op," %s = %g %g %g\n",ptab[11],
			sp->att.roll,sp->att.yaw,sp->att.pitch);
	    fprintf(op," %s = %g\n",ptab[12],sp->agc_pos);

	    fprintf(op," %s = %g\n",ptab[13],sp->snr);
	    fprintf(op," %s = %g\n",ptab[14],sp->ber);
	    fprintf(op," %s = %g\n",ptab[15],sp->pslr);
	    fprintf(op," %s = %g\n",ptab[16],sp->islr);
	    fprintf(op," %s = %g\n",ptab[17],sp->imean);
	    fprintf(op," %s = %g\n",ptab[18],sp->istdv);
	    fprintf(op," %s = %g\n",ptab[19],sp->iprob);
	    fprintf(op," %s = %g\n",ptab[20],sp->qmean);
	    fprintf(op," %s = %g\n",ptab[21],sp->qstdv);
	    fprintf(op," %s = %g\n",ptab[22],sp->qprob);
	    fprintf(op," %s = %g\n",ptab[23],sp->imean_cor);
	    fprintf(op," %s = %g\n",ptab[24],sp->istdv_cor);
	    fprintf(op," %s = %g\n",ptab[25],sp->iprob_cor);
	    fprintf(op," %s = %g\n",ptab[26],sp->qmean_cor);
	    fprintf(op," %s = %g\n",ptab[27],sp->qstdv_cor);
	    fprintf(op," %s = %g\n",ptab[28],sp->qprob_cor);
	    fprintf(op," %s =",ptab[29]);
	    for (i = 0; i < 64; i++) {
		fprintf(op," %d",sp->ihist_cor[i]);
		if (i % 8 == 7)
		    fprintf(op,"\n");
	    }
	    fprintf(op," %s =",ptab[30]);
	    for (i = 0; i < 64; i++) {
		fprintf(op," %d",sp->qhist_cor[i]);
		if (i % 8 == 7)
		    fprintf(op,"\n");
	    }
	    fprintf(op," %s = %g\n",ptab[31],sp->iqrephase);
	    fprintf(op," %s = %d\n",ptab[32],sp->reperror);

	    p_put_mask(sp->fftsc_raf,t);
	    fprintf(op," %s = %s\n",ptab[33],t);
	    p_put_mask(sp->fftsc_rai,t);
	    fprintf(op," %s = %s\n",ptab[34],t);
	    fprintf(op," %s = %g\n",ptab[35],sp->bw_az4_act);
	    p_put_mask(sp->fftsc_az4f,t);
	    fprintf(op," %s = %s\n",ptab[36],t);
	    p_put_mask(sp->fftsc_az1f,t);
	    fprintf(op," %s = %s\n",ptab[37],t);
	    p_put_mask(sp->fftsc_az4i,t);
	    fprintf(op," %s = %s\n",ptab[38],t);
	    p_put_mask(sp->fftsc_az1i,t);
	    fprintf(op," %s = %s\n",ptab[39],t);
	    fprintf(op," %s = %d\n",ptab[40],sp->det_sc);
	    fprintf(op," %s = %g\n",ptab[41],sp->h_az);

	    fprintf(op," %s = %g\n",ptab[42],sp->r_e);
	    fprintf(op," %s = %g\n",ptab[43],sp->f);
	    fprintf(op," %s = %s\n",ptab[44],sp->mcv_file);
	    fprintf(op," %s = %g\n",ptab[45],sp->mcv_max);
	    fprintf(op," %s = %d\n",ptab[46],sp->pro_gain);
	    fprintf(op," %s = %g\n",ptab[47],sp->sat_speed);
	    fprintf(op," %s = %g\n",ptab[48],sp->swth_speed);
	    fprintf(op," %s = %g\n",ptab[49],sp->targ_speed);
	    fprintf(op," %s = %g\n",ptab[50],sp->vrel);
	    fprintf(op," %s = %g\n",ptab[51],sp->lat_rough);
	    fprintf(op," %s = %g\n",ptab[52],sp->lon_rough);
	    fprintf(op," %s = %g\n",ptab[53],sp->fda);
	    fprintf(op," %s = %g\n",ptab[54],sp->fdb);
	    fprintf(op," %s = %g\n",ptab[55],sp->fdc);
	    fprintf(op," %s = %g\n",ptab[56],sp->fdota);
	    fprintf(op," %s = %g\n",ptab[57],sp->fdotb);
	    fprintf(op," %s = %g\n",ptab[58],sp->fdotc);
	    fprintf(op," %s = %d\n",ptab[59],sp->quality);
	    fprintf(op," %s = %s\n",ptab[60],sp->img_name);
	    fprintf(op," %s = %g\n",ptab[61],sp->peak);
	    fprintf(op," %s = %g\n",ptab[62],sp->noise);
	    fprintf(op," ;\n");
	    return (PASS);
	}  /* if iomode */

    /* iomode = 0 (reading) */

    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp[0] = 0x7fffffff;
	fields_exp[1] = 0x7fffffff;
	fields_exp[2] = 0x7fffffff >> (93 - ptablen);
	fields_in[0] = fields_in[1] = fields_in[2] = 0;

    /* read the file & store the parameters */
	for (next_token(t); strcmp(t,";") != 0; next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++)
		if (strcmp(t,ptab[i]) == 0) {
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* FMT_START */
		    get_int (&sp->fmt_start);
		    break;
		case 1:		/* FMT_ID */
		    get_int (&sp->fmt_id);
		    break;
		case 2:		/* BLK_START */
		    get_int (&sp->blk_start);
		    break;
		case 3:		/* BIT_OFF */
		    get_int (&sp->bit_off);
		    break;
		case 4:		/* POLARITY */
		    get_int (&sp->polarity);
		    break;
		case 5:		/* R_CLOSE */
		    get_double (&sp->r_close);
		    break;
		case 6:		/* R_MID */
		    get_double (&sp->r_mid);
		    break;
		case 7:		/* R_FAR */
		    get_double (&sp->r_far);
		    break;
		case 8:		/* LOC_MID */
		    get_int (&sp->loc_mid);
		    break;
		case 9:		/* LOC_FAR */
		    get_int (&sp->loc_far);
		    break;
		case 10:	/* STATEVECTOR */
		    flush_line();
		    p_rw_sv (0,&sp->sv,op);
		    break;
		case 11:	/* ATT */
		    set_separators("= ");
		    get_float (&sp->att.roll);
		    get_float (&sp->att.yaw);
		    get_float (&sp->att.pitch);
		    set_separators("=");
		    break;
		case 12:	/* AGC_POS */
		    get_float (&sp->agc_pos);
		    break;
		case 13:	/* SNR */
		    get_float (&sp->snr);
		    break;
		case 14:	/* BER */
		    get_float (&sp->ber);
		    break;
		case 15:	/* PSLR */
		    get_float (&sp->pslr);
		    break;
		case 16:	/* ISLR */
		    get_float (&sp->islr);
		    break;
		case 17:	/* IMEAN */
		    get_float (&sp->imean);
		    break;
		case 18:	/* ISTDV */
		    get_float (&sp->istdv);
		    break;
		case 19:	/* IPROB */
		    get_float (&sp->iprob);
		    break;
		case 20:	/* QMEAN */
		    get_float (&sp->qmean);
		    break;
		case 21:	/* QSTDV */
		    get_float (&sp->qstdv);
		    break;
		case 22:	/* QPROB */
		    get_float (&sp->qprob);
		    break;
		case 23:	/* IMEAN_COR */
		    get_float (&sp->imean_cor);
		    break;
		case 24:	/* ISTDV_COR */
		    get_float (&sp->istdv_cor);
		    break;
		case 25:	/* IPROB_COR */
		    get_float (&sp->iprob_cor);
		    break;
		case 26:	/* QMEAN_COR */
		    get_float (&sp->qmean_cor);
		    break;
		case 27:	/* QSTDV_COR */
		    get_float (&sp->qstdv_cor);
		    break;
		case 28:	/* QPROB_COR */
		    get_float (&sp->qprob_cor);
		    break;
		case 29:	/* IHIST_COR */
		    set_separators(", ");
		    for (j = 0; j < 64; j++)
			get_int (&sp->ihist_cor[j]);
		    set_separators("=");
		    break;
		case 30:	/* QHIST_COR */
		    set_separators(", ");
		    for (j = 0; j < 64; j++)
			get_int (&sp->qhist_cor[j]);
		    set_separators("=");
		    break;
		case 31:	/* IQREPHASE */
		    get_float (&sp->iqrephase);
		    break;
		case 32:	/* REPERROR */
		    get_int (&sp->reperror);
		    break;
		case 33:	/* FFTSC_RAF */
		    get_binary (&sp->fftsc_raf);
		    break;
		case 34:	/* FFTSC_RAI */
		    get_binary (&sp->fftsc_rai);
		    break;
		case 35:	/* BW_AZ4_ACT */
		    get_float (&sp->bw_az4_act);
		    break;
		case 36:	/* FFTSC_AZ4F */
		    get_binary (&sp->fftsc_az4f);
		    break;
		case 37:	/* FFTSC_AZ1F */
		    get_binary (&sp->fftsc_az1f);
		    break;
		case 38:	/* FFTSC_AZ4I */
		    get_binary (&sp->fftsc_az4i);
		    break;
		case 39:	/* FFTSC_AZ1I */
		    get_binary (&sp->fftsc_az1i);
		    break;
		case 40:	/* DET_SC */
		    get_int (&sp->det_sc);
		    break;
		case 41:	/* H_AZ */
		    get_float (&sp->h_az);
		    break;
	    /* the rest of the parameters are derived parameters */
		case 42:	/* R_E */
		    get_float (&sp->r_e);
		    break;
		case 43:	/* F */
		    get_float (&sp->f);
		    break;
		case 44:	/* MCV_FILE */
		    toupper_sw = 0;
		    next_token (sp->mcv_file);
		    toupper_sw = 1;
		    break;
		case 45:	/* MCV_MAX */
		    get_float (&sp->mcv_max);
		    break;
		case 46:	/* PRO_GAIN */
		    get_int (&sp->pro_gain);
		    break;
		case 47:	/* SAT_SPEED */
		    get_float (&sp->sat_speed);
		    break;
		case 48:	/* SWTH_SPEED */
		    get_float (&sp->swth_speed);
		    break;
		case 49:	/* TARG_SPEED */
		    get_float (&sp->targ_speed);
		    break;
		case 50:	/* VREL */
		    get_float (&sp->vrel);
		    break;
		case 51:	/* LAT_ROUGH */
		    get_float (&sp->lat_rough);
		    break;
		case 52:	/* LON_ROUGH */
		    get_float (&sp->lon_rough);
		    break;
		case 53:	/* FDA */
		    get_float (&sp->fda);
		    break;
		case 54:	/* FDB */
		    get_float (&sp->fdb);
		    break;
		case 55:	/* FDC */
		    get_float (&sp->fdc);
		    break;
		case 56:	/* FDOTA */
		    get_float (&sp->fdota);
		    break;
		case 57:	/* FDOTB */
		    get_float (&sp->fdotb);
		    break;
		case 58:	/* FDOTC */
		    get_float (&sp->fdotc);
		    break;
		case 59:	/* QUALITY */
		    get_int (&sp->quality);
		    break;
		case 60:	/* IMG_NAME */
		    next_token(sp->img_name);
		    break;
		case 61:	/* PEAK */
		    get_float (&sp->peak);
		    break;
		case 62:	/* NOISE */
		    get_float (&sp->noise);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter in %s: %s\n",
				filename,t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << (i % 31);
		ibyte = i / 31;
		if (fields_in[ibyte] & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in[ibyte] |= ibit;
	    }
	}  /* for */
    /* check for missing parameters */
	if ((fields_in[0] != fields_exp[0]) || 
			(fields_in[1] != fields_exp[1]) ||
			(fields_in[2] != fields_exp[2])) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << (i % 31);
		ibyte = i / 31;
		if ((fields_in[ibyte] & ibit) == 0)
		    printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    if ( Cur_Rqst->take_id[0] == 'E' ) {
		if ((fields_in[0] == fields_exp[0]) &&
		    (fields_in[1] == fields_exp[1])) {
		    printf("set NOISE to 2\n");
		    sp->noise = 2.;
		}
		else 
		    ans = FAIL;
	    } else ans = FAIL;
	}
	return (ans);
}


/* p_rw_pream_file(iomode,filename) ------------------------------------
	This routine reads (iomode = 0) or writes (iomode = 1) the 
	preamble data file.
*/

p_rw_pream_file (iomode,filename)
	int iomode;
	char *filename;
{
	PREAM_FILE_PTR pp;
	char t[100];
	static char *ptab[] = { "PREAMBLE",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i;
	int ans = PASS;
	FILE *op;

    /* if iomode=1 (writing), write the output file */
	if (iomode) {
	/* open the output file */
	    if ((op = fopen(filename,"w")) == NULL) {
		printf ("Cannot open output file %s\n",filename);
		return (FAIL);
	    }
	    for (pp = pream_list; pp != NULL; pp = pp->nxt_pre) {
		fprintf(op,"%s\n",ptab[0]);
		p_rw_preamble(1,pp,filename,op);
	    }
	    fprintf(op,"END.\n");
	    fclose(op);
	    return (PASS);
	}  /* if iomode */

    /* iomode=0 (reading) */

    /* clear out any old preamble data */
	p_clear_pream_list();

    /* open the file */
	if (open_file(filename,"") == 0) {
	    printf("cannot open %s\n",filename);
	    return (FAIL);
	}
    /* set up parser parameters */
	set_separators(" ");
	toupper_sw = 1;

    /* read the file & store the parameters */
	for (next_token(t); strcmp(t,"END.") != 0; next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++)
		if (strcmp(t,ptab[i]) == 0) {
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* PREAMBLE */
		    flush_line();
		    p_create_preamble(&pp);
		    if (p_rw_preamble(0,pp,filename,op) == FAIL)
			ans = FAIL;
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter in %s: %s\n",
				filename,t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	}  /* for next_token */
	close_file();
	return (ans);
}


/* p_clear_pream_list() ------------------------------------------------
	This routine clears the preamble list, releasing all
	dynamically allocated blocks back to the system.
*/

p_clear_pream_list()
{
	PREAM_FILE_PTR pp, pp2;
	void free();

	for (pp = pream_list; pp != NULL; pp = pp2) {
	    pp2 = pp->nxt_pre;
	    free (pp);
	}  /* for pp */
	pream_list = NULL;
	pream_max = 0;
}


/* p_create_preamble(ppout) --------------------------------------------
	This routine creates a new preamble data block and links it to
	the preamble list.
*/

p_create_preamble(ppout)
	PREAM_FILE_PTR *ppout;
{
	PREAM_FILE_PTR pp,pp2;
	int i = 0;
	char *malloc();

    /* allocate storage block for preamble */
	if ((pp = (PREAM_FILE_PTR) malloc(sizeof(PREAM_FILE)))
			== NULL) {
	    printf("Out of memory in p_create_preamble\n");
	    exit (1);
	}
    /* link the preamble to the end of the list */
	if (pream_list == NULL)
	    pream_list = pp;
	else {
	    i = 1;
	    for (pp2 = pream_list; pp2->nxt_pre != NULL; 
			pp2 = pp2->nxt_pre)
		i++;
	    pp2->nxt_pre = pp;
	}
	pp->nxt_pre = NULL;
	pp->prenum = i;
	pream_max = i;
	*ppout = pp;
}



/* p_rw_preamble (iomode,pp,filename,op) ----------------------------------
	This routine reads (iomode = 0) or writes (iomode = 1) one 
	preamble data block.
*/

p_rw_preamble (iomode,pp,filename,op)
	int iomode;
	PREAM_FILE_PTR pp;
	char *filename;
	FILE *op;
{
	char t[100];
	static char *ptab[] = { "SAT",
				"DCRS_ID",
				"TAKEID",
				"PRENUM",
				"SEN_MODE",
				"PRF",
				"REC_GAIN",
				"CAL_ATT",
				"NOISE_DTON",
				"CALP_DTON",
				"REPP_DTON",
				"FMT_START",
				"FMT_ID",
				"BLK_START",
				"BIT_OFF",
				"POLARITY",
				"TIME_REF_FMT",
				"TIME_REF_GMT",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, ibit, fields_exp, fields_in;
	int ans = PASS;

    /* if iomode=1 (writing), write the output block */
	if (iomode) {
	    fprintf(op," %s = %s\n",ptab[0],pp->sat);
	    fprintf(op," %s = %s\n",ptab[1],pp->dcrs_id);
	    fprintf(op," %s = %s\n",ptab[2],pp->takeid);
	    fprintf(op," %s = %d\n",ptab[3],pp->prenum);
	    fprintf(op," %s = %s\n",ptab[4],pp->sen_mode);
	    fprintf(op," %s = %.12g\n",ptab[5],pp->prf);
	    fprintf(op," %s = %d\n",ptab[6],pp->rec_gain);
	    fprintf(op," %s = %d\n",ptab[7],pp->cal_att);
	    fprintf(op," %s = %g\n",ptab[8],pp->noise_dton);
	    fprintf(op," %s = %g\n",ptab[9],pp->calp_dton);
	    fprintf(op," %s = %g\n",ptab[10],pp->repp_dton);
	    fprintf(op," %s = %d\n",ptab[11],pp->fmt_start);
	    fprintf(op," %s = %d\n",ptab[12],pp->fmt_id);
	    fprintf(op," %s = %d\n",ptab[13],pp->blk_start);
	    fprintf(op," %s = %d\n",ptab[14],pp->bit_off);
	    fprintf(op," %s = %d\n",ptab[15],pp->polarity);
	    fprintf(op," %s = %.12g\n",ptab[16],pp->time_ref_fmt);
	    fprintf(op," %s = %d:%3.3d:%2.2d:%2.2d:%g\n",ptab[17],
			pp->time_ref_gmt.yr,pp->time_ref_gmt.day,
			pp->time_ref_gmt.hr,pp->time_ref_gmt.min,
			pp->time_ref_gmt.second);
	    fprintf(op," ;\n");
	    return (PASS);
	}  /* if iomode */

    /* iomode = 0 (reading) */

    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp = 0x7fffffff >> (31 - ptablen);
	fields_in = 0;

    /* read the file & store the parameters */
	for (next_token(t); strcmp(t,";") != 0; next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++)
		if (strcmp(t,ptab[i]) == 0) {
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* SAT */
		    toupper_sw = 0;
		    next_token (pp->sat);
		    toupper_sw = 1;
		    break;
		case 1:		/* DCRS_ID */
		    toupper_sw = 0;
		    next_token (pp->dcrs_id);
		    toupper_sw = 1;
		    break;
		case 2:		/* TAKEID */
		    toupper_sw = 0;
		    next_token (pp->takeid);
		    toupper_sw = 1;
		    break;
		case 3:		/* PRENUM */
		    get_int (&pp->prenum);
		    break;
		case 4:		/* SEN_MODE */
		    toupper_sw = 0;
		    next_token (pp->sen_mode);
		    toupper_sw = 1;
		    break;
		case 5:		/* PRF */
		    get_double (&pp->prf);
		    break;
		case 6:		/* REC_GAIN */
		    get_int (&pp->rec_gain);
		    break;
		case 7:		/* CAL_ATT */
		    get_int (&pp->cal_att);
		    break;
		case 8:		/* NOISE_DTON */
		    get_float (&pp->noise_dton);
		    break;
		case 9:		/* CALP_DTON */
		    get_float (&pp->calp_dton);
		    break;
		case 10:	/* REPP_DTON */
		    get_float (&pp->repp_dton);
		    break;
		case 11:	/* FMT_START */
		    get_int (&pp->fmt_start);
		    break;
		case 12:	/* FMT_ID */
		    get_int (&pp->fmt_id);
		    break;
		case 13:	/* BLK_START */
		    get_int (&pp->blk_start);
		    break;
		case 14:	/* BIT_OFF */
		    get_int (&pp->bit_off);
		    break;
		case 15:	/* POLARITY */
		    get_int (&pp->polarity);
		    break;
		case 16:	/* TIME_REF_FMT */
		    get_double (&pp->time_ref_fmt);
		    break;
		case 17:	/* TIME_REF_GMT */
		    set_separators("=:");
		    get_int(&pp->time_ref_gmt.yr);
		    get_int(&pp->time_ref_gmt.day);
		    get_int(&pp->time_ref_gmt.hr);
		    get_int(&pp->time_ref_gmt.min);
		    get_float(&pp->time_ref_gmt.second);
		    set_separators("=");
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter in %s: %s\n",
				filename,t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for */
    /* check for missing parameters */
	if (fields_in != fields_exp) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << i;
		if ((fields_in & ibit) == 0)
		    printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    ans = FAIL;
	}
	return (ans);
}


/* p_rw_postam_file(iomode,filename) -----------------------------------
	This routine reads (iomode = 0) or writes (iomode = 1) the 
	postamble data file.
*/

p_rw_postam_file (iomode,filename)
	int iomode;
	char *filename;
{
	POSTAM_FILE_PTR pp;
	char t[100];
	static char *ptab[] = { "POSTAMBLE",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i;
	int ans = PASS;
	FILE *op;

    /* if iomode=1 (writing), write the output file */
	if (iomode) {
	/* open the output file */
	    if ((op = fopen(filename,"w")) == NULL) {
		printf ("Cannot open output file %s\n",filename);
		return (FAIL);
	    }
	    for (pp = postam_list; pp != NULL; pp = pp->nxt_post) {
		fprintf(op,"%s\n",ptab[0]);
		p_rw_postamble(1,pp,filename,op);
	    }
	    fprintf(op,"END.\n");
	    fclose(op);
	    return (PASS);
	}  /* if iomode */

    /* iomode=0 (reading) */

    /* clear out any old postamble data */
	p_clear_postam_list();

    /* open the file */
	if (open_file(filename,"") == 0) {
	    printf("cannot open %s\n",filename);
	    return (FAIL);
	}
    /* set up parser parameters */
	set_separators(" ");
	toupper_sw = 1;

    /* read the file & store the parameters */
	for (next_token(t); strcmp(t,"END.") != 0; next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++)
		if (strcmp(t,ptab[i]) == 0) {
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* POSTAMBLE */
		    flush_line();
		    p_create_postamble(&pp);
		    if (p_rw_postamble(0,pp,filename,op) == FAIL)
			ans = FAIL;
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter in %s: %s\n",
				filename,t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	}  /* for next_token */
	close_file();
	return (ans);
}


/* p_clear_postam_list() -----------------------------------------------
	This routine clears the postamble list, releasing all
	dynamically allocated blocks back to the system.
*/

p_clear_postam_list()
{
	POSTAM_FILE_PTR pp, pp2;
	void free();

	for (pp = postam_list; pp != NULL; pp = pp2) {
	    pp2 = pp->nxt_post;
	    free (pp);
	}  /* for pp */
	postam_list = NULL;
	postam_max = 0;
}


/* p_create_postamble(ppout) -------------------------------------------
	This routine creates a new postamble data block and links it to
	the postamble list.
*/

p_create_postamble(ppout)
	POSTAM_FILE_PTR *ppout;
{
	POSTAM_FILE_PTR pp,pp2;
	int i = 0;
	char *malloc();

    /* allocate storage block for postamble */
	if ((pp = (POSTAM_FILE_PTR) malloc(sizeof(POSTAM_FILE)))
			== NULL) {
	    printf("Out of memory in p_create_postamble\n");
	    exit (1);
	}
    /* link the postamble to the end of the list */
	if (postam_list == NULL)
	    postam_list = pp;
	else {
	    i = 1;
	    for (pp2 = postam_list; pp2->nxt_post != NULL; 
			pp2 = pp2->nxt_post)
		i++;
	    pp2->nxt_post = pp;
	}
	pp->nxt_post = NULL;
	pp->postnum = i;
	postam_max = i;
	*ppout = pp;
}



/* p_rw_postamble (iomode,pp,filename,op) ---------------------------------
	This routine reads (iomode = 0) or writes (iomode = 1) one 
	postamble data block.
*/

p_rw_postamble (iomode,pp,filename,op)
	int iomode;
	POSTAM_FILE_PTR pp;
	char *filename;
	FILE *op;
{
	char t[100];
	static char *ptab[] = { "TAKEID",
				"POSTNUM",
				"REC_GAIN",
				"CAL_ATT",
				"NOISE_DTOFF",
				"CALP_DTOFF",
				"REPP_DTOFF",
				"FMT_START",
				"FMT_ID",
				"BLK_START",
				"BIT_OFF",
				"POLARITY",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	int i, ibit, fields_exp, fields_in;
	int ans = PASS;

    /* if iomode=1 (writing), write the output block */
	if (iomode) {
	    fprintf(op," %s = %s\n",ptab[0],pp->takeid);
	    fprintf(op," %s = %d\n",ptab[1],pp->postnum);
	    fprintf(op," %s = %d\n",ptab[2],pp->rec_gain);
	    fprintf(op," %s = %d\n",ptab[3],pp->cal_att);
	    fprintf(op," %s = %g\n",ptab[4],pp->noise_dtoff);
	    fprintf(op," %s = %g\n",ptab[5],pp->calp_dtoff);
	    fprintf(op," %s = %g\n",ptab[6],pp->repp_dtoff);
	    fprintf(op," %s = %d\n",ptab[7],pp->fmt_start);
	    fprintf(op," %s = %d\n",ptab[8],pp->fmt_id);
	    fprintf(op," %s = %d\n",ptab[9],pp->blk_start);
	    fprintf(op," %s = %d\n",ptab[10],pp->bit_off);
	    fprintf(op," %s = %d\n",ptab[11],pp->polarity);
	    fprintf(op," ;\n");
	    return (PASS);
	}  /* if iomode */

    /* iomode = 0 (reading) */

    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp = 0x7fffffff >> (31 - ptablen);
	fields_in = 0;

    /* read the file & store the parameters */
	for (next_token(t); strcmp(t,";") != 0; next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++)
		if (strcmp(t,ptab[i]) == 0) {
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* TAKEID */
		    toupper_sw = 0;
		    next_token (pp->takeid);
		    toupper_sw = 1;
		    break;
		case 1:		/* POSTNUM */
		    get_int (&pp->postnum);
		    break;
		case 2:		/* REC_GAIN */
		    get_int (&pp->rec_gain);
		    break;
		case 3:		/* CAL_ATT */
		    get_int (&pp->cal_att);
		    break;
		case 4:		/* NOISE_DTOFF */
		    get_float (&pp->noise_dtoff);
		    break;
		case 5:		/* CALP_DTOFF */
		    get_float (&pp->calp_dtoff);
		    break;
		case 6:		/* REPP_DTOFF */
		    get_float (&pp->repp_dtoff);
		    break;
		case 7:		/* FMT_START */
		    get_int (&pp->fmt_start);
		    break;
		case 8:		/* FMT_ID */
		    get_int (&pp->fmt_id);
		    break;
		case 9:		/* BLK_START */
		    get_int (&pp->blk_start);
		    break;
		case 10:	/* BIT_OFF */
		    get_int (&pp->bit_off);
		    break;
		case 11:	/* POLARITY */
		    get_int (&pp->polarity);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter in %s: %s\n",
				filename,t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for */
    /* check for missing parameters */
	if (fields_in != fields_exp) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << i;
		if ((fields_in & ibit) == 0)
		    printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    ans = FAIL;
	}
	return (ans);
}


/* p_link_pambles() ----------------------------------------------------
	This routine is called after reading the preprocessing file and
	the preamble and postamble files.  The routine completes the
	link between each tape segment data block and its associated
	preamble and postamble data block.
*/

p_link_pambles()
{
	TAPE_SEG_PTR sp;
	PREAM_FILE_PTR pre;
	POSTAM_FILE_PTR post;
	int i;

	for (sp = seg_list; sp != NULL; sp = sp->nxt_seg) {
	    for (pre = pream_list, i = 0; pre != NULL; 
			pre = pre->nxt_pre) {
		if (i == sp->prenum)
		    break;
		i++;
	    }
	    if ((sp->pre = pre) == NULL)
		if (vbose) printf("p_link_pambles: link error on preamble\n");
	    for (post = postam_list, i = 0; post != NULL; 
			post = post->nxt_post) {
		if (i == sp->postnum)
		    break;
		i++;
	    }
	    if ((sp->post = post) == NULL)
		if (vbose) printf("p_link_pambles: link error on postamble\n");
	}
}



/* p_rw_scene_file(iomode,filename) ------------------------------------
	This routine reads (iomode = 0) or writes (iomode = 1) the
	scene file.
*/

p_rw_scene_file(iomode,filename)
	int iomode;
	char *filename;
{
	char t[100];
	static char *ptab[] = { "SW_ID",
				"FILE_NAME",
				"TME_SCENE_CTR",
				"LOC_SCENE_CTR",
				"LNUM_SCENE_CTR",
				"PNUM_SCENE_CTR",
				"SP_ID",
				"WNDW_POS",
				"IQ_GAIN_IMB",
				"BEGIN_POS",
				"BEGIN_VEL",
				"MID_POS",
				"MID_VEL",
				"END_POS",
				"END_VEL",
				"ATT_RATE",
				"IMAGE_ID",
				"LOC_A",
				"LOC_B",
				"LOC_D",
				"LOC_E",
				"NPIXELS",
				"NLINES",
				"MEDIA_LABL",
				"DCRS_START",
				"DCRS_END",
				"ALTITUDE",
				"RE_NADIR",
				"RE_IMAGE_CTR",
				"ASP_VERSION",
				"PROC_TYPE",
				"SPEC_FLAG",
				"COMMENT",
				"R_NEAR",
				"R_MID",
				"R_FAR",
				"FD_G_A",
				"FD_G_B",
				"FD_G_C",
				"FDOT_G_A",
				"FDOT_G_B",
				"FDOT_G_C",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	FILE *op;
	int i, j, n, ibit, ibyte, fields_exp[3], fields_in[3];
	int ans = PASS;

    /* if iomode=1 (writing), write the output block */
	if (iomode) {
	    if ((op = fopen(filename,"w")) == NULL) {
		printf ("Cannot open output file %s\n",filename);
		return (FAIL);
	    }
	    fprintf(op," %s = %s\n",ptab[0],sf.sw_id);
	    fprintf(op," %s = %s\n",ptab[1],sf.file_name);
	    fprintf(op," %s = %s\n",ptab[2],sf.tme_scene_ctr);
	    fprintf(op," %s = %.12g %.12g\n",ptab[3],
			sf.lat_scene_ctr,sf.lon_scene_ctr);
	    fprintf(op," %s = %.d\n",ptab[4],sf.lnum_scene_ctr);
	    fprintf(op," %s = %.d\n",ptab[5],sf.pnum_scene_ctr);
	    fprintf(op," %s = %s\n",ptab[6],sf.sp_id);
	    fprintf(op," %s = %g\n",ptab[7],sf.wndw_pos);
	    fprintf(op," %s = %g\n",ptab[8],sf.iq_gain_imb);
	    fprintf(op," %s = %.12g %.12g %.12g\n",ptab[9],
			sf.x_begin, sf.y_begin, sf.z_begin);
	    fprintf(op," %s = %.12g %.12g %.12g\n",ptab[10],
			sf.vx_begin, sf.vy_begin, sf.vz_begin);
	    fprintf(op," %s = %.12g %.12g %.12g\n",ptab[11],
			sf.x_mid, sf.y_mid, sf.z_mid);
	    fprintf(op," %s = %.12g %.12g %.12g\n",ptab[12],
			sf.vx_mid, sf.vy_mid, sf.vz_mid);
	    fprintf(op," %s = %.12g %.12g %.12g\n",ptab[13],
			sf.x_end, sf.y_end, sf.z_end);
	    fprintf(op," %s = %.12g %.12g %.12g\n",ptab[14],
			sf.vx_end, sf.vy_end, sf.vz_end);
	    fprintf(op," %s = %g %g %g\n",ptab[15],
			sf.pitch_rate,sf.roll_rate,sf.yaw_rate);
	    fprintf(op," %s = %s\n",ptab[16],sf.image_id);
	    fprintf(op," %s = %.12g %.12g\n",ptab[17],
			sf.lat_a,sf.lon_a);
	    fprintf(op," %s = %.12g %.12g\n",ptab[18],
			sf.lat_b,sf.lon_b);
	    fprintf(op," %s = %.12g %.12g\n",ptab[19],
			sf.lat_d,sf.lon_d);
	    fprintf(op," %s = %.12g %.12g\n",ptab[20],
			sf.lat_e,sf.lon_e);
	    fprintf(op," %s = %d\n",ptab[21],sf.np);
	    fprintf(op," %s = %d\n",ptab[22],sf.nl);
	    if (sf.media_labl[0] == ' ' || sf.media_labl[0] == '\0')
		fprintf(op," %s = NONE\n",ptab[23]);
	    else
		fprintf(op," %s = %s\n",ptab[23],sf.media_labl);
	    fprintf(op," %s = %d\n",ptab[24],sf.dcrs_start);
	    fprintf(op," %s = %d\n",ptab[25],sf.dcrs_end);
	    fprintf(op," %s = %g\n",ptab[26],sf.altitude);
	    fprintf(op," %s = %g\n",ptab[27],sf.re_nadir);
	    fprintf(op," %s = %g\n",ptab[28],sf.re_image_ctr);
	    fprintf(op," %s = %s\n",ptab[29],sf.asp_ver);
	    fprintf(op," %s = %s\n",ptab[30],sf.proc_type);
	    if (sf.spec_flg[0] == ' ' || sf.spec_flg[0] == '\0')
		fprintf(op," %s = NONE\n",ptab[31]);
	    else
		fprintf(op," %s = %s\n",ptab[31],sf.spec_flg);
	    if (sf.comment[0] == ' ' || sf.comment[0] == '\0')
		fprintf(op," %s = NONE\n",ptab[32]);
	    else
		fprintf(op," %s = %s\n",ptab[32],sf.comment);
	    fprintf(op," %s = %.12g\n",ptab[33],sf.r_near);
	    fprintf(op," %s = %.12g\n",ptab[34],sf.r_mid);
	    fprintf(op," %s = %.12g\n",ptab[35],sf.r_far);
	    fprintf(op," %s = %.12g\n",ptab[36],sf.fdga);
	    fprintf(op," %s = %.12g\n",ptab[37],sf.fdgb);
	    fprintf(op," %s = %.12g\n",ptab[38],sf.fdgc);
	    fprintf(op," %s = %.12g\n",ptab[39],sf.fdotga);
	    fprintf(op," %s = %.12g\n",ptab[40],sf.fdotgb);
	    fprintf(op," %s = %.12g\n",ptab[41],sf.fdotgc);
	    fclose(op);
/*	    p_aux_scene_file(1,"aux_scene_file");  */
	    p_write_mem_file(sf.i_hstgrm,512,"hstgrm",".i","histogram");
	    if (vbose) printf("...finished writing hstgrm.i\n");
	    return (PASS);
	}  /* if iomode */

    /* iomode = 0 (reading) */

    /* open the file */
	if (open_file(filename,"") == 0) {
	    printf("cannot open %s\n",filename);
	    return (FAIL);
	}
	p_init_scene_file();

    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp[0] = 0x7fffffff;
	fields_exp[1] = 0x7fffffff >> (62 - ptablen);
	fields_in[0] = fields_in[1] = fields_in[2] = 0;

    /* read the file & store the parameters */
	for (n = next_token(t); n > 0; n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++)
		if (strcmp(t,ptab[i]) == 0) {
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* SW_ID */
		    next_token(sf.sw_id);
		    break;
		case 1:		/* FILE_NAME */
		    next_token(sf.file_name);
		    break;
		case 2:		/* TME_SCENE_CTR */
		    next_token(sf.tme_scene_ctr);
		    break;
		case 3:		/* LOC_SCENE_CTR */
		    set_separators("= ");
		    get_double(&sf.lat_scene_ctr);
		    get_double(&sf.lon_scene_ctr);
		    set_separators("=");
		    break;
		case 4:		/* LNUM_SCENE_CTR */
		    get_int(&sf.lnum_scene_ctr);
		    break;
		case 5:		/* PNUM_SCENE_CTR */
		    get_int(&sf.pnum_scene_ctr);
		    break;
		case 6:		/* SP_ID */
		    next_token(sf.sp_id);
		    break;
		case 7:		/* WNDW_POS */
		    get_float(&sf.wndw_pos);
		    break;
		case 8:		/* IQ_GAIN_IMB */
		    get_float(&sf.iq_gain_imb);
		    break;
		case 9:		/* BEGIN_POS */
		    set_separators("= ");
		    get_double(&sf.x_begin);
		    get_double(&sf.y_begin);
		    get_double(&sf.z_begin);
		    set_separators("=");
		    break;
		case 10:	/* BEGIN_VEL */
		    set_separators("= ");
		    get_double(&sf.vx_begin);
		    get_double(&sf.vy_begin);
		    get_double(&sf.vz_begin);
		    set_separators("=");
		    break;
		case 11:	/* MID_POS */
		    set_separators("= ");
		    get_double(&sf.x_mid);
		    get_double(&sf.y_mid);
		    get_double(&sf.z_mid);
		    set_separators("=");
		    break;
		case 12:	/* MID_VEL */
		    set_separators("= ");
		    get_double(&sf.vx_mid);
		    get_double(&sf.vy_mid);
		    get_double(&sf.vz_mid);
		    set_separators("=");
		    break;
		case 13:	/* END_POS */
		    set_separators("= ");
		    get_double(&sf.x_end);
		    get_double(&sf.y_end);
		    get_double(&sf.z_end);
		    set_separators("=");
		    break;
		case 14:	/* END_VEL */
		    set_separators("= ");
		    get_double(&sf.vx_end);
		    get_double(&sf.vy_end);
		    get_double(&sf.vz_end);
		    set_separators("=");
		    break;
		case 15:	/* ATT_RATE */
		    set_separators("= ");
		    get_float(&sf.pitch_rate);
		    get_float(&sf.roll_rate);
		    get_float(&sf.yaw_rate);
		    set_separators("=");
		    break;
		case 16:	/* IMAGE_ID */
		    next_token(sf.image_id);
		    break;
		case 17:	/* LOC_A */
		    set_separators("= ");
		    get_double(&sf.lat_a);
		    get_double(&sf.lon_a);
		    set_separators("=");
		    break;
		case 18:	/* LOC_B */
		    set_separators("= ");
		    get_double(&sf.lat_b);
		    get_double(&sf.lon_b);
		    set_separators("=");
		    break;
		case 19:	/* LOC_D */
		    set_separators("= ");
		    get_double(&sf.lat_d);
		    get_double(&sf.lon_d);
		    set_separators("=");
		    break;
		case 20:	/* LOC_E */
		    set_separators("= ");
		    get_double(&sf.lat_e);
		    get_double(&sf.lon_e);
		    set_separators("=");
		    break;
		case 21:	/* NPIXELS */
		    get_int(&sf.np);
		    break;
		case 22:	/* NLINES */
		    get_int(&sf.nl);
		    break;
		case 23:	/* MEDIA_LABL */
		    next_token(sf.media_labl);
		    if (strcmp(sf.media_labl,"NONE") == 0)
			strcpy(sf.media_labl,"    ");
		    break;
		case 24:	/* DCRS_START */
		    get_int(&sf.dcrs_start);
		    break;
		case 25:	/* DCRS_END */
		    get_int(&sf.dcrs_end);
		    break;
		case 26:	/* ALTITUDE */
		    get_float(&sf.altitude);
		    break;
		case 27:	/* RE_NADIR */
		    get_float(&sf.re_nadir);
		    break;
		case 28:	/* RE_IMAGE_CTR */
		    get_float(&sf.re_image_ctr);
		    break;
		case 29:	/* ASP_VERSION */
		    next_token(sf.asp_ver);
		    break;
		case 30:	/* PROC_TYPE */
		    next_token(sf.proc_type);
		    break;
		case 31:	/* SPEC_FLAG */
		    next_token(t);
		    if (strcmp(t,"NONE") == 0)
			strncpy(sf.spec_flg,"    ",4);
		    else
			strncpy(sf.spec_flg,t,4);
		    break;
		case 32:	/* COMMENT */
		    next_token(sf.comment);
		    if (strcmp(sf.comment,"NONE") == 0)
			strcpy(sf.comment,"    ");
		    break;
		case 33:	/* R_NEAR */
		    get_double(&sf.r_near);
		    break;
		case 34:	/* R_MID */
		    get_double(&sf.r_mid);
		    break;
		case 35:	/* R_FAR */
		    get_double(&sf.r_far);
		    break;
		case 36:	/* FD_G_A */
		    get_double(&sf.fdga);
		    break;
		case 37:	/* FD_G_B */
		    get_double(&sf.fdgb);
		    break;
		case 38:	/* FD_G_C */
		    get_double(&sf.fdgc);
		    break;
		case 39:	/* FDOT_G_A */
		    get_double(&sf.fdotga);
		    break;
		case 40:	/* FDOT_G_B */
		    get_double(&sf.fdotgb);
		    break;
		case 41:	/* FDOT_G_C */
		    get_double(&sf.fdotgc);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter in %s: %s\n",
				filename,t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << (i % 31);
		ibyte = i / 31;
		if (fields_in[ibyte] & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in[ibyte] |= ibit;
	    }
	}  /* for */
    /* check for missing parameters */
	if ((fields_in[0] != fields_exp[0]) || 
			(fields_in[1] != fields_exp[1])) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << (i % 31);
		ibyte = i / 31;
		if ((fields_in[ibyte] & ibit) == 0)
		    printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    ans = FAIL;
	}
	close_file();
/*	if ( ans != FAIL) p_aux_scene_file(0,"aux_scene_file"); */
	if (p_get_mem_file(sf.i_hstgrm,512,"hstgrm",".i") == FAIL)
		printf("... no input histogram file\n");
	return (ans);
}

/* p_aux_scene_file(iomode,filename) -------------------------------
	This routine reads (iomode = 0) or writes (iomode = 1) the
	auxilary scene file.
*/

p_aux_scene_file(iomode,filename)
	int iomode;
	char *filename;
{
	char t[100];
	static char *ptab[] = { "FILE_NAME",
				"TRU_HD",
				"SNSR_MODE",
				"SP_LAT",
				"SP_LON",
				"SP_HD",
				"INC_SCENE_CTR",
				"RNGE_GATE",
				"REC_GAIN_LP",
				"BW_LK_RANGE",
				"BW_TTL_RANGE",
				"GMH_ANG",
				"AL_POS_ERR",
				"AC_POS_ERR",
				"RAD_POS_ERR",
				"AL_VEL_ERR",
				"AC_VEL_ERR",
				"RAD_VEL_ERR",
				"NOISE_SCL_FAC",
				"LNR_CONV_FAC",
				"OFF_CONV_FAC",
				"CAL_UPDATE",
				"AL_LOC_ERR",
				"AC_LOC_ERR",
				"LNE_DIST",
				"PXL_DIST",
				"DIST_SKEW",
				"HTGM_DSCPT",
				"HTGM_SEQ",
				"LNE_SMPL_NUM",
				"AC_SMPL_NUM",
				"LNE_GRP_SIZE",
				"AC_GRP_SIZE",
				"LNE_NUM_SMPL",
				"AC_NUM_SMPL",
				"IMIN_FRST",
				"IMAX_LAST",
				"IVAL_INC",
				"QMIN_FRST",
				"QMAX_LAST",
				"QVAL_INC",
				"NUM_SMPL_OFF",
				"NUM_RL",
				"FRST_CTR_FREQ",
				"LST_CTR_FREQ",
				"MIN_SPTRL_PWR",
				"MAX_SPTRL_PWR",
				"NP_NFLR",
				"NL_NFLR",
				"TRK_ANG",
				"ASC_DSC",
				"ROLL_QLTY",
				"YAW_QLTY",
				"PITCH_QLTY",
				"ROLL_RATE_QLTY",
				"YAW_RATE_QLTY",
				"PITCH_RATE_QLTY",
				"INC_IMAGE_CTR",
				"AZ_DIST",
				"RG_DIST",
				"SQUINT",
				"CAL_EST",
				"RAD_RES",
				"SAT_PNT",
				"RCF_AVG",
				"SNR",
				"LK_ANGLE",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
	FILE *op;
	int i, j, n, ibit, ibyte, fields_exp[3], fields_in[3];
	int ans = PASS;

    /* if iomode=1 (writing), write the output block */
	if (iomode) {
	    if ((op = fopen(filename,"w")) == NULL) {
		printf ("Cannot open output file %s\n",filename);
		return (FAIL);
	    }
	    fprintf(op," %s = %s\n",ptab[0],sf.file_name);
	    fprintf(op," %s = %g\n",ptab[1],sf.tru_hd);
	    if (sf.snsr_mode[0] == '\0')
		fprintf(op," %s = \n",ptab[2]);
	    else
		fprintf(op," %s = %s\n",ptab[2],sf.snsr_mode);
	    fprintf(op," %s = %g\n",ptab[3],sf.sp_lat);
	    fprintf(op," %s = %g\n",ptab[4],sf.sp_lon);
	    fprintf(op," %s = %g\n",ptab[5],sf.sp_hd);
	    fprintf(op," %s = %g\n",ptab[6],sf.inc_scene_ctr);
	    fprintf(op," %s = %g\n",ptab[7],sf.rnge_gate);
	    fprintf(op," %s = %g\n",ptab[8],sf.rec_gain_lp);
	    fprintf(op," %s = %g\n",ptab[9],sf.bw_lk_range);
	    fprintf(op," %s = %g\n",ptab[10],sf.bw_ttl_range);
	    fprintf(op," %s = %g\n",ptab[11],sf.gmh_ang);
	    fprintf(op," %s = %g\n",ptab[12],sf.al_pos_err);
	    fprintf(op," %s = %g\n",ptab[13],sf.ac_pos_err);
	    fprintf(op," %s = %g\n",ptab[14],sf.rad_pos_err);
	    fprintf(op," %s = %g\n",ptab[15],sf.al_vel_err);
	    fprintf(op," %s = %g\n",ptab[16],sf.ac_vel_err);
	    fprintf(op," %s = %g\n",ptab[17],sf.rad_vel_err);
	    fprintf(op," %s = %g\n",ptab[18],sf.noise_scl_fac);
	    fprintf(op," %s = %g\n",ptab[19],sf.lnr_conv_fac);
	    fprintf(op," %s = %g\n",ptab[20],sf.off_conv_fac);
	    if (sf.cal_update[0] == '\0')
		fprintf(op," %s = \n",ptab[21]);
	    else
		fprintf(op," %s = %s\n",ptab[21],sf.cal_update);
	    fprintf(op," %s = %g\n",ptab[22],sf.al_loc_err);
	    fprintf(op," %s = %g\n",ptab[23],sf.ac_loc_err);
	    fprintf(op," %s = %g\n",ptab[24],sf.lne_dist);
	    fprintf(op," %s = %g\n",ptab[25],sf.pxl_dist);
	    fprintf(op," %s = %g\n",ptab[26],sf.dist_skew);
	    if (sf.htgm_dscpt[0] == '\0')
		fprintf(op," %s = \n",ptab[27]);
	    else
		fprintf(op," %s = %s\n",ptab[27],sf.htgm_dscpt);
	    fprintf(op," %s = %d\n",ptab[28],sf.htgm_seq);
	    fprintf(op," %s = %d\n",ptab[29],sf.lne_smpl_num);
	    fprintf(op," %s = %d\n",ptab[30],sf.ac_smpl_num);
	    fprintf(op," %s = %d\n",ptab[31],sf.lne_grp_size);
	    fprintf(op," %s = %d\n",ptab[32],sf.ac_grp_size);
	    fprintf(op," %s = %d\n",ptab[33],sf.lne_num_smpl);
	    fprintf(op," %s = %d\n",ptab[34],sf.ac_num_smpl);
	    fprintf(op," %s = %d\n",ptab[35],sf.imin_frst);
	    fprintf(op," %s = %d\n",ptab[36],sf.imax_last);
	    fprintf(op," %s = %d\n",ptab[37],sf.ival_inc);
	    fprintf(op," %s = %d\n",ptab[38],sf.qmin_frst);
	    fprintf(op," %s = %d\n",ptab[39],sf.qmax_last);
	    fprintf(op," %s = %d\n",ptab[40],sf.qval_inc);
	    fprintf(op," %s = %d\n",ptab[41],sf.num_smpl_off);
	    fprintf(op," %s = %d\n",ptab[42],sf.num_rl);
	    fprintf(op," %s = %g\n",ptab[43],sf.frst_ctr_freq);
	    fprintf(op," %s = %g\n",ptab[44],sf.lst_ctr_freq);
	    fprintf(op," %s = %g\n",ptab[45],sf.min_sptrl_pwr);
	    fprintf(op," %s = %g\n",ptab[46],sf.max_sptrl_pwr);
	    fprintf(op," %s = %d\n",ptab[47],sf.np_nflr);
	    fprintf(op," %s = %d\n",ptab[48],sf.nl_nflr);
	    fprintf(op," %s = %g\n",ptab[49],sf.trk_ang);
	    if (sf.asc_dsc[0] == '\0')
		fprintf(op," %s = \n",ptab[50]);
	    else
		fprintf(op," %s = %s\n",ptab[50],sf.asc_dsc);
	    fprintf(op," %s = %d\n",ptab[51],sf.roll_qlty);
	    fprintf(op," %s = %d\n",ptab[52],sf.yaw_qlty);
	    fprintf(op," %s = %d\n",ptab[53],sf.pitch_qlty);
	    fprintf(op," %s = %d\n",ptab[54],sf.roll_rate_qlty);
	    fprintf(op," %s = %d\n",ptab[55],sf.yaw_rate_qlty);
	    fprintf(op," %s = %d\n",ptab[56],sf.pitch_rate_qlty);
	    fprintf(op," %s = %g\n",ptab[57],sf.inc_image_ctr);
	    fprintf(op," %s = %g\n",ptab[58],sf.az_dist);
	    fprintf(op," %s = %g\n",ptab[59],sf.rg_dist);
	    fprintf(op," %s = %g\n",ptab[60],sf.squint);
	    fprintf(op," %s = %g\n",ptab[61],sf.cal_est);
	    fprintf(op," %s = %g\n",ptab[62],sf.rad_res);
	    fprintf(op," %s = %d\n",ptab[63],sf.sat_pnt);
	    fprintf(op," %s = %g\n",ptab[64],sf.rcf_avg);
	    fprintf(op," %s = %g\n",ptab[65],sf.snr);
	    fprintf(op," %s = %g\n",ptab[66],sf.lk_angle);
	    fclose(op);
	    if (vbose) printf("...finished writing aux_scene_file\n");
	    return (PASS);
	}  /* if iomode */

    /* iomode = 0 (reading) */

    /* open the file */
	if (open_file(filename,"") == 0) {
	    printf("cannot open %s\n",filename);
	    return (FAIL);
	}

    /* set up parser parameters */
	set_separators("=");
	toupper_sw = 1;
	fields_exp[0] = 0x7fffffff;
	fields_exp[1] = 0x7fffffff >> (62 - ptablen);
	fields_in[0] = fields_in[1] = fields_in[2] = 0;

    /* read the file & store the parameters */
	for (n = next_token(t); n > 0; n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++)
		if (strcmp(t,ptab[i]) == 0) {
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* FILE_NAME */
		    next_token(sf.file_name);
		    break;
		case 1:		/* TRU_HD */
		    get_float(&sf.tru_hd);
		    break;
		case 2:		/* SNSR_MODE */
		    next_token(sf.snsr_mode);
		    break;
		case 3:		/* SP_LAT */
		    get_double(&sf.sp_lat);
		    break;
		case 4:		/* SP_LON */
		    get_double(&sf.sp_lon);
		    break;
		case 5:		/* SP_HD */
		    get_float(&sf.sp_hd);
		    break;
		case 6:		/* INC_SCENE_CTR */
		    get_float(&sf.inc_scene_ctr);
		    break;
		case 7:		/* RNGE_GATE */
		    get_float(&sf.rnge_gate);
		    break;
		case 8:		/* REC_GAIN_LP */
		    get_float(&sf.rec_gain_lp);
		    break;
		case 9:		/* BW_LK_RANGE */
		    get_float(&sf.bw_lk_range);
		    break;
		case 10:	/* BW_TTL_RANGE */
		    get_float(&sf.bw_ttl_range);
		    break;
		case 11:	/* GMH_ANG */
		    get_float(&sf.gmh_ang);
		    break;
		case 12:	/* AL_POS_ERR */
		    get_float(&sf.al_pos_err);
		    break;
		case 13:	/* AC_POS_ERR */
		    get_float(&sf.ac_pos_err);
		    break;
		case 14:	/* RAD_POS_ERR */
		    get_float(&sf.rad_pos_err);
		    break;
		case 15:	/* AL_VEL_ERR */
		    get_float(&sf.al_vel_err);
		    break;
		case 16:	/* AC_VEL_ERR */
		    get_float(&sf.ac_vel_err);
		    break;
		case 17:	/* RAD_VEL_ERR */
		    get_float(&sf.rad_vel_err);
		    break;
		case 18:	/* NOISE_SCL_FAC */
		    get_float(&sf.noise_scl_fac);
		    break;
		case 19:	/* LNR_CONV_FAC */
		    get_float(&sf.lnr_conv_fac);
		    break;
		case 20:	/* OFF_CONV_FAC */
		    get_float(&sf.off_conv_fac);
		    break;
		case 21:	/* CAL_UPDATE */
		    next_token(sf.cal_update);
		    break;
		case 22:	/* AL_LOC_ERR */
		    get_float(&sf.al_loc_err);
		    break;
		case 23:	/* AC_LOC_ERR */
		    get_float(&sf.ac_loc_err);
		    break;
		case 24:	/* LNE_DIST */
		    get_float(&sf.lne_dist);
		    break;
		case 25:	/* PXL_DIST */
		    get_float(&sf.pxl_dist);
		    break;
		case 26:	/* DIST_SKEW */
		    get_float(&sf.dist_skew);
		    break;
		case 27:	/* HTGM_DSCPT */
		    next_token(sf.htgm_dscpt);
		    break;
		case 28:	/* HTGM_SEQ */
		    get_int(&sf.htgm_seq);
		    break;
		case 29:	/* LNE_SMPL_NUM */
		    get_int(&sf.lne_smpl_num);
		    break;
		case 30:	/* AC_SMPL_NUM */
		    get_int(&sf.ac_smpl_num);
		    break;
		case 31:	/* LNE_GRP_SIZE */
		    get_int(&sf.lne_grp_size);
		    break;
		case 32:	/* AC_GRP_SIZE */
		    get_int(&sf.ac_grp_size);
		    break;
		case 33:	/* LNE_NUM_SMPL */
		    get_int(&sf.lne_num_smpl);
		    break;
		case 34:	/* AC_NUM_SMPL */
		    get_int(&sf.ac_num_smpl);
		    break;
		case 35:	/* IMIN_FRST */
		    get_int(&sf.imin_frst);
		    break;
		case 36:	/* IMAX_LAST */
		    get_int(&sf.imax_last);
		    break;
		case 37:	/* IVAL_INC */
		    get_int(&sf.ival_inc);
		    break;
		case 38:	/* QMIN_FRST */
		    get_int(&sf.qmin_frst);
		    break;
		case 39:	/* QMAX_LAST */
		    get_int(&sf.qmax_last);
		    break;
		case 40:	/* QVAL_INC */
		    get_int(&sf.qval_inc);
		    break;
		case 41:	/* NUM_SMPL_OFF */
		    get_int(&sf.num_smpl_off);
		    break;
		case 42:	/* NUM_RL */
		    get_int(&sf.num_rl);
		    break;
		case 43:	/* FRST_CTR_FREQ */
		    get_float(&sf.frst_ctr_freq);
		    break;
		case 44:	/* LST_CTR_FREQ */
		    get_float(&sf.lst_ctr_freq);
		    break;
		case 45:	/* MIN_SPTRL_PWR */
		    get_float(&sf.min_sptrl_pwr);
		    break;
		case 46:	/* MAX_SPTRL_PWR */
		    get_float(&sf.max_sptrl_pwr);
		    break;
		case 47:	/* NP_NFLR */
		    get_int(&sf.np_nflr);
		    break;
		case 48:	/* NL_NFLR */
		    get_int(&sf.nl_nflr);
		    break;
		case 49:	/* TRK_ANG */
		    get_float(&sf.trk_ang);
		    break;
		case 50:	/* ASC_DSC */
		    next_token(sf.asc_dsc);
		    break;
		case 51:	/* ROLL_QLTY */
		    get_int(&sf.roll_qlty);
		    break;
		case 52:	/* YAW_QLTY */
		    get_int(&sf.yaw_qlty);
		    break;
		case 53:	/* PITCH_QLTY */
		    get_int(&sf.pitch_qlty);
		    break;
		case 54:	/* ROLL_RATE_QLTY */
		    get_int(&sf.roll_rate_qlty);
		    break;
		case 55:	/* YAW_RATE_QLTY */
		    get_int(&sf.yaw_rate_qlty);
		    break;
		case 56:	/* PITCH_RATE_QLTY */
		    get_int(&sf.pitch_rate_qlty);
		    break;
		case 57:	/* INC_IMAGE_CTR */
		    get_float(&sf.inc_image_ctr);
		    break;
		case 58:	/* AZ_DIST */
		    get_float(&sf.az_dist);
		    break;
		case 59:	/* RG_DIST */
		    get_float(&sf.rg_dist);
		    break;
		case 60:	/* SQUINT */
		    get_float(&sf.squint);
		    break;
		case 61:	/* CAL_EST */
		    get_float(&sf.cal_est);
		    break;
		case 62:	/* RAD_RES */
		    get_float(&sf.rad_res);
		    break;
		case 63:	/* SAT_PNT */
		    get_int(&sf.sat_pnt);
		    break;
		case 64:	/* RCF_AVG */
		    get_float(&sf.rcf_avg);
		    break;
		case 65:	/* SNR */
		    get_float(&sf.snr);
		    break;
		case 66:	/* LK_ANGLE */
		    get_float(&sf.lk_angle);
		    break;
		default:		/* unrecognizeable parameter */
		    printf("unrecognizeable parameter in %s: %s\n",
				filename,t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << (i % 31);
		ibyte = i / 31;
		if (fields_in[ibyte] & ibit)
		    printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in[ibyte] |= ibit;
	    }
	}  /* for */
    /* check for missing parameters */
	if ((fields_in[0] != fields_exp[0]) || 
			(fields_in[1] != fields_exp[1])) {
	    for (i = 0; i < ptablen; i++) {
		ibit = 1 << (i % 31);
		ibyte = i / 31;
		if ((fields_in[ibyte] & ibit) == 0)
		    printf ("missing parameter %s in %s file\n",
				ptab[i],filename);
	    }  /* for i */
	    ans = FAIL;
	}
	close_file();
	return (ans);
}

/* p_init_scene_file() -------------------------------------------------
	This routine initializes the scene file structure in memory.
*/

p_init_scene_file()
{
	zero_bytes(&sf,sizeof(SCENE_FILE));
}

/* p_get_dwp_file(filename_) -------------------------------------------
       This routine read the dwp_base value from dwp data file and
        stores the data in the array.
*/

p_get_dwp_file(filename)
        char *filename;
{
        FILE *fd;
        int i;
        char s[20];

        if ((fd = fopen(filename,"r")) == NULL) {;
            printf("Cannot open %s\n", filename);
            return(FAIL);
        }
        num_dwp_base =1;
        for (i=0; i<num_dwp_base+1; i++){
            if (fgets(s,20,fd)== NULL){
                printf("Can not read file %s\n", filename);
                return(FAIL);
            } else {
                if (i == 0){
                  num_dwp_base = atoi(s);
                  dwp[i] = num_dwp_base;
                  if (vbose) printf("num_dwp_base=%d\n",num_dwp_base);
                }
                else dwp[i] = atoi(s);
                if (vbose) printf("dwp[%d] = %d\n", i, dwp[i]);
             }
        }
        fclose(fd);
}

/* fill_block(s,len,val) -----------------------------------------------
	This routine sets each byte in s, len bytes long, to val.
*/

fill_block(s,len,val)
	char *s;
	int len,val;
{
	while (len--)
	    *s++ = val;
}
