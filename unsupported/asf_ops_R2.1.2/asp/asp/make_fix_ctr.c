/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* 11-30-95 CLW Add subroutine setup_fix_ctr_tbl to initialize values
		 for specific sensor & mode */

#include <stdio.h>
#include <math.h>
#include <procfil.h>
#include <aspdecl.h>
#include <fix_ctr_tbl.h>

/* system configuration parameters */
extern int vbose;               /* 1 = print debugging messages */
extern RQST_PTR Cur_Rqst;       /* pointer to current job request */
extern SV sv1,sv2;              /* current request's statevectors */
extern SP_FILE spf;             /* sensor parameters file */
extern int fst_len;		/* JERS line length */
extern int sw_clutter_lock;
extern int asphw;
extern int do_scan;
extern int cp_flag;
extern char PROC_PATH[];
extern char frame_mode[];
extern int use_rds_scan;
extern int rds_center_format;

int dec;                        /* 1 = decending orbit */
int seg_fmt_start, seg_fmt_end; /* combine fmt_id & fmt_start */
double lat_save, lon_save;
int pre_dwp = 0;
double start_dsec;
GMT gmt_start;
int start_frame;
double dsec = 0.0;
double ctr_time_frame=0.0;
int frame_id;
int start_format;
int first_n_s;
double fix_ctr_tbl_new[900];

/* e1_make_pprs(sp) ----------------------------------------------------
	This routine creates preprocessing data blocks for each
	preprocessing region required in the given segment.
*/

e1_make_pprs(sp)
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
	float prenoise, postnoise;
	int tpre, tpos;
	int dwp_change;

	prf = sp->pre->prf;
	if (vbose)
	    printf("prf=%g\n",prf);
	if ((sp->end_id - sp->fmt_id) < 26000) {
	    printf("...not enough data to process\n");
	    return (FAIL);
	}

	format = sp->fmt_id + 12;
	while (format < sp->end_id) {
	    if ((pp = (PP_BLOCK_PTR) malloc(sizeof(PP_BLOCK))) == NULL) {
		printf("Out of memory in e1_make_pprs\n");
		return (FAIL);
	    }
	    p_init_pp_block(pp);
	    if (sp->ppb_list == NULL)
		sp->ppb_list = pp;
	    else
		pp2->nxt_ppb = pp;
	    pp2 = pp;
	    pp->nxt_ppb = NULL;
	    pp->fmt_id = format;
	    pp->fmt_start = -1;
	    e1_get_fmt_loc(-1,format,sp,&pp->blk_start,&pp->bit_off);
	    pp->polarity = sp->polarity;
	    pp->quality = -1;
	/* calculate the first and last format within a region */
	    first_format = format - 8*prf;
	    last_format  = format + 12*prf;
	    if (first) {
	       first_format = sp->fmt_id;
	       last_format  = first_format + 20*prf;
	    }
	    if (last) {
	       last_format  = sp->end_id;
	       first_format = last_format - 20*prf;
	    }
	if (vbose) printf("first format = %d, last format = %d\n", 
		  first_format, last_format);
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
	/* propagate statevector to this region start */
	    if (vbose)
		printf("...propagating state vector\n");
	    get_fmt_time(sp,format,&pp->sv.gmt);
	    time = get_gmt_diff(&pp->sv.gmt,&sv1.gmt);
	    if (stv_prop(time,pp)==FAIL) return(FAIL);
	/* calculate spacecraft attitude */
	    get_attitude(&pp->sv,&pp->att);
	    get_range(sp,pp);

	/* assign noise for each image */
	/* if no values in preamble and postamble, use value */
	/*    from default parameter file                    */
	    if (sp->pre == NULL && sp->post == NULL) {
		pp->noise = spf.noise;
	    }
	/* if both exist and are within bounds linearly interpolate */
	/*    using the ami format numbers of pre, post, and image. */
	/* otherwise use the one value from either pre or post,     */
	/*    unless both are bad, then use the def. param file.    */
	    else {
		if (sp->pre == NULL) prenoise = 0.0;
		else prenoise = sp->pre->noise_dton;
		if (sp->post == NULL) postnoise = 0.0;
		else postnoise = sp->post->noise_dtoff;
		if (prenoise >= 1.8 && prenoise <= 2.5) {
		  if (postnoise >= 1.8 && postnoise <= 2.5) {
		    tpre = sp->pre->fmt_id;
		    tpos = sp->post->fmt_id;
		    pp->noise = postnoise * (format - tpre)/(tpos-tpre)
			       + prenoise * (tpos - format)/(tpos-tpre);
		  }
		else  /* postnoise must be bad */
		  pp->noise = prenoise;
		}
		else { /* prenoise must be bad */
		  if (postnoise >= 1.8 && postnoise <= 2.5) 
		    pp->noise = postnoise;
		  else  /* both must be bad */
		    pp->noise = spf.noise;
		}
	    }  /* end of noise determination */

	/* get initial doppler parameters */
	    istat = 0;
	    if (get_dops(sp,pp) != PASS)
		return (FAIL);
	    if (vbose)
		printf("...swath speed = %g\n",pp->swth_speed);

	if (first) {
	    first = 0;
	    if ((format = e1_locate_ppr(sp,pp)) == FAIL)
		return (FAIL);
	    if (format < sp->fmt_id || format > (sp->end_id - 2056)) {
		printf("...image not within this segment\n");
		sp->ppb_list = NULL;
		return (PASS);
		}
	/* if not doing standard processing, use format number
	   to determine the correct data window position     */
	/* in all cases go ahead and determine correct dwp -dtc */
	/*    if (strcmp(Cur_Rqst->type,"STD") != 0   		*/
	/*      && strcmp(Cur_Rqst->type,"QLK") != 0) {		*/
	      first_format = format - 8*prf;
	      last_format  = format + 12*prf;
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
	      dwp_change = w;
	/* get window change, if any */
	      if ((++w < sp->win_count) && (last_format >= sp->wfmt[w])) {
		pp->dly2 = sp->wdwp[w];
		pp->dly_chg = sp->wfmt[w];
	        if (vbose)
	  	  printf("window change - wdwp = %d, wfmt = %d\n",
		     sp->wdwp[w], sp->wfmt[w]);
	      }
              if (pp->dly1 == pp->dly2) {
                  pre_dwp = dwp_change;
                  if (vbose) printf("pre_dwp = %d\n", pre_dwp);
              }

	/*    }   do not test for STD and QLK */
	/* update the region location */
	    pp->fmt_id = format;
	    e1_get_fmt_loc(-1,format,sp,&pp->blk_start,&pp->bit_off);
	/* propagate statevector to this region start */
	    get_fmt_time(sp,format,&pp->sv.gmt);
	    time = get_gmt_diff(&pp->sv.gmt,&sv1.gmt);
	    if (stv_prop(time,pp)==FAIL) return(FAIL);
	/* calculate spacecraft attitude */
	    get_attitude(&pp->sv,&pp->att);
	    get_range(sp,pp);

	/* assign noise for each image */
	/* if no values in preamble and postamble, use value */
	/*    from default parameter file                    */
	    if (sp->pre == NULL && sp->post == NULL) {
		pp->noise = spf.noise;
	    }
	/* if both exist and are within bounds linearly interpolate */
	/*    using the ami format numbers of pre, post, and image. */
	/* otherwise use the one value from either pre or post,     */
	/*    unless both are bad, then use the def. param file.    */
	    else {
		if (sp->pre == NULL) prenoise = 0.0;
		else prenoise = sp->pre->noise_dton;
		if (sp->post == NULL) postnoise = 0.0;
		else postnoise = sp->post->noise_dtoff;
		if (prenoise >= 1.8 && prenoise <= 2.5) {
		  if (postnoise >= 1.8 && postnoise <= 2.5) {
		    tpre = sp->pre->fmt_id;
		    tpos = sp->post->fmt_id;
		    pp->noise = postnoise * (format - tpre)/(tpos-tpre)
			       + prenoise * (tpos - format)/(tpos-tpre);
		  }
		else  /* postnoise must be bad */
		  pp->noise = prenoise;
		}
		else { /* prenoise must be bad */
		  if (postnoise >= 1.8 && postnoise <= 2.5) 
		    pp->noise = postnoise;
		  else  /* both must be bad */
		    pp->noise = spf.noise;
		}
	    }  /* end of noise determination */

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
	  /* check for near end of segment. mod. for 2pass, Ming 1-27-95 */
	    if (format > sp->end_id - dformats)
	    /* if (format > sp->end_id - (dformats2 + 2056)) */
		end_sw++;
	    switch (end_sw) {
		case 0:		    /* all but last region */
		    break;
		case 1:		    /* last region */
		    format = sp->end_id - dformats;
		    /* format = sp->end_id - (dformats2 + 2056); */
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

/* e1_locate_ppr(sp,pp) ------------------------------------------------
	This routine calculates the location of the first preprocessing
	region.
*/

e1_locate_ppr(sp,pp)
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
	float prf,fd3p[3],fr3p[3];
	int format,istat,tfmts,nfmts,maxfmt;
	double get_gmt_diff(), cos2();

	deg_to_rad = pi / 180.0;
	format = sp->fmt_id + 12;
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

/* use lat & lon from request to calculate start */
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
    /* remove the test for going beyond end of datatake
	if (sp->post == NULL)
	    maxfmt = sp->end_id;
	else
	    maxfmt = sp->post->fmt_id;
	maxfmt -= 2056;
	if (format > maxfmt)
	    format = maxfmt;
    */
	return (format);
}
/* make_fix_ctr(sp) ---------------------------------------------
	This routine calculates the starting addr for a fixed center
	region.
*/

make_fix_ctr(sp)
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
	double lat_next = 0.;
	double lon_next = 0.;
	float fd3p[3],fr3p[3];
	int tfmts;
	float prenoise, postnoise;
	int tpre, tpos;
	int status;

	first_n_s = 1;
	if(Cur_Rqst->take_id[0] == 'E') {
	    seg_fmt_start = sp->fmt_id;
	    seg_fmt_end = sp->end_id;
	}
	else if(Cur_Rqst->take_id[0] == 'J') {
	    seg_fmt_start = j1_valid_agc(sp);
            if (seg_fmt_start > sp->ppfmt[0])
                printf("ZERO AGC point to fmt %d, was %d\n",seg_fmt_start,sp->ppfmt[0]) ;
	    seg_fmt_end = sp->fmt_end - 30000;
	}
	else if(Cur_Rqst->take_id[0] == 'R') {
	    seg_fmt_start = sp->fmt_start;
	    seg_fmt_end = sp->fmt_end;
	}
	else printf("Undefined Satellite\n");

	if ( setup_fix_ctr_tbl(sp) == FAIL ) return(FAIL); /* CLW adds call*/

	prf = sp->pre->prf;
	if (vbose)
	    printf("prf=%g\n",prf);
	if ((seg_fmt_end - seg_fmt_start) < 26000) {
	    printf("...not enough data to process\n");
	    return (PASS);
	}

	format = seg_fmt_start + 12;
	while (format < seg_fmt_end) {
	    if ((pp = (PP_BLOCK_PTR) malloc(sizeof(PP_BLOCK))) == NULL) {
		printf("Out of memory in make_fix_ctr\n");
		return (FAIL);
	    }
	    p_init_pp_block(pp);
	    if (sp->ppb_list == NULL)
		sp->ppb_list = pp;
	    else
		pp2->nxt_ppb = pp;
	    pp2 = pp;
	    pp->nxt_ppb = NULL;
	    if(Cur_Rqst->take_id[0] == 'E') {
	       	pp->fmt_id = format;
	       	pp->fmt_start = -1;
	       	e1_get_fmt_loc(-1,format,sp,&pp->blk_start,&pp->bit_off);
	    }
	    else if(Cur_Rqst->take_id[0] == 'J') {
	       	pp->fmt_start = format;
	       	pp->fmt_id = 0;
	       	j1_get_fmt_loc(0,sp,format,&fst_len,&pp->blk_start,&pp->bit_off);
	    }
	    else {
	    	pp->fmt_start = format;
		pp->fmt_id = 0;
	    	r1_get_fmt_loc(0,sp,format,&pp->blk_start,&pp->bit_off);
	    }
	    pp->polarity = sp->polarity;
	    pp->quality = -1;
	    pp->lat_rough = lat_next;
	    pp->lon_rough = lon_next;
	    lat_save = lat_next;
	    lon_save = lon_next;
	/* calculate the first and last format within a region */
	    first_format = format - 8*prf;
	    last_format  = format + 12*prf;
	    if (first) {
	       first_format = seg_fmt_start;
	       last_format  = first_format + 20*prf;
	    }
	    if (last) {
	       last_format  = seg_fmt_end;
	       first_format = last_format - 20*prf;
	    }
	if (vbose) printf("first format = %d, last format = %d\n", 
		  first_format, last_format);
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
	/* propagate statevector to this region start */
	    if (vbose)
		printf("...propagating state vector\n");
	/* if Frame job, use sv1 time to start to assure sv prop., 5/17/96 */
	    if (strcmp(Cur_Rqst->type, "STD") == 0) 
	    	get_fmt_time(sp,format,&pp->sv.gmt);
	    else
		pp->sv.gmt = sv1.gmt;
	    time = get_gmt_diff(&pp->sv.gmt,&sv1.gmt);
	    if (vbose) {  /* add in 2/14/96 */
		printf("pp->sv.gmt & sv1.gmt\n");
		dump_gmt(&pp->sv.gmt);
		dump_gmt(&sv1.gmt);
	    }
	    if (stv_prop(time,pp)==FAIL) return(FAIL);
	/* calculate spacecraft attitude */
	    get_attitude(&pp->sv,&pp->att);
	    get_range(sp,pp);

	/* get initial doppler parameters */
	    istat = 0;
	    if (get_dops(sp,pp) != PASS)
		return (FAIL);

	    if (vbose)
		printf("...swath speed = %g\n",pp->swth_speed);

	if (first) {
	    first = 0;
	    start_format = format;
	    first_loc(sp,pp,&format);
            /* use RDS center format to fix the first & second frame problem
              (image not within the region not dropout) CV 7/24/97 */
            if (vbose) printf("VUU: before format = %d\n", format);
            if ((strcmp(Cur_Rqst->type, "STD") != 0) && (use_rds_scan))
                 format = rds_center_format;
            if (vbose)
              printf("VUU: RDS center format of processing frame %d\n", format);
	    if (format<seg_fmt_start || format>(seg_fmt_end-2056)) {
		printf("...image not within this segment");
		printf(",format=%d\n",format);
		sp->ppb_list = NULL;
		return (PASS);
	    }
	    first_format = format - 8*prf;
	    last_format  = format + 12*prf;
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
	/* update the region location */
	    if(Cur_Rqst->take_id[0] == 'E') {
	       	pp->fmt_id = format;
	       	pp->fmt_start = -1;
	       	e1_get_fmt_loc(-1,format,sp,&pp->blk_start,&pp->bit_off);
	    }
	    else if(Cur_Rqst->take_id[0] == 'J') {
	       	pp->fmt_start = format;
	       	pp->fmt_id = 0;
		if ((!asphw) || (do_scan))
	       	  j1_get_fmt_loc(0,sp,format,&fst_len,&pp->blk_start,&pp->bit_off);
		else { /* make sure first offset is correct for 2dfft*/
	       	  j1_get_fmt_loc(1,sp,format,&fst_len,&pp->blk_start,&pp->bit_off);
		  pp->polarity = sp->polarity;
		}
	    }
	    else {
		format = sp->fmt_start + ((format-sp->fmt_start)/8 * 8);
	    	pp->fmt_start = format;
		pp->fmt_id = 0;
	    	r1_get_fmt_loc(0,sp,format,&pp->blk_start,&pp->bit_off);
	    }
	/* propagate statevector to this region start */
	    get_fmt_time(sp,format,&pp->sv.gmt);
	    time = get_gmt_diff(&pp->sv.gmt,&sv1.gmt);
	    if (stv_prop(time,pp)==FAIL) return(FAIL);
	/* calculate spacecraft attitude */
	    get_attitude(&pp->sv,&pp->att);
	    get_range(sp,pp);

	/* get initial doppler parameters */
	    istat = 0;
	    if (get_dops(sp,pp) != PASS)
		return (FAIL);

	/* assign format & time if 1st frame is 220 or 670, MC 7-31-96 */
	    if (pp->frame_id == 220 || pp->frame_id == 670) {
		start_format = format;
		gmt_start = pp->sv.gmt;
	    }
	  } /* end if first */

	/* assign noise for each image */
	/* if no values in preamble and postamble, use value */
	/*    from default parameter file                    */
	    if (sp->pre == NULL && sp->post == NULL) {
		pp->noise = spf.noise;
	    }
	/* if both exist and are within bounds linearly interpolate */
	/*    using the ami format numbers of pre, post, and image. */
	/* otherwise use the one value from either pre or post,     */
	/*    unless both are bad, then use the def. param file.    */
	    else {
		if (sp->pre == NULL) prenoise = 0.0;
		else prenoise = sp->pre->noise_dton;
		if (sp->post == NULL) postnoise = 0.0;
		else postnoise = sp->post->noise_dtoff;
		if (prenoise >= 1.8 && prenoise <= 2.5) {
		  if (postnoise >= 1.8 && postnoise <= 2.5) {
		    tpre = sp->pre->fmt_id;
		    tpos = sp->post->fmt_id;
		    pp->noise = postnoise * (format - tpre)/(tpos-tpre)
			       + prenoise * (tpos - format)/(tpos-tpre);
		  }
		  else {  /* postnoise must be bad */
		    pp->noise = prenoise;
		  }
		}
		else { /* prenoise must be bad */
		  if (postnoise >= 1.8 && postnoise <= 2.5) 
		    pp->noise = postnoise;
		  else  /* both must be bad */
		    pp->noise = spf.noise;
		}
		if (vbose) printf("noise=%g\n",pp->noise);
	    }  /* end of noise determination */
	    count++;
	    if (vbose)
		printf("...creating region %d at format %d\n",count,format);
	    if ((frame_id == 219) || (frame_id == 669)) start_format = format;
	/* if not doing standard processing, only make 1 ppr */
	    if (strcmp(Cur_Rqst->type,"STD") != 0
	    	&& strcmp(Cur_Rqst->type,"QLK") != 0)
		break;
	/* calculate location of next preprocessing region 
	    dtime = 89200.0 / pp->swth_speed;*/
	    dtime = 44600.0 / pp->swth_speed;  /* half frame 3/15/96 */
	    dformats = (int) (dtime * prf + 0.5);
	    dformats2 = dformats / 2;
	    format += dformats;
	
	    status = search_next(sp,pp,format,lat_save,lon_save,&lat_next,&lon_next);
	    if (status == FAIL) return(FAIL);
	    if (vbose ) printf("%d %g %g %g %g\n",
			format,lat_save,lon_save,lat_next,lon_next);
	/* proceed search as RPR job */
	    format = search_rpr(sp,pp,lat_next,lon_next);

	/* check for near end of segment */
	    if (format > seg_fmt_end - (dformats + 2056))
		end_sw++;
	    switch (end_sw) {
		case 0:		    /* all but last region */
		    break;
		case 1:		    /* discard last region */
		    printf("format = %d, not enough for 1 region\n",format);
		    format += 1000000;
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


/* first_loc(sp,pp,format) ---------------------------------------------
	This routine calculate first applicable longitude for a
	segment based on fixed center scheme.
*/

first_loc(sp,pp,format)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	int *format;
{
	double lat_1,lon_1;
	int status;

    /* if time was given, use it to calculate start */
	if (Cur_Rqst->targ.yr) {
	    get_time_fmt(sp,&Cur_Rqst->targ,format);
	}

    /* search the closest lat/lon, and decide satellite direction,
       if user specified lat/lon, use them */
	if (!Cur_Rqst->lat && !Cur_Rqst->lon) {
	    printf("fix scene center working ...\n");
	    status = search_next(sp,pp,*format,200.0,200.0,&lat_1,&lon_1);
	    if (status == FAIL) return(FAIL);
	}
	else {
	    printf("by user spec lat and lon: %g, %g\n", Cur_Rqst->lat, Cur_Rqst->lon);
	    lat_1 = Cur_Rqst->lat;
	    lon_1 = Cur_Rqst->lon;
	}
    /* proceed as RPR job with first set of lat,lon */
	*format = search_rpr(sp,pp,lat_1,lon_1);
	pp->lat_rough = lat_1;
	pp->lon_rough = lon_1;
	lat_save = lat_1;
	lon_save = lon_1;
	return;
}


/* search_rpr(sp,pp,lat,lon) -----------------------------------
   This routine follow the RPR algorithm to search the starting
   format based on input lat and lon.
*/
search_rpr(sp,pp,lat,lon)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	double lat,lon;
{
	GMT start_gmt;
	double latr,lonr,lata,lona,latd,lond,r_e;
	double a[3],b[3],c[3],d[3],e[3];
	double r3p[3], fd_adj;
	double time,distance,deg_to_rad;
	double cosrd,cosra,cosda,sinra,sinda;
	double pi = 3.141592653589793;
	float prf,fd3p[3],fr3p[3];
	int format,istat,tfmts,nfmts,maxfmt;
	double get_gmt_diff(), cos2();
	int deskew;

	prf = sp->pre->prf;
	deg_to_rad = pi / 180.0;
	format = seg_fmt_start + 12;
	get_fmt_time(sp,format,&start_gmt);
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
	deskew = 0;
	fd_adj = 0.0;
	get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,deskew,fd_adj,
		     a,b,c,d,e,&istat);

    /* calculate the track distance to the new lat/lon */
	latr = deg_to_rad * lat;
	lonr = deg_to_rad * lon;
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
	if (distance < 0.0) 
	   printf("...distance is negative =%g",distance);
    /* calculate the format number of the requested location */
	time = distance / pp->swth_speed;
	nfmts = time * prf;
	if (vbose) printf("format=%d, ", format);
	format += nfmts;
	if (vbose) printf("time=%g,nfmts=%d, format=%d\n",time,nfmts,format);
	return(format);
}


/* search_next(sp,pp,format,lat_old,lon_old,lat_new,lon_new)------------
   This routine use linear interpolation between 2 set of lat/lon
   to derive new lat/lon values.
*/ 
int search_next(sp,pp,format,lat_old,lon_old,lat_new,lon_new)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	int format;
	double lat_old,lon_old,*lat_new,*lon_new;
{
	GMT start_gmt;
	double a[3],b[3],c[3],d[3],e[3],r3p[3];
	double time, fd_adj;
	double c0_save, c1_save;
	float prf,fd3p[3],fr3p[3];
	int diff_fmt,istat,tfmts;
	int deskew;
	int err_cor = 0;
	
	prf = sp->pre->prf;
	get_fmt_time(sp,format,&start_gmt);
	time = 102400.0 / pp->swth_speed;
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
	deskew = 0;
	fd_adj = 0.0;

    /* use lat_old (if 1st region, derive) to calculate lat_next.*/
	if (lat_old == 200.) {
       	    time = get_gmt_diff(&pp->sv.gmt,&sv1.gmt);
            if (stv_prop(time,pp)==FAIL) return(FAIL);
	    if (pp->sv.vel.z < 0) dec = 1;
	    else dec = 0;
	    get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,deskew,fd_adj,
		     a,b,c,d,e,&istat);
	    if ((pp->frame_id = lat_table(c[0],dec,lat_new,1)) == FAIL) {
	        printf("c[0]=%g\n",c[0]);
		if (dec){
		   	dec = 0;
		   	printf("Should be acending, error corrected\n");
		}
		else {
		   	dec = 1;	
		   	printf("Should be decending, error corrected\n");
		}
	        if ((pp->frame_id = lat_table(c[0],dec,lat_new,1)) == FAIL){ 
	        	printf("This latitude is not in the boundary of ");
			printf("table: %g\n",c[0]);
			return(FAIL);
		}
	    }
	    c0_save = c[0];
	    c1_save = c[1];
	    printf("format=%d,%d,%g,%g\n",format,pp->frame_id,c0_save,c1_save);
	    printf("start_gmt=%d:%d:%d:%d:%f ",start_gmt.yr,start_gmt.day,
                        start_gmt.hr,start_gmt.min,start_gmt.second);
	}
	else {
	    if ((frame_id >= 219 && frame_id < 228) ||
	      (frame_id >= 669 && frame_id < 678)) {  
		pp->frame_id = frame_id + 1;
		c0_save = lat_old;
		c1_save = lon_old;
	    } else if (frame_id == 228) {
		pp->frame_id = frame_id + 1;
		*lat_new = fix_ctr_tbl[229];
	    	c0_save = lat_old;
	    	c1_save = lon_old;
		dec = 1;
	    } else if (frame_id == 678) {
		pp->frame_id = frame_id + 1;
		*lat_new = fix_ctr_tbl[679];
	    	c0_save = lat_old;
	    	c1_save = lon_old;
		dec = 0;
	    } else {	
	    	if ((pp->frame_id = lat_table(lat_old,dec,lat_new,0)) == FAIL)
			return(FAIL);
	    	c0_save = lat_old;
	    	c1_save = lon_old;
	    }
	}
	if (pp->frame_id == 219 || pp->frame_id == 669)
	    gmt_start = start_gmt; /* start_gmt point to 220, 670 */
	get_fmt_time(sp,format+5000,&start_gmt);
	get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,deskew,fd_adj,
		     a,b,c,d,e,&istat);
	if ((pp->frame_id >= 220 && pp->frame_id < 229) ||
	    (pp->frame_id >= 670 && pp->frame_id < 679)) {
	    if ((pp->frame_id == 220 && lat_old == 200) ||
		(pp->frame_id == 670 && lat_old == 200)) {
		printf("Start at boundary, use interpolation\n");
	    	if ((c1_save-c[1]) < -180) c[1] -= 360;
	    	if ((c1_save-c[1]) > 180) c[1] += 360;
	    	*lon_new = c1_save + (c[1]-c1_save) * (*lat_new-c0_save) /
	 		(c[0]-c0_save);
	    }
	    else
	    	adjust_latlon(sp,pp,c0_save,c1_save,c,lat_new,lon_new); 
	}

	else { /* calculate lat_new based on interpolation */
	    if ((c1_save-c[1]) < -180) c[1] -= 360;
	    if ((c1_save-c[1]) > 180) c[1] += 360;
	    *lon_new = c1_save + (c[1]-c1_save) * (*lat_new-c0_save) /
	 	(c[0]-c0_save);
	}
	if (*lon_new > 180) *lon_new -= 360;
	if (*lon_new < -180) *lon_new += 360;
	printf("Current frame_id = %d\n ",pp->frame_id);
	printf("got new latitude!, next lat=%g, next lon=%g\n",
		*lat_new,*lon_new);
	if (lat_old == 200.) {
	    if ((pp->frame_id > 220 && pp->frame_id < 230) ||
	        (pp->frame_id > 670 && pp->frame_id < 680))   
			frame_id = pp->frame_id;
	    else 
		frame_id = pp->frame_id-1;
	}	
	else 
		frame_id = pp->frame_id;
	return(PASS);
}

/* adjust_latlon(sp,pp,c0_save,c1_save,lat_new,lon_new)-----------------
  This routine calculate the lat & lon for the frame_id in north or
   south pole (frame_id > 220 && < 230 or frame_id > 680 && < 690)
*/

adjust_latlon(sp,pp,c0_save,c1_save,center,lat_new,lon_new)
        TAPE_SEG_PTR sp;
        PP_BLOCK_PTR pp;
        double c0_save,c1_save,*center,*lat_new,*lon_new;
{
        GMT gmt_end,gmt_ctr,gmt_frame,gmt_first,start_gmt,gmt_temp;
        double get_gmt_diff();
        double start_lat, start_lon, end_lat, end_lon;
        double ctr_time_frame, half_time_frame;
        double a[3],b[3],c[3],d[3],e[3],r3p[3];
        double time, fd_adj;
        float prf,fd3p[3],fr3p[3];
        int diff_fmt,istat,tfmts;
        int deskew, frame_count;
        int end_format;
	double time_diff, add_delta, backup_time;
	
        prf = sp->pre->prf;
        time = 102400.0 / pp->swth_speed;
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
        deskew = 0;
        fd_adj = 0.0;

	printf("ADJUST_LATLON: %d\n",first_n_s);
        if (first_n_s) {
          if ((pp->frame_id >= 220) && (pp->frame_id < 230)) {
                start_lat = fix_ctr_tbl[219];
                end_lat = fix_ctr_tbl[229];
                start_frame = 219;
          }
          if ((pp->frame_id >= 670) && (pp->frame_id < 680)) {
                start_lat = fix_ctr_tbl[669];
                end_lat = fix_ctr_tbl[679];
                start_frame = 669;
          }
	  if ((pp->frame_id > 220 && pp->frame_id < 230) || 
		(pp->frame_id > 670 && pp->frame_id < 680)) {
          	get_fmt_time(sp,start_format,&gmt_first);
		gmt_frame = gmt_first;
	        if (pp->frame_id > 220 && pp->frame_id < 230) {
		    backup_time = (pp->frame_id-start_frame) * 6.4;
		    add_seconds(&gmt_first,-backup_time);
		}
		if (pp->frame_id > 670 && pp->frame_id < 680) {
		    backup_time = (pp->frame_id-start_frame) * 4.3;
		    add_seconds(&gmt_first,-backup_time);
		}
          	get_corn_loc(sp,pp,gmt_first,tfmts,r3p,fd3p,fr3p,deskew,fd_adj,
             		a,b,c,d,e,&istat);
		c1_save = c[1];
		c0_save = c[0];

          	start_format = search_rpr(sp,pp,c0_save,c1_save);
		get_fmt_time(sp,start_format+5000,&start_gmt);
		get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,deskew,fd_adj,
		     a,b,c,d,e,&istat);
	        start_lon= c1_save + (c[1]-c1_save) * (start_lat-c0_save) /
	                (c[0]-c0_save);	

          	start_format = search_rpr(sp,pp,start_lat,start_lon);
          	get_fmt_time(sp,start_format,&gmt_start);
		printf("gmt_first=%d:%d:%d:%d:%f ",gmt_first.yr,gmt_first.day,
			gmt_first.hr,gmt_first.min,gmt_first.second);
		printf("gmt_start=%d:%d:%d:%d:%f ",gmt_start.yr,gmt_start.day,
                      gmt_start.hr,gmt_start.min,gmt_start.second);
		printf("gmt_frame=%d:%d:%d:%d:%f ",gmt_frame.yr,gmt_frame.day,
                        gmt_frame.hr,gmt_frame.min,gmt_frame.second);
	  } 
	  else {
		start_lon = c1_save;
		printf("gmt_start=%d:%d:%d:%d:%f ",gmt_start.yr,gmt_start.day,
                        gmt_start.hr,gmt_start.min,gmt_start.second);
	  }

	  /* calculate the lon & format of frame=230(N) or frame=670(S) */
          gmt_end = gmt_start;
          if (!strncmp(Cur_Rqst->take_id,"E",1)) {
             if ((pp->frame_id >= 670) && (pp->frame_id < 680))
          	dsec = 6000 * 9/prf;	 /* S-pole */
	     else 
          	dsec = 10500 * 9/prf;	 /* N-pole */
	  }	
          else if (!strncmp(Cur_Rqst->take_id,"J",1)) {
             if ((pp->frame_id >= 670) && (pp->frame_id < 680))
          	dsec = 5000 * 9/prf;	 /* S-pole */
	     else
          	dsec = 10000 * 9/prf;	 /* N-pole */
	  }
          else if (!strncmp(Cur_Rqst->take_id,"R",1)) {
             if ((pp->frame_id >= 670) && (pp->frame_id < 680))
          	dsec = 6000 * 9/prf;	 /* S-pole */
	     else
          	dsec = 10000 * 9/prf;	 /* N-pole instead of 10500 2/19/97*/
	  }
          add_seconds(&gmt_end,dsec);
          get_corn_loc(sp,pp,gmt_end,tfmts,r3p,fd3p,fr3p,deskew,fd_adj,
             a,b,c,d,e,&istat);
          end_lon = c[1];
          end_format = search_rpr(sp,pp,end_lat,end_lon);
          dsec = (end_format - start_format)/(10*prf);

	  /* determine the frame_id number */
	  frame_count = 1;
	  time_diff = 1.0;
/* Ming, assume half frame is dsec 
          half_time_frame = 102400.0/2/pp->swth_speed;
          add_seconds(&gmt_start,-half_time_frame);
*/
	  add_seconds(&gmt_start,-dsec);
	  gmt_temp = gmt_start;
	  if ((pp->frame_id > 220 && pp->frame_id < 230) || 
	      (pp->frame_id > 670 && pp->frame_id < 680)) {
	     while (time_diff > 0.0) {
		add_seconds(&gmt_temp,dsec);
		time_diff = get_gmt_diff(&gmt_frame,&gmt_temp);
		if (time_diff > 0.0) {
	  	    if (pp->frame_id > 220 && pp->frame_id < 230) {  
			pp->frame_id = 220 + frame_count;
			frame_count += 1;	
		    }
	            else if (pp->frame_id > 670 && pp->frame_id < 680) {
			pp->frame_id = 670 + frame_count;
			frame_count += 1;	
		    }
		}
	     }
	  }
          ctr_time_frame = (fabs)((pp->frame_id - start_frame)*(dsec));
	  first_n_s = 0;
	  printf("start_fmt=%d,end_fmt=%d\n",start_format,end_format);
          printf("start_lat=%g,start_lon=%g,end_lat=%g,end_lon=%g, ",
                  start_lat,start_lon,end_lat,end_lon);
	  printf("gmt_end=%d:%d:%d:%d:%f\n",gmt_end.yr,gmt_end.day,
                        gmt_end.hr,gmt_end.min,gmt_end.second);
          printf("dsec=%g,ctr_time_frame=%g\n ",dsec,ctr_time_frame);

        } /* if first_n_s */
        else
            ctr_time_frame = (fabs)((pp->frame_id-start_frame)*(dsec));

        gmt_ctr = gmt_start;
        add_seconds(&gmt_ctr,ctr_time_frame);
        get_corn_loc(sp,pp,gmt_ctr,tfmts,r3p,fd3p,fr3p,deskew,fd_adj,
             a,b,c,d,e,&istat);
        *lat_new = c[0];
        *lon_new = c[1];
		
}


/* lat_table(c,d,lat_new,first)-------------------------------------------------
   This routine searches a valid fix scene center based on input latitude.
   d = 1 is decending.
*/ 
int lat_table(c,d,lat_new,first)
	double c;
	int d, first;
	double *lat_new;
{
	int i,j;

	if (d == 1) {
	    printf("this is decending\n");
	    for (i=0; i<450; i++) {
		j = i + 225;
		if (c > fix_ctr_tbl[j]) {
		    if (first == 1) {
			*lat_new = fix_ctr_tbl[j];
			return(j+1);
		    }
		    else {
			*lat_new = fix_ctr_tbl[j];
		    	return(j);
		    }
		}
	    }
	}
	else {
	    printf("this is acending\n");
	    for (i=450; i<900; i++) {
		j = (i+225) % 900;
		if (c < fix_ctr_tbl[j]) {
		    if (first == 1) {
			*lat_new = fix_ctr_tbl[j];
			return(j+1);
		    }
		    else {
			*lat_new = fix_ctr_tbl[j];
		    	return(j);
		    }
		}
	    }
	}
	printf("No fix scene center allocated!\n");
	*lat_new = c;
	return(-1);
}

/* cos2(lat1,lon1,lat2,lon2) --------------------------------------
        This routine calculates the earth arc angle of a line
        between points 1 and 2, given the latitude and longitude
        of the two points.
*/

double cos2(lat1,lon1,lat2,lon2)
        double lat1,lon1,lat2,lon2;
{
        double ans;

        ans = (sin(lat1) * sin(lat2)) +
              (cos(lat1) * cos(lat2) * cos(lon1 - lon2));
        return (ans);
}
/* setup_fix_ctr_tbl(sp)
	CLW: Initialize fix_ctr_tbl depends on satellite & its mode
*/
setup_fix_ctr_tbl (sp)
	TAPE_SEG_PTR sp;
{
	int status = PASS;
	char filename[100], beam_seq[10];
	int nc;

	if (cp_flag) {     /* read the fix_frame_tbl file from default */
	   if (!strcmp(frame_mode,"ARCTIC"))
		sprintf(filename,"%sfix_frame_tbl",PROC_PATH);
 	   else { 
		sprintf(filename,"%sfix_frame_tbl.L",PROC_PATH);
		if (vbose) printf("Left looking table %s\n", filename);
	   }
	   sprintf(beam_seq,"ST%d",sp->aux.beam_seq); /* CV 2/19/97*/
	   status = GetODLFrameTable(beam_seq,filename,fix_ctr_tbl_new); 
	   if (status == FAIL) {
	   	printf("Can't not get the fix frame center table\n");
	    	return(FAIL);
	   }
	   fix_ctr_tbl = fix_ctr_tbl_new;
	   return(PASS);
	}
	else {
	   switch ( Cur_Rqst->take_id[0] ){
	      case 'E':
		switch ( Cur_Rqst->take_id[1] ){
		   case '1':
			if (sv1.rev > 211 && sv1.rev < 2167) 
			    	fix_ctr_tbl = E1_fix_00212;
			else if (sv1.rev > 2353 && sv1.rev < 3713)
			    	fix_ctr_tbl = E1_fix_02354;
			else if (sv1.rev > 3734 && sv1.rev < 12744)
			    	fix_ctr_tbl = E1_fix_03735;
			else if (sv1.rev > 12753 && sv1.rev < 14287) 
			    	fix_ctr_tbl = E1_fix_12754;
			else if (sv1.rev > 14301 && sv1.rev < 16746)
			    	fix_ctr_tbl = E1_fix_14302;
			else if (sv1.rev > 16746 && sv1.rev < 19216)
			    	fix_ctr_tbl = E1_fix_16747;
			else if (sv1.rev > 19247 && sv1.rev < 34278)
			    	fix_ctr_tbl = E1_fix_19248;
			else {
			    	fix_ctr_tbl = E1_fix_19248;
				printf("Need new E1_fix_ctr table\n"); 
			}
			break;
		   case '2':
			fix_ctr_tbl = E2_fix_00001;
			break;

		   default:
			status = FAIL;
			break;
		}
		break;
	      case 'J':
		if (sv1.rev > 463 && sv1.rev < 29460) 
			fix_ctr_tbl = J1_fix_00464;
		else {
			fix_ctr_tbl = J1_fix_00464;
			printf("Need new J1_fix_ctr table\n");
		}
		break;
	      case 'R':
		switch ( sp->aux.beam_seq ){
		   case 1: 
			fix_ctr_tbl = R1_STD1_fix_ctr;
			break;
		   case 2: 
			fix_ctr_tbl = R1_STD2_fix_ctr;
			break;
		   case 3: 
			fix_ctr_tbl = R1_STD3_fix_ctr;
			break;
		   case 4: 
			fix_ctr_tbl = R1_STD4_fix_ctr;
			break;
		   case 5: 
			fix_ctr_tbl = R1_STD5_fix_ctr;
			break;
		   case 6: 
			fix_ctr_tbl = R1_STD6_fix_ctr;
			break;
		   case 7: 
			fix_ctr_tbl = R1_STD7_fix_ctr;
			break;
		   case 8: 
			fix_ctr_tbl = R1_WD1;
			break;
		   case 9: 
			fix_ctr_tbl = R1_WD2;
			break;
		   case 10: 
			fix_ctr_tbl = R1_WD3;
			break;
		   default:
			status = FAIL;
			break;
		}
		break;
	      default:
		status = FAIL;
		break;
	    }
	    return(status);
	}
}
