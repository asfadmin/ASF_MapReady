/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <stdio.h>
#include <math.h>
#include <aspdecl.h>
#include <procfil.h>

#define BYTES_BLOCK  4356       /* bytes per DCRSi block */

/* system configuration parameters */
extern int server_hw;
extern int vbose;               /* 1 = print debugging messages */
extern SV sv1,sv2;              /* current request's statevectors */
extern RQST_PTR Cur_Rqst;       /* pointer to current job request */
extern SP_FILE spf;             /* sensor parameters file */

float prf;                      /* pulse rep freq */
extern int fst_len;             /* 1st fmt length after srch-sync */
extern int fmt_len;             /* avg fmt length(bit) for its prf */
extern int fst_len24;           /* 1st fmt length after srch-sync minus 24 */

/* j1_make_pprs(sp,segment) -----------------------------------------------
	This routine creates preprocessing data blocks for each
	preprocessing region required in the given segment.
*/

j1_make_pprs(sp,segment)
	TAPE_SEG_PTR sp;
	int segment;
{
	PP_BLOCK_PTR pp,pp2;
	int k,w,format,dformats,dformats2;
	double time,dtime;
	double get_gmt_diff();
	int end_sw = 0;
	int first = 1;
	int block,offset;
	int first_format, last_format;
	GMT start_gmt;
	double a[3],b[3],c[3],d[3],e[3];
	double r3p[3];
	float fd3p[3],fr3p[3],t_bit;
	int istat,tfmts,fmt_avil,spacing;

	prf = sp->pre->prf;
	if (segment == 1) {
	    format = j1_valid_agc(sp);
	    if (format > sp->ppfmt[0]) {
	    	printf("ZERO AGC point to fmt %d, was %d\n",format,sp->ppfmt[0]);
	    	sp->ppfmt[0] = format;
	    	j1_get_fmt_loc(1,sp,format,&fst_len,&block,&offset);
	    	sp->ppblk[0] = block;
	    	sp->ppoff[0] = offset;
	    }
	}
	if ((sp->fmt_end - sp->ppfmt[0]) < 30000) {
	    printf("...not enough data to process\n");
	    return (FAIL);
	}

    /* re-spacing regions, 12/2/92, M. Chen */
	if (strcmp(Cur_Rqst->type,"STD") == 0) {
	    fmt_avil = sp->fmt_end - 35000 - sp->ppfmt[0];
	    sp->pptotal = (int)(fmt_avil / 20000.) + 1;	
	    spacing = 8 * (int)(fmt_avil / (8*sp->pptotal));
	    if (spacing <= 16000 && sp->pptotal > 1) {
		printf("fmt_avil = %d, s = %d. NO GOOD! use 20K\n");
		spacing = 20000;
	    }
	    else
		sp->pptotal += 1;
	    for (k = 1; k < sp->pptotal; k++) {
	    	sp->ppfmt[k] = sp->ppfmt[0] + spacing*k;
		t_bit = (float)k * spacing * fst_len;
	    	sp->ppblk[k] = sp->ppblk[0] + 
		   (int)((sp->ppoff[0]+t_bit)/(BYTES_BLOCK*8));
	    }
	}
	if (strcmp(Cur_Rqst->type,"QLK") == 0) {
	    if (first) {
	    	format = sp->ppfmt[0];
	    	j1_get_fmt_loc(1,sp,format,&fst_len,&block,&offset);
	    	sp->ppblk[0] = block;
	    	sp->ppoff[0] = offset;
	    }
	    fmt_avil = sp->fmt_end - 30000 - sp->ppfmt[0];
	    sp->pptotal = (int)(fmt_avil / 20000.) + 1;	
	    spacing = 8 * (int)(fmt_avil / (8*sp->pptotal));
	    if (spacing <= 16000 && sp->pptotal > 1) {
		printf("fmt_avil = %d, s = %d. NO GOOD! use 20K\n");
		spacing = 20000;
	    }
	    else
		sp->pptotal += 1;
	    for (k = 1; k < sp->pptotal; k++) {
	    	sp->ppfmt[k] = sp->ppfmt[0] + spacing*k;
		t_bit = (float)k * spacing * fst_len;
	    	sp->ppblk[k] = sp->ppblk[0] + 
		   (int)((sp->ppoff[0]+t_bit)/(BYTES_BLOCK*8));
	    }
	}

	for (k = 0; k < sp->pptotal; k++) {
	    if ((pp = (PP_BLOCK_PTR) malloc(sizeof(PP_BLOCK))) == NULL) {
		printf("Out of memory in j1_make_pprs\n");
		return (FAIL);
	    }
	    p_init_pp_block(pp);
	    if (sp->ppb_list == NULL)
		sp->ppb_list = pp;
	    else
		pp2->nxt_ppb = pp;
	    pp2 = pp;
	    pp->nxt_ppb = NULL;
	    pp->fmt_start = sp->ppfmt[k];
	    pp->blk_start = sp->ppblk[k];
	    pp->bit_off = sp->ppoff[k];
	    pp->polarity = sp->polarity;
	    pp->quality = -1;

	    if (vbose) printf("k=%d, blk_start=%d, bit_off=%d\n", k, pp->blk_start, pp->bit_off);

	/* calculate the first and last format within a region */
	    first_format = pp->fmt_start;
	    last_format  = first_format + 29000.;
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
	    get_fmt_time(sp,first_format,&pp->sv.gmt);
	    time = get_gmt_diff(&pp->sv.gmt,&sv1.gmt);
	    if(stv_prop(time,pp)==FAIL) return(FAIL);
	/* calculate spacecraft attitude */
	    get_attitude(&pp->sv,&pp->att);
/* for JERS1 */
	    pp->att.yaw = 0.0;
	    pp->att.pitch = 0.0;
	    pp->att.roll = 0.0;
	    get_range(sp,pp);

	/* per Dave, assign default noise value here, 2/21/92 */
	    pp->noise = spf.noise;
	/* get initial doppler parameters */
	    if (get_dops(sp,pp) != PASS)
		return (FAIL);
	    if (vbose)
		printf("...swath speed = %g\n",pp->swth_speed);

	if (first) {
	    first = 0;
	    if ((format = j1_locate_ppr(sp,pp)) == FAIL)
		return (FAIL);
	    if (format < sp->fmt_start || format > (sp->fmt_end-2056)) {
		printf("...image not within this segment\n");
		sp->ppb_list = NULL;
		return (PASS);
		}
	/* for all modes, use format number to determine the correct
	   data window position */
/*
	    if (strcmp(Cur_Rqst->type,"STD") != 0
	      && strcmp(Cur_Rqst->type,"QLK") != 0) {
	      first_format = format - 8*prf;
	      last_format  = format + 12*prf;
*/
	      first_format = format;
	      last_format  = format + 29000.;
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
	/* re-sync the starting location if not STD or QLK job */
	    if (strcmp(Cur_Rqst->type,"STD") != 0 &&
		strcmp(Cur_Rqst->type,"QLK") != 0) {
	        pp->fmt_start = format;
		j1_get_fmt_loc(1,sp,format,&fst_len,&block,&offset);
	        pp->blk_start = block;
	        pp->bit_off = offset;
	    }
	/* propagate statevector to this region start */
	    get_fmt_time(sp,format,&pp->sv.gmt);
	    time = get_gmt_diff(&pp->sv.gmt,&sv1.gmt);
	    if(stv_prop(time,pp)==FAIL) return(FAIL);
	/* calculate spacecraft attitude */
	    get_attitude(&pp->sv,&pp->att);
/* for JERS1 */
	    pp->att.yaw = 0.0;
	    pp->att.pitch = 0.0;
	    pp->att.roll = 0.0;
	    get_range(sp,pp);

	/* per Dave, assign default noise value here, 2/21/92 */
	    pp->noise = spf.noise;
	/* get initial doppler parameters */
	    if (get_dops(sp,pp) != PASS)
		return (FAIL);
	} /* end if first */

    /* call corn_loc with estimated start time, to get
       rough lat and long at scene center */
	start_gmt = pp->sv.gmt;
	add_seconds(&start_gmt, 3.0);
	tfmts = (102400.0 / pp->swth_speed) * prf;
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
	get_corn_loc(sp,pp,start_gmt,tfmts,r3p,fd3p,fr3p,0,0.0,
		     a,b,c,d,e,&istat);
	pp->lat_rough = c[0];
	pp->lon_rough = c[1];

	    if (vbose)
		printf("...creating region %d at format %d\n",k+1,format);
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
		    break; /* ??? who use this last format ??? */
	    }
	    fflush(stdout);
	}  /* while */
	printf("...created %d regions\n",sp->pptotal);
	return (PASS);
}
/* j1_valid_agc(sp) -----------------------------------------------
	This routine check agc values and associated format numbers,
	search for first valid agc = 0 (last for more than 10k formats)
	in first 50k formats.  If found, the format number is returned,
	otherwise, return sp->ppfmt[0].
*/

j1_valid_agc(sp)
	TAPE_SEG_PTR sp;
{
	int j, afmt_old;

	if (vbose) printf("Search valid zero agc\n");
	afmt_old = sp->afmt[0];
	for (j = 0; j < sp->agc_count-1; j++) {
	    if (sp->agc[j] == 0 && (sp->afmt[j+1] - afmt_old) > 10000)
		return(sp->afmt[j]);
	    else {
	    	afmt_old = sp->afmt[j+1];
	    	if ((afmt_old - sp->afmt[0]) > 50000)
		    return(sp->ppfmt[0]);
	    }
	}
	return(sp->ppfmt[0]);
}
/* j1_locate_ppr(sp,pp) ------------------------------------------------
	This routine calculates the location of the first preprocessing
	region.
*/

j1_locate_ppr(sp,pp)
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
	float fd3p[3],fr3p[3];
	int format,istat,tfmts,nfmts;
	double get_gmt_diff(), cos2();

	deg_to_rad = pi / 180.0;
	format = sp->ppfmt[0];
    /* if standard request, start at front of take */
	if (strcmp(Cur_Rqst->type,"STD") == 0) {
	    return (format);
	}
    /* if quick look request, ask user to select area of interest */
    /*   The question has already been asked and need not be
	 asked again. (done in select_segment call           */
    /* I think we do not have to advance format for half image ??? */
	if (strcmp(Cur_Rqst->type,"QLK") == 0) {
/*
	    format += (int) ((44800.0 / pp->swth_speed) 
				* sp->pre->prf + 0.5);
*/
	    return (format);
	}
    /* if time was given, use it to calculate start */
	prf = sp->pre->prf;
	if (Cur_Rqst->targ.yr) {
	if (vbose) printf("use time for format start: year= %d\n",
		Cur_Rqst->targ.yr);
	    get_time_fmt(sp,&Cur_Rqst->targ,&format);
	    format -= (int) ((51200.0 / pp->swth_speed) 
				* sp->pre->prf + 0.5);
	    format -= 3300; /* Ming assume average np_delay */
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
	fr3p[0] = -660.0;
	fr3p[1] = -660.0;
	fr3p[2] = -660.0;
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
	if (distance < 0.0) 
	   printf("...distance is negative = %g\n",distance);
    /* calculate the format number of the requested location */
	time = distance / pp->swth_speed;
	nfmts = time * prf;
	format += nfmts;
	format -= (int) ((51200.0 / pp->swth_speed) 
				* sp->pre->prf + 0.5);
	format -= 3300; /* Ming assume average np_delay */
    /* remove the test for going beyond end of datatake
	maxfmt = sp->fmt_end;
	maxfmt -= 2056;
	if (format > maxfmt)
	    format = maxfmt;
    */
	return (format);
}
