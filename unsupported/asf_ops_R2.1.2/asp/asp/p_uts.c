/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* p_uts.c -- processing utility routines */

#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <math.h>
#include <procfil.h>
#include <procdec.h>
#include <procpar.h>
#include <asp_msg.h>

double rsat_echo_delay[] = { 0.0, 7.0, 7.0, 8.0, 8.0, 8.0,
			9.0, 9.0, 8.0, 8.0, 8.0 };

extern TAPE_SEG_PTR seg_list;	/* tape segment list */

/* Parameter and data files */
extern PREAM_FILE_PTR pream_list; /* preamble file list */
extern POSTAM_FILE_PTR postam_list; /* postamble file list */
extern int pream_max;		/* preamble max file # */
extern int postam_max;		/* postamble max file # */

extern double stv[1200];	/* one-minute statevectors array */
extern int nstv;		/* number of statevectors */
extern GMT stv_gmt;		/* time of the first statevector */

extern RQST_PTR Cur_Rqst;	/* pointer to current job request */
extern SV sv1,sv2;		/* state vectors */
extern TC_FILE tcf;		/* time correlation element */

extern SP_FILE spf;		/* sensor parameters file */
extern DPP_FILE dpf;		/* default processing parameters file */
extern EM_FILE emf;		/* Earth model file */

extern TAPE_SEG_PTR seg_list;	/* tape segment list */

extern int vbose;		/* 1 = print debugging messages */
extern int sw_auto_scale;	/* 1 = perform automatic scaling */

extern float fmt_ratio;		/* actual formats / valid formats */


static double half_line;	/* Half of valid range pixels
					 after range compression */

extern char PROC_PATH[80];

/* stime_to_gmt(stime,gmt) ---------------------------------------------
	This routine converts satellite binary time to gmt.
*/

stime_to_gmt (stime,gmt)
	unsigned int stime;
	GMT_PTR gmt;
{
	double dtime,whole,fract;
	int temp;

    /* get offset from base time to desired time, in seconds */
	temp = stime - tcf.bt;
	dtime = temp;
    /* adding 0.35 nanoseconds to the s/c time interval for ERS-1 */
	if (Cur_Rqst->take_id[0] == 'E')
	    dtime *= (((double) tcf.delta + (double) 0.47) * (double) 1.0E-9);
	else
	    dtime *= ((double) tcf.delta * (double) 1.0E-9);
	whole = floor(dtime);
	fract = dtime - whole;
    /* calculate new gmt */
	*gmt = tcf.gmt;
	add_seconds(gmt,whole);
	add_seconds(gmt,fract);
}


/* get_attitude(sv,att) ------------------------------------------------
	This routine calculates the spacecraft attitude from the input
	statevector.
*/

get_attitude(sv,att)
	SV_PTR sv;
	RYP *att;
{
	double ypr[3];

      /* 7/24/96 For R & J */

      if ( Cur_Rqst->take_id[0] == 'R' || Cur_Rqst->take_id[0] == 'J') {
	att->yaw = 0.0;		
        att->pitch = 0.0;
        att->roll = 0.0;
        return;
      }
      else {
        if(sv1.rev < 3779 || sv1.rev > 3865) { /* test for roll-tilt mode */
		sv_attd(sv->pos.x,sv->pos.y,sv->pos.z,
			sv->vel.x,sv->vel.y,sv->vel.z,
			ypr);
		att->yaw = ypr[0];
		att->pitch = ypr[1];
		att->roll = ypr[2];
        }
        else { /* then in the roll-tilt mode */
		att->yaw = 0.0;
		att->pitch = 0.0;
		att->roll = 0.0;
	}
      }
}


/* get_range(sp,pp) ----------------------------------------------------
	This routine calculates the near, mid, and far range for the
	preprocessing region pointed to by pp, in the tape segment sp.
*/

get_range(sp,pp)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
{
	double prf,time,dwp,inc;
/*	double tick = 210.94E-9; dpf.b_dwp, ERS-1 specific clock rate */
	double echo_delay;		/* number of pulse in the air */
	double c = 2.99792458E8;	/* speed of light */

	half_line = (dpf.ns_ra - dpf.nchirp)/2;

	if (Cur_Rqst->take_id[0] == 'E') 
	    echo_delay = 9.0;
	else if ( Cur_Rqst->take_id[0] == 'R' ){
	    echo_delay = rsat_echo_delay[sp->aux.beam_seq];
	    if (vbose) printf("echo_delay %f\n",echo_delay);
	}
	else {
	    if (Cur_Rqst->take_id[0] == 'J')
		echo_delay = 7.0;
	    else
		printf("Unknown mode, echo_delay undefined!\n");
	}
	prf = sp->pre->prf;
	if (pp->dly1 < pp->dly2)
	    dwp = pp->dly1 * dpf.b_dwp - dpf.a_dwp;
	else 
	    dwp = pp->dly2 * dpf.b_dwp - dpf.a_dwp;
	inc = (half_line / spf.csr);
      /* test for roll-tilt mode for ERS-1 */
      if(sv1.rev < 3779 || sv1.rev > 3865 || Cur_Rqst->take_id[0] == 'J')
	time = (echo_delay / prf) + dwp;
      else    /* then in the roll-tilt mode so echo delay is 10.0 */
        time = ((echo_delay + 1.0) / prf) + dwp;
	pp->r_close = time * (c / 2.0);
	time += inc;
	pp->r_mid = time * (c / 2.0);
	time += inc;
	pp->r_far = time * (c / 2.0);
	pp->loc_mid = half_line;
	pp->loc_far = 2 * half_line;
    /*
	printf("near=%g, mid=%g, far=%g\n",pp->r_close,pp->r_mid,
	    pp->r_far);
    */
}


/* get_dops(sp,pp) -----------------------------------------------------
	This routine gets doppler parameters for the given preprocessing
	region, in tape segment sp.
*/

get_dops(sp,pp)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
{
	int istat;
	float gamma, fd[3], fdot[3];
	float xlambda,r_e,f,range;
	double sv[6],abc[3];
	int roll_tilt = 1;

    /* convert inputs to correct size and/or units */
	xlambda = spf.xlambda;
	r_e = emf.r_e * 1000.0;
	f = emf.f;
	sv[0] = pp->sv.pos.x * 1000.0;
	sv[1] = pp->sv.pos.y * 1000.0;
	sv[2] = pp->sv.pos.z * 1000.0;
	sv[3] = pp->sv.vel.x * 1000.0;
	sv[4] = pp->sv.vel.y * 1000.0;
	sv[5] = pp->sv.vel.z * 1000.0;
    /* print inputs (debugging only) */
   
	if (vbose) { 
	    	printf("Getting initial doppler params:\n");
		printf("r_e=%g, f=%g, xlambda=%g\n",r_e,f,xlambda);
		printf("roll=%g, yaw=%g, pitch=%g\n",pp->att.roll,
			pp->att.yaw,pp->att.pitch);
		printf("x=%g, y=%g, z=%g\n",sv[0],sv[1],sv[2]);
		printf("vx=%g, vy=%g, vz=%g\n",sv[3],sv[4],sv[5]);
  	}
 
    /* near range */
	istat = 0;
	range = (float) pp->r_close;
	if (vbose) printf("range= %f\n",range);
	initdops_(sv,&pp->att.roll,&pp->att.yaw,&pp->att.pitch,
		    &xlambda,&r_e,&f,&range,
		    &gamma,&fd[0],&fdot[0],&pp->swth_speed,
		    &pp->sat_speed,&pp->targ_speed,&pp->vrel,&istat);
	if (istat != PASS) {
	    printf("get_dops failed in near range\n");
	    return (FAIL);
	}
    /* far range */
	istat = 0;
	range = (float) pp->r_far;
	initdops_(sv,&pp->att.roll,&pp->att.yaw,&pp->att.pitch,
		    &xlambda,&r_e,&f,&range,
		    &gamma,&fd[2],&fdot[2],&pp->swth_speed,
		    &pp->sat_speed,&pp->targ_speed,&pp->vrel,&istat);
	if (istat != PASS) {
	    printf("get_dops failed in far range\n");
	    return (FAIL);
	}
    /* mid range */
	istat = 0;
	range = (float) pp->r_mid;
	initdops_(sv,&pp->att.roll,&pp->att.yaw,&pp->att.pitch,
		    &xlambda,&r_e,&f,&range,
		    &gamma,&fd[1],&fdot[1],&pp->swth_speed,
		    &pp->sat_speed,&pp->targ_speed,&pp->vrel,&istat);
	if (istat != PASS) {
	    printf("get_dops failed in mid range\n");
	    return (FAIL);
	}
    /* curve fit the 3 sets of doppler values to get a,b,c */
	if (p_get_init_dops(pp) == FAIL) {
	    fit_dops(fd,abc);

	/* Roll-Tilt mod or JERS, actually a correction to init dops */

	    if ((sv1.rev >= 3779 && sv1.rev <= 3865) || 
		     (Cur_Rqst->take_id[0] == 'J') || 
		     (Cur_Rqst->take_id[0] == 'R')) { /*add 2/14/96 */
		roll_tilt = -1;
		if (vbose) printf("we flip the Doppler!\n");
	    }
	    pp->fda = roll_tilt * abc[0];
	    pp->fdb = roll_tilt * abc[1];
	    pp->fdc = roll_tilt * abc[2];
	    fit_dops(fdot,abc);
	    if (Cur_Rqst->take_id[0] == 'R')
	        pp->fdota = abc[0]*1.01; /* Ming's fudge factor,3/7/97 */
	    else
	    	pp->fdota = abc[0];
	    if (Cur_Rqst->take_id[0] == 'J')
	    	pp->fdotb = abc[1]*0.86; /* Tom's F.F., 3/5/93 */
	    else
	    	pp->fdotb = abc[1];
	    pp->fdotc = abc[2];
	}
	if ( vbose ) {
	printf("Initial Doppler parameters:\n");
	printf("...(fda,fdb,fdc) = %g, %g, %g\n",pp->fda,pp->fdb,
		pp->fdc);
	printf("...(fdota,fdotb,fdotc)= %g, %g, %g\n",pp->fdota,
		pp->fdotb,pp->fdotc);
	}
	return (PASS);
}


/* get_corn_loc(sp,pp,start_gmt,nfmts,r3p,fd3p,fr3p,deskew, ------------
		fd_adj,a,b,c,d,e,istat)
	This routine calls the corn_loc routine and returns the
	lat/lon of the 4 corners of the image and the image center.
*/

get_corn_loc(sp,pp,start_gmt,nfmts,r3p,fd3p,fr3p,deskew,fd_adj,
		a,b,c,d,e,istat)
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	GMT start_gmt;
	int nfmts,deskew;
	double r3p[3];
	float fd3p[3],fr3p[3];
	double a[3],b[3],c[3],d[3],e[3],fd_adj;
	int *istat;
{
	double prf,pri;
	double re,rp,svec[6];
	char *getcwd(), cur_dir[80];
	int i;

	prf = sp->pre->prf;
	pri = 1.0 / prf;
	svec[0] = pp->sv.pos.x * 1000.0;
	svec[1] = pp->sv.pos.y * 1000.0;
	svec[2] = pp->sv.pos.z * 1000.0;
	svec[3] = pp->sv.vel.x * 1000.0;
	svec[4] = pp->sv.vel.y * 1000.0;
	svec[5] = pp->sv.vel.z * 1000.0;
	re = emf.r_e * 1000.0;
	rp = emf.r_pole * 1000.0;
	if (strcmp(Cur_Rqst->type,"STD")) {
	    re += Cur_Rqst->ave_hght * 1000.0;
	    rp += Cur_Rqst->ave_hght * 1000.0;
	}
	deskew = !deskew;
	if ( vbose ){

	printf("INPUTS TO CORN_LOC:\n");
	printf("  pos x,y,z = %.12g %.12g %.12g\n",
		svec[0],svec[1],svec[2]);
	printf("  vel x,y,z = %.12g %.12g %.12g\n",
		svec[3],svec[4],svec[5]);
	printf("  svec gmt  = %.4d:%.3d:%.2d:%.2d:%6.3f\n",
		pp->sv.gmt.yr,pp->sv.gmt.day,pp->sv.gmt.hr,
		pp->sv.gmt.min,pp->sv.gmt.second);
	printf("  start gmt = %.4d:%.3d:%.2d:%.2d:%6.3f\n",
		start_gmt.yr,start_gmt.day,start_gmt.hr,
		start_gmt.min,start_gmt.second);
	printf("  pri       = %.12g\n",pri);
	printf("  nfmts     = %d\n",nfmts);
	printf("  r3p       = %.12g %.12g %.12g\n",
		r3p[0],r3p[1],r3p[2]);
	printf("  fd3p      = %g %g %g\n",fd3p[0],fd3p[1],fd3p[2]);
	printf("  fr3p      = %g %g %g\n",fr3p[0],fr3p[1],fr3p[2]);
	printf("  re        = %.12g\n",re);
	printf("  rp        = %.12g\n",rp);
	printf("  xlambda   = %.12g\n",spf.xlambda);
	printf("  deskew    = %d\n",deskew);
	printf("  fd_adj    = %g\n",fd_adj);

	} 
	strcpy( cur_dir, getcwd(cur_dir,80));
	chdir( PROC_PATH );
	corn_loc(
	    /* inputs (should be 13 of 'em) */
		svec,pp->sv.gmt,start_gmt,pri,nfmts,
		r3p,fd3p,fr3p,re,rp,spf.xlambda,deskew,fd_adj,
	    /* outputs (should be 6 of 'em) */
		a,b,c,d,e,istat);

/* Save arrays a, b, c, d, e into five corner location in PP_BLOCK */
/* This is for later writing to scan results file (ODL) */
/*
	for ( i = 0; i < 3; i++ ){
		pp->near_start[i] = a[i];
		pp->far_start[i] = b[i];
		pp->center[i] = c[i];
		pp->near_end[i] = d[i];
		pp->far_end[i] = e[i];
	}
*/
	chdir( cur_dir );
}


/* fit_dops(y,abc) -----------------------------------------------------
	This routine accepts 3 ordinate values, assumed to be spaced at
	half-line intervals, and returns the quadratic coefficients 
	(a=constant, b=linear, c=quadratic) of a line through the 
	points.
*/

fit_dops(y,abc)
	float *y;
	double *abc;
{
	double xi[3][3];
	int i,j;

	if (p_get_mem_file(xi,36,DEF_PATH,"x_inverse") == FAIL) { 
	    printf("fit_dops: cannot open x_inverse file\n");
	    return (FAIL);
	}

	for (i = 0; i < 3; i++) {
	    abc[i] = 0.0;
	    for (j = 0; j < 3; j++)
		abc[i] += (double) (y[j] * xi[i][j]);
	}
	return (PASS);
}


/* make_def_path(type,sensor_id,path) ----------------------------------
	This routine builds the default files directory path for
	the job type and sensor id given.
*/

make_def_path(type,sensor_id,path)
	char *type, *sensor_id, *path;
{
	sprintf(path,"%sdefault.%2.2s/%3.3s/",PROC_PATH,sensor_id,type);
}


/* get_fmt_time(sp,fmt,gmt) --------------------------------------------
	This routine calculates the time associated with format number
	fmt, and returns that value as a Greenwich mean time in gmt.
	NOTE: before this routine may be used, the time reference must
	have been calculated and stored in the global time reference
	variables.
*/

get_fmt_time(sp,fmt,gmt)
	TAPE_SEG_PTR sp;
	int fmt;
	GMT *gmt;
{
	double seconds;

	seconds = (fmt - sp->pre->time_ref_fmt) / sp->pre->prf;
	*gmt = sp->pre->time_ref_gmt;
	add_seconds(gmt,seconds);
}

/* get_time_fmt(sp,gmt,fmt) --------------------------------------------
	This routine reads a Greenwich Mean Time value and calculates
	the format number corresponding to that time.
	NOTE: before this routine may be used, the time reference must
	have been calculated and stored in the global time reference
	variables.
*/

get_time_fmt(sp,gmt,fmt)
	TAPE_SEG_PTR sp;
	GMT *gmt;
	int *fmt;
{
	double seconds;
	double get_gmt_diff();

	seconds = get_gmt_diff(gmt,&sp->pre->time_ref_gmt);
	*fmt = sp->pre->time_ref_fmt + (seconds * sp->pre->prf) + 0.5;
}


/* dump_sv(sv) ---------------------------------------------------------
	This routine prints the statevector pointed to by sv.
*/

dump_sv(sv)
	SV_PTR sv;
{
	printf("  REV = %d\n",sv->rev);
	printf("  POS(x,y,z) = (%g, %g, %g)\n",sv->pos.x, sv->pos.y,
						sv->pos.z);
	printf("  VEL(x,y,z) = (%g, %g, %g)\n",sv->vel.x,
						sv->vel.y, sv->vel.z);
	printf("  GMT TIME = ");
	dump_gmt(&sv->gmt);
	printf("  BINARY TIME = %d\n",sv->time);
}


/* zero_bytes(bp,len) --------------------------------------------
	This routine sets len bytes to zero, starting at the
	given pointer location bp.
*/

zero_bytes(bp,len)
	char *bp;
	int len;
{
	while ((len--) > 0)
	    *bp++ = 0;
}


/* get_image_id(type,id) -----------------------------------------------
	This routine creates an ascii string containing the next
	available image id, and returns that string in "id".
	    "type" is an input string containing "STD", "RPR", "LOW",
	"QLK", "CPX" or "CSD", indicating which type of image is being
	identified.
	    The routine uses the disk file PROC_PATH/image_seq
	to maintain the image id sequence number.  Each time this
	routine is called, the current number is read from the
	file, and the next number is written to the file.
*/

get_image_id(type,id)
	char *type,*id;
{
	FILE *op;
	int t,seq;
	char filename[80],sat_ID[5];

    /* open master image sequence file */
	strncpy(sat_ID,Cur_Rqst->id,1);
	sat_ID[1] = 0;
	if (sat_ID[0] == 'E')
	    sprintf(filename,"%simage_seq",PROC_PATH);
	else
	    sprintf(filename,"%simage_seq.%s",PROC_PATH,sat_ID);

	if ((op = fopen(filename,"r+")) == NULL) {
	    printf("cannot open %s file\n",filename);
	    return (FAIL);
	}

    /* read master image sequence number */
	fscanf(op,"%d",&seq);

    /* format image id in return string variable */
	t = 0;
	if (strcmp(type,"STD") == 0)
	    t = 1;
	if (strcmp(type,"RPR") == 0)
	    t = 1;
	if (strcmp(type,"QLK") == 0)
	    t = 1;
	if (strcmp(type,"LOW") == 0)
	    t = 2;
	if (strcmp(type,"CPX") == 0)
	    t = 5;
	if (strcmp(type,"CSD") == 0)
	    t = 6;
	sprintf(id,"I%d%d00",seq,t);

    /* write new sequence number to file */
	rewind(op);
	fprintf(op,"%7.7d\n",seq+1);
	fclose(op);
	return (PASS);
}


/* set_gain(pp,gain) ---------------------------------------------------
	This routine adjusts the processing parameters to reflect 
	the requested gain.  The parameters are assumed to start out
	in a configuration with a total gain as specified in the
	default processing parameters file.
	NOTE: This routine currently only affects the AZINT
	detector shift value.

	M. Chen, 5/25/92, Minimum detshift set to 1.
*/

set_gain(pp,gain)
	PP_BLOCK_PTR pp;
	int gain;
{
	int sgain,dgain;
	int maxgain,mingain;

        /* If current type is CPX or CSD, do not set gain */
	if (strcmp(Cur_Rqst->type,"CPX") == 0) return; 
	if (strcmp(Cur_Rqst->type,"CSD") == 0) return; 

	if (gain == -99 || ap_detshift == -1)
	    return;
	sgain = gain - dpf.pro_gain;
	maxgain = 3 * ap_detshift;
	mingain = maxgain - 48;
	dgain = 0;
	if (sgain > maxgain) {
	    dgain = sgain - maxgain;
	    printf("requested gain too high, set to %d\n",gain - dgain);
	    sgain = maxgain;
	}
	if (sgain < mingain) {
	    dgain = mingain - sgain;
	    printf("requested gain too low, set to %d\n",gain + dgain);
	    sgain = mingain;
	}
	if (sgain <= 0) {
	   ap_detshift -= ((sgain - 1) / 3);
	   pp->pro_gain += dpf.pro_gain + ((sgain-1)/3)*3;
	}
	else {
	   if (sgain >= 2) {
	   	ap_detshift -= ((sgain + 1) / 3);
		pp->pro_gain += dpf.pro_gain + ((sgain+1)/3)*3;
	   }
	   else {
		ap_detshift -= 1;
		pp->pro_gain += dpf.pro_gain + 3;
	   }
	}
	while (ap_detshift < 1) {
	    ap_detshift++;
	    pp->pro_gain -= 3;
	}
	pp->det_sc = ap_detshift;
       	printf("detshift = %d, pro_gain = %d\n", pp->det_sc, pp->pro_gain);
}


/* p_make_lookup_tbl(pp,data) ------------------------------------------
	This routine constructs the input lookup table for preprocessing
	region pp, and loads it into the array 'data'.  If I and Q means
	and standard deviations are within tolerance, the standard table
	is used.  If not, a new table is calculated using the lookup_ers
	routine.
*/

p_make_lookup_tbl(pp,data)
	PP_BLOCK_PTR pp;
	short int data[];
{
	char filename[80];
	float mean_zero,imean_var,qmean_var,stdv_diff;
	float bias[2],gain[2];

	mean_zero = (float) ((1 << spf.nbits) - 1) / 2.0;
	imean_var = fabs(mean_zero - pp->imean);
	qmean_var = fabs(mean_zero - pp->qmean);
	stdv_diff = 0.0;
	if((pp->istdv + pp->qstdv) != 0.0 )
	  stdv_diff = fabs(pp->istdv - pp->qstdv) 
		    / ((pp->istdv + pp->qstdv) / 2.0);
	if(pp->iqrephase == 0.0) pp->iqrephase = 90.0;
	if ((imean_var > dpf.lmean_tol) || (qmean_var > dpf.lmean_tol)
		    || (stdv_diff > dpf.lstdv_diff_max)
		    || (fabs(pp->iqrephase - 90.0) > 10.0) ) {
	/* make an adjusted table */
	    bias[0] = pp->imean;
	    bias[1] = pp->qmean;
	    gain[0] = pp->istdv;
	    gain[1] = pp->qstdv;
	    lookup_ers(bias,gain,pp->iqrephase,spf.nbits,data);
	}
	else {
	/* use the standard table */
	    sprintf(filename,"%sin_lut_file",DEF_PATH);
	    p_get_mem_file(data,8192,filename,"");
	}
}


/* make_statevectors() -------------------------------------------------
	This routine builds the one-minute statevector table for the
	current job request, and writes it to the job directory.
*/

make_statevectors()
{
	double x,y,z,x_vel,y_vel,z_vel;
	GMT start_gmt, end_gmt;

	printf("Propagating statevectors...\n");
    /* get the statevector parameters from the job request */
	x = sv1.pos.x;
	y = sv1.pos.y;
	z = sv1.pos.z;
	x_vel = sv1.vel.x;
	y_vel = sv1.vel.y;
	z_vel = sv1.vel.z;
    /* calculate the ending time: start time + 100 minutes */
    /*   modify so end time is one minute after datatake   */
    /*   end time for request type other than quick look.  */
	stv_gmt = sv1.gmt;
	if (strcmp(Cur_Rqst->type,"QLK") == 0) {
	  end_gmt = sv1.gmt;
	  add_seconds(&end_gmt,6000.0);
	}
	else {
	  end_gmt = Cur_Rqst->end;
	  add_seconds(&end_gmt,60.0);
	}
    /* zero out the stv array (so we can count entries later) */
	for (nstv = 0; nstv < 1200; nstv++)
	    stv[nstv] = 0.0;
    /* produce the statevectors (in the stv array) */
	chdir(PROC_PATH);
	predict (x,y,z,x_vel,y_vel,z_vel, &sv1.gmt, &end_gmt, stv);
    /* count the statevectors */
	for (nstv = 0; nstv < 200; nstv++) {
	    x = stv[6*nstv  ];
	    y = stv[6*nstv+1];
	    z = stv[6*nstv+2];
	    if (x == 0.0 && y == 0.0 && z == 0.0)
		break;
	}
    /* write the statevectors to disk */
	chdir(JOB_PATH);
	p_rw_sv_file(1,"statevectors");
}


/* stv_prop(time,pp) ---------------------------------------------------
	This routine propagates the initial statevector sv1, from the
	current job request, by 'time' seconds, and stores the result
	in the preprocessing region pointed to by pp.
	The one-minute statevector array is used to provide the nearest
	ASAP-propagated statevector for a reference.
*/

stv_prop(time,pp)
	double time;
	PP_BLOCK_PTR pp;
{
	double tstep = 0.1;
	int s;

    /* choose which statevector to use */
	if (time <= 0.0)
	    s = 0;
	else
	    s = (int) ((time + 30.0) / 60.0);
	if (s > nstv)
	    s = nstv;
    /* calculate the time delta from the chosen statevector */
	time -= s * 60.0;
	s *= 6;
	if (vbose) printf("time=%g, s=%d\n", time,s);
	if (fabs(time) > 90.0 && Cur_Rqst->id[9] != 'P'){
	    asp_msg(LOG_ERR,asp_messages[SV_TOO_FAR]);
	    return(FAIL);
	}
    /* NOTE: newtprop is a FORTRAN call */
	newtprop_(&stv[s],&time,&tstep,&pp->sv.pos);
}


/* get_abcd_dist(a,b,d,e,ad,be,ae,bd) ----------------------------------
	This routine takes as input the lat,lon and earth radius of the
	4 corners of an image, and returns the along track distances and
	diagonal distances in the image, in kilometers.
*/

get_abcd_dist(a,b,d,e,ad,be,ae,bd)
	double a[3],b[3],d[3],e[3];
	double *ad,*be,*ae,*bd;
{
	double axyz[3],bxyz[3],dxyz[3],exyz[3];

	calc_xyz(a[0],a[1],axyz);
	calc_xyz(b[0],b[1],bxyz);
	calc_xyz(d[0],d[1],dxyz);
	calc_xyz(e[0],e[1],exyz);
	get_xyz_dist(axyz,dxyz,ad);
	get_xyz_dist(bxyz,exyz,be);
	get_xyz_dist(axyz,exyz,ae);
	get_xyz_dist(bxyz,dxyz,bd);
}


/* calc_xyz(lat,lon,xyz) -----------------------------------------------
	This routine calculates global x,y,z cartesian coordinates
	from the given latitude and longitude.
*/

calc_xyz(lat,lon,xyz)
	double lat,lon,xyz[3];
{
	double glat,re,req,f;
	double pi = 3.141592653589793;
	double deg_to_rad = pi / 180.0;

	f = 1.0 / emf.f;
	req = emf.r_e * 1000.0;
	glat = atan(tan(lat * deg_to_rad) * (1.0 - f) * (1.0 - f));
	re = (req * (1.0 - f)) 
		/ sqrt(1.0 - (2.0 - f) * f * cos(glat) * cos(glat));
	xyz[0] = re * cos(glat) * cos(lon * deg_to_rad);
	xyz[1] = re * cos(glat) * sin(lon * deg_to_rad);
	xyz[2] = re * sin(glat);
}


/* get_xyz_dist(a,b,ab) ------------------------------------------------
	This routine calculates the distance between the two 3-dimen-
	sional points a and b, and returns it in ab.
*/

get_xyz_dist(a,b,ab)
	double a[3],b[3];
	double *ab;
{
	double x,y,z;

	x = a[0] - b[0];
	y = a[1] - b[1];
	z = a[2] - b[2];
	*ab = sqrt(x*x + y*y + z*z);
}
