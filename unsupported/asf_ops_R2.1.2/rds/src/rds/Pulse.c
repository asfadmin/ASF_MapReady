/*============================================================================
 |  @(#)Pulse.c	1.107 98/04/06 10:42:27
 |
 |  Generic Pulse Functions
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *===========================================================================*/
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <errno.h>
#include "version.h"
#include "Tape.h"
#include "Pulse.h"

static const char sccsid_Pulse_c[] =
        "@(#)Pulse.c	1.107 98/04/06 10:42:27";

#define	NO_DESKEW	1
#define R32(_x)		RoundUp(_x, sizeof(int))

double	nominal_swath[BEAM_MAX];
double	window_duration_error;
LookTable_t looktable[LOOKTABLE_MAX];

/*---------------------------------------------------------------------------*
 |  The following functions are provided in the section archived SAR library
 *---------------------------------------------------------------------------*/
void	initdops_();
void	newtprop_(double svec[6], double* t, double* step, double propvec[6]);
void	sv_attd(double x, double y, double z, double xv, double yv, double zv,
		double attitude[3]);
struct	{double ti, tf, tr;} time_;	/* Common block used by SAR library */

/*---------------------------------------------------------------------------*
 |  NAME
 |	gmt_diff
 |
 |  SYPNOSIS
 |	double gmt_diff(GMT* t1, GMT* t2)	
 |
 |  DESCRIPTION
 |      This routine returns the difference, in seconds, between the
 |      GMT times t1 and t2. (t1 - t2)
 |
 *---------------------------------------------------------------------------*/
double gmt_diff(const GMT* t1, const GMT* t2)
{
    return
       ((t1->yr  - t2->yr ) * (365*24*60*60) +
	(t1->day - t2->day) * (24*60*60) +
	(t1->hr  - t2->hr ) * (60*60) +
	(t1->min - t2->min) * 60 +
	/* adjust for leap year */
	(((t1->yr - 1) / 4) - ((t2->yr - 1) / 4)) * (24*60*60)) +
       ((double)t1->second - (double) t2->second);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |	gmt_add
 |
 |  SYPNOSIS
 |	void gmt_add(GMT* gmt, double sec)
 |
 |  DESCRIPTION
 |	This routine adds sec seconds to the given gmt record.
 |
 *---------------------------------------------------------------------------*/
void gmt_add(GMT* gmt, double sec)
{
    register int days_in_year;
    register double dseconds = sec + gmt->second;

    if (dseconds > 0.0) {
	days_in_year = 365 + ((gmt->yr % 4) == 0);
	while (dseconds >= 60.0) {
	    dseconds -= 60.0;
	    if ((++gmt->min) >= 60) {
		gmt->min = 0;
		if ((++gmt->hr) >= 24) {
		    gmt->hr = 0;
		    if ((++gmt->day) > days_in_year) {
			gmt->yr++;
			gmt->day = 1;
		    }  /* if day */
		}  /* if hr */
	    }  /* if min */
	}  /* while second */
     }
     else {
	days_in_year = 365 + (((gmt->yr - 1) % 4) == 0);
	while ((dseconds) < 0.0) {
	    dseconds += 60.0;
	    if ((--gmt->min) < 0) {
		gmt->min = 59;
		if ((--gmt->hr) < 0) {
		    gmt->hr = 23;
		    if ((--gmt->day) < 1) {
			gmt->yr--;
			gmt->day = days_in_year;
		    }  /* if day */
		}  /* if hr */
	    }  /* if min */
	}  /* while second */
    }
    gmt->second = dseconds;
}
/*---------------------------------------------------------------------------*
 |  NAME
 |	corn_loc
 |
 |  SYPNOSIS
 |	void corn_loc(...)
 |
 |  DESCRIPTION
 |	This routine calculates the four corners of the image
 |	by returning the geocentric latitude, the geocentric longitude
 |	as well as the radius of ellipsoidal earth at target.
 |
 |
 |			  NEAR RANGE
 |		+---------------------------+       ^
 |		|A                         D|       |
 |		|			    |       |
 |		|<---------nl lines-------->|       |
 |		|			    |       |
 |		|                           |    np pixels
 |		|                           |       |
 |         EARLY|             C             | LATE  |
 |		|                           |       |
 |		|                           |       |
 |		|         deskewed          |       |
 |		|                           |       |
 |		|                           |       |
 |		|B                         E|       |
 |		+---------------------------+       X
 |
 | 			FAR RANGE
 |
 |  INPUTS:
 |	double	sv_ref;
 |		Input state vectors consisting of six elements namely,
 |		the position vectors x,y,z in meters together with the
 |		corresponding velocity vectors vx,vy vz in meters/sec
 |		respectively.
 |
 |	GMT*	t_ref;
 |		GMT time with state vector in year,day,hour,min,sec
 |
 |	GMT*	t_start;
 |		GMT of first range line
 |
 |	double	delta;
 |		Time between range lines in seconds
 |
 |	int	nl;
 |		Total number of range lines (not counting 't_start' line)
 |
 |	double	r3points[3];
 |		Three element vector containing the slant range to
 |              the near, the midle and the far points (meter).
 |
 |	float	fd3points[3];
 |		Three element vector containing the Doppler centroid
 |              frequencies of the near, the midle and the far points (Hz). 
 |
 |	float	fr3points[3];
 |		Three element vector containing the Doppler frequency 
 |              rates of the near, the midle and the far points (Hz). 
 |
 |	double	re;
 |		Earth equatorial radius in meters
 |
 |	double	rp;
 |		Earth polar radius in meters
 |
 |	double	lambda;
 |		Wavelength in meters
 |
 |	int	zero_dop;
 |		FALSE if image is deskewed
 |		TRUE if image is not deskewed
 |
 |	double	fd_adj;
 |		Factor to calculate adjustment to locations due to 
 |		skew relative to zero doppler line throughout image.
 |		The skew is caused by change in spacecraft altitude.
 |
 |	int	leftlook;
 |		FALSE => right looking SAR
 |		TRUE  => left looking SAR
 |
 |  OUTPUTS:
 |	double	a[3];
 |		Three elements consisting of latitude,longitude and
 |		radius of ellipsoidal earth at target 'A'
 |
 |	double	b[3];
 |		Three elements consisting of latitude, longitude and
 |		radius of ellipsoidal earth at target 'B'
 |
 |	double	c[3];
 |		Three elements consisting of latitude, longitude and
 |		radius of ellipsoidal earth at target 'C'
 |
 |	double	d[3];
 |		Three elements consisting of latitude, longitude and
 |		radius of ellipsoidal earth at target 'D'
 |
 |	double	e[3];
 |		Three elements consisting of latitude, longitude and
 |		radius of ellipsoidal earth at target 'E'
 |
 |	int*	istat
 |		-1 indicates an error
 |		0  indicates successful operation
 |  HISTORY
 |	3/13/90: modified to enter the slant range, the fd and fr of 3
 |          different slant range points. (QDN)
 |	3/14/90: modified to pass the slant range , the fd and the fr
 |          as vectors for 3 points. (QDN)
 |	5/18/90: modified to consolidate GMT routines with other ASP s/w.
 |	5/29/90: modified to adjust for skew in image. (APS) 
 |	12/6/96: changed to RDS's GMT structure
 |
 *---------------------------------------------------------------------------*/
/* This routine breaks a standard GMT time into its component parts */

#define	time_brk(gmt_x,_year,_day,_hour,_min,_sec)\
{\
    *(_year) = (gmt_x)->yr;\
    *(_day)  = (gmt_x)->day;\
    *(_hour) = (gmt_x)->hr;\
    *(_min)  = (gmt_x)->min;\
    *(_sec)  = (gmt_x)->second;\
}
static
int corn_loc(
	int	leftlook, 
	double* sv_ref,
	GMT*    t_refp,
	GMT*    t_startp,
	double* r3points,
	float*	fd3points,
	float*	fr3points,
	int	nl,
	int	zero_dop,
	double	re,
	double	rp,
	double	lambda,
	double	fd_adj,
	double	delta,
	double*	a,
	double*	b,
	double*	c,
	double*	d,
	double*	e,
	double*	a_propvec,
	double*	b_propvec,
	double*	c_propvec,
	double*	d_propvec,
	double*	e_propvec,
	int*	istat)
{
    double fd_out,fdot_out,fd_a,fd_b,fd_c,fd_d,fd_e,
	fdot_a,fdot_b,fdot_c,dfot_d,fdot_e,	
	ta_diff,tb_diff,tc_diff,td_diff,te_diff,t_step,tt_sec,t_sec,
	r_near,r_mid,r_far;

    GMT	t_ab,t_cc,t_de,t_a,t_b,t_c,t_d,t_e,t_out,t_ref,t_start;
    float fd_near,fd_mid,fd_far,fr_near,fr_mid,fr_far,sec_diff,secs;
    int	i,years,days,hrs,mins;

    r_near = r3points[0]; r_mid = r3points[1]; r_far = r3points[2];
    fd_near = fd3points[0]; fd_mid = fd3points[1]; fd_far = fd3points[2];
    fr_near = fr3points[0]; fr_mid = fr3points[1]; fr_far = fr3points[2];

    t_ref = *t_refp;
    t_start = *t_startp;

    /* Calculate time associated with each pixel locations */
    t_ab = t_start;
    t_sec = delta*nl;

    t_cc = t_start;
    gmt_add(&t_cc, t_sec/2.0);

    t_de = t_start;
    gmt_add(&t_de, t_sec);

    /* Calculate time pixel at beam center */
    fd_a = fd_d = fd_near;
    fd_c = fd_mid;
    fd_b = fd_e = fd_far;

    t_a = t_ab;
    t_b = t_ab;
    t_c = t_cc;
    t_d = t_de;
    t_e = t_de;
	
    fdot_a = fr_near;
    fdot_c = fr_mid;
    fdot_b = fr_far;

    if (zero_dop == 0) {
	t_sec = -fd_a/fabs(fdot_a);
	gmt_add(&t_a, t_sec);
	gmt_add(&t_d, t_sec);

	t_sec = -fd_c/fabs(fdot_c);
	gmt_add(&t_c, t_sec);

	t_sec = -fd_b/fabs(fdot_b);
	gmt_add(&t_b, t_sec);
	gmt_add(&t_e, t_sec);
    }
    /* Calculate statevectors associated with the above times */
    ta_diff = gmt_diff(&t_a, &t_ref);
    tb_diff = gmt_diff(&t_b, &t_ref);
    tc_diff = gmt_diff(&t_c, &t_ref);
    td_diff = gmt_diff(&t_d, &t_ref);
    te_diff = gmt_diff(&t_e, &t_ref);

    t_step = 0.1;

    newtprop_(sv_ref, &ta_diff, &t_step, a_propvec);
    newtprop_(sv_ref, &tb_diff, &t_step, b_propvec);
    newtprop_(sv_ref, &tc_diff, &t_step, c_propvec);
    newtprop_(sv_ref, &td_diff, &t_step, d_propvec);
    newtprop_(sv_ref, &te_diff, &t_step, e_propvec);

    time_brk(&t_a, &years, &days, &hrs, &mins, &secs);
    getlocc_(&leftlook, a_propvec, &r_near, &fd_a, &re, &rp, &lambda,
	     &years, &days, &hrs, &mins, &secs, &a[0], &a[1], &a[2], istat);
    if (*istat != 0)
	return;

    time_brk(&t_b, &years, &days, &hrs, &mins, &secs);
    fd_b += fd_adj * fdot_b;
    getlocc_(&leftlook, b_propvec, &r_far, &fd_b, &re, &rp, &lambda,
	     &years, &days, &hrs, &mins, &secs, &b[0], &b[1], &b[2], istat);
    if (*istat != 0)
	return ;

    time_brk(&t_c, &years, &days, &hrs, &mins, &secs);
    fd_c += 0.5 * fd_adj * fdot_b;
    getlocc_(&leftlook, c_propvec, &r_mid, &fd_c, &re, &rp, &lambda,
	     &years, &days, &hrs, &mins, &secs, &c[0], &c[1], &c[2], istat);
    if(*istat != 0)
	return;

    time_brk(&t_d, &years, &days, &hrs, &mins, &secs);
    getlocc_(&leftlook, d_propvec, &r_near, &fd_d, &re, &rp, &lambda,
	     &years, &days, &hrs, &mins, &secs, &d[0], &d[1], &d[2], istat);
    if (*istat != 0)
	return;

    time_brk(&t_e, &years, &days, &hrs, &mins, &secs);
    fd_e += fd_adj * fdot_b;
    getlocc_(&leftlook, e_propvec, &r_far, &fd_e, &re, &rp, &lambda,
	     &years, &days, &hrs, &mins, &secs, &e[0], &e[1], &e[2], istat);
}
/*---------------------------------------------------------------------------*
 |  NAME
 |	InitBifDiffer - generate bit difference mapping table 
 |
 |  SYPNOSIS
 |	void InitBitDiffer (int *bitDiff);
 |
 |  DESCRIPTION
 |	This routine is used to generate the table 'diff' below.
 *---------------------------------------------------------------------------*/
/*  This bit difference table was generated by 'InitBitDiffer()' */
int diff[] = {
    0,  1,  1,  2,  1,  2,  2,  3,  1,  2,  2,  3,  2,  3,  3,  4,
    1,  2,  2,  3,  2,  3,  3,  4,  2,  3,  3,  4,  3,  4,  4,  5,
    1,  2,  2,  3,  2,  3,  3,  4,  2,  3,  3,  4,  3,  4,  4,  5,
    2,  3,  3,  4,  3,  4,  4,  5,  3,  4,  4,  5,  4,  5,  5,  6,
    1,  2,  2,  3,  2,  3,  3,  4,  2,  3,  3,  4,  3,  4,  4,  5,
    2,  3,  3,  4,  3,  4,  4,  5,  3,  4,  4,  5,  4,  5,  5,  6,
    2,  3,  3,  4,  3,  4,  4,  5,  3,  4,  4,  5,  4,  5,  5,  6,
    3,  4,  4,  5,  4,  5,  5,  6,  4,  5,  5,  6,  5,  6,  6,  7,
    1,  2,  2,  3,  2,  3,  3,  4,  2,  3,  3,  4,  3,  4,  4,  5,
    2,  3,  3,  4,  3,  4,  4,  5,  3,  4,  4,  5,  4,  5,  5,  6,
    2,  3,  3,  4,  3,  4,  4,  5,  3,  4,  4,  5,  4,  5,  5,  6,
    3,  4,  4,  5,  4,  5,  5,  6,  4,  5,  5,  6,  5,  6,  6,  7,
    2,  3,  3,  4,  3,  4,  4,  5,  3,  4,  4,  5,  4,  5,  5,  6,
    3,  4,  4,  5,  4,  5,  5,  6,  4,  5,  5,  6,  5,  6,  6,  7,
    3,  4,  4,  5,  4,  5,  5,  6,  4,  5,  5,  6,  5,  6,  6,  7,
    4,  5,  5,  6,  5,  6,  6,  7,  5,  6,  6,  7,  6,  7,  7,  8,
};
static
void InitBitDiffer(int* bifDiff)
{
    register int i, k;
    static int dif8[16] = { 0,1,1,2,1,2,2,3, 1,2,2,3,2,3,3,4 };

    for (i = 0; i < 256; ++i) {
	bifDiff[i] = 0;
	for (k = 0; k < 8; k+=4)  bifDiff[i] += dif8[(i >> k) & 0xF];
    }
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
U8* Unscramble(const U32** pn, U32* tp, U32 tx, U32 off, size_t len)
{
    register U32 *rp;
    register int  nl, nr, i, n;
    U8* p;

    nl = tx & 7;
    nr = 32 - nl;
    tx >>= 3;
    rp = (U32*) pn[tx];

    tx += off;
    rp += (tx >> 2);
    tp += (tx >> 2);
    tx &= 3;
    p = (U8*)tp + tx;
    i = -1;
    n = len - (4-tx);

    if (tx || n < 0) {
	tx = (~0U >> (tx<<3));
        if (n < 0) {
            tx &= (~0U << (-n<<3));
            tp[0] = (((nl ? ((tp[0]<<nl)|(tp[1]>>nr)) : tp[0]) ^ rp[0]) & tx) |
		    (tp[0] & ~tx);
            return p;
        }
        tp[0] = (((nl ? ((tp[0]<<nl)|(tp[1]>>nr)) : tp[0]) ^ rp[0]) & tx) |
		(tp[0] & ~tx);
        ++i;
    }
    tx = n & 3;
    n >>= 2;

    if (nl)
        while (++i <= n) tp[i] = ((tp[i]<<nl) | (tp[i+1]>>nr)) ^ rp[i];
    else
        while (++i <= n) tp[i] ^= rp[i];

    if (tx) {
	tx = (~0U << ((4-tx)<<3));
        tp[i] = (((nl ? ((tp[i]<<nl)|(tp[i+1]>>nr)) : tp[i]) ^ rp[i]) & tx) |
		(tp[i] & ~tx);
    }
    return p;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
#ifdef  NOSTDARG
int strcmpv (va_alist)
va_dcl
#else

int strcmpv (char *s0, char* s1, ...)
#endif
{
    int found = -1;
    va_list ap;
#ifdef  NOSTDARG
    char *s0, *s1;
    va_start(ap);
    s0 = va_arg(ap, char*);
    s1 = va_arg(ap, char*);
#else
    va_start(ap, s1);
#endif
    while (s1 && (++found, strcmp(s0, s1))) s1 = va_arg(ap, char*);
    return (s1 ? found : -1);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
const char *modeImage[] = {
    "ST1","ST2","ST3","ST4","ST5","ST6","ST7",
    "SWA","SWB","SNA","SNB",
    "WD1","WD2","WD3",
    "FN1","FN2","FN3", "FN4", "FN5",
    "EL1","EH1","EH2","EH3","EH4","EH5","EH6",
    "STD","STD","STD", "XXX"
};
const char *ModeImage(Mode_t x)
{
    return (modeImage[x >= ST1 && x <= JS1 ? x : MODE_INVALID]);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
const char *svTypeImage[] = {
    "UNKNOWN", "PREDICTED", "RESTITUTED", "PRELIMINARY", "PRECISION"
};
const char *SV_TypeImage(SV_Type_t x)
{
    return (x >= PREDICTED && x <= PRECISION ? svTypeImage[x] : "UNKNOWN");
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
const char *svCoordImage[] = {
    "UNKNOWN", "TRUE_EQUATORIAL", "MEAN_OF_DATE"
};
const char *SV_CoordImage(SV_Coord_t x)
{
    return (x >= TRUE_EQUATORIAL && x <= MEAN_OF_DATE
	    ? svCoordImage[x] : "UNKNOWN");
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int LastFrame(const Merge_t* mp, U32 blk, U32 bit, const Job_t* job)
{
    const int n32 = R32(((Satellite_t*)job->sat)->frame_size_max);
    const int len = (n32+job->blk_size-1)/job->blk_size;

    return (blk == (mp->blk_start + mp->blk_cnt - len) &&
            bit  > (job->blk_size * len - n32)*8);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
void Scan_Changes(Merge_t* mp, Segmt_t* sp, Segmt_t* dp,
		  Pulse_t* pp_new, const Job_t* job, int *error)
{
    const Satellite_t* sat = (Satellite_t*) job->sat;
    register Pulse_t* pp = sp->pulse[sp->pulse_cnt-1];
    register int i;

    if (sp->burst_len == 0 && sp->pulse_cnt > 1) {	
	register int n = (*sat->PulseCnt)(pp);

	if (n == (*sat->PulseCnt)(pp_new) &&
	    n == (*sat->PulseCnt)(sp->pulse[sp->pulse_cnt-2]))
	    sp->burst_len = n;
    }
    if (sp->burst_len > 1 && 
       (sp->burst_cnt > 1
	? (sp->pulse_cnt - sp->burst[sp->burst_cnt-1] == sp->burst_len)
	: (sp->pulse_cnt > 2 &&
	   (*sat->SameBurst)(sp->pulse[sp->pulse_cnt-3],
			     sp->pulse[sp->pulse_cnt-2]) < 0 &&
	   (*sat->SameBurst)(sp->pulse[sp->pulse_cnt-2], pp) >= 0 &&
	   (*sat->SameBurst)(pp, pp_new) < 0))) {

        if (dp->burst_cnt > SEGMT_PER_READ_MAX - mp->segmt_cnt) {
	    dp->burst_cnt--; dp->burst++;

	    if (sp->burst_len > 1 && sp->burst_cnt > 1)
		sp->burst[sp->burst_cnt++] = sp->pulse_cnt;
	    else
		sp->burst[sp->burst_cnt++] = sp->pulse_cnt-1;
        }
        else if (! (*error & 1)) {
	    printfLLog(LOG_ERR, "Too many bursts starting %d/%.5d",
		BLKNUM(pp_new->start.blk,sp,job),
		BITNUM(pp_new->start.bit,sp,job));
	    *error |= 1;
        }
    }
    if (sp->pulse_cnt > 2 &&
	(*sat->SameDWP)(pp, pp_new) < 0 &&
	(*sat->SameDWP)(pp, sp->pulse[sp->pulse_cnt-2]) < 0 &&
	(*sat->SameDWP)(pp, sp->pulse[sp->dwp[sp->dwp_cnt-1]]) == 0) {

        if (dp->dwp_cnt > SEGMT_PER_READ_MAX - mp->segmt_cnt) {
	    dp->dwp_cnt--; dp->dwp++;
	    sp->dwp[sp->dwp_cnt++] = sp->pulse_cnt-2;
        }
        else if (! (*error & 2)) {
	    printfLLog(LOG_ERR, "Too many DWPs starting %d/%.5d",
		BLKNUM(pp_new->start.blk,sp,job),
		BITNUM(pp_new->start.bit,sp,job));
	    *error |= 2;
        }
    }
    if (sp->pulse_cnt > 2 &&
	(*sat->SameAGC)(pp, pp_new) < 0 &&
        (*sat->SameAGC)(pp, sp->pulse[sp->pulse_cnt-2]) < 0 &&
	(*sat->SameAGC)(pp, sp->pulse[sp->agc[sp->agc_cnt-1]]) == 0) {

        if (dp->agc_cnt > SEGMT_PER_READ_MAX - mp->segmt_cnt) {
	    dp->agc_cnt--; dp->agc++;
	    sp->agc[sp->agc_cnt++] = sp->pulse_cnt-2;
        }
        else if (! (*error & 4)) {
	    printfLLog(LOG_ERR, "Too many AGCs starting %d/%.5d",
		BLKNUM(pp_new->start.blk,sp,job),
		BITNUM(pp_new->start.bit,sp,job));
	    *error |= 4;
	}
    }
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
#ifdef 	DEBUG
static
void SplitPulse(Merge_t* mp, Segmt_t* sp, Segmt_t* dp, const Job_t* job,
		const int has_tapebuf, int *err)
{
    register int n = (*((Satellite_t*) job->sat)->ExtraPulse)
	(sp, dp->pulse, dp->pulse_cnt, job, has_tapebuf);
    register Pulse_t* pp;
    if (n > 0) {
	register int i;
	for (i = 0; i < n; ++i) {
	    Scan_Changes(mp, sp, dp, pp = *dp->pulse, job, err);
	    sp->pulse_cnt++; dp->pulse_cnt--; dp->pulse++;
	}
    }
    if (n < 0 || dp->pulse_cnt <= n) {
	(*job->Log)(job->err, "Too many pulses starting %d/%.5d\n",
	    BLKNUM(pp->start.blk,sp,job),
	    BITNUM(pp->start.bit,sp,job));
    }
    return (dp->pulse-1);
}
#endif

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
void MergePulses(Segmt_t* ds, const Segmt_t* ss, const Job_t* job)
{
    const Satellite_t* sat = (Satellite_t*) job->sat;
    register int i, k, n;

    if (ds->burst_len == 0)
	ds->burst_len = ss->burst_len;

#ifdef	DEBUG
if (ds+1 != ss)
printf("MergePulse: %d/%d..%d/%d, %dF/%dP/%dB -- %d/%d..%d/%d, %dF/%dP/%dB\n",
ds->start.blk, ds->start.bit, ds->end.blk, ds->end.bit, ds->frame_cnt,
	       ds->pulse_cnt, ds->burst_cnt,
ss->start.blk, ss->start.bit, ss->end.blk, ss->end.bit, ss->frame_cnt,
	       ss->pulse_cnt, ss->burst_cnt);
{
    register int i;
if (ds+1 != ss)
    for (i = 1297; i < ds->burst_cnt; ++i) {
	register int k, max;
	max = (i < ds->burst_cnt-1 ? ds->burst[i+1] : ds->pulse_cnt);

printf("Burst %d: %d..%d\n", i, ds->burst[i], max);

	for (k = ds->burst[i]; k < max; ++k)
	    printf("<< Pulse %d/%d, %d/%d: %dF\n", i, k-ds->burst[i],
			ds->pulse[k]->start.blk,
			ds->pulse[k]->start.bit,
			ds->pulse[k]->frame_cnt);
    }
}
#endif

    if (ds->burst_len > 1) {
	if (n = (ds->pulse_cnt - ds->burst[ds->burst_cnt-1]) +
		(ss->burst_cnt > 1 ? ss->burst[1] : ss->pulse_cnt),
	    n % ds->burst_len == 0) {

	    n = ds->burst_cnt + n/ds->burst_len - 1;
	    memmove(ds->burst+n, ss->burst+1, (ss->burst_cnt-1)*4);
	    for (k = ds->burst_cnt; k < n; ++k)
		ds->burst[k] = ds->burst[k-1] + ds->burst_len;
	    n += ss->burst_cnt-1;
	}
	else if (ss->burst_cnt == 1 && ds->burst_cnt > 1) {

	    n = ds->burst_cnt + n/ds->burst_len;
	    for (k = ds->burst_cnt; k < n; ++k)
		ds->burst[k] = ds->burst[k-1] + ds->burst_len;
	}
	else if (ss->pulse_cnt > 1 && ds->pulse_cnt > 1 &&
	    (*sat->SameBurst)(ds->pulse[ds->pulse_cnt-2],
			      ds->pulse[ds->pulse_cnt-1]) < 0 &&
	    (*sat->SameBurst)(ds->pulse[ds->pulse_cnt-1], ss->pulse[0]) >= 0 &&
	    (*sat->SameBurst)(ss->pulse[0], ss->pulse[1]) < 0) {

	    k = ds->burst_cnt;
	    memmove(ds->burst+k, ss->burst, ss->burst_cnt*4);
	    n = ss->burst_cnt+k;
	}
	else {
	    k = ds->burst_cnt;
	    memmove(ds->burst+k, ss->burst+1, (ss->burst_cnt-1)*4);
	    n = ss->burst_cnt+k-1;
	}
	while (k < n) ds->burst[k++] += ds->pulse_cnt;
	ds->burst_cnt = n;
    }
    if (ds->pulse_cnt > 2 &&
        (*sat->SameDWP)(ds->pulse[ds->pulse_cnt-1], ss->pulse[0]) < 0 &&
	(*sat->SameDWP)(ds->pulse[ds->pulse_cnt-1], 
			ds->pulse[ds->pulse_cnt-2]) < 0 &&
	(*sat->SameDWP)(ds->pulse[ds->dwp[ds->dwp_cnt-1]], ss->pulse[0]) == 0) {
	ss->dwp[0] = -2;
	i = 0;
    }
    else if (ds->pulse_cnt > 1 && ss->pulse_cnt > 1 &&	
        (*sat->SameDWP)(ss->pulse[1], ss->pulse[0]) < 0 &&
        (*sat->SameDWP)(ds->pulse[ds->pulse_cnt-1], ss->pulse[0]) < 0 &&
	(*sat->SameDWP)(ds->pulse[ds->dwp[ds->dwp_cnt-1]], ss->pulse[0]) == 0) {
	ss->dwp[0] = -1;
	i = 0;
    }
    else if (ss->pulse_cnt > 2 &&
        (*sat->SameDWP)(ss->pulse[1], ss->pulse[2]) < 0 &&
        (*sat->SameDWP)(ss->pulse[1], ss->pulse[0]) < 0 &&
	(*sat->SameDWP)(ds->pulse[ds->dwp[ds->dwp_cnt-1]], ss->pulse[0]) == 0) {
	i = 0;
    }
    else if (ss->dwp_cnt > 1 &&
	(*sat->SameDWP)(ds->pulse[ds->dwp[ds->dwp_cnt-1]], 
			ss->pulse[ss->dwp[1]]) < 0) {
	i = 2;
    }
    else {
	i = 1;
    }
    if (ss->dwp_cnt > i && (ss->dwp+i != ds->dwp+ds->dwp_cnt))
	memmove(ds->dwp+ds->dwp_cnt, ss->dwp+i, (ss->dwp_cnt-i)*4);

    n = ds->dwp_cnt+ss->dwp_cnt-i;
    for (k = ds->dwp_cnt; k < n; ++k)
	ds->dwp[k] += ds->pulse_cnt;
    ds->dwp_cnt = n;

    if (ds->pulse_cnt > 2 &&
        (*sat->SameAGC)(ds->pulse[ds->pulse_cnt-1], ss->pulse[0]) < 0 &&
	(*sat->SameAGC)(ds->pulse[ds->pulse_cnt-1], 
			ds->pulse[ds->pulse_cnt-2]) < 0 &&
	(*sat->SameAGC)(ds->pulse[ds->agc[ds->agc_cnt-1]], ss->pulse[0]) == 0) {
	ss->agc[0] = -2;
	i = 0;
    }
    else if (ds->pulse_cnt > 1 && ss->pulse_cnt > 1 &&	
        (*sat->SameAGC)(ss->pulse[1], ss->pulse[0]) < 0 &&
        (*sat->SameAGC)(ds->pulse[ds->pulse_cnt-1], ss->pulse[0]) < 0 &&
	(*sat->SameAGC)(ds->pulse[ds->agc[ds->agc_cnt-1]], ss->pulse[0]) == 0) {
	ss->agc[0] = -1;
	i = 0;
    }
    else if (ss->pulse_cnt > 2 &&
        (*sat->SameAGC)(ss->pulse[1], ss->pulse[2]) < 0 &&
        (*sat->SameAGC)(ss->pulse[1], ss->pulse[0]) < 0 &&
	(*sat->SameAGC)(ds->pulse[ds->agc[ds->agc_cnt-1]], ss->pulse[0]) == 0) {
	i = 0;
    }
    else if (ss->agc_cnt > 1 &&
	(*sat->SameAGC)(ds->pulse[ds->agc[ds->agc_cnt-1]], 
			ss->pulse[ss->agc[1]]) < 0) {
	i = 2;
    }
    else {
	i = 1;
    }
    if (ss->agc_cnt > i && (ss->agc+i != ds->agc+ds->agc_cnt))
	memmove(ds->agc+ds->agc_cnt, ss->agc+i, (ss->agc_cnt-i)*4);

    n = ds->agc_cnt+ss->agc_cnt-i;
    for (k = ds->agc_cnt; k < n; ++k)
	ds->agc[k] += ds->pulse_cnt;
    ds->agc_cnt = n;

    if (ss->pulse != ds->pulse+ds->pulse_cnt)
	memmove(ds->pulse+ds->pulse_cnt, ss->pulse, ss->pulse_cnt*4);

    ds->pulse_cnt += ss->pulse_cnt;
    ds->frame_cnt += ss->frame_cnt;
    ds->bit_error += ss->bit_error;
    ds->end = ss->end;

#ifdef	DEBUG
{
    register int i;
if (ds+1 != ss)
    for (i = 1297; i < ds->burst_cnt; ++i) {
	register int k, max;
	max = (i < ds->burst_cnt-1 ? ds->burst[i+1] : ds->pulse_cnt);

	for (k = ds->burst[i]; k < max; ++k)
	    printf(">> Pulse %d/%d, %d/%d: %dF\n", i, k-ds->burst[i],
			ds->pulse[k]->start.blk,
			ds->pulse[k]->start.bit,
			ds->pulse[k]->frame_cnt);
    }
if (ds+1 != ss)
printf("MergePulse: %d/%d..%d/%d, %dF/%dP/%dB -- DONE\n",
ds->start.blk, ds->start.bit, ds->end.blk, ds->end.bit, ds->frame_cnt,
	       ds->pulse_cnt, ds->burst_cnt);
}
#endif
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
Segmt_t* Segmt_malloc(Merge_t* mp, const Job_t* job)
{
    register int i;
    register Segmt_t* sp;
    register Segmt_t* dp = &mp->segmt_free;

    if (mp->segmt_cnt > 3 && (sp = &mp->segmt[mp->segmt_cnt-1],
	! LastFrame(mp, sp->end.blk, sp->end.bit, job))) {

	/*  Segment too short -> return its memory resources to free pool */

	dp->pulse_cnt += sp->pulse_cnt;
	dp->burst_cnt += sp->burst_cnt;
	dp->dwp_cnt += sp->dwp_cnt;
	dp->agc_cnt += sp->agc_cnt;

	dp->pulse -= sp->pulse_cnt;
	dp->burst -= sp->burst_cnt;
	dp->dwp -= sp->dwp_cnt;
	dp->agc -= sp->agc_cnt;

	/*  Turn this into a GAP segment and merge it with previous segment */

	sp->pulse_cnt = sp->burst_cnt = sp->dwp_cnt = sp->agc_cnt =
	sp->frame_cnt = 0;
	--sp;
	if (sp->frame_cnt == 0) {
	    sp->end = sp[1].end;
	    mp->segmt_cnt--;
	}
    }
    if (mp->segmt_cnt >= SEGMT_PER_READ_MAX) {
	(*job->Log)(job->err, "Too many segments in read #%d\n", mp->blk_start);
	return NULL;
    }
    sp = &mp->segmt[mp->segmt_cnt++];
    sp->mode = MODE_INVALID;
    sp->type = 0;
    sp->reverse = job->reverse;
    sp->pulse_pcm = -1;
    sp->pulse_cnt = sp->pulse_0 = sp->burst_0 = sp->burst_len = sp->pulse_err =
    sp->frame_cnt = sp->dwp_cnt = sp->agc_cnt = sp->ppr_cnt = sp->bit_error =
    sp->burst_cnt = 0;
    sp->cal = sp->prf = sp->lookangle = sp->hidelta = 0;

    memset(&sp->gmt_start, 0, sizeof(GMT));
    memset(&sp->gmt_end, 0, sizeof(GMT));

    return sp;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
void Scan_Position(TapePos_t* pos, const Scan_t* fs, const Job_t* job)
{
    const int k = job->blk_size*8;
    const int n = R32(((Satellite_t*)job->sat)->frame_size_max)/4;

    if (fs->i < n) {
	pos->bit = fs->n + (fs->i << 5) + (k - ((n<<5)%k));
	pos->blk = fs->blk_start + pos->bit/k - ((n<<5)+k-1)/k;
    }
    else {
	pos->bit = fs->n + ((fs->i - n) << 5);
	pos->blk = fs->blk_start + pos->bit/k;
    }
    pos->bit %= k;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
Merge_t* Scan(Merge_t* mp, const Job_t* job)
{
    const Satellite_t* sat = (Satellite_t*) job->sat;
    const int fixed_size = (sat->frame_size_min == sat->frame_size_max);
    const int n32 = R32(sat->frame_size_max);
    register Segmt_t* sp, *dp = &mp->segmt_free;
    register Frame_t* fp;
    void* tapebuf = mp->tapebuf;
    Scan_t fs;

    memset(&fs, 0, sizeof(Scan_t));
    fs.n = (n32 - sat->frame_size_max) * 8 + 1;
    fs.buf = (U32*) ((*job->Buf)(&tapebuf,0,0) - n32);
    fs.len = mp->blk_cnt * (job->blk_size/sizeof(U32));
    fs.frame.tapebuf = tapebuf;
    fs.blk_start = mp->blk_start;

    sp = Segmt_malloc(mp, job);
    Scan_Position(&sp->start, &fs, job);

    while (fp = (*sat->Scan)(&fs, job)) {
	register Pulse_t* pp;
	register int data_type, pp_type;
	int err;

	sp->end = fp->start;
#ifdef	DEBUG
    printf("GAP <%d/%.5d, %d/%.5d, %dF, %dP> #%d%s\n",
	sp->start.blk, sp->start.bit, sp->end.blk, sp->end.bit,
	sp->frame_cnt, sp->pulse_cnt, mp->segmt_cnt-1,
        (sp->end.bit - sp->start.bit +
        (sp->end.blk - sp->start.blk)*job->blk_size*8) % (sat->frame_size*8)
	? " - ODD" : "");
#endif
	if (sp->start.blk != sp->end.blk || sp->start.bit != sp->end.bit) {
	    register int n,k;

	    /*  See if we can recover any frames or pulses from this gap */

	    if (fixed_size &&
		(n = (sp->end.bit - sp->start.bit +
                     (sp->end.blk - sp->start.blk)*job->blk_size*8),
		 k = sat->frame_size*8,
		(n % k) == 0 &&
		(n / k) <= (job->aux_recover*sat->frame_per_pulse_max) &&
		mp->segmt_cnt > 1)) {

		if (--sp, sp->pulse_cnt == 0) {
		    /*  Recovering frames with corrupted SYNC codes */

		    sp->frame_cnt += n/k;
		    sp->end = sp[1].end;
		}
		else {
		    /*  Recovering pulses with corrupted SYNC codes */

		    sp->pulse[sp->pulse_cnt-1]->frame_cnt += n/k;
		    sp->frame_cnt += n/k;;
		    sp->end = sp[1].end;

		    n = (*sat->ExtraPulse)(sp, dp->pulse, dp->pulse_cnt,job,1);
		    if (n > 0) {
			register int i;
			for (i = 0; i < n; ++i) {
			    Scan_Changes(mp, sp, dp, *dp->pulse, job, &err);
			    sp->pulse_cnt++; 
			    dp->pulse_cnt--; dp->pulse++;
			}
		    }
		    if (n < 0 || dp->pulse_cnt <= n) {
			printfLLog(LOG_ERR,"Too many pulses starting %d/%.5d",
		BLKNUM(sp->pulse[sp->pulse_cnt-1]->start.blk,sp,job),
		BITNUM(sp->pulse[sp->pulse_cnt-1]->start.bit,sp,job));

			return NULL;
		    }
		    sp->pulse_err |=
	(sp->pulse[sp->pulse_cnt-1]->frame_cnt > sat->frame_per_pulse_max);
		}

		--mp->segmt_cnt;
	    }
	    if (! (sp = Segmt_malloc(mp, job)))
		return NULL;
	}
	sp->start = fp->start;
	pp = NULL;

	while (fp != NULL) {
	    if ((data_type = (*sat->NewPulse)(NULL, &fs, sp, job, pp)) == 0
	    ||  (data_type == -1 && pp &&
		 pp->frame_cnt < sat->frame_per_pulse_min) ) {

		if (pp) {
		    /*  Add frame to pulse & unscramble if decoding */
		    if (!job->scan_job && sat->PRN[0])
			Unscramble(sat->PRN, fs.buf+fs.i, fs.n,
			    	   sat->header_size, sat->data_size);
		    pp->frame_cnt++;
		    pp->bit_error += fs.frame.bit_error;
		}
		else if (!job->scan_job && sat->PRN[0] && mp->segmt_cnt <= 2) {

		    /*  Unscramble frames preceding first pulse */
		    Unscramble(sat->PRN, fs.buf+fs.i, fs.n,
			       sat->header_size, sat->data_size);
		}
	    }
	    else if (!pp || (sp->type != data_type && pp_type == data_type &&
			     sp->pulse_cnt > 2)) {

		/*  Found the first pulse of the segment */
		register char *s;

		if (! pp)
		    sp->end = fp->start;
		else {
		    sp->end = pp->start;
		    sp->pulse_cnt--;

		    if (sp->burst[sp->burst_cnt-1] == sp->pulse_cnt) {
			sp->burst_cnt--; dp->burst_cnt++; dp->burst--;
		    }
		    if (sp->agc[sp->agc_cnt-1] == sp->pulse_cnt) {
			sp->agc_cnt--; dp->agc_cnt++; dp->agc--;
		    }
		    if (sp->dwp[sp->dwp_cnt-1] == sp->pulse_cnt) {
			sp->dwp_cnt--; dp->dwp_cnt++; dp->dwp--;
		    }
		    if (job->scan_job && sp->type > 0)
			(*sat->CAL)(&sp->cal, sp, job);
		}
#ifdef	DEBUG 
printf("%s <%d/%.5d, %d/%.5d, %dF, %dP> #%d, type %d/%d\n", 
	!pp ? "FRM" : (sp->type < 0 ? (sp->type == -1 ? "TRU" : "SWT")
		    : (sp->type ==1 ? "C11" : (sp->type==2 ? "C22" : "C??"))),
	sp->start.blk, sp->start.bit, sp->end.blk, sp->end.bit,
	sp->frame_cnt, sp->pulse_cnt, mp->segmt_cnt-1, sp->type, data_type);
#endif
		if (sp->start.blk != sp->end.blk || 
		    sp->start.bit != sp->end.bit) {
		    register int n;

		    if (mp->segmt_cnt > 1) {
			if (--sp, sp->frame_cnt == 0) {

			    /* do nothing */
			}
			else if (sp->pulse_cnt == 0) {
			    register int k;

			    if (fixed_size &&
			       (n =(sp->end.bit-sp->start.bit +
				   (sp->end.blk-sp->start.blk)*job->blk_size*8),
				k = sat->frame_size*8,
				n % k == 0)) {

				sp->frame_cnt = n/k + sp[1].frame_cnt;
				sp->bit_error += sp[1].bit_error;
			    }
			    sp->end = sp[1].end;
			    --mp->segmt_cnt;
			}
			else {
			    sp->pulse[sp->pulse_cnt-1]->frame_cnt
					  += sp[1].frame_cnt;
			    sp->frame_cnt += sp[1].frame_cnt;
			    sp->bit_error += sp[1].bit_error;
			    sp->end = sp[1].end;

			    n = (*sat->ExtraPulse)(sp, dp->pulse,
						   dp->pulse_cnt, job, 1);
			    if (n > 0) {
				register int i;
				for (i = 0; i < n; ++i) {
				    Scan_Changes(mp, sp, dp, *dp->pulse, job,
						&err);
				    sp->pulse_cnt++;
				    dp->pulse_cnt--; dp->pulse++;
				}
			    }
			    if (n < 0 || dp->pulse_cnt <= n) {
				printfLLog(LOG_ERR,
				    "Too many pulses starting %d/%.5d",
			BLKNUM(sp->pulse[sp->pulse_cnt-1]->start.blk,sp,job),
			BITNUM(sp->pulse[sp->pulse_cnt-1]->start.bit,sp,job));
				return NULL;
			    }
			    sp->pulse_err |=
	(sp->pulse[sp->pulse_cnt-1]->frame_cnt > sat->frame_per_pulse_max);
			    --mp->segmt_cnt;
			}
		    }
		    if (! (sp = Segmt_malloc(mp, job)))
			return NULL;
		}
		if ((s = "pulses", dp->pulse_cnt == 0) ||
		    (s = "bursts", dp->burst_cnt == 0) ||
		    (s = "DWPs", dp->dwp_cnt == 0) ||
		    (s = "AGCs", dp->agc_cnt == 0)) {
		    (*job->Log)(job->err, "Too many %s in read #%d\n",
				s, mp->blk_start);
		    return NULL;
		}
		if (pp) {
		    sp->pulse_cnt = 1;	
		    sp->pulse = dp->pulse-1;
		    sp->start = pp->start;
		}
		else {
		    sp->pulse_cnt = 0;	
		    sp->pulse = dp->pulse;
		    sp->start = fp->start;
		}
		sp->type  = pp_type = data_type;
		sp->burst = dp->burst++; dp->burst_cnt--;
		sp->dwp   = dp->dwp++;   dp->dwp_cnt--;
		sp->agc   = dp->agc++;   dp->agc_cnt--;
		sp->pulse_cnt = sp->burst[0] = sp->dwp[0] = sp->agc[0] =err = 0;
		sp->burst_cnt = sp->dwp_cnt = sp->agc_cnt = mp->pulse_exist = 1;

		(*sat->NewPulse)(pp = *dp->pulse, &fs, sp, job, 0);
		sp->pulse_cnt++; 
		dp->pulse_cnt--; dp->pulse++;
	    }
	    else { /* Found subsequent pulse */

		register int n;
		sp->pulse_err |= (pp->frame_cnt > sat->frame_per_pulse_max);

		n = (*sat->ExtraPulse)(sp, dp->pulse, dp->pulse_cnt, job, 1);
		if (n > 0) {
		    register int i;
		    for (i = 0; i < n; ++i) {
			Scan_Changes(mp, sp, dp, pp = *dp->pulse, job, &err);
			sp->pulse_cnt++; 
			dp->pulse_cnt--; dp->pulse++;
		    }
		}
		if (n < 0 || dp->pulse_cnt <= n) {
		    (*job->Log)(job->err, "Too many pulses starting %d/%.5d\n",
			 BLKNUM(pp->start.blk,sp,job),
			 BITNUM(pp->start.bit,sp,job));
		    return NULL;
		}
		if (pp_type == data_type)
		    sp->type = data_type;
		pp_type = data_type;

		(*sat->NewPulse)(pp = *dp->pulse, &fs, sp, job, 0);
		Scan_Changes(mp, sp, dp, pp, job, &err);
		sp->pulse_cnt++;
		dp->pulse_cnt--; dp->pulse++;
	    }
	    sp->frame_cnt++;
	    fp = (*sat->NxtFrame)(&fs, job, pp);
	    sp->bit_error += fs.frame.bit_error;
	}
	sp->end = fs.frame.start;

	if (sp->pulse_cnt > 0) {
	    register int n;
	    sp->pulse_err |= (pp->frame_cnt > sat->frame_per_pulse_max);
	    
	    n = (*sat->ExtraPulse)(sp, dp->pulse, dp->pulse_cnt, job, 1);

	    if (n > 0) {
		register int i;
		for (i = 0; i < n; ++i) {
		    Scan_Changes(mp, sp, dp, pp = *dp->pulse, job, &err);
		    sp->pulse_cnt++; 
		    dp->pulse_cnt--; dp->pulse++;
		}
	    }
	    if (n < 0 || dp->pulse_cnt <= n) {
		(*job->Log)(job->err, "Too many pulses starting %d/%.5d\n",
		    BLKNUM(pp->start.blk,sp,job),
		    BITNUM(pp->start.bit,sp,job));
	    }
	}
#ifdef	DEBUG
printf("%s <%d/%.5d, %d/%.5d, %dF, %dP, %dB> #%d, remains %dB, type %d\n",
	sp->pulse_cnt ? (sp->type < 0 ? (sp->type == -1 ? "TRU" : "SWT")
		      : (sp->type ==1 ? "C11" : (sp->type==2 ? "C22" : "C??")))
		      : (sp->frame_cnt  ? "FRM" : "GAP"),
	sp->start.blk, sp->start.bit, sp->end.blk, sp->end.bit,
	sp->frame_cnt, sp->pulse_cnt, sp->burst_cnt, 
	mp->segmt_cnt-1, dp->burst_cnt, sp->type);
#endif
	if (job->scan_job && pp && sp->type > 0)
	    (*sat->CAL)(&sp->cal, sp, job);

	if (sp->start.blk != sp->end.blk || sp->start.bit != sp->end.bit) {

	    if (mp->segmt_cnt > 1) {
		register int n;

		if (sp->pulse_cnt > 0) {
		    --sp;
		    if (sp->type == sp[1].type ||
		       (sp->type == -1 &&
			sp[1].pulse_cnt < job->min_pulse_per_read)) {

			const int pulse_cnt = sp->pulse_cnt+sp[1].pulse_cnt;
			const int burst_cnt = sp->burst_cnt+sp[1].burst_cnt;
			const int dwp_cnt = sp->dwp_cnt+sp[1].dwp_cnt;
			const int agc_cnt = sp->agc_cnt+sp[1].agc_cnt;
			register int n;

			MergePulses(sp, sp+1, job);
			--mp->segmt_cnt;

			dp->pulse -= (n = pulse_cnt - sp->pulse_cnt);
			dp->pulse_cnt += n;
			dp->burst -= (n = burst_cnt - sp->burst_cnt);
			dp->burst_cnt += n;
			dp->dwp -= (n = dwp_cnt - sp->dwp_cnt);
			dp->dwp_cnt += n;
			dp->agc -= (n = agc_cnt - sp->agc_cnt);
			dp->agc_cnt += n;
		    }
		    else if (sp->frame_cnt > 0 &&
			sp[1].type != -1 &&
			sp[1].pulse_cnt < job->min_pulse_per_read) {

			sp->frame_cnt += sp[1].frame_cnt;
			sp->bit_error += sp[1].bit_error;
			sp->end = sp[1].end;
			--mp->segmt_cnt;
		    }
		}
		else if (--sp, sp->frame_cnt == 0) {

		    /* do nothing */
	        }
		else if (sp->pulse_cnt == 0) {
		    register int k;

		    if (fixed_size &&
		       (n = (sp->end.bit - sp->start.bit +
			     (sp->end.blk - sp->start.blk)*job->blk_size*8),
			k = sat->frame_size*8,
			n % k == 0)) {

			sp->frame_cnt = n/k + sp[1].frame_cnt;
			sp->bit_error += sp[1].bit_error;
		    }
		    sp->end = sp[1].end;
		    --mp->segmt_cnt;
		}
		else {
		    sp->pulse[sp->pulse_cnt-1]->frame_cnt += sp[1].frame_cnt;
		    sp->frame_cnt += sp[1].frame_cnt;
		    sp->bit_error += sp[1].bit_error;
		    sp->end = sp[1].end;

		    n = (*sat->ExtraPulse)(sp, dp->pulse, dp->pulse_cnt, job,1);

		    if (n > 0) {
			register int i;
			for (i = 0; i < n; ++i) {
			    Scan_Changes(mp, sp, dp, *dp->pulse, job, &err);
			    sp->pulse_cnt++;
			    dp->pulse_cnt--; dp->pulse++;
			}
		    }
		    if (n < 0 || dp->pulse_cnt <= n) {
			printfLLog(LOG_ERR,
				"Too many pulses starting %d/%.5d\n",
			BLKNUM(sp->pulse[sp->pulse_cnt-1]->start.blk,sp,job),
			BITNUM(sp->pulse[sp->pulse_cnt-1]->start.bit,sp,job));

			return NULL;
		    }
		    sp->pulse_err |=
	(sp->pulse[sp->pulse_cnt-1]->frame_cnt > sat->frame_per_pulse_max);
		    --mp->segmt_cnt;
		}
	    }
	    if (! (sp = Segmt_malloc(mp, job)))
		return NULL;
	}
	sp->start = fs.frame.start;

    } /* Next segment */
    sp->end = fs.frame.start;

#ifdef	DEBUG
printf("GAP <%d/%d, %d/%d, %dF, %dP> #%d ------ LAST\n\n",
	sp->start.blk, sp->start.bit, sp->end.blk, sp->end.bit,
	sp->frame_cnt, sp->pulse_cnt, mp->segmt_cnt-1);
#endif
    if (sp->start.blk == sp->end.blk && sp->start.bit == sp->end.bit)
	mp->segmt_cnt--;

    else if (mp->segmt_cnt > 1 && (sp = &mp->segmt[mp->segmt_cnt-2],
	sp->frame_cnt == 0 && sp[1].frame_cnt == 0)) {

	sp->end = sp[1].end;
	mp->segmt_cnt--;
    }
    return mp;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
Merge_t* Merge(Merge_t* mp, const Job_t* job)
{
    const Satellite_t* sat = (Satellite_t*) job->sat;
    const int fixed_size = (sat->frame_size_min == sat->frame_size_max);
    register Segmt_t* ds = &mp->merge[*mp->merge_cnt-1];
    register Segmt_t* ss = &mp->segmt[0];
    register int k, n;

#ifdef DEBUG
{
    register int i, ns = 0;
    printf("\nMerging %d/%d..%d/%d: %dF/%dP, =%d/%d\n",
	   BLKNUM(ds->start.blk,ds,job), BITNUM(ds->start.bit,ds,job),
	   BLKNUM(ds->end.blk,ds,job), BITNUM(ds->end.bit,ds,job),
	   ds->frame_cnt, ds->pulse_cnt, *mp->merge_cnt-1, mp->merge_max);

    for (i = 0; i < mp->segmt_cnt; ++i) {
	register int k;
	register Segmt_t* sp = &mp->segmt[i];

	printf("   with %d/%d..%d/%d: %dF/%dP, =%d\n",
	       BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
	       BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job),
	       sp->frame_cnt, sp->pulse_cnt, ns++);

	if (sp->pulse_cnt > 0)
	    printf("  P#%d: %d/%d\n", 0, 
		   BLKNUM(sp->pulse[0]->start.blk,sp,job),
		   BITNUM(sp->pulse[0]->start.bit,sp,job));
	for (k = 1; k < sp->pulse_cnt; ++k)
	    printf("  P#%d: %d/%d, %d\n", k, 
		   BLKNUM(sp->pulse[k]->start.blk,sp,job),
		   BITNUM(sp->pulse[k]->start.bit,sp,job),
	    (sp->pulse[k]->start.blk-sp->pulse[k-1]->start.blk)*job->blk_size*8+
	    (sp->pulse[k]->start.bit-sp->pulse[k-1]->start.bit));
    }
}
#endif
    /*  Check for bogus gap  */
    if (mp->segmt_cnt > 0)
    while (ss->frame_cnt == 0 &&
        ((ss->end.blk == ds->end.blk && ss->end.bit <= ds->end.bit) ||
         (ss->end.blk <  ds->end.blk)))
        ++ss;

    --ss;
    while (((++ss) - &mp->segmt[0]) < mp->segmt_cnt) {

	if (ss->pulse_cnt > 0) {
	    if (ds->pulse_cnt > 0 &&
		ss->type != ds->type &&
		ss->pulse_cnt < job->min_pulse_per_read &&
		ss+1 != &mp->segmt[mp->segmt_cnt] &&
		ds->type == ss[1].type) {

		MergePulses(ds, ss, job);
		ss++;
	    }
	    if (ds->type != ss->type)
		break;

	    MergePulses(ds, ss, job);
	    continue;
	}
	if (ds->frame_cnt == 0) {
	    register Segmt_t* dp;
	    register Pulse_t* pp;

	    if (!fixed_size ||
		( n = (ss->end.blk - ds->start.blk) * (job->blk_size*8) +
		     (ss->end.bit - ds->start.bit),
		 k = sat->frame_size*8,
		(n % k) != 0 ||
		(n/= k) > job->aux_recover * sat->frame_per_pulse_max)) {

		if (ss->frame_cnt == 0) {
		    ds->end = ss->end;
		    ++ss;
		}
		break;
	    }		
	    ds->frame_cnt = n;
	    ds->end = ss->end;

	    if (*mp->merge_cnt <= 2)
		continue;

	    if (--ds, --(*mp->merge_cnt), ds->pulse_cnt == 0) {
		ds->frame_cnt += ds[1].frame_cnt;
		ds->bit_error += ds[1].bit_error;
		ds->end = ds[1].end;
		--(*mp->merge_cnt);
		continue;
	    }
	    dp = &(mp-1)->segmt_free;
	    pp = ds->pulse[ds->pulse_cnt-1];
	    n = ds[1].frame_cnt;

	    if (pp->frame_cnt+n > job->aux_recover*sat->frame_per_pulse_max) {
		++ds; ++(*mp->merge_cnt);
		continue;
	    }
	    pp->frame_cnt += n;
	    ds->frame_cnt += n;
	    ds->bit_error += ds[1].bit_error;
	    ds->end = ds[1].end;
	    mp->pulse_exist = 1;

	    ds->pulse_err |= (ds->pulse[ds->pulse_cnt-1]->frame_cnt >
			      sat->frame_per_pulse_max);
	    k = (*sat->ExtraPulse)(ds, ds->pulse+ds->pulse_cnt, dp->pulse_cnt,
			      job, !job->scan_job);
	    if (k < 0) {
		(*job->Log)(job->err, "Too many pulses in read #%d\n",
			    (mp-1)->blk_start);
		return NULL;
	    }
	    if (k > 0) {
		register int i;
		int err = 0;
		for (i = 0; i < k; ++i) {
		    Scan_Changes(mp-1, ds, dp, ds->pulse[ds->pulse_cnt],
				 job, &err);
		    ds->pulse_cnt++; dp->pulse_cnt--; dp->pulse++;
		}
	    }
	}
	else if (!fixed_size ||
		((n = ss->frame_cnt) == 0 &&
		 (n = (ss->end.blk - ds->end.blk) * (job->blk_size*8) +
		      (ss->end.bit - ds->end.bit),
		  k = sat->frame_size*8,
		 (n % k) != 0 ||
		 (n/= k) > job->aux_recover*sat->frame_per_pulse_max))) {
	    break;
	}
	else if (ds->pulse_cnt == 0) {
	    ds->frame_cnt += n;
	    ds->bit_error += ss->bit_error;
	    ds->end = ss->end;
	}
	else {
	    register Segmt_t* dp = &(mp-1)->segmt_free;
	    register Pulse_t* pp = ds->pulse[ds->pulse_cnt-1];

	    if (pp->frame_cnt+n > job->aux_recover*sat->frame_per_pulse_max)
		break;

	    pp->frame_cnt += n;
	    ds->frame_cnt += n;
	    ds->bit_error += ss->bit_error;
	    ds->end = ss->end;
	    mp->pulse_exist = 1;

	    ds->pulse_err |= (ds->pulse[ds->pulse_cnt-1]->frame_cnt >
			      sat->frame_per_pulse_max);
	    k = (*sat->ExtraPulse)(ds, ds->pulse+ds->pulse_cnt, dp->pulse_cnt,
				   job, !job->scan_job);
	    if (k < 0) {
		(*job->Log)(job->err, "Too many pulses in read #%d\n",
			    (mp-1)->blk_start);
		return NULL;
	    }
	    if (k > 0) {
		register int i;
		int err = 0;
		for (i = 0; i < k; ++i) {
		    Scan_Changes(mp-1, ds, dp, ds->pulse[ds->pulse_cnt],
				 job, &err);
		    ds->pulse_cnt++; dp->pulse_cnt--; dp->pulse++;
		}
	    }
	}
    }
    n = mp->segmt_cnt - (ss - &mp->segmt[0]);

    if ((*mp->merge_cnt += n) >= mp->merge_max) {
	(*job->Log)(job->err, "Too many segments\n");
	return NULL;
    }
    if (n > 0) memmove(ds+1, ss, n*sizeof(Segmt_t));

#ifdef	DEBUG
    printf("----------- After merge --------\n");
    for (k = 0; k < *mp->merge_cnt; ++k) {
	Segmt_t* ss = &mp->merge[k];
	printf("SS %.2d: %d/%d, %d/%d, %dF, %dP, %dB, %dW, %dA>\n", k,
		ss->start.blk, ss->start.bit, ss->end.blk, ss->end.bit,
		ss->frame_cnt, ss->pulse_cnt, ss->burst_cnt,
		ss->dwp_cnt, ss->agc_cnt);
    }    
    printf("----------- END merge ----------\n");
#endif
    return mp;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int Segmt_cmp(const Merge_t* m0, const Merge_t* m1)
{
    const Segmt_t* s0 = &m0->segmt[0];
    const Segmt_t* s1 = &m1->segmt[0];
    register int n = s0->start.blk - s1->start.blk;
    return (n ? n : (s0->start.bit - s1->start.bit));
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int Merge_cmp(const Segmt_t* ss, const Segmt_t* ds)
{
    return (ss->start.blk <  ds->end.blk ||
	   (ss->start.blk == ds->end.blk && ss->start.bit <= ds->end.bit));
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
static
int PPR_Save (PPR_t* pp, char* buf, char* c0, char* c1, const Job_t* job)
{
    register int fmt_0, fmt_N;
    static const char frame_format[] =
"\n\
%s   OBJECT = FRAME%s\n\
%s    FRAME_ID = %d%s\n\
%s    START_FORMAT = %d%s\n\
%s    END_FORMAT = %d%s\n\
%s    START_ADDRESS = %d%s\n\
%s    START_BIT_OFFSET = %d%s\n\
%s    END_ADDRESS = %d%s\n\
%s    END_BIT_OFFSET = %d%s\n\
%s    START_TIME = %.4d-%.3dT%.2d:%.2d:%09.6f%s\n\
%s    END_TIME = %.4d-%.3dT%.2d:%.2d:%09.6f%s\n\
%s    CENTER_TIME = %.4d-%.3dT%.2d:%.2d:%09.6f%s\n\
%s    CENTER_LAT = %f%s\n\
%s    CENTER_LON = %f%s\n\
%s    NEAR_START_LAT = %f%s\n\
%s    NEAR_START_LON = %f%s\n\
%s    FAR_START_LAT = %f%s\n\
%s    FAR_START_LON = %f%s\n\
%s    NEAR_END_LAT = %f%s\n\
%s    NEAR_END_LON = %f%s\n\
%s    FAR_END_LAT = %f%s\n\
%s    FAR_END_LON = %f%s\n\
%s    SL_RNG_1ST_PIX = %f%s\n\
%s    SL_RNG_MID_PIX = %f%s\n\
%s    SL_RNG_LAST_PIX = %f%s\n\
%s    ASC_DESC = \"%c\"%s\n\
%s    OBJECT = STATE_VECTOR_RECORD%s\n\
%s     OBJECT = STATE_VECTOR_METADATA%s\n\
%s      PLATFORM = \"%s\"%s\n\
%s      STATE_VECTOR_PRECISION = \"%s\"%s\n\
%s      COORDINATE_SYSTEM = \"%s\"%s\n\
%s      REVOLUTION = %d%s\n\
%s     END_OBJECT = STATE_VECTOR_METADATA%s\n\
%s     OBJECT = STATE_VECTOR_DATA%s\n\
%s      TIME  = %.4d-%.3dT%.2d:%.2d:%09.6f%s\n\
%s      X_POSITION = %f%s\n\
%s      Y_POSITION = %f%s\n\
%s      Z_POSITION = %f%s\n\
%s      X_VELOCITY = %f%s\n\
%s      Y_VELOCITY = %f%s\n\
%s      Z_VELOCITY = %f%s\n\
%s     END_OBJECT = STATE_VECTOR_DATA%s\n\
%s    END_OBJECT = STATE_VECTOR_RECORD%s\n\
%s   END_OBJECT = FRAME%s\n\
";
    register Segmt_t* sp = (Segmt_t*) pp->segmt;
    if (sp->burst_len <= 1) {
	fmt_0 = pp->fmt_start;
	fmt_N = fmt_0 + pp->fmt_cnt;
    }
    else {
	Satellite_t* sat = (Satellite_t*) job->sat;
	register int nb = (sp->mode == SNA ? 2 : (sp->mode == SNB ? 3 : 4));
	fmt_0 = sp->burst[sp->burst_0 + pp->fmt_start * nb] - sp->pulse_0;
	fmt_N = fmt_0 + pp->fmt_cnt * nb * sp->burst_len;
    }
    return sprintf (buf, frame_format, c0, c1,
	 c0, pp->frame_id + 1, c1,	/* base 1 */
	 c0, fmt_0, c1,
	 c0, fmt_N, c1,
         c0, sp->reverse ? BLKNUM(pp->end.blk,sp,job)
                         : BLKNUM(pp->start.blk,sp,job), c1,
         c0, sp->reverse ? BITNUM(pp->end.bit,sp,job)
                         : BITNUM(pp->start.bit,sp,job), c1,
         c0, sp->reverse ? BLKNUM(pp->start.blk,sp,job)
                         : BLKNUM(pp->end.blk,sp,job), c1,
         c0, sp->reverse ? BITNUM(pp->start.bit,sp,job)
                         : BITNUM(pp->end.bit,sp,job), c1,
	 c0, pp->gmt_start.yr, pp->gmt_start.day, pp->gmt_start.hr,
	     pp->gmt_start.min, pp->gmt_start.second, c1,
	 c0, pp->gmt_end.yr, pp->gmt_end.day, pp->gmt_end.hr,
	     pp->gmt_end.min, pp->gmt_end.second, c1,
	 c0, pp->gmt_center.yr, pp->gmt_center.day, pp->gmt_center.hr,
	     pp->gmt_center.min, pp->gmt_center.second, c1,
	 c0, pp->ll_center[0], c1,
	 c0, pp->ll_center[1], c1,
	 c0, pp->ll_1_near[0], c1,
	 c0, pp->ll_1_near[1], c1,
	 c0, pp->ll_1_far[0], c1,
	 c0, pp->ll_1_far[1], c1,
	 c0, pp->ll_N_near[0], c1,
	 c0, pp->ll_N_near[1], c1,
	 c0, pp->ll_N_far[0], c1,
	 c0, pp->ll_N_far[1], c1,
	 c0, pp->range.near/1000., c1,
	 c0, pp->range.mid/1000., c1,
	 c0, pp->range.far/1000., c1,
	 c0, pp->pass, c1, c0, c1, c0, c1,
	 c0, job->platform, c1,
	 c0, SV_TypeImage(job->stv.precision), c1,
	 c0, SV_CoordImage(job->stv.coord_sys), c1,
	 c0, job->rev, c1, c0, c1, c0, c1,
	 c0, pp->gmt_center.yr, pp->gmt_center.day, pp->gmt_center.hr,
	     pp->gmt_center.min, pp->gmt_center.second, c1,
	 c0, pp->pv_center[0]/1000, c1,
	 c0, pp->pv_center[1]/1000, c1,
	 c0, pp->pv_center[2]/1000, c1,
	 c0, pp->pv_center[3], c1,
	 c0, pp->pv_center[4], c1,
	 c0, pp->pv_center[5], c1, c0, c1, c0, c1, c0, c1
    );
}
/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
int Segmt_Save(Segmt_t* ds, size_t ds_cnt, char* buf, const Job_t* job)
{
    static const char datatake_format[] = "\
/* This scan results file was automatically generated by %s */\n\
/* Copyright (C) 1996, California Institute of Technology.         */\n\
/* ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged. */\n\n\
OBJECT = SCAN_RESULTS_FILE\n\
 OBJECT = COMMON_HEADER\n\
  TIME = %.4d-%.3dT%.2d:%.2d:%.2d\n\
  MSG_TYPE = \"SCAN_RESULTS_FILE\"\n\
  DESTINATION = \"%s\"\n\
  SOURCE = \"%s\"\n\
  NUMBER_OF_RECORDS = 1\n\
 END_OBJECT = COMMON_HEADER\n\
\n\
 OBJECT = BODY\n\
  JOB_ID = %d\n\
  PLATFORM = \"%s\"\n\
  SENSOR = \"%c\"\n\
  REVOLUTION = %d\n\
  SEQUENCE = %d\n\
  ACTIVITY_ID = \"%s\"\n\
  MEDIA_TYPE = \"%s\"\n\
  MEDIA_LOCATION = \"%s\"\n\
  MEDIA_ID = \"%s\"\n\
  RECORDER_ID = \"%s\"\n\
  STATION_ID = \"%s\"\n\
  FRAME_MODE = \"%s\"\n\
  SITE_NAME = \"%s\"\n\
  SCAN_RESULTS_FILE = \"%s\"\n\
\n\
  OBJECT = GHA_CORRECTION\n\
   TIME = %.4d-%.3dT%.2d:%.2d:%09.6f\n\
   ANGLE = %f\n\
  END_OBJECT = GHA_CORRECTION\n\
\n\
  OBJECT = STATE_VECTOR_RECORD\n\
   OBJECT = STATE_VECTOR_METADATA\n\
    PLATFORM = \"%s\"\n\
    STATE_VECTOR_PRECISION = \"%s\"\n\
    COORDINATE_SYSTEM = \"%s\"\n\
    REVOLUTION = %d\n\
   END_OBJECT = STATE_VECTOR_METADATA\n\
   OBJECT = STATE_VECTOR_DATA\n\
    TIME = %.4d-%.3dT%.2d:%.2d:%09.6f\n\
    X_POSITION = %f\n\
    Y_POSITION = %f\n\
    Z_POSITION = %f\n\
    X_VELOCITY = %f\n\
    Y_VELOCITY = %f\n\
    Z_VELOCITY = %f\n\
   END_OBJECT = STATE_VECTOR_DATA\n\
  END_OBJECT = STATE_VECTOR_RECORD\n\
\n\
  OBJECT = TIME_CORRELATION\n\
   REVOLUTION = %d\n\
   TIME = %.4d-%.3dT%.2d:%.2d:%09.6f\n\
   PLATFORM_TIME = %u\n\
   CLOCK_CYCLE = %d\n\
  END_OBJECT = TIME_CORRELATION\n\
\n\
  SEGMENT_COUNT = %d\n\
  DATA_DIRECTION = \"%s\"\n\
  START_ADDRESS = %d\n\
  END_ADDRESS = %d\n\
  START_TIME = %.4d-%.3dT%.2d:%.2d:%09.6f\n\
  END_TIME = %.4d-%.3dT%.2d:%.2d:%09.6f\n\
  PRE_CAL1_POW = %e\n\
  PRE_CAL2_POW = %e\n\
  POST_CAL1_POW = %e\n\
  POST_CAL2_POW = %e\n\
";
    static const char segmt_format[] =
"\n\
%s  OBJECT = SEGMENT%s\n\
%s   SEGMENT_ID = %d%s\n\
%s   MODE = \"%s\"%s\n\
%s   START_ADDRESS = %d%s\n\
%s   END_ADDRESS = %d%s\n\
%s   START_TIME = %.4d-%.3dT%.2d:%.2d:%09.6f%s\n\
%s   END_TIME = %.4d-%.3dT%.2d:%.2d:%09.6f%s\n\
%s   FRAME_COUNT = %d%s\n\
%s   PRF = %f%s\n\
";
    const Satellite_t* sat = (Satellite_t*) job->sat;
    register int i, k, fd, n=0, segmt_start = -1, segmt_first = -1, segmt_cnt=0;
    register Segmt_t* sp = NULL;
    register char *p;
    struct tm tm;
    GMT t0, tN;
    time_t t;

    for (i = 0; i < ds_cnt; ++i)
	if ((sp = &ds[i])->pulse_cnt >= PULSE_PER_SEGMT_MIN &&
	    sp->type == -1 && (!job->mode_check || sp->mode == job->mode) &&
	    sp->ppr_cnt > 0 && gmt_diff(&sp->gmt_end, &sp->gmt_start) > 0.0 &&
	    !(gmt_diff(&sp->gmt_end, &job->start_time) < -job->gmt_diff_max ||
	      gmt_diff(&sp->gmt_start, &job->end_time) > job->gmt_diff_max)) {

	    if (segmt_first == -1)
		segmt_first = i;
	    segmt_cnt++;
	}
    if ((fd = open((p = strchr(job->sca_file, ':')) ? p+1 : job->sca_file,
		   O_WRONLY | O_CREAT | O_TRUNC, 0644)) == -1) {
	(*job->Log)(job->err, "can't create %s, %s",
		    job->sca_file, strerror(errno));
        return 0;
    }
    localtime_r((time(&t), &t), &tm);
    k = (i = ds_cnt - 2) - 1;
    n += sprintf (buf+n, datatake_format, version_id,
	 tm.tm_year+1900, tm.tm_yday+1, tm.tm_hour, tm.tm_min, tm.tm_sec,
	 job->src,
	 job->dst,
	 job->job_id,
	 job->platform,
	 job->sensor,
	 job->rev,
	 job->seq,
	 job->activity,
	 job->media_type,
	 job->media_loc,
	 job->media_id, 
	 job->recorder,
	 job->station,
	 job->frame_mode,
	 job->site_name,
	 job->sca_file,
	 job->gha.time.yr, job->gha.time.day, job->gha.time.hr,
		job->gha.time.min, job->gha.time.second,
	 job->gha.angle,

	 job->platform,
	 SV_TypeImage(job->stv.precision),
	 SV_CoordImage(job->stv.coord_sys),
	 job->stv.rev,
	 job->stv.t0.yr, job->stv.t0.day, job->stv.t0.hr, job->stv.t0.min,
		job->stv.t0.second,
	 job->stv.sv[0][0]/1000, job->stv.sv[0][1]/1000, job->stv.sv[0][2]/1000,
	 job->stv.sv[0][3], job->stv.sv[0][4], job->stv.sv[0][5],
	
	 job->tcf.rev,
	 job->tcf.gmt.yr, job->tcf.gmt.day, job->tcf.gmt.hr, job->tcf.gmt.min,
		job->tcf.gmt.second,
	 job->tcf.bt,
	 job->tcf.delta,

	 segmt_cnt,
	 job->reverse ? "REVERSE" : "FORWARD",
	 job->start.blk,
	 job->end.blk,

	 job->start_time.yr, job->start_time.day, job->start_time.hr,
		job->start_time.min, job->start_time.second,

	 job->end_time.yr, job->end_time.day, job->end_time.hr,
		job->end_time.min, job->end_time.second,

	 (segmt_first>=2 && ds[segmt_first-2].type==1 ? ds[segmt_first-2].cal :
	 (segmt_first>=1 && ds[segmt_first-1].type==1 ? ds[segmt_first-1].cal :
		0.0)),
	 (segmt_first>=2 && ds[segmt_first-2].type==2 ? ds[segmt_first-2].cal :
	 (segmt_first>=2 && ds[segmt_first-1].type==2 ? ds[segmt_first-1].cal :
		0.0)),
	 (segmt_first<=i && ds[segmt_first+1].type==1 ? ds[segmt_first+1].cal :
	 (segmt_first<=k && ds[segmt_first+2].type==1 ? ds[segmt_first+2].cal :
		0.0)),
	 (segmt_first<=i && ds[segmt_first+1].type==2 ? ds[segmt_first+1].cal :
	 (segmt_first<=k && ds[segmt_first+2].type==2 ? ds[segmt_first+2].cal :
		0.0))
    );
    segmt_cnt = 0;

    /*--------------------*
     |  For each SEGMENT
     *--------------------*/

    for (i = 0; i < ds_cnt; ++i) {
	register char *c0 = "/*", *c1 = " */";
	double delay;
	register Pulse_t* pp;
	register int end_blk;
	
	if (! (((sp = &ds[i])->pulse_cnt >= PULSE_PER_SEGMT_MIN &&
		 sp->type == -1) || sp->type > 0))
	    continue;

	if (sp->type > 0)
	    n += sprintf(buf+n, "\n/** CAL-%d segment **/", sp->type);
#ifdef DEBUG
	else if (sp->type == -2)
	    n += sprintf(buf+n, "\n/** Beam switch segment **/");
#endif
	else if (job->mode_check && sp->mode != job->mode)
	    n += sprintf(buf+n, "\n/** Mode mismatched segment **/");

	else if (gmt_diff(&sp->gmt_end, &sp->gmt_start) <= 0 ||
	    gmt_diff(&sp->gmt_end, &job->start_time) < -job->gmt_diff_max ||
	    gmt_diff(&sp->gmt_start, &job->end_time) > job->gmt_diff_max)
	
	    n += sprintf(buf+n, sp->gmt_start.yr == 1970
			? "\n/** Segment with un-determinable GMT **/"
			: "\n/** Segment with bad GMT **/");
	else if (sp->ppr_cnt == 0)
	    n += sprintf(buf+n, "\n/** Segment with no frames **/");
	else {
	    segmt_cnt++;
	    c0 = c1 = "";
	}
	pp = sp->pulse[sp->pulse_0+sp->pulse_cnt-1];
	end_blk =  pp->start.blk
		+ (pp->start.bit + pp->frame_cnt * sat->frame_size*8 - 1)
		/ (job->blk_size*8);

	n += sprintf (buf+n, segmt_format, c0, c1,
	     c0, segmt_cnt, c1,
	     c0, ModeImage(sp->mode), c1,
	     c0, job->reverse
		 ? BLKNUM(end_blk, sp,job)
		 : BLKNUM(sp->pulse[sp->pulse_0]->start.blk, sp,job), c1,
	     c0, job->reverse
		 ? BLKNUM(sp->pulse[sp->pulse_0]->start.blk, sp,job)
		 : BLKNUM(end_blk, sp,job), c1,
	     c0, sp->gmt_start.yr, sp->gmt_start.day, sp->gmt_start.hr,
		 sp->gmt_start.min, sp->gmt_start.second, c1,
	     c0, sp->gmt_end.yr, sp->gmt_end.day, sp->gmt_end.hr,
		 sp->gmt_end.min, sp->gmt_end.second, c1,
	     c0, sp->ppr_cnt, c1,
	     c0, sp->prf, c1
	);
	if (sp->type == -1)
	    for (k = 0; k < sp->ppr_cnt; ++k)
		 n += PPR_Save(&sp->ppr[k], buf+n, c0, c1, job);

	n += sprintf(buf+n, "\n%s   WINDOW_POS_CHANGES = (", c0);
	if (sp->type != -1 || sp->ppr_cnt == 0) {

	    /*  No DWP's at all */
	    n += sprintf(buf+n, ")%s\n%s   WINDOW_POS_VALUES = (", c1, c0);
	}
	else {
	    register int i;
	    for (i = 0; i < sp->dwp_cnt && sp->dwp[i] <= sp->pulse_0; ++i);
	    for (k = --i; k < sp->dwp_cnt; ++k)
		n += sprintf(buf+n, "%s%d", k==i ? "" :
		     ((k-i)%8 ? ", " : (*c0 ? ", */\n/*    " : ",\n   ")),
		     k==i ? 0 : (sp->dwp[k]-sp->pulse_0));

	    n += sprintf(buf+n, ")%s\n%s   WINDOW_POS_VALUES = (", c1, c0);
	    for (k = i; k < sp->dwp_cnt; ++k)
		n += sprintf(buf+n, "%s%d", k==i ? "" :
		     ((k-i)%8 ? ", " : (*c0 ? ", */\n/*    " : ",\n   ")),
		     (*sat->DWP)(&delay, 
			(PulseList_t)&sp->pulse[sp->dwp[k]], sp, job));
	}
	if (sp->type != -1 || sp->ppr_cnt == 0 ||
	   (sp->agc_cnt <= 1 && !(*sat->AGC)(
		(PulseList_t)&sp->pulse[sp->agc[sp->pulse_0]],sp,job))) {

	    /*  No AGC's at all */
	    n += sprintf(buf+n, ")%s\n%s   AGC_CHANGES = (", c1, c0);
	    n += sprintf(buf+n, ")%s\n%s   AGC_VALUES = (", c1, c0);
	}
	else {
	    register int i;
	    n += sprintf(buf+n, ")\n%s   AGC_CHANGES = (", c0);

	    for (i = 0; i < sp->agc_cnt && sp->agc[i] <= sp->pulse_0; ++i);
	    for (k = --i; k < sp->agc_cnt; ++k) {
		n += sprintf(buf+n, "%s%d", k==i ? "" :
		    ((k-i)%8 ? ", " : (*c0 ? ", */\n/*    " : ",\n   ")),
		    k==i ? 0 : (sp->agc[k]-sp->pulse_0));
	    }
	    n += sprintf(buf+n, ")\n%s   AGC_VALUES = (", c0);
	    for (k = i; k < sp->agc_cnt; ++k)
		n += sprintf(buf+n, "%s%d", k==i ? "" :
		    ((k-i)%8 ? ", " : (*c0 ? ", */\n/*    " : ",\n   ")),
		    (*sat->AGC)(
			(PulseList_t)&sp->pulse[sp->agc[k]], sp, job));
	}
	n += sprintf(buf+n, ")%s\n%s  END_OBJECT = SEGMENT%s\n", c1, c0, c1);

	if (write(fd, buf, n) != n) {
	    (*job->Log)(job->err, "can't write %d bytes to %s, %s", n,
		        job->sca_file, strerror(errno));
	    close(fd);
	    return 0;
	}
	n = 0;
    } /* Next SEGMENT */

    n += sprintf(buf+n,
	" END_OBJECT = BODY\nEND_OBJECT = SCAN_RESULTS_FILE\nEND\n");

    if (write(fd, buf, n) != n) {
	(*job->Log)(job->err, "can't write %d bytes to %s, %s", n,
		   job->sca_file, strerror(errno));
	close(fd);
	return 0;
    }
    close(fd);
    return n;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |	The following routine exists only for exclusive use by utc2gha.f
 *============================================================================*/
GHA_t gha;
void get_ref_gha_(int *yr, int *day, int *hr, int *min, double *sec,double *ang)
{
    *ang = gha.angle;
    *yr  = gha.time.yr;
    *day = gha.time.day;
    *hr  = gha.time.hr;
    *min = gha.time.min;
    *sec = gha.time.second;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
#define	LO_NORTH	219	/* 0 based, not 1 as output in scan results */
#define HI_NORTH	229
#define LO_SOUTH	669
#define	HI_SOUTH	679
#define HI_LAT(_i)	((LO_NORTH<(_i) && (_i)<HI_NORTH) ||\
			 (LO_SOUTH<(_i) && (_i)<HI_SOUTH))
#define FrameSize(m,j)	(GetFrame(m,j))->azim
#define FrameLat(m,j,i)	(GetFrame(m,j))->lat[i]

FFC_t fix_ctr[FFC_MAX];

FFC_t* GetFrame(Mode_t mode, const Job_t* job)
{
    register FFC_t* fp;
    register int i, rev = job->rev, left = job->frame_mode[1] != 'R';

    for (i = 0; i < FFC_MAX; ++i) {
	fp = &fix_ctr[i];
	if (fp->mode == MODE_INVALID)
	    return NULL;
	if (fp->mode == mode && fp->left == left &&
	    fp->rmin <= rev  && fp->rmax >= rev)
	    break;
    }
    return (i == FFC_MAX ? NULL : fp);
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *============================================================================*/
static
int NextFrame(Mode_t mode, const Job_t* job, int id, double *lat, int *pass)
{
    register FFC_t* fp = GetFrame(mode, job);
    register int i;

    if (!fp) return -1;
    i = (id + 1) % fp->cnt;
    *lat = fp->lat[i];
    *pass = i > (LO_NORTH+HI_NORTH)/2 && i < (LO_SOUTH+HI_SOUTH)/2 ? 'D' : 'A';
    return i;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |      This routine returns the next nearest ascending (or descending) fixed
 |	scene center latitude to the given latitude.
 |
 *============================================================================*/
static
int FindFrame(Mode_t mode, const Job_t* job, double *lat, int *pass)
{
    register FFC_t* fp = GetFrame(mode, job);
    register int i;
    if (!fp) return -1;

    if (*pass == 'A') {
	if (*lat >= fp->lat[LO_NORTH]) {
	    for (i = LO_NORTH; i <= (LO_NORTH+HI_NORTH)/2; ++i)
		if (*lat <= fp->lat[i])
		    break;
	    *lat = fp->lat[i];
	    return (*pass = i <= (LO_NORTH+HI_NORTH)/2 ? 'A' : 'D', i);
	}
	if (*lat <= fp->lat[HI_SOUTH]) {
	    for (i = (LO_SOUTH+HI_SOUTH)/2+1; i <= HI_SOUTH; ++i)
		if (*lat <= fp->lat[i])
		    break;
	    *lat = fp->lat[i];
	    return i;
	}
	if (*lat > 0) {
	    for (i = 0; i < LO_NORTH; ++i)
		if (*lat <= fp->lat[i])
		    break;
	    *lat = fp->lat[i];
	    return i;
	}
	else {
	    for (i = HI_SOUTH; i < fp->cnt; ++i)
		if (*lat <= fp->lat[i])
		    break;
	    *lat = fp->lat[i];
	    return (i < fp->cnt ? i : 0);
	}
    }
    else { /* Descending */
	register int n = fp->cnt/2;

	if (*lat >= fp->lat[HI_NORTH]) {
	    for (i = (LO_NORTH+HI_NORTH)/2+1; i <= HI_NORTH; ++i)
		if (*lat >= fp->lat[i])
		    break;
	    *lat = fp->lat[i];
	    return i;
	}
	if (*lat <= fp->lat[LO_SOUTH]) {
	    for (i = LO_SOUTH; i <= (LO_SOUTH+HI_SOUTH)/2; ++i)
		if (*lat >= fp->lat[i])
		    break;
	    *lat = fp->lat[i];
	    return (*pass = i <= (LO_SOUTH+HI_SOUTH)/2 ? 'D' : 'A', i);
	}
	if (*lat > 0) {
	    for (i = HI_NORTH; i < n; ++i)
		if (*lat >= fp->lat[i])
		    break;
	    *lat = fp->lat[i];
	    return i;
	}
	else {
	    for (i = n; i < LO_SOUTH; ++i)
		if (*lat >= fp->lat[i])
		    break;
	    *lat = fp->lat[i];
	    return i;
	}
    }
}

/*=============================================================================
 |  NAME
 |      fit_dops -
 |
 |  SYPNOSIS
 |	void fit_dops(double xi[3][3], float *abc)
 |
 |  DESCRIPTION
 |      This routine accepts 3 ordinate values, assumed to be spaced at
 |      half-line intervals, and returns the quadratic coefficients
 |      (a=constant, b=linear, c=quadratic) of a line through the points.
 |
 *============================================================================*/
void fit_dops(double xi[3][3], float *y)
{
    int i,j;
    double abc[3] = {0.0, 0.0, 0.0};
    for (i = 0; i < 3; ++i)
	for (j = 0; j < 3; ++j) abc[i] += y[j] * xi[i][j];
    for (i = 0; i < 3; ++i) y[i] = abc[i];
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *============================================================================*/
int get_pass(double* pos_near_start, double* pos_center, double* pos_far_end)
{
    double xx, yy, zz, d1, d2;
/*
    xx = pos_near_start[0] * pos_near_start[0];
    yy = pos_near_start[1] * pos_near_start[1];
    zz = pos_near_start[2] * pos_near_start[2];
    d1 = acos(sqrt((xx + yy) / (xx + yy + zz)));
    xx = pos_far_end[0] * pos_far_end[0];
    yy = pos_far_end[1] * pos_far_end[1];
    zz = pos_far_end[2] * pos_far_end[2];
    d2 = acos(sqrt((xx + yy) / (xx + yy + zz)));
    return (pos_center[5] > 0.0 ? (d2 > d1 ? 'A' : 'D')
				: (d2 < d1 ? 'A' : 'D'));
*/
    return (pos_center[5] > 0.0 ? 'A' : 'D');
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |	Generate STV_MAX predicted statevectors of 1-minute interval
 |
 *============================================================================*/
STV_t* stv_init(STV_t* ref, GMT* gmt0, double sv0[6])
{
    typedef struct {double px,py,pz,vx,vy,vz;} PV;
    static const PV zero = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    double sv[6];
    register int i, k;
    GMT tN = *gmt0;
    ref->t0 = tN;
    gmt_add(&tN, STV_MAX * STV_SPACING);

    for (i = 0; i < 6; ++i) 
	sv[i] = sv0[i] / 1000;

    for (i = 0; i < sizeof(ref->sv)/sizeof(sv); ++i)
	*(PV*) ref->sv[i] = zero;

    predict(sv[0], sv[1], sv[2], sv[3], sv[4], sv[5], &ref->t0, &tN, ref->sv);

    for (i=0; ref->sv[i][0]!=0 && ref->sv[i][1]!=0 && ref->sv[i][2]!=0; ++i)
	for (k = 0; k < 6; ++k) ref->sv[i][k] *= 1000;

#ifdef  DEBUG
{
    register int k;
    printf("%d predicted statevectors follow:\n", i);
    for (k = 0; k < i; ++k)
        printf("%d: %f %f %f %f %f %f\n", k,
                ref->sv[k][0], ref->sv[k][1], ref->sv[k][2],
                ref->sv[k][3], ref->sv[k][4], ref->sv[k][5]);
    fflush(stdout);
}
#endif
    return (i < sizeof(ref->sv)/sizeof(sv) ? ref : 0);
} 
    
/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |	This routine propagates the predicted statevectors 'ref' to GMT tp1. 
 |	and stores the result in the new statevector 'sv1'.
 |
 *============================================================================*/
double* stv_prop(const STV_t* ref, GMT* tp1, double sv1[6])
{
    double tstep = 0.1;
    double tprop = gmt_diff(tp1, &ref->t0);
    int i = (tprop <= 0 ? 0 : ((tprop + STV_SPACING/2.0) / STV_SPACING));

    if (i > STV_MAX-1) i = STV_MAX-1;
    tprop -= i * STV_SPACING;
    if (fabs(tprop) > STV_SPACING * 1.5)
	return NULL;

    newtprop_(ref->sv[i], &tprop, &tstep, sv1);
    return sv1;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
Range_t* get_range(double sv[6],
    double prf, double lkang, double delay, double wdlen, Range_t* r)
{
    int ecoair;			/* Number of echos in the air */
    double theda, height;	/* Spacecraft height */
    if (!sv || !r) return 0;

    height  = sqrt(sv[0]*sv[0]+sv[1]*sv[1]+sv[2]*sv[2]);
    wdlen  /= 2;		/* Window duration / 2 */
    lkang  *= RAD_PER_DEG;	/* Look angle to radians */
    theda   = asin(sin(lkang) * height / EMF_RE);
    ecoair  = ((height * sin(theda - lkang) / sin(theda)) / (MC/2)) * prf;
    delay  += ecoair / prf;

    r->near = (delay * (MC/2));
    r->mid  = (delay += wdlen) * (MC/2);
    r->far  = (delay += wdlen) * (MC/2);

#ifdef  DEBUG
    printf("Pos: <%f, %f, %f>\n", sv[0], sv[1], sv[2]);
    printf("Vel: <%f, %f, %f>\n", sv[3], sv[4], sv[5]);
    printf("Range: <%f, %f, %f>\n", r->near, r->mid, r->far);
#endif
    return r;
}

/*============================================================================*
 |  NAME
 |      get_RYP -
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |      This routine calculates spacecraft attitude from input statevector.
 |
 *============================================================================*/
RYP* get_RYP(const double sv[6], RYP* att)
{
    double ryp[3];
    if (!sv || !att) return 0;
    sv_attd(sv[0]/1000., sv[1]/1000., sv[2]/1000.,
	    sv[3]/1000., sv[4]/1000., sv[5]/1000., ryp);
    att->yaw   = ryp[0];
    att->pitch = ryp[1];
    att->roll  = ryp[2];
#ifdef  DEBUG
    printf("get_RYP(): yaw %g, pitch %g, roll %g\n", ryp[0], ryp[1], ryp[2]);
#endif
    return att;
}

/*============================================================================*
 |  NAME
 |      get_speed -
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |      This routine gets doppler parameters for given preprocessing region.
 |
 *============================================================================*/
int get_speed(int left_look, double sv[6], RYP* att, Range_t* r, double *v_swth,
	      float fd[3], float fdot[3])
{
    int istat;
    float sat_speed, targ_speed, swath_speed, vrel, range;
    float gamma, re = EMF_RE, f = EMF_F, xlambda = XLAMBDA;

    if (!att || !r) return -4;

    /*--------------*
     |  Near range
     *--------------*/
    initdops_( &left_look, sv, &att->roll, &att->yaw, &att->pitch,
	     &xlambda, &re, &f, (range = r->near, &range),
	     /* Output */
             &gamma, &fd[0], &fdot[0], &swath_speed,
             &sat_speed, &targ_speed, &vrel, (istat = 0, &istat));
    if (istat != 0) {
        printfLLog(LOG_ERR, "get_speed() failed in near range");
        return -1;
    }
    /*-------------*
     |  Far range
     *-------------*/
    initdops_( &left_look, sv, &att->roll, &att->yaw, &att->pitch,
             &xlambda, &re, &f, (range = r->far, &range),
	     /* Output */
	     &gamma, &fd[2], &fdot[2], &swath_speed, 
	     &sat_speed, &targ_speed, &vrel, (istat = 0, &istat));
    if (istat != 0) {
        printfLLog(LOG_ERR, "get_speed() failed in far range");
        return -2;
    }
    /*-------------*
     |  Mid range
     *-------------*/
    initdops_( &left_look, sv, &att->roll, &att->yaw, &att->pitch,
             &xlambda, &re, &f, (range = r->mid, &range),
	     /* Output */
	     &gamma, &fd[1], &fdot[1], &swath_speed, 
	     &sat_speed, &targ_speed, &vrel, (istat = 0, &istat));
    if (istat != 0) {
        printfLLog(LOG_ERR, "get_speed() failed in mid range");
        return -3;
    }
    *v_swth = swath_speed;

#ifdef DEBUG
printf("swath_speed %f\n", swath_speed);
#endif
    return 0;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *============================================================================*/
static
double get_dwp(Segmt_t* sp, int fmt_0, int fmt_cnt, const Job_t* job)
{
    const Satellite_t* sat = (Satellite_t*) job->sat;
    register int lo = 0, hi = sp->dwp_cnt;
    double delay;

    while (lo < hi) {
	const int i = (lo+hi)/2;
	const int f = sp->dwp[i];

	if (f == fmt_0) {
	    lo = i+1;
	    break;
	}
	if (f > fmt_0)
	    hi = i;
	else 
	    lo = i+1;
    }
    hi = (*sat->DWP)(&delay, (PulseList_t)&sp->pulse[sp->dwp[lo-1]], sp, job);
#ifdef	DEBUG
    printf("B %d, delay %f, dwp %d::  fmt_0 %d/%d/%d\n",
	(*((Satellite_t*)job->sat)->Beam)(sp->pulse[fmt_0]),
	delay, hi, fmt_0, sp->dwp[lo-1], sp->dwp[lo]);
#endif
    return delay;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *============================================================================*/
static
void get_ll_center(
    double   ll_center[3],	/* Lat/Lon/Rad. at image center */
    /*  Input */
    const double ll_1_near[3],	/* Lat/Lon/Rad. at near start corner */
    const double ll_1_far[3],	/* Lat/Lon/Rad. at far start corner */
    const double ll_N_near[3], 	/* Lat/Lon/Rad. at near end corner */
    const double ll_N_far[3])	/* Lat/Lon/Rad. at far end corner */
{
    double x, y, xmin, xmax, ymin, ymax;

    if ((ll_1_near[0] >  70.0 && ll_1_far[0] >  70.0 &&
	 ll_N_near[0] >  70.0 && ll_N_far[0] >  70.0) ||
	(ll_1_near[0] < -70.0 && ll_1_far[0] < -70.0 &&
	 ll_N_near[0] < -70.0 && ll_N_far[0] < -70.0)) {

	double slat = (ll_1_near[0] < 0 ? -70 : 70);

	ll_ssmi_(&ll_1_near[0], &ll_1_near[1], &xmin, &ymin, &slat);
	xmax = xmin;
	ymax = ymin;
#ifdef	DEBUG
printf("Nea_1 %f/%f, x = %f, y = %f\n", ll_1_near[0], ll_1_near[1], xmin, ymin);
#endif

	ll_ssmi_(&ll_1_far[0], &ll_1_far[1], &x, &y, &slat);
#ifdef	DEBUG
printf("Far_1 %f/%f, x = %f, y = %f\n", ll_1_far[0], ll_1_far[1], x, y);
#endif
	if (xmin > x) xmin = x; else if (xmax < x) xmax = x;
	if (ymin > y) ymin = y; else if (ymax < y) ymax = y;

	ll_ssmi_(&ll_N_near[0], &ll_N_near[1], &x, &y, &slat);
#ifdef	DEBUG
printf("Nea_N %f/%f, x = %f, y = %f\n", ll_N_near[0], ll_N_near[1], x, y);
#endif
	if (xmin > x) xmin = x; else if (xmax < x) xmax = x;
	if (ymin > y) ymin = y; else if (ymax < y) ymax = y;

	ll_ssmi_(&ll_N_far[0], &ll_N_far[1], &x, &y, &slat);
#ifdef	DEBUG
printf("Far_N %f/%f, x = %f, y = %f\n", ll_N_far[0], ll_N_far[1], x, y);
#endif
	if (xmin > x) xmin = x; else if (xmax < x) xmax = x;
	if (ymin > y) ymin = y; else if (ymax < y) ymax = y;

	x = (xmin + xmax) / 2;
	y = (ymin + ymax) / 2;
	ssmi_ll_(&x, &y, &ll_center[0], &ll_center[1], &slat);
#ifdef	DEBUG
printf("Ctr   %f/%f, x = %f, y = %f\n", ll_center[0], ll_center[1], x, y);
#endif
    }
    else {
	double yadj = ((ll_1_near[0] > 0 && ll_1_far[0] > 0 &&
			ll_N_near[0] > 0 && ll_N_far[0] > 0) ? 0 : 1e7);
	double lon_1_near = ll_1_near[1];
	double lon_N_near = ll_N_near[1];
	double lon_1_far  = ll_1_far[1];
	double lon_N_far  = ll_N_far[1];
	int zone;

	if (ll_1_near[1] * ll_N_far[1] > 0 &&
	    ll_1_near[1] * ll_1_far[1] > 0 &&
	    ll_1_near[1] * ll_N_near[1]> 0) {
	    zone = (180 + (ll_1_near[1] + ll_N_far[1])/2) / 6 + 1;
	}
	else if ((ll_1_near[1] > 0 && (180-ll_1_near[1]) > (180+ll_N_far[1])) ||
		 (ll_1_near[1] < 0 && (180+ll_1_near[1]) < (180-ll_N_far[1]))) {
	    if (ll_1_near[1] < 0) lon_1_near += 360;
	    if (ll_N_near[1] < 0) lon_N_near += 360;
	    if (ll_1_far[1]  < 0) lon_1_far  += 360;
	    if (ll_N_far[1]  < 0) lon_N_far  += 360;
	    zone = 60;
	}
	else {
	    if (ll_1_near[1] > 0) lon_1_near -= 360;
	    if (ll_N_near[1] > 0) lon_N_near -= 360;
	    if (ll_1_far[1]  > 0) lon_1_far  -= 360;
	    if (ll_N_far[1]  > 0) lon_N_far  -= 360;
	    zone = 1;
	}
	ll_utm_(&ll_1_near[0], &lon_1_near, &xmin, &ymin, &yadj, &zone);
	xmax = xmin;
	ymax = ymin;
#ifdef	DEBUG
printf("Ne1 :: %f/%f, %f, %f/%f - %d\n",ll_1_near[0], ll_1_near[1], lon_1_near,
	xmin, ymin, zone);
#endif

	ll_utm_(&ll_1_far[0], &lon_1_far, &x, &y, &yadj, &zone);
	if (xmin > x) xmin = x; else if (xmax < x) xmax = x;
	if (ymin > y) ymin = y; else if (ymax < y) ymax = y;
#ifdef	DEBUG
printf("Fa1 :: %f/%f, %f, %f/%f - %d\n", ll_1_far[0], ll_1_far[1], lon_1_far,
	x,y, zone);
#endif

	ll_utm_(&ll_N_near[0], &lon_N_near, &x, &y, &yadj, &zone);
	if (xmin > x) xmin = x; else if (xmax < x) xmax = x;
	if (ymin > y) ymin = y; else if (ymax < y) ymax = y;
#ifdef	DEBUG
printf("NeN :: %f/%f, %f, %f/%f - %d\n", ll_N_near[0], ll_N_near[1],lon_N_near,
	x,y, zone);
#endif

	ll_utm_(&ll_N_far[0], &lon_N_far, &x, &y, &yadj, &zone);
	if (xmin > x) xmin = x; else if (xmax < x) xmax = x;
	if (ymin > y) ymin = y; else if (ymax < y) ymax = y;
#ifdef	DEBUG
printf("FaN :: %f/%f, %f, %f/%f - %d\n", ll_N_far[0], ll_N_far[1], lon_N_far,
	x,y, zone);
#endif

	x = (xmin + xmax) / 2;
	y = (ymin + ymax) / 2;
	utm_ll_(&x, &y, &ll_center[0], &ll_center[1], &yadj, &zone);
#ifdef	DEBUG
printf("Cen :: %f/%f, %f/%f - %d\n", ll_center[0],ll_center[1], x,y, zone);
#endif
	if (ll_center[1] > 180)
	    ll_center[1] -= 360;
	else if (ll_center[1] < -180)
	    ll_center[1] += 360;
    }
    ll_center[2] = (ll_1_near[2]+ll_N_far[2]+ll_1_far[2]+ll_N_near[2]) / 4;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *============================================================================*/
static
int get_location(
    /*  Output */
    double   ll_near_start[3],		/* Lat/Lon/Rad. at near start corner */
    double   ll_far_start[3],		/* Lat/Lon/Rad. at far start corner */
    double   ll_center[3],		/* Lat/Lon/Rad. at image center */
    double   ll_near_end[3], 		/* Lat/Lon/Rad. at near end corner */
    double   ll_far_end[3],		/* Lat/Lon/Rad. at far end corner */
    double   pv_near_start[6],		/* Pos/Vel. at near start corner */
    double   pv_far_start[6],		/* Pos/Vel. at far start corner */
    double   pv_center[6],		/* Pos/Vel. at image center */
    double   pv_near_end[6], 		/* Pos/Vel. at near end corner */
    double   pv_far_end[6],		/* Pos/Vel. at far end corner */
    GMT*     gmt_start,			/* GMT of first range line */
    GMT*     gmt_center,		/* GMT at center */
    GMT*     gmt_end,			/* GMT of last range line */
    Range_t* range,			/* Near/Mid/Far ranges of image */
    double*  swth_speed,		/* Swath speed at center */

    /*  Input */
    const Satellite_t* sat,		/* Satellite specific functions */
    const STV_t* stv_ref,		/* Reference statevector */
    int	     framemode,			/* 'R' => right looking, else left */
    double   framesize,			/* Frame size in meters */
    int	     nc,			/* Number of scansar cycles */

    /*  Input Per Corner-Burst */
    GMT*    gmt,			/* GMT of first line of frame */
    int	    np,				/* Number of pulses in this burst */
    double  prf,			/* Pulse repetition frequency */
    double  lookangle,			/* Nominal look angle in degrees */
    double  wduration,			/* Window duration in seconds */
    double  dwp_delay,			/* Window position change in seconds */

    ...)
{
    typedef struct {double x,y,z;} LL;
    typedef struct {double x,y,z,a,b,c;} PV;

    register int n;
    double ll_1_near[3], ll_N_near[3], ll_1_far[3], ll_N_far[3];
    double pv_1_near[6], pv_N_near[6], pv_1_far[6], pv_N_far[6];
    double vs_1_near, vs_N_near, vs_1_far, vs_N_far, delta;
    Range_t rn_1_near, rn_N_near, rn_1_far, rn_N_far;
    GMT t0;
    va_list ap;

    if (nc == 0) {
	float  fd3p[3], fr3p[3];
	double fd_adj = 0.0;
	double sv_center[6];
	int istat, left_look = (framemode != 'R');
	RYP att;

	/*--------------------* 
	 |  Single beam case
	 *--------------------*/
	delta = np / prf;
	*gmt_center = *gmt_end = *gmt_start = *gmt;
	gmt_add(gmt_center, delta/2);
	gmt_add(gmt_end, delta);

	if (! stv_prop(stv_ref, gmt_center, sv_center)) {
/*
	    printfLLog(LOG_DEBUG,
"Can't propagate to %.4d-%.3dT%.2d:%.2d:%09.6f, prf %f",
	    gmt_center->yr, gmt_center->day, gmt_center->hr,
	    gmt_center->min, gmt_center->second, prf);
*/
	    return -1;
	}
	/*  Calculate number of range lines using S/C swath speed at center */
	istat = get_speed(
	    left_look,
	    sv_center,
	    (*sat->RYP)(sv_center, &att),
	    get_range(sv_center, prf, lookangle, dwp_delay, wduration, range),
	    swth_speed,
	    fd3p,
	    fr3p
	);
	if (istat < 0)
	    return -1;

	if (sat->fd3p[0] >= 0) {
	    typedef struct {float x[3];} X;
	    *(X*)fd3p = *(X*)sat->fd3p;
	    *(X*)fr3p = *(X*)sat->fr3p;
	}
	corn_loc(
	    left_look,
	    /* Inputs (should be 13 of 'em) */
	    sv_center, gmt_center, gmt_start, (double*) range, fd3p, fr3p, np,
	    NO_DESKEW, EMF_RE, EMF_RP, XLAMBDA, fd_adj, 1/prf,
	    /* Outputs (should be 11 of 'em) */
	    ll_near_start, ll_far_start, ll_center, ll_near_end, ll_far_end,
	    pv_near_start, pv_far_start, pv_center, pv_near_end, pv_far_end,
	    &istat);

	return (istat != 0 ? -1 : ((framesize / *swth_speed) * prf + 1));
    }
    va_start(ap, dwp_delay);

    /*---------------------*
     |  Near Start Corner
     *---------------------*/
    n = get_location(
	ll_near_start, ll_far_start, ll_center, ll_near_end, ll_far_end,
	pv_near_start, pv_far_start, pv_center, pv_near_end, pv_far_end,
	gmt_start, gmt_center, gmt_end, &rn_1_near, &vs_1_near,
	sat, stv_ref, framemode, framesize, 0,
	gmt, np, prf, lookangle, wduration, dwp_delay
    );
#ifdef	DEBUG
printf("Nea_1: %.4d-%.3dT%.2d:%.2d:%09.6f, LK %f DW %f, %f/%f\n",
	gmt->yr, gmt->day, gmt->hr, gmt->min, gmt->second, lookangle,dwp_delay,
	wduration, prf);
#endif
    if (n < 0) return -1;

    *(LL*) ll_1_near = *(LL*) ll_near_start;
    *(PV*) pv_1_near = *(PV*) pv_near_start;
    t0 = *gmt_start;
	
    /*---------------------*
     |  Far Start Corner
     *---------------------*/
    gmt = va_arg(ap, GMT*);
    np  = va_arg(ap, int);
    prf = va_arg(ap, double);
    lookangle = va_arg(ap, double);
    wduration = va_arg(ap, double);
    dwp_delay = va_arg(ap, double);

    n = get_location(
	ll_near_start, ll_far_start, ll_center, ll_near_end, ll_far_end,
	pv_near_start, pv_far_start, pv_center, pv_near_end, pv_far_end,
	gmt_start, gmt_center, gmt_end, &rn_1_far, &vs_1_far,
	sat, stv_ref, framemode, framesize, 0,
	gmt, np, prf, lookangle, wduration, dwp_delay
    );
#ifdef	DEBUG
printf("Far_1: %.4d-%.3dT%.2d:%.2d:%09.6f, LK %f DW %f, %f/%f\n",
	gmt->yr, gmt->day, gmt->hr, gmt->min, gmt->second, lookangle,dwp_delay,
	wduration, prf);
#endif
    if (n < 0) return -1;

    *(LL*) ll_1_far = *(LL*) ll_far_start;
    *(PV*) pv_1_far = *(PV*) pv_far_start;

    /*-------------------*
     |  Near End Corner
     *-------------------*/
    gmt = va_arg(ap, GMT*);
    np  = va_arg(ap, int);
    prf = va_arg(ap, double);
    lookangle = va_arg(ap, double);
    wduration = va_arg(ap, double);
    dwp_delay = va_arg(ap, double);

    n = get_location(
	ll_near_start, ll_far_start, ll_center, ll_near_end, ll_far_end,
	pv_near_start, pv_far_start, pv_center, pv_near_end, pv_far_end,
	gmt_start, gmt_center, gmt_end, &rn_N_near, &vs_N_near,
	sat, stv_ref, framemode, framesize, 0,
	gmt, np, prf, lookangle, wduration, dwp_delay
    );
#ifdef	DEBUG
printf("Nea_N: %.4d-%.3dT%.2d:%.2d:%09.6f, LK %f DW %f, %f/%f\n",
	gmt->yr, gmt->day, gmt->hr, gmt->min, gmt->second, lookangle,dwp_delay,
	wduration, prf);
#endif
    if (n < 0) return -1;

    *(LL*) ll_N_near = *(LL*) ll_near_end;
    *(PV*) pv_N_near = *(PV*) pv_near_end;

    /*-------------------*
     |  Far End Corner
     *-------------------*/
    gmt = va_arg(ap, GMT*);
    np  = va_arg(ap, int);
    prf = va_arg(ap, double);
    lookangle = va_arg(ap, double);
    wduration = va_arg(ap, double);
    dwp_delay = va_arg(ap, double);

    va_end(ap);

    n = get_location(
	ll_near_start, ll_far_start, ll_center, ll_near_end, ll_far_end,
	pv_near_start, pv_far_start, pv_center, pv_near_end, pv_far_end,
	gmt_start, gmt_center, gmt_end, &rn_N_far, &vs_N_far,
	sat, stv_ref, framemode, framesize, 0,
	gmt, np, prf, lookangle, wduration, dwp_delay
    );
#ifdef	DEBUG
printf("Far_N: %.4d-%.3dT%.2d:%.2d:%09.6f, LK %f DW %f, %f/%f\n",
	gmt->yr, gmt->day, gmt->hr, gmt->min, gmt->second, lookangle,dwp_delay,
	wduration, prf);
#endif
    if (n < 0) return -1;
    
    *(LL*) ll_N_far = *(LL*) ll_far_end;

    /*------------------------------------------*
     |  Merge frame info. from 4 corner bursts
     *------------------------------------------*/
    *(LL*) ll_near_start = *(LL*) ll_1_near;
    *(LL*) ll_near_end   = *(LL*) ll_N_near;
    *(LL*) ll_far_start  = *(LL*) ll_1_far;
    *(PV*) pv_near_start = *(PV*) pv_1_near;
    *(PV*) pv_near_end   = *(PV*) pv_N_near;
    *(PV*) pv_far_start  = *(PV*) pv_1_far;

    /*  Near/Mid/Far Range */
    range->near = (rn_1_near.near + rn_N_near.near) / 2;
    range->far  = (rn_1_far.far + rn_N_far.far) / 2;
    range->mid  = (range->near + range->far) / 2;

    *gmt_start = t0;

    /*  Statevector at center */
    *gmt_center = *gmt_start;
    gmt_add(gmt_center, (delta = gmt_diff(gmt_end, gmt_start)) / 2);
    stv_prop(stv_ref, gmt_center, pv_center);

    /*  Center Lat/Lon = Center of the immediate enclosing rectangle */
    get_ll_center(ll_center, ll_1_near, ll_1_far, ll_N_near, ll_N_far);

    /*  Swath speed at center */ 
    *swth_speed = (vs_1_near + vs_1_far + vs_N_near + vs_N_far) / 4;

    return (int) ((framesize / *swth_speed) * nc / delta + 1);
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
static
int PPR_distance(const PPR_t* pp, double target_lat, int target_pass)
{
    double x0, x1, y0, y1, delta, target_lon;
    target_lon =
	(pp->pass == target_pass) ? pp->ll_center[1] :
        (pp->ll_center[1] < 0 ? (180+pp->ll_center[1]) :(pp->ll_center[1]-180));

    if ((pp->ll_1_near[0] >  70.0 && pp->ll_1_far[0] >  70.0 &&
	 pp->ll_N_near[0] >  70.0 && pp->ll_N_far[0] >  70.0) ||
	(pp->ll_1_near[0] < -70.0 && pp->ll_1_far[0] < -70.0 &&
	 pp->ll_N_near[0] < -70.0 && pp->ll_N_far[0] < -70.0)) {

	double slat = (pp->ll_1_near[0] < 0 ? -70 : 70);
#ifndef	DEBUG
	ll_ssmi_(&pp->ll_1_near[0], &pp->ll_center[1], &x0, &y0, &slat);
	ll_ssmi_(&pp->ll_N_near[0], &pp->ll_center[1], &x1, &y1, &slat);
	delta = pp->fmt_cnt / sqrt((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0));
	ll_ssmi_(&pp->ll_center[0], &pp->ll_center[1], &x0, &y0, &slat);
	ll_ssmi_(&target_lat, &target_lon, &x1, &y1, &slat);
    }
    else {
	double yadj = ((pp->ll_1_near[0] > 0 && pp->ll_1_far[0] > 0 &&
			pp->ll_N_near[0] > 0 && pp->ll_N_far[0] > 0) ? 0 : 1e7);
	int zone = (180 + pp->ll_center[1]) / 6 + 1;

	ll_utm_(&pp->ll_1_near[0], &pp->ll_center[1], &x0, &y0, &yadj, &zone);
	ll_utm_(&pp->ll_N_near[0], &pp->ll_center[1], &x1, &y1, &yadj, &zone);
	delta = pp->fmt_cnt / sqrt((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0));
	ll_utm_(&pp->ll_center[0], &pp->ll_center[1], &x0, &y0, &yadj, &zone);
	ll_utm_(&target_lat, &target_lon, &x1, &y1, &yadj, &zone);
    }
#else
	ll_ssmi_(&pp->ll_1_near[0], &pp->ll_1_near[1], &x0, &y0, &slat);
	ll_ssmi_(&pp->ll_N_near[0], &pp->ll_N_near[1], &x1, &y1, &slat);
	delta = pp->fmt_cnt / sqrt((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0));
	ll_ssmi_(&pp->ll_center[0], &pp->ll_center[1], &x0, &y0, &slat);
	ll_ssmi_(&target_lat, &target_lon, &x1, &y1, &slat);
    }
    else {
	double yadj = ((pp->ll_1_near[0] > 0 && pp->ll_1_far[0] > 0 &&
			pp->ll_N_near[0] > 0 && pp->ll_N_far[0] > 0) ? 0 : 1e7);
	double lon_1_near = pp->ll_1_near[1];
	double lon_N_near = pp->ll_N_near[1];
	double lon_1_far  = pp->ll_1_far[1];
	double lon_N_far  = pp->ll_N_far[1];
	double lon_center = pp->ll_center[1];
	int zone;

	if (pp->ll_1_near[1] * pp->ll_N_far[1] > 0 &&
	    pp->ll_1_near[1] * pp->ll_1_far[1] > 0 &&
	    pp->ll_1_near[1] * pp->ll_N_near[1]> 0) {

	    zone = (180 + (pp->ll_1_near[1] + pp->ll_N_far[1])/2) / 6 + 1;
	}
	else if ((pp->ll_1_near[1] > 0 &&
			(180-pp->ll_1_near[1]) > (180+pp->ll_N_far[1])) ||
		 (pp->ll_1_near[1] < 0 &&
			(180+pp->ll_1_near[1]) < (180-pp->ll_N_far[1]))) {

	    if (pp->ll_1_near[1] < 0) lon_1_near += 360;
	    if (pp->ll_N_near[1] < 0) lon_N_near += 360;
	    if (pp->ll_1_far[1]  < 0) lon_1_far  += 360;
	    if (pp->ll_N_far[1]  < 0) lon_N_far  += 360;
	    if (pp->ll_center[1] < 0) lon_center += 360;
	    zone = 60;
	}
	else {
	    if (pp->ll_1_near[1] > 0) lon_1_near -= 360;
	    if (pp->ll_N_near[1] > 0) lon_N_near -= 360;
	    if (pp->ll_1_far[1]  > 0) lon_1_far  -= 360;
	    if (pp->ll_N_far[1]  > 0) lon_N_far  -= 360;
	    if (pp->ll_center[1] > 0) lon_center -= 360;
	    zone = 1;
	}
	ll_utm_(&pp->ll_1_near[0], &lon_1_near, &x0, &y0, &yadj, &zone);
	ll_utm_(&pp->ll_N_near[0], &lon_N_near, &x1, &y1, &yadj, &zone);
	delta = pp->fmt_cnt / sqrt((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0));
	ll_utm_(&pp->ll_center[0], &lon_center, &x0, &y0, &yadj, &zone);
	ll_utm_(&target_lat, &target_lon, &x1, &y1, &yadj, &zone);
    }
#endif
    delta *= sqrt((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0));
    x0 = pp->fmt_cnt * 0.4;
/*  x0 = (((Segmt_t*) pp->segmt)->burst_len > 1 ? 0.08 : 0.4); */

#ifdef	DEBUG
printf("target_lat %f/%c, center %f/%c, delta %f/%d, x0 %f\n",
	target_lat, target_pass, 
	pp->ll_center[0], pp->pass, delta, (int)delta, x0);
#endif
    if (delta > x0)
	delta = x0;

    if (delta < 1.0 && fabs(pp->ll_center[0] - target_lat) > 
		(((Segmt_t*) pp->segmt)->burst_len > 1 ? 1.0e-2 : 1.0e-3))
	delta = 1.0;
	
    return (target_pass == 'A')
	? ((pp->pass == 'A') ? (target_lat > pp->ll_center[0] ? delta : -delta)
	    		     : -delta /* (target_lat < 0 ? delta : -delta) */)
	: ((pp->pass == 'D') ? (target_lat < pp->ll_center[0] ? delta : -delta)
	    		     : -delta /* (target_lat > 0 ? delta : -delta) */);
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |	c0	Starting cycle (for ScanSAR) or pulse (for continuous).
 |	nc 	Number of cycles or pulses.
 |  DESCRIPTION
 *============================================================================*/
static
int PPR_location(PPR_t* pp, Segmt_t* sp, const Job_t* job, int c0, int nc,
		double gmt_offset)
{
    const int nb = (sp->mode == SNA ? 2 : (sp->mode == SNB ? 3 : 4));
    const Satellite_t* sat = (Satellite_t*) job->sat;
    register int i, n, cN = c0 + nc - 1;

    if (c0 < 0 ||
	cN >= (sp->burst_len > 1 ? (sp->burst_cnt/nb) : sp->pulse_cnt)) {
#ifdef	DEBUG
printf("PPR_location: not enough data - at %d, want next %d, has only %d\n",
	c0, nc, sp->burst_len > 1 ? (sp->burst_cnt/nb) : sp->pulse_cnt);
#endif
	return -1;
    }
    if (sp->burst_len <= 1) {
	GMT gmt;
	double dwp_delay = get_dwp(sp, c0+sp->pulse_0, nc, job);

	pp->start = sp->pulse[c0+sp->pulse_0]->start;
	i = cN + sp->pulse_0;
	(*sat->GMT)(&pp->gmt_start, 
	    (PulseList_t)&sp->pulse[c0+sp->pulse_0], sp, job);
	gmt = pp->gmt_start;
	if (gmt_offset != 0)
	    gmt_add(&gmt, gmt_offset);

	n = get_location(
	pp->ll_1_near, pp->ll_1_far, pp->ll_center, pp->ll_N_near, pp->ll_N_far,
        pp->pv_1_near, pp->pv_1_far, pp->pv_center, pp->pv_N_near, pp->pv_N_far,
	&pp->gmt_start,&pp->gmt_center,&pp->gmt_end, &pp->range,&pp->swth_speed,
	    sat, &job->stv, job->frame_mode[1], FrameSize(sp->mode, job), 0,
	    &gmt, nc, sp->prf, sp->lookangle,
	    (*sat->WDuration)((PulseList_t)&sp->pulse[c0+sp->pulse_0], sp, job),
	    dwp_delay
	);
    }
    else { /* ScanSAR */
	const int np     = sp->burst_len;
	const int near_1 = sp->burst[i = sp->burst_0+c0*nb];
	const int near_N = sp->burst[n = sp->burst_0+cN*nb];
	const int far_1  = sp->burst[i + nb-1];
	const int far_N  = sp->burst[n + nb-1];

	double near_1_dwp = get_dwp(sp, near_1, np, job);
	double near_1_prf = (*sat->PRF)(
				(PulseList_t)&sp->pulse[near_1], sp, job);
	double near_1_ang = (*sat->LookAngle)(
				(PulseList_t)&sp->pulse[near_1], sp, job);
	double near_1_win = (*sat->WDuration)(
				(PulseList_t)&sp->pulse[near_1], sp, job);

	double near_N_dwp = get_dwp(sp, near_N, np, job);
	double near_N_prf = near_1_prf;
	double near_N_ang = near_1_ang;
	double near_N_win = near_1_win;

	double far_1_dwp  = get_dwp(sp, far_1, np, job);
	double far_1_prf  = (*sat->PRF)(
				(PulseList_t)&sp->pulse[far_1], sp, job);
	double far_1_ang  = (*sat->LookAngle)(
				(PulseList_t)&sp->pulse[far_1], sp, job);
	double far_1_win  = (*sat->WDuration)(
				(PulseList_t)&sp->pulse[far_1], sp, job);

	double far_N_dwp  = get_dwp(sp, far_N, np, job);
	double far_N_prf  = far_1_prf;
	double far_N_ang  = far_1_ang;
	double far_N_win  = far_1_win;

	GMT near_1_gmt, near_N_gmt, far_1_gmt, far_N_gmt;

	pp->start = sp->pulse[near_1]->start;
	i = far_N+np-1;

	(*sat->GMT)(&near_1_gmt, (PulseList_t)&sp->pulse[near_1], sp, job);
	(*sat->GMT)(&near_N_gmt, (PulseList_t)&sp->pulse[near_N], sp, job);
	(*sat->GMT)(&far_1_gmt,  (PulseList_t)&sp->pulse[far_1],  sp, job);
	(*sat->GMT)(&far_N_gmt,  (PulseList_t)&sp->pulse[far_N],  sp, job);

	if (gmt_offset != 0) {
	    gmt_add(&near_1_gmt, gmt_offset);
	    gmt_add(&near_N_gmt, gmt_offset);
	    gmt_add(&far_1_gmt,  gmt_offset);
	    gmt_add(&far_N_gmt,  gmt_offset);
	}
	n = get_location(
        pp->ll_1_near,pp->ll_1_far, pp->ll_center, pp->ll_N_near, pp->ll_N_far,
        pp->pv_1_near,pp->pv_1_far, pp->pv_center, pp->pv_N_near, pp->pv_N_far,
	&pp->gmt_start,&pp->gmt_center,&pp->gmt_end,&pp->range,&pp->swth_speed,
	    sat, &job->stv, job->frame_mode[1], FrameSize(sp->mode, job), nc,
	    &near_1_gmt, np, near_1_prf, near_1_ang, near_1_win, near_1_dwp,
	    &far_1_gmt,  np, far_1_prf,  far_1_ang,  far_1_win,  far_1_dwp,
	    &near_N_gmt, np, near_N_prf, near_N_ang, near_N_win, near_N_dwp,
	    &far_N_gmt,  np, far_N_prf,  far_N_ang,  far_N_win,  far_N_dwp
	);
#ifdef	DEBUG
printf("Ne1: %.4d-%.3dT%.2d:%.2d:%09.6f p %f w %f d %f %f/%f\n",
	near_1_gmt.yr, near_1_gmt.day, near_1_gmt.hr,
	near_1_gmt.min, near_1_gmt.second,
	near_1_prf, near_1_win, near_1_dwp, pp->ll_1_near[0],pp->ll_1_near[1]);
printf("NeN: %.4d-%.3dT%.2d:%.2d:%09.6f p %f w %f d %f %f/%f\n",
	near_N_gmt.yr, near_N_gmt.day, near_N_gmt.hr,
	near_N_gmt.min, near_N_gmt.second,
	near_N_prf, near_N_win, near_N_dwp, pp->ll_N_near[0],pp->ll_N_near[1]);
printf("Fa1: %.4d-%.3dT%.2d:%.2d:%09.6f p %f w %f d %f %f/%f\n",
	far_1_gmt.yr, far_1_gmt.day, far_1_gmt.hr,
	far_1_gmt.min, far_1_gmt.second,
	far_1_prf, far_1_win, far_1_dwp, pp->ll_1_far[0], pp->ll_1_far[1]);
printf("FaN: %.4d-%.3dT%.2d:%.2d:%09.6f p %f w %f d %f %f/%f\n",
	far_N_gmt.yr, far_N_gmt.day, far_N_gmt.hr,
	far_N_gmt.min, far_N_gmt.second,
	far_N_prf, far_N_win, far_N_dwp, pp->ll_N_far[0], pp->ll_N_far[1]);
#endif
    }
    if (n < 0) return n;

    if (i+1 < sp->pulse_cnt)
	pp->end = sp->pulse[i+1]->start;
    else {
	pp->end = sp->pulse[i]->start;
	pp->end.bit += sp->pulse[i]->frame_cnt * sat->frame_size*8 - 1;
	pp->end.blk += pp->end.bit / (job->blk_size*8);
	pp->end.bit %= job->blk_size*8;
    }
    pp->fmt_start = c0;
    pp->fmt_cnt = nc;
    pp->pass = get_pass(pp->pv_1_near, pp->pv_center, pp->pv_N_far);

#ifdef	DEBUG
    printf("CTR:  %f/%f/%c, GMT %.4d-%.3dT%.2d:%.2d:%09.6f, %d\n\n",
	pp->ll_center[0], pp->ll_center[1], pp->pass,
	pp->gmt_center.yr, pp->gmt_center.day, pp->gmt_center.hr,
	pp->gmt_center.min, pp->gmt_center.second, c0);
#endif
    return n;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
static
PPR_t* goto_lat(PPR_t* pp, Segmt_t* sp, const Job_t* job, double lat, int pass,
		int extrapolate, GMT* gmt)
{
    PPR_t ps;
    const int nb = (sp->mode == SNA ? 2 : (sp->mode == SNB ? 3 : 4));
    register int hi = sp->burst_len > 1 ? (sp->burst_cnt/nb) : sp->pulse_cnt;
    register int lo = 0;
    register int n  = pp->fmt_cnt * 0.4; /* PPR_distance(pp, lat, pass); */

    if (extrapolate) {
	const double pri = sp->burst_len > 1
	    ? (gmt_diff(&pp->gmt_end, &pp->gmt_start) / pp->fmt_cnt)
	    : (1 / sp->prf);
	register int fmt_start = pp->fmt_start;
	register int fmt_initial = pp->fmt_start;

	lo -= (int)(120/pri);
	hi += (int)(120/pri);

	for (;;) {
	    if ((n > 0 ? n : -n) >= (hi-lo))
		n = (n > 0 ? (hi-lo) : (lo-hi))/2;

	    if (n == 0 || lo >= hi)
		break;
#ifdef	DEBUG
printf("Goto: @%d, LAT %f/%c, lo %d, hi %d, skipping %d.\n",
	fmt_start, pp->ll_center[0], pp->pass, lo, hi, n);
#endif
	    ps = *pp;
	    if (PPR_location(pp, sp, job, fmt_initial, pp->fmt_cnt,
		    (fmt_start + n - fmt_initial) * pri)
		<= 0) {
		*pp = ps;
		hi = fmt_start + (n > 0 ? n : 0);
		n /= 2;
	    }
	    else if (fmt_start += n, (n = PPR_distance(pp, lat, pass)) > 0)
		lo = fmt_start;
	    else 
		hi = fmt_start;
	}
#ifdef	DEBUG
printf("Goto: @%d, LAT %f/%c, DES %f/%c, lo %d hi %d, n %d *********\n",
	fmt_start, pp->ll_center[0], pp->pass, lat, pass, lo, hi, n);
#endif
    }
    else {
	for (;;) {
	    if ((n > 0 ? n : -n) >= (hi-lo))
		n = (n > 0 ? (hi-lo) : (lo-hi))/2;

	    if (n == 0 || lo >= hi)
		break;
#ifdef	DEBUG
printf("Goto: @%d, LAT %f/%c, lo %d, hi %d, skipping %d\n",
	pp->fmt_start, pp->ll_center[0], pp->pass, lo, hi, n);
#endif
	    ps = *pp;
	    if (PPR_location(pp, sp, job, pp->fmt_start+n, pp->fmt_cnt, 0)
		<= 0) {
		*pp = ps;
		hi = pp->fmt_start + (n > 0 ? n : 0);
		n /= 2;
	    }
	    else if ((n = PPR_distance(pp, lat, pass)) > 0)
		lo = pp->fmt_start;
	    else
		hi = pp->fmt_start;
	}
#ifdef	DEBUG
printf("Goto: @%d, LAT %f/%c, DES %f/%c, lo %d hi %d, n %d ---------\n",
	pp->fmt_start, pp->ll_center[0], pp->pass, lat, pass, lo, hi, n);
#endif
    }
    if (fabs(pp->ll_center[0] - lat) > (sp->burst_len > 1 ? 1.0e-2 : 1.0e-3)) {

	if (extrapolate)
	    return NULL;

	ps = *pp;
	if (! goto_lat(pp, sp, job, lat, pass, 1, gmt))
	    return NULL;

	if (fabs(gmt_diff(&pp->gmt_center, &ps.gmt_center)) > job->lat_error)
	    return NULL;

	*pp = ps;
    }
    *gmt = pp->gmt_center;
    return pp;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
static
PPR_t* goto_gmt(PPR_t* pp, Segmt_t* sp, const Job_t* job, GMT* desired_gmt)
{
    PPR_t ps = *pp;
    const double pri = sp->burst_len > 1
	? (gmt_diff(&pp->gmt_end, &pp->gmt_start) / pp->fmt_cnt)
	: (1 / sp->prf);
    register double off = gmt_diff(&pp->gmt_center, desired_gmt);
    register int n;

    while ((off < 0 ? -off : off) > pri && (n = -off/pri)) {
	PPR_t ps = *pp;
	if (PPR_location(pp, sp, job, pp->fmt_start+n, pp->fmt_cnt, 0) <= 0) {
	    *pp = ps;
	    break;
	}
	off = gmt_diff(&pp->gmt_center, desired_gmt);
    }
    return (fabs(off) <= job->lat_error ? pp : NULL);
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
static
int get_hidelta(register PPR_t* pp, Segmt_t* sp, const Job_t* job)
{
    const Satellite_t* sat = (Satellite_t*) job->sat;
    register FFC_t* fp = GetFrame(sp->mode, job);
    register int i, i0, i1, pass0, pass1, updated = 0;
    PPR_t ps = *pp;
    double lat0, lat1;
    GMT t0, t1;

    if (sp->hidelta > 0)
	return 1;

    if (sp->hidelta < 0)
	return 0;

    if (pp->frame_id < HI_NORTH) {
	i0 = LO_NORTH; pass0 = 'A';
	i1 = HI_NORTH; pass1 = 'D';
    }
    else {
	i0 = LO_SOUTH; pass0 = 'D';
	i1 = HI_SOUTH; pass1 = 'A';
    }
    lat0 = FrameLat(sp->mode, job, i0);
    lat1 = FrameLat(sp->mode, job, i1);

    if (goto_lat(pp, sp, job, lat0, pass0, 0, &t0)) {
	*pp = ps;
	if (goto_lat(pp, sp, job, lat1, pass1, 0, &t1))
	    updated = 1;
    }
    if (! updated) {
	*pp = ps;
	if (goto_lat(pp, sp, job, lat0, pass0, 1, &t0)) {
	    *pp = ps;
	    if (goto_lat(pp, sp, job, lat1, pass1, 1, &t1))
		updated = 1;
	}
    }
    if (! updated) {
	printfLLog(LOG_ERR, "Frames %d..%d are not equally time-spaced",
		   i0+1, i1+1);
	sp->hidelta = -1;
	return 0;
    }
    sp->hidelta = gmt_diff(&t1, &t0) / (HI_NORTH - LO_NORTH);
    sp->gmt_hilat = t0;
    *pp = ps;

#ifdef	DEBUG
printf("hidelta: %f, ref: %.4d-%.3dT%.2d:%.2d:%09.6f\n", sp->hidelta,
	t0.yr, t0.day, t0.hr, t0.min, t0.second);
#endif
    return 1;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
PPR_t* PPR_first(register PPR_t* pp, Segmt_t* sp, const Job_t* job)
{
    const Satellite_t* sat = (Satellite_t*) job->sat;
    register int i, nc;
    register PPR_t* ok;
    double new_lat, pri;
    int new_pass;
    GMT new_gmt;
    pp->segmt = (void*) sp;

    if (! GetFrame(sp->mode, job)) {
        printfLLog(LOG_ERR,
	"%s/%d/%s frame table missing - no frames generated\n",
            ModeImage(sp->mode), job->rev, job->frame_mode);
        return NULL;
    }
    nc = sp->burst_len > 1
       ? ((sp->mode == SNA ? job->min_burst_sna :
	  (sp->mode == SNB ? job->min_burst_snb :
	  (sp->mode == SWA ? job->min_burst_swa : job->min_burst_swb))) /
	  (sp->mode == SNA ? 2 : (sp->mode == SNB ? 3 : 4)))
       : (15.0 * sp->prf);

    /*-----------------------------------------------------------*
     |  Find the location of the earliest image in the datatake 
     *-----------------------------------------------------------*/
    if ((i = PPR_location(pp, sp, job, 0, nc, 0)) <= 0 ||
	PPR_location(pp, sp, job, 0, (i <= nc ? nc : i), 0) <= 0) {

	if (2*nc <= (sp->burst_len > 1 ? sp->burst_cnt : sp->pulse_cnt))
	    printfLLog(LOG_ERR, 
	    "%s %d/%.5d..%d/%.5d: Should there be >=1 frame??",
		(sp->mode==ES1 || sp->mode==ES2) ? "ERS" : ModeImage(sp->mode),
		BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
		BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job));
	return NULL;
    }
    new_pass = pp->pass;
    new_lat = pp->ll_center[0];
    pp->frame_id = FindFrame(sp->mode, job, &new_lat, &new_pass);

#ifdef	DEBUG
printf("First %d fmt %d, %.4d-%.3dT%.2d:%.2d:%09.6f, LAT %f/%c TO %f/%c\n",
 	pp->frame_id, pp->fmt_start,
        pp->gmt_center.yr, pp->gmt_center.day, pp->gmt_center.hr,
        pp->gmt_center.min, pp->gmt_center.second,
	pp->ll_center[0], pp->pass, new_lat, new_pass);
#endif

    if (!HI_LAT(pp->frame_id) || !get_hidelta(pp, sp, job)) {

#ifdef	DEBUG
printf("First ID %d fmt %d, %.4d-%.3dT%.2d:%.2d:%09.6f, LAT %f/%c TO %f/%c\n",
 	pp->frame_id, pp->fmt_start,
        pp->gmt_center.yr, pp->gmt_center.day, pp->gmt_center.hr,
        pp->gmt_center.min, pp->gmt_center.second,
	pp->ll_center[0], pp->pass, new_lat, new_pass);
#endif
	if (! (ok = goto_lat(pp, sp, job, new_lat, new_pass, 0, &new_gmt)) &&
	    2*nc <= (sp->burst_len > 1 ? sp->burst_cnt : sp->pulse_cnt))

	    printfLLog(LOG_ERR, 
	    "%s %d/%.5d..%d/%.5d: Should there be >=1 frame?",
		(sp->mode==ES1 || sp->mode==ES2) ? "ERS" : ModeImage(sp->mode),
		BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
		BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job));
	return ok;
    }
#ifdef	DEBUG
printf("First: HI %d fmt %d, %.4d-%.3dT%.2d:%.2d:%09.6f, LAT %f/%c TO %f/%c\n",
 	pp->frame_id, pp->fmt_start,
        pp->gmt_center.yr, pp->gmt_center.day, pp->gmt_center.hr,
        pp->gmt_center.min, pp->gmt_center.second,
	pp->ll_center[0], pp->pass, new_lat, new_pass);
#endif
    new_gmt = sp->gmt_hilat;
    i = ceil(gmt_diff(&pp->gmt_center, &sp->gmt_hilat) / sp->hidelta);
    gmt_add(&new_gmt, i*sp->hidelta);
    pp->frame_id = i + (pp->frame_id < HI_NORTH ? LO_NORTH : LO_SOUTH);

#ifdef DEBUG
printf("First Frame %d: %.4d-%.3dT%.2d:%.2d:%09.6f\n", i,
        new_gmt.yr, new_gmt.day, new_gmt.hr, new_gmt.min, new_gmt.second);
#endif

    if (! (ok = goto_gmt(pp, sp, job, &new_gmt)) &&
	2*nc <= (sp->burst_len > 1 ? sp->burst_cnt : sp->pulse_cnt))

	printfLLog(LOG_ERR, 
	"%s %d/%.5d..%d/%.5d: Shouldn't there be >=1 frame!",
	    (sp->mode==ES1 || sp->mode==ES2) ? "ERS" : ModeImage(sp->mode),
	    BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
	    BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job));
    return ok;
}

/*=============================================================================
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
PPR_t* PPR_next(register PPR_t* new, PPR_t* cur, const Job_t* job)
{
    register Segmt_t* sp = (Segmt_t*) cur->segmt;
    double new_lat = cur->ll_center[0];
    int new_pass = cur->pass;
    GMT new_gmt;
    *new = *cur;
    new->frame_id = NextFrame(sp->mode,job,cur->frame_id, &new_lat,&new_pass);

#ifdef	DEBUG
printf("Next: %.4d-%.3dT%.2d:%.2d:%09.6f, LAT %f/%c/%d TO %f/%c/%d\n",
        cur->gmt_center.yr, cur->gmt_center.day, cur->gmt_center.hr,
        cur->gmt_center.min, cur->gmt_center.second,
	cur->ll_center[0], cur->pass, cur->frame_id,
	new_lat, new_pass, new->frame_id);
#endif
    if (!HI_LAT(new->frame_id) || !get_hidelta(new, sp, job)) {
	return (goto_lat(new, sp, job, new_lat, new_pass, 0, &new_gmt) &&
		new->fmt_start != cur->fmt_start ? new : 0);
    }
    new_gmt = cur->gmt_center;
    gmt_add(&new_gmt, sp->hidelta);

    return (goto_gmt(new, sp, job, &new_gmt) &&
	    new->fmt_start != cur->fmt_start ? new : 0);
}
