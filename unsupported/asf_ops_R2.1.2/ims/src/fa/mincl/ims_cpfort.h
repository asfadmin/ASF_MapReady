#ifndef IMS_CPFORT_H

#define IMS_CPFORT_H

static char *sccsCPFORT = "@(#)ims_cpfort.h	5.1  17 Mar 1996";

/* header file for the code formerly known as fortran
 * Corey Porter
 * 12/19/95
 */

/* a quick and useful define */
#define abs(a) (((a)<0)?(-(a)):(a))

/* some useful/necessary IMS includes */

#include <ims_query.h>
#include <ims_job_control.h>
#include <ims_fa_reports.h>


/*
 * The actual exported functions.  Some are in a different form (i.e., a
 * function) from the original fortran.  I suppose this could be a little
 * better.
 */

/*
 * The char *asftime mentioned below is in the following format:
 * YYYY:DDD:HH:MM:SS.SSS
 */

double asf2gha(char asftime[]);
double asf2jd(char asftime[]);
int ims_j1ll2grs(double xincl,long icyrevs,long icydays,double rsp_0_lon,
						 long n_rows, double grd_lat, double grd_lon, long *path,
	  				 long *row);
void sephem(double tj,double *x);
void ims_solarangs(
    char *asftime,
    double xlat,
    double xlon,
    long *horiz,
    long *iazmuth);
double solazd(double*,double*);

/* 
 * support functions from the original fortran.  These shouldn't be used,
 * but I'll catalog them here just for fun ;-)
 */

#if 0
void a_ephem(
	double isun,
	double imoon,
	double isrp,
	double t,
	double es[],
	double em[]);
void a_kepler(double am,double e,double *ea,double *se,double *ce);
void coord(double x[],double gm,double y[]);
void setsun(double t,double es[]);
long yddd2jd(long iyear,long iday);
#endif


/* 
 * Random useful math/vector functions
 */

double dmod(double a,double b);
double mymod(double a,int b);
double dot(double a[],double b[],int n);
void ucross(double *x,double *p1,double *p2);
void ucrosm(double*,double *,double *,double *);


/* 
 * some time conversion functions
 */

double asf2et(char[]);
double asf2jday(char[]);
int chasft(char[]);
double et2utc(double);
double etmtim(double,char[]);
int ims_j1rt2rsp(IMS_MSG_STRUCT*,
						 IMS_JOB_USER_SPEC*,
						 long,
						 char[],
						 char[],
						 long *,
						 double *);
double utc2et(double);

#endif
