#ifdef IMS_MATH_H

#define IMS_MATH_H
static char *sccsMATH = "@(#)ims_math.h	5.1  17 Mar 1996";

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

void ims_solarangs(char *,double,double,long*,long*);
int  ims_j1ll2grs(double,long,long,double,long,double,double,long*,long*);
int ims_j1rt2rsp(IMS_MSG_STRUCT*,
						 IMS_JOB_USER_SPEC*,
						 long,
						 char[],
						 char[],
						 char[],
						 long *,
						 double *,
						 double *);
#endif
