/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* time_uts.c -- utility routines for manipulating GMT times */

#include <procfil.h>



/* get_gmt_diff(t1,t2) ---------------------------------------------

	This routine returns the difference, in seconds, between the
	GMT times t1 and t2. (t1 - t2)
*/

double get_gmt_diff(t1,t2)
	GMT *t1,*t2;
{
	double ms = 60.0;
	double hs = 60.0 * ms;
	double ds = 24.0 * hs;
	double ys = 365.0 * ds;
	double diff;

	diff = (t1->yr - t2->yr) * ys;
	diff += (t1->day - t2->day) * ds;
	diff += (t1->hr - t2->hr) * hs;
	diff += (t1->min - t2->min) * ms;
	diff += t1->second - t2->second;
    /* adjust for leap year(s) */
	diff += (((t1->yr - 1) / 4) - ((t2->yr - 1) / 4)) * ds;
	return (diff);
}


/* add_seconds(gmt,sec) --------------------------------------------
	This routine adds sec seconds to the given gmt record.
	This routine cannot handle large values for sec because
	the variable gmt->second is in floating point and starts
	to loose significant digits in the fractional part of the
	seconds when sec exceeds 10000.000.  Fix is to use double
	precision local variable during the computations then put it
	back together at the end.
*/

add_seconds(gmt,sec)
	GMT_PTR gmt;
	double sec;
{
	double dseconds;
	int days_in_year;

	dseconds = sec + (double) (gmt->second);
/*	printf("dseconds =%g\n",dseconds);
	printf ("gmt=%d:%d:%d:%d:%f\n",gmt->yr,gmt->day,gmt->hr,
	gmt->min, gmt->second);*/
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
	}  /* if */
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
	}  /* else */
	gmt->second = dseconds;
/*	printf ("gmt=%d:%d:%d:%d:%f\n",gmt->yr,gmt->day,gmt->hr,
	gmt->min, gmt->second);*/
}


/* get_month_day(year,yday,mo,day) -------------------------------------
	This routine uses year and day values to calculate the month 
	and day-of-month, and returns those values in mo and day.
*/

get_month_day(year,yday,mo,day)
	int year,yday;
	int *mo,*day;
{
	static int days[13] = { 0,31,28,31,30,31,30,31,31,30,31,30,31 };

    /* compensate for leap year (works OK until 2100) */
	days[2] = (year % 4) ? 28 : 29;
    /* get month and day of month */
	*day = yday;
	for (*mo = 1; *mo < 13 && *day > days[*mo]; (*mo)++)
	    *day -= days[*mo];
}


/* dump_gmt(t) ---------------------------------------------------------
	This routine prints the GMT pointed to by t.
*/

dump_gmt(t)
	GMT_PTR t;
{
	printf("%d:%d:%d:%d:%g\n",t->yr,t->day,t->hr,t->min,
		t->second);
}
/* days_in_year(mo,day,yday) -------------------------------------
*/

days_in_year(year,mo,day,yday)
	int *yday;
	int year,mo,day;
{
	static int days[13] = { 0,31,28,31,30,31,30,31,31,30,31,30,31 };
	register int i;

    /* compensate for leap year (works OK until 2100) */
	days[2] = (year % 4) ? 28 : 29;
	*yday = 0;
	for ( i = 1; i < mo; i++ ) *yday += days[mo];	
	*yday += day;
}
