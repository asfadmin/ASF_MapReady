/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*
 * Fortran-callable interface to the gettimeofday() system call
 * gettime_() returns the timeval, getzone_() returns timezone info.
 */

#include <sys/time.h>

void gettime_(sec, usec)
int *sec, *usec;
{
	struct timeval tv;

	gettimeofday(&tv, (char *)0L);
	*sec=tv.tv_sec;
	*usec=tv.tv_usec;
}

void getzone_(zone, dst)
int *zone, *dst;
{
	struct timezone tz;

	gettimeofday((char *)0L, &tz);
	*zone=tz.tz_minuteswest;
	*dst=tz.tz_dsttime;
}
