/* SccsId[]= @(#)gmt2double.c	2.41 3/24/98 */
static char sccsid_gmt2double[]= "@(#)PPgmt2double.c:2.41";

/* SccsId[]= @(#)gmt2double.c	1.19 1/15/97 */
#include <time.h>
#include <stdio.h>
#include <varargs.h>

#define ERROR	-1
#define OK	0

int gmt2double(gmt, datetime, va_alist)
char *gmt;
double *datetime;
va_dcl
{
    va_list ap;
    char timebuf[64];
    int gmtlen, secs;
    double frac;
    struct tm t = {0};

    va_start(ap);
    gmtlen = va_arg(ap,int);
    if (gmtlen > sizeof(timebuf) || gmtlen < 17) {
	return ERROR;
    }

    memmove(timebuf, gmt, gmtlen);
    timebuf[gmtlen] = 0;

    if (gmtlen == 17) {
	if (sscanf(timebuf, "%d-%dT%d:%d:%d", &t.tm_year, &t.tm_mday, 
		   &t.tm_hour, &t.tm_min, &t.tm_sec) != 5)
	    return ERROR;
	frac = t.tm_sec;
    }
    else {
	if (sscanf(timebuf, "%d-%dT%d:%d:%lf", &t.tm_year, &t.tm_mday, 
		   &t.tm_hour, &t.tm_min, &frac) != 5)
	    return ERROR;
	t.tm_sec = frac;
    }

    if (t.tm_hour > 23 || t.tm_min  > 59  || t.tm_sec > 61 ||
	t.tm_mday < 1  || t.tm_mday > 366 || t.tm_year < 1900)
	return ERROR;

    t.tm_isdst = -1;
    t.tm_year -= 1900;
    secs = mktime(&t); 
/*    printf (" 2 year(-1900) %d %d %d\n",t.tm_year,t.tm_mon,t.tm_mday); */

    datetime[0] = (t.tm_year+1900)*10000 + (t.tm_mon+1)*100 + t.tm_mday; 
    datetime[1] = (t.tm_hour*10000) + (t.tm_min*100) + frac;
    return OK;
}
