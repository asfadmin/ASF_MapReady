/* SccsId[]= @(#)gmt_decode.c	2.41 3/24/98 */
#include <time.h>
#include <varargs.h>

#define ERROR	-1
#define OK	0

int gmt_decode_(gmt, year, doy, hour, min, sec, va_alist)
char *gmt;
int *year, *doy, *hour, *min;
double *sec;
va_dcl
{
  va_list ap;
  char timebuf[64];
  int gmtlen, secs;
  double frac;
  struct tm t = {0};

  printf("gmt_str %s\n", gmt);
  va_start(ap);
  gmtlen = va_arg(ap,int);
  if (gmtlen > sizeof(timebuf) || gmtlen < 17)
    return ERROR;

  memmove(timebuf, gmt, gmtlen);
  timebuf[gmtlen] = 0;

  if (gmtlen == 17) {
    if (sscanf(timebuf, "%d-%dT%d:%d:%d", &t.tm_year, &t.tm_mday, 
	       &t.tm_hour, &t.tm_min, &t.tm_sec) != 5)
      return ERROR;
/*
    printf("gmtlen = 17\n");
    printf("t.tm_year= %d\n", t.tm_year);
    printf("t.tm_yday= %d\n", t.tm_mday);
    printf("t.tm_hour= %d\n", t.tm_hour);
    printf("t.tm_min= %d\n", t.tm_min);
    printf("t.tm_sec= %d\n", t.tm_sec);
*/
    frac = t.tm_sec;
    }
  else {
    if (sscanf(timebuf, "%d-%dT%d:%d:%lf", &t.tm_year, &t.tm_mday, 
	       &t.tm_hour, &t.tm_min, &frac) != 5)
      return ERROR;
/*
    printf("gmtlen not equal 17\n");
    printf("t.tm_year %d\n", t.tm_year);
    printf("t.tm_yday %d\n", t.tm_mday);
    printf("t.tm_hour %d\n", t.tm_hour);
    printf("t.tm_min %d\n", t.tm_min);
    printf("frac %g\n", frac);
*/
    t.tm_sec = frac;
    }
  if (t.tm_hour > 23 || t.tm_min  > 59  || t.tm_sec > 61 ||
      t.tm_mday < 1  || t.tm_mday > 366 || t.tm_year < 1900)
    return ERROR;


/*
  t.tm_isdst = -1;
  t.tm_year -= 1900;
  secs = mktime(&t);
*/

  *year = t.tm_year;
  *doy = t.tm_mday;
  *hour = t.tm_hour;
  *min = t.tm_min;
  *sec = frac;
  
/*
  datetime[0] = (t.tm_year+1900)*10000 + (t.tm_mon+1)*100 + t.tm_mday;
  datetime[1] = (t.tm_hour*10000) + (t.tm_min*100) + frac;
*/

  return OK;
}
