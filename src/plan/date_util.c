#include "date.h"
#include "dateUtil.h"
#include "asf.h"

/*Extract date from the metadata-style given string:
instr="DD-MMM-YYYY, hh:mm:ss"
index  000000000011111111112
index  012345678901234567890
*/
void parse_date(const char *inStr,ymd_date *date,hms_time *time)
{
  char mon[][5]= 
    {"","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
  char buf[100];
  int i,sec;
#define subStr(start,len,dest) strncpy(buf,&inStr[start],len);buf[len]=0;sscanf(buf,"%d",dest);
  subStr(7,4,&date->year);
  for (i=0; i<13; i++) {
    strncpy(buf, &inStr[3], 3);
    buf[3] = 0;
    if (strcmp_case(uc(buf), mon[i]) == 0)
      date->month = i;
  }
  subStr(0,2,&date->day);
  subStr(13,2,&time->hour);
  subStr(16,2,&time->min);
  subStr(19,2,&sec);
  time->sec=sec;
#undef subStr
}

const char *date_str(double s)
{
  char mon[][5]= 
    {"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"};
  julian_date jd;
  hms_time t;
  ymd_date d;
  static char buf[64];

  sec2date(s, &jd, &t);
  date_jd2ymd(&jd, &d);

  //sprintf(buf, "%02d-%s-%4d, %02d:%02d:%02d", d.day, mon[d.month], d.year,
  //        t.hour, t.min, (int)(t.sec+.5));
  sprintf(buf, "%02d/%02d %02d:%02d", d.month, d.day, t.hour, t.min);
  return buf;
}

double seconds_from_s(const char *date_str)
{
  ymd_date d;
  hms_time t;
  parse_date(date_str, &d, &t);
  t.sec = 0;

  julian_date jd;
  date_ymd2jd(&d, &jd);

  return date2sec(&jd, &t);
}
         
double seconds_from_l(long date)
{
  ymd_date d;
  long_to_date(date, &d.year, &d.month, &d.day);

  julian_date jd;
  date_ymd2jd(&d, &jd);

  hms_time t;
  t.hour = 0;
  t.min = 0;
  t.sec = 0.001;

  return date2sec(&jd, &t);
}
