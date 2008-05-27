/*DateUtil:
    A set of utilities to convert dates between various
formats.
*/
#include "asf.h"
#include "dateUtil.h"
#include <time.h>
#include <assert.h>

/*Get number of days in Gregorian Year.*/
int date_getDaysInYear(int year)
{
    if (year<1536)
        {printf("ERROR! The gregorian calender was not used before year %d!\n",year);exit(1);}
    if (((year%4)==0)&&(((year%100)!=0)||((year%400)==0)))
        return 366;
    else
        return 365;
}

/*Get month/day given year, julian day.*/
static int monthStart_leap[13]=
    {1,32,61,92,122,153,183,214,245,275,306,336,367};
static int monthStart_nonleap[13]=
    {1,32,60,91,121,152,182,213,244,274,305,335,366};
void date_jd2ymd(julian_date *in,ymd_date *out)
{
    int *monthStart;
    int currMonth;
    int daysInYear=date_getDaysInYear(in->year);
    if (in->jd>daysInYear)
        {printf("ERROR! Invalid julian day '%d' passed to date_jd2ymd!\n",in->jd);exit(1);}
    if (daysInYear==366)
        monthStart=monthStart_leap;
    else
        monthStart=monthStart_nonleap;
    for (currMonth=1;monthStart[currMonth]<=in->jd;currMonth++) {}
    out->year=in->year;
    out->month=currMonth;
    out->day=in->jd-monthStart[currMonth-1]+1;
}
/*Get julian day given month/day.*/
void date_ymd2jd(ymd_date *in,julian_date *out)
{
    int *monthStart;
    int daysInYear=date_getDaysInYear(in->year);
    if ((in->month>12)||(in->day>31))
        {printf("ERROR! Invalid month '%d' or day '%d' passed to date_ymd2jd!\n",in->month,in->day);exit(1);}
    if (daysInYear==366)
        monthStart=monthStart_leap;
    else
        monthStart=monthStart_nonleap;
    out->year=in->year;
    out->jd=monthStart[in->month-1]+(in->day-1);
}

/*Compute the number of seconds since midnight, Jan 1, 1900.
This is sub-millisecond accurate; but ignores leap seconds.
*/
double date2sec(julian_date *date,hms_time *time)
{
    double ret=date_hms2sec(time);
    int year;
    for (year=1900;year<date->year;year++)
        ret+=date_getDaysInYear(year)*DAY2SEC;
    ret+=(date->jd-1)*DAY2SEC;
    return ret;
}

/*Compute the date corresponding to this number of seconds
since midnight, Jan 1, 1900.
This is sub-millisecond accurate; but ignores leap seconds.
*/
void sec2date(double secs,julian_date *date,hms_time *time)
{
    int year=1900;
    while (secs>=date_getDaysInYear(year)*DAY2SEC)
        secs-=date_getDaysInYear(year++)*DAY2SEC;
    date->year=year;
    date->jd=1+(int)(secs/DAY2SEC);
    secs-=(date->jd-1)*DAY2SEC;
    date_sec2hms(secs,time);
}


/*Get Julian Day (since Jan. 1, 4712 BC) given Gregorian Year, Month, and day.
Uses an interesting algorithm from the NASDA Downlink Reference document,
which easily and implicitly handles all the oddities of Gregorian dates
(leap year rules, variable number of days per month, etc.)*/

double date_getJD(ymd_date *in)
{
    double ret;
    int month=in->month,year=in->year;
    if (month==1)
    {
        month=13;
        year--;
    } else if (month==2)
    {
        month=14.0;
        year--;
    }
    ret=1721088.5+in->day+floor(365.25*year)+
        floor(year/400.0)-floor(year/100.0)+
        floor(30.59*(month-2.0));
    return ret;
}

/*Get ESA-Modified Julian Day given conventional year and julian day.
This is defined as "Days after Jan. 1, 1950".*/
int date_getMJD(julian_date *in)
{
    int curYear=1950;
    int curDay=0;
    while (curYear<in->year)
        curDay+=date_getDaysInYear(curYear++);
    return curDay+in->jd-1;
}

/*Convert hour/minute/second to seconds in day.*/
double date_hms2sec(hms_time *in)
{
    return (in->hour*60.0+in->min)*60+in->sec;
}

/*Convert seconds in day to hour/minute/second.*/
void date_sec2hms(double sec,hms_time *out)
{
    out->hour=(int)(sec/(60*60));
    sec-=(out->hour)*60*60;
    out->min=(int)(sec/(60));
    sec-=(out->min)*60;
    out->sec=sec;
}


/* Convert a year, month, day into a gmt_day (ie day of the year)
 ---------------------------------------------------------------*/
int date_ymd2gmt(ymd_date *date)
 {
   static int DIM[13]={0,31,28,31,30,31,30,31,31,30,31,30,31}; /*Days in Month*/
   int gmt_day;
   int i = 0;

   /* Check for Leap Year */
   if (((date->year%4)==0)&&(((date->year%100)!=0)||((date->year%400)==0))) DIM[2]=29;
   else
     DIM[2] = 28;

   gmt_day = date->day;
   while (i<date->month) { gmt_day += DIM[i++]; }

   return(gmt_day);
 }


/*************************************************************
   Date/Time Input Parsing Routines
 ************************************************************/

/* Date Utility Routine: Convert ODL style date to structure.
   Input format is YYYY-DDDTHH:MM:SS.TTT */
void parse_odlTime(const char *str, ymd_date *date, hms_time *time)
{
        char tmpBuf[6];
        int  cnt=0;
        julian_date jul;

        tmpBuf[0] = str[cnt++]; tmpBuf[1] = str[cnt++];
        tmpBuf[2] = str[cnt++]; tmpBuf[3] = str[cnt++];
        tmpBuf[4] = '\0'; jul.year = atoi(tmpBuf);
        cnt++;

        tmpBuf[0] = str[cnt++]; tmpBuf[1] = str[cnt++];
        tmpBuf[2] = str[cnt++]; tmpBuf[3] = '\0'; jul.jd = atoi(tmpBuf);
        cnt++;

        date_jd2ymd(&jul,date);

        tmpBuf[0] = str[cnt++]; tmpBuf[1] = str[cnt++];
        tmpBuf[2] = '\0'; time->hour = atoi(tmpBuf);
        cnt++;

        tmpBuf[0] = str[cnt++]; tmpBuf[1] = str[cnt++];
        tmpBuf[2] = '\0'; time->min = atoi(tmpBuf);
        cnt++;

        tmpBuf[0] = str[cnt++]; tmpBuf[1] = str[cnt++];
        cnt++;
        tmpBuf[2] = str[cnt++]; tmpBuf[3] = str[cnt++];
        tmpBuf[4] = str[cnt++];
        tmpBuf[5] = '\0'; time->sec = atof(tmpBuf)/1000.0;
}


/*Date Utility routine: Convert LZ style date to structure.
  Input format is YYYYMMDDHHMMSSTTT */
void parse_ymdTime(const char *inStr,ymd_date *date,hms_time *time)
{
        char tmpBuf[6];
        int  cnt=0;

        tmpBuf[0] = inStr[cnt++]; tmpBuf[1] = inStr[cnt++];
        tmpBuf[2] = inStr[cnt++]; tmpBuf[3] = inStr[cnt++];
        tmpBuf[4] = '\0'; date->year = atoi(tmpBuf);

        tmpBuf[0] = inStr[cnt++]; tmpBuf[1] = inStr[cnt++];
        tmpBuf[2] = '\0'; date->month = atoi(tmpBuf);

        tmpBuf[0] = inStr[cnt++]; tmpBuf[1] = inStr[cnt++];
        tmpBuf[2] = '\0'; date->day = atoi(tmpBuf);

        tmpBuf[0] = inStr[cnt++]; tmpBuf[1] = inStr[cnt++];
        tmpBuf[2] = '\0'; time->hour = atoi(tmpBuf);

        tmpBuf[0] = inStr[cnt++]; tmpBuf[1] = inStr[cnt++];
        tmpBuf[2] = '\0'; time->min = atoi(tmpBuf);

        tmpBuf[0] = inStr[cnt++]; tmpBuf[1] = inStr[cnt++];
        tmpBuf[2] = inStr[cnt++]; tmpBuf[3] = inStr[cnt++];
        tmpBuf[4] = inStr[cnt++];
        tmpBuf[5] = '\0'; time->sec = atof(tmpBuf)/1000.0;
}


/* Parse a reference time string of the form YYYYDDDHHMMSS[TTT]
 -------------------------------------------------------------*/
void parse_refTime(const char *refTime, julian_date *julDay, hms_time *time)
 {
    char tmpStr[16];
    int  cnt = 0;

    tmpStr[0] = refTime[cnt++]; tmpStr[1] = refTime[cnt++];
    tmpStr[2] = refTime[cnt++]; tmpStr[3] = refTime[cnt++];
    tmpStr[4] = '\n';
    julDay->year = atoi(tmpStr);
    tmpStr[0] = refTime[cnt++]; tmpStr[1] = refTime[cnt++];
    tmpStr[2] = refTime[cnt++]; tmpStr[3] = '\n';
    julDay->jd = atoi(tmpStr);
    tmpStr[0] = refTime[cnt++]; tmpStr[1] = refTime[cnt++]; tmpStr[2] = '\n';
    time->hour = atoi(tmpStr);
    tmpStr[0] = refTime[cnt++]; tmpStr[1] = refTime[cnt++]; tmpStr[2] = '\n';
    time->min = atoi(tmpStr);
    tmpStr[0] = refTime[cnt++]; tmpStr[1] = refTime[cnt++]; tmpStr[2] = '\n';
    time->sec = atof(tmpStr);
    if (strlen(refTime)>cnt)
     {
        tmpStr[0] = refTime[cnt++]; tmpStr[1] = refTime[cnt++];
        tmpStr[2] = refTime[cnt++]; tmpStr[3] = '\n';
        time->sec = (atof(tmpStr)/1000.0);
     }

 }

/*Extract date from the metadata-style given string:
instr="DD-MMM-YYYY, hh:mm:ss"
index  000000000011111111112
index  012345678901234567890
*/
void parse_DMYdate(const char *inStr,ymd_date *date,hms_time *time)
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

/************************************************************
   Date/Time Mathematical Routines
 ***********************************************************/

/* Calculate seconds elapsed from given refYear to the
    seekYear, seekDay (julian day), and seekTime
 ----------------------------------------------------*/
double timeSince(ymd_date *seekDate, hms_time *seekTime, int refYear)
 {
  double totTime;
  int    totDays=0,
         year;
  julian_date julDay;

  date_ymd2jd(seekDate,&julDay);
  for (year = refYear; year < julDay.year; year++) totDays += date_getDaysInYear(year);
  totDays += julDay.jd;
  totTime = totDays * 24.0 * 3600.0 + date_hms2sec(seekTime);

  return(totTime);
 }

/* Calculate absolute seconds difference from date1 to date2
 ----------------------------------------------------------*/
double date_difference( ymd_date *date1, hms_time *time1,
                        ymd_date *date2, hms_time *time2)
 {
   double t1, t2;
   t1 = timeSince(date1,time1,1990);
   t2 = timeSince(date2,time2,1990);
   return(fabs(t1-t2));
 }

/*------------------------------
  Compare time1 to time2;
     return -1 if time1 < time2,
     return  1 if time1 > time2
     return  0 if time1 = time2
 ------------------------------*/
int compare_time(ymd_date *date1, hms_time *time1,
                 ymd_date *date2, hms_time *time2)
 {

   if (date1->year < date2->year) return(-1);
   else if (date1->year > date2->year) return(1);

   if (date1->month < date2->month) return(-1);
   else if (date1->month > date2->month) return(1);

   if (date1->day < date2->day) return(-1);
   else if (date1->day > date2->day) return(1);

   if (time1->hour < time2->hour) return(-1);
   else if (time1->hour > time2->hour) return(1);

   if (time1->min < time2->min) return(-1);
   else if (time1->min > time2->min) return(1);

   if (time1->sec < time2->sec) return(-1);
   else if (time1->sec > time2->sec) return(1);

   return(0);
 }


/* Add delta seconds onto the the given date,time
 -----------------------------------------------*/
void add_time(double delta, ymd_date *date, hms_time *time)
 {
   static int DIM[13]={0,31,28,31,30,31,30,31,31,30,31,30,31};/*Days in Month*/
   long tyear, tmonth, tday, thour, tmin;
   double tsec;
   long ovrflw = 0;

   /* Check for Leap Year */
   if (((date->year%4)==0)&&(((date->year%100)!=0)||((date->year%400)==0))) DIM[2]=29;

   tyear  = date->year;
   tmonth = date->month;
   tday   = date->day;
   thour  = time->hour;
   tmin   = time->min;
   tsec   = time->sec;

   tsec  += delta;

   while (tsec >= 60.0) { ovrflw+=1; tsec -=60.0; }
   if (ovrflw) {
     tmin += ovrflw; ovrflw = 0; while (tmin >= 60.0) { ovrflw+=1; tmin -=60.0; }
     if (ovrflw) {
       thour += ovrflw; ovrflw = 0; while (thour >= 24.0) { ovrflw += 1; thour -=24.0; }
       if (ovrflw) {
         tday += ovrflw; ovrflw = 0;
         while (tday > DIM[tmonth])
          {
            tday-=DIM[tmonth];
            tmonth++;
            if (tmonth > 12)
              {
                 tmonth -= 12;
                 tyear +=1;

                 /* Check for Leap Year */
                 if (((tyear%4)==0)&&(((tyear%100)!=0)||((tyear%400)==0))) DIM[2]=29;
                 else DIM[2]=28;
              }
          }
        }
      }
    }

   date->year = tyear;
   date->month = tmonth;
   date->day  = tday;
   time->hour = thour;
   time->min  = tmin;
   time->sec  = tsec;
 }


/* Subtract delta seconds from the the given date,time
 ----------------------------------------------------*/
void sub_time(double delta, ymd_date *date, hms_time *time)
 {
   static int DIM[13]={0,31,28,31,30,31,30,31,31,30,31,30,31};/*Days in Month*/
   long tyear, tmonth, tday, thour, tmin;
   double tsec;
   long undrflw = 0;

   /* Check for Leap Year */
   if (((date->year%4)==0)&&(((date->year%100)!=0)||((date->year%400)==0))) DIM[2]=29;

   tyear  = (long) date->year; tmonth = (long) date->month; tday   = (long) date->day;
   thour  = (long) time->hour; tmin   = (long) time->min; tsec   = time->sec;

   tsec  -= delta;
   while (tsec < 0.0) {undrflw-=1; tsec +=60.0; }
   if (undrflw<0) {
     tmin += undrflw; undrflw = 0; while (tmin < 0) {undrflw-=1; tmin +=60.0; }
     if (undrflw<0) {
       thour += undrflw; undrflw = 0; while (thour < 0) { undrflw -= 1; thour +=24.0; }
       if (undrflw<0) {
         tday += undrflw; undrflw = 0;
         while (tday < 0)
           {
              tmonth--;
              if (tmonth==0)
                 {
                   tmonth=12; tyear-=1;
                   if (((tyear%4)==0)&&(((tyear%100)!=0)||((tyear%400)==0))) DIM[2]=29;
                   else DIM[2]=28;
                 }
              tday+=DIM[tmonth];
           }
        }
      }
    }

   date->year = (int) tyear;
   date->month = (int) tmonth;
   date->day  = (int) tday;
   time->hour = (int) thour;
   time->min  = (int) tmin;
   time->sec  = tsec;
}

/*
   Compute the average ymd_date date and hms_time time
  -----------------------------------------------------*/
void average_ymdTimes(ymd_date *date1, ymd_date *date2,
                      hms_time *time1, hms_time *time2,
                      ymd_date *ave_date, hms_time *ave_time)
{
    double secs1, secs2, ave_secs;
    julian_date jd_1, jd_2, ave_jd;

    date_ymd2jd(date1, &jd_1); // Julian date contains year and day number within that year
    date_ymd2jd(date2, &jd_2);

    secs1 = date2sec(&jd_1, time1); // Seconds from midnight, Jan 1, 1900 to julian date plus seconds into that day
    secs2 = date2sec(&jd_2, time2);
    ave_secs = (secs1 + secs2) / 2.0;

    sec2date(ave_secs, &ave_jd, ave_time);
    date_jd2ymd(&ave_jd, ave_date);
}


/************Img_SceneCenterDateTime*************************************
Date I/O Utilites:
    These write characters into a raw buffer.
They don't even append a '/0' to the end of their
inputs, because they are intended for use with fixed-
size ASCII structures.
    They return a pointer to just past the end
of the string they wrote.
*/

/*Extract a DSSR-style date from the given string:
instr="YYYYMMDDhhmmssttt"
index  01234567890123456
*/
void date_dssr2date(const char *inStr,ymd_date *date,hms_time *time)
{
    char buf[100];
    int sec,msec;
#define subStr(start,len,dest) strncpy(buf,&inStr[start],len);buf[len]=0;sscanf(buf,"%d",dest);
    subStr(0,4,&date->year);
    subStr(4,2,&date->month);
    subStr(6,2,&date->day);
    subStr(8,2,&time->hour);
    subStr(10,2,&time->min);
    subStr(12,2,&sec);
    subStr(14,3,&msec);
    time->sec=sec+msec/1000.0;
}

/*Extract second DSSR-style date from the given string:
instr="DD-MMM-YYYY hh:mm:ss.ttt"
index  012345678901234567890123
*/
void date_dssr2time(const char *inStr,ymd_date *date,hms_time *time)
{
  char mon[][5]=
    {"","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
    char buf[100];
    int i,sec,msec;
#define subStr(start,len,dest) strncpy(buf,&inStr[start],len);buf[len]=0;sscanf(buf,"%d",dest);
        subStr(7,4,&date->year);
        for (i=0; i<13; i++) {
      strncpy(buf, &inStr[3], 3);
      buf[3] = 0;
      if (strcmp(uc(buf), mon[i]) == 0)
        date->month = i;
    }
        subStr(0,2,&date->day);
    subStr(12,2,&time->hour);
    subStr(15,2,&time->min);
    subStr(18,2,&sec);
    subStr(21,3,&msec);
    time->sec=sec+msec/1000.0;
}

// Extract ALOS summary style date from the given string:
// instr="YYYYMMDD hh:mm:ss.ttt"
// index  012345678901234567890
void date_alos2date(const char *inStr,ymd_date *date,hms_time *time)
{
    char buf[100];
    int sec,msec;
#define subStr(start,len,dest) strncpy(buf,&inStr[start],len);buf[len]=0;sscanf(buf,"%d",dest);
    subStr(0,4,&date->year);
    subStr(4,2,&date->month);
    subStr(6,2,&date->day);
    subStr(9,2,&time->hour);
    subStr(12,2,&time->min);
    subStr(15,2,&sec);
    subStr(18,3,&msec);
    time->sec=sec+msec/1000.0;
}
// Extract SIR-C summary style date from the given string:
// instr="YYYY/MM/DD hh:mm:ss.ttt"
// index  01234567890123456789012
void date_sirc2date(const char *inStr,ymd_date *date,hms_time *time)
{
    char buf[100];
    int sec,msec;
#define subStr(start,len,dest) strncpy(buf,&inStr[start],len);buf[len]=0;sscanf(buf,"%d",dest);
    subStr(0,4,&date->year);
    subStr(5,2,&date->month);
    subStr(8,2,&date->day);
    subStr(11,2,&time->hour);
    subStr(14,2,&time->min);
    subStr(17,2,&sec);
    subStr(20,3,&msec);
    time->sec=sec+msec/1000.0;
}


/* Convert DSSR style date to time stamp
instr="YYYYMMDDhhmmssttt"
index  01234567890123456
*/
void date_dssr2time_stamp(ymd_date *date,hms_time *time, char *t_stamp)
{
  struct tm t;

  t.tm_year = date->year - 1900;
  t.tm_mon = date->month - 1;
  t.tm_mday = date->day;
  t.tm_sec = time->sec;
  t.tm_min = time->min;
  t.tm_hour = time->hour;
  t.tm_isdst = -1;
  strftime(t_stamp, 22, "%d-%b-%Y, %H:%M:%S", &t);
}

void date_shr2date_stamp(const char *inStr, char *d_stamp)
{
  char buf[100], tmp[10];
#define subStr2(start,len,dest) strncpy(buf,&inStr[start],len);buf[len]=0;sscanf(buf,"%s",dest);
  subStr2(0,2,d_stamp);
  strcat(d_stamp,"-");
  subStr2(2,3,tmp);
  strcat(d_stamp,tmp);
  strcat(d_stamp,"-");
  subStr2(5,2,tmp);
  strcat(d_stamp,tmp);
}

/*
WriteInt: Internal call.
    Write the N low-order decimal digits of the given
integer into the given field. Returns pointer to just past
end of written data;
*/
char *date_writeInt(int number,int N,char *dest)
{
    int digitNo;
    for (digitNo=N-1;digitNo>=0;digitNo--)
    {
        dest[digitNo]=(number%10)+'0';
        number/=10;
    }
    return &(dest[N]);
}

/*Writes YY&MM&DD, where sep==&*/
char * date_printY2Kdate(ymd_date *in,char sep,char *dest)
{
    dest=date_writeInt(in->year,2,dest);
    if (sep) *dest++=sep;
    dest=date_writeInt(in->month,2,dest);
    if (sep) *dest++=sep;
    dest=date_writeInt(in->day,2,dest);
    *dest++ = '\0';
    return dest;
}

/*Writes YYYY&MM&DD, where sep==&*/
char * date_printDate(ymd_date *in,char sep,char *dest)
{
    dest=date_writeInt(in->year,4,dest);
    if (sep) *dest++=sep;
    dest=date_writeInt(in->month,2,dest);
    if (sep) *dest++=sep;
    dest=date_writeInt(in->day,2,dest);
    *dest++ = '\0';
    return dest;
}

/*Writes HH&MM&SS.CCCC,
where sep==& (no spaces if sep=='\0')
where there are prec "C"'s. (no decimal if prec==0).*/
char * date_printTime(hms_time *in,int prec,char sep,char *dest)
{
    dest=date_writeInt(in->hour,2,dest);
    if (sep) *dest++=sep;
    dest=date_writeInt(in->min,2,dest);
    if (sep) *dest++=sep;
    dest=date_writeInt((int)in->sec,2,dest);
    if (prec)
    {
        double frac=(in->sec-(int)in->sec);
        int i;
        *dest++='.';
        for (i=0;i<prec;i++) frac*=10.0;
        dest=date_writeInt((int)frac,prec,dest);
    }
    *dest++ = '\0';
    return dest;
}


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
  julian_date jd;
  hms_time t;
  ymd_date d;
  static char buf[64];

  sec2date(s, &jd, &t);
  date_jd2ymd(&jd, &d);

  sprintf(buf, "%02d/%02d %02d:%02d", d.month, d.day, t.hour, t.min);
  return buf;
}

const char *date_str_long(double s)
{
  char mon[][5]= 
    {"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"};
  julian_date jd;
  hms_time t;
  ymd_date d;
  static char buf[64];

  sec2date(s, &jd, &t);
  date_jd2ymd(&jd, &d);

  sprintf(buf, "%02d-%s-%4d, %02d:%02d:%02d", d.day, mon[d.month], d.year,
          t.hour, t.min, (int)(t.sec+.5));
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

int is_leap_year(int year)
{
    return (!(year % 4) && (year % 100)) || !(year % 400);
}

int is_valid_date(long l)
{
    char year[32], month[32], day[32];
    sprintf(year, "%ld", l);
    if (strlen(year) != 8)
        return FALSE;

    strcpy(month, year+4);
    strcpy(day, month+2);
    year[4] = '\0';
    month[2] = '\0';

    int y = atoi(year);
    int m = atoi(month);
    int d = atoi(day);

    if (y >= 1900 && y <= 9999 &&  // year between 1900 and 9999
        m >= 1    && m <= 12   &&  // month between 1 and 12
        d >= 1    && d <= 31)      // day between 1 and 31
    {
        // now check month/day combo
        if (m==2 && d>29)
            return FALSE;

        if (m==2 && d==29 && !is_leap_year(y))
            return FALSE;

        if (d==31 && (m==4 || m==6 || m==9 || m==11))
            return FALSE;

        // all tests passed
        return TRUE;
    }

    // failed
    return FALSE;
}

long current_date()
{
    time_t t = time(NULL);
    struct tm *ts = localtime(&t);
    return date_to_long(ts->tm_year+1900, ts->tm_mon+1, ts->tm_mday);
}

long add_days(long l, int d)
{
    // slow implementation for now
    long ret = l;
    while (d-->0)
        ret = add_a_day(ret);
    return ret;
}

void long_to_date(long l, int *y, int *m, int *d)
{
    char year[32], month[32], day[32];
    sprintf(year, "%ld", l);
    if (strlen(year) != 8) {
        *y = *m = *d = -1;
        return;
    }

    strcpy(month, year+4);
    strcpy(day, month+2);
    year[4] = '\0';
    month[2] = '\0';

    *y = atoi(year);
    *m = atoi(month);
    *d = atoi(day);
}

long date_to_long(int y, int m, int d)
{
    char tmp[32];
    sprintf(tmp, "%04d%02d%02d", y, m, d);
    long l = atol(tmp);
    assert(is_valid_date(l));
    return l;
}

int get_day_of_week(long l)
{
    if (is_valid_date(l)) {
        int y,m,d;
        long_to_date(l, &y, &m, &d);

        if (m < 3) {
            m += 12;
            y -= 1;
        }
        return (d+2*m+(int)(6*(m+1)/10)+y+(int)(y/4)-(int)(y/100)+(int)(y/400)+1
) % 7;
    }
    printf("Invalid date: %ld\n", l);
    assert(0);
    return -1;
}

long subtract_a_day(long l)
{
    int y, m, d;
    long_to_date(l, &y, &m, &d);

    --d;

    // check if went back to previous month
    if (d < 1) {
        switch (m) {
            case 1:  // have to go to previous year
                m=12;
                d=31;
                --y;
                break;

            case 2:  // previous month has 31 days
            case 4:
            case 6:
            case 8:
            case 9:
            case 11:
                --m;
                d=31;
                break;

            case 5: // previous month has 30 days
            case 7:
            case 10:
            case 12:
                --m;
                d=30;
                break;

            case 3:  // previous month is Feb, yargh
                m=2;
                d = is_leap_year(y) ? 29 : 28;
                break;

            default:
                assert(0);
                break;
        }
    }

    long l2 = date_to_long(y,m,d);
    assert(is_valid_date(l2));
    return l2;
}

long add_a_day(long l)
{
    int y, m, d;
    long_to_date(l, &y, &m, &d);

    ++d;

    // check if went to next month
    if (d > 28) {
        switch (m) {
            case 12:  // have to go to next year?
                if (d==32) {
                    m=d=1;
                    ++y;
                }
                break;

            case 2:  // special code for Feb
                if (( is_leap_year(y) && d==30) ||
                    (!is_leap_year(y) && d==29)) {
                    m=3; d=1;
                }
                break;

            case 1: // month has 31 days
            case 3:
            case 5:
            case 7:
            case 8:
            case 10:
                if (d==32) {
                    ++m;
                    d=1;
                }
                break;

            case 4:  // month has 30 days
            case 6:
            case 9:
            case 11:
                if (d==31) {
                    ++m;
                    d=1;
                }
                break;

            default:
                assert(0);
                break;
        }
    }

    long l2 = date_to_long(y,m,d);
    assert(is_valid_date(l2));
    return l2;
}

int date_diff(long date1, long date2)
{
  long startdate = date1;
  long enddate = date2;
  if (date1 > date2) {
    startdate = date2;
    enddate = date1;
  }
  int ndays = 0;
  while (startdate < enddate) {
    startdate = add_a_day(startdate);
    ++ndays;
  }

  return ndays;
}

void date_tester()
{
    int i;
    long l;

    // normal tests
    assert(is_valid_date(20070101));
    assert(is_valid_date(20080202));
    assert(is_valid_date(20090303));
    assert(is_valid_date(20100404));
    assert(is_valid_date(20110505));
    assert(is_valid_date(20121212));
    assert(!is_valid_date(20071301));
    assert(!is_valid_date(20071232));
    assert(!is_valid_date(20081232));
    assert(!is_valid_date(20110030));
    assert(!is_valid_date(20070631));
    assert(!is_valid_date(20071131));
    assert(!is_valid_date(20070431));
    assert(!is_valid_date(20290631));
    assert(!is_valid_date(99990931));
    assert(!is_valid_date(59990931));
    assert(is_valid_date(20080229));
    assert(!is_valid_date(20090229));
    assert(!is_valid_date(20100229));
    assert(!is_valid_date(20110229));
    assert(is_valid_date(20120229));
    assert(!is_valid_date(23000229));
    assert(is_valid_date(24000229));

    assert(get_day_of_week(20070611)==1); // today, 6/11/07, is Monday
    assert(get_day_of_week(20070612)==2); // tomorrow is Tuesday
    assert(get_day_of_week(20070610)==0); // yesterday was Sunday
    assert(get_day_of_week(20070613)==3); // 13th is Wednesday
    assert(get_day_of_week(20080101)==2); // new years 2008 is Tuesday
    assert(get_day_of_week(20080229)==5); // leap day 2008 is Friday
    assert(get_day_of_week(20080125)==5); // my birthday next year is Friday

    assert(add_a_day(20071231)==20080101);
    assert(subtract_a_day(20080101)==20071231);

    for (l=20070101; l<23991230; l=add_a_day(l))
        assert(subtract_a_day(add_a_day(l))==l);
    for (l=99070101; l<99991230; l=add_a_day(l))
        assert(add_a_day(subtract_a_day(l))==l);

    l = 20070101;
    for (i=0; i<366*2000; ++i) {
        int dow1 = get_day_of_week(l);
        l = add_a_day(l);
        assert(is_valid_date(l));
        int dow2 = get_day_of_week(l);
        assert((dow1+1)%7 == dow2%7);
        int y,m,d;
        long_to_date(l,&y,&m,&d);
        assert(l==date_to_long(y,m,d));
    }
    for (i=0; i<366*2000; ++i) {
        int dow1 = get_day_of_week(l);
        l = subtract_a_day(l);
        int dow2 = get_day_of_week(l);
        assert((dow2+1)%7 == dow1%7);
    }

    int y2, m2, d2;
    long_to_date(l, &y2, &m2, &d2);
    assert(y2==2007);
    assert(m2==1);
    assert(d2==1);
}

