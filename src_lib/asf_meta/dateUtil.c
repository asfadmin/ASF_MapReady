/*DateUtil:
	A set of utilities to convert dates between various
formats.
*/
#include "asf.h"
#include "dateUtil.h"

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



/*************************************************
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
void date_dssr2time(const char *inStr,hms_time *time)
{
	char buf[100];
	int sec,msec;
#define subStr(start,len,dest) strncpy(buf,&inStr[start],len);buf[len]=0;sscanf(buf,"%d",dest);
	subStr(12,2,&time->hour);
	subStr(15,2,&time->min);
	subStr(18,2,&sec);
	subStr(21,3,&msec);
	time->sec=sec+msec/1000.0;
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
