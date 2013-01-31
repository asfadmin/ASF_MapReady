
#include <stdio.h>
#include <stdlib.h>
#include "seasat.h"


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

