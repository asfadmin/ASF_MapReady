/*DateUtil:
	A set of utilities to convert dates between various
formats.
*/
#include "asf.h"
#include "dateUtil.h"

/*Get number of days in Gregorian Year.*/
int date_getDaysInYear(int year)
{
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
	{
		printf("ERROR! Invalid julian day '%d' passed to jd2monthDay!\n",in->jd);
		exit(1);
	}
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
	{
		printf("ERROR! Invalid month '%d' or day '%d' passed to monthDay2jd!\n",in->month,in->day);
		exit(1);
	}
	if (daysInYear==366)
		monthStart=monthStart_leap;
	else
		monthStart=monthStart_nonleap;
	out->year=in->year;
	out->jd=monthStart[in->month-1]+(in->day-1);
}

/*Get Julian Day (since 10,000 BC) given Gregorian Year, Month, and day.
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

/*Convert hour/minute/second to milliseconds in day.*/
double date_hms2msec(hms_time *in)
{
	return ((in->hour*60.0+in->min)*60+in->sec)*1000.0;
}

/*Convert milliseconds in day to hour/minute/second.*/
void date_msec2hms(double msec,hms_time *out)
{
	out->hour=(int)(msec/(1000.0*60*60));
	msec-=(out->hour)*(1000.0*60*60);
	out->min=(int)(msec/(1000.0*60));
	msec-=(out->min)*(1000.0*60);
	out->sec=msec/1000.0;
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
	return dest;
}
