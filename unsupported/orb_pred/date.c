/****************************************************************
FUNCTION NAME: format_date

SYNTAX:  format_date(in_day,in_year,out_day,out_year)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    in_day	double		calculated julian day
    in_year	int		year corresponding to in_day
    out_day	double *	formatted day
    out_year	int *		corresponding year

DESCRIPTION:
   format_date() takes the calculated date and returns 
   the year & the julian day.

RETURN VALUE:
   None.

SPECIAL CONSIDERATIONS:
   A calc. JD of 0 corresponds to Dec. 35 of the previous year. 

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"

/*  check for leap year. return 1 if yes, 0 if no. */
int leap_year(year)
int year;
{
  if (year%400 == 0)
  	return 1;
  else if ( (year%4 == 0) && (year%100 != 0) )
	return 1;
  else
    return 0;
}

void format_date(in_day,in_year,out_day,out_year)
double in_day;
int in_year;
double *out_day;
int *out_year;
{
  static double days[] = { 365, 366};

  if (in_day >= 0.0 && in_day < 1.0) {
    in_day += days[leap_year(--in_year)];
    format_date(in_day,in_year,out_day,out_year);
  }
  else if (in_day < 1.0) {
    in_day = days[leap_year(--in_year)] - fabs(in_day);
    format_date(in_day,in_year,out_day,out_year);
  } else if (in_day >= days[leap_year(in_year)]+1) {
    in_day -= days[leap_year(in_year++)];
    format_date(in_day,in_year,out_day,out_year);
  } else {
    *out_day = in_day;
    *out_year = in_year;
  }

  return;
}
