/***************************************************************************
   This function converts Julian seconds to year/month/day/hour/minute/second 
   format, i.e., yyyymmddhhmmss.  The function works only for days after 
   January 1, 1900, though it is not difficult to modify the code to work 
   for days before January 1, 1900. January 1, 1900 corresponds to
   the 2415020.5th Julian day which is the 2.086577712e+11th Julian 
   second.

   Input: Julian_seconds
   Output: yyyymmddhhmmss

   Warning: String buffer length has to be at least 15 characters long.

   Author:  Shelby Yang.   7/15/97 at JPL
   *************************************************************************/

#include <stdio.h>
#include <math.h>

static int days_in_year[2][1] = {365,366};
static int day_table[2][13] = {
  {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
  {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
};

int seconds_to_time(double julian_seconds, char* yyyymmddhhmmss)
{
  int leap, year, month, day, hour, minute, second;
  int i;
  double julian_days, julian_day_1900_1_1;

  /* julian day of January, 1, 1900 */
  julian_day_1900_1_1 = 2415020.5;
  julian_days = julian_seconds/86400.0 - julian_day_1900_1_1;

  year = 1900;
  for(;;) {
    leap = 0;
    if(year%4 == 0 && year%100 != 0 || year%400 == 0) {
      leap = 1;
    }
    if((int)ceil(julian_days) > days_in_year[leap][0]) {
      julian_days -= (double) days_in_year[leap][0];
      year++;
    } else {
      day = (int) ceil(julian_days);
      month = 1;
      for(i = 1; day > day_table[leap][i]; i++) {
	day -= day_table[leap][i];
	month++;
      }

      julian_days -= floor(julian_days);
      julian_days = julian_days*24.0;
      hour = (int) floor(julian_days);

      julian_days -= floor(julian_days);
      julian_days = julian_days*60.0;
      minute = (int)floor(julian_days);

      julian_days -= floor(julian_days);
      julian_days = julian_days*60.0;
      second = (int) floor(julian_days); 
      break;
    }
  }

  sprintf(yyyymmddhhmmss,"%04d%02d%02d%02d%02d%02d", 
	  year,month,day,hour,minute,second);
  
  return 1;
}

