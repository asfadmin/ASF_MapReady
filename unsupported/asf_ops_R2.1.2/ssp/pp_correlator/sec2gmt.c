/* SccsId[]= @(#)sec2gmt.c	2.41 3/24/98 */
static char sccsid_sec2gmt[]= "@(#)PPsec2gmt.c:2.41";

/***************************************************************************
   This function converts Julian seconds to GMT format time, e.g.,
   1997-002T00:23:25.000.  The function works only for days after 
   January 1, 1900, though it is not difficult to modify the code to work 
   for days before January 1, 1900. January 1, 1900 corresponds to
   the 2415020.5th Julian day which is the 2.086577712e+11th Julian 
   second.

   Input: Julian_seconds
   Output: GMT

   Warning: String buffer length gmt has to be at least 23 characters long.

   Author:  Shelby Yang.   3/25/97 at JPL
   *************************************************************************/

#include <stdio.h>
#include <math.h>

static int days_in_year[2][1] = {365,366};
  
int sec2gmt(double julian_seconds, char* gmt)
{
  int leap, year, day, hour, minute;
  int i;
  double julian_days, second, julian_day_1900_1_1;

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

      julian_days -= floor(julian_days);
      julian_days = julian_days*24.0;
      hour = (int) floor(julian_days);

      julian_days -= floor(julian_days);
      julian_days = julian_days*60.0;
      minute = (int)floor(julian_days);

      julian_days -= floor(julian_days);
      julian_days = julian_days*60.0;
      second = julian_days; 
      break;
    }
  }

  sprintf(gmt,"%04d-%03dT%02d:%02d:%06.3lf", year,day,hour,minute,second);
  
  return 1;
}

