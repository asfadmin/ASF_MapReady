/* Tests for the DateTime class.  */

// FIXME: remove after done fixing tests.
#include <stdio.h>

#include <assert.h>
#include <math.h>
#include <stdlib.h>

#include "date_time.h"

#define FALSE 0
#define TRUE !FALSE

/* Base year, day of year, and second of day.  We will add and
   subtract from these to form some other times to test.  */
#define BASE_YEAR 1996
#define BASE_DAY_OF_YEAR 54
#define BASE_SECOND_OF_DAY 42000.42
/* Equivalent base modified julian day.  */
#define BASE_MJD (50136.0 + (long double) BASE_SECOND_OF_DAY / SECONDS_PER_DAY)
#define BASE_MONTH 2
#define BASE_DAY_OF_MONTH 23
#define BASE_HOUR_OF_DAY ( (int) floor (BASE_SECOND_OF_DAY \
				        / (SECONDS_PER_HOUR)))
#define BASE_MINUTE_OF_HOUR \
  ( (int) floor ((BASE_SECOND_OF_DAY - BASE_HOUR_OF_DAY * SECONDS_PER_HOUR) \
	         / SECONDS_PER_MINUTE))
#define BASE_SECOND_OF_MINUTE \
  (BASE_SECOND_OF_DAY - BASE_HOUR_OF_DAY * SECONDS_PER_HOUR \
   - BASE_MINUTE_OF_HOUR * SECONDS_PER_MINUTE)
#define BASE_JD ((long double) BASE_MJD + MJD_OFFSET)
#define BASE_JDS ((long double) BASE_MJD + MJD_OFFSET - JDS_OFFSET)
/* Equivalent coordinated universal time, computed by hand from
   International Earth Rotation Service Bulletin B Issue 98 using
   linear interpolation.  */
#define BASE_EQUIVALENT_UTC 50136.48539625000000L
/* Equivalent UT1R time, calculated in the same way as
   BASE_EQUIVALENT_UTC.  */
#define BASE_EQUIVALENT_UT1R 50136.48540150729476L

#define APPROXIMATE_DAYS_PER_MONTH 30
/* Date after addition of about a month of time.  */
#define DATE_IN_FUTURE_MJD ( (long double) BASE_MJD \
                             + APPROXIMATE_DAYS_PER_MONTH)
#define DATE_IN_FUTURE_YEAR 1996
#define DATE_IN_FUTURE_DAY_OF_YEAR (BASE_DAY_OF_YEAR + \
                                    APPROXIMATE_DAYS_PER_MONTH)
#define DATE_IN_FUTURE_SECOND_OF_DAY BASE_SECOND_OF_DAY
#define DATE_IN_FUTURE_MONTH 3
#define DATE_IN_FUTURE_DAY_OF_MONTH 24

#define MONTHS_PER_YEAR 12
/* Date after subtraction of about a year.  */
#define DATE_IN_PAST_MJD ( (long double) BASE_MJD - MONTHS_PER_YEAR \
                           * APPROXIMATE_DAYS_PER_MONTH)

/* Return true iff two day values compare equal within the advertised
   tolerance.  */
#define COMPARE_DAYS(a, b) \
  (fabsl (a - b) < TIME_PRECISION_IN_SECONDS / SECONDS_PER_DAY ? TRUE : FALSE)
/* Return true iff two second values compare equal with the advertised
   tolerance.  */
#define COMPARE_SECONDS(a, b) \
  (fabs (a - b) < TIME_PRECISION_IN_SECONDS ? TRUE : FALSE)

/* Assert that the fields of the instance structure are correctly set
   to the base values (BASE_BASE_YEAR, BASE_DAY_OF_YEAR, etc.).  */
static void
assert_base_values (DateTime *dt)
{
  /* Compare values in year/day/second form.  */
  assert (dt->year == BASE_YEAR);
  assert (dt->day_of_year == BASE_DAY_OF_YEAR);
  assert (COMPARE_SECONDS (dt->second_of_day, BASE_SECOND_OF_DAY));

  /* Compare modified julian day value.  */
  assert (COMPARE_DAYS (dt->mjd, BASE_MJD));
}

int
main (void)
{
  /* Test creation methods.  */

  DateTime *dt = date_time_new (BASE_YEAR, BASE_DAY_OF_YEAR, 
				BASE_SECOND_OF_DAY, TDT);
  assert_base_values (dt);
  /* Do the UTC and UT1R translations work as expected?  */
  assert (COMPARE_DAYS (date_time_mjd (dt, UTC), BASE_EQUIVALENT_UTC));
  assert (COMPARE_DAYS (date_time_mjd (dt, UT1R), BASE_EQUIVALENT_UT1R));
  date_time_free (dt);
  
  dt = date_time_new_from_mjd (BASE_MJD, TDT);
  assert_base_values (dt);
  date_time_free (dt);
  
  dt = date_time_new_from_ymds (BASE_YEAR, BASE_MONTH, BASE_DAY_OF_MONTH,
				BASE_SECOND_OF_DAY, TDT);
  assert_base_values (dt);
  date_time_free (dt);
  
  dt = date_time_new_from_jd (BASE_JD, TDT);
  assert_base_values (dt);
  date_time_free (dt);
  
  dt = date_time_new_from_jds (BASE_JDS, TDT);
  assert_base_values (dt);
  
  /* Test synthesizing methods.  */

  assert (date_time_day_of_month (dt, TDT) == BASE_DAY_OF_MONTH);
  assert (date_time_hour_of_day (dt, TDT) == BASE_HOUR_OF_DAY);
  assert (date_time_minute_of_hour (dt, TDT) == BASE_MINUTE_OF_HOUR);
  assert (COMPARE_SECONDS (date_time_second_of_minute (dt, TDT), 
			   BASE_SECOND_OF_MINUTE));
  assert (COMPARE_DAYS (date_time_jd (dt, TDT), BASE_JD));
  assert (COMPARE_DAYS (date_time_jds (dt, TDT), BASE_JDS));

  /* Test copy method.  */
  
  DateTime *dt2 = date_time_copy (dt);
  assert_base_values (dt2);
  
  /* Test date arithmetic.  */
  
  date_time_add_seconds (dt2, APPROXIMATE_DAYS_PER_MONTH * SECONDS_PER_DAY);
  assert (dt2->year == DATE_IN_FUTURE_YEAR 
	  && dt2->day_of_year == DATE_IN_FUTURE_DAY_OF_YEAR
	  && COMPARE_SECONDS (dt2->second_of_day, 
			      DATE_IN_FUTURE_SECOND_OF_DAY));
  assert (COMPARE_DAYS (dt2->mjd, DATE_IN_FUTURE_MJD));
  assert (COMPARE_SECONDS (date_time_difference (dt2, dt),
			   APPROXIMATE_DAYS_PER_MONTH * SECONDS_PER_DAY));
  assert (date_time_month (dt2, TDT) == DATE_IN_FUTURE_MONTH);
  assert (date_time_day_of_month (dt2, TDT) == DATE_IN_FUTURE_DAY_OF_MONTH);
  
  date_time_free (dt2);
  dt2 = date_time_copy (dt);
  date_time_subtract_seconds (dt2, MONTHS_PER_YEAR * APPROXIMATE_DAYS_PER_MONTH
			           * SECONDS_PER_DAY);
  assert (COMPARE_DAYS (dt2->mjd, DATE_IN_PAST_MJD));
  assert (COMPARE_SECONDS (date_time_difference (dt2, dt), 
	  -(MONTHS_PER_YEAR * APPROXIMATE_DAYS_PER_MONTH * SECONDS_PER_DAY)));

  exit (EXIT_SUCCESS);
}
