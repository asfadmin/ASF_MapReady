/* Implementation of the interface in date_time.h.  */

#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "basic_types.h"
#include "date_time.h"
#include "utilities.h"

/* Number of days in each of the months of a non-leap year.  Note that
   any function using this array will probably need to reset the value
   for February as appropriate for the current year each time.  */
static int days_in_months[13]
= {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

/* Set the day count for days_in_months[2] (February) as appropriate
   for year year.  */
static void
reset_february_day_count (int year)
{
  /* If its a leap year,  */
  if ( ((year % 4) == 0 ) && (((year % 100) != 0) || ((year % 400) == 0)) ) {
    /* then February has 29 days,  */
    days_in_months[2] = 29;
  } else {
    /* otherwise it has 28 days.  */
    days_in_months[2] = 28;
  }
}  

static void
year_day_of_year_to_year_month_day (int year, int day_of_year, int *month, 
				    int *day_of_month)
{
  reset_february_day_count (year);
  *month = 0;			/* Month. */
  int current_day = 0;
  for ( *month = 0 ; current_day < day_of_year ; (*month)++ ) {
    current_day += days_in_months[*month + 1];
  }
  /* Day of month.  */
  *day_of_month = day_of_year  - (current_day - days_in_months[*month]);
}

/* Given a year, day of year, and second of day, return a modified
   julian day.  */
static long double
year_day_second_to_mjd (int year, int day_of_year, double second_of_day)
{
  /* Convert year and day of year to year, month, and day of month.  */
  int y = year;			/* Shorthand for year.  */
  int m, d;			/* Month and day of month.  */
  year_day_of_year_to_year_month_day (y, day_of_year, &m, &d);

  /* Convert to julian day using the expression from "A Machine
     Algorithm for Processing Calendar Dates" by Henry Fleigel and
     Thomas Van Flandern.  */
  long double jd = d - 32075 + 1461 * (y + 4800 + (m - 14) / 12) / 4
    + 367 * (m - 2 - (m - 14) / 12 * 12) / 12 
    - 3 * ((y + 4900 + (m - 14) / 12) / 100) / 4 - 0.5;

  /* Return the modified julian day.  */
  return jd - MJD_OFFSET + (long double) second_of_day / SECONDS_PER_DAY;
}

/* Converts a given year, month, and day of month into a day of year.  */
static int
year_month_day_to_day_of_year (int year, int month, int day)
{
  int result;			/* Result to be returned.  */

  reset_february_day_count (year);
  result = day;
  int idx;
  for ( idx = 1 ; idx < month ; idx++ ) {
    result += days_in_months[idx];
  }

  return result;
}

static void
mjd_to_year_day_second (long double mjd, int *year, int *day_of_year, 
			double *second_of_day)
{
  /* I'm not sure the algorithm used here can handle negative days,
     and they shouldn't ever come up anyway.  */
  assert (mjd >= 0);
 
  /* Compute the julain day equivalent to the mjd.  */
  long double jd = mjd + MJD_OFFSET;
  assert (jd <= INT_MAX + 1.0);

 /* Use Jean Meeus algorithm from "Astronomical Formulae for
    Calculators" to get the calendar date.  */
  long double z_as_long_double;
  /* FIXME: The code between this comment and the line containing the
     commented out modfl call can be replaced by the modfl call when
     we have better C99 compliance.  */
  long double f = jd + 0.5 - floor (jd + 0.5);
  z_as_long_double = floor (jd + 0.5);
  /* long double f = modfl (jd + 0.5, &z_as_long_double); */
  int z = z_as_long_double;
  int a;
  if ( z < 2299161 ) {
    a = z;
  } else {
    int alpha = (z - 1867216.25) / 36524.25;
    a = z + 1 + alpha - (alpha / 4);
  }
  int b = a + 1524;
  int c = (b - 122.1) / 365.25;
  int d = 365.25 * c;
  int e = (b - d) / 30.6001;
  long double day_of_month = b - d - floor (30.6001 * e) + f;
  int month;
  if ( e <= 13 ) {
    month = e - 1;
  } else {
    month = e - 13;
  }
  if ( month >= 3 ) {
    *year = c - 4716;
  } else {
    *year = c - 4715;
  }

  /* Now fill in the day of year and second of day parts.  */
  *day_of_year = year_month_day_to_day_of_year (*year, month, day_of_month);
  *second_of_day = f * SECONDS_PER_DAY;
}

/* Specify a column of interest in the time offset table datafile.
   See the lookup_time_offset function.  */
typedef enum {
  UT1_MINUS_UTC,
  UT1_MINUS_UT1R,
  TAI_MINUS_UTC
} lookup_table_column_t;

/* Interpolate between entries in the lookup table to find the offset
   between between two different time scales for julian day mjd.  The
   lookup_table_column argument specifies the column of interest.  The
   lookup table has entries cataloged by UTC modified julian days, but
   if a TDT or UT1R time is supplied things should work well enough:
   for a table with entries only once per day, ~30 seconds of error in
   the index we want to interpolate to is pretty insignificant.  To
   save a bit of time, the data file is only reparsed if its
   modification time has changed.  */
static double
lookup_time_offset (const char *lookup_table_file, 
		    lookup_table_column_t lookup_table_column, 
		    long double mjd) 
{
  /* Properties of the lookup table.  */
  
  static const int table_columns = 3;
  static const int ut1_minus_utc_column_index = 0; 
  static const int ut1_minus_ut1r_column_index = 1; 
  static const int tai_minus_utc_column_index = 2;
 /* We will grow this as we read the file.  */
  static int table_lines = 0;
  /* Table entries are keyed by modified julian day.  */
  static int *table_mjd_keys = NULL;	
  static double **table = NULL;	/* Lookup table.  */

  /* If the lookup table file has changed, throw out the existing
     table data so it will get reloaded.  */
  static time_t old_modification_time = ( (time_t) -1);
  struct stat file_stats;
  int return_code = stat (lookup_table_file, &file_stats);
  assert (return_code == 0);
  if ( file_stats.st_mtime != old_modification_time 
       && table_mjd_keys != NULL) {
    free (table_mjd_keys);
    table_mjd_keys = NULL;
    int ii;			/* Index variable.  */
    for ( ii = 0 ; ii < table_lines ; ii++ ) {
      free (table[ii]);
    }
    free (table);
    table = NULL;
  }

  old_modification_time = file_stats.st_mtime;

  /* Load data from the file into the table, growing the table as we
     go.  */
  if ( table == NULL ) {
    FILE *lt = fopen (lookup_table_file, "r");
    assert (lt != NULL);
    /* Maximum line length, not including trailing null character.  */
    const int maximum_line_length = 200;
    char *line_buffer = malloc ((maximum_line_length + 1) * sizeof (char));
    while ( fgets (line_buffer, maximum_line_length + 1, lt) != NULL ) {
      if ( strncmp (line_buffer, "MJD\tYear", strlen ("MJD\tYear")) == 0 ) {
	continue;
      } else {
	table_lines++;
	table_mjd_keys = realloc (table_mjd_keys, table_lines * sizeof (int));
	table = realloc (table, table_lines * sizeof (double *));
	int cl = table_lines - 1; /* Current line of the table.  */
	table[cl] = malloc (table_columns * sizeof (double));
	/* Number of table columns we care about.  */
	const int columns_of_interest = 4;
	int assigned_field_count 
	  = sscanf (line_buffer, "%d %*d %*s %*d %lf %lf %lf\n", 
		    &table_mjd_keys[cl], 
		    &table[cl][ut1_minus_utc_column_index], 
		    &table[cl][ut1_minus_ut1r_column_index], 
		    &table[cl][tai_minus_utc_column_index]);
	assert (assigned_field_count == columns_of_interest);
      }
    }
    return_code = fclose (lt);
    assert (return_code == 0);
  }

  /* Do a binary search for the closest thing to the mjd of interest
     which we have.  */
  assert (mjd < INT_MAX);
  int mjd_int = lround (mjd);
  int min_index = 0;
  int max_index = table_lines - 1;
  int cli = table_lines / 2;
  while ( table_mjd_keys[cli] != mjd_int && min_index != max_index ) {
    if ( table_mjd_keys[cli] > mjd_int ) {
      max_index = cli;
    } else {
      min_index = cli;
    }
    cli = (max_index - min_index) / 2 + min_index;
  }

  /* Index of neighbor we will interpolate with.  */
  int neighbor_index;	
  if ( table_mjd_keys[cli] < mjd ) {
    neighbor_index = cli + 1;
  } else {
    neighbor_index = cli - 1;
  }

  /* We better now have two table entries, one from each side of the
     current mjd.  */
  assert (cli != neighbor_index);
  assert ((mjd >= table_mjd_keys[cli] && mjd <= table_mjd_keys[neighbor_index])
	  || (mjd <= table_mjd_keys[cli] 
	      && mjd >= table_mjd_keys[neighbor_index]));

  /* FIXME: interpolating accross leap seconds is the very likely the
     wrong thing to do, as there is a sharp discontinuity in UTC which
     probably propagates into the other calculations (i.e. everything
     should probably be stepped at the appropriate instant).  I
     haven't really thought this problem through though.  */
  if ( table[cli][tai_minus_utc_column_index]
       != table[neighbor_index][tai_minus_utc_column_index] ) {
    assert (0);			/* Not implemented yet.  */
  }

  /* Indicies of table entries before and after the mjd.  */
  int ib = cli < neighbor_index ? cli : neighbor_index;
  int ia = cli < neighbor_index ? neighbor_index : cli;

  /* Column of interest.  */
  int column_index;	
  if ( lookup_table_column == UT1_MINUS_UTC ) {
    column_index = ut1_minus_utc_column_index;
  } else if ( lookup_table_column == UT1_MINUS_UT1R ) {
    column_index = ut1_minus_ut1r_column_index;
  } else if ( lookup_table_column == TAI_MINUS_UTC ) {
    column_index = tai_minus_utc_column_index;
  } else {
    assert (FALSE);		/* Shouldn't get here.  */
  }

  /* Return the interpolated value.  */
  return ((1.0 - (mjd - table_mjd_keys[ib])) * table[ib][column_index]
	  + (1.0 - (table_mjd_keys[ia] - mjd)) * table[ia][column_index]);
}

/* The name of the in this librarie's part of the share directory in
   which we hope to find the lookup table for the offsets between the
   time scales.  */
static const char *time_scale_lookup_table_file = "time_scale_offsets";

/* Convert a modified julian day in time_scale to TDT.  */
static long double 
convert_to_terrestrial_dynamical_time (long double mjd, 
				       time_scale_t time_scale)
{
  static char *lookup_table_file = NULL;
  if ( lookup_table_file == NULL ) {
    lookup_table_file = datafile_path (time_scale_lookup_table_file);
  }

  /* Offsets between various time scales.  Converting involves solving
     some equations involving these differences.  */
  const double tdt_minus_tai = 32.184;
  double ut1_minus_utc = lookup_time_offset (lookup_table_file,
					     UT1_MINUS_UTC, mjd);
  double ut1_minus_ut1r = lookup_time_offset (lookup_table_file,
					      UT1_MINUS_UT1R, mjd);
  double tai_minus_utc = lookup_time_offset (lookup_table_file, 
					     TAI_MINUS_UTC, mjd);
  if ( time_scale == TDT ) {
    return mjd;
  } else if ( time_scale == UT1R ) {
    // FIXME: this method is currently broken.  It needs to be fixed
    // in the ways that he corresponding convert_from method was
    // fixed.
    assert (0);
    double ut1r_minus_tai = ut1_minus_utc - ut1_minus_ut1r - tai_minus_utc;
    double ut1r_minus_tdt = ut1r_minus_tai - tdt_minus_tai;
    /* We should maybe in theory be dividing by the astronomical
       seconds per day here, but the variation is only a couple of
       milliseconds, which would make only a tiny difference in an
       already small adjustment.  */
    return mjd - ( (long double) ut1r_minus_tdt / SECONDS_PER_DAY);
  } else if ( time_scale == UTC ) {
    // FIXME: this method is currently broken.  It needs to be fixed
    // in the ways that he corresponding convert_from method was
    // fixed.
    assert (0);
    double tdt_minus_utc = tdt_minus_tai + tai_minus_utc;
    return mjd + ( (long double) tdt_minus_utc / SECONDS_PER_DAY);
    assert (FALSE);
  } else {
    assert (FALSE);		/* Shouldn't be here.  */
  }
}

/* Convert a modified julian day in DTD to an mjd in time_scale.  This
   routine is almost completely symmetric with the
   convert_to_terrestrial_dynamical_time function.*/
static long double
convert_from_terrestrial_dynamical_time (long double mjd, 
					 time_scale_t time_scale)
{
  static char *lookup_table_file = NULL;
  if ( lookup_table_file == NULL ) {
    /* When testing we want to load local copies of datafile, for
       installed code we want the installed datafile.  */
    lookup_table_file = datafile_path (time_scale_lookup_table_file);
  }

  /* Offsets between various time scales.  Converting involves solving
     some equations involving these differences.  */
  const long double tdt_minus_tai = 32.184;
  long double ut1_minus_utc = lookup_time_offset (lookup_table_file,
						  UT1_MINUS_UTC, mjd);
  long double ut1_minus_ut1r = lookup_time_offset (lookup_table_file,
						   UT1_MINUS_UT1R, mjd);
  long double tai_minus_utc = lookup_time_offset (lookup_table_file, 
						  TAI_MINUS_UTC, mjd);
  if ( time_scale == TDT ) {
    return mjd;
  } else if ( time_scale == UT1R ) {
    long double ut1r_minus_tai 
      = ut1_minus_utc - ut1_minus_ut1r - tai_minus_utc;
    long double ut1r_minus_tdt = ut1r_minus_tai - tdt_minus_tai;
    return mjd + ( (long double) ut1r_minus_tdt / SECONDS_PER_DAY);
  } else if ( time_scale == UTC ) {
    long double tdt_minus_utc = tdt_minus_tai + tai_minus_utc;
    return mjd - ( (long double) tdt_minus_utc / SECONDS_PER_DAY);
  } else {
    assert (FALSE);		/* Shouldn't be here.  */
  }
}

DateTime *
date_time_new (int year, int day_of_year, double second_of_day,
	       time_scale_t time_scale)
{
  DateTime *self = malloc (sizeof (DateTime));
  assert (self != NULL);

  self->mjd = year_day_second_to_mjd (year, day_of_year, second_of_day);
  self->mjd = convert_to_terrestrial_dynamical_time (self->mjd, time_scale);
  mjd_to_year_day_second (self->mjd, &(self->year), &(self->day_of_year), 
			  &(self->second_of_day));

  return self;
}
  
DateTime *
date_time_new_from_mjd (long double mjd, time_scale_t time_scale)
{
  /* I'm not sure the algorithm used here can handle negative days,
     and they shouldn't ever come up anyway.  */
  assert (mjd >= 0);
  /* Allocate new object.  */
  DateTime *self = malloc (sizeof (DateTime));

  self->mjd = convert_to_terrestrial_dynamical_time (mjd, time_scale);
  mjd_to_year_day_second (self->mjd, &(self->year), &(self->day_of_year), 
			  &(self->second_of_day));

  return self;
}

DateTime *
date_time_new_from_ymds (int year, int month, int day, double second_of_day,
			 time_scale_t time_scale)
{
  DateTime *self = malloc (sizeof (DateTime));

  int day_of_year = year_month_day_to_day_of_year (year, month, day);
  self->mjd = year_day_second_to_mjd (year, day_of_year, second_of_day);
  self->mjd = convert_to_terrestrial_dynamical_time (self->mjd, time_scale);
  mjd_to_year_day_second (self->mjd, &(self->year), &(self->day_of_year), 
			  &(self->second_of_day));

  return self;
}

DateTime *
date_time_new_from_jd (long double jd, time_scale_t time_scale)
{
  return date_time_new_from_mjd (jd - MJD_OFFSET, time_scale);
}

DateTime *
date_time_new_from_jds (long double jds, time_scale_t time_scale)
{
  return date_time_new_from_mjd (jds + JDS_OFFSET - MJD_OFFSET, time_scale);
}

DateTime *
date_time_copy (DateTime *self)
{
  DateTime *copy = malloc (sizeof (DateTime));

  copy->year = self->year;
  copy->day_of_year = self->day_of_year;
  copy->second_of_day = self->second_of_day;
  copy->mjd = self->mjd;

  return copy;
}

/* Convert a MJD in TDT to year, month, day_of_month in time_scale.  */
static void
mjd_tdt_to_year_month_day (long double mjd_tdt, int *year, int *month, 
			   int *day_of_month, time_scale_t time_scale)
{
  /* MJD in terrestrial dynamical time.  */
  long double mjd
    = convert_from_terrestrial_dynamical_time (mjd_tdt, time_scale);
  int day_of_year;
  double second_of_day;
  mjd_to_year_day_second (mjd, year, &day_of_year, &second_of_day);
  year_day_of_year_to_year_month_day (*year, day_of_year, month, day_of_month);
}

int
date_time_year (DateTime *self, time_scale_t time_scale)
{
  int year, month, day_of_month;
  mjd_tdt_to_year_month_day (self->mjd, &year, &month, &day_of_month, 
			     time_scale);

  return year;
}  

int
date_time_month (DateTime *self, time_scale_t time_scale)
{
  int year, month, day_of_month;
  mjd_tdt_to_year_month_day (self->mjd, &year, &month, &day_of_month, 
			     time_scale);

  return month;
}

int
date_time_day_of_month (DateTime *self, time_scale_t time_scale)
{
  int year, month, day_of_month;
  mjd_tdt_to_year_month_day (self->mjd, &year, &month, &day_of_month, 
			     time_scale);

  return day_of_month;
}

static void
mjd_tdt_to_year_day_second (long double mjd_tdt, int *year, int *day, 
			    double *second_of_day, time_scale_t time_scale)
{
  /* MJD in desired time scale.  */
  long double mjd
    = convert_from_terrestrial_dynamical_time (mjd_tdt, time_scale);
  mjd_to_year_day_second (mjd, year, day, second_of_day);
}

int
date_time_hour_of_day (DateTime *self, time_scale_t time_scale)
{
  int year, day_of_year;
  double second_of_day;
  mjd_tdt_to_year_day_second (self->mjd, &year, &day_of_year, &second_of_day,
			      time_scale);
  
  return second_of_day / (SECONDS_PER_MINUTE * MINUTES_PER_HOUR);
}

int
date_time_minute_of_hour (DateTime *self, time_scale_t time_scale)
{
  int year, day_of_year;
  double second_of_day;
  mjd_tdt_to_year_day_second (self->mjd, &year, &day_of_year, &second_of_day,
			      time_scale);

  static const int seconds_per_hour = SECONDS_PER_MINUTE * MINUTES_PER_HOUR;

  return ((second_of_day - (seconds_per_hour
			    * floor (second_of_day 
				     / seconds_per_hour)))
	  / SECONDS_PER_MINUTE);
}

double
date_time_second_of_minute (DateTime *self, time_scale_t time_scale)
{
  int year, day_of_year;
  double second_of_day;
  mjd_tdt_to_year_day_second (self->mjd, &year, &day_of_year, &second_of_day,
			      time_scale);

  return second_of_day - (SECONDS_PER_MINUTE 
			  * floor (second_of_day / SECONDS_PER_MINUTE));
}

long double
date_time_mjd (DateTime *self, time_scale_t time_scale)
{
  return convert_from_terrestrial_dynamical_time (self->mjd, time_scale);
}

long double
date_time_jd (DateTime *self, time_scale_t time_scale)
{
  return (convert_from_terrestrial_dynamical_time (self->mjd, time_scale)
	  + MJD_OFFSET);
}

long double
date_time_jds (DateTime *self, time_scale_t time_scale)
{
  return (convert_from_terrestrial_dynamical_time (self->mjd, time_scale) 
	  + MJD_OFFSET - JDS_OFFSET);
}

double
date_time_difference (DateTime *self, DateTime *other)
{
  return (self->mjd - other->mjd) * SECONDS_PER_DAY;
}

/* Add seconds to an existing DateTime (changing it).  */
void
date_time_add_seconds (DateTime *self, double seconds)
{
  self->mjd += (long double) seconds / SECONDS_PER_DAY;
  mjd_to_year_day_second (self->mjd, &(self->year), &(self->day_of_year), 
			  &(self->second_of_day));
}

/* Subtract seconds from an existing DateTime (changing it).  */
void
date_time_subtract_seconds (DateTime *self, double seconds)
{
  self->mjd -= (long double) seconds / SECONDS_PER_DAY;
  assert (self->mjd >= 0);
  mjd_to_year_day_second (self->mjd, &(self->year), &(self->day_of_year), 
			  &(self->second_of_day));
}

/* Free a dynamicly allocated DateTime object.  */
void
date_time_free (DateTime *self)
{
  free (self);
}
