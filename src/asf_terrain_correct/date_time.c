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

#include <gsl/gsl_math.h>

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
double
year_day_second_to_mjd (int year, int day_of_year, double second_of_day)
{
  /* Convert year and day of year to year, month, and day of month.  */
  int y = year;			/* Shorthand for year.  */
  int m, d;			/* Month and day of month.  */
  year_day_of_year_to_year_month_day (y, day_of_year, &m, &d);

  /* Convert to julian day using the expression from "A Machine
     Algorithm for Processing Calendar Dates" by Henry Fleigel and
     Thomas Van Flandern.  */
  double jd = d - 32075 + 1461 * (y + 4800 + (m - 14) / 12) / 4
    + 367 * (m - 2 - (m - 14) / 12 * 12) / 12 
    - 3 * ((y + 4900 + (m - 14) / 12) / 100) / 4 - 0.5;

  /* Return the modified julian day.  */
  return jd - MJD_OFFSET + (double) second_of_day / SECONDS_PER_DAY;
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
mjd_to_year_day_second (double mjd, int *year, int *day_of_year, 
			double *second_of_day)
{
  /* I'm not sure the algorithm used here can handle negative days,
     and they shouldn't ever come up anyway.  */
  assert (mjd >= 0);
 
  /* Compute the julain day equivalent to the mjd.  */
  double jd = mjd + MJD_OFFSET;
  assert (jd <= INT_MAX + 1.0);

 /* Use Jean Meeus algorithm from "Astronomical Formulae for
    Calculators" to get the calendar date.  */
  double z_as_long_double;
  /* FIXME: The code between this comment and the line containing the
     commented out modfl call can be replaced by the modfl call when
     we have better C99 compliance.  */
  double f = jd + 0.5 - floor (jd + 0.5);
  z_as_long_double = floor (jd + 0.5);
  /* double f = modfl (jd + 0.5, &z_as_long_double); */
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
  double day_of_month = b - d - floor (30.6001 * e) + f;
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
  TAI_MINUS_UTC,
  DELTA_PSI
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
		    double mjd) 
{
  /* At the moment, we only have time offset data back to about
     November 2nd, 1995 (MJD 50023), and up to about Jan. 26, 2005
     (MJD 53396).  More data can easily be obtained for later dates
     from the International Earth Rotation Service (ITRS), for earlier
     dates a different bulletin format may be used which might make
     this difficult.  If the table is expanded these assertions will
     have to change.  */
//  assert (mjd > 50023);
//  assert (mjd < 53396);

  /* Properties of the lookup table.  */
  static const int table_columns = 4;
  static const int ut1_minus_utc_column_index = 0; 
  static const int ut1_minus_ut1r_column_index = 1; 
  static const int tai_minus_utc_column_index = 2;
  static const int delta_psi_column_index = 3;

  /* We will grow this as we read the file.  */
  static int table_lines = 0;

  /* Table entries are keyed by modified julian day.  */
  static int *table_mjd_keys = NULL;	
  static double **table = NULL;	/* Lookup table.  */

  /* If the lookup table file has changed, throw out the existing
     table data so it will get reloaded.  */
  static time_t old_modification_time = ((time_t) -1);
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
	const int columns_of_interest = 5;
	int assigned_field_count 
	  = sscanf (line_buffer, "%d %*d %*s %*d %lf %lf %lf %lf\n", 
		    &(table_mjd_keys[cl]), 
		    &(table[cl][ut1_minus_utc_column_index]), 
		    &(table[cl][ut1_minus_ut1r_column_index]), 
		    &(table[cl][tai_minus_utc_column_index]),
		    &(table[cl][delta_psi_column_index]));
	assert (assigned_field_count == columns_of_interest);
      }
    }
    return_code = fclose (lt);
    assert (return_code == 0);
  }

  /* Do a binary search for the closest thing to the mjd of interest
     which we have.  */
  if ( mjd >= INT_MAX ) {
    assert (mjd < INT_MAX);
  }
  int mjd_int = (int) (mjd + 0.5);
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

  // outside the range of the table
  if (cli == 0 || cli == table_lines - 1)
  {
    printf("Warning: "
	   "Do not have time offsets available for this date/time.\n");
    return 0.0;
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
  int column_index=0;	
  if ( lookup_table_column == UT1_MINUS_UTC ) {
    column_index = ut1_minus_utc_column_index;
  } 
  else if ( lookup_table_column == UT1_MINUS_UT1R ) {
    column_index = ut1_minus_ut1r_column_index;
  } 
  else if ( lookup_table_column == TAI_MINUS_UTC ) {
    column_index = tai_minus_utc_column_index;
  }
  else if (lookup_table_column == DELTA_PSI ) {
    column_index = delta_psi_column_index;
  } 
  else {
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

/* Some functions in this library use a static data file, the path of
   which is determnined at run time.  This variable holds the name of
   the path, or NULL to indicate that it hasn't been looked up
   yet.  */
static char *lookup_table_file = NULL;

/* Convert a modified julian day in time_scale to TDT.  */
static double 
convert_to_terrestrial_dynamical_time (double mjd, 
				       time_scale_t time_scale)
{
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
    // FIXME: these methods don't have test cases.
    double ut1r_minus_tai 
      = ut1_minus_utc - ut1_minus_ut1r - tai_minus_utc;
    double ut1r_minus_tdt = ut1r_minus_tai - tdt_minus_tai;
    return mjd - ( (double) ut1r_minus_tdt / SECONDS_PER_DAY);
  } else if ( time_scale == UTC ) {
    // FIXME: these methods don't have test cases.
    double tdt_minus_utc = tdt_minus_tai + tai_minus_utc;
    return mjd + ( (double) tdt_minus_utc / SECONDS_PER_DAY);
    assert (FALSE);
  } else {
    assert (FALSE);		/* Shouldn't be here.  */
  }
  return 0.0;
}

/* Convert a modified julian day in DTD to an mjd in time_scale.  This
   routine is almost completely symmetric with the
   convert_to_terrestrial_dynamical_time function.*/
static double
convert_from_terrestrial_dynamical_time (double mjd, 
					 time_scale_t time_scale)
{
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
    double ut1r_minus_tai 
      = ut1_minus_utc - ut1_minus_ut1r - tai_minus_utc;
    double ut1r_minus_tdt = ut1r_minus_tai - tdt_minus_tai;
    return mjd + ( (double) ut1r_minus_tdt / SECONDS_PER_DAY);
  } else if ( time_scale == UTC ) {
    double tdt_minus_utc = tdt_minus_tai + tai_minus_utc;
    return mjd - ( (double) tdt_minus_utc / SECONDS_PER_DAY);
  } else {
    assert (FALSE);		/* Shouldn't be here.  */
  }
  return 0.0;
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
date_time_new_from_mjd (double mjd, time_scale_t time_scale)
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
date_time_new_from_jd (double jd, time_scale_t time_scale)
{
  return date_time_new_from_mjd (jd - MJD_OFFSET, time_scale);
}

DateTime *
date_time_new_from_jds (double jds, time_scale_t time_scale)
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
mjd_tdt_to_year_month_day (double mjd_tdt, int *year, int *month, 
			   int *day_of_month, time_scale_t time_scale)
{
  /* MJD in terrestrial dynamical time.  */
  double mjd
    = convert_from_terrestrial_dynamical_time (mjd_tdt, time_scale);
  int day_of_year;
  double second_of_day;
  mjd_to_year_day_second (mjd, year, &day_of_year, &second_of_day);
  year_day_of_year_to_year_month_day (*year, day_of_year, month, day_of_month);
}

// FIXME: all the methods may run into troube when UTC time scale
// results are requested that fall on leap seconds -- wrongful
// arbitrary choice of one of the two possible values at the
// discontinuity is the least bad possibility.

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
mjd_tdt_to_year_day_second (double mjd_tdt, int *year, int *day, 
			    double *second_of_day, time_scale_t time_scale)
{
  /* MJD in desired time scale.  */
  double mjd
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

double
date_time_mjd (DateTime *self, time_scale_t time_scale)
{
  return convert_from_terrestrial_dynamical_time (self->mjd, time_scale);
}

double
date_time_jd (DateTime *self, time_scale_t time_scale)
{
  return (convert_from_terrestrial_dynamical_time (self->mjd, time_scale)
	  + MJD_OFFSET);
}

double
date_time_jds (DateTime *self, time_scale_t time_scale)
{
  return (convert_from_terrestrial_dynamical_time (self->mjd, time_scale) 
	  + MJD_OFFSET - JDS_OFFSET);
}

/* Compute the mean obliquity of the ecliptic for a given time, in
   radians.  See "Satellite Orbits: Models, Methods, and
   Applications", section 5.3.2, by Oliver Montenbruck and Eberhard
   Gill for details.  */
static double
mean_obliquity (double long mjd_tt)
{
  static DateTime *standard_epoch = NULL;
  if ( standard_epoch == NULL ) {
    standard_epoch = date_time_new (2000, 1, 12 * SECONDS_PER_HOUR, TDT);
  }

  const double julian_century = 36525.0;

  double T = ((mjd_tt - date_time_mjd (standard_epoch, TDT)) 
		   / julian_century);

  return (M_PI / 180.0) * (23.43929111 
			   - ((46.8150 + (0.00059 - 0.001813 * T) * T) 
			      * T / 3600.0));
}

// Fractional part of a number.
static double 
fractional_part (double x)
{ 
  return fmod (x, 1.0);
}

// The floating point remainder of x / y.
static double 
float_modulo (double x, double y)
{ 
  return y * fractional_part (x / y);
}

/* Determine the nutation in longitude counted in the ecliptic.  This
   uses the 1980 International Astronomical Union (IAU) nutation
   theory.  See "Satellite Geodesy 2nd Edition" by Gunter Seeber.  */
static double
delta_psi (double mjd_tt)
{
  static DateTime *standard_epoch = NULL;
  if ( standard_epoch == NULL ) {
    standard_epoch = date_time_new (2000, 1, 12 * SECONDS_PER_HOUR, TDT);
  }

  // Length of one julian century in days.
  const double one_julian_century = 36525.0;

  // Constants
  const double t  = ((mjd_tt - date_time_mjd (standard_epoch, TDT)) 
			  / one_julian_century);
  const double t2 = t *t;
  const double t3 = t2 * t;
  const double asprev = 360.0 * 3600.0;  // Arcseconds per revolution.

  // Number of coefficients in series.
#define COEFFICIENT_COUNT 106

  // Series coefficients.
  const long int c[COEFFICIENT_COUNT][9] =
  {
    //
    // l  l' F  D Om    dpsi    *T     deps     *T      #
    //
    {  0, 0, 0, 0, 1,-1719960,-1742,  920250,   89 },   //   1
    {  0, 0, 0, 0, 2,   20620,    2,   -8950,    5 },   //   2
    { -2, 0, 2, 0, 1,     460,    0,    -240,    0 },   //   3
    {  2, 0,-2, 0, 0,     110,    0,       0,    0 },   //   4
    { -2, 0, 2, 0, 2,     -30,    0,      10,    0 },   //   5
    {  1,-1, 0,-1, 0,     -30,    0,       0,    0 },   //   6
    {  0,-2, 2,-2, 1,     -20,    0,      10,    0 },   //   7
    {  2, 0,-2, 0, 1,      10,    0,       0,    0 },   //   8
    {  0, 0, 2,-2, 2, -131870,  -16,   57360,  -31 },   //   9
    {  0, 1, 0, 0, 0,   14260,  -34,     540,   -1 },   //  10
    {  0, 1, 2,-2, 2,   -5170,   12,    2240,   -6 },   //  11
    {  0,-1, 2,-2, 2,    2170,   -5,    -950,    3 },   //  12
    {  0, 0, 2,-2, 1,    1290,    1,    -700,    0 },   //  13
    {  2, 0, 0,-2, 0,     480,    0,      10,    0 },   //  14
    {  0, 0, 2,-2, 0,    -220,    0,       0,    0 },   //  15
    {  0, 2, 0, 0, 0,     170,   -1,       0,    0 },   //  16
    {  0, 1, 0, 0, 1,    -150,    0,      90,    0 },   //  17
    {  0, 2, 2,-2, 2,    -160,    1,      70,    0 },   //  18
    {  0,-1, 0, 0, 1,    -120,    0,      60,    0 },   //  19
    { -2, 0, 0, 2, 1,     -60,    0,      30,    0 },   //  20
    {  0,-1, 2,-2, 1,     -50,    0,      30,    0 },   //  21
    {  2, 0, 0,-2, 1,      40,    0,     -20,    0 },   //  22
    {  0, 1, 2,-2, 1,      40,    0,     -20,    0 },   //  23
    {  1, 0, 0,-1, 0,     -40,    0,       0,    0 },   //  24
    {  2, 1, 0,-2, 0,      10,    0,       0,    0 },   //  25
    {  0, 0,-2, 2, 1,      10,    0,       0,    0 },   //  26
    {  0, 1,-2, 2, 0,     -10,    0,       0,    0 },   //  27
    {  0, 1, 0, 0, 2,      10,    0,       0,    0 },   //  28
    { -1, 0, 0, 1, 1,      10,    0,       0,    0 },   //  29
    {  0, 1, 2,-2, 0,     -10,    0,       0,    0 },   //  30
    {  0, 0, 2, 0, 2,  -22740,   -2,    9770,   -5 },   //  31
    {  1, 0, 0, 0, 0,    7120,    1,     -70,    0 },   //  32
    {  0, 0, 2, 0, 1,   -3860,   -4,    2000,    0 },   //  33
    {  1, 0, 2, 0, 2,   -3010,    0,    1290,   -1 },   //  34
    {  1, 0, 0,-2, 0,   -1580,    0,     -10,    0 },   //  35
    { -1, 0, 2, 0, 2,    1230,    0,    -530,    0 },   //  36
    {  0, 0, 0, 2, 0,     630,    0,     -20,    0 },   //  37
    {  1, 0, 0, 0, 1,     630,    1,    -330,    0 },   //  38
    { -1, 0, 0, 0, 1,    -580,   -1,     320,    0 },   //  39
    { -1, 0, 2, 2, 2,    -590,    0,     260,    0 },   //  40
    {  1, 0, 2, 0, 1,    -510,    0,     270,    0 },   //  41
    {  0, 0, 2, 2, 2,    -380,    0,     160,    0 },   //  42
    {  2, 0, 0, 0, 0,     290,    0,     -10,    0 },   //  43
    {  1, 0, 2,-2, 2,     290,    0,    -120,    0 },   //  44
    {  2, 0, 2, 0, 2,    -310,    0,     130,    0 },   //  45
    {  0, 0, 2, 0, 0,     260,    0,     -10,    0 },   //  46
    { -1, 0, 2, 0, 1,     210,    0,    -100,    0 },   //  47
    { -1, 0, 0, 2, 1,     160,    0,     -80,    0 },   //  48
    {  1, 0, 0,-2, 1,    -130,    0,      70,    0 },   //  49
    { -1, 0, 2, 2, 1,    -100,    0,      50,    0 },   //  50
    {  1, 1, 0,-2, 0,     -70,    0,       0,    0 },   //  51
    {  0, 1, 2, 0, 2,      70,    0,     -30,    0 },   //  52
    {  0,-1, 2, 0, 2,     -70,    0,      30,    0 },   //  53
    {  1, 0, 2, 2, 2,     -80,    0,      30,    0 },   //  54
    {  1, 0, 0, 2, 0,      60,    0,       0,    0 },   //  55
    {  2, 0, 2,-2, 2,      60,    0,     -30,    0 },   //  56
    {  0, 0, 0, 2, 1,     -60,    0,      30,    0 },   //  57
    {  0, 0, 2, 2, 1,     -70,    0,      30,    0 },   //  58
    {  1, 0, 2,-2, 1,      60,    0,     -30,    0 },   //  59
    {  0, 0, 0,-2, 1,     -50,    0,      30,    0 },   //  60
    {  1,-1, 0, 0, 0,      50,    0,       0,    0 },   //  61
    {  2, 0, 2, 0, 1,     -50,    0,      30,    0 },   //  62
    {  0, 1, 0,-2, 0,     -40,    0,       0,    0 },   //  63
    {  1, 0,-2, 0, 0,      40,    0,       0,    0 },   //  64
    {  0, 0, 0, 1, 0,     -40,    0,       0,    0 },   //  65
    {  1, 1, 0, 0, 0,     -30,    0,       0,    0 },   //  66
    {  1, 0, 2, 0, 0,      30,    0,       0,    0 },   //  67
    {  1,-1, 2, 0, 2,     -30,    0,      10,    0 },   //  68
    { -1,-1, 2, 2, 2,     -30,    0,      10,    0 },   //  69
    { -2, 0, 0, 0, 1,     -20,    0,      10,    0 },   //  70
    {  3, 0, 2, 0, 2,     -30,    0,      10,    0 },   //  71
    {  0,-1, 2, 2, 2,     -30,    0,      10,    0 },   //  72
    {  1, 1, 2, 0, 2,      20,    0,     -10,    0 },   //  73
    { -1, 0, 2,-2, 1,     -20,    0,      10,    0 },   //  74
    {  2, 0, 0, 0, 1,      20,    0,     -10,    0 },   //  75
    {  1, 0, 0, 0, 2,     -20,    0,      10,    0 },   //  76
    {  3, 0, 0, 0, 0,      20,    0,       0,    0 },   //  77
    {  0, 0, 2, 1, 2,      20,    0,     -10,    0 },   //  78
    { -1, 0, 0, 0, 2,      10,    0,     -10,    0 },   //  79
    {  1, 0, 0,-4, 0,     -10,    0,       0,    0 },   //  80
    { -2, 0, 2, 2, 2,      10,    0,     -10,    0 },   //  81
    { -1, 0, 2, 4, 2,     -20,    0,      10,    0 },   //  82
    {  2, 0, 0,-4, 0,     -10,    0,       0,    0 },   //  83
    {  1, 1, 2,-2, 2,      10,    0,     -10,    0 },   //  84
    {  1, 0, 2, 2, 1,     -10,    0,      10,    0 },   //  85
    { -2, 0, 2, 4, 2,     -10,    0,      10,    0 },   //  86
    { -1, 0, 4, 0, 2,      10,    0,       0,    0 },   //  87
    {  1,-1, 0,-2, 0,      10,    0,       0,    0 },   //  88
    {  2, 0, 2,-2, 1,      10,    0,     -10,    0 },   //  89
    {  2, 0, 2, 2, 2,     -10,    0,       0,    0 },   //  90
    {  1, 0, 0, 2, 1,     -10,    0,       0,    0 },   //  91
    {  0, 0, 4,-2, 2,      10,    0,       0,    0 },   //  92
    {  3, 0, 2,-2, 2,      10,    0,       0,    0 },   //  93
    {  1, 0, 2,-2, 0,     -10,    0,       0,    0 },   //  94
    {  0, 1, 2, 0, 1,      10,    0,       0,    0 },   //  95
    { -1,-1, 0, 2, 1,      10,    0,       0,    0 },   //  96
    {  0, 0,-2, 0, 1,     -10,    0,       0,    0 },   //  97
    {  0, 0, 2,-1, 2,     -10,    0,       0,    0 },   //  98
    {  0, 1, 0, 2, 0,     -10,    0,       0,    0 },   //  99
    {  1, 0,-2,-2, 0,     -10,    0,       0,    0 },   // 100
    {  0,-1, 2, 0, 1,     -10,    0,       0,    0 },   // 101
    {  1, 1, 0,-2, 1,     -10,    0,       0,    0 },   // 102
    {  1, 0,-2, 2, 0,     -10,    0,       0,    0 },   // 103
    {  2, 0, 0, 2, 0,      10,    0,       0,    0 },   // 104
    {  0, 0, 2, 4, 2,     -10,    0,       0,    0 },   // 105
    {  0, 1, 0, 1, 0,      10,    0,       0,    0 }    // 106
  };

  // Mean arguments of luni-solar motion

  double l;		      // Mean anomaly of the moon.
  double lp;		      // Mean anomaly of the sun.
  double F;		      // Mean argument of latitude.
  // Mean longitude elongation fo the moon from the sun.
  double D;
  double Om;		      // Mean longitude of the ascending node.
  
  l  = float_modulo (485866.733 + (1325.0 * asprev +  715922.633) * t    
		     + 31.310 * t2 + 0.064 * t3, asprev);
  lp = float_modulo (1287099.804 + (  99.0 * asprev + 1292581.224) * t
		     - 0.577 * t2 - 0.012 * t3, asprev);
  F  = float_modulo (335778.877 + (1342.0 * asprev +  295263.137) * t    
		     - 13.257 * t2 + 0.011 * t3, asprev);
  D  = float_modulo (1072261.307 + (1236.0 * asprev + 1105601.328) * t    
		     - 6.891 * t2 + 0.019 * t3, asprev);
  Om = float_modulo (450160.280 - (   5.0 * asprev +  482890.539) * t    
		     + 7.455 * t2 + 0.008 * t3, asprev);

  double asprad = 3600.0 * 180.0 / M_PI;     // Arcseconds per radian.

  // Nutation in longitude, in radians, to be returned.
  double result = 0.0;

  int ii;
  for ( ii = 0;  ii < COEFFICIENT_COUNT ; ii++ ) {
    // Angle argument.
    double arg  
      = (c[ii][0] * l + c[ii][1] * lp + c[ii][2] * F + c[ii][3] * D 
	 + c[ii][4] * Om ) / asprad;
    result += ( c[ii][5] + c[ii][6] * t ) * sin (arg);
    // We don't need delta_epsilon at the moment.
    // deps += ( c[ii][7] + c[ii][8] * t ) * cos (arg);
  };
      
  result = 1.0E-5 * result / asprad;

  return result;
}
  
/* Evaluate the so-called equation of equinoxes, which is equal to the
   Greenwich Mean Sidereal Time minus the Greenwich actual sidereal
   Time, for a given time.  The result is in seconds.  See "Satellite
   Geodesy Second Edition", Section 2.2.2 for details. */
static double
equation_of_equinoxes (double mjd_tt)
{
  return delta_psi (mjd_tt) * cos (mean_obliquity (mjd_tt));
}

double
date_time_earth_angle (DateTime *self)
{
  /* The official relationship between UT1 and UTC changed on this
     date.  */
  static DateTime *change_over_date = NULL;
  if (change_over_date == NULL ) {
    change_over_date = date_time_new (2003, 1, 0, UTC);
  }

  // FIXME: For some goofy reason, to match the hour angle embedded in
  // the data, we need to use the old pre-2003 way of computing the
  // hour angle.  I suspect this is just what the processor does and
  // has never been fixed.  So for now we have this || 1 crap here.
  if ( date_time_is_before (self, change_over_date) || 1) {
      static DateTime *standard_epoch = NULL;
    if ( standard_epoch == NULL ) {
      standard_epoch = date_time_new (2000, 1, 12 * SECONDS_PER_HOUR, UT1R);
    }
    /* Here we trust the magic in "Satellite Geodesy 2nd Edition",
       section 2.2.2.  */
    /* Julian century length in days. */
    const double one_julian_century = 36525.0;
    /* Current modified julian day in sidereal time.  */
    double cmjd = date_time_mjd (self, UT1R);
    /* Sidereal time of start of current day in julian centuries from
       standard epoch.  */
    double t0 = ((floor (cmjd) - date_time_mjd (standard_epoch, UT1R)) 
		      / one_julian_century);
    /* Current sidereal time in julian centuries from standard
       epoch.  */
    double tu = ((cmjd - date_time_mjd (standard_epoch, UT1R)) 
		      / one_julian_century);
    /* Greenwich mean sidereal time in radians.  */
    double gmst = (6 * SECONDS_PER_HOUR + 41 * SECONDS_PER_MINUTE 
			+ 50.54841 + (1.002737909350795 * (cmjd - floor (cmjd))
				      * SECONDS_PER_DAY)
			+ 8640184.812866 * t0 + 0.093104 * pow (tu, 2.0) 
			- 6.2e-6 * pow (tu, 3.0));
    gmst = 2 * M_PI * fractional_part (gmst / SECONDS_PER_DAY);

    double gast = fmod (gmst 
			+ equation_of_equinoxes (date_time_mjd (self, TDT)),
			2 * M_PI);

    return gast;
  }
  else {
    /* Here we use the new relationship, described in International
       Earth Rotation Service (IERS) section Technical Note No. 32,
       section 5.4.4.  */  
    double tu = date_time_jd (self, UT1R) - 2451545.0;
    return fmod (2 * M_PI * (0.7790572732640 + 1.00273781191135448 * tu),
		 2 * M_PI);
  }
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
  self->mjd += (double) seconds / SECONDS_PER_DAY;
  mjd_to_year_day_second (self->mjd, &(self->year), &(self->day_of_year), 
			  &(self->second_of_day));
}

/* Subtract seconds from an existing DateTime (changing it).  */
void
date_time_subtract_seconds (DateTime *self, double seconds)
{
  self->mjd -= (double) seconds / SECONDS_PER_DAY;
  assert (self->mjd >= 0);
  mjd_to_year_day_second (self->mjd, &(self->year), &(self->day_of_year), 
			  &(self->second_of_day));
}

/* Return true iff self is before other.  */
int
date_time_is_before (DateTime *self, DateTime *other)
{
  return date_time_difference (self, other) < 0.0;
}

/* Return true iff self is after other.  */
int
date_time_is_after (DateTime *self, DateTime *other)
{
  return date_time_difference (self, other) > 0.0;
}

/* Free a dynamicly allocated DateTime object.  */
void
date_time_free (DateTime *self)
{
  free (self);
}
