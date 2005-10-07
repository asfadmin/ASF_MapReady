/* An exact Terrestrial Time (TT) (formerly Terrestrial Dynamical Time
   (TDT)) date and time class.  TDT relates to other prominent time
   scales as follows:

        TDT = TAI + 32.184 s
	UTC = TAI - n * 1 s
	|UT1 - UTC| <= 0.9 s

   where UTC is the Uniform Time Coordinated, TAI is the Temps
   Atomique International (atomic time), UT1 is universal time, and n
   is the number of leap seconds in effect at a given TDT.  TDT, TAI,
   and UTC are uniform time scales which do not correspond perfectly
   to the actual orientation of the conventional terrestrial system
   with respect to space.  UTC is kept within 0.9 seconds of UT1 by
   the occasional insertion or removal of leap seconds.  However, 0.9
   s is a long time when satellite position is the dependent parameter
   of interest.  Accurately converting between UTC and UT1 and the
   corresponding Greenwitch Mean Sidereal Time (GMST) or hour angle
   requires knowledge of |UT1 - UTC| for the time in question.  This
   data is provided by the International Earth Rotation Service (IERS)
   as a monthly bulletin known as IERS Bulletin B.  IERS Bulletin B
   also provides offsets to a time referred to as UT1R, which is a
   regularized (i.e. high pass filtered) form of UT1 (it ignores the
   effects of body tides, which may amount to as much as 2.5 ms worth
   of time).  Data file archives of existing IERS Bulletin B issues
   are used by these routines for dates in the past with respect to
   the development of this library.  For future dates, the library
   refuses to operate.  A full description of these time issues
   appears in "Satellite Geodesy, 2nd Edition", by Gunter Seeber.
   However, his description of the time signal DUT1 = UT1 - UTC,
   avalable in IERS Bulletin D, is inadequate.  DUT1 as distributed in
   IERS Bulletin D is by definition only accurate to within 0.1 s,
   which is not accurate enough for satellite geodesy.

   This class supports creation and interpretation in a variety of
   units, and some simple date arithmetic.  */

#ifndef DATE_TIME_H
#define DATE_TIME_H

/* The algorithms used seem to give at least this much precision,
   i.e. the test comparisons pass with this value as the tolerance
   after a single conversion or date arithmetic operation.  */
// FIXME: I swear the tests worked with 1.0e-21 in here on solaris,
// and I thought double was supposed to be the same IEEE standard
// thing on all platforms.  Also, it seems the value that ends up in
// the DateTime structure is not the same as the second of day
// argument that is passed into date_time_new, and nothing happens but
// an assignment.  I don't know if this is gcc bug or AMD FPU problem
// or what.
#define TIME_PRECISION_IN_SECONDS (1.0e-8L)
#define SECONDS_PER_MINUTE 60
#define MINUTES_PER_HOUR 60
#define SECONDS_PER_HOUR (SECONDS_PER_MINUTE * MINUTES_PER_HOUR)
#define HOURS_PER_DAY 24
#define SECONDS_PER_DAY (SECONDS_PER_HOUR * HOURS_PER_DAY)
/* Modified julian day zero is julian day MJD_OFFSET.  */
#define MJD_OFFSET ( (double) 2400000.5)
/* Julian date for science day zero is julian day JDS_OFFSET.  */
#define JDS_OFFSET ( (double) 2436099.5)

/* These class data members may be read directly from the structure,
   but should not be modified or created except with the interface
   functions.  Note that the values stored in the structure are always
   TDT times, no matter which time scale was used to create the
   object.  To get the time in other scales use the accessor
   functions.  */
typedef struct {
  int year;
  int day_of_year;		/* Counting from 1.  */
  double second_of_day;
  /* Equivalent time in modified julian days.  */
  double mjd;		
} DateTime;

/* All the member functions that take or return a time include an
   argument of type time_scale_t which specifies time scale in which
   the time is supplied or in which it is to be returned.  */
typedef enum {
  TDT,				/* Terrestrial dynamical time.  */
  UT1R,				/* Universal time. */
  UTC				/* Universal time coordinated.  */
} time_scale_t;

/* Create a new DateTime object from a TDT year, day of year, and
   second of day.  */
DateTime *
date_time_new (int year, int day_of_year, double second_of_day, 
	       time_scale_t time_scale);

/* Create a new DateTime object from a modified julian day.  Modified
   julian days begin at midnight November 17, 1858.  */
DateTime *
date_time_new_from_mjd (double mjd, time_scale_t time_scale);

/* Create a new DateTime object from a year, month, day, and second of
   day.  */
DateTime *
date_time_new_from_ymds (int year, int month, int day, double second_of_day,
			 time_scale_t time_scale);

/* Create a new DateTime object from a julian day.  Julian days begin
   at noon January 1, 4713 BC (making noon January 2, 4713 BC julian
   day number 1.0).  */
DateTime *
date_time_new_from_jd (double jd, time_scale_t time_scale);

/* Create a new DateTime object from a so-called "Julian Date for
   Space".  This system starts at midnight on September 17, 1957.  I
   haven't seen it used much at all, its here mainly so its absence
   doesn't confuse the people who have heard of it and not of modified
   julian days into thinking modified julian days are what they
   want.  */
DateTime *
date_time_new_from_jds (double jds, time_scale_t time_scale);

/* Create a new independent DateTime object from an existing model.  */
DateTime *
date_time_copy (DateTime *self);

/* Get year in which self falls.  */
int
date_time_year (DateTime *self, time_scale_t time_scale);

/* Get day of year.  */
int 
date_time_day_of_year (DateTime *self, time_scale_t time_scale);

/* Get second of day.  */
double
date_time_second_of_day (DateTime *self, time_scale_t time_scale);

/* Get month of the year.  */
int
date_time_month (DateTime *self, time_scale_t time_scale);

/* Get day of the month.  */
int
date_time_day_of_month (DateTime *self, time_scale_t time_scale);

/* Get hour of day.  */
int
date_time_hour_of_day (DateTime *self, time_scale_t time_scale);

/* Get minute of hour.  */
int
date_time_minute_of_hour (DateTime *self, time_scale_t time_scale);

/* Get second of minute.  */
double
date_time_second_of_minute (DateTime *self, time_scale_t time_scale);

/* Get time in modified julian days.  */
double
date_time_mjd (DateTime *self, time_scale_t time_scale);

/* Get time in julian days.  */
double
date_time_jd (DateTime *self, time_scale_t time_scale);

/* Get time as a so-called "Julian Date for Space".  */
double
date_time_jds (DateTime *self, time_scale_t time_scale);

/* Get the rotation angle in radians of the earth relative to the
   International Celestial Reference System (ICRS).  */
double
date_time_earth_angle (DateTime *self);

/* Find the time difference from other to self, in seconds.  */
double
date_time_difference (DateTime *self, DateTime *other);

/* Add seconds to an existing DateTime (changing it).  There is a real
   possibility of error buildup if small quantities of time are
   repeatedly added.  */
void
date_time_add_seconds (DateTime *self, double seconds);

/* Subtract seconds from an existing DateTime (changing it).  The same
   caveat regarding error buildup apply to this method as to the
   date_time_add_seconds method.  */
void
date_time_subtract_seconds (DateTime *self, double seconds);

/* Return true iff self is before other.  */
int
date_time_is_before (DateTime *self, DateTime *other);

/* Return true iff self is after other.  */
int
date_time_is_after (DateTime *self, DateTime *other);

/* Free DateTime object.  */
void
date_time_free (DateTime *self);

#endif /* DATE_TIME_H */
