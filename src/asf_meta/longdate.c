#include "asf.h"
#include "dateUtil.h"
#include <time.h>
#include <assert.h>

double seconds_from_long(long date)
{
  ymd_date d;
  long_to_date(date, &d.year, &d.month, &d.day);

  julian_date jd;
  date_ymd2jd(&d, &jd);

  hms_time t;
  t.hour = 0;
  t.min = 0;
  t.sec = 0.001;

  return date2sec(&jd, &t);
}

int is_leap_year(int year)
{
    return (!(year % 4) && (year % 100)) || !(year % 400);
}

int is_valid_date(long l)
{
    char year[32], month[32], day[32];
    sprintf(year, "%ld", l);
    if (strlen(year) != 8)
        return FALSE;

    strcpy(month, year+4);
    strcpy(day, month+2);
    year[4] = '\0';
    month[2] = '\0';

    int y = atoi(year);
    int m = atoi(month);
    int d = atoi(day);

    if (y >= 1900 && y <= 9999 &&  // year between 1900 and 9999
        m >= 1    && m <= 12   &&  // month between 1 and 12
        d >= 1    && d <= 31)      // day between 1 and 31
    {
        // now check month/day combo
        if (m==2 && d>29)
            return FALSE;

        if (m==2 && d==29 && !is_leap_year(y))
            return FALSE;

        if (d==31 && (m==4 || m==6 || m==9 || m==11))
            return FALSE;

        // all tests passed
        return TRUE;
    }

    // failed
    return FALSE;
}

long current_date()
{
    time_t t = time(NULL);
    struct tm *ts = localtime(&t);
    return date_to_long(ts->tm_year+1900, ts->tm_mon+1, ts->tm_mday);
}

long add_days(long l, int d)
{
    // slow implementation for now
    long ret = l;
    while (d-->0)
        ret = add_a_day(ret);
    return ret;
}

void long_to_date(long l, int *y, int *m, int *d)
{
    char year[32], month[32], day[32];
    sprintf(year, "%ld", l);
    if (strlen(year) != 8) {
        *y = *m = *d = -1;
        return;
    }

    strcpy(month, year+4);
    strcpy(day, month+2);
    year[4] = '\0';
    month[2] = '\0';

    *y = atoi(year);
    *m = atoi(month);
    *d = atoi(day);
}

long date_to_long(int y, int m, int d)
{
    char tmp[32];
    sprintf(tmp, "%04d%02d%02d", y, m, d);
    long l = atol(tmp);
    assert(is_valid_date(l));
    return l;
}

int get_day_of_week(long l)
{
    if (is_valid_date(l)) {
        int y,m,d;
        long_to_date(l, &y, &m, &d);

        if (m < 3) {
            m += 12;
            y -= 1;
        }
        return (d+2*m+(int)(6*(m+1)/10)+y+(int)(y/4)-(int)(y/100)+(int)(y/400)+1
) % 7;
    }
    printf("Invalid date: %ld\n", l);
    assert(0);
    return -1;
}

long subtract_a_day(long l)
{
    int y, m, d;
    long_to_date(l, &y, &m, &d);

    --d;

    // check if went back to previous month
    if (d < 1) {
        switch (m) {
            case 1:  // have to go to previous year
                m=12;
                d=31;
                --y;
                break;

            case 2:  // previous month has 31 days
            case 4:
            case 6:
            case 8:
            case 9:
            case 11:
                --m;
                d=31;
                break;

            case 5: // previous month has 30 days
            case 7:
            case 10:
            case 12:
                --m;
                d=30;
                break;

            case 3:  // previous month is Feb, yargh
                m=2;
                d = is_leap_year(y) ? 29 : 28;
                break;

            default:
                assert(0);
                break;
        }
    }

    long l2 = date_to_long(y,m,d);
    assert(is_valid_date(l2));
    return l2;
}

long add_a_day(long l)
{
    int y, m, d;
    long_to_date(l, &y, &m, &d);

    ++d;

    // check if went to next month
    if (d > 28) {
        switch (m) {
            case 12:  // have to go to next year?
                if (d==32) {
                    m=d=1;
                    ++y;
                }
                break;

            case 2:  // special code for Feb
                if (( is_leap_year(y) && d==30) ||
                    (!is_leap_year(y) && d==29)) {
                    m=3; d=1;
                }
                break;

            case 1: // month has 31 days
            case 3:
            case 5:
            case 7:
            case 8:
            case 10:
                if (d==32) {
                    ++m;
                    d=1;
                }
                break;

            case 4:  // month has 30 days
            case 6:
            case 9:
            case 11:
                if (d==31) {
                    ++m;
                    d=1;
                }
                break;

            default:
                assert(0);
                break;
        }
    }

    long l2 = date_to_long(y,m,d);
    assert(is_valid_date(l2));
    return l2;
}

int date_diff(long date1, long date2)
{
  long startdate = date1;
  long enddate = date2;
  if (date1 > date2) {
    startdate = date2;
    enddate = date1;
  }
  int ndays = 0;
  while (startdate < enddate) {
    startdate = add_a_day(startdate);
    ++ndays;
  }

  return ndays;
}

