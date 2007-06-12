#include "date.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

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

    if (y >= 2007 && y <= 9999 &&  // year between 2007 and 9999
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
        return (d+2*m+(int)(6*(m+1)/10)+y+(int)(y/4)-(int)(y/100)+(int)(y/400)+1) % 7;
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

int add_a_day(long l)
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

void date_tester()
{
    int i;
    long l;

    // normal tests
    assert(is_valid_date(20070101));
    assert(is_valid_date(20080202));
    assert(is_valid_date(20090303));
    assert(is_valid_date(20100404));
    assert(is_valid_date(20110505));
    assert(is_valid_date(20121212));
    assert(!is_valid_date(20071301));
    assert(!is_valid_date(20071232));
    assert(!is_valid_date(20081232));
    assert(!is_valid_date(20110030));
    assert(!is_valid_date(20070631));
    assert(!is_valid_date(20071131));
    assert(!is_valid_date(20070431));
    assert(!is_valid_date(20290631));
    assert(!is_valid_date(99990931));
    assert(!is_valid_date(59990931));
    assert(is_valid_date(20080229));
    assert(!is_valid_date(20090229));
    assert(!is_valid_date(20100229));
    assert(!is_valid_date(20110229));
    assert(is_valid_date(20120229));
    assert(!is_valid_date(23000229));
    assert(is_valid_date(24000229));

    assert(get_day_of_week(20070611)==1); // today, 6/11/07, is Monday
    assert(get_day_of_week(20070612)==2); // tomorrow is Tuesday
    assert(get_day_of_week(20070610)==0); // yesterday was Sunday
    assert(get_day_of_week(20070613)==3); // 13th is Wednesday
    assert(get_day_of_week(20080101)==2); // new years 2008 is Tuesday
    assert(get_day_of_week(20080229)==5); // leap day 2008 is Friday
    assert(get_day_of_week(20080125)==5); // my birthday next year is Friday

    assert(add_a_day(20071231)==20080101);
    assert(subtract_a_day(20080101)==20071231);

    for (l=20070101; l<23991230; l=add_a_day(l))
        assert(subtract_a_day(add_a_day(l))==l);
    for (l=99070101; l<99991230; l=add_a_day(l))
        assert(add_a_day(subtract_a_day(l))==l);

    l = 20070101;
    for (i=0; i<366*2000; ++i) {
        int dow1 = get_day_of_week(l);
        l = add_a_day(l);
        assert(is_valid_date(l));
        int dow2 = get_day_of_week(l);
        assert((dow1+1)%7 == dow2%7);
        int y,m,d;
        long_to_date(l,&y,&m,&d);
        assert(l==date_to_long(y,m,d));
    }
    for (i=0; i<366*2000; ++i) {
        int dow1 = get_day_of_week(l);
        l = subtract_a_day(l);
        int dow2 = get_day_of_week(l);
        assert((dow2+1)%7 == dow1%7);
    }
    
    int y2, m2, d2;
    long_to_date(l, &y2, &m2, &d2);
    assert(y2==2007);
    assert(m2==1);
    assert(d2==1);
}
