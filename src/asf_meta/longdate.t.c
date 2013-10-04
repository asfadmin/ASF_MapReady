#include "CUnit/Basic.h"
#include "dateUtil.h"

void test_longdate()
{
    int i;
    long l;

    // normal tests
    CU_ASSERT(is_valid_date(20070101));
    CU_ASSERT(is_valid_date(20080202));
    CU_ASSERT(is_valid_date(20090303));
    CU_ASSERT(is_valid_date(20100404));
    CU_ASSERT(is_valid_date(20110505));
    CU_ASSERT(is_valid_date(20121212));
    CU_ASSERT(!is_valid_date(20071301));
    CU_ASSERT(!is_valid_date(20071232));
    CU_ASSERT(!is_valid_date(20081232));
    CU_ASSERT(!is_valid_date(20110030));
    CU_ASSERT(!is_valid_date(20070631));
    CU_ASSERT(!is_valid_date(20071131));
    CU_ASSERT(!is_valid_date(20070431));
    CU_ASSERT(!is_valid_date(20290631));
    CU_ASSERT(!is_valid_date(99990931));
    CU_ASSERT(!is_valid_date(59990931));
    CU_ASSERT(is_valid_date(20080229));
    CU_ASSERT(!is_valid_date(20090229));
    CU_ASSERT(!is_valid_date(20100229));
    CU_ASSERT(!is_valid_date(20110229));
    CU_ASSERT(is_valid_date(20120229));
    CU_ASSERT(!is_valid_date(23000229));
    CU_ASSERT(is_valid_date(24000229));

    CU_ASSERT(get_day_of_week(20070611)==1); // today, 6/11/07, is Monday
    CU_ASSERT(get_day_of_week(20070612)==2); // tomorrow is Tuesday
    CU_ASSERT(get_day_of_week(20070610)==0); // yesterday was Sunday
    CU_ASSERT(get_day_of_week(20070613)==3); // 13th is Wednesday
    CU_ASSERT(get_day_of_week(20080101)==2); // new years 2008 is Tuesday
    CU_ASSERT(get_day_of_week(20080229)==5); // leap day 2008 is Friday
    CU_ASSERT(get_day_of_week(20080125)==5); // my birthday next year is Friday

    CU_ASSERT(add_a_day(20071231)==20080101);
    CU_ASSERT(subtract_a_day(20080101)==20071231);

    int do_all_tests=0;
    if (do_all_tests) {
      for (l=20070101; l<23991230; l=add_a_day(l))
        CU_ASSERT(subtract_a_day(add_a_day(l))==l);
      for (l=99070101; l<99991230; l=add_a_day(l))
        CU_ASSERT(add_a_day(subtract_a_day(l))==l);

      l = 20070101;
      for (i=0; i<366*2000; ++i) {
        int dow1 = get_day_of_week(l);
        l = add_a_day(l);
        CU_ASSERT(is_valid_date(l));
        int dow2 = get_day_of_week(l);
        CU_ASSERT((dow1+1)%7 == dow2%7);
        int y,m,d;
        long_to_date(l,&y,&m,&d);
        CU_ASSERT(l==date_to_long(y,m,d));
      }
      for (i=0; i<366*2000; ++i) {
        int dow1 = get_day_of_week(l);
        l = subtract_a_day(l);
        int dow2 = get_day_of_week(l);
        CU_ASSERT((dow2+1)%7 == dow1%7);
      }
    }
    else {
      for (l=20061231; l<20220101; l=add_days(l,33)) {
        CU_ASSERT(subtract_a_day(add_a_day(l))==l);
        int dow1 = get_day_of_week(l);
        long l2 = add_a_day(l);
        CU_ASSERT(is_valid_date(l2));
        int dow2 = get_day_of_week(l2);
        CU_ASSERT((dow1+1)%7 == dow2%7);
        int y,m,d;
        long_to_date(l,&y,&m,&d);
        CU_ASSERT(l==date_to_long(y,m,d));
      }
    }

    int y2, m2, d2; 
    l = 20070101;
    long_to_date(l, &y2, &m2, &d2);
    CU_ASSERT(y2==2007);
    CU_ASSERT(m2==1);
    CU_ASSERT(d2==1);
    l = subtract_a_day(l);
    long_to_date(l, &y2, &m2, &d2);
    CU_ASSERT(y2==2006);
    CU_ASSERT(m2==12);
    CU_ASSERT(d2==31);
    y2 = 2000; m2 = 2; d2 = 29;
    l = date_to_long(y2,m2,d2);
    CU_ASSERT(l==20000229);
    CU_ASSERT(is_valid_date(l));
    l = add_a_day(l);
    CU_ASSERT(is_valid_date(l));
    long_to_date(l, &y2, &m2, &d2);
    CU_ASSERT(y2==2000);
    CU_ASSERT(m2==3);
    CU_ASSERT(d2==1);
}

