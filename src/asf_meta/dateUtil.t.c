#include "CUnit/Basic.h"
#include "dateUtil.h"

static void test_apr(ymd_date *ymd, hms_time *hms, int d)
{
//printf("--> %d %d %d %d %d %f\n",
//       ymd->year, ymd->month, ymd->day, hms->hour, hms->min, hms->sec);
  CU_ASSERT(ymd->year==2013);
  CU_ASSERT(ymd->month==4);
  CU_ASSERT(ymd->day==d);
  CU_ASSERT(hms->hour==12);
  CU_ASSERT(hms->min==34);
  CU_ASSERT(fabs(hms->sec-56.789)<.0001);

  julian_date jd;
  date_ymd2jd(ymd,&jd);
  CU_ASSERT(jd.year==ymd->year);
  CU_ASSERT(jd.jd==d+90);

  ymd_date ymd2;
  hms_time hms2=*hms;
  date_jd2ymd(&jd,&ymd2);
  CU_ASSERT(ymd->year==ymd2.year);
  CU_ASSERT(ymd->month==ymd2.month);
  CU_ASSERT(ymd->day==ymd2.day);

  add_time(24*60*60,&ymd2,&hms2);
  CU_ASSERT(ymd2.year==2013);
  CU_ASSERT(ymd2.month==4);
  CU_ASSERT(ymd2.day==d+1);
  CU_ASSERT(hms2.hour==12);
  CU_ASSERT(hms2.min==34);
  CU_ASSERT(fabs(hms2.sec-56.789)<.0001);
 
  double interval = 2*(24*60*60*365) + 5*(24*60*60) + 8*(60*60) + 3*(60) + 1.1;
  add_time(interval,&ymd2,&hms2);
  CU_ASSERT(ymd2.year==2015);
  CU_ASSERT(ymd2.month==4);
  CU_ASSERT(ymd2.day==d+6);
  CU_ASSERT(hms2.hour==20);
  CU_ASSERT(hms2.min==37);
  CU_ASSERT(fabs(hms2.sec-57.889)<.0001);

  CU_ASSERT(compare_time(&ymd2,&hms2,ymd,hms)==1);
  CU_ASSERT(compare_time(ymd,hms,&ymd2,&hms2)==-1);
  CU_ASSERT(compare_time(&ymd2,&hms2,&ymd2,&hms2)==0);

  ymd2=*ymd;
  hms2=*hms;

  hms2.sec+=.01;
  CU_ASSERT(compare_time(&ymd2,&hms2,ymd,hms)==1);
  hms2.sec-=.02;
  CU_ASSERT(compare_time(&ymd2,&hms2,ymd,hms)==-1);
  ymd2.year+=1;
  CU_ASSERT(compare_time(&ymd2,&hms2,ymd,hms)==1);
  ymd2.year-=1;
  CU_ASSERT(compare_time(&ymd2,&hms2,ymd,hms)==-1);

  ymd2=*ymd;
  hms2=*hms;
  
  sub_time(interval,&ymd2,&hms2);
  CU_ASSERT(ymd2.year==2011);
  CU_ASSERT(ymd2.month==4);
  CU_ASSERT(ymd2.day==d-4); // do not subtract 5 because 2012 was a leap year
  CU_ASSERT(hms2.hour==4);
  CU_ASSERT(hms2.min==31);
  CU_ASSERT(fabs(hms2.sec-55.689)<.0001);

  date_ymd2jd(&ymd2,&jd);
  double t1 = date2sec(&jd,&hms2);
  date_ymd2jd(ymd,&jd);
  double t2 = date2sec(&jd,hms);
  CU_ASSERT(fabs(t2-t1-interval)<.0001);

  add_time(interval,&ymd2,&hms2);
  CU_ASSERT(ymd2.year==ymd->year);
  CU_ASSERT(ymd2.month==ymd->month);
  CU_ASSERT(ymd2.day==ymd->day);
  CU_ASSERT(hms2.hour==hms->hour);
  CU_ASSERT(hms2.min==hms->min);
  CU_ASSERT(fabs(hms2.sec-hms->sec)<.0001);

//printf("%s\n",buf); //08-Apr-2013, 12:34:56
  char buf[100];
  ymd_date ymd3;
  hms_time hms3;
  date_dssr2time_stamp(&ymd2,&hms2,buf);
  parse_date(buf,&ymd3,&hms3);
  CU_ASSERT(ymd2.year==ymd3.year);
  CU_ASSERT(ymd2.month==ymd3.month);
  CU_ASSERT(ymd2.day==ymd3.day);
  CU_ASSERT(hms2.hour==hms3.hour);
  CU_ASSERT(hms2.min==hms3.min);
  CU_ASSERT((int)floor(hms2.sec)==(int)hms3.sec);

}

void test_date()
{
  int i;
  for (i=1978; i<2020; ++i)
    CU_ASSERT(date_getDaysInYear(i)==(i%4==0?366:365));

  ymd_date ymd;
  hms_time hms;
  julian_date jd;

//printf("oldTime\n");
  parse_odlTime("2013-097T12:34:56.789", &ymd, &hms);
  test_apr(&ymd, &hms, 7);

//printf("DMYdate\n");
  parse_DMYdate("08-APR-2013, 12:34:56", &ymd, &hms);
  add_time(.789,&ymd,&hms);
  test_apr(&ymd, &hms, 8);

//printf("refTime\n");
  parse_refTime("2013099123456789",&jd,&hms);
  date_jd2ymd(&jd,&ymd);
  test_apr(&ymd, &hms, 9);

//printf("ymdTime\n");
  parse_ymdTime("20130410123456789",&ymd,&hms);
  test_apr(&ymd, &hms, 10);

//printf("dssr2date\n");
  date_dssr2date("20130418123456789",&ymd,&hms);
  test_apr(&ymd, &hms, 18);

//printf("dssr2time\n");
  date_dssr2time("19-apr-2013 12:34:56.789",&ymd,&hms);
  test_apr(&ymd, &hms, 19);

//printf("alos2date\n");
  date_alos2date("20130420 12:34:56.789",&ymd,&hms);
  test_apr(&ymd, &hms, 20);

//printf("terrasar2date\n");
  date_terrasar2date("2013-04-21T12:34:56.789000T",&ymd,&hms);
  test_apr(&ymd, &hms, 21);

//printf("sirc2date\n");
  date_sirc2date("2013/04/22 12:34:56.789",&ymd,&hms);
  test_apr(&ymd, &hms, 22);

//printf("ppr2date\n");
  date_ppr2date("2013-113-12:34:56.789",&jd,&hms);
  date_jd2ymd(&jd,&ymd);
  test_apr(&ymd, &hms, 23);

//printf("parse_date\n");
  parse_date("06-APR-2013, 12:34:56",&ymd,&hms);
  add_time(.789,&ymd,&hms);
  test_apr(&ymd, &hms, 6);

//printf("ursa2date\n");
  ursa2date("APR-24-2013 12:34:56",&ymd,&hms);
  add_time(.789,&ymd,&hms);
  test_apr(&ymd, &hms, 24);

  char buf[100];
  date_printDate(&ymd,'-',buf);
  CU_ASSERT(strcmp(buf,"2013-04-24")==0);
  date_printY2Kdate(&ymd,'/',buf);
  CU_ASSERT(strcmp(buf,"13/04/24")==0);
}
