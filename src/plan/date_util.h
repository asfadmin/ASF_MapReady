#include "dateUtil.h"

void parse_date(const char *inStr,ymd_date *date,hms_time *time);
const char *date_str(double s);
double seconds_from_s(const char *date_str);
double seconds_from_l(long date);
