/* Some functions that can manage dates-as-longs  */

int is_leap_year(int year);
int is_valid_date(long l);
void long_to_date(long l, int *y, int *m, int *d);
long date_to_long(int y, int m, int d);
int get_day_of_week(long l);
long subtract_a_day(long l);
int add_a_day(long l);
void date_tester(void);
