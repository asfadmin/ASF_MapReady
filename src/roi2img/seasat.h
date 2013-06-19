
#define VERSION 1.0
#define	FRAME_LEN	 147.5
#define INT_FRAME_LEN	 148
#define SAMPLES_PER_LINE 13680		/* decoded samples per output line */
#define SAMPLES_PER_FRAME 228		/* encoded samples per minor frame - this should be calculated? */
#define BIT_ERRORS	 7		/* number of allowable bit errors in the sync code */
#define MAX_CONTIGUOUS_MISSES 60	/* number of allowable fill data frames before the end of a dataset */

#define MAX_CLOCK_DRIFT  4097   /* 12 bit field */

#include "seasat_slant_shift.h"

typedef struct {
    int year;/*Gregorian year (e.g. 1998)*/
    int jd;/*Julian day of year (e.g. 33, for February 2nd.)*/
} julian_date;

typedef struct {
    int year;/*Gregorian year (e.g. 1998)*/
    int month;/*1-based month of year (e.g. 3 for March).*/
    int day;/*1-based day of month.*/
} ymd_date;

typedef struct {
    int hour;/*Military-time hour of day (e.g. 00 for midnight hour.)*/
    int min;/*Minute of hour, from 0 to 59*/
    double sec;/*Second of minute, from 0.000... to 59.999...*/
} hms_time;

/*********************************************
Calender/Date Conversion utilities:*/
/*Get number of days in given Gregorian Year.*/
int date_getDaysInYear(int year);

/*Get month/day given year and julian day of year; vice versa.
 ------------------------------------------------------------*/
void date_jd2ymd(julian_date *in,ymd_date *out);
void date_ymd2jd(ymd_date *in,julian_date *out);

/*Convert hour/minute/second to seconds in day; vice versa.
 ---------------------------------------------------------*/
double date_hms2sec(hms_time *in);
void date_sec2hms(double sec,hms_time *out);




/***************************************************************************************
  Seasat Raw Data Format Structures...

	First 3 bytes are the sync code
	Next 8 bits are always the fill flag/frame #  -- SEASAT_aux
	Bits 33 - 40 vary for the first 10 frames     -- SEASAY_aux_# where # = {0..9}
***************************************************************************************/
	
typedef struct {
    unsigned char fill_flag:1;
    unsigned char frame_no:7;
} SEASAT_aux;

typedef struct {
    unsigned char  lsd_year:4;
    unsigned char  station:4;
} SEASAT_aux_0;

typedef struct {
    unsigned char  msec0:8;
} SEASAT_aux_1;

typedef struct {
    unsigned char  msec8:8;
} SEASAT_aux_2;

typedef struct {
    unsigned char  msec16:8;
} SEASAT_aux_3;

typedef struct {
    unsigned char  day_of_year0:5;
    unsigned char  msec24:3;
} SEASAT_aux_4;

typedef struct {
    unsigned char  clock_drift0:4;
    unsigned char  day_of_year5:4;
} SEASAT_aux_5;

typedef struct {
    unsigned char  clock_drift4:8;
} SEASAT_aux_6;

typedef struct {
    unsigned char  scan_indicator:1;
    unsigned char  bits_per_sample:3;
    unsigned char  mfr_lock:1;
    unsigned char  prf_code:3;
} SEASAT_aux_7;

typedef struct {
    unsigned char  delay_10:4;
    unsigned char  delay_1:4;
} SEASAT_aux_8;

typedef struct {
    unsigned char scu_bit:1;
    unsigned char sdf_bit:1;
    unsigned char adc_bit:1;
    unsigned char time_gate_bit:1;
    unsigned char local_prf_bit:1;
    unsigned char auto_prf_bit:1;
    unsigned char prf_lock_bit:1;
    unsigned char local_delay_bit:1;
} SEASAT_aux_9;

typedef struct {
	SEASAT_aux_0	*aux0;
	SEASAT_aux_1	*aux1;
	SEASAT_aux_2	*aux2;
	SEASAT_aux_3	*aux3;
	SEASAT_aux_4	*aux4;
	SEASAT_aux_5	*aux5;
	SEASAT_aux_6	*aux6;
	SEASAT_aux_7	*aux7;
	SEASAT_aux_8	*aux8;
	SEASAT_aux_9	*aux9;
} SEASAT_raw_header;

/***************************************************************************************
  Seasat Formatted Data Structures...
***************************************************************************************/

typedef struct {
	unsigned char     lsd_year;
	unsigned char     station_code;
	unsigned long int msec;
	unsigned int      day_of_year;
	unsigned int      clock_drift;  
	unsigned char     no_scan_indicator_bit;
	unsigned char     bits_per_sample;
	unsigned char     mfr_lock_bit;
	unsigned char     prf_rate_code;
	unsigned char     delay;
	unsigned char     scu_bit;
	unsigned char     sdf_bit;
	unsigned char     adc_bit;
	unsigned char     time_gate_bit;
	unsigned char     local_prf_bit;
	unsigned char     auto_prf_bit;
	unsigned char     prf_lock_bit;
	unsigned char     local_delay_bit;
}  SEASAT_header;

int find_sync(FILE *fp);
int find_sync_no_advance(FILE *fp, int *be);
int find_unaligned_sync_no_advance(FILE *fp, int *be);
int find_one_sync(FILE *fp);
int bit_errors(unsigned char ref, unsigned char pat);
void shift_buffer(unsigned char *buf, unsigned char first_val);
void display_aux(SEASAT_raw_header *r, int field);
void decode_raw(SEASAT_raw_header *r, SEASAT_header *s);
void display_decoded_header(int major_cnt, long int this_sync, SEASAT_header *s, int found_cnt);
void print_decoded_header(char *outheadername,int major_cnt,long int this_sync,
			SEASAT_header *s,int found_cnt, int which);
int get_next_frameno(FILE *fpin, int aligned);
void create_input_tle_file(julian_date target_date,hms_time target_time,const char *ofile);
int time2rev(julian_date target_date,hms_time target_time);
void propagate_state_vector(const char* infile);
void decode_headers(SEASAT_raw_header *r, unsigned char *buf, int *header);
int decode_payload(unsigned char *buf, unsigned char *obuff, int *optr);
void fix_state_vectors(int year, int julianDay, int hour, int min, double sec);
void dump_all_headers(FILE *fp_all_hdrs,int major_cnt,long int major_sync_loc,SEASAT_header *s);





