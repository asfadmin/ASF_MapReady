#ifndef __LINE_HEADER_H__
#define __LINE_HEADER_H__


typedef struct {
  int line_num;
  int rec_num;
  int n_left_pixel;
  int n_data_pixel;
  int n_right_pixel;
  int sensor_updf;
  int acq_year;
  int acq_day;
  int acq_msec;
  short int sar_chan_id;
  short int sar_chan_code;
  short int tran_polar;
  short int recv_polar;
  int prf;
  int scan_id;
  int sr_first;
  int sr_mid;
  int sr_last;
  int fdc_first;
  int fdc_mid;
  int fdc_last;
  int ka_first;
  int ka_mid;
  int ka_last;
  int nadir_ang;
  int squint_ang;
  int geo_updf;
  int lat_first;
  int lat_mid;
  int lat_last;
  int long_first;
  int long_mid;
  int long_last;
  int north_first;
  int north_last;
  int east_first;
  int east_last;
  int heading;
} alos_processed_line_t;

typedef struct {
  int line_num;
  int rec_num;
  int n_left_pixel;
  int n_data_pixel;
  int n_right_pixel;
  int sensor_updf;
  int acq_year;
  int acq_day;
  int acq_msec;
  short int sar_chan_id;
  short int sar_chan_code;
  short int tran_polar;
  short int recv_polar;
  int prf;
  int sr_first;
  int sr_mid;
  int sr_last;
  int fdc_first;
  int fdc_mid;
  int fdc_last;
  int ka_first;
  int ka_mid;
  int ka_last;
  int nadir_ang;
  int squint_ang;
  int geo_updf;
  int null_f;
  int lat_first;
  int lat_mid;
  int lat_last;
  int long_first;
  int long_mid;
  int long_last;
  int north_first;
  int north_last;
  int east_first;
  int east_last;
  int heading;
} rsat_processed_line_t;

// Prototypes
alos_processed_line_t *read_alos_proc_line_header(const char *inName, 
						  int line_number);
rsat_processed_line_t *read_rsat_proc_line_header(const char *inName,
						  int line_number);

#endif
