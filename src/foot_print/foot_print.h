#ifndef _FOOT_PRINT_H_
#define _FOOT_PRINT_H_

#include "dateUtil.h"
#include "asf_vector.h"
#include "asf_raster.h"
#include "asf.h"
#include "asf_meta.h"
#include "gsl/gsl_sort.h"
#include "stdio.h"

#define PSR_MIN_ORBIT 1633
#define PSR_MAX_ORBIT 200000
#define PSR_ORBITS_PER_CYCLE 671

#define R1_MIN_ORBIT 84
#define R1_MAX_ORBIT 65219
#define R1_ORBITS_PER_CYCLE 343

#define E1_A_MIN_ORBIT 126
#define E1_A_MAX_ORBIT 2103
#define E1_A_ORBITS_PER_CYCLE 43

#define E1_B_MIN_ORBIT 2354
#define E1_B_MAX_ORBIT 3695
#define E1_B_ORBITS_PER_CYCLE 43

#define E1_C_MIN_ORBIT 3901
#define E1_C_MAX_ORBIT 12707
#define E1_C_ORBITS_PER_CYCLE 501

#define E1_D_MIN_ORBIT 12754
#define E1_D_MAX_ORBIT 14300
#define E1_D_ORBITS_PER_CYCLE 43

#define E1_E_MIN_ORBIT 14302
#define E1_E_MAX_ORBIT 16745
#define E1_E_ORBITS_PER_CYCLE 43

#define E1_F_MIN_ORBIT 16747
#define E1_F_MAX_ORBIT 19247
#define E1_F_ORBITS_PER_CYCLE 1784

#define E1_G_MIN_ORBIT 19248
#define E1_G_MAX_ORBIT 45251
#define E1_G_ORBITS_PER_CYCLE 501

#define E2_MIN_ORBIT 202
#define E2_MAX_ORBIT 200000
#define E2_ORBITS_PER_CYCLE 501

#define E1_TANDEM_MIN_ORBIT 19248
#define E1_TANDEM_MAX_ORBIT 45251
#define E1_TANDEM_ORBITS_PER_CYCLE 501

#define E2_TANDEM_MIN_ORBIT 202
#define E2_TANDEM_MAX_ORBIT 25564
#define E2_TANDEM_ORBITS_PER_CYCLE 501

#define J1_MIN_ORBIT 1826
#define J1_MAX_ORBIT 36581
#define J1_ORBITS_PER_CYCLE 659

double fbs_modes[] = { 9.9,14.0,18.0,21.5,25.8,28.8,30.8,34.3,36.9,38.8,41.5,
		       45.2,50.8 };
double fbd_modes[] = { 9.9,21.5,28.8,34.3,36.9,41.5,45.2,50.8 };
double plr_modes[] = { 9.7,13.8,20.5,21.5,23.1,26.2 };
double wb1_modes[] = { 24.6,25.9,27.1 };
double wb2_modes[] = { 27.1 };
#define FBS_COUNT 13
#define FBD_COUNT 8
#define PLR_COUNT 6
#define WB1_COUNT 3
#define WB2_COUNT 1

typedef struct {
  char name[20];
  char satellite[5];
  char beam_mode[5];
  double off_nadir;
  int stack_id;
  int frame_count;
  int track;
  int orbit;
  int frame;
  char date[25];
  char orbit_dir[20];
  int path;
  int terrain; // 1 land, 0 water
  double center_lat;
  double center_lon;
  double near_start_lat;
  double near_start_lon;
  double far_start_lat;
  double far_start_lon;
  double far_end_lat;
  double far_end_lon;
  double near_end_lat;
  double near_end_lon;
  char mission[15];
} granule_t;

typedef struct {
  int frame[7201];
} frame_stack_t;


#endif
