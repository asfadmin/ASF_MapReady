/*Calibration header file.
*/
#ifndef _calibrate_h_
#define _calibrate_h_

typedef enum {
  r_AMP=1,
  r_SIGMA,
  r_BETA,
  r_GAMMA,
  r_SIGMA_DB,
  r_BETA_DB,
  r_GAMMA_DB,
  r_POWER
} radiometry_t;

typedef struct {
  double A,B,C,D,E,F,G,H,I,J,K;
} quadratic_2d;

typedef enum {
  asf_cal=0,          // ASF:  a0, a1 and a2 plus noise vector
  asf_scansar_cal=1,  // SCANSAR:  a0, a1 and a2 plus noise vector
  esa_cal=2,          // ESA:  calibration constant K
  rsat_cal=3,         // RSAT: look up table approach
  alos_cal=4          // ALOS: calibration coefficient CF
} cal_type;

typedef struct {
  double a0, a1, a2;  // calibration coefficient
  double noise[256];  // noise values
  int sample_count;
} asf_cal_params;

typedef struct {
  double a0, a1, a2;  // calibration coefficient
  double noise[256];  // noise values
  int tablePix;
  int numLines;
} asf_scansar_cal_params;

typedef struct {
  double k;           // absolute calibration constant
  double ref_incid;   // reference incidence angle
} esa_cal_params;

typedef struct {
  double *lut;        // output scaling look up table
  int n;              // number of table entries
  int samp_inc;       // table entry sampling increment
  double a3;          // linear scaling offset
  int slc;            // flag indicating data is SLC
  int focus;          // flag indicating data processed by FOCUS
} rsat_cal_params;

typedef struct {
  double cf;          // calibration factor
} alos_cal_params;

typedef struct {
  asf_cal_params* asf;
  asf_scansar_cal_params *asf_scansar;
  esa_cal_params* esa;
  rsat_cal_params* rsat;
  alos_cal_params* alos;
  quadratic_2d incid;
  radiometry_t radiometry;
} cal_params;

#endif
