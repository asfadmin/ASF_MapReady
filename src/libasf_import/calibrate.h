/*Calibration header file.
*/
#ifndef _calibrate_h_
#define _calibrate_h_

#include "asf_meta.h"
#include "asf_import.h"

typedef struct {
  double A,B,C,D,E,F,G,H,I,J,K;
} quadratic_2d;

typedef enum {
  asf_cal=0,  // ASF:  a0, a1 and a2 plus noise vector
  esa_cal=1,  // ESA:  calibration constant K
  rsat_cal=2, // RSAT: look up table approach
  alos_cal=3  // ALOS: calibration coefficient CF
} cal_type;

typedef struct {
  double a0, a1, a2;  // calibration coefficient
  double noise[512];  // noise values
  int tablePix;
  int numLines;
} asf_cal_params;

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
  esa_cal_params* esa;
  rsat_cal_params* rsat;
  alos_cal_params* alos;
  quadratic_2d incid;
  radiometry_t radiometry;
} cal_params;

// cal_params.c
quadratic_2d get_incid(char *sarName, meta_parameters *meta);
cal_params *create_cal_params(const char *inSAR, meta_parameters *meta);
float get_cal_dn(cal_params *cal, int line, int sample, float inDn, 
		 int dbFlag);
quadratic_2d find_quadratic(const double *out, const double *x,
                            const double *y, int numPts);

#endif
