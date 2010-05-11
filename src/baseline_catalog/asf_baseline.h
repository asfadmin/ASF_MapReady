#ifndef _ASF_BASELINE_H_
#define _ASF_BASELINE_H_

#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <time.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "asf.h"
#include "asf_meta.h"
#include "geolocate.h"
#include "dateUtil.h"
#include "asf_vector.h"
#include "shapefil.h"
#include "gsl/gsl_sort.h"

#define SIZE 500000
#define MAX_ORBITS 100 // in a month

// Definition of orbit range and repeat cycles
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
#define E2_MAX_ORBIT 200000 // still rock and rolling
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


// Structures
struct srf_file {
  char file_name[30];
  char sensor[5];
  char beam_mode[10];
  int orbit;
  int sequence;
  double scan;
  int frames;
};

struct srf_orbit {
  char sensor[10];// name of sensor
  int orbit;      // orbit number
  int seq;        // sequence number
  float off_nadir;// off-nadir angle - only used for PALSAR
  char orbit_dir; // orbit direction
  int frame;      // frame number
  float c_lat;    // center latitude
  float c_lon;    // center longitude
  float ns_lat;   // near start latitude
  float ns_lon;   // near start longitude
  float fs_lat;   // far start latitude
  float fs_lon;   // far start longitude
  float ne_lat;   // near end latitude
  float ne_lon;   // near end longitude
  float fe_lat;   // far end latitude
  float fe_lon;   // far end longitude
  double x;       // state vector position in x
  double y;       // state vector position in y
  double z;       // state vector position in z
  double vx;      // state vector velocity in x
  double vy;      // state vector velocity in y
  double vz;      // state vector velocity in z
  double range;   // slant range to mid pixel
  double doppler; // Doppler frequence (constant term)
  char time[30];  // acquisition time
};

struct base_pair {
  char m_sensor[10]; // sensor of master image
  char s_sensor[10]; // sensor of slave image
  char mode[5];      // beam mode
  float off_nadir;   // off-nadir angle - only used for PALSAR
  int track;         // track number
  int frame;         // frame number
  char orbit_dir[15];// orbit direction
  int master;        // orbit of master image
  int m_seq;         // sequence of master image
  char m_time[30];   // acquisition time of master image
  int slave;         // orbit of slave image
  int s_seq;         // sequence of slave image
  char s_time[30];   // acquisition time of slave image
  int b_par;         // parallel baseline
  int b_perp;        // perpendicular baseline
  int b_temp;        // temporal baseline
  float c_lat;       // center latitude
  float c_lon;       // center longitude
  float ns_lat;      // near start latitude
  float ns_lon;      // near start longitude
  float fs_lat;      // far start latitude
  float fs_lon;      // far start longitude
  float ne_lat;      // near end latitude
  float ne_lon;      // near end longitude
  float fe_lat;      // far end latitude
  float fe_lon;      // far end longitude
};

// Prototypes
void baseline2kml(int ii, struct base_pair *pairs, FILE *fp);
void baseline2shape(int ii, struct base_pair *pairs, 
		    DBFHandle dbase, SHPHandle shape);
void baseline_catalog(char *sensor, char *beam_mode, char *input, 
		      char *output_dir);
void setup_files(char *sensor, char *beam_mode, char *input_dir, 
		 char *output_dir, int *nTracks, int *nFrames);
void read_srf(char *input_dir, char *inFile, 
	      struct srf_orbit **srf_orbits, int *nOrbits);
void read_palsar(char *inFile, struct srf_orbit **srf_orbits, int *nOrbits);
void determine_baseline(char *m_sensor, char *s_sensor, char *mode, int track, 
			int orbit, struct srf_orbit *srf, int nOrbits, 
			struct base_pair **pairs, int *nPairs);
void generate_products(char *output_dir, struct base_pair *pairs, int nPairs);


#endif
