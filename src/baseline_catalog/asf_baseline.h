#ifndef _ASF_BASELINE_H_
#define _ASF_BASELINE_H_

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

struct srf_orbit {
  int orbit;
  int seq;
  int index;
  struct srf_orbit *next;
};

struct base_info {
  int orbit;      // orbit number
  int frame;      // frame number
  int seq;        // sequence number (only of interest for ordering)
  char orbit_dir; // orbit direction
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
  struct base_info *next;
};

struct base_pair {
  char sensor[10];   // sensor
  char mode[5];      // beam mode
  int track;         // track number
  int frame;         // frame number
  char orbit_dir[15];// orbit direction
  int master;        // orbit of master image
  int m_seq;         // sequence of master image
  char  m_time[30];  // acquisition time of master image
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
void baseline2kml(struct base_pair *pairs, int nPairs, FILE *fp);
void baseline2shape(struct base_pair *pairs, int nPairs, char *shapeFile);
void baseline_catalog(char *beam_mode);
void filter_srf(char *output, char *sensor, char *mode, int *nFiles);
void find_pairs(int track, char *list, char *pairs_list, int *nPairs);
void read_srf(char *list, int nFiles, char *pairs_list, int nPairs, 
	      char *sensor, char *beam_mode, 
	      struct base_info **srf, struct base_pair **base_pairs);
void determine_baseline(char *sensor, int track, struct base_info *srf, 
			struct base_pair *pairs, int nPairs);
void generate_products(struct base_pair *pairs, int nPairs);

#endif
