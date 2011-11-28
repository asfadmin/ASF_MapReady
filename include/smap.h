#ifndef __SMAP_H__
#define __SMAP_H__

#include "asf_meta.h"

// SMAP meta structure
typedef struct {
  char algorithm_descriptor[2048];
  char collection_description[2048];
  int east_bounding_coordinate;
  char equator_crossing_date[10];
  float equator_crossing_longitude;
  char equator_crossing_time[13];
  char granule_name[256];
  float grid_spacing;
  char input_name[256];
  int north_bounding_coordinate;
  char orbit_direction[12];
  float orbit_inclination;
  float orbit_period;
  char orbit_start_date[10];
  char orbit_start_time[13];
  char orbit_stop_date[10];
  char orbit_stop_time[13];
  char production_date_time[24];
  char project_id[16];
  float radar_resolution;
  char range_beginning_date[10];
  char range_beginning_time[13];
  char range_ending_date[10];
  char range_ending_time[13];
  char short_name[16];
  int south_bounding_coordinate;
  int start_orbit_number;
  int stop_orbit_number;
  int version_id;
  int west_bounding_coordinate;
} smap_meta;


// Function prototypes
meta_parameters* smap2meta(smap_meta *smap);
smap_meta *read_smap_meta(const char *dataFile);

#endif
