#ifndef __SMAP_H__
#define __SMAP_H__

#include "asf_meta.h"

// SMAP meta structure
typedef struct {
	// dataset identification
  char file_name[256];
  char short_name[25];
  char creation_date_time[30];
    
  // extent
  double north_bounding_coordinate;
  double south_bounding_coordinate;
  double east_bounding_coordinate;
  double west_bounding_coordinate;
  char range_beginning_date_time[30];
  char range_ending_date_time[30];

	// orbital information
	double epoch;
	double argument_of_perigee;
  char equator_crossing_date_time[30];
  double equator_crossing_longitude;
  double inclination;
  double mean_motion;
  double right_ascension_ascending_node;
  double orbit_period;
  double eccentricity;
  char orbit_direction[15];
  char orbit_start_date_time[30];
  char orbit_stop_date_time[30];

} smap_meta;


// Function prototypes
meta_parameters* smap2meta(smap_meta *smap);
smap_meta *read_smap_meta(const char *dataFile);

#endif
