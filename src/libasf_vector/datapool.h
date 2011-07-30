#ifndef _DATAPOOL_H_
#define _DATAPOOL_H_

typedef struct {
  char granule_name[64];
  char platform[64];
  char sensor[64];
  char beam_mode[64];
  char beam_mode_description[255];
  int orbit;
  int path_number;
  int frame_number;
  char acquisition_date[64];
  char processing_date[64];
  char processing_level[10];
  char start_time[64];
  char end_time[64];  
  double center_lat;
  double center_lon;
  double near_start_lat;
  double near_start_lon;
  double far_start_lat;
  double far_start_lon;
  double near_end_lat;
  double near_end_lon;
  double far_end_lat;
  double far_end_lon;
  double faraday_rotation;
  char orbit_direction[32];
  char url[255];
  double size;
  double off_nadir_angle;
} datapool_type_t;

void shape_datapool_init(char *inFile, char *header);

#endif
