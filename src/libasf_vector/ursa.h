#ifndef _URSA_H_
#define _URSA_H_

typedef struct {
  char granule_name[64];
  char granule_type[64];
  char platform[64];
  char sensor[64];
  int orbit;
  char beam_mode[64];
  double off_nadir_angle;
  char start_time[64];
  char end_time[64];
  double near_start_lat;
  double near_start_lon;
  double far_start_lat;
  double far_start_lon;
  double near_end_lat;
  double near_end_lon;
  double far_end_lat;
  double far_end_lon;
  double center_lat;
  double center_lon;
  int path_number;
  int frame_number;
  int cloud_cover;
  double faraday_rotation;
} ursa_type_t;

void shape_ursa_init(char *inFile, char *header);

#endif
