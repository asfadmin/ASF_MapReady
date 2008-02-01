#ifndef BEAM_MODE_TABLE_H
#define BEAM_MODE_TABLE_H

typedef struct
{
    double look_angle;
    double width_m;
    double length_m;
    double image_time;
} BeamModeInfo;

BeamModeInfo *get_beam_mode_info(const char *satellite, const char *beam_mode);

#endif
