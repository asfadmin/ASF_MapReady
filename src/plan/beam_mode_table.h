#ifndef PLAN_H
#define PLAN_H

typdef struct
{
    double look_angle;
    double width_m;
    double length_m;
} BeamModeInfo;

BeamModeInfo *get_beam_mode_info(const char *satellite, const char *beam_mode);

#endif
