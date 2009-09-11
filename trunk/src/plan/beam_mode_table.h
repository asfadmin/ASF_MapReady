#ifndef BEAM_MODE_TABLE_H
#define BEAM_MODE_TABLE_H

typedef struct
{
    double min_look_angle;
    double max_look_angle;
    double look_angle_increment;
    double width_m;
    double length_m;
    double image_time;
    int num_buffer_frames;
    char allowed_look_angles[128];
} BeamModeInfo;

BeamModeInfo *get_beam_mode_info(const char *satellite, const char *beam_mode);
void get_all_beam_modes(const char *satellite, int *num_out,
                        char ***names_out, double **min_looks_out,
                        double **max_looks_out, double **look_incrs_out,
                        char ***allowed_look_angles_out);

#endif
