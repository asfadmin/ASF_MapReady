#ifndef PLAN_H
#define PLAN_H

#include "polygon.h"
#include "asf_meta.h"

// An "overlap" is a position of the satellite from which it
// images the target region
typedef struct {
    double pct;
    Poly *viewable_region;
    int utm_zone;
    stateVector state_vector;
    double t, clat, clon;
} OverlapInfo;

// A "pass" is a set of consecutive frames (overlaps) that all
// overlap the target region
typedef struct {
    int num;
    double start_time;
    char *start_time_as_string;
    OverlapInfo **overlaps;
    double total_pct;
    char dir;
    int orbit;
    double orbit_part;
    double start_lat, stop_lat;
    double duration;
} PassInfo;

// A "PassCollection" is a set of passes
typedef struct {
    int num;
    PassInfo **passes;
    double clat, clon;
    Poly *aoi;
} PassCollection;

/*  This is the external interface to the planner  */

// These are the options for the "pass_type"
#define ASCENDING_OR_DESCENDING 0
#define ASCENDING_ONLY 1
#define DESCENDING_ONLY 2

// Inputs:
//   satellite:    ERS-1, ERS-2, ALOS, etc
//   beam_mode: 
//   startdate:    e.g. 20070101
//   enddate:      e.g. 20071231
//   min_lat:      
//   max_lat:
//   clat,clon:    center of bounding box
//   pass_type:    as above
//   aoi:          utm coordinates for the target region
//   tle_filename: TLE file
// Output:
//   pc:           pass collection struct, as above
// Returns the number of acquisitions found

int plan(const char *satellite, const char *beam_mode, double look_angle,
         long startdate, long enddate, double min_lat, double max_lat,
         double clat, double clon, int pass_type,
         int zone, Poly *aoi, const char *tle_filename,
         PassCollection **pc, char **errorstring);

int is_valid_date(long date);
void pass_collection_free(PassCollection *pc);
void pass_collection_to_kml(PassCollection *pc, const char *kml_file);

const char *get_tle_info(const char *tle_filename, const char *satellite);

void get_all_beam_modes(const char *satellite, int *num_out,
                        char ***names_out, double **min_looks_out,
                        double **max_looks_out, double **look_incrs_out,
                        char ***allowed_look_angles_out);

// Customized projection functions for the planner.
// "zone" : when this +/- 999, polar stereo is used.
//          otherwise, utm is used (with the given zone)
void ll2pr(double lat, double lon, int zone, double *projX, double *projY);
void pr2ll(double projX, double projY, int zone, double *lat, double *lon);
int proj2lineSamp(meta_parameters *meta, int zone,
                  double proj_x, double proj_y,
                  double elev, double *line, double *samp);

#endif
