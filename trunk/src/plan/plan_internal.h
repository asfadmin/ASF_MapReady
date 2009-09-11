#ifndef PLAN_INT_H
#define PLAN_INT_H

#include "plan.h"
#include "dateUtil.h"
#include "asf_meta.h"
#include "sgpsdp.h"

/* kml.c */
void kml_aoi(FILE *kml_file, double clat, double clon, Poly *aoi);
void write_pass_to_kml(FILE *kml_file, double lat, double lon, PassInfo *info);

/* tle.c */
void read_tle(const char *tle_filename, const char *satellite, sat_t *sat);
stateVector tle_propagate(sat_t *sat, double t);
double secs_to_jul(double t);
double jul_to_secs(double jul);
double time_to_secs(int year, int doy, double fod);

/* overlap.c */
OverlapInfo *overlap_new(int pct, int n, Poly *viewable_region,
                         int zone, double clat, double clon, stateVector *st,
                         double t);
void overlap_free(OverlapInfo *oi);

/* pass.c */
PassInfo *pass_info_new(int orbit, double orbit_part, char dir);
void pass_info_add(PassInfo *pass_info, double t, OverlapInfo *oi);
void pass_info_set_duration(PassInfo *pass_info, double duration);
void pass_info_set_stop_latitude(PassInfo *pass_info, double stop_latitude);
void pass_info_set_start_latitude(PassInfo *pass_info, double start_latitude);
void pass_info_free(PassInfo *info);
PassCollection *pass_collection_new(double clat, double clon, Poly *aoi);
void pass_collection_add(PassCollection *pc, PassInfo *info);

#endif
