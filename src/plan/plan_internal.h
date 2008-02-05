#ifndef PLAN_INT_H
#define PLAN_INT_H

#include "plan.h"
#include "date_util.h"
#include "asf_meta.h"
#include "sgpsdp.h"

/* kml.c */
void kml_aoi(FILE *kml_file, double clat, double clon, Polygon *aoi);
void write_pass_to_kml(FILE *kml_file, double lat, double lon, PassInfo *info);

/* tle.c */
void read_tle(const char *tle_filename, const char *satellite, sat_t *sat);
stateVector tle_propagate(sat_t *sat, double t);
double secs_to_jul(double t);

/* overlap.c */
OverlapInfo *overlap_new(int pct, int n, Polygon *viewable_region,
                         int zone, double clat, double clon, stateVector *st,
                         double t);
void overlap_free(OverlapInfo *oi);

/* pass.c */
PassInfo *pass_info_new(void);
void pass_info_add(PassInfo *info, double t, char dir, int orbit,
                   int frame, OverlapInfo *oi);
void pass_info_free(PassInfo *info);
PassCollection *pass_collection_new(double clat, double clon, Polygon *aoi);
void pass_collection_add(PassCollection *pc, PassInfo *info);

#endif
