#ifndef PLAN_INT_H
#define PLAN_INT_H

#include "plan.h"
#include "date_util.h"
#include "pass.h"

/* kml.c */
void kml_aoi(FILE *kml_file, double clat, double clon, Polygon *aoi);
void write_pass_to_kml(FILE *kml_file, double lat, double lon, PassInfo *pi);

/* tle.c */
void read_tle(const char *tle_filename, const char *satellite,
              stateVector *st, double *st_time);

#endif
