#ifndef PLAN_H
#define PLAN_H

#include "polygon.h"
#include "asf_meta.h"
#include "date_util.h"

typedef struct {
    double pct;
    Polygon *viewable_region;
    int utm_zone;
    stateVector state_vector;
    double t, clat, clon;
} OverlapInfo;

typedef struct {
    int num;
    OverlapInfo **overlaps;
} PassInfo;

void plan(const char *satellite, const char *beam_mode,
          long startdate, long enddate,
          double min_lat, double max_lat,
          double clat, double clon, Polygon *aoi,
          meta_parameters *meta, const char *outFile);

#endif
