#ifndef PLAN_H
#define PLAN_H

#include "polygon.h"
#include "asf_meta.h"

void plan(const char *satellite, const char *beam_mode,
          long startdate, long enddate,
          double min_lat, double max_lat,
          double clat, double clon, Polygon *aoi,
          meta_parameters *meta, const char *outFile);

#endif
