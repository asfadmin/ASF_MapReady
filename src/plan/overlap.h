#ifndef OVERLAP_H
#define OVERLAP_H

#include "asf_meta.h"
#include "polygon.h"

typedef struct {
    double pct;
    Polygon *viewable_region;
    int utm_zone;
    stateVector state_vector;
    double t, clat, clon;
} OverlapInfo;

OverlapInfo *overlap_new(int pct, int n, Polygon *viewable_region,
                         double clat, double clon, stateVector *st,
                         double t);
void overlap_free(OverlapInfo *oi);

#endif
