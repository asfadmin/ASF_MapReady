#ifndef PASS_H
#define PASS_H

#include "overlap.h"

// A "pass" is a set of consecutive frames (overlaps) that all
// overlap the target region

typedef struct {
    int num;
    double start_time;
    OverlapInfo **overlaps;
} PassInfo;

PassInfo *pass_info_new(void);
void pass_info_add(PassInfo *pi, double t, OverlapInfo *oi);
void pass_info_free(PassInfo *pi);

#endif
