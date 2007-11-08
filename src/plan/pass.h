#ifndef PASS_H
#define PASS_H

#include "overlap.h"

typedef struct {
    int num;
    double start_time;
    OverlapInfo **overlaps;
} PassInfo;

PassInfo *pass_info_new(void);
void pass_info_add(double t, PassInfo *pi, OverlapInfo *oi);
void pass_info_free(PassInfo *pi);

#endif
