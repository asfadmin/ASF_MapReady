#include "plan_internal.h"
#include <assert.h>

PassInfo *pass_info_new()
{
    PassInfo *ret = MALLOC(sizeof(PassInfo));

    ret->num = 0;
    ret->overlaps = NULL;
    ret->start_time = -1;

    return ret;
}

void pass_info_add(double t, PassInfo *pi, OverlapInfo *oi)
{
    if (pi->start_time == -1) {
        assert(pi->num == 0);
        pi->start_time = t;
    }

    int i;
    pi->num += 1;

    // create a new overlaps array 
    OverlapInfo **overlaps = MALLOC(sizeof(OverlapInfo*)*(pi->num));

    // copy over old array, add new overlap info
    for (i=0; i<pi->num-1; ++i)
        overlaps[i] = pi->overlaps[i];
    overlaps[pi->num-1] = oi;

    // free old array (but not what was pointed to)
    FREE(pi->overlaps);

    // new array replaces old
    pi->overlaps = overlaps;
}

void pass_info_free(PassInfo *pi)
{
    int i;
    for (i=0; i<pi->num; ++i)
        overlap_free(pi->overlaps[i]);
    free(pi->overlaps);
    free(pi);
}
