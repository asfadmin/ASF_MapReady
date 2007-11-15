#include "plan_internal.h"
#include "asf_vector.h"
#include <assert.h>

PassInfo *pass_info_new()
{
    PassInfo *ret = MALLOC(sizeof(PassInfo));

    ret->num = 0;
    ret->overlaps = NULL;
    ret->start_time = -1;
    ret->start_time_as_string = NULL;
    return ret;
}

void pass_info_add(PassInfo *pi, double t, OverlapInfo *oi)
{
    if (pi->start_time == -1) {
        assert(pi->num == 0);
        pi->start_time = t;
        pi->start_time_as_string = STRDUP(date_str(t));
        pi->total_pct = 0;
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

    // update coverage
    pi->total_pct += oi->pct;
}

void pass_info_free(PassInfo *pi)
{
    int i;
    for (i=0; i<pi->num; ++i)
        overlap_free(pi->overlaps[i]);
    FREE(pi->overlaps);
    FREE(pi->start_time_as_string);
    FREE(pi);
}

//-----------------------------------------------------------------------------

PassCollection *pass_collection_new(double clat, double clon, Polygon *aoi)
{
  PassCollection *ret = MALLOC(sizeof(PassCollection));

  ret->num = 0;
  ret->passes = NULL;

  ret->clat = clat;
  ret->clon = clon;
  ret->aoi = aoi;

  return ret;
}

void pass_collection_add(PassCollection *pc, PassInfo *pi)
{
  int i;
  pc->num += 1;

  // create a new passes array 
  PassInfo **passes = MALLOC(sizeof(PassInfo*)*(pc->num));

  // copy over old array, add new pass info
  for (i=0; i<pc->num-1; ++i)
    passes[i] = pc->passes[i];
  passes[pc->num-1] = pi;

  // free old array (but not what was pointed to), and replace old with new
  FREE(pc->passes);
  pc->passes = passes;
}

void pass_collection_free(PassCollection *pc)
{
  if (pc) {
    int i;
    for (i=0; i<pc->num; ++i)
      pass_info_free(pc->passes[i]);
    FREE(pc->passes);
    // do not free the aoi polygon!
    FREE(pc);
  }
}

void pass_collection_to_kml(PassCollection *pc, const char *kml_file)
{
  FILE *ofp = FOPEN(kml_file, "w");
  kml_header(ofp);
  kml_aoi(ofp, pc->clat, pc->clon, pc->aoi);

  int i;
  for (i=0; i<pc->num; ++i)
    write_pass_to_kml(ofp, pc->clat, pc->clon, pc->passes[i]);

  kml_footer(ofp);
  fclose(ofp);
}

