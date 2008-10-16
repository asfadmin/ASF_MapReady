#include "plan_internal.h"
#include "asf_vector.h"
#include <assert.h>

PassInfo *pass_info_new(int orbit, double orbit_part, char dir)
{
    PassInfo *ret = MALLOC(sizeof(PassInfo));

    ret->num = 0;
    ret->overlaps = NULL;
    ret->start_time = -1;
    ret->start_time_as_string = NULL;
    ret->dir = dir;
    ret->orbit = orbit;
    ret->orbit_part = orbit_part;
    ret->start_lat = -999;
    ret->stop_lat = -999;
    ret->duration = -999;
    return ret;
}

void pass_info_add(PassInfo *pass_info, double t, OverlapInfo *oi)
{
    if (pass_info->start_time == -1) {
        assert(pass_info->num == 0);
        pass_info->start_time = t;
        pass_info->start_time_as_string = STRDUP(date_str(t));
        pass_info->total_pct = 0;
    }

    int i;

    pass_info->num += 1;

    // create a new overlaps array
    OverlapInfo **overlaps = MALLOC(sizeof(OverlapInfo*)*(pass_info->num));

    // copy over old array, add new overlap info
    for (i=0; i<pass_info->num-1; ++i)
        overlaps[i] = pass_info->overlaps[i];
    overlaps[pass_info->num-1] = oi;

    // free old array (but not what was pointed to)
    FREE(pass_info->overlaps);

    // new array replaces old
    pass_info->overlaps = overlaps;

    // update coverage
    pass_info->total_pct += oi->pct;
}

void pass_info_set_duration(PassInfo *pass_info, double duration)
{
    pass_info->duration = duration;
}

void pass_info_set_start_latitude(PassInfo *pass_info, double start_latitude)
{
    pass_info->start_lat = start_latitude;
}

void pass_info_set_stop_latitude(PassInfo *pass_info, double stop_latitude)
{
    pass_info->stop_lat = stop_latitude;
}

void pass_info_free(PassInfo *pass_info)
{
    int i;
    for (i=0; i<pass_info->num; ++i)
        overlap_free(pass_info->overlaps[i]);
    FREE(pass_info->overlaps);
    FREE(pass_info->start_time_as_string);
    FREE(pass_info);
}

//-----------------------------------------------------------------------------

PassCollection *pass_collection_new(double clat, double clon, Poly *aoi)
{
  PassCollection *ret = MALLOC(sizeof(PassCollection));

  ret->num = 0;
  ret->passes = NULL;

  ret->clat = clat;
  ret->clon = clon;
  ret->aoi = aoi;

  return ret;
}

void pass_collection_add(PassCollection *pc, PassInfo *pass_info)
{
  int i;
  pc->num += 1;

  // create a new passes array 
  PassInfo **passes = MALLOC(sizeof(PassInfo*)*(pc->num));

  // copy over old array, add new pass info
  for (i=0; i<pc->num-1; ++i)
    passes[i] = pc->passes[i];
  passes[pc->num-1] = pass_info;

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

