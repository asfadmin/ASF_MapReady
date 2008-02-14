#include "plan.h"
#include "plan_internal.h"

#include "asf.h"
#include "asf_meta.h"

#include "date.h"
#include "date_util.h"
#include "beam_mode_table.h"

#include "asf_vector.h"
#include "sgpsdp.h"

#include <stdlib.h>
#include <assert.h>

static int iabs(int a)
{
  return a > 0 ? a : -a;
}

static void
get_target_position(stateVector *st, double look, double yaw,
                    double sr, vector *targPos)
{
  vector relPos = vecNew(sin(yaw), -sin(look)*cos(yaw), -cos(look)*cos(yaw));

  // relPos unit vector points from s/c to targPos.
  // Rotate into earth axes
  vector look_matrix[3];
  look_matrix[2] = st->pos;
  vecNormalize(&look_matrix[2]);
  vector v = st->vel;
  vecNormalize(&v);
  vecCross(look_matrix[2],v,&look_matrix[1]);
  vecCross(look_matrix[1],look_matrix[2],&look_matrix[0]);
  vecMul(look_matrix,relPos,&relPos);

  // scale relPos so it reaches from s/c to targPos 
  vecScale(&relPos,sr);
  vecAdd(relPos,st->pos,targPos);
}

static int
get_target_latlon(stateVector *st, double look, double *tlat, double *tlon)
{
  // satellite height, from the state vector
  double ht = vecMagnitude(st->pos);

  // earth radius, calculate from WGS84 values, and approx latitude
  double re = 6378137.0;
  double rp = 6356752.31414;
  double lat = asin(st->pos.z/ht);
  double er = re*rp/sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));

  // calculate slant range by law of cosines
  // (where we know two sides, but not the angle between them)
  double D = er*er - ht*ht*sin(look)*sin(look);
  if (D < 0) 
    return 0; // can't see the Earth from here
  //double sr1 = -ht*cos(look) + sqrt(D);
  //double sr2 = -ht*cos(look) - sqrt(D);
  double sr1 = ht*cos(look) + sqrt(D);
  double sr2 = ht*cos(look) - sqrt(D);
  double sr = sr1>sr2 ? sr2 : sr1;

  // target position, first in inertial coords, then convert to lat/lon
  vector target;
  get_target_position(st, look, 0, sr, &target);

  double glat; // geocentric
  cart2sph(target,&er,&glat,tlon);
  
  // tlon should be -180,180
  // tlat needs to be geodetic
  // both should be degrees
  *tlon *= R2D;
  if (*tlon < -180.) *tlon += 360.;

  *tlat = R2D*atan(tan(glat)*re*re/(rp*rp));

  return 1;
}

static Polygon *
get_viewable_region(stateVector *st, BeamModeInfo *bmi,
                    int target_zone, double target_lat, double target_lon)
{
  double center_lat, center_lon, center_x, center_y;
  get_target_latlon(st, bmi->look_angle, &center_lat, &center_lon);

  // return NULL if we are "far away"
  int zone = utm_zone(center_lon);
  if (iabs(zone - target_zone) > 2)
    return NULL;
  if (fabs(center_lat - target_lat) > 20)
    return NULL;

  //printf("center: %f %f\n", center_lat, center_lon);
  latLon2UTM_zone(center_lat, center_lon, 0, target_zone,
                  &center_x, &center_y);

  // location of a point just a bit ahead of us
  stateVector ahead_st = propagate(*st, 0, 1);
  double ahead_lat, ahead_lon, ahead_x, ahead_y;
  get_target_latlon(&ahead_st, bmi->look_angle, &ahead_lat, &ahead_lon);
  latLon2UTM_zone(ahead_lat, ahead_lon, 0, target_zone, &ahead_x, &ahead_y);
  //printf("ahead: %f %f\n", ahead_lat, ahead_lon);

  // now we know the orientation of the rectangle on the ground
  // ==> can find the 4 corners
  double tilt = atan2(ahead_y-center_y, ahead_x-center_x);
  double xl = bmi->length_m;
  double yl = bmi->width_m;

  double x_side1 = cos(tilt)*xl;
  double y_side1 = sin(tilt)*xl;
  double x_side2 = sin(tilt)*yl;
  double y_side2 = cos(tilt)*yl;

  double x[4], y[4];

  x[0] = center_x + x_side1/2. - x_side2/2.;
  y[0] = center_y + y_side1/2. - y_side2/2.;

  x[1] = x[0] - x_side1;
  y[1] = y[0] - y_side1;

  x[2] = x[1] + x_side2;
  y[2] = y[1] - y_side2;

  x[3] = x[2] + x_side1;
  y[3] = y[2] + y_side1;

  //int i;
  //printf("corners:\n");
  //for (i=0; i<4; ++i) {
  //  double lat, lon;
  //  UTM2latLon(x[i],y[i],0,zone,&lat,&lon);
  //  printf("%f %f\n", lat, lon);
  //}

  return polygon_new_closed(4, x, y);
}

static double random_in_interval(double lo, double hi)
{
  return lo + (hi-lo)*((double)rand() / ((double)(RAND_MAX)));
}

static OverlapInfo *
overlap(double t, stateVector *st, BeamModeInfo *bmi,
        int zone, double clat, double clon, Polygon *aoi)
{
  Polygon *viewable_region = get_viewable_region(st, bmi, zone, clat, clon);

  if (!viewable_region)
    return NULL; // no overlap

  if (polygon_overlap(aoi, viewable_region)) {
    // need a good method to test for overlap amount!
    // for now we have this sort of kludgey method... which is
    // probably a bit slow:
    //   generate 1000 points in the aoi -- pct is how many are also
    //   in the viewable region
    srand(42);
    int i=0,pct=0,n=1000;
    double xmin, xmax, ymin, ymax;
    polygon_get_bbox(aoi, &xmin, &xmax, &ymin, &ymax);

    while (i<n) {
      double x = random_in_interval(xmin, xmax);
      double y = random_in_interval(ymin, ymax);
      if (point_in_polygon(aoi, x, y)) {
        ++i;
        if (point_in_polygon(viewable_region, x, y))
          ++pct;
      }
    }

    return overlap_new(pct, n, viewable_region, zone, clat, clon, st, t);
  }
  else {
    // no overlap
    free(viewable_region);
    return NULL;
  }
}

int plan(const char *satellite, const char *beam_mode,
         long startdate, long enddate, double min_lat, double max_lat,
         double clat, double clon, int pass_type,
         int zone, Polygon *aoi, const char *tle_filename,
         PassCollection **pc_out, char **errorstring)
{
  BeamModeInfo *bmi = get_beam_mode_info(satellite, beam_mode);
  if (!bmi) {
    *errorstring = STRDUP("Unknown satellite/beam mode combination.\n");
    return -1;
  }

  double start_secs = seconds_from_l(startdate);
  double end_secs = seconds_from_l(enddate);

  sat_t sat;
  read_tle(tle_filename, satellite, &sat);

  // no deep space orbits can be planned
  assert((sat.flags & DEEP_SPACE_EPHEM_FLAG) == 0);

  printf("Target:\n"
         "  UTM:  zone=%d\n"
         "        %f %f\n"
         "        %f %f\n"
         "        %f %f\n"
         "        %f %f\n",
         zone,
         aoi->x[0], aoi->y[0],
         aoi->x[1], aoi->y[1],
         aoi->x[2], aoi->y[2],
         aoi->x[3], aoi->y[3]);

  double curr = start_secs;
  double incr = bmi->image_time;
  stateVector st = tle_propagate(&sat, start_secs-incr);
  double lat_prev = sat.ssplat;
  int i,num_found = 0;

  // Calculate the number of frames to include before we hit the
  // area of interest.  Add 1 (i.e., round up), but if user puts in
  // zero seconds, then we want 0 lead-up frames.
  PassCollection *pc = pass_collection_new(clat, clon, aoi);

  while (curr < end_secs) {
    st = tle_propagate(&sat, curr);
    char dir = sat.ssplat > lat_prev ? 'A' : 'D';

    if ((dir=='A' && pass_type!=DESCENDING_ONLY) ||
        (dir=='D' && pass_type!=ASCENDING_ONLY))
    {
      OverlapInfo *oi = overlap(curr, &st, bmi, zone, clat, clon, aoi);
      if (oi) {
        int n=0;
        PassInfo *pass_info = pass_info_new();

        for (i=bmi->num_buffer_frames; i>0; --i) {
          double t = curr - i*incr;
          stateVector st1 = tle_propagate(&sat, t);
          Polygon *region = get_viewable_region(&st1, bmi, zone, clat, clon);
          OverlapInfo *oi1 = overlap_new(0, 1000, region, zone, clat, clon,
                                         &st1, t);
          pass_info_add(pass_info, t, dir, sat.orbit, sat.orbit_part*2778, oi1);
        }

        while (curr < end_secs && oi) {
          pass_info_add(pass_info, curr, dir, sat.orbit, sat.orbit_part*2778, oi);
          ++n;

          curr += incr;
          st = tle_propagate(&sat, curr);

          oi = overlap(curr, &st, bmi, zone, clat, clon, aoi);
        }

        for (i=0; i<bmi->num_buffer_frames; ++i) {
          double t = curr + i*incr;
          stateVector st1 = tle_propagate(&sat, t);
          Polygon *region = get_viewable_region(&st1, bmi, zone, clat, clon);
          OverlapInfo *oi1 = overlap_new(0, 1000, region, zone, clat, clon,
                                         &st1, t);
          pass_info_add(pass_info, t, dir, sat.orbit, sat.orbit_part*2778, oi1);
        }

        if (n>0) {
          pass_collection_add(pc, pass_info);
          ++num_found;
        }
      }
    }

    curr += incr;
    lat_prev = sat.ssplat;

    asfPercentMeter((curr-start_secs)/(end_secs-start_secs));
  }
  asfPercentMeter(1.0);

  *pc_out = pc;
  return num_found;
}

