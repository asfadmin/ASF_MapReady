#include "plan.h"
#include "plan_internal.h"

#include "asf.h"
#include "asf_meta.h"

#include "date.h"
#include "date_util.h"
#include "beam_mode_table.h"

#include "asf_vector.h"

#include <stdlib.h>

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
  vecCross(st->pos,v,&look_matrix[1]);
  vecCross(look_matrix[1],st->pos,&look_matrix[0]);
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
  double sr1 = -ht*cos(look) + sqrt(D);
  double sr2 = -ht*cos(look) - sqrt(D);
  double sr = sr1>sr2 ? sr2 : sr1;

  // target position, first in inertial coords, then convert to lat/lon
  vector target;
  get_target_position(st, look, sr, 0, &target);

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

  latLon2UTM_zone(center_lat, center_lon, 0, target_zone,
                  &center_x, &center_y);

  // location of a point just a bit ahead of us
  stateVector ahead_st = propagate(*st, 0, 1);
  double ahead_lat, ahead_lon, ahead_x, ahead_y;
  get_target_latlon(&ahead_st, bmi->look_angle, &ahead_lat, &ahead_lon);
  latLon2UTM_zone(ahead_lat, ahead_lon, 0, target_zone, &ahead_x, &ahead_y);

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

  x[0] = center_x + x_side1/2. + x_side2/2.;
  y[0] = center_y + y_side1/2. + y_side2/2.;

  x[1] = x[0] - x_side1;
  y[1] = y[0] - y_side1;

  x[2] = x[1] + x_side2;
  y[2] = y[1] - y_side2;

  x[3] = x[2] + x_side1;
  y[3] = y[2] + y_side1;

  return polygon_new_closed(4, x, y);
}

static double random_in_interval(double lo, double hi)
{
  return lo + (hi-lo)*((double)rand() / ((double)(RAND_MAX)));
}

/*
// Useful debug routine -- exports the polygons in a useful format
// for cutting&pasting into octave/matlab.
static int print_polys(Polygon *p1, Polygon *p2)
{
  printf("p1 = [ %f %f; %f %f; %f %f; %f %f; %f %f ]; "
         "p2 = [ %f %f; %f %f; %f %f; %f %f; %f %f ];\n",
         p1->x[0], p1->y[0],
         p1->x[1], p1->y[1],
         p1->x[2], p1->y[2],
         p1->x[3], p1->y[3],
         p1->x[4], p1->y[4],
         p2->x[0], p2->y[0],
         p2->x[1], p2->y[1],
         p2->x[2], p2->y[2],
         p2->x[3], p2->y[3],
         p2->x[4], p2->y[4]);
  return 1;
}
*/

static OverlapInfo *
overlap(double t, stateVector *st, BeamModeInfo *bmi,
        int zone, double clat, double clon, Polygon *aoi)
{
  Polygon *viewable_region = get_viewable_region(st, bmi, zone, clat, clon);

  if (!viewable_region)
    return FALSE; // no overlap

  if (polygon_overlap(aoi, viewable_region)) {
    // need a good method to test for overlap amount!
    // for now we have this sort of kludgey method... which is
    // probably a bit slow:
    //   generate 100 points in the aoi -- pct is how many are also
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
    return NULL;
  }
}

static int
check_crossing(PassCollection *pc, double start_time, double end_time,
               double state_vector_time, stateVector *st,
               BeamModeInfo *bmi, int zone, double clat, double clon,
               Polygon *aoi)
{
    double t1 = start_time;
    PassInfo *pi = pass_info_new();
    double delta = bmi->image_time;
    int is_overlap;

    do {
      double t = t1 + state_vector_time;

      //{
      //  double lat, lon, llat, llon;
      //  get_target_latlon(&st1, 0, &lat, &lon);
      //  get_target_latlon(&st1, bmi->look_angle, &llat, &llon);
      //  printf("1: t=%s  Satellite location: "
      //         "%10.3f %10.3f\n", 
      //         date_str(t), lat, lon);
      //  printf("1:                          Looking at        : "
      //         "%10.3f %10.3f\n",
      //         llat, llon);
      //}

      // Look for overlap when the satellite is in this position
      OverlapInfo *oi = overlap(t, st, bmi, zone, clat, clon, aoi);
      if (oi)
        pass_info_add(pi, t, oi);

      // moving ahead to the next check point
      *st = propagate(*st, t, t+delta);
      t1 += delta;

      // we'll continue as long as we're finding overlap, even if
      // we pass the official "end time" (where we cross out of the
      // latitude range)
      is_overlap = oi != NULL;
    }
    while (t1 < end_time || is_overlap);
    
    int found = pi->num > 0;
    if (found)
      pass_collection_add(pc, pi);

    return found;
}

static void
find_crossings(BeamModeInfo *bmi, double start_secs,
               stateVector *start_stVec, double start_lat,
               double min_lat, double max_lat,
               double *time1_in, double *time1_out,
               double *time2_in, double *time2_out,
               double *full_cycle_time, int *first_is_ascending)
{
  *time1_in = *time1_out = -1;
  *time2_in = *time2_out = -1;
  *full_cycle_time = -1;

  int ncrossings_target=0, ncrossings_startlat=0;
  int in_target_lat_range = FALSE;

  const double normal_delta = 1;
  const double end_delta = .0002;
  double delta = normal_delta;

  double curr = start_secs;
  stateVector st = *start_stVec;

  double lat_prev = -999;
  int iter=0;

  while (1) {
    ++iter;

    double lat, lon, llat, llon;
    get_target_latlon(&st, 0, &lat, &lon);
    get_target_latlon(&st, bmi->look_angle, &llat, &llon);

    // looking for two crossings:
    //  1) crossing the target latitude (moving into from top or bottom)
    //  2) crossing the starting latitude

    if (lat_prev != -999 && iter > 1) {
      if ((lat_prev < start_lat && lat > start_lat) ||
          (lat_prev > start_lat && lat < start_lat)) {
        printf("Crossed starting latitude at t=%f, lat=%f, iter=%d\n",
               curr-start_secs, lat, iter);

        ncrossings_startlat++;
        if (ncrossings_startlat == 2) {
          printf(" --> Finished complete cycle: t=%f\n", curr-start_secs);
          if (delta == end_delta) {
            *full_cycle_time=curr-start_secs;
            break;
          } else {
            printf("     Refining full cycle time estimate.\n");
            printf("     Backing up to time = %f\n", curr-2*delta-start_secs);
            ncrossings_startlat = 1;
            curr -= 2*delta;
            delta = end_delta;
            lat=lat_prev;
            lat_prev=-999;
            st = propagate(*start_stVec, start_secs, curr);
          }
        }
      }
    }

    if (lat_prev != -999) {

      int crossed_min = (lat_prev < min_lat && lat > min_lat) ||
                        (lat_prev > min_lat && lat < min_lat);

      int crossed_max = (lat_prev < max_lat && lat > max_lat) ||
                        (lat_prev > max_lat && lat < max_lat);

      //if (delta == normal_delta)
      //  printf("min %f %f %f - %d, max %f %f %f - %d\n",
      //         lat_prev, min_lat, lat, crossed_min,
      //         lat_prev, max_lat, lat, crossed_max);

      if (crossed_min || crossed_max)
      {
        // either crossed into or out of the target latitude range
        in_target_lat_range = !in_target_lat_range;

        if (in_target_lat_range) {
          printf("Crossed into target range at t=%f, lat=%f, iter=%d\n",
                 curr-start_secs, lat, iter);
        } else {
          printf("Crossed out of target range at t=%f, lat=%f, iter=%d\n",
                 curr-start_secs, lat, iter);
        }

        if (in_target_lat_range) {
          if (ncrossings_target == 0) {
            *first_is_ascending = lat > lat_prev;
            *time1_in = curr-start_secs;
          } else if (ncrossings_target == 1) {
            *time2_in = curr-start_secs;
          } else {
            printf("No way dude!\n");
          }
          ++ncrossings_target;
        }

        // if we crossed BOTH min_lat AND max_lat, mark that we have
        // also left the target rangex
        if (crossed_min && crossed_max)
          in_target_lat_range = !in_target_lat_range;

        if (!in_target_lat_range) {
          if (ncrossings_target == 1) {
            *time1_out = curr-start_secs;
          } else if (ncrossings_target == 2) {
            *time2_out = curr-start_secs;
          } else {
            printf("No way dude!\n");
          }
        }
      }
    }

    lat_prev = lat;

    // The first of these is much slower, theoretically more accurate
    // but from my experimentation not *that* much more accurate...
    //st = propagate(*start_stVec, start_secs, curr+delta);
    st = propagate(st, curr, curr+delta);
    curr += delta;
  }
}

int plan(const char *satellite, const char *beam_mode,
         long startdate, long enddate, double min_lat, double max_lat,
         double clat, double clon, int pass_type, int zone, Polygon *aoi,
         const char *tle_filename, PassCollection **pc_out,
         char **errorstring)
{
  BeamModeInfo *bmi = get_beam_mode_info(satellite, beam_mode);
  if (!bmi) {
    *errorstring = STRDUP("Unknown satellite/beam mode combination.\n");
    return -1;
  }

  double start_secs = seconds_from_l(startdate);
  double end_secs = seconds_from_l(enddate);

  stateVector tle_stVec;
  double tle_secs;

  read_tle(tle_filename, satellite, &tle_stVec, &tle_secs);

  stateVector start_stVec = propagate(tle_stVec, tle_secs, start_secs);

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

  double start_lat, start_lon;
  get_target_latlon(&start_stVec, 0, &start_lat, &start_lon);
  printf("Starting latitude: %f\n", start_lat);

  double time1_in, time1_out, time2_in, time2_out, full_cycle_time;
  int first_is_ascending=-1;

  // Look for the times when we cross the latitude range of the area of
  // interest.  Also, figure out the time required for a full revolution.
  find_crossings(bmi, start_secs, &start_stVec, start_lat, min_lat, max_lat,
                 &time1_in, &time1_out, &time2_in, &time2_out,
                 &full_cycle_time, &first_is_ascending);

  // find_crossings() is supposed to set "first_is_ascending"
  if (first_is_ascending == -1) {
    *errorstring =
      STRDUP("Failed to detect which pass is ascending, and\n"
             "which is descending!  Does this satellite have a\n"
             "fully polar orbit?");
    return -1;
  }

  // move the start times back a little, as a safety margin
  time1_in -= 4;
  time2_in -= 4;

  printf("Time to first target crossing: %f\n", time1_in);
  printf("Time to end of first target crossing: %f\n", time1_out);
  printf("Time to second target crossing: %f\n", time2_in);
  printf("Time to end of second target crossing: %f\n", time2_out);
  printf("Time for complete cycle: %f\n", full_cycle_time);

  // Set up the ascending/descending filters.  Don't necessarily know
  // if the first or second crossing the is ascending one.
  int check_first_crossing, check_second_crossing;
  if (pass_type==ASCENDING_OR_DESCENDING) {
    check_first_crossing = check_second_crossing = TRUE;
  }
  else if (pass_type==ASCENDING_ONLY) {
    check_first_crossing = first_is_ascending;
    check_second_crossing = !first_is_ascending;
  }
  else if (pass_type==DESCENDING_ONLY) {
    check_first_crossing = !first_is_ascending;
    check_second_crossing = first_is_ascending;
  }
  else {
    // this should never happen
    asfPrintError("Internal error: invalid pass_type: %d\n", pass_type);
  }

  // Iteration #2: Looking for overlaps.

  double curr = start_secs;
  stateVector st = start_stVec;

  PassCollection *pc = pass_collection_new(clat, clon, aoi);

  // Use time info gathered above to propagate straight to the areas of
  // interest.  There are two crossings to check, since we cross the latitude
  // range twice on each circuit.
  int num_found = 0;
  while (curr < end_secs) {

    if (check_first_crossing) {
      double t = curr + time1_in;
      stateVector st1 = propagate(st, curr, t);

      num_found +=
        check_crossing(pc, time1_in, time1_out, t, &st1, bmi,
                       zone, clat, clon, aoi);
    }

    if (check_second_crossing) {
      double t = curr + time2_in;
      stateVector st2 = propagate(st, curr, t);

      num_found +=
        check_crossing(pc, time2_in, time2_out, t, &st2, bmi,
                       zone, clat, clon, aoi);
    }

    st = propagate(st, curr, curr + full_cycle_time);
    curr += full_cycle_time;

    asfPercentMeter((curr-start_secs)/(end_secs-start_secs));
  }
  asfPercentMeter(1.0);

  *pc_out = pc;
  return num_found;
}
