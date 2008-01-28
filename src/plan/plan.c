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
  //double sr1 = -ht*cos(look) + sqrt(D);
  //double sr2 = -ht*cos(look) - sqrt(D);
  double sr1 = ht*cos(look) + sqrt(D);
  double sr2 = ht*cos(look) - sqrt(D);
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

static int
check_crossing(PassCollection *pc, double start_time, double end_time,
               double state_vector_time, stateVector *st,
               BeamModeInfo *bmi, int zone, double clat, double clon,
               Polygon *aoi, char dir)
{
    double t1 = start_time;
    PassInfo *pass_info = pass_info_new();
    double delta = bmi->image_time;
    int is_overlap;
    int first = TRUE;
    int n_backups = 0;

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
      // If we find it at the starting time, we will back up until
      // we find that there is no overlap
      OverlapInfo *oi = overlap(t, st, bmi, zone, clat, clon, aoi);
      if (oi) {
        if (first) {
          // we might have jumped in in the middle, so start backing up
          // (actually, this shouldn't happen much since we padded up above)
          *st = propagate(*st, t1, t1-delta);
          ++n_backups;
          t1 -= delta;
          free(oi);
          if (n_backups > 100)
            printf("Getting a little out of hand!\n");
          else
            continue;
        } else {
          // normal case, just add this as the first entry
          pass_info_add(pass_info, t, dir, oi);
        }
      }

      // moving ahead to the next check point
      *st = propagate(*st, t, t+delta);
      t1 += delta;

      // we'll continue as long as we're finding overlap, even if
      // we pass the official "end time" (where we cross out of the
      // latitude range)
      is_overlap = oi != NULL;
      first = FALSE;
    }
    while (t1 < end_time || is_overlap);
    
    //if (n_backups > 0)
    //  printf("Had to back up %d times.\n", n_backups);

    int found = pass_info->num > 0;
    if (found)
      pass_collection_add(pc, pass_info);

    return found;
}

static void
find_crossings(BeamModeInfo *bmi, sat_t *sat, 
               double start_secs, double start_lat,
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
  stateVector st = tle_propagate(sat, start_secs);

  double lat_prev = -999;
  int iter=0;

  while (1) {
    ++iter;

    double lat, lon, llat, llon;
    get_target_latlon(&st, 0, &lat, &lon);
    get_target_latlon(&st, bmi->look_angle, &llat, &llon);
    if (delta==normal_delta)
      printf("lat: %f %f, lon: %f %f\n", lat, sat->ssplat, lon, sat->ssplon);

    if (iter==4) {
      printf("Iter 4\n");
    }

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
            st = tle_propagate(sat, curr);
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
    curr += delta;

    st = tle_propagate(sat, curr);
  }
}

static double seconds_from_stVec(meta_parameters *meta, int vecnum)
{
  julian_date jd;
  jd.year = meta->state_vectors->year;
  jd.jd = meta->state_vectors->julDay;

  hms_time hms;
  date_sec2hms(meta->state_vectors->second, &hms);
  return date2sec(&jd, &hms) + meta->state_vectors->vecs[vecnum].time;
}

//static void
//read_stVec_from_meta(const char *filename, stateVector *st, double *t)
//{
//  meta_parameters *meta = meta_read(filename);
//
//  *st = meta->state_vectors->vecs[0].vec;
//  *t = seconds_from_stVec(meta, 0);
//
//  meta_free(meta);
//}


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

  sat_t sat;
  read_tle(tle_filename, satellite, &sat);
  select_ephemeris(&sat);

  // no deep space orbits can be planned
  assert(sat.flags & DEEP_SPACE_EPHEM_FLAG);

  stateVector start_stVec = tle_propagate(&sat, start_secs);

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
  find_crossings(bmi, &sat, start_secs, start_lat, min_lat, max_lat,
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

  char dir1, dir2;
  if (first_is_ascending) {
    dir1 = 'A';
    dir2 = 'D';
  } else {
    dir1 = 'D';
    dir2 = 'A';
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
      stateVector st1 = tle_propagate(&sat, t);

      num_found +=
        check_crossing(pc, time1_in, time1_out, t, &st1, bmi,
                       zone, clat, clon, aoi, dir1);
    }

    if (check_second_crossing) {
      double t = curr + time2_in;
      stateVector st2 = tle_propagate(&sat, t);

      num_found +=
        check_crossing(pc, time2_in, time2_out, t, &st2, bmi,
                       zone, clat, clon, aoi, dir2);
    }

    st = tle_propagate(&sat, curr + full_cycle_time);
    curr += full_cycle_time;

    asfPercentMeter((curr-start_secs)/(end_secs-start_secs));
  }
  asfPercentMeter(1.0);

  *pc_out = pc;
  return num_found;
}

/*
static void test(double xpos, double ypos, double zpos,
                 double xvel, double yvel, double zvel)
{
  stateVector st;
  double lat, lon;

  st.pos.x = xpos;
  st.pos.y = ypos;
  st.pos.z = zpos;

  st.vel.x = xvel;
  st.vel.y = yvel;
  st.vel.z = zvel;

  get_target_latlon(&st, 0, &lat, &lon);
  printf("%12.2f %12.2f %12.2f %8.2f %8.2f\n",
         st.pos.x, st.pos.y, st.pos.z, lat, lon);
}

static void test_stuff()
{
  printf("         X            Y            Z       LAT      LON\n");

  double h = 6664139.068;

  // these don't matter
  double xv = 100;
  double yv = 100;
  double zv = 100;

  // circle in x/y plane with z=0
  double h2 = sqrt(h*h/2.);
  test(h, 0, 0, xv, yv, zv);
  test(h2, h2, 0, xv, yv, zv);
  test(0, h, 0, xv, yv, zv);
  test(-h2, h2, 0, xv, yv, zv);
  test(-h, 0, 0, xv, yv, zv);
  test(-h2, -h2, 0, xv, yv, zv);
  test(0, -h, 0, xv, yv, zv);
  test(h2, -h2, 0, xv, yv, zv);
  test(h, 0, 0, xv, yv, zv);
  printf("\n");

  // circle in z/y plane with x=0
  test(0, h, 0, xv, yv, zv);
  test(0, h2, h2, xv, yv, zv);
  test(0, 0, h, xv, yv, zv);
  test(0, -h2, h2, xv, yv, zv);
  test(0, -h, 0, xv, yv, zv);
  test(0, -h2, -h2, xv, yv, zv);
  test(0, 0, -h, xv, yv, zv);
  test(0, h2, -h2, xv, yv, zv);
  test(0, h, 0, xv, yv, zv);
  printf("\n");

  // circle at z=5000
  double z = 5000; 
  double h1 = sqrt(h*h-z*z);      // z^2 + h1^2 = h^2
  h2 = sqrt((h*h-z*z)/2.); // z^2 + h2^2 + h2^2 = h^2
  test(h1, 0, z, xv, yv, zv);
  test(h2, h2, z, xv, yv, zv);
  test(0, h1, z, xv, yv, zv);
  test(-h2, h2, z, xv, yv, zv);
  test(-h1, 0, z, xv, yv, zv);
  test(-h2, -h2, z, xv, yv, zv);
  test(0, -h1, z, xv, yv, zv);
  test(h2, -h2, z, xv, yv, zv);
  test(h1, 0, z, xv, yv, zv);
  printf("\n");

  // circle at z=h-5000
  z = h-5000; 
  h1 = sqrt(h*h-z*z);      // z^2 + h1^2 = h^2
  h2 = sqrt((h*h-z*z)/2.); // z^2 + h2^2 + h2^2 = h^2
  test(h1, 0, z, xv, yv, zv);
  test(h2, h2, z, xv, yv, zv);
  test(0, h1, z, xv, yv, zv);
  test(-h2, h2, z, xv, yv, zv);
  test(-h1, 0, z, xv, yv, zv);
  test(-h2, -h2, z, xv, yv, zv);
  test(0, -h1, z, xv, yv, zv);
  test(h2, -h2, z, xv, yv, zv);
  test(h1, 0, z, xv, yv, zv);
}
*/

/*
static void test_stuff2(const char *tle_filename, const char *satellite)
{
  meta_parameters *m1 = meta_read("o.meta");
  meta_parameters *m2 = meta_read("o2.meta");

  double st1_time = seconds_from_stVec(m1, 0);

  sat_t sat;
  read_tle(tle_filename, satellite, &sat);
  //select_ephemeris(&sat);

  stateVector st1 = m1->state_vectors->vecs[0].vec;
  stateVector st2 = m1->state_vectors->vecs[2].vec;

  stateVector st1_prop = tle_propagate(&sat, st1_time+15.403594971);

  printf("\n         Propagated       Actual\n");
  printf("pos.x  %12.2f %12.2f\n", st1_prop.pos.x, st2.pos.x);
  printf("pos.y  %12.2f %12.2f\n", st1_prop.pos.y, st2.pos.y);
  printf("pos.z  %12.2f %12.2f\n", st1_prop.pos.z, st2.pos.z);
  printf("vel.x  %12.2f %12.2f\n", st1_prop.vel.x, st2.vel.x);
  printf("vel.y  %12.2f %12.2f\n", st1_prop.vel.y, st2.vel.y);
  printf("vel.z  %12.2f %12.2f\n", st1_prop.vel.z, st2.vel.z);

  st2 = m2->state_vectors->vecs[0].vec;
  double st2_time = seconds_from_stVec(m2, 0);

  st1_prop = tle_propagate(&sat, st2_time);

  printf("\n         Propagated       Actual\n");
  printf("pos.x  %12.2f %12.2f\n", st1_prop.pos.x, st2.pos.x);
  printf("pos.y  %12.2f %12.2f\n", st1_prop.pos.y, st2.pos.y);
  printf("pos.z  %12.2f %12.2f\n", st1_prop.pos.z, st2.pos.z);
  printf("vel.x  %12.2f %12.2f\n", st1_prop.vel.x, st2.vel.x);
  printf("vel.y  %12.2f %12.2f\n", st1_prop.vel.y, st2.vel.y);
  printf("vel.z  %12.2f %12.2f\n", st1_prop.vel.z, st2.vel.z);
}
*/

int prop(const char *satellite, const char *beam_mode,
         const char *tle_filename, long startdate, long enddate,
         double **out_lat, double **out_lon, int *num)
{
  //test_stuff();
  //test_stuff2(tle_filename, satellite);
  //exit(1);

  double lat[32768], lon[32768];

  BeamModeInfo *bmi = get_beam_mode_info(satellite, beam_mode);
  if (bmi) {
    double start_secs = seconds_from_l(startdate);
    double end_secs = seconds_from_l(enddate);

    sat_t sat;
    read_tle(tle_filename, satellite, &sat);
    //select_ephemeris(&sat);

    // no deep space orbits can be planned
    assert((sat.flags & DEEP_SPACE_EPHEM_FLAG) == 0);

    // propagate to the starting time
    stateVector start_stVec = tle_propagate(&sat, start_secs);

    double curr = start_secs;
    stateVector st = start_stVec;
    int iter = 0;
    double delta = 6;

    while (curr < end_secs) {
      // use 0 for nadir
      //get_target_latlon(&st, 0, &lat[iter], &lon[iter]);
      lat[iter] = sat.ssplat;
      lon[iter] = sat.ssplon;

      if (iter%10==0) {
        julian_date jd;
        hms_time t;
        ymd_date d;
        sec2date(curr, &jd, &t);
        date_jd2ymd(&jd, &d);
        //printf("%5d %5.1f %12.2f %12.2f %12.2f %12.2f %8.2f %8.2f\n",
        printf("%5d %5.1f %12.2f %02d/%02d/%04d %0d:%02d:%04.1f %8.2f %8.2f\n",
               iter, curr-start_secs, secs_to_jul(curr),
               d.month, d.day, d.year, t.hour, t.min, t.sec,
               /*st.pos.x, st.pos.y, st.pos.z,*/ lat[iter], lon[iter]);
      }

      if (iter++ >= 32767) {
        printf("32767 points reached!  Stopping propagation.\n");
        break;
      }

      st = tle_propagate(&sat, curr+delta);
      curr += delta;
    }

    *num = iter;
    *out_lat = (double*) MALLOC(sizeof(double)*(iter+1));
    *out_lon = (double*) MALLOC(sizeof(double)*(iter+1));

    int i;
    for (i=0; i<iter; ++i) {
      (*out_lat)[i] = lat[i];
      (*out_lon)[i] = lon[i];
    }

    return TRUE;
  }
  else {
    return FALSE;
  }
}
