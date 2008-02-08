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

//static double seconds_from_stVec(meta_parameters *meta, int vecnum)
//{
//  julian_date jd;
//  jd.year = meta->state_vectors->year;
//  jd.jd = meta->state_vectors->julDay;
//
//  hms_time hms;
//  date_sec2hms(meta->state_vectors->second, &hms);
//  return date2sec(&jd, &hms) + meta->state_vectors->vecs[vecnum].time;
//}

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
         double clat, double clon, int pass_type, double lead_time,
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
  int num_found = 0;

  // Calculate the number of frames to include before we hit the
  // area of interest.  Add 1 (i.e., round up), but if user puts in
  // zero seconds, then we want 0 lead-up frames.
  int num_leadup_frames;
  if (lead_time > 0)
    num_leadup_frames = lead_time/incr + 1;
  else
    num_leadup_frames = 0;
  printf("Number of lead frames: %d\n", num_leadup_frames);

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

        int i;
        for (i=num_leadup_frames; i>0; --i) {
          double t = curr - i*incr;
          stateVector st1 = tle_propagate(&sat, t);
          Polygon *region = get_viewable_region(&st1, bmi, zone, clat, clon);
          OverlapInfo *oi1 = overlap_new(0, 1000, region, zone, clat, clon,
                                         &st1, t);
          pass_info_add(pass_info, t, dir, sat.orbit, sat.orbit_part*671, oi1);
        }

        while (curr < end_secs && oi) {
          pass_info_add(pass_info, curr, dir, sat.orbit, sat.orbit_part*671, oi);
          ++n;

          curr += incr;
          st = tle_propagate(&sat, curr);

          oi = overlap(curr, &st, bmi, zone, clat, clon, aoi);
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

  sat_t sat;
  read_tle(tle_filename, satellite, &sat);
  printf("TLE: %d %d\n", sat.tle.epoch_year, sat.tle.epoch_day);
  printf("StV: %d %d\n", m1->state_vectors->year, m1->state_vectors->julDay);

  int i;
  for (i=0; i<m1->state_vectors->num; ++i) {
    stateVector st1 = m1->state_vectors->vecs[i].vec;
    double st1_time = seconds_from_stVec(m1, i);
    
    stateVector st1_prop = tle_propagate(&sat, st1_time);
    
    printf("\n#%02d      Propagated       Actual     Error\n", i);
    printf("pos.x  %12.2f %12.2f %12.6f%%\n", st1_prop.pos.x, st1.pos.x,
           100.*fabs(st1_prop.pos.x-st1.pos.x)/st1.pos.x);
    printf("pos.y  %12.2f %12.2f %12.6f%%\n", st1_prop.pos.y, st1.pos.y,
           100.*fabs(st1_prop.pos.y-st1.pos.y)/st1.pos.y);
    printf("pos.z  %12.2f %12.2f %12.6f%%\n", st1_prop.pos.z, st1.pos.z,
           100.*fabs(st1_prop.pos.z-st1.pos.z)/st1.pos.z);
    printf("vel.x  %12.2f %12.2f %12.6f%%\n", st1_prop.vel.x, st1.vel.x,
           100.*fabs(st1_prop.vel.x-st1.vel.x)/st1.vel.x);
    printf("vel.y  %12.2f %12.2f %12.6f%%\n", st1_prop.vel.y, st1.vel.y,
           100.*fabs(st1_prop.vel.y-st1.vel.y)/st1.vel.y);
    printf("vel.z  %12.2f %12.2f %12.6f%%\n", st1_prop.vel.z, st1.vel.z,
           100.*fabs(st1_prop.vel.z-st1.vel.z)/st1.vel.z);
  }
}
*/
int prop(const char *satellite, const char *beam_mode,
         const char *tle_filename, long startdate, long enddate,
         double **out_lat, double **out_lon,
         double **out_llat, double **out_llon,
         int *num)
{
  //test_stuff();
  //test_stuff2(tle_filename, satellite);
  //exit(1);

  BeamModeInfo *bmi = get_beam_mode_info(satellite, beam_mode);
  if (bmi) {
    double start_secs = seconds_from_l(startdate);
    double end_secs = seconds_from_l(enddate);

    sat_t sat;
    read_tle(tle_filename, satellite, &sat);

    // no deep space orbits can be planned
    assert((sat.flags & DEEP_SPACE_EPHEM_FLAG) == 0);

    // propagate to the starting time
    stateVector start_stVec = tle_propagate(&sat, start_secs);

    double curr = start_secs;
    stateVector st = start_stVec;
    int iter = 0;
    double delta = 6;
    double lat[1024], lon[1024];
    double llat[1024], llon[1024];

    //float ang=0;
    //while (ang < bmi->look_angle) {
    //  double lat, lon;
    //  get_target_latlon(&st, ang, &lat, &lon);
    //  printf("%6.2f %6.2f %6.2f\n", ang*R2D, lat, lon);
    //  ang += .1*D2R;
    //}
    //exit(1);

    while (curr < end_secs) {

      // use 0 for nadir
      double lat0, lon0, llat0, llon0;
      get_target_latlon(&st, 0, &lat0, &lon0);
      get_target_latlon(&st, bmi->look_angle, &llat0, &llon0);

      if (iter%10==0) {
        julian_date jd;
        hms_time t;
        ymd_date d;
        sec2date(curr, &jd, &t);
        date_jd2ymd(&jd, &d);
        //printf("%5d %5.1f %12.2f %12.2f %12.2f %12.2f %8.2f %8.2f\n",
        printf("%5d %02d/%02d/%04d %2d:%02d:%02.0f "
               "%11.2f %11.2f %11.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %f\n",
               iter, d.month, d.day, d.year, t.hour, t.min, t.sec,
               st.pos.x, st.pos.y, st.pos.z,
               sat.ssplat, sat.ssplon,
               lat0, lon0, llat0, llon0,
               hypot(lat0-llat0,lon0-llon0));
      }

      if (iter%20==0) {
        lat[iter/20]=lat0;
        lon[iter/20]=lon0;
        llat[iter/20]=llat0;
        llon[iter/20]=llon0;
        if (iter/20>1022) {
          printf("Stopping... too many points.\n");
          break;
        }
      }

      ++iter;
      st = tle_propagate(&sat, curr+delta);
      curr += delta;
    }

    *num = iter/20;
    *out_lat = (double*) MALLOC(sizeof(double)*(iter+1));
    *out_lon = (double*) MALLOC(sizeof(double)*(iter+1));
    *out_llat = (double*) MALLOC(sizeof(double)*(iter+1));
    *out_llon = (double*) MALLOC(sizeof(double)*(iter+1));

    int i;
    for (i=0; i<iter; ++i) {
      (*out_lat)[i] = lat[i];
      (*out_lon)[i] = lon[i];
      (*out_llat)[i] = llat[i];
      (*out_llon)[i] = llon[i];
    }

    return TRUE;
  }
  else {
    return FALSE;
  }
}
