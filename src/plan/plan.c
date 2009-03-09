#include "plan.h"
#include "plan_internal.h"
#include "meta_project.h"

#include "asf.h"
#include "asf_meta.h"

#include "dateUtil.h"
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

static int is_utm(int zone)
{
  int ret = zone != -999 && zone != 999;
  if (ret) {
    assert(zone >= -60 && zone <= 60 && zone != 0);
  }
  return ret;
}

static Poly *
get_viewable_region(stateVector *st, BeamModeInfo *bmi, double look_angle,
                    int target_zone, double target_lat, double target_lon,
                    double *region_clat, double *region_clon)
{
  static const int verbose = FALSE;

  if (bmi->min_look_angle>0) {
    assert(look_angle >= D2R*(bmi->min_look_angle-.0001));
    assert(look_angle <= D2R*(bmi->max_look_angle+.0001));
  }

  double center_lat, center_lon, center_x, center_y;
  get_target_latlon(st, look_angle, &center_lat, &center_lon);

  // return NULL if we are "far away" (UTM only)
  if (is_utm(target_zone)) {
    int zone = utm_zone(center_lon);
    if (iabs(zone - target_zone) > 2)
      return NULL;
  }
  if (fabs(center_lat - target_lat) > 20)
    return NULL;

  ll2pr(center_lat, center_lon, target_zone, &center_x, &center_y);
  if (verbose) {
    printf("--------------------------------------------------------------\n");
    printf("center: x,y: %f %f lat,lon: %f %f\n",
           center_x, center_y, center_lat, center_lon);
  }

  // location of a point just a bit ahead of us
  stateVector ahead_st = propagate(*st, 0, 1);
  double ahead_lat, ahead_lon, ahead_x, ahead_y;
  get_target_latlon(&ahead_st, look_angle, &ahead_lat, &ahead_lon);
  ll2pr(ahead_lat, ahead_lon, target_zone, &ahead_x, &ahead_y);
  if (verbose)
    printf("ahead: x,y: %f %f lat,lon: %f %f\n",
           ahead_x, ahead_y, ahead_lat, ahead_lon);

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

  //x[0] = center_x + x_side1/2. - x_side2/2.;
  //y[0] = center_y + y_side1/2. - y_side2/2.;
  x[0] = center_x + x_side1/2. - x_side2/2.;
  y[0] = center_y + y_side1/2. + y_side2/2.;

  x[1] = x[0] - x_side1;
  y[1] = y[0] - y_side1;

  x[2] = x[1] + x_side2;
  y[2] = y[1] - y_side2;

  x[3] = x[2] + x_side1;
  y[3] = y[2] + y_side1;

  if (verbose) {
    int i;
    printf("corners:\n");
    double xa=0, ya=0;
    for (i=0; i<4; ++i) {
      double lat, lon;
      pr2ll(x[i],y[i],target_zone,&lat,&lon);
      printf("x,y: %f %f lat,lon: %f %f\n", x[i], y[i], lat, lon);
      xa += x[i]; ya += y[i];
    }
    xa/=4.; ya/=4.;
    double lata, lona;
    pr2ll(xa,ya,target_zone, &lata, &lona);
    printf("avg x,y: %f %f lat,lon: %f %f\n\n", xa, ya, lata, lona);
  }

  if (region_clat) *region_clat = center_lat;
  if (region_clon) *region_clon = center_lon;

  return polygon_new_closed(4, x, y);
}

static double random_in_interval(double lo, double hi)
{
  return lo + (hi-lo)*((double)rand() / ((double)(RAND_MAX)));
}

static OverlapInfo *
overlap(double t, stateVector *st, BeamModeInfo *bmi, double look_angle,
        int zone, double clat, double clon, Poly *aoi)
{
  Poly *viewable_region =
    get_viewable_region(st, bmi, look_angle, zone, clat, clon, NULL, NULL);

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
    polygon_free(viewable_region);
    return NULL;
  }
}

static void get_latitude_range(Poly *region, int zone, char dir,
                               double *start_lat, double *end_lat)
{
  double lat1, lat2, lon;

  double tcx = 0.5 * (region->x[1] + region->x[2]); // top center x
  double tcy = 0.5 * (region->y[1] + region->y[2]); // top center y
  pr2ll(tcx, tcy, zone, &lat1, &lon);

  double bcx = 0.5 * (region->x[0] + region->x[3]); // bottom center x
  double bcy = 0.5 * (region->y[0] + region->y[3]); // bottom center y
  pr2ll(bcx, bcy, zone, &lat2, &lon);

  if ((dir=='A' && lat1<lat2) || (dir=='D' && lat1>lat2)) {
    *start_lat = lat1;
    *end_lat = lat2;
  }
  else {
    *start_lat = lat2;
    *end_lat = lat1;
  }
}

int plan(const char *satellite, const char *beam_mode, double look_angle,
         long startdate, long enddate, double min_lat, double max_lat,
         double clat, double clon, int pass_type,
         int zone, Poly *aoi, const char *tle_filename,
         PassCollection **pc_out, char **errorstring)
{
  BeamModeInfo *bmi = get_beam_mode_info(satellite, beam_mode);
  if (!bmi) {
    *errorstring = STRDUP("Unknown satellite/beam mode combination.\n");
    return -1;
  }

  // Convert the input start/end times from longdates (dates as long ints)
  // to seconds from some reference time.  (midnight jan 1, 1900)
  // We add a day to the end date, so that the passed-in range is inclusive
  double start_secs = seconds_from_long(startdate);
  double end_secs = seconds_from_long(add_a_day(enddate));

  sat_t sat;
  read_tle(tle_filename, satellite, &sat);

  // if the planned acquisition period is further away than this amount,
  // we subtract multiples of the repeat cyle time until we are within
  // this threshold time of this tle
  double tle_time =
    time_to_secs(sat.tle.epoch_year, sat.tle.epoch_day, sat.tle.epoch_fod);

  // FIXME this is alos specific FIXME!!
  // should really read this out of a config file... or calculate it
  // from the TLE info?
  const double repeat_days = 46;
  const double orbits_per_cycle = 671;

  // repeat time, in seconds, all time references are in seconds
  double repeat_cycle_time = repeat_days * 24.*60.*60.;

  // how many days to pad the repeat cycle time, before modding the
  // start/end time
  const double days_of_padding = 5;

  // this is the cutoff, 
  double threshold = repeat_cycle_time + 24*60*60 * days_of_padding;

  int cycles_adjustment=0;
  if (start_secs-tle_time > threshold) {
    while (start_secs-tle_time > threshold) {
      start_secs -= repeat_cycle_time;
      end_secs -= repeat_cycle_time;
      ++cycles_adjustment;
    }
  }
  else if (tle_time-start_secs > threshold) { // planning backwards...
    while (tle_time-start_secs > threshold) {
      start_secs += repeat_cycle_time;
      end_secs += repeat_cycle_time;
      --cycles_adjustment;
    }
  }

  double time_adjustment = cycles_adjustment*repeat_cycle_time;
  if (cycles_adjustment != 0)
    asfPrintStatus("Adjusted start/end times %s by %d repeat cycle%s.\n",
           cycles_adjustment > 0 ? "forward" : "backward",
           cycles_adjustment > 0 ? cycles_adjustment : -cycles_adjustment,
           cycles_adjustment == 1 || cycles_adjustment == -1 ? "" : "s");

  // no deep space orbits can be planned
  assert((sat.flags & DEEP_SPACE_EPHEM_FLAG) == 0);

  if (is_utm(zone)) {
    asfPrintStatus("Target: UTM zone %d\n"
                   "  (%10.2f, %10.2f)  (%10.2f, %10.2f)\n"
                   "  (%10.2f, %10.2f)  (%10.2f, %10.2f)\n",
                   zone,
                   aoi->x[0], aoi->y[0],
                   aoi->x[1], aoi->y[1],
                   aoi->x[2], aoi->y[2],
                   aoi->x[3], aoi->y[3]);
  }
  else {
    asfPrintStatus("Target: Polar Stereo %s\n"
                   "  (%10.2f, %10.2f)  (%10.2f, %10.2f)\n"
                   "  (%10.2f, %10.2f)  (%10.2f, %10.2f)\n",
                   zone>0 ? "North" : "South",
                   aoi->x[0], aoi->y[0],
                   aoi->x[1], aoi->y[1],
                   aoi->x[2], aoi->y[2],
                   aoi->x[3], aoi->y[3]);
  }

  double curr = start_secs;
  double incr = bmi->image_time;
  stateVector st = tle_propagate(&sat, start_secs-incr);
  double lat_prev = sat.ssplat;
  int i,num_found = 0;

  // 
  // Calculate the number of frames to include before we hit the
  // area of interest.  Add 1 (i.e., round up), but if user puts in
  // zero seconds, then we want 0 lead-up frames.
  PassCollection *pc = pass_collection_new(clat, clon, aoi);

  asfPrintStatus("Searching...\n");
  while (curr < end_secs) {
    st = tle_propagate(&sat, curr);
    char dir = sat.ssplat > lat_prev ? 'A' : 'D';

    if ((dir=='A' && pass_type!=DESCENDING_ONLY) ||
        (dir=='D' && pass_type!=ASCENDING_ONLY))
    {
      OverlapInfo *oi =
        overlap(curr, &st, bmi, look_angle, zone, clat, clon, aoi);

      if (oi) {
        int n=0;

        // Calculate the orbit number -- we have to fudge this if we
        // modded the start time.
        int orbit_num = sat.orbit + orbits_per_cycle*cycles_adjustment;

        // This is an alternate way of calculating the orbit number that
        // was being used during testing... seems to produce numbers close
        // to (within 1) of the number obtained by sgpsdp...
        //double secs_per_orbit = repeat_cycle_time / orbits_per_cycle;
        // ALOS was launced on 1/24/06
        //double launch_secs = seconds_from_long(20060124);
        //double orbit_num2 = (curr - launch_secs) / secs_per_orbit;
        //orbit_num2 += orbits_per_cycle*cycles_adjustment;
        //printf("%f %f %f\n", orbit_num + sat.orbit_part,
        //       orbit_num2, orbit_num+sat.orbit_part-orbit_num2);

        // UPDATE!!  All this orbit number calculation business doesn't get
        // used for ALOS planning -- orbit number is re-calculated using
        // time since a refrence orbit.
        // See planner.c -- get_alos_orbit_number_at_time()

        PassInfo *pass_info = pass_info_new(orbit_num, sat.orbit_part, dir);
        double start_time = curr - bmi->num_buffer_frames*incr;

        // add on the buffer frames before the area of interest
        for (i=bmi->num_buffer_frames; i>0; --i) {
          double t = curr - i*incr;
          stateVector st1 = tle_propagate(&sat, t);
          double rclat, rclon; // viewable region center lat/lon

          Poly *region = get_viewable_region(&st1, bmi, look_angle,
                                             zone, clat, clon, &rclat, &rclon);

          if (region) {
            OverlapInfo *oi1 = overlap_new(0, 1000, region, zone, clat, clon,
                                           &st1, t);
            pass_info_add(pass_info, t+time_adjustment, oi1);

            if (pass_info->start_lat == -999) {
              // at the first valid buffer frame -- set starting latitude
              double start_lat, end_lat;
              get_latitude_range(region, zone, dir, &start_lat, &end_lat);
              pass_info_set_start_latitude(pass_info, start_lat);
            }
          }
        }

        // add the frames that actually image the area of interest
        while (curr < end_secs && oi) {
          pass_info_add(pass_info, curr+time_adjustment, oi);
          ++n;

          curr += incr;
          st = tle_propagate(&sat, curr);

          oi = overlap(curr, &st, bmi, look_angle, zone, clat, clon, aoi);
        }

        double end_time = curr + (bmi->num_buffer_frames-1)*incr;
        pass_info_set_duration(pass_info, end_time-start_time);

        // add on the buffer frames after the area of interest
        for (i=0; i<bmi->num_buffer_frames; ++i) {
          double t = curr + i*incr;
          stateVector st1 = tle_propagate(&sat, t);
          double rclat, rclon; // viewable region center lat/lon
          Poly *region = get_viewable_region(&st1, bmi, look_angle,
                                             zone, clat, clon, &rclat, &rclon);
          if (region) {
            OverlapInfo *oi1 = overlap_new(0, 1000, region, zone, clat, clon,
                                           &st1, t);
            pass_info_add(pass_info, t+time_adjustment, oi1);

            // set stopping latitude -- each frame overwrites the previous,
            // so the last valid frame will set the stopping latitude
            double start_lat, end_lat;
            get_latitude_range(region, zone, dir, &start_lat, &end_lat);
            pass_info_set_stop_latitude(pass_info, end_lat);
          }
        }

        // make sure we set all the required "after the fact" info
        // if not, then do not add the pass... must be invalid
        // (these used to be asserts, so it doesn't seem to ever happen)
        if (n>0 &&
            pass_info->start_lat != -999 &&
            pass_info->stop_lat != -999 &&
            pass_info->duration != -999)
        {
          // add the pass!
          pass_collection_add(pc, pass_info);
          ++num_found;
        }
        else
        {
          asfPrintStatus("Invalid pass found.  Skipped... \n"
                         " -- number of frames: %d, dir: %c\n"
                         " -- date: %s, orbit %d, %f\n --> (%f,%f,%f)\n",
                         n, pass_info->dir,
                         pass_info->start_time_as_string,
                         pass_info->orbit, pass_info->orbit_part,
                         pass_info->start_lat, pass_info->stop_lat,
                         pass_info->duration);
        }
      }
    }

    curr += incr;
    lat_prev = sat.ssplat;

    //printf("Lat: %f, Orbit: %d, Orbit Part: %f\n", sat.ssplat,
    //       (int)sat.orbit, sat.orbit_part);

    asfPercentMeter((curr-start_secs)/(end_secs-start_secs));
  }
  asfPercentMeter(1.0);

  *pc_out = pc;
  return num_found;
}

// Projection functions that replace UTM2latLon() and latLon2UTM()
// These, unlike those, always set the false northing value to 0,
// so they are guaranteed invertible.  Also, when zone is +-999, we
// use polar stereographic instead of utm... yeah, a kludge
void ll2pr(double lat, double lon, int zone, double *projX, double *projY)
{
  meta_projection *meta_proj = meta_projection_init();
  meta_proj->datum = WGS84_DATUM;

  project_parameters_t pps;

  if (zone == 999 || zone == -999) {
    pps.ps.is_north_pole = zone>0 ? 1 : 0;
    pps.ps.slat = zone>0 ? 70 : -70;
    pps.ps.slon = 150;
    pps.ps.false_easting = 0;
    pps.ps.false_northing = 0;

    meta_proj->type = POLAR_STEREOGRAPHIC;
  }
  else {
    pps.utm.zone = zone;
    pps.utm.scale_factor = 0.9996;
    pps.utm.lon0 = (double) (zone - 1) * 6.0 - 177.0;
    pps.utm.lat0 = 0.0;
    pps.utm.false_easting = 500000.0;
    pps.utm.false_northing = 0.0;

    meta_proj->type = UNIVERSAL_TRANSVERSE_MERCATOR;
  }

  meta_proj->param = pps;

  double projZ;
  latlon_to_proj(meta_proj, 'R', lat*D2R, lon*D2R, 0, projX, projY, &projZ);
  FREE(meta_proj);
}

void pr2ll(double projX, double projY, int zone, double *lat, double *lon)
{
  meta_projection *meta_proj = meta_projection_init();
  meta_proj->datum = WGS84_DATUM;

  project_parameters_t pps;

  if (zone == 999 || zone == -999) {
    pps.ps.is_north_pole = zone>0 ? 1 : 0;
    pps.ps.slat = zone>0 ? 70 : -70;
    pps.ps.slon = 150;
    pps.ps.false_easting = 0;
    pps.ps.false_northing = 0;

    meta_proj->type = POLAR_STEREOGRAPHIC;
  }
  else {
    pps.utm.zone = zone;
    pps.utm.scale_factor = 0.9996;
    pps.utm.lon0 = (double) (zone - 1) * 6.0 - 177.0;
    pps.utm.lat0 = 0.0;
    pps.utm.false_easting = 500000.0;
    pps.utm.false_northing = 0.0;

    meta_proj->type = UNIVERSAL_TRANSVERSE_MERCATOR;
  }

  meta_proj->param = pps;

  double h;
  proj_to_latlon(meta_proj, projX, projY, 0, lat, lon, &h);
  FREE(meta_proj);

  *lat *= R2D;
  *lon *= R2D;
}

int proj2lineSamp(meta_parameters *meta, int zone,
                  double proj_x, double proj_y,
                  double elev, double *line, double *samp)
{
  assert(meta->projection);

  double lat, lon, x, y, z;
  pr2ll(proj_x, proj_y, zone, &lat, &lon);

  if (meta->projection->type == LAT_LONG_PSEUDO_PROJECTION) {
    // we don't have to do anything in this projection, as the
    // values are already in lat/lon
    x = lon;
    y = lat;
    z = 0; // not used
  }
  else {
    latlon_to_proj(meta->projection, 'R', lat*D2R, lon*D2R, elev, &x, &y, &z);
  }

  *samp = (x - meta->projection->startX) / meta->projection->perX
           - meta->general->start_sample;
  *line = (y - meta->projection->startY) / meta->projection->perY
           - meta->general->start_line;

  return 0;
}
