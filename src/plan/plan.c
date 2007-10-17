#include "plan.h"

#include "asf.h"
#include "asf_meta.h"

#include "date.h"
#include "beam_mode_table.h"

void get_target_position(stateVector *st, double look, double yaw,
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
  vecCross(az,v,&look_matrix[1]);
  vecCross(ay,az,&look_matrix[0]);
  vecMul(look_matrix,relPos,&relPos);

  // scale relPos so it reaches from s/c to targPos 
  vecScale(&relPos,sr);
  vecAdd(relPos,st->pos,targPos);
}

int get_target_latlon(stateVector st, double look, double *lat, double *lon)
{
  // satellite height, from the state vector
  double ht = vecMagnitude(st.pos);

  // earth radius, calculate from WGS84 values, and approx latitude
  double re = 6378137.0;
  double rp = 6356752.31414;
  double lat = asin(st.pos.z/ht);
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
  cart2sph(target,er,&glat,lon);
  
  // lon should be -180,180
  // lat needs to be geodetic
  // both should be degrees
  *lon *= R2D; glat *= R2D;
  if (*lon < -180.) *lon += 360.;
  *lat = atan(tan(glat)*re*re/(rp*rp));
}

Polygon *get_viewable_region(stateVector st, BeamModeInfo *bmi)
{
  double center_lat, center_lon, center_x, center_y;
  int zone;

  get_target_latlon(st, bmi->look_angle, &center_lat, &center_lon);
  zone = utm_zone(center_lon);
  latLon2UTM_zone(center_lat, center_lon, zone, &center_x, &center_y);

  // location of a point just a bit ahead of us
  stateVector st_ahead = propagate(st, 0, 1);
  double ahead_lat, ahead_lon, ahead_x, ahead_y;
  get_target_latlon(ahead_st, bmi->look_angle, &ahead_lat, &ahead_lon);
  latLon2UTM_zone(ahead_lat, ahead_lon, zone, &ahead_x, &ahead_y);

  // now we know the orientation of the rectangle on the ground
  // ==> can find the 4 corners
  double tilt = atan2(ahead_y - center_y, ahead_x - center_x);
  double xl = bmi->length_m;
  double yl = bmi->width_m;

  double x[4], y[4];
  x[0] = center_x + cos(tilt)*xl/2. + sin(tilt)*yl/2.;
  y[0] = center_y + sin(tilt)*xl/2. + cos(tilt)*xl/2.;

  x[1] = x[0] - cos(tilt)*xl;
  y[1] = y[0] - sin(tilt)*xl;

  x[2] = x[1] + sin(tilt)*yl;
  y[2] = y[1] - cos(tilt)*yl;

  x[3] = x[2] + cos(tilt)*xl;
  y[3] = y[2] + sin(tilt)*xl;

  return polygon_new_closed(4, x, y);
}

int overlap(double t, stateVector st, BeamModeInfo *bmi,
            Polygon *aoi, OverlapInfo *overlap_info)
{
  
}
            
void plan(const char *satellite, const char *beam_mode,
          long startdate, long enddate, Polygon *aoi,
          meta_parameters *meta, const char *outFile)
{
  BeamModeInfo *bmi = get_beam_mode_info(satellite, beam_mode);
  if (!bmi)
    asfPrintError("Invalid satellite/beam mode.\n");

  double img_secs = something;
  double start_secs = time_diff(img_secs);
  double end_secs = time_diff(img_secs);
  const double delta = 15; //hmm

  stateVector start_stVec = propagate(meta->state_vectors->vecs[0],
                                      img_secs, start_secs);
  double curr = start_secs;
  while (curr < end_secs) {

    stateVector st = propagate(start_stVec, start_secs, curr);

    OverlapInfo overlap_info;
    if (overlap(curr, st, bmi, aoi, &overlap_info) {

    }

    curr += delta;
  }
}
