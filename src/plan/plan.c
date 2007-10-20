#include "plan.h"

#include "asf.h"
#include "asf_meta.h"

#include "date.h"
#include "dateUtil.h"
#include "beam_mode_table.h"

#include "asf_vector.h"

#include <stdlib.h>

typedef struct {
    double pct;
    Polygon *viewable_region;
    int utm_zone;
    stateVector state_vector;
    double t, clat, clon;
} OverlapInfo;

typedef struct {
    int num;
    OverlapInfo **overlaps;
} PassInfo;

PassInfo *pass_info_new()
{
  PassInfo *ret = MALLOC(sizeof(PassInfo));
  ret->num = 0;
  ret->overlaps = NULL;
  return ret;
}

void pass_info_add(PassInfo *pi, OverlapInfo *oi)
{
  pi->num += 1;
  
  OverlapInfo **overlaps = MALLOC(sizeof(OverlapInfo*)*(pi->num));
  
  int i;
  for (i=0; i<pi->num-1; ++i)
    overlaps[i] = pi->overlaps[i];
  overlaps[pi->num-1] = oi;
  pi->overlaps = overlaps;
}

void pass_info_free(PassInfo *pi)
{
  int i;
  for (i=0; i<pi->num-1; ++i)
    free(pi->overlaps[i]);
  free(pi->overlaps);
  free(pi);
}

static int iabs(int a)
{
  return a > 0 ? a : -a;
}

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
  vecCross(st->pos,v,&look_matrix[1]);
  vecCross(look_matrix[1],st->pos,&look_matrix[0]);
  vecMul(look_matrix,relPos,&relPos);

  // scale relPos so it reaches from s/c to targPos 
  vecScale(&relPos,sr);
  vecAdd(relPos,st->pos,targPos);
}

int get_target_latlon(stateVector *st, double look, double *tlat, double *tlon)
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

Polygon *
get_viewable_region(stateVector *st, BeamModeInfo *bmi,
                    double target_lat, double target_lon)
{
  int target_zone = utm_zone(target_lon);

  double center_lat, center_lon, center_x, center_y;
  get_target_latlon(st, bmi->look_angle, &center_lat, &center_lon);

  int zone = utm_zone(center_lon);

  // return NULL if we are "far away"
  if (iabs(zone - target_zone) > 1)
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
  double tilt = atan2(ahead_y - center_y, ahead_x - center_x);
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

int print_polys(Polygon *p1, Polygon *p2)
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

int overlap(double t, stateVector *st, BeamModeInfo *bmi,
            double clat, double clon, Polygon *aoi, OverlapInfo **overlap_info)
{
  Polygon *viewable_region = get_viewable_region(st, bmi, clat, clon);

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

    OverlapInfo *oi = MALLOC(sizeof(OverlapInfo));
    oi->pct = ((double)pct)/((double)n);
    oi->viewable_region = viewable_region;
    oi->utm_zone = utm_zone(clon);
    oi->state_vector = *st;
    oi->clat = clat;
    oi->clon = clon;
    oi->t = t;
    *overlap_info = oi;

    //print_polys(viewable_region, aoi);
    return TRUE;
  }
  else {
    // no overlap
    return FALSE;
  }
}

/*Extract date from the metadata-style given string:
instr="DD-MMM-YYYY, hh:mm:ss"
index  000000000011111111112
index  012345678901234567890
*/
static void parse_date(const char *inStr,ymd_date *date,hms_time *time)
{
  char mon[][5]= 
    {"","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
  char buf[100];
  int i,sec;
#define subStr(start,len,dest) strncpy(buf,&inStr[start],len);buf[len]=0;sscanf(buf,"%d",dest);
  subStr(7,4,&date->year);
  for (i=0; i<13; i++) {
    strncpy(buf, &inStr[3], 3);
    buf[3] = 0;
    if (strcmp_case(uc(buf), mon[i]) == 0)
      date->month = i;
  }
  subStr(0,2,&date->day);
  subStr(13,2,&time->hour);
  subStr(16,2,&time->min);
  subStr(19,2,&sec);
  time->sec=sec;
#undef subStr
}

const char *date_str(double s)
{
  char mon[][5]= 
    {"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"};
  julian_date jd;
  hms_time t;
  ymd_date d;
  static char buf[64];

  sec2date(s, &jd, &t);
  date_jd2ymd(&jd, &d);

  sprintf(buf, "%02d-%s-%4d, %02d:%02d:%02d", d.day, mon[d.month], d.year,
          t.hour, t.min, (int)(t.sec+.5));
  return buf;
}

static double seconds_from_s(const char *date_str)
{
  ymd_date d;
  hms_time t;
  parse_date(date_str, &d, &t);
  t.sec = 0;

  julian_date jd;
  date_ymd2jd(&d, &jd);

  return date2sec(&jd, &t);
}
         
static double seconds_from_l(long date)
{
  ymd_date d;
  long_to_date(date, &d.year, &d.month, &d.day);

  julian_date jd;
  date_ymd2jd(&d, &jd);

  hms_time t;
  t.hour = 0;
  t.min = 0;
  t.sec = 0.001;

  return date2sec(&jd, &t);
}

void kml_aoi(FILE *kml_file, double clat, double clon, Polygon *aoi)
{
  double lat_UL, lon_UL;
  double lat_UR, lon_UR;
  double lat_LL, lon_LL;
  double lat_LR, lon_LR;

  int z = utm_zone(clon);
  UTM2latLon(aoi->x[0], aoi->y[0], 0.0, z, &lat_UL, &lon_UL);
  UTM2latLon(aoi->x[1], aoi->y[1], 0.0, z, &lat_UR, &lon_UR);
  UTM2latLon(aoi->x[2], aoi->y[2], 0.0, z, &lat_LR, &lon_LR);
  UTM2latLon(aoi->x[3], aoi->y[3], 0.0, z, &lat_LL, &lon_LL);

  double lat_min=lat_UL, lat_max=lat_UL;
  double lon_min=lon_UL, lon_max=lon_UL;

  if (lat_UR < lat_min) lat_min = lat_UR;
  if (lat_LL < lat_min) lat_min = lat_LL;
  if (lat_LR < lat_min) lat_min = lat_LR;

  if (lat_UR > lat_max) lat_max = lat_UR;
  if (lat_LL > lat_max) lat_max = lat_LL;
  if (lat_LR > lat_max) lat_max = lat_LR;

  if (lon_UR < lon_min) lon_min = lon_UR;
  if (lon_LL < lon_min) lon_min = lon_LL;
  if (lon_LR < lon_min) lon_min = lon_LR;

  if (lon_UR > lon_max) lon_max = lon_UR;
  if (lon_LL > lon_max) lon_max = lon_LL;
  if (lon_LR > lon_max) lon_max = lon_LR;

  fprintf(kml_file, "<Placemark>\n");
  fprintf(kml_file, "  <description><![CDATA[\n");
  //fprintf(kml_file, "<strong>Area Of Interest</strong>\n");
  fprintf(kml_file, "<strong>Latitude Range</strong>: %5.1f to %5.1f<br>\n",
          lat_min, lat_max);
  fprintf(kml_file, "<strong>Longitude Range</strong>: %5.1f to %5.1f<br>\n",
          lon_min, lon_max);
  fprintf(kml_file, "  ]]></description>\n");
  fprintf(kml_file, "  <name>Area Of Interest</name>\n");
  fprintf(kml_file, "  <LookAt>\n");
  fprintf(kml_file, "    <longitude>%.10f</longitude>\n", clon);
  fprintf(kml_file, "    <latitude>%.10f</latitude>\n", clat);
  fprintf(kml_file, "    <range>400000</range>\n");
  fprintf(kml_file, "    <tilt>30</tilt>\n");
  fprintf(kml_file, "  </LookAt>\n");
  fprintf(kml_file, "  <visibility>1</visibility>\n");
  fprintf(kml_file, "  <open>1</open>\n");
  fprintf(kml_file, "  <Style>\n");
  fprintf(kml_file, "    <LineStyle>\n");
  fprintf(kml_file, "      <color>ff0033ff</color>\n");
  fprintf(kml_file, "      <width>3</width>\n");
  fprintf(kml_file, "    </LineStyle>\n");
  fprintf(kml_file, "    <PolyStyle>\n");
  //fprintf(kml_file, "      <color>1fff5500</color>\n");
  fprintf(kml_file, "      <color>1f0011ff</color>\n");
  fprintf(kml_file, "    </PolyStyle>\n");
  fprintf(kml_file, "  </Style>\n");
  fprintf(kml_file, "  <Polygon>\n");
  fprintf(kml_file, "    <extrude>1</extrude>\n");
  fprintf(kml_file, "    <altitudeMode>absolute</altitudeMode>\n");
  fprintf(kml_file, "    <outerBoundaryIs>\n");
  fprintf(kml_file, "      <LinearRing>\n");
  fprintf(kml_file, "        <coordinates>\n");
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UL, lat_UL);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_LL, lat_LL);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_LR, lat_LR);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UR, lat_UR);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UL, lat_UL);
  fprintf(kml_file, "        </coordinates>\n");
  fprintf(kml_file, "      </LinearRing>\n");
  fprintf(kml_file, "    </outerBoundaryIs>\n");
  fprintf(kml_file, "  </Polygon>\n");
  fprintf(kml_file, "</Placemark>\n");
}

void kml_overlap(FILE *kml_file, OverlapInfo *oi)
{
  double lat_UL, lon_UL;
  double lat_UR, lon_UR;
  double lat_LL, lon_LL;
  double lat_LR, lon_LR;

  UTM2latLon(oi->viewable_region->x[0], oi->viewable_region->y[0], 0.0,
             oi->utm_zone, &lat_UL, &lon_UL);
  UTM2latLon(oi->viewable_region->x[1], oi->viewable_region->y[1], 0.0,
             oi->utm_zone, &lat_UR, &lon_UR);
  UTM2latLon(oi->viewable_region->x[2], oi->viewable_region->y[2], 0.0,
             oi->utm_zone, &lat_LR, &lon_LR);
  UTM2latLon(oi->viewable_region->x[3], oi->viewable_region->y[3], 0.0,
             oi->utm_zone, &lat_LL, &lon_LL);

  fprintf(kml_file, "  <Polygon>\n");
  fprintf(kml_file, "    <extrude>1</extrude>\n");
  fprintf(kml_file, "    <altitudeMode>absolute</altitudeMode>\n");
  fprintf(kml_file, "    <outerBoundaryIs>\n");
  fprintf(kml_file, "      <LinearRing>\n");
  fprintf(kml_file, "        <coordinates>\n");
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UL, lat_UL);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_LL, lat_LL);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_LR, lat_LR);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UR, lat_UR);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UL, lat_UL);
  fprintf(kml_file, "        </coordinates>\n");
  fprintf(kml_file, "      </LinearRing>\n");
  fprintf(kml_file, "    </outerBoundaryIs>\n");
  fprintf(kml_file, "  </Polygon>\n");

  free(oi->viewable_region);
}

void found(FILE *kml_file, double t, double lat, double lon, PassInfo *pi)
{
  int i;

  fprintf(kml_file, "<Placemark>\n");
  fprintf(kml_file, "  <description><![CDATA[\n");
  fprintf(kml_file, "<strong>Time</strong>: %s<br>\n", date_str(t));
  fprintf(kml_file, "Contains %d frames<br><br>\n", pi->num);
  
  for (i=0; i<pi->num; ++i) {
    fprintf(kml_file, "  <strong>Frame %d</strong><br>\n", i+1);
    OverlapInfo *oi = pi->overlaps[i];
    fprintf(kml_file, "    Time: %s<br>    Overlap: %5.1f%%<br>\n",
            date_str(oi->t), oi->pct*100);
  }

  fprintf(kml_file, "  ]]></description>\n");
  fprintf(kml_file, "  <name>%s</name>\n", date_str(t));
  fprintf(kml_file, "  <LookAt>\n");
  fprintf(kml_file, "    <longitude>%.10f</longitude>\n", lon);
  fprintf(kml_file, "    <latitude>%.10f</latitude>\n", lat);
  fprintf(kml_file, "    <range>400000</range>\n");
  fprintf(kml_file, "    <tilt>30</tilt>\n");
  fprintf(kml_file, "  </LookAt>\n");
  fprintf(kml_file, "  <visibility>1</visibility>\n");
  fprintf(kml_file, "  <open>1</open>\n");
  fprintf(kml_file, "  <Style>\n");
  fprintf(kml_file, "    <LineStyle>\n");
  fprintf(kml_file, "      <color>ffff9900</color>\n");
  fprintf(kml_file, "      <width>3</width>\n");
  fprintf(kml_file, "    </LineStyle>\n");
  fprintf(kml_file, "    <PolyStyle>\n");
  fprintf(kml_file, "      <color>1fff5500</color>\n");
  fprintf(kml_file, "    </PolyStyle>\n");
  fprintf(kml_file, "  </Style>\n");
  fprintf(kml_file, "  <MultiGeometry>\n");

  for (i=0; i<pi->num; ++i) {
    kml_overlap(kml_file, pi->overlaps[i]);
  }

  fprintf(kml_file, "  </MultiGeometry>\n");
  fprintf(kml_file, "</Placemark>\n");

/*
      printf("Found one (#%d in this sequence):\n"
             "   Time: %s\n"
             "   State Vector: position= %f, %f, %f\n"
             "                 velocity= %f, %f, %f\n"
             "   Imaged area: zone= %d\n"
             "                %f %f\n"
             "                %f %f\n"
             "                %f %f\n"
             "                %f %f\n"
             "   Percentage: %f\n",
             n, date_str(t), st->pos.x, st->pos.y, st->pos.z,
             st->vel.x, st->vel.y, st->vel.z,
             oi->utm_zone,
             oi->viewable_region->x[0], oi->viewable_region->y[0],
             oi->viewable_region->x[1], oi->viewable_region->y[1],
             oi->viewable_region->x[2], oi->viewable_region->y[2],
             oi->viewable_region->x[3], oi->viewable_region->y[3],
             oi->pct);
*/
}

void plan(const char *satellite, const char *beam_mode,
          long startdate, long enddate, double min_lat, double max_lat,
          double clat, double clon, Polygon *aoi,
          meta_parameters *meta, const char *outFile)
{
  BeamModeInfo *bmi = get_beam_mode_info(satellite, beam_mode);
  if (!bmi)
    asfPrintError("Invalid satellite/beam mode.\n");

  double img_secs = seconds_from_s(meta->general->acquisition_date);

  double start_secs = seconds_from_l(startdate);
  double end_secs = seconds_from_l(enddate);

  const double normal_delta = 1;
  const double end_delta = .0002;
  double delta = normal_delta;

  stateVector start_stVec = propagate(meta->state_vectors->vecs[0].vec,
                                      img_secs, start_secs);

  printf("Target:\n"
         "  UTM:  zone=%d\n"
         "        %f %f\n"
         "        %f %f\n"
         "        %f %f\n"
         "        %f %f\n",
         utm_zone(clon),
         aoi->x[0], aoi->y[0],
         aoi->x[1], aoi->y[1],
         aoi->x[2], aoi->y[2],
         aoi->x[3], aoi->y[3]);

  FILE *ofp = FOPEN(outFile, "w");
  kml_header(ofp);
  kml_aoi(ofp, clat, clon, aoi);

  // new plan.
  // keep iterating until we circle the earth once
  // having done this, we have:
  //  -- 2 passes between the given latitude range (A&D)
  //  -- how long it takes to circle the earth
  double start_lat, start_lon;
  get_target_latlon(&start_stVec, 0, &start_lat, &start_lon);

  printf("Start latitude: %f\n", start_lat);

  double time1_in=-1, time1_out=-1,
    time2_in=-1, time2_out=-1, full_cycle_time=-1;
  int ncrossings_target=0, ncrossings_startlat=0;
  int in_target_lat_range = FALSE;

  double curr = start_secs;
  stateVector st = start_stVec;

  double lat_prev = -999;
  int iter=0;

  // First loop: don't even check for overlap
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
            full_cycle_time=curr-start_secs;
            break;
          } else {
            printf("     Refining full cycle time estimate.\n");
            printf("     Backing up to time = %f\n", curr-2*delta-start_secs);
            ncrossings_startlat = 1;
            curr -= 2*delta;
            delta = end_delta;
            lat=lat_prev;
            lat_prev=-999;
            st = propagate(start_stVec, start_secs, curr);
          }
        }
      }
    }

    if (lat_prev != -999) {
      if (((lat_prev < min_lat && lat > min_lat) ||
           (lat_prev > min_lat && lat < min_lat)) ||
          ((lat_prev < max_lat && lat > max_lat) ||
           (lat_prev > max_lat && lat < max_lat)))
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
            time1_in = curr-start_secs;
          } else if (ncrossings_target == 1) {
            time2_in = curr-start_secs;
          } else {
            printf("No way dude!\n");
          }
          ++ncrossings_target;
        } else {
          if (ncrossings_target == 1) {
            time1_out = curr-start_secs;
          } else if (ncrossings_target == 2) {
            time2_out = curr-start_secs;
          } else {
            printf("No way dude!\n");
          }
        }
      }
    }

    lat_prev = lat;
    // The first of these is much slower, theoretically more accurate
    // but from my experimentation not much more accurate...
    //st = propagate(start_stVec, start_secs, curr+delta);
    st = propagate(st, curr, curr+delta);

    curr += delta;
  }

  delta = 15; // eh?
  printf("Time to first target crossing: %f\n", time1_in);
  printf("Time to end of first target crossing: %f\n", time1_out);
  printf("Time to second target crossing: %f\n", time2_in);
  printf("Time to end of second target crossing: %f\n", time2_out);
  printf("Time for complete cycle: %f\n", full_cycle_time);

  curr = start_secs;
  st = start_stVec;

  // second loop: looking for overlaps
  while (curr < end_secs) {
    OverlapInfo *overlap_info;
    int num=0;
    int still_overlapping = FALSE;
    double pass_start_time = -1;

    double t1 = time1_in - 4;
    stateVector st1 = propagate(st, curr, t1+curr);
    PassInfo *pi = pass_info_new();

    while (t1 < time1_out || still_overlapping) {

      double t = t1 + curr;
      ++num;

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

      if (overlap(t, &st1, bmi, clat, clon, aoi, &overlap_info)) {
        if (pass_start_time == -1)
          pass_start_time = t;
        pass_info_add(pi, overlap_info);
        //found(ofp, num, curr, &st1, &overlap_info);
        still_overlapping=TRUE;
      } else {
        still_overlapping=FALSE;
      }

      st1 = propagate(st1, t, t+delta);
      //st1 = propagate(st, curr, t+delta);
      t1 += delta;
    }

    if (pass_start_time > 0)
      found(ofp, pass_start_time, clat, clon, pi);

    pass_info_free(pi);
    pi = pass_info_new();

    double t2 = time2_in - 4;
    stateVector st2 = propagate(st, curr, t2+curr);
    num = 0;
    still_overlapping = FALSE;
    pass_start_time = -1;

    while (t2 < time2_out || still_overlapping) {

      double t = t2 + curr;

      //{
      //  double lat, lon, llat, llon;
      //  get_target_latlon(&st2, 0, &lat, &lon);
      //  get_target_latlon(&st2, bmi->look_angle, &llat, &llon);
      //  printf("2: t=%s  Satellite location: "
      //         "%10.3f %10.3f\n",
      //         date_str(t), lat, lon);
      //  printf("2:                          Looking at        : "
      //         "%10.3f %10.3f\n",
      //         llat, llon);
      //}

      if (overlap(t, &st2, bmi, clat, clon, aoi, &overlap_info)) {
        if (pass_start_time == -1)
          pass_start_time = t;
        pass_info_add(pi, overlap_info);
        //found(ofp, num, curr, &st2, &overlap_info);
        still_overlapping=TRUE;
      } else {
        still_overlapping=FALSE;
      }

      st2 = propagate(st2, t, t+delta);
      //st2 = propagate(st, curr, t+delta);
      t2 += delta;
    }

    if (pass_start_time > 0)
      found(ofp, pass_start_time, clat, clon, pi);

    pass_info_free(pi);

    st = propagate(st, curr, curr+full_cycle_time);
    curr += full_cycle_time;
  }

  kml_footer(ofp);
  fclose(ofp);
}
