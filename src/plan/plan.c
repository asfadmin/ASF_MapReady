#include "plan.h"

#include "asf.h"
#include "asf_meta.h"

#include "date.h"
#include "dateUtil.h"
#include "beam_mode_table.h"

#include <stdlib.h>

typedef struct {
    double pct;
    Polygon *viewable_region;
    int utm_zone;
    stateVector state_vector;
} OverlapInfo;

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
                    double target_lat, double target_lon,
                    int *zone)
{
  int target_zone = utm_zone(target_lon);

  double center_lat, center_lon, center_x, center_y;
  get_target_latlon(st, bmi->look_angle, &center_lat, &center_lon);

  *zone = utm_zone(center_lon);

  // return NULL if we are "far away"
  if (iabs(*zone - target_zone) > 1)
    return NULL;
  if (fabs(center_lat - target_lat) > 20)
    return NULL;

  latLon2UTM_zone(center_lat, center_lon, 0, *zone, &center_x, &center_y);

  // location of a point just a bit ahead of us
  stateVector ahead_st = propagate(*st, 0, 1);
  double ahead_lat, ahead_lon, ahead_x, ahead_y;
  get_target_latlon(&ahead_st, bmi->look_angle, &ahead_lat, &ahead_lon);
  latLon2UTM_zone(ahead_lat, ahead_lon, 0, *zone, &ahead_x, &ahead_y);

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
            double clat, double clon, Polygon *aoi, OverlapInfo *overlap_info)
{
  int zone;
  Polygon *viewable_region = get_viewable_region(st, bmi, clat, clon, &zone);

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

    overlap_info->pct = ((double)pct)/((double)n);
    printf("Coverage: %d %d %f%%\n", pct, n, 100*overlap_info->pct);
    overlap_info->viewable_region = viewable_region;
    overlap_info->utm_zone = zone;
    overlap_info->state_vector = *st;

    print_polys(viewable_region, aoi);
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

void plan(const char *satellite, const char *beam_mode,
          long startdate, long enddate,
          double clat, double clon, Polygon *aoi,
          meta_parameters *meta, const char *outFile)
{
  BeamModeInfo *bmi = get_beam_mode_info(satellite, beam_mode);
  if (!bmi)
    asfPrintError("Invalid satellite/beam mode.\n");

  double img_secs = seconds_from_s(meta->general->acquisition_date);

  double start_secs = seconds_from_l(startdate);
  double end_secs = seconds_from_l(enddate);

  const double delta = 15; //hmm

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

  double curr = start_secs;
  while (curr < end_secs) {

    stateVector st = propagate(start_stVec, start_secs, curr);

    double lat, lon, llat, llon;
    get_target_latlon(&st, 0, &lat, &lon);
    get_target_latlon(&st, bmi->look_angle, &llat, &llon);

    //if (fabs(llat-clat)<1 && fabs(llon-clon)<1) {
    //  printf("t=%s  Satellite location: %10.3f %10.3f\n", date_str(curr),
    //         lat, lon);
    //  printf("                         Looking at        : %10.3f %10.3f\n",
    //         llat, llon);
    //}
    //printf("%f %f %f\n", curr-start_secs, llat, llon);

    OverlapInfo overlap_info;
    if (overlap(curr, &st, bmi, clat, clon, aoi, &overlap_info)) {
      printf("Found one:\n"
             "   Time: %s\n"
             "   State Vector: position= %f, %f, %f\n"
             "                 velocity= %f, %f, %f\n"
             "   Imaged area: zone= %d\n"
             "                %f %f\n"
             "                %f %f\n"
             "                %f %f\n"
             "                %f %f\n"
             "   Percentage: %f\n",
             date_str(curr), st.pos.x, st.pos.y, st.pos.z,
             st.vel.x, st.vel.y, st.vel.z,
             overlap_info.utm_zone,
             overlap_info.viewable_region->x[0],
             overlap_info.viewable_region->y[0],
             overlap_info.viewable_region->x[1],
             overlap_info.viewable_region->y[1],
             overlap_info.viewable_region->x[2],
             overlap_info.viewable_region->y[2],
             overlap_info.viewable_region->x[3],
             overlap_info.viewable_region->y[3],
             overlap_info.pct);

      free(overlap_info.viewable_region);
    }

    curr += delta;
  }
}
