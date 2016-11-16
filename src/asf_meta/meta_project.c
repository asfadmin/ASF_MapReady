#include "asf_meta.h"
#include "jpl_proj.h"
#include "asf_nan.h"
#include "meta_init_stVec.h"
#include "spheroids.h"
#include "libasf_proj.h"
#include "matrix.h"
#include <assert.h>

#define SQR(A) ((A)*(A))
#define ecc2(minor,major) (1.0 - ((minor*minor)/(major*major)))

/*Convert projection units (meters) to geodetic latitude and longitude (degrees).*/
void proj_to_latlon(meta_projection *proj, double x, double y, double z,
        double *lat, double *lon, double *height)
{
  if (proj==NULL)
    bail("NULL projection parameter structure passed to proj_to_latlon!\n");

  switch(proj->type)
    {
    case ALBERS_EQUAL_AREA:
      project_albers_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum);
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      project_lamaz_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum);
      break;
    case LAMBERT_CONFORMAL_CONIC:
      project_lamcc_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum);
      break;
    case POLAR_STEREOGRAPHIC:
      project_ps_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum);
      break;
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      project_utm_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum);
      break;
    case MERCATOR:
      project_mer_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum);
      break;
    case EQUI_RECTANGULAR:
      project_eqr_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum);
      break;
    case EQUIDISTANT:
      project_eqc_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum);
      break;
    case SINUSOIDAL:
      project_sin_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum);
      break;
    case SCANSAR_PROJECTION:
      asfPrintError("'proj_to_latlon' not defined for SCANSAR_PROJECTION.\n"
        "Use 'scan_latlon' instead.\n");
      break;
    case LAT_LONG_PSEUDO_PROJECTION:
      *lat = y*D2R;
      *lon = x*D2R;
      break;
    case STATE_PLANE: // temporary hack
      *lat = 0.0;
      *lon = 0.0;
      break;
    case UNKNOWN_PROJECTION:
    default:
      printf("Unrecognized map projection '%c' passed to proj_to_latlon!\n",
       proj->type);
      exit(1);
    }
}

void rotate_z(vector *v,double theta)
{
  double xNew,yNew;

  xNew = v->x*cosd(theta)+v->y*sind(theta);
  yNew = -v->x*sind(theta)+v->y*cosd(theta);
  v->x = xNew; v->y = yNew;
}

void rotate_y(vector *v,double theta)
{
  double xNew,zNew;

  zNew = v->z*cosd(theta)+v->x*sind(theta);
  xNew = -v->z*sind(theta)+v->x*cosd(theta);
  v->x = xNew; v->z = zNew;
}

void airsar_to_latlon(meta_parameters *meta,
                      double xSample, double yLine, double height,
                      double *lat, double *lon)
{
    if (!meta->airsar)
        asfPrintError("airsar_to_latlon() called with no airsar block!\n");

    const double a = 6378137.0;           // semi-major axis
    const double b = 6356752.3412;          // semi-minor axis
    const double e2 = 0.00669437999014;   // ellipticity
    const double e12 = 0.00673949674228;  // second eccentricity

    // we try to cache the matrices needed for the computation
    // this makes sure we don't reuse the cache incorrectly (i.e., on
    // data (=> an airsar block) which doesn't match what we cached for)
    static meta_airsar *cached_airsar_block = NULL;

    // these are the cached transformation parameters
    static matrix *m = NULL;
    static double ra=-999, o1=-999, o2=-999, o3=-999;

    if (!m)
        m = matrix_alloc(3,3); // only needs to be done once

    // if we aren't calculating with the exact same airsar block, we
    // need to recalculate the transformation block
    int recalc = !cached_airsar_block ||
        cached_airsar_block->lat_peg_point != meta->airsar->lat_peg_point ||
        cached_airsar_block->lon_peg_point != meta->airsar->lon_peg_point ||
        cached_airsar_block->head_peg_point != meta->airsar->head_peg_point;

    if (recalc) {
        // cache airsar block, so we can be sure we're not reusing
        // the stored data incorrectly
        if (cached_airsar_block)
            free(cached_airsar_block);
        cached_airsar_block = meta_airsar_init();
        *cached_airsar_block = *(meta->airsar);

        asfPrintStatus("Calculating airsar transformation parameters...\n");

        // now precalculate data
        double lat_peg = meta->airsar->lat_peg_point*D2R;
        double lon_peg = meta->airsar->lon_peg_point*D2R;
        double head_peg = meta->airsar->head_peg_point*D2R;
        double re = a / sqrt(1-e2*sin(lat_peg)*sin(lat_peg));
        double rn = (a*(1-e2)) / pow(1-e2*sin(lat_peg)*sin(lat_peg), 1.5);
        ra = (re*rn) / (re*cos(head_peg)*cos(head_peg)+rn*sin(head_peg)*sin(head_peg));

        matrix *m1, *m2;
        m1 = matrix_alloc(3,3);
        m2 = matrix_alloc(3,3);

        m1->coeff[0][0] = -sin(lon_peg);
        m1->coeff[0][1] = -sin(lat_peg)*cos(lon_peg);
        m1->coeff[0][2] = cos(lat_peg)*cos(lon_peg);
        m1->coeff[1][0] = cos(lon_peg);
        m1->coeff[1][1] = -sin(lat_peg)*sin(lon_peg);
        m1->coeff[1][2] = cos(lat_peg)*sin(lon_peg);
        m1->coeff[2][0] = 0.0;
        m1->coeff[2][1] = cos(lat_peg);
        m1->coeff[2][2] = sin(lat_peg);

        m2->coeff[0][0] = 0.0;
        m2->coeff[0][1] = sin(head_peg);
        m2->coeff[0][2] = -cos(head_peg);
        m2->coeff[1][0] = 0.0;
        m2->coeff[1][1] = cos(head_peg);
        m2->coeff[1][2] = sin(head_peg);
        m2->coeff[2][0] = 1.0;
        m2->coeff[2][1] = 0.0;
        m2->coeff[2][2] = 0.0;

        o1 = re*cos(lat_peg)*cos(lon_peg)-ra*cos(lat_peg)*cos(lon_peg);
        o2 = re*cos(lat_peg)*sin(lon_peg)-ra*cos(lat_peg)*sin(lon_peg);
        o3 = re*(1-e2)*sin(lat_peg)-ra*sin(lat_peg);

        matrix_mult(m,m1,m2);
        matrix_free(m1);
        matrix_free(m2);
    }

    // Make sure we didn't miss anything
    assert(ra != -999 && o1 != -999 && o2 != -999 && o3 != -999);

    //------------------------------------------------------------------
    // Now the actual computation, using the cached matrix etc

    // convenience aliases
    double c0 = meta->airsar->cross_track_offset;
    double s0 = meta->airsar->along_track_offset;
    double ypix = meta->general->y_pixel_size/meta->general->line_scaling;
    double xpix = meta->general->x_pixel_size/meta->general->sample_scaling;

    // radar coordinates
    double c_lat = (xSample*xpix+c0)/ra;
    double s_lon = (yLine*ypix+s0)/ra;

    //height += meta->airsar->gps_altitude;

    // radar coordinates in WGS84
    double t1 = (ra+height)*cos(c_lat)*cos(s_lon);
    double t2 = (ra+height)*cos(c_lat)*sin(s_lon);
    double t3 = (ra+height)*sin(c_lat);

    double c1 = m->coeff[0][0]*t1 + m->coeff[0][1]*t2 + m->coeff[0][2]*t3;
    double c2 = m->coeff[1][0]*t1 + m->coeff[1][1]*t2 + m->coeff[1][2]*t3;
    double c3 = m->coeff[2][0]*t1 + m->coeff[2][1]*t2 + m->coeff[2][2]*t3;

    // shift into local Cartesian coordinates
    double x = c1 + o1;// + 9.0;
    double y = c2 + o2;// - 161.0;
    double z = c3 + o3;// - 179.0;

    // local Cartesian coordinates into geographic coordinates
    double d = sqrt(x*x+y*y);
    double theta = atan2(z*a, d*b);
    *lat = R2D*atan2(z+e12*b*sin(theta)*sin(theta)*sin(theta),
                     d-e2*a*cos(theta)*cos(theta)*cos(theta));
    *lon = R2D*atan2(y, x);
}

void uavsar_to_latlon(meta_parameters *meta,
                      double xSample, double yLine, double height,
                      double *lat, double *lon)
{
    if (!meta->uavsar)
        asfPrintError("uavsar_to_latlon() called with no uavsar block!\n");

    const double a = 6378137.0;           // semi-major axis
    const double b = 6356752.3412;          // semi-minor axis
    const double e2 = 0.00669437999014;   // ellipticity
    const double e12 = 0.00673949674228;  // second eccentricity

    // we try to cache the matrices needed for the computation
    // this makes sure we don't reuse the cache incorrectly (i.e., on
    // data (=> an uavsar block) which doesn't match what we cached for)
    static meta_uavsar *cached_uavsar_block = NULL;

    // these are the cached transformation parameters
    static matrix *m = NULL;
    static double ra=-999, o1=-999, o2=-999, o3=-999;

    if (!m)
        m = matrix_alloc(3,3); // only needs to be done once

    // if we aren't calculating with the exact same airsar block, we
    // need to recalculate the transformation block
    int recalc = !cached_uavsar_block ||
        cached_uavsar_block->lat_peg_point != meta->uavsar->lat_peg_point ||
        cached_uavsar_block->lon_peg_point != meta->uavsar->lon_peg_point ||
        cached_uavsar_block->head_peg_point != meta->uavsar->head_peg_point;

    if (recalc) {
        // cache airsar block, so we can be sure we're not reusing
        // the stored data incorrectly
        if (cached_uavsar_block)
            free(cached_uavsar_block);
        cached_uavsar_block = meta_uavsar_init();
        *cached_uavsar_block = *(meta->uavsar);

        // now precalculate data
        double lat_peg = meta->uavsar->lat_peg_point*D2R;
        double lon_peg = meta->uavsar->lon_peg_point*D2R;
        double head_peg = meta->uavsar->head_peg_point*D2R;
        double re = a / sqrt(1-e2*sin(lat_peg)*sin(lat_peg));
        double rn = (a*(1-e2)) / pow(1-e2*sin(lat_peg)*sin(lat_peg), 1.5);
        ra = (re*rn) / (re*cos(head_peg)*cos(head_peg)+rn*sin(head_peg)*sin(head_peg));

        matrix *m1, *m2;
        m1 = matrix_alloc(3,3);
        m2 = matrix_alloc(3,3);

        m1->coeff[0][0] = -sin(lon_peg);
        m1->coeff[0][1] = -sin(lat_peg)*cos(lon_peg);
        m1->coeff[0][2] = cos(lat_peg)*cos(lon_peg);
        m1->coeff[1][0] = cos(lon_peg);
        m1->coeff[1][1] = -sin(lat_peg)*sin(lon_peg);
        m1->coeff[1][2] = cos(lat_peg)*sin(lon_peg);
        m1->coeff[2][0] = 0.0;
        m1->coeff[2][1] = cos(lat_peg);
        m1->coeff[2][2] = sin(lat_peg);

        m2->coeff[0][0] = 0.0;
        m2->coeff[0][1] = sin(head_peg);
        m2->coeff[0][2] = -cos(head_peg);
        m2->coeff[1][0] = 0.0;
        m2->coeff[1][1] = cos(head_peg);
        m2->coeff[1][2] = sin(head_peg);
        m2->coeff[2][0] = 1.0;
        m2->coeff[2][1] = 0.0;
        m2->coeff[2][2] = 0.0;

        o1 = re*cos(lat_peg)*cos(lon_peg)-ra*cos(lat_peg)*cos(lon_peg);
        o2 = re*cos(lat_peg)*sin(lon_peg)-ra*cos(lat_peg)*sin(lon_peg);
        o3 = re*(1-e2)*sin(lat_peg)-ra*sin(lat_peg);

        matrix_mult(m,m1,m2);
        matrix_free(m1);
        matrix_free(m2);
    }

    // Make sure we didn't miss anything
    assert(ra != -999 && o1 != -999 && o2 != -999 && o3 != -999);

    //------------------------------------------------------------------
    // Now the actual computation, using the cached matrix etc

    // convenience aliases
    double c0 = meta->uavsar->cross_track_offset;
    double s0 = meta->uavsar->along_track_offset;
    double ypix = meta->general->y_pixel_size/meta->general->line_scaling;
    double xpix = meta->general->x_pixel_size/meta->general->sample_scaling;

    // radar coordinates
    double c_lat = (xSample*xpix+c0)/ra;
    double s_lon = (yLine*ypix+s0)/ra;

    //height += meta->airsar->gps_altitude;

    // radar coordinates in WGS84
    double t1 = (ra+height)*cos(c_lat)*cos(s_lon);
    double t2 = (ra+height)*cos(c_lat)*sin(s_lon);
    double t3 = (ra+height)*sin(c_lat);

    double c1 = m->coeff[0][0]*t1 + m->coeff[0][1]*t2 + m->coeff[0][2]*t3;
    double c2 = m->coeff[1][0]*t1 + m->coeff[1][1]*t2 + m->coeff[1][2]*t3;
    double c3 = m->coeff[2][0]*t1 + m->coeff[2][1]*t2 + m->coeff[2][2]*t3;

    // shift into local Cartesian coordinates
    double x = c1 + o1;// + 9.0;
    double y = c2 + o2;// - 161.0;
    double z = c3 + o3;// - 179.0;

    // local Cartesian coordinates into geographic coordinates
    double d = sqrt(x*x+y*y);
    double theta = atan2(z*a, d*b);
    *lat = R2D*atan2(z+e12*b*sin(theta)*sin(theta)*sin(theta),
                     d-e2*a*cos(theta)*cos(theta)*cos(theta));
    *lon = R2D*atan2(y, x);
}

void alos_to_latlon(meta_parameters *meta,
        double xSample, double yLine, double z,
        double *lat, double *lon, double *height)
{
    assert(meta->transform);
    assert(meta->transform->parameter_count == 4 ||
           meta->transform->parameter_count == 10 ||
	   meta->transform->parameter_count == 25 ||
	   meta->transform->parameter_count == 45);

    double *x = meta->transform->x;
    double *y = meta->transform->y;

    if (z != 0.0) {
        // height correction applies directly to x (range direction)
        double incid = meta_incid(meta, yLine, xSample);

        // shift RIGHT in ascending images, LEFT in descending
        if (meta->general->orbit_direction=='A')
          xSample += z*tan(PI/2-incid)/meta->general->x_pixel_size;
        else
          xSample -= z*tan(PI/2-incid)/meta->general->x_pixel_size;
    }

    double i, j;
    if (meta->transform->parameter_count < 25) {
      i = xSample + 1;
      j = yLine + 1;
    }
    else {
      i = xSample;
      j = yLine;
    }

    // extended SAR data transformation
    if (meta->transform->parameter_count == 25) {
        i -= meta->transform->origin_pixel;
        j -= meta->transform->origin_line;
	double i2 = i*i;
	double j2 = j*j;
	double i3 = i2*i;
	double j3 = j2*j;
	double i4 = i2*i2;
	double j4 = j2*j2;
        *lon = y[0]*i4*j4 + y[1]*i4*j3 + y[2]*i4*j2 + y[3]*i4*j + y[4]*i4 +
               y[5]*i3*j4 + y[6]*i3*j3 + y[7]*i3*j2 + y[8]*i3*j + y[9]*i3 +
	       y[10]*i2*j4 + y[11]*i2*j3 + y[12]*i2*j2 + y[13]*i2*j + 
	       y[14]*i2 + y[15]*i*j4 + y[16]*i*j3 + y[17]*i*j2 + y[18]*i*j +
               y[19]*i + y[20]*j4 + y[21]*j3 + y[22]*j2 + y[23]*j + y[24];
        *lat = x[0]*i4*j4 + x[1]*i4*j3 + x[2]*i4*j2 + x[3]*i4*j + x[4]*i4 +
               x[5]*i3*j4 + x[6]*i3*j3 + x[7]*i3*j2 + x[8]*i3*j + x[9]*i3 +
	       x[10]*i2*j4 + x[11]*i2*j3 + x[12]*i2*j2 + x[13]*i2*j + 
	       x[14]*i2 + x[15]*i*j4 + x[16]*i*j3 + x[17]*i*j2 + x[18]*i*j +
               x[19]*i + x[20]*j4 + x[21]*j3 + x[22]*j2 + x[23]*j + x[24];
    }
    // optical data transformation
    else if (meta->transform->parameter_count == 10) {
	double i2 = i*i;
	double j2 = j*j;
	double i3 = i2*i;
	double j3 = j2*j;
        *lat = y[0] + y[1]*i + y[2]*j + y[3]*i*j + y[4]*i2 + y[5]*j2 +
               y[6]*i2*j + y[7]*i*j2 + y[8]*i3 + y[9]*j3;
        *lon = x[0] + x[1]*i + x[2]*j + x[3]*i*j + x[4]*i2 + x[5]*j2 +
               x[6]*i2*j + x[7]*i*j2 + x[8]*i3 + x[9]*j3;
    }
    // SAR data transformation
    else if (meta->transform->parameter_count == 4) {
        *lat = y[0] + y[1]*j + y[2]*i + y[3]*i*j;
        *lon = x[0] + x[1]*j + x[2]*i + x[3]*i*j;
    }
    else if (meta->transform->parameter_count == 45) {
        j = xSample - meta->transform->origin_pixel;
        i = yLine - meta->transform->origin_line;
	double i2 = i*i;
	double j2 = j*j;
	double i3 = i2*i;
	double j3 = j2*j;
	double i4 = i2*i2;
	double j4 = j2*j2;
        *lon = y[0] +
               y[1]*i + y[2]*j +
               y[3]*i*i + y[4]*i*j + y[5]*j*j +
               y[6]*i*i*i +
	       y[7]*i*i*j +
	       y[8]*i*j*j +
	       y[9]*j*j*j +
	       y[10]*i*i*i*i +
	       y[11]*i*i*i*j +
	       y[12]*i*i*j*j +
	       y[13]*i*j*j*j +
	       y[14]*j*j*j*j +
	       y[15]*i3*i*j +
	       y[16]*i3*j*j +
	       y[17]*i*i*j3 +
	       y[18]*i*j*j3 +
	       y[19]*i3*i*j*j +
	       y[20]*i3*j3 +
	       y[21]*i*i*j*j3 +
	       y[22]*i3*i*j3 +
	       y[23]*i3*j*j3 +
	       y[24]*i3*i*j*j3 +
	       y[25]*i3*i*i +
	       y[26]*j3*j*j +
	       y[27]*i3*i3 +
	       y[28]*i3*i*i*j +
	       y[29]*j3*j3 +
	       y[30]*i*j*j*j3 +
	       y[31]*i3*i*i3 +
	       y[32]*i3*i3*j +
	       y[33]*i3*i*i*j*j +
	       y[34]*i*i*j*j*j3 +
	       y[35]*i*j3*j3 +
	       y[36]*j3*j*j3 +
	       y[37]*i3*i3*i*i +
	       y[38]*i3*i3*i*j +
	       y[39]*i3*i3*j*j +
	       y[40]*i3*j3*i*i +
	       y[41]*j3*j3*j*j +
	       y[42]*j3*j3*j*i +
	       y[43]*j3*j3*i*i +
	       y[44]*i3*j3*j*j;
        *lat = x[0] +
               x[1]*i + x[2]*j +
               x[3]*i*i + x[4]*i*j + x[5]*j*j +
               x[6]*i*i*i +
	       x[7]*i*i*j +
	       x[8]*i*j*j +
	       x[9]*j*j*j +
	       x[10]*i*i*i*i +
	       x[11]*i*i*i*j +
	       x[12]*i*i*j*j +
	       x[13]*i*j*j*j +
	       x[14]*j*j*j*j +
	       x[15]*i3*i*j +
	       x[16]*i3*j*j +
	       x[17]*i*i*j3 +
	       x[18]*i*j*j3 +
	       x[19]*i3*i*j*j +
	       x[20]*i3*j3 +
	       x[21]*i*i*j*j3 +
	       x[22]*i3*i*j3 +
	       x[23]*i3*j*j3 +
	       x[24]*i3*i*j*j3 +
	       x[25]*i3*i*i +
	       x[26]*j3*j*j +
	       x[27]*i3*i3 +
	       x[28]*i3*i*i*j +
	       x[29]*j3*j3 +
	       x[30]*i*j*j*j3 +
	       x[31]*i3*i*i3 +
	       x[32]*i3*i3*j +
	       x[33]*i3*i*i*j*j +
	       x[34]*i*i*j*j*j3 +
	       x[35]*i*j3*j3 +
	       x[36]*j3*j*j3 +
	       x[37]*i3*i3*i*i +
	       x[38]*i3*i3*i*j +
	       x[39]*i3*i3*j*j +
	       x[40]*i3*j3*i*i +
	       x[41]*j3*j3*j*j +
	       x[42]*j3*j3*j*i +
	       x[43]*j3*j3*i*i +
	       x[44]*i3*j3*j*j;
    }
    else {
        asfPrintError("Unsupported parameter_count in transform block: %d", meta->transform->parameter_count);
    }

    *height = z;  // FIXME: Do we need to correct the height at all?
}

void scan_to_latlon(meta_parameters *meta,
        double x, double y, double z,
        double *lat_d, double *lon, double *height)
{
  double qlat, qlon;
  double lat,radius;
  vector pos;
  meta_projection *proj = meta->projection;

  if (z != 0.0) {
    // height correction applies directly to y (range direction)
    double line, samp;
    line = (y-proj->startY)/proj->perY - meta->general->start_line;
    samp = (x-proj->startX)/proj->perX - meta->general->start_sample;
    double sr = meta_get_slant(meta,line,samp);
    double er = proj->param.atct.rlocal;
    double ht = meta_get_sat_height(meta,line,samp);
    double cos_ang = (sr*sr + er*er - ht*ht)/(2.0*sr*er);
    if (cos_ang > 1) cos_ang = 1;
    if (cos_ang < -1) cos_ang = -1;
    double incid = PI-acos(cos_ang);
    x += z*tan(PI/2-incid);
  }

  if (meta->sar->look_direction=='R')
    qlat = -x/proj->param.atct.rlocal; /* Right looking sar */
  else
    qlat =  x/proj->param.atct.rlocal; /* Left looking sar */
  qlon = y/(proj->param.atct.rlocal*cos(qlat));

  sph2cart(proj->param.atct.rlocal, qlat, qlon, &pos);

  rotate_z(&pos,-proj->param.atct.alpha3);
  rotate_y(&pos,-proj->param.atct.alpha2);
  rotate_z(&pos,-proj->param.atct.alpha1);

  cart2sph(pos,&radius,&lat,lon);
  *lon *= R2D;
  lat *= R2D;
  *lat_d = atand(tand(lat) / (1-ecc2(proj->re_minor,proj->re_major)));
  *height = z;  // FIXME: Do we need to correct the height at all?
}

void location_to_latlon(meta_parameters *meta,
			double x, double y, double z,
			double *lat_d, double *lon, double *height)
{
  double x_ul, y_ul, x_ur, y_ur, x_ll, y_ll, x_lr, y_lr;
  
  // Define the UTM zone to make sure everything happens to make sure that
  // everything happens in the right coordinate space.
  int zone = utm_zone(meta->general->center_longitude);

  // Get corner coordinates into UTM
  latLon2UTM_zone(meta->location->lat_start_near_range,
		  meta->location->lon_start_near_range, 0.0, zone,
		  &x_ul, &y_ul);
  latLon2UTM_zone(meta->location->lat_start_far_range,
		  meta->location->lon_start_far_range, 0.0, zone,
		  &x_ur, &y_ur);
  latLon2UTM_zone(meta->location->lat_end_near_range,
		  meta->location->lon_end_near_range, 0.0, zone,
		  &x_ll, &y_ll);
  latLon2UTM_zone(meta->location->lat_end_far_range,
		  meta->location->lon_end_far_range, 0.0, zone,
		  &x_lr, &y_lr);

  // Bilinear interpolation to determine line/sample position in UTM coords
  double x_scale = x / meta->general->sample_count;
  double y_scale = y / meta->general->line_count;
  double x_up = x_ul + x_scale*(x_ur - x_ul);
  double y_up = y_ul + x_scale*(y_ur - y_ul);
  double x_down = x_ll + x_scale*(x_lr - x_ll);
  double y_down = y_ll + x_scale*(y_lr - y_ll);
  double x_sample = x_up + y_scale*(x_down - x_up);
  double y_line = y_up + y_scale*(y_down - y_up);

  // Convert from UTM coordinates back to lat/lon and call it a day
  UTM2latLon(x_sample, y_line, 0.0, zone, lat_d, lon);
  *height = 0.0;
}

static void ll_ac(meta_projection *proj, char look_dir, double lat_r, double lon, double *c1, double *c2)
{
  double qlat, qlon;
  double lat,radius;
  vector pos;

  lat = atan(tan(lat_r)*(1 - ecc2(proj->re_minor,proj->re_major)));
  sph2cart(proj->param.atct.rlocal,lat,lon,&pos);

  rotate_z(&pos,proj->param.atct.alpha1);
  rotate_y(&pos,proj->param.atct.alpha2);
  rotate_z(&pos,proj->param.atct.alpha3);

  cart2sph(pos,&radius,&qlat,&qlon);

  *c1 = qlon*proj->param.atct.rlocal*cos(qlat);
  if (look_dir=='R')
    *c2 = -1.0*qlat*proj->param.atct.rlocal;  /* right looking */
  else
    *c2 = qlat * proj->param.atct.rlocal;   /* left looking */
}


/*Convert projection units (meters) from geodetic latitude and longitude (degrees).*/
void latlon_to_proj(meta_projection *proj, char look_dir,
        double lat, double lon, double height,
        double *x, double *y, double *z)
{
  double geoc_lat;

  if (proj==NULL)
    bail("NULL projection parameter structure passed to ll_to_proj!\n");

  switch (proj->type)
    {
    case SCANSAR_PROJECTION:
      ll_ac(proj, look_dir, lat, lon, y, x);
      break;
    case ALBERS_EQUAL_AREA:
      project_albers(&(proj->param), lat, lon, height, x, y, z, proj->datum);
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      project_lamaz(&(proj->param), lat, lon, height, x, y, z, proj->datum);
      break;
    case LAMBERT_CONFORMAL_CONIC:
      project_lamcc(&(proj->param), lat, lon, height, x, y, z, proj->datum);
      break;
    case POLAR_STEREOGRAPHIC:
      project_ps(&(proj->param), lat, lon, height, x, y, z, proj->datum);
      break;
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      project_utm(&(proj->param), lat, lon, height, x, y, z, proj->datum);
      break;
    case MERCATOR:
      project_mer(&(proj->param), lat, lon, height, x, y, z, proj->datum);
      break;
    case EQUI_RECTANGULAR:
      // Some special treatment required for PROJ4 limitation
      geoc_lat = atan(tan(lat)/(1-ecc2(proj->re_minor,proj->re_major)));
      project_eqr(&(proj->param), geoc_lat, lon, height, x, y, z, proj->datum);
      break;
    case EQUIDISTANT:
      // Some special treatment required for PROJ4 limitation
      geoc_lat = atan(tan(lat)/(1-ecc2(proj->re_minor,proj->re_major)));
      project_eqc(&(proj->param), geoc_lat, lon, height, x, y, z, proj->datum);
      break;
    case SINUSOIDAL:
      project_sin(&(proj->param), lat, lon, height, x, y, x, proj->datum);
      break;
    case LAT_LONG_PSEUDO_PROJECTION:
      *x = lon*R2D;
      *y = lat*R2D;
      *z = height;
      break;
    default:
      printf("Unrecognized map projection '%c' passed to latlon_to_proj!\n",
       proj->type);
      exit(1);
    }
}

void fill_in_utm(double lat, double lon, project_parameters_t *pps)
{
  pps->utm.zone = utm_zone(lon);
  pps->utm.scale_factor = 0.9996;
  pps->utm.lon0 = (double) (pps->utm.zone - 1) * 6.0 - 177.0;
  pps->utm.lat0 = 0.0;
  pps->utm.false_easting = 500000.0;
  if (lat > 0.0)
    pps->utm.false_northing = 0.0;
  else
    pps->utm.false_northing = 10000000.0;
}

static void latLon2proj_imp(double lat, double lon, double elev,
                            char *projFile, double *projX, double *projY)
{
  project_parameters_t pps;
  projection_type_t proj_type;
  datum_type_t datum = WGS84_DATUM;
  spheroid_type_t spheroid = WGS84_SPHEROID;
  meta_projection *meta_proj;
  double projZ;

  if (projFile)
  {
      // Read projection file
    read_proj_file(projFile, &pps, &proj_type, &datum, &spheroid);

      // Report the conversion type
      switch(proj_type)
      {
          case ALBERS_EQUAL_AREA:
              asfPrintStatus("Lat/Lon to Albers Equal Area\n\n");
              break;
          case LAMBERT_AZIMUTHAL_EQUAL_AREA:
              asfPrintStatus("Lat/Lon to Lambert Azimuthal Equal Area\n\n");
              break;
          case LAMBERT_CONFORMAL_CONIC:
              asfPrintStatus("Lat/Lon to Lambert Conformal Conic\n\n");
              break;
          case POLAR_STEREOGRAPHIC:
              asfPrintStatus("Lat/Lon to Polar Stereographic\n\n");
              break;
          case UNIVERSAL_TRANSVERSE_MERCATOR:
              asfPrintStatus("Lat/Lon to UTM\n\n");
              break;
          case MERCATOR:
              asfPrintStatus("Lat/Lon to Mercator\n\n");
              break;
          case EQUI_RECTANGULAR:
              asfPrintStatus("Lat/Lon to Equirectangular\n\n");
              break;
          case EQUIDISTANT:
              asfPrintStatus("Lat/Lon to Equidistant\n\n");
              break;
          case SINUSOIDAL:
              asfPrintStatus("Lat/Lon to Sinusoidal\n\n");
              break;
          case STATE_PLANE:
              // Not implemented.
              assert (0);
              break;
          case SCANSAR_PROJECTION:
              // Not implemented.
              assert (0);
              break;
          case LAT_LONG_PSEUDO_PROJECTION:
              // Not implemented.
              assert (0);
              break;
          default:
              assert (0);   // Shouldn't be here.
              break;
      }
  }
  else
  {
      proj_type = UNIVERSAL_TRANSVERSE_MERCATOR;
  }

  // Initialize meta_projection block
  meta_proj = meta_projection_init();
  meta_proj->type = proj_type;
  meta_proj->datum = datum;
  meta_proj->spheroid = spheroid;

  if (proj_type == UNIVERSAL_TRANSVERSE_MERCATOR)
    fill_in_utm(lat, lon, &meta_proj->param);
  else
    meta_proj->param = pps;
  latlon_to_proj(meta_proj, 'R', lat*D2R, lon*D2R, elev, projX, projY, &projZ);
}

void latLon2UTM(double lat, double lon, double elev,
                double *projX, double *projY)
{
    latLon2proj_imp(lat, lon, elev, NULL, projX, projY);
}

void latLon2UTM_zone(double lat, double lon, double elev, int zone,
                double *projX, double *projY)
{
  project_parameters_t pps;
  meta_projection *meta_proj;
  double projZ;

  // Initialize meta_projection block
  meta_proj = meta_projection_init();
  meta_proj->type = UNIVERSAL_TRANSVERSE_MERCATOR;
  meta_proj->datum = WGS84_DATUM;
  pps.utm.zone = zone;
  pps.utm.scale_factor = 0.9996;
  pps.utm.lon0 = (double) (zone - 1) * 6.0 - 177.0;
  pps.utm.lat0 = 0.0;
  pps.utm.false_easting = 500000.0;
  pps.utm.false_northing = lat>0 ? 0.0 : 10000000.0;
  meta_proj->param = pps;
  latlon_to_proj(meta_proj, 'R', lat*D2R, lon*D2R, elev, projX, projY, &projZ);
}

void latLon2proj(double lat, double lon, double elev, char *projFile,
     double *projX, double *projY)
{
    asfRequire(projFile != NULL, "A projection file is required.\n");
    latLon2proj_imp(lat, lon, elev, projFile, projX, projY);
}

void UTM2latLon(double projX, double projY, double elev, int zone,
                double *lat, double *lon)
{
  project_parameters_t pps;
  projection_type_t proj_type;
  meta_projection *meta_proj;
  double h;

  proj_type = UNIVERSAL_TRANSVERSE_MERCATOR;

  pps.utm.zone = zone;
  pps.utm.scale_factor = 0.9996;
  pps.utm.lon0 = (double) (zone - 1) * 6.0 - 177.0;
  pps.utm.lat0 = 0.0;
  pps.utm.false_easting = 500000.0;
  pps.utm.false_northing = 0.0;

  // Initialize meta_projection block
  meta_proj = meta_projection_init();
  meta_proj->type = proj_type;
  meta_proj->datum = WGS84_DATUM; // assumed...
  meta_proj->param = pps;

  proj_to_latlon(meta_proj, projX, projY, elev, lat, lon, &h);

  *lat *= R2D;
  *lon *= R2D;
}

void EQR2latLon(double projX, double projY, double *lat, double *lon)
{
  project_parameters_t pps;
  projection_type_t proj_type;
  meta_projection *meta_proj;
  double h;

  proj_type = EQUI_RECTANGULAR;

  pps.eqr.central_meridian = 0.0;
  pps.eqr.orig_latitude = 0.0;
  pps.eqr.false_easting = 0.0;
  pps.eqr.false_northing = 0.0;

  // Initialize meta_projection block
  meta_proj = meta_projection_init();
  meta_proj->type = proj_type;
  meta_proj->datum = WGS84_DATUM; // assumed...
  meta_proj->param = pps;

  proj_to_latlon(meta_proj, projX, projY, 0.0, lat, lon, &h);

  *lat *= R2D;
  *lon *= R2D;

  free(meta_proj);
}

void to_radians(projection_type_t pt, project_parameters_t * pps)
{
    switch (pt)
    {
  case UNIVERSAL_TRANSVERSE_MERCATOR:
            if (!ISNAN(pps->utm.lon0))
                pps->utm.lon0 *= D2R;
            if (!ISNAN(pps->utm.lat0))
                pps->utm.lat0 *= D2R;

            break;

  case POLAR_STEREOGRAPHIC:
            if (!ISNAN(pps->ps.slon))
                pps->ps.slon *= D2R;
            if (!ISNAN(pps->ps.slat))
                pps->ps.slat *= D2R;

            break;

  case ALBERS_EQUAL_AREA:
            if (!ISNAN(pps->albers.center_meridian))
                pps->albers.center_meridian *= D2R;
            if (!ISNAN(pps->albers.orig_latitude))
                pps->albers.orig_latitude *= D2R;
            if (!ISNAN(pps->albers.std_parallel1))
                pps->albers.std_parallel1 *= D2R;
            if (!ISNAN(pps->albers.std_parallel2))
                pps->albers.std_parallel2 *= D2R;

            break;

  case LAMBERT_AZIMUTHAL_EQUAL_AREA:
            if (!ISNAN(pps->lamaz.center_lat))
                pps->lamaz.center_lat *= D2R;
            if (!ISNAN(pps->lamaz.center_lon))
                pps->lamaz.center_lon *= D2R;

            break;

  case LAMBERT_CONFORMAL_CONIC:
            if (!ISNAN(pps->lamcc.plat1))
                pps->lamcc.plat1 *= D2R;
            if (!ISNAN(pps->lamcc.plat2))
                pps->lamcc.plat2 *= D2R;
            if (!ISNAN(pps->lamcc.lat0))
                pps->lamcc.lat0 *= D2R;
            if (!ISNAN(pps->lamcc.lon0))
                pps->lamcc.lon0 *= D2R;

            break;

    case MERCATOR:
      if (!ISNAN(pps->mer.standard_parallel))
	pps->mer.standard_parallel *= D2R;
      if (!ISNAN(pps->mer.central_meridian))
	pps->mer.central_meridian *= D2R;
      if (!ISNAN(pps->mer.orig_latitude))
	pps->mer.orig_latitude *= D2R;

      break;

    case EQUI_RECTANGULAR:
      if (!ISNAN(pps->eqr.central_meridian))
	pps->eqr.central_meridian *= D2R;
      if (!ISNAN(pps->eqr.orig_latitude))
	pps->eqr.orig_latitude *= D2R;

      break;

    case EQUIDISTANT:
      if (!ISNAN(pps->eqc.central_meridian))
	pps->eqc.central_meridian *= D2R;
      if (!ISNAN(pps->eqc.orig_latitude))
	pps->eqc.orig_latitude *= D2R;

      break;

    case SINUSOIDAL:
      if (!ISNAN(pps->sin.longitude_center))
	pps->sin.longitude_center *= D2R;
      break;

  default:
            asfPrintError("Image file is not map-projected.  Use asf_geocode or\n"
                          "Geocode tab to geocode the image file before proceeding.\n");
    }
}

void to_degrees(projection_type_t pt, project_parameters_t * pps)
{
    switch (pt)
    {
  case UNIVERSAL_TRANSVERSE_MERCATOR:
            if (!ISNAN(pps->utm.lon0))
                pps->utm.lon0 *= R2D;
            if (!ISNAN(pps->utm.lat0))
                pps->utm.lat0 *= R2D;

            break;

  case POLAR_STEREOGRAPHIC:
            if (!ISNAN(pps->ps.slon))
                pps->ps.slon *= R2D;
            if (!ISNAN(pps->ps.slat))
                pps->ps.slat *= R2D;

            break;

  case ALBERS_EQUAL_AREA:
            if (!ISNAN(pps->albers.center_meridian))
                pps->albers.center_meridian *= R2D;
            if (!ISNAN(pps->albers.orig_latitude))
                pps->albers.orig_latitude *= R2D;
            if (!ISNAN(pps->albers.std_parallel1))
                pps->albers.std_parallel1 *= R2D;
            if (!ISNAN(pps->albers.std_parallel2))
                pps->albers.std_parallel2 *= R2D;

            break;

  case LAMBERT_AZIMUTHAL_EQUAL_AREA:
            if (!ISNAN(pps->lamaz.center_lat))
                pps->lamaz.center_lat *= R2D;
            if (!ISNAN(pps->lamaz.center_lon))
                pps->lamaz.center_lon *= R2D;

            break;

  case LAMBERT_CONFORMAL_CONIC:
            if (!ISNAN(pps->lamcc.plat1))
                pps->lamcc.plat1 *= R2D;
            if (!ISNAN(pps->lamcc.plat2))
                pps->lamcc.plat2 *= R2D;
            if (!ISNAN(pps->lamcc.lat0))
                pps->lamcc.lat0 *= R2D;
            if (!ISNAN(pps->lamcc.lon0))
                pps->lamcc.lon0 *= R2D;

            break;

    case MERCATOR:
      if (!ISNAN(pps->mer.central_meridian))
	pps->mer.central_meridian *= R2D;
      if (!ISNAN(pps->mer.orig_latitude))
	pps->mer.orig_latitude *= R2D;
      if (!ISNAN(pps->mer.standard_parallel))
	pps->mer.standard_parallel *= R2D;
      break;

    case EQUI_RECTANGULAR:
      if (!ISNAN(pps->eqr.central_meridian))
	pps->eqr.central_meridian *= R2D;
      if (!ISNAN(pps->eqr.orig_latitude))
	pps->eqr.orig_latitude *= R2D;
      break;

    case EQUIDISTANT:
      if (!ISNAN(pps->eqc.central_meridian))
	pps->eqc.central_meridian *= R2D;
      if (!ISNAN(pps->eqc.orig_latitude))
	pps->eqc.orig_latitude *= R2D;
      break;

    case SINUSOIDAL:
      if (!ISNAN(pps->sin.longitude_center))
	pps->sin.longitude_center *= R2D;
      break;

  default:
            asfPrintError("Image file is not map-projected.  Use asf_geocode or\n"
                          "Geocode tab to geocode the image file before proceeding.\n");
    }
}


/**************************************************************************
 * atct_init:
 * calculates alpha1, alpha2, and alpha3, which are some sort of coordinate
 * rotation amounts, in degrees.  This creates a latitude/longitude-style
 * coordinate system centered under the satellite at the start of imaging.
 * You must pass it a state vector from the start of imaging.            */
void atct_init(meta_projection *proj,stateVector st)
{
  vector up={0.0,0.0,1.0};
  vector z_orbit, y_axis, a, nd;
  double alpha3_sign;
  double alpha1,alpha2,alpha3;

  vecCross(st.pos,st.vel,&z_orbit);vecNormalize(&z_orbit);

  vecCross(z_orbit,up,&y_axis);vecNormalize(&y_axis);

  vecCross(y_axis,z_orbit,&a);vecNormalize(&a);

  alpha1 = atan2_check(a.y,a.x)*R2D;
  alpha2 = -1.0 * asind(a.z);
  if (z_orbit.z < 0.0)
    {
      alpha1 +=  180.0;
      alpha2 = -1.0*(180.0-fabs(alpha2));
    }

  vecCross(a,st.pos,&nd);vecNormalize(&nd);
  alpha3_sign = vecDot(nd,z_orbit);
  alpha3 = acosd(vecDot(a,st.pos)/vecMagnitude(st.pos));
  if (alpha3_sign<0.0)
    alpha3 *= -1.0;

  proj->param.atct.alpha1=alpha1;
  proj->param.atct.alpha2=alpha2;
  proj->param.atct.alpha3=alpha3;
}

/**************************************************************************
 * atct_init_from_leader:
 * calculates alpha1, alpha2, and alpha3, which are some sort of coordinate
 * rotation amounts, in degrees.  This creates a latitude/longitude-style
 * coordinate system centered under the satellite at the start of imaging.
 * Rather than using a passed-in state vector, the initial state vector is
 * read from the leader file.
 */
void atct_init_from_leader(const char *leaderName, meta_projection *proj)
{
    meta_parameters *meta = raw_init();
    stateVector st_start;
    ceos_description *ceos = 
      get_ceos_description_ext(leaderName, REPORT_LEVEL_NONE, FALSE);

    // Azimuth time per pixel need to be known for state vector propagation
    ceos_init_sar_general(ceos, leaderName, meta, TRUE);

    ceos_read_stVecs(leaderName, ceos, meta);
    st_start = meta_get_stVec(meta, 0.0);
    fixed2gei(&st_start,0.0);/* Remove earth's spin JPL's AT/CT projection requires this */

    atct_init(proj, st_start);
    meta_free(meta);
}

void write_esri_proj_file(char *inFile)
{
  FILE *fp;
  char esri_prj_file_name[255];

  create_name (esri_prj_file_name, inFile, ".prj");

  fp = FOPEN(esri_prj_file_name, "w");
  fprintf(fp,
      "GEOGCS[\"GCS_WGS_1984\","
      "DATUM[\"D_WGS_1984\","
      "SPHEROID[\"WGS_1984\",6378137,298.257223563]],"
      "PRIMEM[\"Greenwich\",0],"
      "UNIT[\"Degree\",0.017453292519943295]]");
  FCLOSE(fp);
}

char *meta2esri_proj(meta_parameters *meta, char *projFile)
{
  FILE *fpIn;
  char projcsStr[100], geogcsStr[200], projStr[500], datumStr[150];
  char spheroidStr[100], *error;
  static char out[1024];

  project_parameters_t pps;
  projection_type_t proj_type;
  datum_type_t datum;
  spheroid_type_t spheroid;
  double semimajor;
  double inv_flattening ;

  // Get projection information
  if (meta &&
      (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR ||
       meta->projection->type == POLAR_STEREOGRAPHIC ||
       meta->projection->type == ALBERS_EQUAL_AREA ||
       meta->projection->type == LAMBERT_CONFORMAL_CONIC ||
       meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)) {
    
    pps = meta->projection->param;
    proj_type = meta->projection->type;
    datum = meta->projection->datum;
    spheroid = meta->projection->spheroid;
    semimajor = meta->projection->re_major;
    inv_flattening = 
      semimajor / (semimajor - meta->projection->re_minor);
  }
  else if (projFile) {
    parse_proj_args_file(projFile, &pps, &proj_type, &datum, &spheroid, &error);
    fpIn = FOPEN(projFile, "r");
    //spheroid = get_spheroid(fpIn);
    FCLOSE(fpIn);

    switch (spheroid)
      {
      case BESSEL_SPHEROID:
      	semimajor = BESSEL_SEMIMAJOR;
      	inv_flattening = BESSEL_INV_FLATTENING;
    	  break;
      case CLARKE1866_SPHEROID:
      	semimajor = CLARKE1866_SEMIMAJOR;
      	inv_flattening = CLARKE1866_INV_FLATTENING;
    	  break;
      case GEM6_SPHEROID:
      	semimajor = GEM6_SEMIMAJOR;
      	inv_flattening = GEM6_INV_FLATTENING;
    	  break;
      case GEM10C_SPHEROID:
      	semimajor = GEM10C_SEMIMAJOR;
      	inv_flattening = GEM10C_INV_FLATTENING;			\
    	  break;
      case GRS1980_SPHEROID:
      	semimajor = GRS1980_SEMIMAJOR;
      	inv_flattening = GRS1980_INV_FLATTENING;
    	  break;
      case INTERNATIONAL1924_SPHEROID:
      	semimajor = INTERNATIONAL1924_SEMIMAJOR;
      	inv_flattening = INTERNATIONAL1924_INV_FLATTENING;
    	  break;
      case INTERNATIONAL1967_SPHEROID:
      	semimajor = INTERNATIONAL1967_SEMIMAJOR;
      	inv_flattening = INTERNATIONAL1967_INV_FLATTENING;
    	  break;
      case WGS72_SPHEROID:
      	semimajor = WGS72_SEMIMAJOR;
      	inv_flattening = WGS72_INV_FLATTENING;
    	  break;
      case WGS84_SPHEROID:
      	semimajor = WGS84_SEMIMAJOR;
      	inv_flattening = WGS84_INV_FLATTENING;
    	  break;
      case HUGHES_SPHEROID:
      	semimajor = HUGHES_SEMIMAJOR;
      	inv_flattening = HUGHES_INV_FLATTENING;
    	  break;
      default:
        asfPrintError("Unknown spheroid: %d\n", spheroid);
        break;
      }
  }

  // Convert the projection information into ESRI projection format
  if ((meta &&
       (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR ||
        meta->projection->type == POLAR_STEREOGRAPHIC ||
        meta->projection->type == ALBERS_EQUAL_AREA ||
        meta->projection->type == LAMBERT_CONFORMAL_CONIC ||
        meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)) || projFile) {

    // Construct geographic coordinate system string
    sprintf(geogcsStr, "GEOGCS[");
    sprintf(datumStr, "DATUM[");
    switch (datum)
      {
      case EGM96_DATUM:
      	break;
      case ED50_DATUM:
      	strcat(geogcsStr, "\"GCS_European_1950\",");
      	strcat(datumStr, "\"D_European_1950\",");
    	  break;
      case ETRF89_DATUM:
      	strcat(geogcsStr, "\"GCS_ETRS_1989\",");
      	strcat(datumStr, "\"D_ETRS_1989\",");
    	  break;
      case ITRF97_DATUM:
    	  break;
      case NAD27_DATUM:
      	strcat(geogcsStr, "\"GCS_North_American_1927\",");
      	strcat(datumStr, "\"D_North_American_1927\",");
    	  break;
      case NAD83_DATUM:
      	strcat(geogcsStr, "\"GCS_North_American_1983\",");
      	strcat(datumStr, "\"D_North_American_1983\",");
    	  break;
      case WGS72_DATUM:
      	strcat(geogcsStr, "\"GCS_WGS_1972\",");
      	strcat(datumStr, "\"D_WGS_1972\",");
    	  break;
      case WGS84_DATUM:
      	strcat(geogcsStr, "\"GCS_WGS_1984\",");
      	strcat(datumStr, "\"D_WGS_1984\",");
    	  break;
      case HUGHES_DATUM:
      	strcat(geogcsStr, "\"GCS_HUGHES\",");
      	strcat(datumStr, "\"D_HUGHES\",");
    	  break;
      default:
        asfPrintError("Unknown datum: %d\n", datum);
        break;
      }
    strcat(geogcsStr, datumStr);
    switch (spheroid)
      {
      case BESSEL_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"BESSEL\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case CLARKE1866_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"CLARKE_1866\",%.0f,%.9f]]",
    	  	semimajor, inv_flattening);
    	  break;
      case GEM6_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"GEM6\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case GEM10C_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"GEM10C\",%.0f,%.9f]]",
		      semimajor, inv_flattening);
    	  break;
      case GRS1980_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"GRS_1980\",%.0f,%.9f]]", 
      		semimajor, inv_flattening);
    	  break;
      case INTERNATIONAL1924_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"International_1924\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case INTERNATIONAL1967_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"International_1967\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case WGS72_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"WGS_1972\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case WGS84_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"WGS_1984\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case HUGHES_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"HUGHES\",%.0f,%9f]]",
      		semimajor, inv_flattening);
    	  break;
      default:
        asfPrintError("Unknown spheroid: %d\n", spheroid);
        break;
      }
    strcat(geogcsStr, spheroidStr);  
    
    // Construct projection string
    switch (proj_type)
      {
      default:
        asfPrintError("Unknown proj_type: %d\n", proj_type);
        break;
      case LAT_LONG_PSEUDO_PROJECTION:
    	  break;
      case UNIVERSAL_TRANSVERSE_MERCATOR:
      	sprintf(projcsStr, "PROJCS[\"Universal_Transverse_Mercator\"");
      	sprintf(projStr, "PROJECTION[\"Transverse_Mercator\"],PARAMETER[\""
      		"False_Easting\",%.1f],PARAMETER[\"False_Northing\",%.1f],"
      		"PARAMETER[\"Central_Meridian\",%.1f],PARAMETER["
      		"\"Scale_Factor\",%.4f],PARAMETER[\"Latitude_Of_Origin\",%.1f],"
      		"UNIT[\"Meter\",1.0]",
    		pps.utm.false_easting, pps.utm.false_northing, 
		    pps.utm.lon0, pps.utm.scale_factor, pps.utm.lat0);
    	  break;
      case POLAR_STEREOGRAPHIC:
      	if (!isfinite(pps.ps.false_easting))
	        pps.ps.false_easting = 0.0;
      	if (!isfinite(pps.ps.false_northing))
      	  pps.ps.false_northing = 0.0;
      	sprintf(projcsStr, "PROJCS[\"Polar_Stereographic\"");
      	sprintf(projStr, "PROJECTION[\"Stereographic\"],PARAMETER["
      		"\"False_Easting\",%.1f],PARAMETER[\"False_Northing\",%.1f],"
      		"PARAMETER[\"Central_Meridian\",%.1f],PARAMETER["
      		"\"Standard_Parallel_1\",%.1f],UNIT[\"Meter\",1.0]",
    		pps.ps.false_easting, pps.ps.false_northing, pps.ps.slon, pps.ps.slat);
    	  break;
      case ALBERS_EQUAL_AREA:
      	sprintf(projcsStr, "PROJCS[\"Albers_Equal_Area_Conic\"");
      	sprintf(projStr, "PROJECTION[\"Albers\"],PARAMETER[\"False_Easting\","
      		"%.1f],PARAMETER[\"False_Northing\",%.1f],PARAMETER["
      		"\"Central_Meridian\",%.1f],PARAMETER[\"Standard_Parallel_1\","
      		"%.1f],PARAMETER[\"Standard_Parallel_2\",%.1f],PARAMETER["
      		"\"Latitude_Of_Origin\",%.1f],UNIT[\"Meter\",1.0]",
      		pps.albers.false_easting, pps.albers.false_northing, 
      		pps.albers.center_meridian, pps.albers.std_parallel1, 
      		pps.albers.std_parallel2, pps.albers.orig_latitude);
    	  break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      	sprintf(projcsStr, "PROJCS[\"Lambert_Azimuthal_Equal_Area\"");
      	sprintf(projStr, "PROJECTION[\"\"],PARAMETER[\"False_Easting\","
      		"%.1f],PARAMETER[\"False_Northing\",%.1f],PARAMETER["
      		"\"Central_Meridian\",%.1f],PARAMETER["
      		"\"Latitude_Of_Origin\",%.1f],UNIT[\"Meter\",1.0]",
      		pps.lamaz.false_easting, pps.lamaz.false_northing, 
      		pps.lamaz.center_lat, pps.lamaz.center_lon);
    	  break;
      case LAMBERT_CONFORMAL_CONIC:
      	sprintf(projcsStr, "PROJCS[\"Lambert_Conformal_Conic\"");
      	sprintf(projStr, "PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER["
      		"\"False_Easting\",%.1f],PARAMETER[\"False_Northing\",%.1f],"
      		"PARAMETER[\"Central_Meridian\",%.1f],PARAMETER["
      		"\"Standard_Parallel_1\",%.1f],PARAMETER["
      		"\"Standard_Parallel_2\",%.1f],PARAMETER["
      		"\"Latitude_Of_Origin\",%.1f],UNIT[\"Meter\",1.0]",
      		pps.lamcc.false_easting, pps.lamcc.false_northing, 
      		pps.lamcc.lon0, pps.lamcc.plat1, pps.lamcc.plat2, 
      		pps.lamcc.lat0);
    	  break;
      case MERCATOR:
        break;
      case EQUI_RECTANGULAR:
    	  break;
      }
    
    sprintf(out, "%s,%s,PRIMEM[\"Greenwich\",0],UNIT[\"Degree\","
	    "0.0174532925199432955]],%s]\n", projcsStr, geogcsStr, projStr);
  }
  else
    sprintf(out,
	    "GEOGCS[\"GCS_WGS_1984\","
	    "DATUM[\"D_WGS_1984\","
	    "SPHEROID[\"WGS_1984\",6378137,298.257223563]],"
	    "PRIMEM[\"Greenwich\",0],"
	    "UNIT[\"Degree\",0.017453292519943295]]");

  return out;
}

void write_asf2esri_proj(meta_parameters *meta, char *projFile, char *outFile)
{
  char esri_prj_file_name[255];
  create_name (esri_prj_file_name, outFile, ".prj");
  FILE *fpOut = FOPEN(esri_prj_file_name, "w");
  fprintf(fpOut, "%s", meta2esri_proj(meta, projFile));
  FCLOSE(fpOut);
}
