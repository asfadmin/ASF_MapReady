#include "asf_meta.h"
#include "jpl_proj.h"
#include "asf_nan.h"
#include "matrix.h"
#include <assert.h>

#define ecc2 (sqrt(1.0 - (proj->re_minor*proj->re_minor)/(proj->re_major*proj->re_major)) \
            * sqrt(1.0 - (proj->re_minor*proj->re_minor)/(proj->re_major*proj->re_major)))

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
    case SCANSAR_PROJECTION:
      asfPrintError("'proj_to_latlon' not defined for SCANSAR_PROJECTION.\n"
        "Use 'scan_latlon' instead.\n");
      break;
    case LAT_LONG_PSEUDO_PROJECTION:
      *lat = y*D2R;
      *lon = x*D2R;
      break;
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

    const double a = 6378137.0;		        // semi-major axis
    const double b = 6356752.3412;	        // semi-minor axis
    const double e2 = 0.00669437999014; 	// ellipticity
    const double e12 = 0.00673949674228;	// second eccentricity

    // we try to cache the matrices needed for the computation
    // this makes sure we don't reuse the cache incorrectly (i.e., on
    // data (=> an airsar block) which doesn't match what we cached for)
    static meta_airsar *cached_airsar_block = NULL;

    // these are the cached transformation parameters
    static float **m = NULL;
    static double ra = -999, o1 = -999, o2 = -999, o3 = -999;

    if (!m)
        m = matrix(1,3,1,3); // only needs to be done once

    int recalc = !cached_airsar_block ||
        cached_airsar_block->lat_peg_point != meta->airsar->lat_peg_point ||
        cached_airsar_block->lon_peg_point != meta->airsar->lon_peg_point ||
        cached_airsar_block->head_peg_point != meta->airsar->head_peg_point ||
        cached_airsar_block->along_track_offset != meta->airsar->along_track_offset ||
        cached_airsar_block->cross_track_offset != meta->airsar->cross_track_offset;

    if (recalc) {
        // cache airsar block, so we can be sure we're not reusing
        // the stored data incorrectly
        if (cached_airsar_block)
            free(cached_airsar_block);
        cached_airsar_block = meta_airsar_init();
        *cached_airsar_block = *(meta->airsar);

        asfPrintStatus("Calculating airsar transformation parameters...\n");

        // now precalculate data
        double lat_peg = meta->airsar->lat_peg_point;
        double lon_peg = meta->airsar->lon_peg_point;
        double head_peg = meta->airsar->head_peg_point;
        double re = a / sqrt(1-e2*sin(lat_peg)*sin(lat_peg));
        double rn = (a*(1-e2)) / pow(1-e2*sin(lat_peg)*sin(lat_peg), 1.5);
        ra = (re*rn) / (re*cos(head_peg)*cos(head_peg)+rn*sin(head_peg)*sin(head_peg));

        float **m1, **m2;
        m1 = matrix(1,3,1,3);
        m2 = matrix(1,3,1,3);

        m1[1][1] = -sin(lon_peg);
        m1[1][2] = -sin(lat_peg)*cos(lon_peg);
        m1[1][3] = cos(lat_peg)*cos(lon_peg);
        m1[2][1] = cos(lon_peg);
        m1[2][2] = -sin(lat_peg)*sin(lon_peg);
        m1[2][3] = cos(lat_peg)*sin(lon_peg);
        m1[3][1] = 0.0;
        m1[3][2] = cos(lat_peg);
        m1[3][3] = sin(lat_peg);
      
        m2[1][1] = 0.0;
        m2[1][2] = sin(head_peg);
        m2[1][3] = -cos(head_peg);
        m2[2][1] = 0.0;
        m2[2][2] = cos(head_peg);
        m2[2][3] = sin(head_peg);
        m2[3][1] = 1.0;
        m2[3][2] = 0.0;
        m2[3][3] = 0.0;

        o1 = re*cos(lat_peg)*cos(lon_peg)-ra*cos(lat_peg)*cos(lon_peg);
        o2 = re*cos(lat_peg)*sin(lon_peg)-ra*cos(lat_peg)*sin(lon_peg);
        o3 = re*(1-e2)*sin(lat_peg)-ra*sin(lat_peg);

        matrix_multiply(m1,m2,m,3,3,3);
        free_matrix(m1,1,3,1,3);
        free_matrix(m2,1,3,1,3);
    }

    // Make sure we didn't miss anything
    assert(ra != -999 && o1 != -999 && o2 != -999 && o3 != -999);

    //------------------------------------------------------------------
    // Now the actual computation, using the cached matrix etc

    // convenience aliases
    double c0 = meta->airsar->cross_track_offset;
    double s0 = meta->airsar->along_track_offset;
    double ypix = meta->general->y_pixel_size;
    double xpix = meta->general->x_pixel_size;

    // radar coordinates
    double c_lat = (xSample * ypix + c0) / ra;
    double s_lon = (yLine * xpix + s0) / ra;

    // radar coordinates in WGS84
    double t1 = (ra+height)*cos(c_lat)*cos(s_lon);
    double t2 = (ra+height)*cos(c_lat)*sin(s_lon);
    double t3 = (ra+height)*sin(c_lat);
      
    double c1 = m[1][1]*t1 + m[1][2]*t2 + m[1][3]*t3;
    double c2 = m[2][1]*t1 + m[2][2]*t2 + m[2][3]*t3;
    double c3 = m[3][1]*t1 + m[3][2]*t2 + m[3][3]*t3;
      
    // shift into local Cartesian coordinates
    double x = c1 + o1;// + 9.0;
    double y = c2 + o2;// - 161.0;
    double z = c3 + o3;// - 179.0;
      
    // local Cartesian coordinates into geographic coordinates
    double d = sqrt(x*x+y*y);
    double theta = atan2(z*a, d*b);
    *lat = atan2(z+e12*b*sin(theta)*sin(theta)*sin(theta), 
            d-e2*a*cos(theta)*cos(theta)*cos(theta));
    *lon = atan2(y, x);
}

void alos_to_latlon(meta_parameters *meta,
        double xSample, double yLine, double z,
        double *lat, double *lon, double *height)
{
    assert(meta->transform);
    assert(meta->transform->parameter_count == 4 ||
           meta->transform->parameter_count == 10);

    double x[10], y[10];
    int ii;

    for (ii=0; ii<meta->transform->parameter_count; ii++) {
      x[ii] = meta->transform->x[ii];
      y[ii] = meta->transform->y[ii];
    }

    if (z != 0.0) {
        // height correction applies directly to y (range direction)
        double incid = meta_incid(meta, yLine, xSample);
        xSample += z*tan(PI/2-incid)/meta->general->x_pixel_size;
    }

    double i = xSample + 1;
    double j = yLine + 1;

    // optical data transformation
    if (meta->transform->parameter_count == 10) {
        *lat = y[0] + y[1]*i + y[2]*j + y[3]*i*j + y[4]*i*i + y[5]*j*j +
            y[6]*i*i*j + y[7]*i*j*j + y[8]*i*i*i + y[9]*j*j*j;
        *lon = x[0] + x[1]*i + x[2]*j + x[3]*i*j + x[4]*i*i + x[5]*j*j +
            x[6]*i*i*j + x[7]*i*j*j + x[8]*i*i*i + x[9]*j*j*j;
    }
    // SAR data transformation
    else if (meta->transform->parameter_count == 4) {
        *lat = y[0] + y[1]*j + y[2]*i + y[3]*i*j;
        *lon = x[0] + x[1]*j + x[2]*i + x[3]*i*j;
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
  *lat_d = atand(tand(lat) / (1-ecc2));
        *height = z;  // FIXME: Do we need to correct the height at all?
}


void ll_ac(meta_projection *proj, char look_dir, double lat_d, double lon, double *c1, double *c2)
{
  double qlat, qlon;
  double lat,radius;
  vector pos;

  lat= atand(tand(lat_d)*(1 - ecc2));
  sph2cart(proj->param.atct.rlocal,lat*D2R,lon*D2R,&pos);

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
    case LAT_LONG_PSEUDO_PROJECTION:
      *x = lat*D2R;
      *y = lon*D2R;
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
  meta_projection *meta_proj;
  double projZ;

  if (projFile)
  {
      // Read projection file
      read_proj_file(projFile, &pps, &proj_type);

      // Report the conversion type
      switch(proj_type)
      {
          case ALBERS_EQUAL_AREA:
              printf("Lat/Lon to Albers Equal Area\n\n");
              break;
          case LAMBERT_AZIMUTHAL_EQUAL_AREA:
              printf("Lat/Lon to Lambert Azimuthal Equal Area\n\n");
              break;
          case LAMBERT_CONFORMAL_CONIC:
              printf("Lat/Lon to Lambert Conformal Conic\n\n");
              break;
          case POLAR_STEREOGRAPHIC:
              printf("Lat/Lon to Polar Stereographic\n\n");
              break;
          case UNIVERSAL_TRANSVERSE_MERCATOR:
              printf("Lat/Lon to UTM\n\n");
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
  meta_proj->datum = WGS84_DATUM;

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
  pps.utm.false_northing = lat>0 ? 0.0 : 10000000.0;

  // Initialize meta_projection block
  meta_proj = meta_projection_init();
  meta_proj->type = proj_type;
  meta_proj->datum = WGS84_DATUM; // assumed...
  meta_proj->param = pps;

  proj_to_latlon(meta_proj, projX, projY, elev, lat, lon, &h);

  *lat *= R2D;
  *lon *= R2D;
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
