/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   These are the external entry points for
   the geolocation-related routines.

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independance.
****************************************************************/
#include <assert.h>
#include "asf.h"
#include "asf_meta.h"
#include <libasf_proj.h>
//#include "jpl_proj.h"

/*******************************************************************
 * Prototypes                                                     */
double *get_a_coeffs(meta_parameters *meta);
double *get_b_coeffs(meta_parameters *meta);
int is_valid_map2s_transform(meta_parameters *meta);
int is_valid_map2l_transform(meta_parameters *meta);
int is_valid_ll2l_transform(meta_parameters *meta);
int is_valid_ll2s_transform(meta_parameters *meta);

/*Geolocation Calls:*/

/*******************************************************************
 * meta_get_original_line_sample:
 * Converts a given line and sample to that for the non-multilooked,
 * non-windowed, equivalent image.  This function shows how
 * gracelessly our metadata represents the different types of images
 * we have to cope with. */
void meta_get_original_line_sample(meta_parameters *meta,
        int line, int sample,
        int *original_line, int *original_sample)
{
  *original_line   = line   * meta->sar->line_increment
        + meta->general->start_line;
  *original_sample = sample * meta->sar->sample_increment
        + meta->general->start_sample;
}

/******************************************************************
 * meta_get_orig:
 * DEPRECATED.  You probably want meta_get_original_line_sample.  */
void meta_get_orig(void *fake_ddr, int y, int x,int *yOrig,int *xOrig)
{
  struct DDR *ddr=(struct DDR *)fake_ddr;
  *xOrig=(x)*ddr->sample_inc + (ddr->master_sample-1);
  *yOrig=(y)*ddr->line_inc + (ddr->master_line-1);
}

static double map_sr2gr(meta_parameters *meta, double l, double s)
{
    double ht = meta_get_sat_height(meta, l, s);
    double srfp = meta_get_slant(meta, l, 0);
    double er = meta_get_earth_radius(meta, l, s);
    double x = 1. + (ht - er)/er;
    double y0 = srfp / er;
    double grfp = er * acos((1. + x*x - y0*y0)/(2.*x));
    double srcp = srfp + s*meta->transform->source_pixel_size;
    double y1 = srcp/er;
    double grcp = er*acos((1. + x*x - y1*y1)/(2.*x));
    return (grcp - grfp)/meta->transform->target_pixel_size;
}

static double map_gr2sr(meta_parameters *meta, double l, double s)
{
    double ht = meta_get_sat_height(meta, l, s);
    double er = meta_get_earth_radius(meta, l, s);
    double srfp = meta_get_slant(meta, l, 0);
    double grfp = er * acos((ht*ht+er*er-srfp*srfp)/(2.*ht*er));
    double grcp = grfp + s * meta->transform->target_pixel_size;
    double srcp = sqrt(ht*ht+er*er-2.*ht*er*cos(grcp/er));
    return (srcp - srfp)/meta->transform->source_pixel_size;
}

/*******************************************************************
 * meta_get_latLon:
 * Converts given line and sample to geodetic latitude and longitude.
 * Works with all image types.*/
int meta_get_latLon(meta_parameters *meta,
                    double yLine, double xSample,double elev,
                    double *lat,double *lon)
{
  double hgt;

  if (meta->projection) {
    /*Map-Projected. Use projection information to calculate lat & lon.*/
    double px,py,pz=0.0;
    px = meta->projection->startX + ((xSample + meta->general->start_sample)
             * meta->projection->perX);
    py = meta->projection->startY + ((yLine + meta->general->start_line)
             * meta->projection->perY);
    /*Currently have to handle scansar separately, because it depends on
      a lot more info than the other projections */
    if (meta->projection->type == SCANSAR_PROJECTION) {
        scan_to_latlon(meta, px, py, elev, lat, lon, &hgt);
    } else {
        proj_to_latlon(meta->projection, px, py, pz, lat, lon, &hgt);
        *lat *= R2D;
        *lon *= R2D;
    }
  }
  else if (meta->airsar) {
    double l = yLine, s = xSample;
    if (meta->sar) {
          l = meta->general->start_line +
                  meta->general->line_scaling * yLine;
          s = meta->general->start_sample +
                  meta->general->sample_scaling * xSample;
    }
    airsar_to_latlon(meta, s, l, elev, lat, lon);
  }
  else if (meta->uavsar) {
    double l = yLine, s = xSample;
    if (meta->sar) {
          l = meta->general->start_line +
                  meta->general->line_scaling * yLine;
          s = meta->general->start_sample +
                  meta->general->sample_scaling * xSample;
    }
    uavsar_to_latlon(meta, s, l, elev, lat, lon);
  }
  else if (meta->latlon) {
    int ix, iy, sample_count = meta->general->sample_count;
    float a00, a10, a01, a11;
    if (xSample <= 1.0)
    	ix = 1;
  	else if (xSample >= (meta->general->sample_count - 2))
  		ix = meta->general->sample_count - 2;
  	else
  		ix = floor(xSample);
  	if (yLine <= 1.0)
  		iy = 1;
  	else if (yLine >= (meta->general->line_count - 2))
  		iy = meta->general->line_count - 2;
  	else
  		iy = floor(yLine);
  		
    a00 = meta->latlon->lat[iy*sample_count + ix];
    a10 = meta->latlon->lat[iy*sample_count + ix+1] -
    	meta->latlon->lat[iy*sample_count + ix];
    a01 = meta->latlon->lat[(iy+1)*sample_count + ix] -
    	meta->latlon->lat[iy*sample_count + ix];
    a11 = meta->latlon->lat[iy*sample_count + ix] -
    	meta->latlon->lat[iy*sample_count + ix+1] -
    	meta->latlon->lat[(iy+1)*sample_count + ix] +
    	meta->latlon->lat[(iy+1)*sample_count + ix+1];
    *lat = (a00 + a10 * (xSample - ix) + a01 * (yLine - iy) 
	    + a11 * (xSample - ix) * (yLine - iy));

    a00 = meta->latlon->lon[iy*sample_count + ix];
    a10 = meta->latlon->lon[iy*sample_count + ix+1] -
    	meta->latlon->lon[iy*sample_count + ix];
    a01 = meta->latlon->lon[(iy+1)*sample_count + ix] -
    	meta->latlon->lon[iy*sample_count + ix];
    a11 = meta->latlon->lon[iy*sample_count + ix] -
    	meta->latlon->lon[iy*sample_count + ix+1] -
    	meta->latlon->lon[(iy+1)*sample_count + ix] +
    	meta->latlon->lon[(iy+1)*sample_count + ix+1];
    *lon = (a00 + a10 * (xSample - ix) + a01 * (yLine - iy) 
	    + a11 * (xSample - ix) * (yLine - iy));
  }
  else if (meta->transform) {
      /* ALOS data (not projected) -- use transform block */
      double l = yLine, s = xSample;
      if (meta->sar) {
	s = meta->general->start_sample +
                  meta->general->sample_scaling * s;
	l = meta->general->start_line +
	          meta->general->line_scaling * l;
      }
      if (meta->sar && meta->sar->image_type == 'G' &&
          strcmp_case(meta->transform->type, "slant") == 0)
      {
        // map s (sample) value from Ground to Slant
        double new_s = map_gr2sr(meta, l, s);
        //printf("meta_get_latLon -- mapped gr to sr: %f -> %f\n", s, new_s);
        s = new_s;
      }
      else if (meta->sar && meta->sar->image_type == 'S' &&
               strcmp_case(meta->transform->type, "ground") == 0)
      {
        // not implemented, don't think we need this?
        asfPrintError("meta_get_latLon: ground transform block "
                      "with a slant range image.\n");
        double new_s = map_sr2gr(meta, l, s);
        //printf("meta_get_latLon -- mapped sr to gr: %f -> %f\n", s, new_s);
        s = new_s;
      }
      alos_to_latlon(meta, s, l, elev, lat, lon, &hgt);
      //printf("alos_to_latlon: %f, %f ==> %f, %f\n", l, s, *lat, *lon);
  }
  else if (meta->sar &&
           (meta->sar->image_type=='S' || meta->sar->image_type=='G')) {
      /*Slant or ground range.  Use state vectors and doppler.*/
      double slant,doppler,time;
      meta_get_timeSlantDop(meta,yLine,xSample,&time,&slant,&doppler);
      meta_timeSlantDop2latLon(meta,
              time,slant,doppler,elev,
              lat,lon);
  } 
  else if (meta->location) {
    double l = yLine, s = xSample;
    if (meta->sar) {
          l = meta->general->start_line +
                  meta->general->line_scaling * yLine;
          s = meta->general->start_sample +
                  meta->general->sample_scaling * xSample;
    }
    location_to_latlon(meta, s, l, elev, lat, lon, &hgt);
  }
  else { /*Bogus image type.*/
    asfPrintError(
      "meta_get_latLon: Couldn't figure out what kind of image this is!\n"
      "meta->transform = %p, so it isn't ALOS.\n"
      "meta->sar = %p, and it isn't Slant/Ground range.\n"
      "meta->projection = %p, so it isn't Projected, or Scansar.\n"
      "meta->location = %p, so doesn't have location information.\n"
      "meta->general->name: %s\n",
      meta->transform, meta->sar, meta->projection, meta->location,
      meta->general ? meta->general->basename : "(null)");
    return 1; /* Not Reached */
  }
  //if (*lon < -180) *lon += 360;
  //if (*lon > 180) *lon -= 360;
  return 0;
}


/*******************************************************************
 * meta_timeSlantDop2latLon:
 * Converts the given time, slant range, doppler, and elevation off
 * earth's surface into a latitude and longitude.*/
int meta_timeSlantDop2latLon(meta_parameters *meta,
                             double time, double slant,double dop,double elev,
                             double *lat,double *lon)
{
  double ignored;
  stateVector stVec;
  stVec=meta_get_stVec(meta,time);
  fixed2gei(&stVec,0.0);/*Subtract Earth's spin.*/

  return getLatLongMeta(stVec,meta,slant,dop,elev,
                              lat,lon,&ignored);
}

/*******************************************************************
 * meta_get_timeSlantDop:
 * Converts a given line and sample in image into time, slant-range,
 * and doppler.  Works with all image types.*/
void meta_get_timeSlantDop(meta_parameters *meta,
  double yLine,double xSample,double *time,double *slant,double *dop)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
    || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

  if (meta->transform)
  {
    double lat,lon;
    meta_get_latLon(meta,yLine,xSample,0.0,&lat,&lon);
    latLon2timeSlant(meta,lat,lon,time,slant,dop);
  }
  // just for testing
  else if (meta->sar->image_type=='S'||meta->sar->image_type=='G')
  { /*Slant or ground range.  These are easy.*/
    *slant = meta_get_slant(meta,yLine,xSample);
    *time  = meta_get_time(meta,yLine,xSample);
    if (dop != NULL)
    {
      if (meta->sar->deskewed == 1)
        *dop=0.0;
      else
        *dop=meta_get_dop(meta,yLine,xSample);
    }
  } else if (meta->sar->image_type=='P' ||
           meta->sar->image_type=='R')
  {
    double lat,lon;
    meta_get_latLon(meta,yLine,xSample,0.0,&lat,&lon);
    latLon2timeSlant(meta,lat,lon,time,slant,dop);
  } else
  {  /*Bogus image type.*/
    printf("Error! Invalid image type '%c' passed to meta_get_timeSlantDop!\n",
      meta->sar->image_type);
    exit(1);
  }
}

/******************************************************************
 * Internal utilities for meta_get_lineSamp() */
typedef struct {
  double lat;
  double lon;
} lat_lon;

static double get_distance(lat_lon one,lat_lon two)
{
  double dlat=one.lat-two.lat;
  double dlon;

  // some kludgery to handle crossing the meridian
  // get "two" to be on the same side as "one"
  if (fabs(one.lon-two.lon) > 300) {
    if (one.lon < 0 && two.lon > 0) two.lon -= 360;
    if (one.lon > 0 && two.lon < 0) two.lon += 360;
  }

  dlon = one.lon - two.lon;

  /* Scale longitude difference to take into accound the fact
     that longitude lines are a lot closer at the pole.  */
  dlon *= cos (one.lat * PI / 180.0);

  return dlat * dlat + dlon * dlon;
}

static int get_error(meta_parameters *meta,
                     lat_lon target,double elev,
                     double xSamp,double yLine,
                     double *error)
{
  lat_lon new;
  int err;
  err = meta_get_latLon(meta,yLine,xSamp,elev,&new.lat,&new.lon);
  if (err)
    return err;
  *error = get_distance(new,target);
  return 0;
}

/******************************************************************
 * meta_get_lineSamp:
 * Converts given latitude and longitude back to the original line
 * and sample.*/
static int meta_get_lineSamp_imp(meta_parameters *meta,
          double x_start, double y_start,
          double lat,double lon,double elev,
          double *yLine,double *xSamp, double tolerance)
{
  /*Number of pixels along which to
    perform finite difference approximation of the derivative.*/
  const double DELTA = 0.1;

  double x = x_start;
  double y = y_start;
  double x_old=-9999, y_old=-9999;
  double dx, dy;
  int iter=0,err=0;
  lat_lon target;

  target.lat = lat;
  target.lon = lon;
  //printf("delta: %f, tolerance: %f\n", DELTA, tolerance);
  while (fabs(x-x_old)+fabs(y-y_old)>DELTA*tolerance)
  {
    double cur_err, tmp, del_x, del_y, rad;

    err = get_error(meta,target,elev,x,y,&cur_err);
    //printf("%f, err=%d\n", cur_err, err);
    if (err)
      return err;
    
    get_error(meta,target,elev,x+DELTA,y,&tmp);
    del_x = (tmp-cur_err)/DELTA;
    
    get_error(meta,target,elev,x,y+DELTA,&tmp);
    del_y = (tmp-cur_err)/DELTA;
    
    rad = fabs(del_x) + fabs(del_y);
    
    //printf("iter %d: x=%6.1f; y=%6.1f, cur_err=%.6f\n",iter,x,y,cur_err);
    
    x_old = x;
    y_old = y;
    dx = (fabs(del_x)/rad)*cur_err/del_x;
    x -= dx;
    dy = (fabs(del_y)/rad)*cur_err/del_y;
    y -= dy;

    iter++;
    if (iter>1000)
      return 1;
  }
  //printf("  %d iterations\n",iter);
  if (!meta_is_valid_double(x) || !meta_is_valid_double(y)) {
    //printf("  %d iterations: nan\n",iter);
    return 1;
  }

  *yLine=y-DELTA/2;
  *xSamp=x-DELTA/2;

  return 0;
}

static double tolerance = 0.2;
void meta_set_lineSamp_tolerance(double tol)
{
  tolerance = tol;
}

double meta_get_lineSamp_tolerance()
{
  return tolerance;
}

int meta_get_lineSamp(meta_parameters *meta,
                      double lat,double lon,double elev,
                      double *yLine,double *xSamp)
{
  if (meta->projection && meta->projection->type != SCANSAR_PROJECTION) {
    meta_projection *mp = meta->projection;
    meta_general *mg = meta->general;

    double x, y, z;
    latlon_to_proj(mp, 'R', lat*D2R, lon*D2R, elev, &x, &y, &z);

    *yLine = (y - mp->startY)/mp->perY - mg->start_line;

    if (meta->projection->type == LAT_LONG_PSEUDO_PROJECTION) {
      double sx = mp->startX;
      double dx1 = fabs(sx - x);
      double dx2 = fabs(sx - x + 360);
      double dx3 = fabs(sx - x - 360);
      if (dx2 < dx1 && dx2 < dx3) x -= 360;
      if (dx3 < dx1 && dx3 < dx2) x += 360;
    }

    *xSamp = (x - mp->startX)/mp->perX - mg->start_sample;

    return 0;
  }

  // only use the reverse transform for Palsar -- for some reason, the
  // Prism and Avnir reverse transforms are not very good.  Palsar is the
  // only one with 25 parameters at this point... and if the other two
  // do start to use 25 in the future, we will try to use them...

  if (meta->transform) {

    if (meta->transform->use_reverse_transform == MAGIC_UNSET_INT) {
      double *a = get_a_coeffs(meta);
      double *b = get_b_coeffs(meta);

      if (a != NULL && b != NULL) {
          //strcmp_case(meta->general->sensor, "ALOS")==0 &&
          //strcmp_case(meta->general->sensor_name, "SAR")==0) {
        meta->transform->use_reverse_transform = TRUE;
        asfPrintStatus("PALSAR/Sentinel -- Using reverse transform block.\n");
      }
      else {
        meta->transform->use_reverse_transform = FALSE;
        asfPrintStatus("Not PALSAR -- Using iterative reverse transform.\n");
      }
    }

    if (meta->sar && meta->sar->image_type == 'G' &&
        strcmp_case(meta->transform->type, "slant") == 0 &&
        meta->transform->use_reverse_transform) {
      asfPrintStatus("PALSAR -- Slant range transform, but image is ground "
                     "range.\n          Using iterative reverse transform.\n");
      meta->transform->use_reverse_transform = FALSE;
    }

    if (meta->transform->use_reverse_transform) {
      double *a = get_a_coeffs(meta); // Usually meta->transform->map2ls_a;
      double *b = get_b_coeffs(meta); // Usually meta->transform->map2ls_b;

      if (a != NULL && b  != NULL)
      {
        int pc = meta->transform->parameter_count;
        // pc should max out at asf_meta.h MAX_FITTING_ORDER
        assert(pc==45 || pc==25 || pc==10 || pc==4);
      
        if (meta->transform->parameter_count >= 25) {
          //printf("before - lat: %.4f, lon: %.4f\n", lat, lon);
          lat -= meta->transform->origin_lat;
          lon -= meta->transform->origin_lon;
          //printf("after - lat: %.4f, lon: %.4f\n", lat, lon);
        }
        // If a valid transform block was found, use it...
        double lat2 = lat  * lat;
        double lon2 = lon  * lon;
        double lat3 = lat2 * lat;
        double lon3 = lon2 * lon;
        double lat4 = lat2 * lat2;
        double lon4 = lon2 * lon2;
        
        if (meta->transform->parameter_count == 25) {
          *xSamp = 
            a[0] *lat4*lon4 + a[1] *lat4*lon3 + a[2] *lat4*lon2 +
            a[3] *lat4*lon  + a[4] *lat4      + a[5] *lat3*lon4 + 
            a[6] *lat3*lon3 + a[7] *lat3*lon2 + a[8] *lat3*lon  +
            a[9] *lat3      + a[10]*lat2*lon4 + a[11]*lat2*lon3 +
            a[12]*lat2*lon2 + a[13]*lat2*lon  + a[14]*lat2      +
            a[15]*lat*lon4  + a[16]*lat*lon3  + a[17]*lat*lon2  +
            a[18]*lat*lon   + a[19]*lat       + a[20]*lon4      +
            a[21]*lon3      + a[22]*lon2      + a[23]*lon       +
            a[24];

          *yLine =
            b[0] *lat4*lon4 + b[1] *lat4*lon3 + b[2] *lat4*lon2 +
            b[3] *lat4*lon  + b[4] *lat4      + b[5] *lat3*lon4 +
            b[6] *lat3*lon3 + b[7] *lat3*lon2 + b[8] *lat3*lon  +
            b[9] *lat3      + b[10]*lat2*lon4 + b[11]*lat2*lon3 +
            b[12]*lat2*lon2 + b[13]*lat2*lon  + b[14]*lat2      +
            b[15]*lat*lon4  + b[16]*lat*lon3  + b[17]*lat*lon2  +
            b[18]*lat*lon   + b[19]*lat       + b[20]*lon4      +
            b[21]*lon3      + b[22]*lon2      + b[23]*lon       +
            b[24];
        }
        else if (meta->transform->parameter_count == 10) {
          *xSamp =
            a[0]          + a[1]*lat      + a[2]*lon      + a[3]*lat*lon +
            a[4]*lat2     + a[5]*lon2     + a[6]*lat2*lon + a[7]*lat*lon2 +
            a[8]*lat3     + a[9]*lon3;

          *yLine =
            b[0]          + b[1]*lat      + b[2]*lon      + b[3]*lat*lon +
            b[4]*lat2     + b[5]*lon2     + b[6]*lat2*lon + b[7]*lat*lon2 +
            b[8]*lat3     + b[9]*lon3;
        }
        else if (meta->transform->parameter_count == 45) {
	    double j = lon;
	    double i = lat;
	    double i2 = i*i;
	    double j2 = j*j;
	    double i3 = i2*i;
	    double j3 = j2*j;
	    double i4 = i2*i2;
	    double j4 = j2*j2;
	    *yLine = a[0] +
                   a[1]*i + a[2]*j +
                   a[3]*i*i + a[4]*i*j + a[5]*j*j +
		   a[6]*i*i*i +
		   a[7]*i*i*j +
		   a[8]*i*j*j +
		   a[9]*j*j*j +
		   a[10]*i*i*i*i +
		   a[11]*i*i*i*j +
		   a[12]*i*i*j*j +
		   a[13]*i*j*j*j +
		   a[14]*j*j*j*j +
		   a[15]*i3*i*j +
		   a[16]*i3*j*j +
		   a[17]*i*i*j3 +
		   a[18]*i*j*j3 +
		   a[19]*i3*i*j*j +
		   a[20]*i3*j3 +
		   a[21]*i*i*j*j3 +
		   a[22]*i3*i*j3 +
		   a[23]*i3*j*j3 +
		   a[24]*i3*i*j*j3 +
		   a[25]*i3*i*i +
		   a[26]*j3*j*j +
		   a[27]*i3*i3 +
		   a[28]*i3*i*i*j +
		   a[29]*j3*j3 +
		   a[30]*i*j*j*j3 +
		   a[31]*i3*i*i3 +
		   a[32]*i3*i3*j +
		   a[33]*i3*i*i*j*j +
		   a[34]*i*i*j*j*j3 +
		   a[35]*i*j3*j3 +
		   a[36]*j3*j*j3 +
		   a[37]*i3*i3*i*i +
		   a[38]*i3*i3*i*j +
		   a[39]*i3*i3*j*j +
		   a[40]*i3*j3*i*i +
		   a[41]*j3*j3*j*j +
		   a[42]*j3*j3*j*i +
		   a[43]*j3*j3*i*i +
		   a[44]*i3*j3*j*j;
	    *xSamp = b[0] +
		   b[1]*i + b[2]*j +
		   b[3]*i*i + b[4]*i*j + b[5]*j*j +
		   b[6]*i*i*i +
		   b[7]*i*i*j +
		   b[8]*i*j*j +
		   b[9]*j*j*j +
		   b[10]*i*i*i*i +
		   b[11]*i*i*i*j +
		   b[12]*i*i*j*j +
		   b[13]*i*j*j*j +
		   b[14]*j*j*j*j +
		   b[15]*i3*i*j +
		   b[16]*i3*j*j +
		   b[17]*i*i*j3 +
		   b[18]*i*j*j3 +
		   b[19]*i3*i*j*j +
		   b[20]*i3*j3 +
		   b[21]*i*i*j*j3 +
		   b[22]*i3*i*j3 +
		   b[23]*i3*j*j3 +
		   b[24]*i3*i*j*j3 +
		   b[25]*i3*i*i +
		   b[26]*j3*j*j +
		   b[27]*i3*i3 +
		   b[28]*i3*i*i*j +
		   b[29]*j3*j3 +
		   b[30]*i*j*j*j3 +
		   b[31]*i3*i*i3 +
		   b[32]*i3*i3*j +
		   b[33]*i3*i*i*j*j +
		   b[34]*i*i*j*j*j3 +
		   b[35]*i*j3*j3 +
		   b[36]*j3*j*j3 +
		   b[37]*i3*i3*i*i +
		   b[38]*i3*i3*i*j +
		   b[39]*i3*i3*j*j +
		   b[40]*i3*j3*i*i +
		   b[41]*j3*j3*j*j +
		   b[42]*j3*j3*j*i +
		   b[43]*j3*j3*i*i +
		   b[44]*i3*j3*j*j;
	    }
	    else {
	      asfPrintError("Invalid number of parameters: %d\n", meta->transform->parameter_count);
	    }
 
        if (elev != 0.0) {
          // note that we don't need to worry about an expensive meta_incid()
          // call, since for Palsar it is calculated from the transform block
	  			int rangeLooks = meta->general->sample_scaling;
          double incid = meta_incid(meta, *yLine, *xSamp/rangeLooks);
          
          // shift LEFT in ascending images, RIGHT in descending
          if (meta->general->orbit_direction=='A') {
						if (meta->sar->image_type == 'S')
							*xSamp -= 
								elev*sin(PI/2-incid)/meta->general->x_pixel_size*rangeLooks;
						else
							*xSamp -= 
								elev*tan(PI/2-incid)/meta->general->x_pixel_size*rangeLooks;
						}
		      else {
						if (meta->sar->image_type == 'S')
							*xSamp += 
								elev*sin(PI/2-incid)/meta->general->x_pixel_size*rangeLooks;
						else
							*xSamp += 
								elev*tan(PI/2-incid)/meta->general->x_pixel_size*rangeLooks;
					}
        }
        
        // we use 0-based indexing, whereas these functions are 1-based.
        if (meta->transform->parameter_count < 25) {
          *xSamp -= 1;
          *yLine -= 1;
        }
       
        if (meta->sar && meta->sar->image_type == 'G' &&
            strcmp_case(meta->transform->type, "slant") == 0)
        {
            // map slant range value *xSamp to ground range
            double new_samp = map_gr2sr(meta, *yLine, *xSamp);
            //printf("meta_get_lineSamp -- mapped sr to gr: %f -> %f\n",
            //       *xSamp, new_samp);
            *xSamp = new_samp;

        } else if (meta->sar && meta->sar->image_type == 'S' &&
            strcmp_case(meta->transform->type, "ground") == 0)
        {
            // don't think we need this?
            asfPrintError("meta_get_lineSamp: ground transform block "
                          "with a slant range image!\n");
            double new_samp = map_sr2gr(meta, *yLine, *xSamp);
            //printf("meta_get_lineSamp -- mapped gr to sr: %f -> %f\n",
            //       *xSamp, new_samp);
            *xSamp = new_samp;
        } 
        // result is in the original line/sample count units,
        // adjust in case we have scaled
        *yLine -= meta->general->start_line;
        *xSamp -= meta->general->start_sample;
        *yLine /= meta->general->line_scaling;
        *xSamp /= meta->general->sample_scaling;
        
        return 0;
      }
    }
  }

  // no shortcuts -- use the iterative method
  double tol_incr = tolerance;
  double x0, y0, tol = tolerance;
  int err,num_iter = 0;
  
  // Plan: added a tolerance factor to meta_get_lineSamp_imp, slowly
  // increase it as we get more and more desperate for convergence.
  // We shouldn't ever get past the first iteration, the only cases
  // where this was needed were cases near the zero doppler line, which
  // was fixed up in another way.
  while (++num_iter <= 6)
  {
    //if (num_iter > 1)
    //  printf("Iteration #%d: tolerance: %f\n", num_iter, tol);
    
    x0 = meta->general->sample_count/2;
    y0 = meta->general->line_count/2;
    err = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev,
                                yLine, xSamp, tol);
    if (!err) return 0;
    
    // First attempt failed to converge... try another starting location,
    // near one of the corners.  If this corner fails, then we'll try each
    // of the other corners.
    //printf("Failed to converge at center point... trying UL corner.\n");
    x0 = meta->general->sample_count/8;
    y0 = meta->general->line_count/8;
    err = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev,
                                yLine, xSamp, tol);
    if (!err) return 0;
    
    //printf("Failed to converge at UL corner... trying LR corner.\n");
    x0 = 7*meta->general->sample_count/8;
    y0 = 7*meta->general->line_count/8;
    err = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev,
                                yLine, xSamp, tol);
    if (!err) return 0;
    
    //printf("Failed to converge at LR corner... trying LL corner.\n");
    x0 = meta->general->sample_count/8;
    y0 = 7*meta->general->line_count/8;
    err = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev,
                                yLine, xSamp, tol);
    if (!err) return 0;
    
    //printf("Failed to converge at LL corner... trying UR corner.\n");
    x0 = 7*meta->general->sample_count/8;
    y0 = meta->general->line_count/8;
    err = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev,
                                yLine, xSamp, tol);
    if (!err) return 0;
    
    //printf("Failed to converge at UR corner... trying (0,0) ??\n");
    x0 = y0 = 0.0;
    err = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev,
                                yLine, xSamp, tol);
    if (!err) return 0;
    
    tol += tol_incr;
  }

  // All methods failed...
  
  // Return center point just for something to return...
  *xSamp = meta->general->sample_count/2;
  *yLine = meta->general->line_count/2;
  return 1;
}

void meta_get_corner_coords(meta_parameters *meta)
{
  double lat, lon;

  if (!meta->location)
    meta->location = meta_location_init();
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  meta_get_latLon(meta, 0, 0, 0.0, &lat, &lon);
  meta->location->lon_start_near_range = lon;
  meta->location->lat_start_near_range = lat;
  meta_get_latLon(meta, 0, ns, 0.0, &lat, &lon);
  meta->location->lon_start_far_range = lon;
  meta->location->lat_start_far_range = lat;
  meta_get_latLon(meta, nl, 0, 0.0, &lat, &lon);
  meta->location->lon_end_near_range = lon;
  meta->location->lat_end_near_range = lat;
  meta_get_latLon(meta, nl, ns, 0.0, &lat, &lon);
  meta->location->lon_end_far_range = lon;
  meta->location->lat_end_far_range = lat;
}

double *get_a_coeffs(meta_parameters *meta)
{
    // Return a valid set of coefficients for transforming
    // from lat/lon to sample (use together with get_b_coeffs()
    // for going from lat/lon to line so you will have a line/samp
    // pair.)
    double *ret = NULL;

    if (meta && meta->transform) {
        if (is_valid_ll2s_transform(meta) &&
	    meta->transform->parameter_count >= 25)
	    ret = meta->transform->l;
	else if (is_valid_map2s_transform(meta)) {
            ret = meta->transform->map2ls_a;
        }
        else if (is_valid_ll2s_transform(meta)) {
            ret = meta->transform->s;
        }
        else {
            ret = NULL;
        }
    }

    return ret;
}

double *get_b_coeffs(meta_parameters *meta)
{
    // Return a valid set of coefficients for transforming
    // from lat/lon to line (use together with get_a_coeffs()
    // for going from lat/lon to sample so you will have a line/samp
    // pair.)
    double *ret = NULL;

    if (meta && meta->transform) {
        if (is_valid_ll2l_transform(meta) &&
	    meta->transform->parameter_count >= 25)
	    ret = meta->transform->s;
        else if (is_valid_map2l_transform(meta)) {
            ret = meta->transform->map2ls_b;
        }
        else if (is_valid_ll2l_transform(meta)) {
            ret = meta->transform->l;
        }
        else {
            ret = NULL;
        }
    }

    return ret;
}

int is_valid_map2s_transform(meta_parameters *meta)
{
    int ret = 1, i;

    if (meta && meta->transform && meta->transform->parameter_count > 0)
    {
        for (i=0; i<meta->transform->parameter_count; i++) {
            if (!meta_is_valid_double(meta->transform->map2ls_a[i])) {
                ret = 0;
                break;
            }
        }
    }

    return ret;
}

int is_valid_map2l_transform(meta_parameters *meta)
{
    int ret = 1, i;

    if (meta && meta->transform && meta->transform->parameter_count > 0)
    {
        for (i=0; i<meta->transform->parameter_count; i++) {
            if (!meta_is_valid_double(meta->transform->map2ls_b[i])) {
                ret = 0;
                break;
            }
        }
    }

    return ret;
}

int is_valid_ll2l_transform(meta_parameters *meta)
{
    int ret = 1, i;

    if (meta && meta->transform && meta->transform->parameter_count > 0)
    {
        for (i=0; i<meta->transform->parameter_count; i++) {
            if (!meta_is_valid_double(meta->transform->l[i])) {
                ret = 0;
                break;
            }
        }
    }

    return ret;
}

int is_valid_ll2s_transform(meta_parameters *meta)
{
    int ret = 1, i;

    if (meta && meta->transform && meta->transform->parameter_count > 0)
    {
        for (i=0; i<meta->transform->parameter_count; i++) {
            if (!meta_is_valid_double(meta->transform->s[i])) {
                ret = 0;
                break;
            }
        }
    }

    return ret;
}

void meta_get_bounding_box(meta_parameters *meta,
                           double *plat_min, double *plat_max,
                           double *plon_min, double *plon_max)
{
    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;

    double lat, lon;
    double lat_min, lat_max, lon_min, lon_max;
    meta_get_latLon(meta, 0, 0, 0, &lat, &lon);
    lat_min = lat;
    lat_max = lat;
    lon_min = lon;
    lon_max = lon;

    meta_get_latLon(meta, nl-1, 0, 0, &lat, &lon);
    if (lat<lat_min) lat_min=lat;
    if (lat>lat_max) lat_max=lat;
    if (lon<lon_min) lon_min=lon;
    if (lon>lon_max) lon_max=lon;

    meta_get_latLon(meta, nl-1, ns-1, 0, &lat, &lon);
    if (lat<lat_min) lat_min=lat;
    if (lat>lat_max) lat_max=lat;
    if (lon<lon_min) lon_min=lon;
    if (lon>lon_max) lon_max=lon;

    meta_get_latLon(meta, 0, ns-1, 0, &lat, &lon);
    if (lat<lat_min) lat_min=lat;
    if (lat>lat_max) lat_max=lat;
    if (lon<lon_min) lon_min=lon;
    if (lon>lon_max) lon_max=lon;

    //printf("Bounding Box:\n");
    //printf("LAT: %.1f %.1f\n", lat_min, lat_max);
    //printf("LON: %.1f %.1f\n", lon_min, lon_max);

    *plat_min = lat_min;
    *plat_max = lat_max;

    *plon_min = lon_min;
    *plon_max = lon_max;
}
