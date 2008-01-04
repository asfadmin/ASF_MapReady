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
    //}
    //else if (meta->projection->type == LAT_LONG_PSEUDO_PROJECTION) {
    //    *lat = py;
    //    *lon = px;
    }
    else {
        proj_to_latlon(meta->projection, px, py, pz, lat, lon, &hgt);
        *lat *= R2D;
        *lon *= R2D;
    }
    return 0;
  }
  else if (meta->airsar) {
      double l = yLine, s = xSample;
      if (meta->sar) {
          l = (double)meta->sar->original_line_count/
              (double)meta->general->line_count * yLine;
          s = (double)meta->sar->original_sample_count/
              (double)meta->general->sample_count * xSample;
      }
      airsar_to_latlon(meta, s, l, elev, lat, lon);
      return 0;
  }
  else if (meta->transform) {
      double l = yLine, s = xSample;
      if (meta->sar) {
          l = (double)meta->sar->original_line_count/
              (double)meta->general->line_count * yLine;
          s = (double)meta->sar->original_sample_count/
              (double)meta->general->sample_count * xSample;
      }
      alos_to_latlon(meta, s, l, elev, lat, lon, &hgt);
      return 0;
  }
  else if (meta->sar &&
           (meta->sar->image_type=='S' || meta->sar->image_type=='G')) {
      /*Slant or ground range.  Use state vectors and doppler.*/
      double slant,doppler,time;
      meta_get_timeSlantDop(meta,yLine,xSample,&time,&slant,&doppler);
      return meta_timeSlantDop2latLon(meta,
              time,slant,doppler,elev,
              lat,lon);
  } else { /*Bogus image type.*/
    asfPrintError(
      "meta_get_latLon: Couldn't figure out what kind of image this is!\n"
      "meta->transform = %p, so it isn't ALOS.\n"
      "meta->airsar = %p, so it isn't Airsar.\n"
      "meta->sar = %p, and it isn't Slant/Ground range.\n"
      "meta->projection = %p, so it isn't Projected, or Scansar.\n"
      "meta->general->name: %s\n",
      meta->transform, meta->airsar, meta->sar, meta->projection,
      meta->general ? meta->general->basename : "(null)");
    return 1; /* Not Reached */
  }
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

  if (meta->sar->image_type=='S'||meta->sar->image_type=='G')
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
  double x_old=1000, y_old=1000;
  double dx, dy;
  int iter=0,err=0;
  lat_lon target;

  target.lat = lat;
  target.lon = lon;
  while (fabs(x-x_old)+fabs(y-y_old)>DELTA*tolerance)
  {
    double cur_err, tmp, del_x, del_y, rad;

    err = get_error(meta,target,elev,x,y,&cur_err);
//printf("%lf, err=%d\n", cur_err, err);
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

  *yLine=y-DELTA/2;
  *xSamp=x-DELTA/2;

  return 0;
}

int meta_get_lineSamp(meta_parameters *meta,
                      double lat,double lon,double elev,
                      double *yLine,double *xSamp)
{
    double x0, y0;
    int err;
    double tol = 0.2;
    int num_iter = 0;

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

        tol += 0.2;
    }

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
  meta_get_latLon(meta, 0, 0, 0.0, &lat, &lon);
  meta->location->lon_start_near_range = lon;
  meta->location->lat_start_near_range = lat;
  meta_get_latLon(meta, 0, meta->general->sample_count, 0.0, &lat, &lon);
  meta->location->lon_start_far_range = lon;
  meta->location->lat_start_far_range = lat;
  meta_get_latLon(meta, meta->general->line_count, 0, 0.0, &lat, &lon);
  meta->location->lon_end_near_range = lon;
  meta->location->lat_end_near_range = lat;
  meta_get_latLon(meta, meta->general->line_count, meta->general->sample_count,
                  0.0, &lat, &lon);
  meta->location->lon_end_far_range = lon;
  meta->location->lat_end_far_range = lat;
}
