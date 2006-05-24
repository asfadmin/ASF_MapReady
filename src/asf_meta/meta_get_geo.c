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
#include "jpl_proj.h"

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
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

  *original_line   = line   * meta->sar->line_increment
				+ meta->general->start_line;
  *original_sample = sample * meta->sar->sample_increment
				+ meta->general->start_sample;
}

/*******************************************************************
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
void meta_get_latLon(meta_parameters *meta,
	double yLine, double xSample,double elev,double *lat,double *lon)
{
  if ( meta->projection != NULL 
       && meta->projection->type == LAT_LONG_PSEUDO_PROJECTION ) {
    *lon = meta->projection->startX + ((xSample + meta->general->start_sample)
				       * meta->projection->perX);
    *lat = meta->projection->startY + ((yLine + meta->general->start_line) 
				       * meta->projection->perY);
  } else if (meta->sar->image_type=='S' || meta->sar->image_type=='G') { 
    /*Slant or ground range.  Use state vectors and doppler.*/
    double slant,doppler,time;
    meta_get_timeSlantDop(meta,yLine + meta->general->start_line,
			  xSample, + meta->general->start_sample,
			  &time,&slant,&doppler);
    meta_timeSlantDop2latLon(meta,
			     time,slant,doppler,elev,
			     lat,lon);
  } else if (meta->sar->image_type=='P') {
    /*Map-Projected. Use projection information to calculate lat & lon.*/
    double px,py;
    px = meta->projection->startX + ((xSample + meta->general->start_sample)
				     * meta->projection->perX);
    py = meta->projection->startY + ((yLine + meta->general->start_line)
				     * meta->projection->perY);
    proj_to_ll(meta->projection, meta->sar->look_direction, px, py,
	       lat,lon);
  } else { /*Bogus image type.*/
    printf("Error! Invalid image type '%c' passed to meta_get_latLon!\n",
	   meta->sar->image_type);
    exit(1);
  }
}


/*******************************************************************
 * meta_timeSlantDop2latLon:
 * Converts the given time, slant range, doppler, and elevation off
 * earth's surface into a latitude and longitude.*/
void meta_timeSlantDop2latLon(meta_parameters *meta,
	double time, double slant,double dop,double elev,
	double *lat,double *lon)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	double ignored;
	stateVector stVec;
	stVec=meta_get_stVec(meta,time);
	fixed2gei(&stVec,0.0);/*Subtract Earth's spin.*/
		
	getLatLongMeta(stVec,meta,slant,dop,elev,
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
	} else  if (meta->sar->image_type=='P') /*Map-projected image.  These are more difficult.*/
	{
		double lat,lon;
		meta_get_latLon(meta,yLine,xSample,0.0,&lat,&lon);
		latLon2timeSlant(meta,lat,lon,time,slant,dop);
	} else
	{	/*Bogus image type.*/
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
double get_distance(lat_lon one,lat_lon two)
{
	double dlat=one.lat-two.lat;
	double dlon;
	if ( one.lon < -170 && two.lon > 170 ) {
	  dlon = (180 + one.lon) + (180 - two.lon);
	}
	else if ( one.lon > 170 && two.lon < -170 ) {
	  dlon = (180 - one.lon) + (180 + two.lon);
	}
	else {
	  dlon = one.lon - two.lon;
	}

	/* Scale longitude difference to take into accound the fact
	   that longitude lines are a lot closer at the pole.  */
	dlon *= cos (one.lat * PI / 180.0);

	return dlat * dlat + dlon * dlon;
}
double get_error(meta_parameters *meta,
	lat_lon target,double elev, double xSamp,double yLine)
{
	lat_lon new;
	meta_get_latLon(meta,yLine,xSamp,elev,&new.lat,&new.lon);
	return get_distance(new,target);
}

/******************************************************************
 * meta_get_lineSamp:
 * Converts given latitude and longitude back to the original line
 * and sample.*/
static int meta_get_lineSamp_imp(meta_parameters *meta,
				  double x_start, double y_start,
				  double lat,double lon,double elev,
				  double *yLine,double *xSamp)
{
#define DELTA 0.1 /*Number of pixels along which to 
  perform finite difference approximation of the derivative.*/
        double x = x_start;
        double y = y_start;
	double x_old=1000, y_old=1000;
	int iter=0;
	lat_lon target;


	target.lat = lat;
	target.lon = lon;
	while (fabs(x-x_old)+fabs(y-y_old)>DELTA)
	{
		double cur_err = get_error(meta,target,elev,x,y);
		double del_x   = (get_error(meta,target,elev,x+DELTA,y)-cur_err)/DELTA;
		double del_y   = (get_error(meta,target,elev,x,y+DELTA)-cur_err)/DELTA;
		double rad     = fabs(del_x) + fabs(del_y);

		//printf(" %d: x=%6.1f; y=%6.1f, err=%.6f\n",iter,x,y,cur_err);
	       
		x_old=x;y_old=y;
		x=x-(fabs(del_x)/rad)*cur_err/del_x;
		y=y-(fabs(del_y)/rad)*cur_err/del_y;
		iter++;

		if (iter>1000) return 0;
	}
	//printf("  %d iterations\n",iter);
 
	*yLine=y-DELTA/2;
	*xSamp=x-DELTA/2;
	return 1;
}

void meta_get_lineSamp(meta_parameters *meta,
		       double lat,double lon,double elev,
		       double *yLine,double *xSamp)
{
  // It should be totally easy to make this work (since pixels
  // correspond to lat/long values) No effort has been made to make
  // this routine work with pseudoprojected images yet though.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

  double x0, y0;
  int ret;

  x0 = meta->general->sample_count/2;
  y0 = meta->general->line_count/2;
  ret = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev, yLine, xSamp);
  if (ret) return;

  // First attempt failed to converge... try another starting location,
  // near one of the corners.  If this corner fails, then we'll try each
  // of the other corners.
  //printf("Failed to converge at center point... trying UL corner.\n");
  x0 = meta->general->sample_count/8;
  y0 = meta->general->line_count/8;
  ret = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev, yLine, xSamp);
  if (ret) return;

  //printf("Failed to converge at UL corner... trying LR corner.\n");
  x0 = 7*meta->general->sample_count/8;
  y0 = 7*meta->general->line_count/8;
  ret = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev, yLine, xSamp);
  if (ret) return;

  //printf("Failed to converge at LR corner... trying LL corner.\n");
  x0 = meta->general->sample_count/8;
  y0 = 7*meta->general->line_count/8;
  ret = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev, yLine, xSamp);
  if (ret) return;

  //printf("Failed to converge at LL corner... trying UR corner.\n");
  x0 = 7*meta->general->sample_count/8;
  y0 = meta->general->line_count/8;
  ret = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev, yLine, xSamp);
  if (ret) return;

  //printf("Failed to converge at UR corner... trying (0,0) ??\n");
  x0 = y0 = 0.0;
  ret = meta_get_lineSamp_imp(meta, x0, y0, lat, lon, elev, yLine, xSamp);
  if (ret) return;

  // All corners failed... probably we're just doomed.
  printf("meta_get_lineSamp: Failed to converge for center & all corners.\n");
  printf("                      %g %g %g\n", lat, lon, elev);

  // Return center point just for something to return...
  *xSamp = meta->general->sample_count/2;
  *yLine = meta->general->line_count/2;
}

void meta_get_corner_coords(meta_parameters *meta)
{
  double lat, lon;

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
