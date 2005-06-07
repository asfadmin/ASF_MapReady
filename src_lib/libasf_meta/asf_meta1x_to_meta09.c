#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "libasf_meta.h"
#include "proj.h"
#include "earth_radius2datum.h"

void asf_meta1x_to_meta09(meta_parameters *meta1x, char *version,
			  meta_parameters *meta09, struct DDR *ddr)
{
  int ii;
  int proj_invalid = 0;
  
  /**********************************************
   * Map metadata version 1.x to version 0.9
   **********************************************/

  /* Fill geo_parameters structure */
  meta09->geo->type = meta1x->sar->image_type;
  meta09->geo->proj = meta1x->projection;
  meta09->geo->lookDir = meta1x->sar->look_direction;
  meta09->geo->deskew = meta1x->sar->deskewed;
  meta09->geo->xPix = meta1x->general->x_pixel_size;
  meta09->geo->yPix = meta1x->general->y_pixel_size;
  meta09->geo->rngPixTime = meta1x->sar->range_time_per_pixel;
  meta09->geo->azPixTime = meta1x->sar->azimuth_time_per_pixel;
  meta09->geo->timeShift = meta1x->sar->time_shift;
  meta09->geo->slantShift = meta1x->sar->slant_shift;
  meta09->geo->slantFirst = meta1x->sar->slant_range_first_pixel;
  meta09->geo->wavelen = meta1x->sar->wavelength;
  meta09->geo->dopRange[0] = meta1x->sar->range_doppler_coefficients[0];
  meta09->geo->dopRange[1] = meta1x->sar->range_doppler_coefficients[1];
  meta09->geo->dopRange[2] = meta1x->sar->range_doppler_coefficients[2];
  meta09->geo->dopAz[0] = meta1x->sar->azimuth_doppler_coefficients[0];
  meta09->geo->dopAz[1] = meta1x->sar->azimuth_doppler_coefficients[1];
  meta09->geo->dopAz[2] = meta1x->sar->azimuth_doppler_coefficients[2];

  /* Fill ifm_parameters structure */
  meta09->ifm->ht = meta1x->sar->satellite_height;
  meta09->ifm->er = meta1x->sar->earth_radius;
  meta09->ifm->nLooks = meta1x->sar->look_count;
  meta09->ifm->orig_nLines = meta1x->sar->original_line_count;
  meta09->ifm->orig_nSamples = meta1x->sar->original_sample_count;

  /* point meta->stVec at meta->state_vectors */
  meta09->stVec = meta1x->state_vectors;

  /* Allocate and fill extra_info structure */
  strcpy(meta09->info->sensor, meta1x->general->sensor);
  strcpy(meta09->info->mode, meta1x->general->mode);
  strcpy(meta09->info->processor, meta1x->general->processor);
  meta09->info->orbit = meta1x->general->orbit;
  meta09->info->bitErrorRate = meta1x->general->bit_error_rate;
  strcpy(meta09->info->satBinTime, meta1x->sar->satellite_binary_time);
  strcpy(meta09->info->satClkTime, meta1x->sar->satellite_clock_time);
  meta09->info->prf = meta1x->sar->prf;


  /************************************
   * Fill in the DDR structure 
   ************************************/

  /* Initialize ddr values */
  c_intddr(ddr);
  
  /* Number of lines & samples; both int */
  ddr->nl = meta1x->general->line_count;
  ddr->ns = meta1x->general->sample_count;
  /* Number of bands; int */
  ddr->nbands = 1;
  /* Data type; int */
  switch (meta1x->general->data_type) 
    {
    case BYTE:           ddr->dtype = DTYPE_BYTE;    break;
    case INTEGER16:      ddr->dtype = DTYPE_SHORT;   break;
    case INTEGER32:      ddr->dtype = DTYPE_LONG;    break;
    case REAL32:         ddr->dtype = DTYPE_FLOAT;   break;
    case REAL64:         ddr->dtype = DTYPE_DOUBLE;  break;
    case COMPLEX_REAL32: ddr->dtype = DTYPE_COMPLEX; break;
    default:
      /*      ddr->dtype = -1;*/
      /* Let's work with the assumption for the moment that we only deal 
	 with floating point imagery */
      ddr->dtype = DTYPE_FLOAT;
      break;
    }
  /* Worthless date & time fields; both char[12] */
  strcpy (ddr->last_used_date,"");
  strcpy (ddr->last_used_time,"");
  /* System byte ordering style; char[12] */
  if (0==strcmp(meta1x->general->system,"big_ieee"))
    strcpy(ddr->system,"ieee-std");
  else if (0==strcmp(meta1x->general->system,"lil_ieee"))
    strcpy(ddr->system,"ieee-lil");
  else if (0==strcmp(meta1x->general->system,"cray_float"))
    strcpy(ddr->system,"cray-unicos");
  else /* "???" ... no meta equivalent of "ibm-mvs" */
    strcpy(meta1x->general->system,"other-msc");
  
  /* Projection units; char[12] */
  if (meta1x->projection) /* if projection struct has been allocated */
    strncpy(ddr->proj_units, meta1x->projection->units, 12);
  else
    strcpy(ddr->proj_units, "meters"); /* Safe to assume meters */
  ddr->valid[DDPUV] = VALID;
  
  /* Increment per sample in x & y directions; both double */
  ddr->line_inc   = meta1x->sar->line_increment;
  ddr->sample_inc = meta1x->sar->sample_increment;
  ddr->valid[DDINCV] = 
    ((meta1x->sar->line_increment == meta1x->sar->line_increment)
     &&(meta1x->sar->sample_increment == meta1x->sar->sample_increment))
    ? VALID : INVAL;
  
  /* Line/sample relative to master image; both int */
  ddr->master_line   = meta1x->general->start_line + 1;
  ddr->master_sample = meta1x->general->start_sample + 1;
  
  /* Projection distance per pixel; both double (pixel size)*/
  if (meta1x->projection) {
    ddr->pdist_y = fabs(meta1x->projection->perY);
    ddr->pdist_x = fabs(meta1x->projection->perX);
  }
  else {
    ddr->pdist_y = meta1x->general->y_pixel_size;
    ddr->pdist_x = meta1x->general->x_pixel_size;
  }
  ddr->valid[DDPDV] = VALID;
  
  /* Projection dependent stuff */
  if (meta1x->sar->image_type=='P') {
    meta_projection *proj = meta1x->projection;
    /* UTM zone code or 62 if n/a; int */
    ddr->zone_code = (proj->type==UNIVERSAL_TRANSVERSE_MERCATOR) ? 
      proj->param.utm.zone : 62;
    ddr->valid[DDZCV] = VALID;
    /* Projection type; int
     * AND
     * Projection coefficients array; double[15]
     *  Entire coefficients array is 0.0 for Geographic and UTM;
     *  meta structure does not currently support geographic or 
     albers projections */
    switch (proj->type) 
      {
      case SCANSAR_PROJECTION: 
	/* Along-track/cross-track... ddr has no atct projection, 
	   default to UTM */
	/*Can't do anything here until we add AT/CT to asf_geolib.*/
	proj_invalid=1;
	break;
      case ALBERS_EQUAL_AREA:/* Albers Conic Equal Area */
	ddr->proj_code = ALBERS;
	ddr->valid[DDPCV] = VALID;
	ddr->proj_coef[0] = proj->re_major;
	ddr->proj_coef[1] = proj->re_minor;
	ddr->proj_coef[2] = 
	  packed_deg(proj->param.albers.std_parallel1); /*standard parallel1*/
	ddr->proj_coef[3] = 
	  packed_deg(proj->param.albers.std_parallel2); /*standard parallel2*/
	ddr->proj_coef[4] = 
	  packed_deg(proj->param.albers.center_meridian);  /*Center longitude 
							     of proj*/
	ddr->proj_coef[5] = 
	  packed_deg(proj->param.albers.orig_latitude);  /*Center latitude 
							   of proj*/
	ddr->proj_coef[6] = 0.0; /*False Easting*/
	ddr->proj_coef[7] = 0.0; /*False Northing*/
	ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
	break;
      case LAMBERT_CONFORMAL_CONIC:/* Lambert Conformal Conic */
	ddr->proj_code = LAMCC;
	ddr->valid[DDPCV] = VALID;
	ddr->proj_coef[0] = proj->re_major;
	ddr->proj_coef[1] = proj->re_minor;
	ddr->proj_coef[2] = 
	  packed_deg(proj->param.lamcc.plat1); /*standard parallel1*/
	ddr->proj_coef[3] = 
	  packed_deg(proj->param.lamcc.plat2); /*standard parallel2*/
	ddr->proj_coef[4] = 
	  packed_deg(proj->param.lamcc.lon0);  /*Center longitude of proj*/
	ddr->proj_coef[5] = 
	  packed_deg(proj->param.lamcc.lat0);  /*Center latitude of proj*/
	ddr->proj_coef[6] = 0.0; /*False Easting*/
	ddr->proj_coef[7] = 0.0; /*False Northing*/
	ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
	break;
      case POLAR_STEREOGRAPHIC:/* Polar Stereographic */
	ddr->proj_code = PS;
	ddr->valid[DDPCV] = VALID;
	ddr->proj_coef[0] = proj->re_major;
	ddr->proj_coef[1] = proj->re_minor;
	ddr->proj_coef[4] = 
	  packed_deg(proj->param.ps.slon);/*Longitude down below pole of map*/
	ddr->proj_coef[5] = 
	  packed_deg(proj->param.ps.slat);/*Latitude of true scale*/
	ddr->proj_coef[6] = 0.0; /*False Easting*/
	ddr->proj_coef[7] = 0.0; /*False Northing*/
	ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
	break;
      case UNIVERSAL_TRANSVERSE_MERCATOR:/* Universal Transverse Mercator */
	ddr->proj_code = UTM;
	ddr->valid[DDPCV] = VALID;
	/*Unnecessary since the zone is specified*/
	ddr->proj_coef[0] = 
	  meta1x->general->center_latitude; /*any longitude in proj*/
	ddr->proj_coef[1] = 
	  meta1x->general->center_longitude;/*any latitude in proj*/
	ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
	break;
      default:/*Der?*/
	proj_invalid=1;
	ddr->proj_code = -1;
	break;
      }
    /* Datum Code; int */
    ddr->datum_code = earth_radius2datum(proj->re_major, proj->re_minor);
    ddr->valid[DDDCV] = (ddr->datum_code==-1) ? INVAL : VALID;
    /* Corner Coordinates; all double[2] */
    ddr->upleft[0] = proj->startY;
    ddr->upleft[1] = proj->startX;
    ddr->loleft[0] = proj->startY + meta1x->general->line_count * proj->perY;
    ddr->loleft[1] = proj->startX;
    ddr->upright[0] = proj->startY;
    ddr->upright[1] = proj->startX + 
      meta1x->general->sample_count * proj->perX;
    ddr->loright[0] = proj->startY + 
      meta1x->general->line_count * proj->perY;
    ddr->loright[1] = proj->startX + 
      meta1x->general->sample_count * proj->perX;
    ddr->valid[DDCCV] = ((proj->startY == proj->startY)
			 &&(proj->startX == proj->startX))
      ? VALID : INVAL;
    if (proj_invalid)
      {
	for (ii=0;ii<4;ii++) 
	  ddr->valid[ii] = UNKNOW;
	ddr->valid[DDCCV] = INVAL;
      }
  } /* End projection info */
}

meta_parameters *asf_meta09_to_meta1x(meta_parameters *meta09, struct DDR *ddr,
				      char *version)
{
  meta_parameters *meta1x;

  /* Initialize the metadata structure for requested version */
  meta1x = asf_meta_init(version);
  
  /**********************************************
   * Map metadata version 0.9 to version 1.x
   **********************************************/

  /* Fill general structure */
  meta1x->general->x_pixel_size = meta09->geo-> xPix;
  meta1x->general->y_pixel_size = meta09->geo->yPix;
  strcpy(meta1x->general->sensor, meta09->info->sensor);
  strcpy(meta1x->general->mode, meta09->info->mode);
  strcpy(meta1x->general->processor, meta09->info->processor);
  meta1x->general->orbit = meta09->info->orbit;
  meta1x->general->bit_error_rate = meta09->info->bitErrorRate;
  /* If no projection block is available we still might need these parameters for
     some calculations. Needs to be checked at some stage. Hardwiring them in is not
     exactly the way to do this. */
  meta1x->general->re_major = 6378144.0;
  meta1x->general->re_minor = 6356754.9;

  /* Fill SAR structure */
  meta1x->sar->image_type = meta09->geo->type;
  meta1x->sar->look_direction = meta09->geo->lookDir;
  meta1x->sar->deskewed = meta09->geo->deskew;
  meta1x->sar->range_time_per_pixel = meta09->geo->rngPixTime;
  meta1x->sar->azimuth_time_per_pixel = meta09->geo->azPixTime;
  meta1x->sar->time_shift = meta09->geo->timeShift;
  meta1x->sar->slant_shift = meta09->geo->slantShift;
  meta1x->sar->slant_range_first_pixel = meta09->geo->slantFirst;
  meta1x->sar->wavelength = meta09->geo->wavelen;
  meta1x->sar->range_doppler_coefficients[0] = meta09->geo->dopRange[0];
  meta1x->sar->range_doppler_coefficients[1] = meta09->geo->dopRange[1];
  meta1x->sar->range_doppler_coefficients[2] = meta09->geo->dopRange[2];
  meta1x->sar->azimuth_doppler_coefficients[0] = meta09->geo->dopAz[0];
  meta1x->sar->azimuth_doppler_coefficients[1] = meta09->geo->dopAz[1];
  meta1x->sar->azimuth_doppler_coefficients[2] = meta09->geo->dopAz[2];
  meta1x->sar->satellite_height = meta09->ifm->ht;
  meta1x->sar->earth_radius = meta09->ifm->er;
  meta1x->sar->look_count = meta09->ifm->nLooks;
  meta1x->sar->original_line_count = meta09->ifm->orig_nLines;
  meta1x->sar->original_sample_count = meta09->ifm->orig_nSamples;
  strcpy(meta1x->sar->satellite_binary_time, meta09->info->satBinTime);
  strcpy(meta1x->sar->satellite_clock_time, meta09->info->satClkTime);
  meta1x->sar->prf = meta09->info->prf;

  /* State vectors are stored in identical structures */
  meta1x->state_vectors = meta09->stVec;

  /* Fill projection structure */
  if (meta09->geo->type=='P') {
    meta1x->projection = meta09->geo->proj;
    if (meta09->geo->proj->type == 'U') {
      meta1x->projection->type = UNIVERSAL_TRANSVERSE_MERCATOR;
      meta1x->projection->param.utm.zone = meta09->geo->proj->param.utm.zone;
      meta1x->projection->param.utm.lat0 = ddr->proj_coef[0];
      meta1x->projection->param.utm.lon0 = ddr->proj_coef[1];
      if (meta1x->projection->param.utm.lat0 > 0.0)
	meta1x->projection->param.utm.false_northing = 0;
      else 
	meta1x->projection->param.utm.false_northing = 10000000;
      meta1x->projection->param.utm.false_easting = 500000;
      meta1x->projection->param.utm.scale_factor = 0.9996; /* default */
    }
    else if (meta09->geo->proj->type == 'P') {
      meta1x->projection->type = POLAR_STEREOGRAPHIC;
      meta1x->projection->param.ps.slat = meta09->geo->proj->param.ps.slat;
      meta1x->projection->param.ps.slon = meta09->geo->proj->param.ps.slon;
    }
    else if (meta09->geo->proj->type == 'L') {
      meta1x->projection->type = LAMBERT_CONFORMAL_CONIC;
      meta1x->projection->param.lamcc.plat1 = meta09->geo->proj->param.lamcc.plat1;
      meta1x->projection->param.lamcc.plat2 = meta09->geo->proj->param.lamcc.plat2;
      meta1x->projection->param.lamcc.lat0 = meta09->geo->proj->param.lamcc.lat0;
      meta1x->projection->param.lamcc.lon0 = meta09->geo->proj->param.lamcc.lon0;
    }
    else if (meta09->geo->proj->type == 'A')
      meta1x->projection->type = SCANSAR_PROJECTION;
    meta1x->projection->startX = meta09->geo->proj->startX;
    meta1x->projection->startY = meta09->geo->proj->startY;
    meta1x->projection->perY = -ddr->pdist_y;
    meta1x->projection->perX = ddr->pdist_x;
    meta1x->projection->hem = meta09->geo->proj->hem;
    meta1x->projection->re_major = meta09->geo->proj->re_major;
    meta1x->projection->re_minor = meta09->geo->proj->re_minor;
  }

  /****************************************
   * Get values out of the DDR structure 
   ****************************************/

  /* Number of lines & samples */
  meta1x->general->line_count = ddr->nl;
  meta1x->general->sample_count = ddr->ns;

  /* Data type */
  switch (ddr->dtype) 
    {
    case DTYPE_BYTE:
      meta1x->general->data_type = BYTE;
      break;
    case DTYPE_SHORT:
      meta1x->general->data_type = INTEGER16;
      break;
    case DTYPE_LONG:
      meta1x->general->data_type = INTEGER32;
      break;
    case DTYPE_FLOAT:
      meta1x->general->data_type = REAL32;
      break;
    case DTYPE_DOUBLE:
      meta1x->general->data_type = REAL64;
      break;
    case DTYPE_COMPLEX:
      meta1x->general->data_type = COMPLEX_REAL32;
      break;
    }

  /* System byte ordering style */
  if (0==strcmp(ddr->system, "ieee-std"))
    strcpy(meta1x->general->system, "big_ieee");
  else if (0==strcmp(ddr->system, "ieee-lil"))
    strcpy(meta1x->general->system, "lil_ieee");
  else if (0==strcmp(ddr->system, "cray-unicos"))
    strcpy(meta1x->general->system, "cray_float");
  else /* "???" ... no meta equivalent of "ibm-mvs" */
    strcpy(meta1x->general->system,"other-msc");
  
  /* Projection units */
  if (meta1x->projection) /* if projection struct has been allocated */
    strncpy(meta1x->projection->units, ddr->proj_units, 12);
  
  /* Increment per sample in x & y directions */
  meta1x->sar->line_increment = ddr->line_inc;
  meta1x->sar->sample_increment = ddr->sample_inc;
  
  /* Line/sample relative to master image */
  meta1x->general->start_line = ddr->master_line - 1;
  meta1x->general->start_sample = ddr->master_sample - 1;
  
  /* Projection distance per pixel */
  meta1x->general->y_pixel_size = ddr->pdist_y;
  meta1x->general->x_pixel_size = ddr->pdist_x;

  /* Center latitude/longitude
     By now we should have all the information to get derive geographic coordinates
     from the metadata. 
     Requires the old metadata library call. Leave it out for the moment. 
  meta_get_latLon(meta1x, meta1x->general->line_count, meta1x->general->sample_count,
		  0.0, &(meta1x->general->center_latitude), 
		  &(meta1x->general->center_longitude)); */

  return meta1x;
  
}
