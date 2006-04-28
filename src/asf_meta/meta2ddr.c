#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "asf_meta.h"
#include "proj.h"
#include "earth_radius2datum.h"


void meta2ddr(meta_parameters *meta, struct DDR *ddr)
{
	int ii;
	int proj_invalid = 0;

/* Initialize ddr values */
	c_intddr(ddr);
		
/* Number of lines & samples; both int */
	ddr->nl = meta->general->line_count;
	ddr->ns = meta->general->sample_count;
/* Number of bands; int */
	ddr->nbands = 1;
/* Data type; int */
	switch (meta->general->data_type) {
	  case BYTE:           ddr->dtype = DTYPE_BYTE;    break;
	  case INTEGER16:      ddr->dtype = DTYPE_SHORT;   break;
	  case INTEGER32:      ddr->dtype = DTYPE_LONG;    break;
	  case REAL32:         ddr->dtype = DTYPE_FLOAT;   break;
	  case REAL64:         ddr->dtype = DTYPE_DOUBLE;  break;
	  case COMPLEX_REAL32: ddr->dtype = DTYPE_COMPLEX; break;
	  default:
	    printf("WARNING * There is not a direct conversion from meta.general.data_type\n"
	           "        * to ddr.dtype. Setting ddr.dtype value to -1.\n");
	    ddr->dtype = -1;
	    break;
	}
/* Worthless date & time fields; both char[12] */
	strcpy (ddr->last_used_date,"");
	strcpy (ddr->last_used_time,"");
/* System byte ordering style; char[12] */
	if (0==strcmp(meta->general->system,"big_ieee"))
		strcpy(ddr->system,"ieee-std");
	else if (0==strcmp(meta->general->system,"lil_ieee"))
		strcpy(ddr->system,"ieee-lil");
	else if (0==strcmp(meta->general->system,"cray_float"))
		strcpy(ddr->system,"cray-unicos");
	else /* "???" ... no meta equivalent of "ibm-mvs" */
		strcpy(meta->general->system,"other-msc");

/* Projection units; char[12] */
	if (meta->projection) /* if projection struct has been allocated */
		strncpy(ddr->proj_units, meta->projection->units, 12);
	else
        	strcpy(ddr->proj_units, "meters"); /* Safe to assume meters */
	ddr->valid[DDPUV] = VALID;

/* Increment per sample in x & y directions; both double */
	ddr->line_inc   = meta->sar->line_increment;
	ddr->sample_inc = meta->sar->sample_increment;
	ddr->valid[DDINCV] = ((meta->sar->line_increment   == meta->sar->line_increment)
	                    &&(meta->sar->sample_increment == meta->sar->sample_increment))
	                ? VALID : INVAL;

/* Line/sample relative to master image; both int */
	ddr->master_line   = meta->general->start_line + 1;
	ddr->master_sample = meta->general->start_sample + 1;

/* Projection distance per pixel; both double (pixel size)*/
	ddr->pdist_y = meta->general->y_pixel_size;
	ddr->pdist_x = meta->general->x_pixel_size;
	ddr->valid[DDPDV] = ((meta->general->y_pixel_size == meta->general->y_pixel_size)
	                   &&(meta->general->x_pixel_size == meta->general->x_pixel_size))
	                ? VALID : INVAL;

/* Projection dependent stuff */
	if (meta->sar->image_type=='P') {
		meta_projection *proj = meta->projection;
	/* UTM zone code or 62 if n/a; int */
		ddr->zone_code = (proj->type==UNIVERSAL_TRANSVERSE_MERCATOR) ? 
                  proj->param.utm.zone : 62;
		ddr->valid[DDZCV] = VALID;
		
		/* If the projection block perX and perY are
		   available, we should use them for the pixel
		   sizes.  */
		if ( meta->projection->type != SCANSAR_PROJECTION ) {
		  ddr->pdist_x = fabs(meta->projection->perX);
		  ddr->pdist_y = fabs(meta->projection->perY);
		}

	/* Projection type; int
	 * AND
	 * Projection coefficients array; double[15]
	 *  Entire coefficients array is 0.0 for Geographic and UTM;
	 *  meta structure does not currently support geographic or albers projections */
		switch (proj->type) {
		    case SCANSAR_PROJECTION: /* Along-track/cross-track... ddr has no atct projection, default to UTM */
			/*Can't do anything here until we add AT/CT to asf_geolib.*/
			proj_invalid=1;
/*	        	printf("Warning in asf_meta library function meta2ddr:\n"
			       "    DDR files do not support JPL's along-track/cross-track projection.\n"
			       "    For valid DDR data, you should geocode your data under a different\n"
			       "    projection (use ASF tool 'geocode').\n");*/
			break;
		    case ALBERS_EQUAL_AREA:/* Albers Conic Equal Area */
			ddr->proj_code = ALBERS;
			ddr->valid[DDPCV] = VALID;
			ddr->proj_coef[0] = proj->re_major;
			ddr->proj_coef[1] = proj->re_minor;
			ddr->proj_coef[2] = packed_deg(proj->param.albers.std_parallel1); /*standard parallel1*/
			ddr->proj_coef[3] = packed_deg(proj->param.albers.std_parallel2); /*standard parallel2*/
			ddr->proj_coef[4] = packed_deg(proj->param.albers.center_meridian);  /*Center longitude of proj*/
			ddr->proj_coef[5] = packed_deg(proj->param.albers.orig_latitude);  /*Center latitude of proj*/
			ddr->proj_coef[6] = 0.0; /*False Easting*/
			ddr->proj_coef[7] = 0.0; /*False Northing*/
			ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
			break;
		    case LAMBERT_CONFORMAL_CONIC:/* Lambert Conformal Conic */
			ddr->proj_code = LAMCC;
			ddr->valid[DDPCV] = VALID;
			ddr->proj_coef[0] = proj->re_major;
			ddr->proj_coef[1] = proj->re_minor;
			ddr->proj_coef[2] = packed_deg(proj->param.lamcc.plat1); /*standard parallel1*/
			ddr->proj_coef[3] = packed_deg(proj->param.lamcc.plat2); /*standard parallel2*/
			ddr->proj_coef[4] = packed_deg(proj->param.lamcc.lon0);  /*Center longitude of proj*/
			ddr->proj_coef[5] = packed_deg(proj->param.lamcc.lat0);  /*Center latitude of proj*/
			ddr->proj_coef[6] = 0.0; /*False Easting*/
			ddr->proj_coef[7] = 0.0; /*False Northing*/
			ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
			break;
		    case POLAR_STEREOGRAPHIC:/* Polar Stereographic */
			ddr->proj_code = PS;
			ddr->valid[DDPCV] = VALID;
			ddr->proj_coef[0] = proj->re_major;
			ddr->proj_coef[1] = proj->re_minor;
			ddr->proj_coef[4] = packed_deg(proj->param.ps.slon);/*Longitude down below pole of map*/
			ddr->proj_coef[5] = packed_deg(proj->param.ps.slat);/*Latitude of true scale*/
			ddr->proj_coef[6] = 0.0; /*False Easting*/
			ddr->proj_coef[7] = 0.0; /*False Northing*/
			ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
			break;
		    case UNIVERSAL_TRANSVERSE_MERCATOR:/* Universal Transverse Mercator */
			ddr->proj_code = UTM;
			ddr->valid[DDPCV] = VALID;
		    /*Unnecessary since the zone is specified*/
			ddr->proj_coef[0] = meta->general->center_latitude; /*any longitude in proj*/
			ddr->proj_coef[1] = meta->general->center_longitude;/*any latitude in proj*/
			ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
			break;
		    default:/*Der?*/
			printf("Unrecognized map projection type '%c' passed to meta2ddr!\n",proj->type);
			printf("Continuing...\n");
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
		ddr->loleft[0] = proj->startY + meta->general->line_count * proj->perY;
		ddr->loleft[1] = proj->startX;
		ddr->upright[0] = proj->startY;
		ddr->upright[1] = proj->startX + meta->general->sample_count * proj->perX;
		ddr->loright[0] = proj->startY + meta->general->line_count * proj->perY;
		ddr->loright[1] = proj->startX + meta->general->sample_count * proj->perX;
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
