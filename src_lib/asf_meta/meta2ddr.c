#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "asf_meta.h"
#include "ddr.h"
#include "proj.h"

#define EARTH_RADIUS_MICRON 0.1
#define FLOAT_COMPARE(a, b) (abs(a - b) < EARTH_RADIUS_MICRON ? 1 : 0)

/***********Arrays taken from asf_geolib's sphdz.c (Sept 2002)************
CODE     DATUM NAME                 EQUITORIAL RADIUS       POLAR RADIUS
 0:   Clarke 1866                      6378206.4           6356583.8
 1:   Clarke 1880                      6378249.145         6356514.86955
 2:   Bessel                           6377397.155         6356078.96284
 3:   International 1967               6378157.5           6356772.2
 4:   International 1909               6378388.0           6356911.94613
 5:   WGS 72                           6378135.0           6356750.519915
 6:   Everest                          6377276.3452        6356075.4133                          
 7:   WGS 66                           6378145.0           6356759.769356                      
 8:   GRS 1980/WGS 84                  6378137.0           6356752.31414   
 9:   Airy                             6377563.396         6356256.91
10:   Modified Everest                 6377304.063         6356103.039        
11:   Modified Airy                    6377340.189         6356034.448
12:   Walbeck                          6378137.0           6356752.314245
13:   Southeast Asia                   6378155.0           6356773.3205
14:   Australian National              6378160.0           6356774.719
15:   Krassovsky                       6378245.0           6356863.0188                          
16:   Hough                            6378270.0           6356794.343479
17:   Mercury 1960                     6378166.0           6356784.283666
18:   Modified Mercury 1968            6378150.0           6356768.337303   
19:   Sphere of Radius 6370997 meters  6370997.0           6370997.0
*************************************************************************/
int earth_radius2datum(double re_major, double re_minor)
{
	static double major[20] = /*EQUITORIAL RADIUS*/
		{6378206.4,      6378249.145,    6377397.155,    6378157.5,      6378388.0,
		 6378135.0,      6377276.3452,   6378145.0,      6378137.0,      6377563.396,
		 6377304.063,    6377340.189,    6378137.0,      6378155.0,      6378160.0,
		 6378245.0,      6378270.0,      6378166.0,      6378150.0,      6370997.0};
	static double minor[20] = /*POLAR RADIUS*/
		{6356583.8,      6356514.86955,  6356078.96284,  6356772.2,      6356911.94613,
		 6356750.519915, 6356075.4133,   6356759.769356, 6356752.31414,  6356256.91,
		 6356103.039,    6356034.448,    6356752.314245, 6356773.3205,   6356774.719,
		 6356863.0188,   6356794.343479, 6356784.283666, 6356768.337303, 6370997.0};
	int major_index = 0, minor_index = 0;

	for (major_index=0; major_index<20; major_index++) {
		if ( FLOAT_COMPARE(major[major_index],re_major) ) {
			for (minor_index=0; minor_index<20; minor_index++) {
				if ( FLOAT_COMPARE(minor[minor_index],re_minor) ) {
					if ( FLOAT_COMPARE(major_index,minor_index) ) {
					 	return major_index;
					}
				}
			}
		}
	}
	if ( FLOAT_COMPARE(major[minor_index],re_major) ) {
		return minor_index;
	}

	printf("\n"
	       "WARNING: Function earth_radius2datum was unable to figure\n"
	       "         supported datum... setting DDR datum code to -1\n");
	return (-1);
}

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
		ddr->zone_code = (proj->type=='U') ? proj->param.utm.zone : 62;
		ddr->valid[DDZCV] = VALID;
	/* Projection type; int
	 * AND
	 * Projection coefficients array; double[15]
	 *  Entire coefficients array is 0.0 for Geographic and UTM;
	 *  meta structure does not currently support geographic or albers projections */
		switch (proj->type) {
		    case 'A': /* Along-track/cross-track... ddr has no atct projection, default to UTM */
			/*Can't do anything here until we add AT/CT to asf_geolib.*/
			proj_invalid=1;
	        	printf("Warning in asf_meta library function meta2ddr:\n"
			       "    DDR files do not support JPL's along-track/cross-track projection.\n"
			       "    For valid DDR data, you should geocode your data under a different\n"
			       "    projection (use ASF tool 'geocode').\n");
			break;
		    case 'L':/* Lambert Conformal Conic */
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
		    case 'P':/* Polar Stereographic */
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
		    case 'U':/* Universal Transverse Mercator */
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
