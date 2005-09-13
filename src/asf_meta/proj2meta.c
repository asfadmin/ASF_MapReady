#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "asf_meta.h"
#include "ddr.h"
#include "proj.h"
#include "earth_radius2datum.h"

#define EARTH_RADIUS_MICRON 0.1

double unpacked_deg(double angle);

void proj2meta(struct DDR *ddr, meta_parameters *meta)
{
	meta_projection *proj;

	/* Initialize metadata projection structure */
	proj = meta_projection_init();
		
	/* Fill in the DDR information */
	strncpy(proj->units, ddr->proj_units, 12);

	switch (ddr->proj_code) {
	  case ALBERS:/* Albers Conic Equal Area */
            meta->sar->image_type = 'P';
	    proj->type = ALBERS_EQUAL_AREA;
	    proj->re_major = ddr->proj_coef[0];
	    proj->re_minor = ddr->proj_coef[1];
	    proj->param.albers.std_parallel1 = unpacked_deg(ddr->proj_coef[2]);   /*standard parallel1*/
	    proj->param.albers.std_parallel2 = unpacked_deg(ddr->proj_coef[3]);   /*standard parallel2*/
	    proj->param.albers.center_meridian = unpacked_deg(ddr->proj_coef[4]); /*Center longitude of proj*/
	    proj->param.albers.orig_latitude = unpacked_deg(ddr->proj_coef[5]);   /*Center latitude of proj*/
	    break;
	  case LAMCC:/* Lambert Conformal Conic */
            meta->sar->image_type = 'P';
	    proj->type = LAMBERT_CONFORMAL_CONIC;
	    proj->re_major = ddr->proj_coef[0];
	    proj->re_minor = ddr->proj_coef[1];
	    proj->param.lamcc.plat1 = unpacked_deg(ddr->proj_coef[2]); /*standard parallel1*/
	    proj->param.lamcc.plat2 = unpacked_deg(ddr->proj_coef[3]); /*standard parallel2*/
	    proj->param.lamcc.lon0 = unpacked_deg(ddr->proj_coef[4]);  /*Center longitude of proj*/
	    proj->param.lamcc.lat0 = unpacked_deg(ddr->proj_coef[5]);  /*Center latitude of proj*/
	    break;
	  case LAMAZ:/* Lambert Azimuthal Equal Area */
            meta->sar->image_type = 'P';
	    proj->type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
	    proj->re_major = ddr->proj_coef[0];
	    proj->re_minor = ddr->proj_coef[1];
	    proj->param.lamaz.center_lon = unpacked_deg(ddr->proj_coef[4]); /*Center longitude of proj*/
	    proj->param.lamaz.center_lat = unpacked_deg(ddr->proj_coef[5]);
	  case PS:/* Polar Stereographic */
            meta->sar->image_type = 'P';
	    proj->type = POLAR_STEREOGRAPHIC;
	    proj->re_major = ddr->proj_coef[0];
	    proj->re_minor = ddr->proj_coef[1];
	    proj->param.ps.slon = ddr->proj_coef[4]; /*Longitude down below pole of map*/
	    proj->param.ps.slat = ddr->proj_coef[5]; /*Latitude of true scale*/
	    break;
	  case UTM:/* Universal Transverse Mercator */
            meta->sar->image_type = 'P';
	    proj->type = UNIVERSAL_TRANSVERSE_MERCATOR;
	    proj->param.utm.zone = ddr->zone_code;
	    break;
	}

	/* Datum Code */
	datum2earth_radius(ddr->datum_code,
                           &(proj->re_major), &(proj->re_minor));

	/* Starting Coordinates */
	proj->startY = ddr->upleft[0];
	proj->startX = ddr->upleft[1];
	proj->perY = (ddr->loright[0] - proj->startY) / meta->general->line_count;
	proj->perX = (ddr->loright[1] - proj->startX) / meta->general->sample_count;
	proj->hem = (meta->general->center_latitude>0) ? 'N' : 'S';

	meta->projection = proj;
}
