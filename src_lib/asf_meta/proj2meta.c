#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "asf_meta.h"
#include "ddr.h"
#include "proj.h"
#include "earth_radius2datum.h"

#define EARTH_RADIUS_MICRON 0.1

void proj2meta(struct DDR *ddr, meta_parameters *meta)
{
	int ii;
	int proj_invalid = 0;
	meta_projection *proj;

	/* Initialize metadata projection structure */
	proj = meta_projection_init();
		
	/* Fill in the DDR information */
	strncpy(proj->units, ddr->proj_units, 12);

	switch (ddr->proj_code) {
	  case ALBERS:/* Albers Conic Equal Area */
	    proj->type = ALBERS_EQUAL_AREA;
	    proj->re_major = ddr->proj_coef[0];
	    proj->re_minor = ddr->proj_coef[1];
	    proj->param.albers.std_parallel1 = ddr->proj_coef[2];   /*standard parallel1*/
	    proj->param.albers.std_parallel2 = ddr->proj_coef[3];   /*standard parallel2*/
	    proj->param.albers.center_meridian = ddr->proj_coef[4]; /*Center longitude of proj*/
	    proj->param.albers.orig_latitude = ddr->proj_coef[5];   /*Center latitude of proj*/
	    break;
	  case LAMCC:/* Lambert Conformal Conic */
	    proj->type = LAMBERT_CONFORMAL_CONIC;
	    proj->re_major = ddr->proj_coef[0];
	    proj->re_minor = ddr->proj_coef[1];
	    proj->param.lamcc.plat1 = ddr->proj_coef[2]; /*standard parallel1*/
	    proj->param.lamcc.plat2 = ddr->proj_coef[3]; /*standard parallel2*/
	    proj->param.lamcc.lon0 = ddr->proj_coef[4];  /*Center longitude of proj*/
	    proj->param.lamcc.lat0 = ddr->proj_coef[5];  /*Center latitude of proj*/
	    break;
	  case PS:/* Polar Stereographic */
	    proj->type = POLAR_STEREOGRAPHIC;
	    proj->re_major = ddr->proj_coef[0];
	    proj->re_minor = ddr->proj_coef[1];
	    proj->param.ps.slon = ddr->proj_coef[4]; /*Longitude down below pole of map*/
	    proj->param.ps.slat = ddr->proj_coef[5]; /*Latitude of true scale*/
	    break;
	  case UNIVERSAL_TRANSVERSE_MERCATOR:/* Universal Transverse Mercator */
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

}
