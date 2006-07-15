#ifndef _JPL_PROJ_H_
#define _JPL_PROJ_H_

/******************************************************************************
NAME:JPL_proj

SYNOPSIS:
	A set of map projection routines from JPL.
Includes Along-Track/Cross-Track (AT/CT), 
Universal Transverse Mercator,
Polar Sterographic, and Lambert.

******************************************************************************/

#include "asf_meta/meta_parameters.h" /* for meta_projection */
#include "asf_meta/util.h" /* for stateVector */

/*Initialize Along-Track/Cross-Track projection with
given fixed-earth state vector.*/
void atct_init(asf::meta_projection *proj,asf::stateVector stVec);

/*Convert given longitude (degrees) to UTM zone code.*/
int UTM_zone(double longitude);

/*Convert projection units (meters) to geodetic latitude and longitude (degrees).*/
void ll_to_proj(asf::meta_projection *proj,char look_dir,double lat_d,double lon,double *p1,double *p2);
void proj_to_ll(asf::meta_projection *proj,char look_dir,double p1, double p2, double *lat_d, double *lon);
#endif
