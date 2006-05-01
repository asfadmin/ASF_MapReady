/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   These are the external entry points.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independance.
****************************************************************/
#include <assert.h>
#include "asf.h"
#include "asf_meta.h"
#include "asf_nan.h"

/*Interferometry calls:*/

/*******************************************************************************
 * meta_get_sat_height:
 * Return the satellite height (in meters) from the center of the earth at a
 * specific line & sample of the image */
double meta_get_sat_height(meta_parameters *meta, long line, long sample)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	double sat_height = MAGIC_UNSET_DOUBLE;
	
	/* Satellite height should be calculated (as it is in the 'else'), but
	   due to delivery schedule restrictions we had to fall back on the old
	   way of retrieving this info (storing it in the .meta file) */
	if (meta_is_valid_double(meta->sar->satellite_height)) {
		sat_height = meta->sar->satellite_height;
	}
	else {
		double time;
		stateVector stVec;

		time = meta_get_time(meta, line, sample);
        	stVec = meta_get_stVec(meta, time);
        	sat_height = sqrt(  stVec.pos.x * stVec.pos.x
		                  + stVec.pos.y * stVec.pos.y
				  + stVec.pos.z * stVec.pos.z);
	}
	
	return sat_height;
}

/*******************************************************************************
 * meta_get_earth_radius:
 * Return the earth radius (in meters) at a specific line & sample of the image
 */
double meta_get_earth_radius(meta_parameters *meta, long line, long sample)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	double earth_rad = MAGIC_UNSET_DOUBLE;

	/* Earth radius should be calculated (as it is in the 'else'), but due
	   to delivery schedule restrictions we had to fall back on the old way
	   of retrieving this info (storing it in the .meta file) */
	if (meta_is_valid_double(meta->sar->earth_radius)) {
		earth_rad = meta->sar->earth_radius;
	}
	else {
		double re, rp, lat, ht, time;
		stateVector stVec;
	/* If re & rp are valid then set them to meta values, otherwise WGS84 */
		re = (meta_is_valid_double(meta->general->re_major))
		       ? meta->general->re_major : 6378137.0;
		rp = (meta_is_valid_double(meta->general->re_minor))
		       ? meta->general->re_minor : 6356752.31414;
	/* Actual algorithm */
		time = meta_get_time(meta, line, sample);
        	stVec = meta_get_stVec(meta, time);
        	ht = sqrt(  stVec.pos.x * stVec.pos.x
		          + stVec.pos.y * stVec.pos.y
			  + stVec.pos.z * stVec.pos.z);
        	lat = asin(stVec.pos.z/ht);
        	earth_rad = (re*rp)
		        / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
	}
	
	return earth_rad;
}

void meta_get_slants(meta_parameters *meta,double *slantFirst, double *slantPer)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	*slantFirst = meta->sar->slant_range_first_pixel;
	*slantPer   = meta->general->x_pixel_size;
}

double meta_get_k(meta_parameters *meta)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	return 2*PI/meta->sar->wavelength;
}
double meta_scene_frac(meta_parameters *meta,int y)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	return (double)(y - meta->sar->original_line_count/2)
			/ (double)meta->sar->original_line_count;
}
void meta_interp_baseline(meta_parameters *meta,const baseline base,int y,double *Bn_y,double *Bp_y)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	double frac=meta_scene_frac(meta,y);
	*Bn_y = base.Bn + base.dBn*frac;
	*Bp_y = base.Bp + base.dBp*frac;
}
double meta_flat(meta_parameters *meta,double y,double x)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL 
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	return meta_look(meta,y,x) - meta_look(meta, 0, meta->sar->original_sample_count/2);
}
double meta_flat_phase(meta_parameters *meta,const baseline base,int y,int x)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	   || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	double flat=meta_flat(meta,y,x);
	double Bn_y,Bp_y;
	meta_interp_baseline(meta,base,y,&Bn_y,&Bp_y);
	return 2.0*meta_get_k(meta)*(Bp_y*cos(flat)-Bn_y*sin(flat));
}
double meta_phase_rate(meta_parameters *meta,const baseline base,int y,int x)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	double sr=meta_get_slant(meta,y,x);
	double flat=meta_flat(meta,y,x);
	double incid=meta_incid(meta,y,x);
	double Bn_y,Bp_y;
	meta_interp_baseline(meta,base,y,&Bn_y,&Bp_y);
/*Note: this is the slant range times sin of the incidence angle, 
	divided by the derivative of meta_flat_phase.*/
	return (sr*sin(incid))/(2.0*meta_get_k(meta)*(-Bp_y*sin(flat)-Bn_y*cos(flat)));
}
