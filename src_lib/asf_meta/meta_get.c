/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   These are the external entry points
for general routines.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independance.
  1.5 - P. Denny    08/02     Update to new metadate structs
****************************************************************/
#include "asf.h"
#include "asf_meta.h"

#ifndef SQR
# define SQR(x) ((x)*(x))
#endif

/*General Calls:*/
/************************************************************
 * meta_get_time:
 * Convert a line, sample pair to a time, slant-range pair.
 * These only work for SR and GR images.  They apply the time
 * and slant range correction fudge factors. Returns seconds
 * and meters.*/
double meta_get_time(meta_parameters *meta,double yLine, double xSample)
{
    /*Slant or ground range -- easy.*/
	if (meta->sar->image_type=='S' || meta->sar->image_type=='G')
		return yLine*meta->sar->azimuth_time_per_pixel+meta->sar->time_shift;
    /*Map projected -- not as easy.*/
	else if (meta->sar->image_type=='P')
	{
		double time,slant;
		meta_get_timeSlantDop(meta,yLine,xSample,&time,&slant,NULL);
		return time + meta->sar->time_shift;
	}
	else /*Unknown projection type.*/
	{
		printf("Error!  Unknown projection type '%c' passed to meta_get_time!\n",
			meta->sar->image_type);
		exit(1);
	}
	return 0.0;/*<- for whining compilers.*/
}
double meta_get_slant(meta_parameters *meta,double yLine, double xSample)
{
	if (meta->sar->image_type=='S')/*Slant range is easy.*/
		return meta->sar->slant_range_first_pixel
			+ xSample * meta->general->x_pixel_size
			+ meta->sar->slant_shift;
	else if (meta->sar->image_type=='G')/*Ground range is tougher.*/
	{/*We figure out the ground angle, phi, for this pixel, then
	use that and the law of cosines to find the slant range.*/
		double er = meta_get_earth_radius(meta,yLine,xSample);
		double ht = meta_get_sat_height(meta,yLine,xSample);
		double minPhi = acos((SQR(ht)+SQR(er)
			- SQR(meta->sar->slant_range_first_pixel)) / (2.0*ht*er));
		double phi = minPhi+xSample*(meta->general->x_pixel_size / er);
		double slantRng = sqrt(SQR(ht)+SQR(er)-2.0*ht*er*cos(phi));
		return slantRng + meta->sar->slant_shift;
	} 
	else if (meta->sar->image_type=='P')/*Map projected images are tougher.*/
	{
		double time,slant;
		meta_get_timeSlantDop(meta,yLine,xSample,&time,&slant,NULL);
		return slant+meta->sar->slant_shift;
	}
	else /*Unknown image type.*/
	{
		printf("Error!  Unknown SAR image type '%c' passed to meta_get_slant!\n",
			meta->sar->image_type);
		exit(1);
	}
	return 0.0;/*<- for whining compilers.*/
}

/*******************************************************
 * meta_get_dop:
 * Converts a line, sample pair to the doppler value at
 * that location. Returns Hz. Only works for SR & GR. */
double meta_get_dop(meta_parameters *meta,double yLine, double xSample)
{
	return meta->sar->range_doppler_coefficients[0]+
	       meta->sar->range_doppler_coefficients[1]*xSample+
	       meta->sar->range_doppler_coefficients[2]*xSample*xSample+
	       meta->sar->azimuth_doppler_coefficients[1]*yLine+
	       meta->sar->azimuth_doppler_coefficients[2]*yLine*yLine;
}


/**********************************************************
 * meta_get_stVec:
 * Return fixed-earth state vector for the given time.
 * Steps through state vector list; then interpolates the
 * right pair.*/
stateVector meta_get_stVec(meta_parameters *meta,double time)
{
	int stVecNo;
	stateVector ret;
	if (meta->state_vectors==NULL)
	{
		printf("Error! Requested a state vector, but"
			"no state vectors exist in the file!\n");
		exit(1);
	}
	if (meta->state_vectors->vector_count<2)
	{
		printf("Error! Only %d state vector exist in file!\n",
			meta->state_vectors->vector_count);
		exit(1);
	}
	stVecNo=0;
	while (stVecNo < meta->state_vectors->vector_count - 2
		&& meta->state_vectors->vecs[stVecNo+1].time<time)
		stVecNo++;
	
	interp_stVec(&meta->state_vectors->vecs[stVecNo].vec,
			 meta->state_vectors->vecs[stVecNo].time,
			&meta->state_vectors->vecs[stVecNo+1].vec,
			 meta->state_vectors->vecs[stVecNo+1].time,
			&ret,time);
	return ret;
}
/**********************************************************
 * meta_incid:  Returns the incidence angle
 * This is the angle measured by the target between straight
 * up and the satellite. Returns radians.*/
double meta_incid(meta_parameters *meta,double y,double x)
{
	double sr = meta_get_slant(meta,y,x);
	double er = meta_get_earth_radius(meta,y,x);
	double ht = meta_get_sat_height(meta,y,x);
	return PI-acos((SQR(sr) + SQR(er) - SQR(ht)) / (2.0*sr*er));
}

/**********************************************************
 * meta_look: Return the look angle
 * This is the angle measured by the satellite between
 * earth's center and the target point x. Returns radians*/
double meta_look(meta_parameters *meta,double y,double x)
{
	double sr = meta_get_slant(meta,y,x);
	double er = meta_get_earth_radius(meta,y,x);
	double ht = meta_get_sat_height(meta,y,x);
	return acos((SQR(sr) + SQR(ht) - SQR(er)) / (2.0*sr*ht));
}
