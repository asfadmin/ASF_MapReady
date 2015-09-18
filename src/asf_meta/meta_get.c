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
  1.6 - P. Denny    01/03     Added meta_get_system
****************************************************************/
#include <assert.h>
#include "asf.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "dateUtil.h"

#ifndef SQR
# define SQR(x) ((x)*(x))
#endif

#ifdef MIN
#  undef MIN
#endif
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#ifdef MAX
#  undef MAX
#endif
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

/*General Calls:*/

/*********************************************************
 * meta_get_system: Figures byte ordering system
 * straight copy of c_getsys "algorithm" for DDR */
char *meta_get_system(void)
{
#if defined(ASF_BIG_IEEE)
    return "BIG_IEEE";
#elif defined(ASF_LIL_IEEE)
    return "LIL_IEEE";
#elif defined(cray_float)
    return "cray_float";
#else
    return MAGIC_UNSET_STRING;
#endif
}

/* SAR calls */
/************************************************************
 * meta_get_time:
 * Convert a line, sample pair to a time, slant-range pair.
 * These only work for SR and GR images.  They apply the time
 * and slant range correction fudge factors. Returns seconds
 * and meters.*/
double meta_get_time(meta_parameters *meta,double yLine, double xSample)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
      || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

    /*Slant or ground range -- easy.*/
  if (meta->sar->image_type=='S' || meta->sar->image_type=='G') // ||
//        (meta->sar->image_type=='P' && meta->projection->type==SCANSAR_PROJECTION))
    {
          return (yLine+meta->general->start_line)*meta->sar->azimuth_time_per_pixel+
              meta->sar->time_shift;
    }
    /*Map projected -- not as easy.*/
    else if (meta->sar->image_type=='P' || meta->sar->image_type=='R') {
        double time,slant;
        meta_get_timeSlantDop(meta,yLine,xSample,&time,&slant,NULL);
        return time + meta->sar->time_shift;
    }
    else /*Unknown projection type.*/
    {
        printf("Error!  Unknown SAR image type '%c' passed to meta_get_time!\n",
            meta->sar->image_type);
        exit(1);
    }
    return 0.0;/*<- for whining compilers.*/
}
double meta_get_slant(meta_parameters *meta,double yLine, double xSample)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
      || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

    if (meta->sar->image_type=='S')/*Slant range is easy.*/
        return meta->sar->slant_range_first_pixel
            + (xSample+meta->general->start_sample) * meta->general->x_pixel_size
            + meta->sar->slant_shift;
    else if (meta->sar->image_type=='G')/*Ground range is tougher.*/
    {/*We figure out the ground angle, phi, for this pixel, then
    use that and the law of cosines to find the slant range.*/
        double er = meta_get_earth_radius(meta,yLine,xSample);
        double ht = meta_get_sat_height(meta,yLine,xSample);
        double minPhi = acos((SQR(ht)+SQR(er)
            - SQR(meta->sar->slant_range_first_pixel)) / (2.0*ht*er));
        double phi = minPhi+
            (xSample+meta->general->start_sample)*(meta->general->x_pixel_size / er);
        double slantRng = sqrt(SQR(ht)+SQR(er)-2.0*ht*er*cos(phi));
        return slantRng + meta->sar->slant_shift;
    }
    else if (meta->sar->image_type=='P' ||
         meta->sar->image_type=='R')
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
  assert (meta->sar); // && (meta->sar->image_type == 'S'
         //   || meta->sar->image_type == 'G'));

  yLine += meta->general->start_line;
  xSample += meta->general->start_sample;
  
  if (meta->doppler && meta->doppler->tsx) {
    int ii;
    julian_date imgStartDate, imgDopplerDate;
    hms_time imgStartTime, imgDopplerTime;
    double time, refTime, *coeff;
    imgStartDate.year = meta->state_vectors->year;
    imgStartDate.jd = meta->state_vectors->julDay;
    date_sec2hms(meta->state_vectors->second, &imgStartTime);
    imgDopplerDate.year = meta->doppler->tsx->year;
    imgDopplerDate.jd = meta->doppler->tsx->julDay;
    date_sec2hms(meta->doppler->tsx->second, &imgDopplerTime);
    double imgAzimuthTime = date2sec(&imgStartDate, &imgStartTime) +
      yLine * meta->sar->range_time_per_pixel;
    double dopAzimuthStart = date2sec(&imgDopplerDate, &imgDopplerTime);
    for (ii=0; ii<meta->doppler->tsx->doppler_count; ii++) {
      time = dopAzimuthStart + meta->doppler->tsx->dop[ii].time;
      if (time > imgAzimuthTime)
	break;
    }
    int max = ii;
    int min = ii - 1;
    double dopAzimuthMin = dopAzimuthStart + meta->doppler->tsx->dop[min].time;
    double dopRangeTimeMin = meta->doppler->tsx->dop[min].first_range_time + 
      xSample * meta->sar->azimuth_time_per_pixel;
    refTime = meta->doppler->tsx->dop[min].reference_time;
    coeff = (double *) MALLOC(sizeof(double)*
			      meta->doppler->tsx->dop[min].poly_degree);
    double dopplerMin = 0.0;
    for (ii=0; ii<=meta->doppler->tsx->dop[min].poly_degree; ii++) {
      coeff[ii] = meta->doppler->tsx->dop[min].coefficient[ii];
      dopplerMin += coeff[ii] * pow(dopRangeTimeMin - refTime, ii);
    }
    FREE(coeff);
    double dopAzimuthMax = dopAzimuthStart + meta->doppler->tsx->dop[max].time;
    refTime = meta->doppler->tsx->dop[min].reference_time;
    coeff = (double *) MALLOC(sizeof(double)*
			      meta->doppler->tsx->dop[max].poly_degree);
    double dopplerMax = 0.0;
    for (ii=0; ii<=meta->doppler->tsx->dop[max].poly_degree; ii++) {
      coeff[ii] = meta->doppler->tsx->dop[max].coefficient[ii];
      dopplerMax += coeff[ii] * pow(dopRangeTimeMin - refTime, ii);
    }
    FREE(coeff);
    return dopplerMin + (dopplerMax - dopplerMin) / 
      (dopAzimuthMax - dopAzimuthMin) * (imgAzimuthTime - dopAzimuthMin);
  }
  else if (meta->doppler && meta->doppler->r2) {
    int ii;
    double doppler = 0.0;
    radarsat2_doppler_params *r2 = meta->doppler->r2;
    double slant_time = 
      r2->time_first_sample + xSample * meta->sar->range_time_per_pixel;
    for (ii=0; ii<r2->doppler_count; ++ii)
      doppler += 
	r2->centroid[ii] * pow((slant_time - r2->ref_time_centroid), ii);

    return doppler;
  }
  else
    return meta->sar->range_doppler_coefficients[0]+
      meta->sar->range_doppler_coefficients[1]*xSample+
      meta->sar->range_doppler_coefficients[2]*xSample*xSample+
      meta->sar->azimuth_doppler_coefficients[1]*yLine+
      meta->sar->azimuth_doppler_coefficients[2]*yLine*yLine;
}

/*State Vector calls */
/**********************************************************
 * meta_get_stVec:
 * Return fixed-earth state vector for the given time.
 * Steps through state vector list; then interpolates the
 * right pair.*/
stateVector meta_get_stVec(meta_parameters *meta,double time)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
      || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

    int stVecNo;
    stateVector ret;
    if (meta->state_vectors==NULL)
    {
        printf( "* ERROR in meta library function meta_get_stVec:\n"
            "* Requested a state vector, but no state vectors exist in the meta file!\n");
        exit(EXIT_FAILURE);
    }
    if (meta->state_vectors->vector_count<2)
    {
        printf( "* ERROR in meta library function meta_get_stVec:\n"
            "* Only %d state vector%s exist in file!\n",
            meta->state_vectors->vector_count,
            (meta->state_vectors->vector_count != 1) ? "s" : "");
        exit(EXIT_FAILURE);
    }
    if (meta->state_vectors->vector_count == 9) {
      // Use 8th order Legendre interpolation scheme
      ret = meta_interp_stVec(meta, time);
    }
    else {
      // Use standard interpolation scheme
      stVecNo=0;
      while (stVecNo < meta->state_vectors->vector_count - 2
	     && meta->state_vectors->vecs[stVecNo+1].time<time)
	stVecNo++;
      interp_stVec(&meta->state_vectors->vecs[stVecNo].vec,
		   meta->state_vectors->vecs[stVecNo].time,
		   &meta->state_vectors->vecs[stVecNo+1].vec,
		   meta->state_vectors->vecs[stVecNo+1].time,
		   &ret,time);
    }
    
    return ret;
}

int meta_uses_incid_polynomial(meta_parameters *meta)
{
  return strcmp_case(meta->general->sensor, "ALOS") == 0 &&
      strcmp_case(meta->general->sensor_name, "SAR") == 0 &&
      meta_is_valid_double(meta->sar->incid_a[0]) &&
      meta->sar->incid_a[0] != 0 &&
      meta->sar->incid_a[1] != 0 &&
      meta->sar->incid_a[2] != 0 &&
      meta->sar->incid_a[3] != 0 &&
      meta->sar->incid_a[4] != 0 &&
      meta->sar->incid_a[5] != 0;
}

/* Calculation calls */
/**********************************************************
 * meta_incid:  Returns the incidence angle
 * This is the angle measured by the target between straight
 * up and the satellite. Returns radians.*/
double meta_incid(meta_parameters *meta,double y,double x)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  if (strcmp_case(meta->general->sensor, "UAVSAR") == 0)
    // placeholder for incidence angle information, most likely coming from
    // a band in the image file
    return 0.0;
  else
    assert (meta->projection == NULL || 
	    meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

  double sr = meta_get_slant(meta,y,x);

  if (meta_uses_incid_polynomial(meta)) {
    // Use the incidence angle polynomial if it is available and non-zero.
    double R = sr/1000.;
    double R2=R*R;
    return
      meta->sar->incid_a[0] +
      meta->sar->incid_a[1] * R +
      meta->sar->incid_a[2] * R2 +
      meta->sar->incid_a[3] * R2 * R +
      meta->sar->incid_a[4] * R2 * R2 +
      meta->sar->incid_a[5] * R2 * R2 * R;

  } else {
    double er = meta_get_earth_radius(meta,y,x);
    double ht = meta_get_sat_height(meta,y,x);
    return PI-acos((SQR(sr) + SQR(er) - SQR(ht)) / (2.0*sr*er));
  }
}

/**********************************************************
 * meta_look: Return the look angle
 * This is the angle measured by the satellite between
 * earth's center and the target point x. Returns radians*/
double meta_look(meta_parameters *meta,double y,double x)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
      || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

    double sr = meta_get_slant(meta,y,x);
    double er = meta_get_earth_radius(meta,y,x);
    double ht = meta_get_sat_height(meta,y,x);
    return acos((SQR(sr) + SQR(ht) - SQR(er)) / (2.0*sr*ht));
}

/* meta_yaw: return yaw value at the specified point in radians */
double meta_yaw(meta_parameters *meta, double y, double x)
{
    double look, yaw, sr, t, dop;
    meta_get_timeSlantDop(meta, y, x, &t, &sr, &dop);
    stateVector st = meta_get_stVec(meta,t);
    GEOLOCATE_REC *g = init_geolocate_meta(&st,meta);
    getLookYaw(g,sr,dop,&look,&yaw);
    free_geolocate(g);
    return yaw;
}

/**********************************************************
 * slant_from_incide: Calculate the slant range, if we know
 * the incidence angle, the earth radius, and the satellite height
 * incid should be in radians */
double slant_from_incid(double incid,double er,double ht)
{
  double i=PI-incid;
  double D=SQR(ht) - SQR(er*sin(i));
  double sr=er*cos(i) + sqrt(D);

  // check degenerate cases
  if (D<0 || sr<0 || er*cos(i) - sqrt(D) > 0)
    asfPrintError("Satellite orbit is below the Earth's surface!\n");

  return sr;
}

/**********************************************************
 * look_from_incid: Calculate the look angle, if we know the
 * incidence angle, the earth radius, and the satellite height
 * incid should be in radians, returns radians. */
double look_from_incid(double incid,double er,double ht)
{
  double sr=slant_from_incid(incid,er,ht);
  return acos((SQR(sr) + SQR(ht) - SQR(er)) / (2.0*sr*ht));
}

// Code ported from Delft getorb Fortran code
// http://www.deos.tudelft.nl/ers/precorbs/tools/getorb_pack.shtml
static stateVector interpolate_prc_vector_position(meta_state_vectors *stVec, 
						   double time)
{
  stateVector outVec;

  if (stVec->vector_count != 9)
    asfPrintError("This function needs nine state vectors.\n"
		  "It should not have been called with this metadata.\n");
  double t1 = stVec->vecs[0].time;
  double tn = stVec->vecs[8].time;
  double trel = (time-t1)/(tn-t1)*8.0 + 1.0;
  int itrel = MAX(0, MIN((int)(trel + 0.5) - 4, 0));
  double x = trel - itrel;
  double teller = (x-1)*(x-2)*(x-3)*(x-4)*(x-5)*(x-6)*(x-7)*(x-8)*(x-9);
  int kx;
  if (FLOAT_EQUIVALENT(teller, 0.0)) {
    kx = (int)(x + 0.5) - 1;
    outVec.pos.x = stVec->vecs[kx].vec.pos.x;
    outVec.pos.y = stVec->vecs[kx].vec.pos.y;
    outVec.pos.z = stVec->vecs[kx].vec.pos.z;
  }
  else {
    outVec.pos.x = 0.0;
    outVec.pos.y = 0.0;
    outVec.pos.z = 0.0;
    int noemer[9] = {40320, -5040, 1440, -720, 576, -720, 1440, -5040, 40320};
    for (kx=0; kx<9; kx++) {
      double coeff = teller/noemer[kx]/(x-kx-1);
      outVec.pos.x = outVec.pos.x + coeff*stVec->vecs[kx].vec.pos.x;
      outVec.pos.y = outVec.pos.y + coeff*stVec->vecs[kx].vec.pos.y;
      outVec.pos.z = outVec.pos.z + coeff*stVec->vecs[kx].vec.pos.z;
    }
  }

  return outVec;
}

stateVector meta_interp_stVec(meta_parameters *meta,double time)
{
  stateVector velOne, velTwo, outVec;

  if (meta->state_vectors->vector_count != 9)
    asfPrintError("This function needs nine state vectors.\n"
		  "It should not have been called with this metadata.\n");

  // Determine the position of the state vector
  outVec = interpolate_prc_vector_position(meta->state_vectors, time);

  // Determine the velocity of the state vector
  velOne = interpolate_prc_vector_position(meta->state_vectors, time-0.5);
  velTwo = interpolate_prc_vector_position(meta->state_vectors, time+0.5);
  outVec.vel.x = velTwo.pos.x - velOne.pos.x;
  outVec.vel.y = velTwo.pos.y - velOne.pos.y;
  outVec.vel.z = velTwo.pos.z - velOne.pos.z;

  return outVec;
}
