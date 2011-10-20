/****************************************************************
NAME:  deskew_dem

USAGE:  deskew_dem [-i inSARfile bit] [-log <file>]]
			<inDEMfile> <outfile>

SYNOPSIS:

    deskew_dem removes incidence angle skew from a slant-range
    DEM, and interpolates across areas that didn't phase unwrap.

    If <outfile> has the extension .dem or .ht, deskew_dem will
    remove the incidence angle skew from the input slant-range DEM.

    If the <outfile> has the extention of .img, or .amp, deskew_dem
    will output a terrain-corrected amplitude image, based on the input file
    <inDEMfile>.

    If the -g option is passed, the terrain correction is only
    geometric-- no radiometric incidence angle normalization will
    occur.

    The -log switch allows the output to be written to a log file.

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:     PURPOSE:
    ---------------------------------------------------------------
    1.0	     7/97  O. Lawlor   Deskew Interferometry DEMs.
    1.1	     6/98  O. Lawlor   Made more consistent with reskew_dem.
    1.2	     7/01  R. Gens     Added log file switch.
    1.35     4/02  P. Denny    Updated commandline parsing & usage()
    2.0      2/04  P. Denny    Removed use of DDR; upgraded to meta v1.1
                                Removed <ceos> command line argument
                                Fix sr2gr & gr2sr functions from leaving memory
                                Use newer io functions (eg: get_float_line)

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   deskew_dem -- this program removes incidence-angle skew and maps from   *
*		  slant range to ground range.				    *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/

#include "asf.h"
#include "asf_meta.h"
#include "asf_sar.h"
#include "vector.h"
#include "asf_raster.h"
#include <string.h>
#include <assert.h>

struct deskew_dem_data {
        int numLines, numSamples;
        double grPixelSize;
        double *slantGR;/*Slant range pixel #*/
        double *heightShiftGR;
        double *heightShiftSR;
        double *groundSR;/*Ground range pixel #*/
        double *slantRangeSqr,*slantRange;
        double *incidAng,*sinIncidAng,*cosIncidAng;
        double minPhi, maxPhi, phiMul;
	double *cosineScale;
        meta_parameters *meta;
};

static const float badDEMht=BAD_DEM_HEIGHT;
//static int maxBreakLen=20;
static const int maxBreakLen=5;

static int n_layover=0;
static int n_shadow=0;
static int n_user=0;

#define phi2grX(phi) (((phi)-d->minPhi)*d->phiMul)
#define grX2phi(gr) (d->minPhi+(gr)/d->phiMul)

static int eq(float a, float b, float tol)
{
    return fabs(a-b)<tol;
}

static float SR2GR(struct deskew_dem_data *d, float srX, float height)
{
        if (srX<0) srX=0;
        if (srX>=d->numSamples-1) srX=d->numSamples-1;
	double dx,srXSeaLevel=srX-height*d->heightShiftSR[(int)srX];
	int ix;
    /*Prevent ix index (and ix+1) from leaving the bounds of allotted memory*/
	if (srXSeaLevel<0) srXSeaLevel=0;
	if (srXSeaLevel>=d->numSamples-1)  srXSeaLevel=d->numSamples-2;
	ix=(int)srXSeaLevel;
	dx=srXSeaLevel-ix;
    /*Linear interpolation on groundSR array*/
	return d->groundSR[ix]+dx*(d->groundSR[ix+1]-d->groundSR[ix]);
}

static float dem_gr2sr(struct deskew_dem_data *d,float grX,float height)
{
	double dx,grXSeaLevel=grX-height*d->heightShiftGR[(int)grX];
	int ix;
    /*Prevent ix index (and ix+1) from leaving the bounds of allotted memory*/
	if (grXSeaLevel<0) grXSeaLevel=0;
	if (grXSeaLevel>=d->numSamples-1)  grXSeaLevel=d->numSamples-2;
	ix=(int)grXSeaLevel;
	dx=grXSeaLevel-ix;
    /*Linear interpolation on slantGR array*/
	return d->slantGR[ix]+dx*(d->slantGR[ix+1]-d->slantGR[ix]);
}

static void dem_sr2gr(struct deskew_dem_data *d,float *inBuf,float *outBuf,
                      int ns, int fill_holes)
{
    int outX=0,inX,xInterp;
    int lastOutX=-1;
    float lastOutValue=badDEMht;

    for (outX=0; outX<ns; outX++)
        outBuf[outX]=badDEMht;

    for (inX=0;inX<ns;inX++)
    {
        float height=inBuf[inX];
        outX=(int)SR2GR(d,(float)inX,height);
        
        if ((height>badDEMht)&&(outX>=0)&&(outX<ns))
        {
            // if either end is NO_DEM_DATA, fill the range with that value
            if (eq(lastOutValue,NO_DEM_DATA,.0001) ||
                eq(height,NO_DEM_DATA,.0001))
            {
              for (xInterp=lastOutX+1;xInterp<=outX;xInterp++)
                    outBuf[xInterp]=NO_DEM_DATA;                
            }
            // normal case: two valid slant range value to interpolate through
            else if (lastOutValue!=badDEMht &&
                (fill_holes || outX-lastOutX<maxBreakLen))
            {
                float curr=lastOutValue;
                float delt=(height-lastOutValue)/(outX-lastOutX);
                curr+=delt;
                for (xInterp=lastOutX+1;xInterp<=outX;xInterp++) {
                  outBuf[xInterp]=curr;
                  curr+=delt;
                }
            }
            lastOutValue=height;
            lastOutX=outX;
        }
    }
}

static double calc_ranges(struct deskew_dem_data *d,meta_parameters *meta)
{
    int x;
    double slantFirst,slantPer;
    double er=meta_get_earth_radius(meta, meta->general->line_count/2,
                                    meta->general->sample_count/2);
    double satHt=meta_get_sat_height(meta, meta->general->line_count/2,
                                     meta->general->sample_count/2);
    double saved_ER=er;
    double er2her2,phi,phiAtSeaLevel,slantRng;
    int ns = meta->general->sample_count;

    meta_get_slants(meta,&slantFirst,&slantPer);
    slantFirst+=slantPer*meta->general->start_sample+1;
    slantPer*=meta->sar->sample_increment;
    er2her2=er*er-satHt*satHt;
    d->minPhi=acos((satHt*satHt+er*er-slantFirst*slantFirst)/
                   (2.0*satHt*er));
    
/*Compute arrays indexed by slant range pixel:*/
    for (x=0;x<ns;x++)
    {
	/*Precompute slant range for SR pixel x.*/
        d->slantRange[x]=slantFirst+x*slantPer;
        d->slantRangeSqr[x]=d->slantRange[x]*d->slantRange[x];
	/*Compute incidence angle for SR pixel x.*/
        d->incidAng[x]=M_PI-acos((d->slantRangeSqr[x]+er2her2)/
                                 (2.0*er*d->slantRange[x]));
        d->sinIncidAng[x]=sin(d->incidAng[x]);
        d->cosIncidAng[x]=cos(d->incidAng[x]);
    }
    
    d->maxPhi=acos((satHt*satHt+er*er-d->slantRangeSqr[ns-1])/(2.0*satHt*er));
    d->phiMul=(ns-1)/(d->maxPhi-d->minPhi);

/*Compute arrays indexed by ground range pixel: slantGR and heightShiftGR*/
    for (x=0;x<ns;x++)
    {
        er=saved_ER;
        phiAtSeaLevel=grX2phi(x);
        slantRng=sqrt(satHt*satHt+er*er-2.0*satHt*er*cos(phiAtSeaLevel));
        d->slantGR[x]=(slantRng-slantFirst)/slantPer;
        er+=1000.0;
        phi=acos((satHt*satHt+er*er-slantRng*slantRng)/(2*satHt*er));
        d->heightShiftGR[x]=(phi2grX(phi)-x)/1000.0;
    }
/*Compute arrays indexed by slant range pixel: groundSR and heightShiftSR*/
    for (x=0;x<ns;x++)
    {
        er=saved_ER;
        phiAtSeaLevel=acos((satHt*satHt+er*er-d->slantRangeSqr[x])/
                           (2*satHt*er));
        d->groundSR[x]=phi2grX(phiAtSeaLevel);
        er+=1000.0;
        slantRng=sqrt(satHt*satHt+er*er-2.0*satHt*er*cos(phiAtSeaLevel));
        d->heightShiftSR[x]=((slantRng-slantFirst)/slantPer-x)/1000.0;
    }
    er=saved_ER;
    return er/d->phiMul;
}

static void mask_float_line(int ns, int fill_value, float *in, float *inMask,
                            float *grDEM, struct deskew_dem_data *d, int interp)
{
    int x;
    for (x=0; x<ns; x++)
    {
        if (inMask[x] == MASK_USER_MASK)
        {
            ++n_user;

            // a -1 indicates leave the actual data.
            // other values are user specified values that should be put in
            if (fill_value != LEAVE_MASK)
                in[x] = fill_value;
        }

        // where we have no DEM data, set output to 0
        if (eq(grDEM[x],NO_DEM_DATA,.0001)) {
          in[x] = 0.0;
        }

        // where we have layover, we output to 0, if -no-interp was specified
        if (interp && (inMask[x]==MASK_LAYOVER || inMask[x]==MASK_SHADOW)) {
          in[x] = 0.0;
        }
    }
}

static void geo_compensate(struct deskew_dem_data *d,float *grDEM, float *in,
                           float *out, int ns, int doInterp, float *mask,
                           int line)
{
    int i,grX;
    int valid_data_yet=0;
    const int num_hits_required_for_layover=2;
    int *sr_hits=NULL;
    float max_height=grDEM[0];
    double last_good_height = 0;
    double max_valid_srX = -1;
    double max_valid_srX_height = 0;

    if (mask) {
        // The "sr_hits" tracks points in ground range that map to the same
        // point in slant range, for the purposes of detecting layover
        sr_hits=(int*)MALLOC(sizeof(int)*ns*num_hits_required_for_layover);

        for (grX=0; grX<ns; ++grX) {
            if (mask[grX] != MASK_USER_MASK && mask[grX] != MASK_INVALID_DATA)
                mask[grX] = MASK_NORMAL;

            // Initially, all slant range points haven't been hit ==> -1
            for (i=0; i<num_hits_required_for_layover; ++i)
                sr_hits[i*ns+grX] = -1;
        }
    }

    // height of the satellite at this line
    double sat_ht = meta_get_sat_height(d->meta, line, d->numSamples/2);

    // shadow tracker -- this is the negative cosine of the biggest look
    // angle found so far.  As we move across we image, this should increase
    // if it doesn't ==> shadow
    double biggest_look = -2;

    for (grX=0;grX<ns;grX++)
    {
        double height=grDEM[grX];
        if (height < -900) height = badDEMht;

        if (height!=badDEMht)
        {
            double srX=dem_gr2sr(d,grX,height);
            if (height > max_height) max_height = height;
            last_good_height = height;

            if (srX >= 1 && srX < ns-1)
            {
                int x=floor(srX);
                double dx=srX-x;
                if (doInterp) {
                    /* bilinear interp */
                    out[grX]=(1-dx)*in[x] + dx*in[x+1];
                }
                else {
                    /* nearest neighbor */
                    out[grX]= dx <= 0.5 ? in[x] : in[x+1];
                }
                valid_data_yet=1;

                if (srX > max_valid_srX) {
                    max_valid_srX = srX;
                    max_valid_srX_height = height;
                }

                if (sr_hits) {
                    //--------------------------------------------------------
                    // Layover 
                    int is_layover = TRUE; // until we learn otherwise
                    for (i=0; i<num_hits_required_for_layover; ++i) {
                        if (sr_hits[i*ns+x] == -1) {
                            // i'th time we hit this pixel, save the grX that
                            // led us here for later
                            sr_hits[i*ns+x] = grX;
                            is_layover = FALSE;
                            break;
                        }
                    }
                    if (is_layover) {
                        // mask all pixels that landed here (at x) as layover
                        for (i=0; i<num_hits_required_for_layover; ++i) {
                            if (mask[sr_hits[i*ns+x]] == MASK_NORMAL) {
                                ++n_layover;
                                mask[sr_hits[i*ns+x]] = MASK_LAYOVER;
                            }
                        }
                        // including the current one
                        if (mask[grX] == MASK_NORMAL) {
                            mask[grX] = MASK_LAYOVER;
                            ++n_layover;
                        }
                    }

                    //--------------------------------------------------------
                    // Shadow
                    double h = sat_ht;
                    // first calculate at height==0, get phi (the angle
                    // between sat_ht and er).  The meta_get_slant call should
                    // be quick since we are in slant range already.
                    double sr = meta_get_slant(d->meta, line, grX);
                    double er = meta_get_earth_radius(d->meta, line, grX);
                    double phi_cos_x2 = (h*h + er*er - sr*sr)/(h*er);
                    // now account for the height
                    er += grDEM[grX];
                    sr = sqrt(h*h + er*er - h*er*phi_cos_x2);
                    // so, cur_look is the (cosine of the) look angle when
                    // pointing at the terrain
                    double cur_look = - (sr*sr + h*h - er*er)/(2*sr*h);
                    
                    if (cur_look >= biggest_look) {
                        // normal case -- no shadow
                        biggest_look = cur_look;
                    } else {
                        // this point is shadowed by the point that generated
                        // the current "biggest_look" value
                        if (mask[grX] == MASK_NORMAL) {
                            mask[grX] = MASK_SHADOW;
                            ++n_shadow;
                        }
                    }
                }
            }
            else {
                // source value for this pixel outside the image -- use 0.
                out[grX] = 0;
            }
        }
        else {
            // Bad DEM height.

            // Hard to say what to do here.  It depends on what the bad DEM
            // height means -- if it is water, we can just copy over the pixel
            // but if it means outside the DEM, we should leave it blank?

            // Try using "last_good_height" ... should be close ...
            // and then copy the pixel over.

            // Note that because of the "valid_data_yet" condition we shouldn't
            // ever use a bad (uninitialized, i.e. 0) "last_good_height" value

            // User can use the mask to eliminate the copied pixels, if desired
            double srX=dem_gr2sr(d,grX,last_good_height);

            if (valid_data_yet && srX >= 0 && srX < ns-1) {
                out[grX] = in[(int)(srX+.5)];

                if (out[grX] != 0.0) {
                    if (srX > max_valid_srX) {
                        max_valid_srX = srX;
                        max_valid_srX_height = last_good_height;
                    }
                }
            } 
            else {
                out[grX] = 0;
            }
        }
    }

    // close 1-pixel holes in the mask
    if (mask) {
        for (grX=2;grX<ns-2;grX++) {
            if (mask[grX]==MASK_NORMAL) {
                if (mask[grX-1]==MASK_LAYOVER && mask[grX+1]==MASK_LAYOVER) {
                    ++n_layover;
                    mask[grX]=MASK_LAYOVER;
                }
                else if (mask[grX-1]==MASK_SHADOW && mask[grX+1]==MASK_SHADOW) {
                    ++n_shadow;
                    mask[grX]=MASK_SHADOW;
                }
            }
        }
        for (grX=3;grX<ns-3;grX++) {
            if (mask[grX-1]==MASK_NORMAL && mask[grX]==MASK_NORMAL &&
                mask[grX-2]==MASK_LAYOVER && mask[grX+1]==MASK_LAYOVER)
            {
                mask[grX-1] = MASK_LAYOVER;
                mask[grX] = MASK_LAYOVER;
                n_layover += 2;
            }
        }
    }

    //-----------------------------------------------------------------------
    // Invalid data, on the left & right edges

    // Find the left & right edges of the valid data in slant range, convert
    // to ground range, then mark from those points outward as invalid data.
    double min_valid_srX = ns-1;
    double min_valid_srX_height = 0;
    last_good_height = 0;
    for (grX=ns-1;grX>=0;grX--)
    {
        double height=grDEM[grX];
        if (height > -900 && height!=badDEMht) {
            last_good_height = height;
            double srX=dem_gr2sr(d,grX,height);
            if (srX>=0 && srX < min_valid_srX) {
                min_valid_srX = srX;
                min_valid_srX_height = height;
            }
        }
        else {
            double srX=dem_gr2sr(d,grX,last_good_height);
            if (srX>=0 && srX < min_valid_srX) {
                min_valid_srX = srX;
                min_valid_srX_height = last_good_height;
                if (out[grX] == 0.0)
                    out[grX] = in[(int)(srX+.5)];
            }
        }
    }

    if (mask) {
        int max_valid_grX = 
            (int)floor(SR2GR(d, max_valid_srX, max_valid_srX_height));
        int min_valid_grX =
            (int)ceil(SR2GR(d, min_valid_srX, min_valid_srX_height));

        if (max_valid_grX < ns-1 && max_valid_grX >= 0)        
            for (i=max_valid_grX; i<ns; ++i)
                mask[i] = MASK_INVALID_DATA;
        if (min_valid_grX < ns-1 && min_valid_grX >= 0)        
            for (i=min_valid_grX; i>=0; --i)
                if (out[i]==0.0) mask[i] = MASK_INVALID_DATA;

        free(sr_hits);
    }
}

static void geodetic_to_ecef(double lat, double lon, double h, Vector *v)
{
  const double a = 6378144.0;    // GEM-06 Ellipsoid.
  const double e = 8.1827385e-2; // GEM-06 Eccentricity
  const double e2 = e*e;

  lat *= D2R;
  lon *= D2R;

  double sin_lat = sin(lat);
  double cos_lat = cos(lat);

  double f = sqrt(1 - e2*sin_lat*sin_lat);
  double af = a/f;

  v->x = (af + h)*cos_lat*cos(lon);
  v->y = (af + h)*cos_lat*sin(lon);
  v->z = (a*(1-e2)/f + h)*sin_lat;
}

static Vector get_satpos(meta_parameters *meta, int line)
{
  int ns = meta->general->sample_count;
  double t = meta_get_time(meta, line, ns/2);

  // find the state vector closest to the specified time
  int ii,closest_ii=0;
  double closest_diff=9999999;
  for (ii=0; ii<meta->state_vectors->vector_count; ++ii) {
    double diff = fabs(meta->state_vectors->vecs[ii].time - t);
    if (diff < closest_diff) {
      closest_ii = ii;
      closest_diff = diff;
    }
  }

  stateVector closest_vec = meta->state_vectors->vecs[closest_ii].vec;
  double closest_time = meta->state_vectors->vecs[closest_ii].time;

  stateVector stVec = propagate(closest_vec, closest_time, t);

  Vector satpos;
  satpos.x = stVec.pos.x;
  satpos.y = stVec.pos.y;
  satpos.z = stVec.pos.z;
  return satpos;
}

static void calculate_vectors_for_line(meta_parameters *meta_img, float *demLine, int line, Vector **vectorLine, Vector *nextVectors, Vector *verticals)
{
  int jj;
  double lat, lon;
  int ns = meta_img->general->sample_count;

  for(jj = 0; jj < ns; ++jj) {
    Vector *v = MALLOC(sizeof(Vector));
    meta_get_latLon(meta_img, line, jj, 0, &lat, &lon);
    geodetic_to_ecef(lat, lon, demLine[jj], v);
    vectorLine[jj] = v;

    Vector v2;
    geodetic_to_ecef(lat, lon, demLine[jj], &verticals[jj]);
    geodetic_to_ecef(lat, lon, demLine[jj]+100, &v2);
    vector_subtract(&verticals[jj], &v2);
    vector_multiply(&verticals[jj], 1./vector_magnitude(&verticals[jj]));

    meta_get_latLon(meta_img, line+1, jj, 0, &lat, &lon);
    geodetic_to_ecef(lat, lon, demLine[jj], &nextVectors[jj]);
  }
}

static void push_next_vector_line(Vector ***localVectors, Vector *nextVectors, Vector *verticals, meta_parameters *meta_img, float *demLine, int line)
{
  int ns = meta_img->general->sample_count;
  Vector **vectorsLine = MALLOC(sizeof(Vector*)*ns);
  calculate_vectors_for_line(meta_img, demLine, line, vectorsLine, nextVectors, verticals);

  int i;
  for(i = 0; i < ns; i++)
    if(localVectors[0] != NULL)
      vector_free(localVectors[0][i]);
  FREE(localVectors[0]);

  localVectors[0] = localVectors[1];
  localVectors[1] = localVectors[2];
  localVectors[2] = vectorsLine;
}

static Vector * calculate_normal(Vector ***localVectors, int sample)
{
  Vector *v1, *v2, *normal;

  v1 = vector_copy(localVectors[0][sample]);
  vector_subtract(v1, localVectors[2][sample]);

  v2 = vector_copy(localVectors[1][sample-1]);
  vector_subtract(v2, localVectors[1][sample+1]);

  normal = vector_cross(v2, v1);
  vector_multiply(normal, 1./vector_magnitude(normal));

  vector_free(v1);
  vector_free(v2);
  
  return normal;
}

static float
calculate_correction(meta_parameters *meta_in, int line, int samp,
                     Vector *satpos, Vector *n, Vector *p, Vector *p_next)
{
  // R: vector from ground point (p) to satellite (satpos)
  Vector *R = vector_copy(satpos);
  vector_subtract(R, p);
  vector_multiply(R, 1./vector_magnitude(R));

  Vector *x = vector_copy(p);
  vector_subtract(x, p_next);
  vector_multiply(x, 1./vector_magnitude(x));

  // Rx: R cross x -- image plane normal
  Vector *Rx = vector_cross(R,x);

  // cos(phi) is the correction factor we need
  double cosphi = vector_dot(Rx,n);
  //if (cosphi < 0) cosphi = -cosphi;

  vector_free(x);
  vector_free(R);
  vector_free(Rx);

  // need to remove old correction factor (sin of the incidence angle)
  double incid = meta_incid(meta_in, line, samp);
  return cosphi / sin(incid);
}

static void shift_gr(struct deskew_dem_data *d, float *in, float *out)
{
    int x, newX, ns=d->numSamples;

    for (x=0; x<ns; ++x) {
        newX = (int)floor(d->slantGR[x]);
        if (newX<0) {
            out[x] = in[0];
        }
        else if (newX>ns-2) {
            out[x] = in[ns-1];
        }
        else {
            // simple linear interp
            double frac = d->slantGR[x] - (double)newX;
            out[x] = in[newX]*(1.-frac) + in[newX+1]*frac;
        }
    }
}

static void get_dem_line(FILE * inDemGroundFp, meta_parameters *metaDEMground, FILE *inDemSlantFp,
                         meta_parameters *metaDEMSlant, int which_gr_dem, struct deskew_dem_data *d,
                         int line, float *tmpbuf, float *backconverted_dem, float *grDem_geo_out, float *grDem_rad_out)
{
  get_float_line (inDemSlantFp, metaDEMSlant, line, tmpbuf);
  dem_sr2gr(d, tmpbuf, backconverted_dem, d->numSamples, TRUE /* fill_holes */ );

  if(inDemGroundFp) {
    get_float_line (inDemGroundFp, metaDEMground, line, tmpbuf);
    shift_gr (d, tmpbuf, grDem_rad_out);
  }
  else {
    memcpy(grDem_rad_out, backconverted_dem, sizeof(float)*d->numSamples);
  }

  if(which_gr_dem == ORIGINAL_GR_DEM) {
    memcpy(grDem_geo_out, grDem_rad_out, sizeof(float)*d->numSamples);
  }
  else {
    memcpy(grDem_geo_out, backconverted_dem, sizeof(float)*d->numSamples);
  }
}

static void push_dem_lines(FILE * inDemGroundFp, meta_parameters *metaDEMground, FILE *inDemSlantFp,
                         meta_parameters *metaDEMSlant, int which_gr_dem, struct deskew_dem_data *d,
                         int line, float *tmpbuf, float **backconverted_dem, float **grDem_geo_out, float **grDem_rad_out)
{
  float *geoDemLine, *radDemLine, *backconvertedDemLine;
  if(line >= d->numLines) {
    geoDemLine = radDemLine = backconvertedDemLine = NULL;
  }
  else {
    geoDemLine = MALLOC(sizeof(float)*d->numSamples);
    radDemLine = MALLOC(sizeof(float)*d->numSamples);
    backconvertedDemLine = MALLOC(sizeof(float)*d->numSamples);

    get_dem_line(inDemGroundFp, metaDEMground, inDemSlantFp, metaDEMSlant, which_gr_dem,
                 d, line, tmpbuf, backconvertedDemLine, geoDemLine, radDemLine);
  }

  FREE(grDem_geo_out[0]);
  FREE(backconverted_dem[0]);
  FREE(grDem_rad_out[0]);

  int x;
  for(x = 0; x < 2; x++) {
    grDem_rad_out[x] = grDem_rad_out[x+1];
    grDem_geo_out[x] = grDem_geo_out[x+1];
    backconverted_dem[x] = backconverted_dem[x+1];
  }

  grDem_rad_out[2] = radDemLine;
  grDem_geo_out[2] = geoDemLine;
  backconverted_dem[2] = backconvertedDemLine;
}

static void filter_mask(char *maskName)
{
  int ii, jj;
  meta_parameters *meta = meta_read(maskName);
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  FILE *fp = fopenImage (maskName, "rb");
  float *buf = MALLOC(sizeof(float)*ns*nl);
  get_float_lines(fp, meta, 0, nl, buf); 
  FCLOSE(fp);

  int iter=1;
  while (iter < 100) {
    int num = 0;
    for (jj=2; jj<ns-2; ++jj) {
      for (ii=2; ii<nl-4; ++ii) {
        if (buf[ii*ns + jj] == MASK_NORMAL &&
            buf[(ii-1)*ns + jj] == MASK_LAYOVER &&
            (buf[(ii+1)*ns + jj] == MASK_LAYOVER || buf[(ii+2)*ns + jj] == MASK_LAYOVER ||
             buf[(ii+3)*ns + jj] == MASK_LAYOVER))
        {
          buf[ii*ns + jj] = MASK_LAYOVER;
          ++num;
          ++n_layover;
        }
      }
    }
    asfPrintStatus("Vertical iter %d, added: %d\n", iter, num);
    if (num > 0) {
      num = 0;
      for (ii=2; ii<nl-2; ++ii) {
        for (jj=2; jj<ns-4; ++jj) {
          if (buf[ii*ns + jj] == MASK_NORMAL &&
              buf[ii*ns + jj-1] == MASK_LAYOVER &&
              (buf[ii*ns + jj+1] == MASK_LAYOVER || buf[ii*ns + jj+2] == MASK_LAYOVER ||
               buf[ii*ns + jj+3] == MASK_LAYOVER))
          {
            buf[ii*ns + jj] = MASK_LAYOVER;
            ++num;
            ++n_layover;
          }
        }
      }
      asfPrintStatus("Horizontal iter %d, added: %d\n", iter, num);
    }
    if (num == 0)
      break;
    ++iter;
  }

  asfPrintStatus("Layover smoothing took %d iterations.\n", iter);
  asfPrintStatus("Writing filtered mask...\n");
  fp = fopenImage(maskName, "wb");
  for (ii=0; ii<nl; ++ii)
    put_float_line(fp, meta, ii, buf + ii*ns);
  FCLOSE(fp);
  FREE(buf); 
  meta_free(meta);
}

/* inSarName can be NULL, in this case doRadiometric is ignored */
/* inMaskName can be NULL, in this case outMaskName is ignored */
int deskew_dem (char *inDemSlant, char *inDemGround, char *outName,
            char *inSarName, int doRadiometric, char *inMaskName,
            char *outMaskName, int fill_holes, int fill_value,
            int which_gr_dem)
{
  float *inSarLine;
  FILE *inDemSlantFp, *inDemGroundFp = NULL, *inSarFp, *outFp,
    *inMaskFp = NULL, *outMaskFp = NULL;
  meta_parameters *metaDEMslant, *metaDEMground = NULL, *outMeta,
    *inSarMeta, *inMaskMeta = NULL;
  char **bands = NULL;
  char msg[256];
  int ns, inSarFlag, inMaskFlag, outMaskFlag;
  register int x, y, b;
  struct deskew_dem_data d;
  int band_count = 1;           // in case no SAR image is passed in

  inSarFlag = inSarName != NULL;
  inMaskFlag = inMaskName != NULL;
  outMaskFlag = outMaskName != NULL;

  inSarFp = NULL;
  inSarMeta = NULL;

/*Extract metadata*/
  metaDEMslant = meta_read (inDemSlant);
  if (inDemGround)
    metaDEMground = meta_read (inDemGround);
  outMeta = meta_read (inDemSlant);

  if (metaDEMslant->sar->image_type == 'P') {
    asfPrintError ("DEM cannot be map projected for this program "
                   "to work!\n");
    return FALSE;
  }
  if (inSarFlag) {
    inSarMeta = meta_read (inSarName);
    band_count = inSarMeta->general->band_count;
    d.meta = inSarMeta;
    if (inSarMeta->sar->image_type == 'P') {
      asfPrintError ("SAR image cannot be map projected for this "
                     "program to work!\n");
      return FALSE;
    }
    outMeta->general->data_type = inSarMeta->general->data_type;
    bands = 
      extract_band_names(inSarMeta->general->bands, inSarMeta->general->band_count);
  }
  else {
    d.meta = NULL;
  }

  d.numLines = metaDEMslant->general->line_count;
  d.numSamples = metaDEMslant->general->sample_count;
  ns = d.numSamples;
  if (metaDEMground && metaDEMground->general->sample_count != ns) {
    asfPrintError ("Slant/Ground mismatch. %d %d\n",
                   metaDEMground->general->sample_count, ns);
  }

/*Allocate vectors.*/
  d.slantGR = (double *) MALLOC (sizeof (double) * ns);
  d.groundSR = (double *) MALLOC (sizeof (double) * ns);
  d.heightShiftSR = (double *) MALLOC (sizeof (double) * ns);
  d.heightShiftGR = (double *) MALLOC (sizeof (double) * ns);
  d.slantRange = (double *) MALLOC (sizeof (double) * ns);
  d.slantRangeSqr = (double *) MALLOC (sizeof (double) * ns);
  d.incidAng = (double *) MALLOC (sizeof (double) * ns);
  d.sinIncidAng = (double *) MALLOC (sizeof (double) * ns);
  d.cosIncidAng = (double *) MALLOC (sizeof (double) * ns);
  d.cosineScale = NULL;

/*Set up the output meta file.*/
  d.grPixelSize = calc_ranges (&d, metaDEMslant);
  outMeta->sar->image_type = 'G';
  outMeta->general->x_pixel_size = d.grPixelSize;

/* We use 0 to fill in around the edges (currently user can't configure
   this value), so set the no_data value accordingly */
  outMeta->general->no_data = 0.0;

/*Open files.*/
  inDemSlantFp = fopenImage (inDemSlant, "rb");
  if (inDemGround)
    inDemGroundFp = fopenImage (inDemGround, "rb");

  outFp = fopenImage (outName, "wb");
  if (inSarFlag) {
    inSarFp = fopenImage (inSarName, "rb");
    outMeta->general->band_count = inSarMeta->general->band_count;
    strcpy (outMeta->general->bands, inSarMeta->general->bands);
  }
  if (inMaskFlag) {
    if (!inSarFlag)
      asfPrintError ("Cannot produce a mask without a SAR!\n");
    inMaskMeta = meta_read (inMaskName);

    meta_general *smg = inSarMeta->general;
    meta_general *mmg = inMaskMeta->general;
    if ((smg->line_count != mmg->line_count) ||
        (smg->sample_count != mmg->sample_count)) {
      asfPrintStatus (" SAR Image: %dx%d LxS.\n"
                      "Mask Image: %dx%d LxS.\n",
                      inSarMeta->general->line_count,
                      inSarMeta->general->sample_count,
                      inMaskMeta->general->line_count,
                      inMaskMeta->general->sample_count);

      asfPrintError ("The mask and the SAR image must be the "
                     "same size.\n");
    }
  }

/* output file's metadata is all set, now */
  meta_write (outMeta, outName);

/* Blather at user about what is going on */
  strcpy (msg, "");
  if (inDemGroundFp)
    sprintf (msg, "%sDEM is in ground range.\n", msg);
  else
    sprintf (msg, "%sDEM is in slant range, but will be corrected.\n", msg);

  if (inSarFlag)
    sprintf (msg, "%sCorrecting image", msg);
  else
    sprintf (msg, "%sCorrecting DEM", msg);

  if (doRadiometric)
    sprintf (msg, "%s geometrically and radiometrically.\n", msg);
  else
    sprintf (msg, "%s geometrically.\n", msg);

  asfPrintStatus (msg);

/*Allocate input buffers.*/
  if (inSarFlag) {
    inSarLine = (float *) MALLOC (sizeof (float) * ns);
  }
  else {
    inSarLine = NULL;
  }

  float corrections[ns];
  float angles[ns];
  float maskLine[ns];
  float outLine[ns];
  Vector verticals[ns];
  float *localRadDemLines[3] = { NULL, NULL, NULL };
  float *localGeoDemLines[3] = { NULL, NULL, NULL };
  float *localbackconvertedDemLines[3] = { NULL, NULL, NULL };
  Vector **localVectors[3] = { NULL, NULL, NULL };
  Vector nextVectors[ns];

  n_layover = n_shadow = n_user = 0;

/* Initialize side products */
  const char *tmpdir = get_asf_tmp_dir();
  char *sideProductsImg = MALLOC(sizeof(char)*(strlen(tmpdir)+64));
  sprintf(sideProductsImg, "%s/terrcorr_side_products.img", tmpdir);
  char *sideProductsMeta = appendExt(sideProductsImg, ".meta");
  meta_parameters *side_meta = meta_copy(metaDEMslant);

  if (doRadiometric) {
    side_meta->general->band_count  = 4;
    strcpy(side_meta->general->bands, "INCIDENCE_ANGLE,DEM_HEIGHT,RADIOMETRIC_CORRECTION,ANGLES");
  } else {
    side_meta->general->band_count  = 2;
    strcpy(side_meta->general->bands, "INCIDENCE_ANGLE,DEM_HEIGHT");
  }

  FILE *sideProductsFp = FOPEN(sideProductsImg, "wb");

/*Open the mask, if we have one*/
  if (inMaskFlag)
    inMaskFp = fopenImage (inMaskName, "rb");
  if (outMaskFlag)
    outMaskFp = fopenImage (outMaskName, "wb");

  push_dem_lines(inDemGroundFp, metaDEMground, inDemSlantFp, metaDEMslant, which_gr_dem,
                 &d, 0, outLine, localbackconvertedDemLines, localGeoDemLines, localRadDemLines);
  if(doRadiometric)
    push_next_vector_line(localVectors, nextVectors, verticals, inSarMeta, localRadDemLines[2], 0);

  /*Rectify data.*/
  for (y = 0; y < d.numLines; y++) {
    push_dem_lines(inDemGroundFp, metaDEMground, inDemSlantFp, metaDEMslant, which_gr_dem,
                   &d, y+1, outLine, localbackconvertedDemLines, localGeoDemLines, localRadDemLines);
    if(y < d.numLines - 1 && doRadiometric)
      push_next_vector_line(localVectors, nextVectors, verticals, inSarMeta, localRadDemLines[2], y+1);

    /* Make an empty mask */
    for (x = 0; x < ns; ++x)
      maskLine[x] = 1;
    
    if (inMaskFlag) {
      // Read in the next line of the mask, update the values
      get_float_line (inMaskFp, inMaskMeta, y, maskLine);
      for (x = 0; x < ns; ++x) {
        if (maskLine[x] == 2.0)
          maskLine[x] = MASK_INVALID_DATA;
        else if (is_masked (maskLine[x]))
          maskLine[x] = MASK_USER_MASK;
      }

      geo_compensate (&d, localGeoDemLines[1], maskLine, outLine, ns, 0, NULL, y);

      for (x = 0; x < ns; ++x)
        maskLine[x] = outLine[x];
    }

    for (x=0; x<ns; ++x) {
      corrections[x] = (float)(R2D*d.incidAng[x]);
    }
    put_band_float_line(sideProductsFp, side_meta, 0, y, corrections); // Record the incidence angles
    put_band_float_line(sideProductsFp, side_meta, 1, y, localGeoDemLines[1]);

    if (doRadiometric) {
      if (y > 0 && y < d.numLines - 1) {
#ifndef ALTERNATIVE_NORMALS
        // method from rtc
        Vector satpos = get_satpos(inSarMeta, y);
        for(x=1; x < ns-1; ++x) {
          Vector * normal = calculate_normal(localVectors, x);
          corrections[x] = calculate_correction(inSarMeta, y, x, &satpos, normal, localVectors[1][x], &nextVectors[x]);
          // If the Ulander correction is ever negative, that is layover
          if (corrections[x] < 0) {
            if (maskLine[x] == MASK_NORMAL) {
              ++n_layover;
              maskLine[x] = MASK_LAYOVER;
            }
            corrections[x] *= -1;
          }
          angles[x] = R2D * acos(vector_dot(normal, &verticals[x]));
          vector_free(normal);
        }
#else
        // method we'd like to use here in deskew_dem
        // FIXME: this method does not appear to work
        Vector vert = {0, 0, 1};
        Vector X = {0, -1, 0};
        for(x=1; x < ns-1; ++x) {          
          Vector terrainNormal, R, *RX;
          terrainNormal.x=(localGeoDemLines[1][x-1]-localGeoDemLines[1][x+1])/(2*d.grPixelSize);
          terrainNormal.y=(localGeoDemLines[2][x]-localGeoDemLines[0][x])/(2*d.grPixelSize);
          terrainNormal.z=1.0;
          vector_multiply(&terrainNormal, 1./vector_magnitude(&terrainNormal));
          R.x = -d.sinIncidAng[x];
          R.y = 0;
          R.z = d.cosIncidAng[x];
          RX = vector_cross(&R, &X);
          double cosphi = fabs(vector_dot(&terrainNormal, RX));
          corrections[x] = (cosphi / d.sinIncidAng[x]);
          angles[x] = R2D * acos(vector_dot(&terrainNormal, &vert));
          vector_free(RX);
        }
#endif
        // now store everything
        put_band_float_line(sideProductsFp, side_meta, 2, y, corrections);
        put_band_float_line(sideProductsFp, side_meta, 3, y, angles);
      }
      else {
        for(x = 0; x < ns; x++) {
          corrections[x] = 1.0;
          angles[x] = 0;
        }
        put_band_float_line(sideProductsFp, side_meta, 2, y, corrections);
        put_band_float_line(sideProductsFp, side_meta, 3, y, angles);
      }
    }

    // do this line in all of the bands
    for (b = 0; b < band_count; ++b) {
      if (inSarFlag) {
        get_band_float_line (inSarFp, inSarMeta, b, y, inSarLine);

        geo_compensate (&d, localGeoDemLines[1], inSarLine, outLine,
                        ns, 1, maskLine, y);
        if((y == 0 || y == d.numLines - 1) && doRadiometric) {
          memset(outLine, 0, sizeof(float)*ns);
        }
        else if (y > 0 && y < d.numLines - 1 && doRadiometric) {
          for(x = 0; x < ns; ++x) {
            outLine[x] = get_rad_cal_dn(inSarMeta, y, x, bands[b], outLine[x], corrections[x]);
          }
          outLine[0] = outLine[ns-1] = 0;
        }
      }

      // subtract away the masked region
      mask_float_line (ns, fill_value, outLine,
                       maskLine, localbackconvertedDemLines[1], &d, !fill_holes);

      put_band_float_line (outFp, outMeta, b, y, outLine);
    }
    if (outMaskFlag)
      put_float_line (outMaskFp, outMeta, y, maskLine);

    asfLineMeter (y, d.numLines);
  }
  FCLOSE(sideProductsFp);

  meta_write(side_meta, sideProductsMeta);
  meta_free(side_meta);

  if (inMaskFlag) {
    FCLOSE (inMaskFp);
    meta_free (inMaskMeta);
  }

/*Write the updated mask*/
  if (outMaskFlag) {
    FCLOSE (outMaskFp);

    // the mask has just 1 band, regardless of how many input has
    outMeta->general->band_count = 1;
    strcpy (outMeta->general->bands, "LAYOVER_MASK");

    // mask doesn't really have a radiometry, just set amp
    outMeta->general->radiometry = r_AMP;

    // write the mask's metadata, then print mask stats
    meta_write (outMeta, outMaskName);
  
    asfPrintStatus("Cleaning up layover/shadow mask...\n");
    filter_mask(outMaskName);

    int tot = ns * d.numLines;
    asfPrintStatus ("Mask Statistics:\n"
                    "    Layover Pixels: %9d/%d (%f%%)\n"
                    "     Shadow Pixels: %9d/%d (%f%%)\n"
                    "User Masked Pixels: %9d/%d (%f%%)\n",
                    n_layover, tot, 100. * (float) n_layover / tot,
                    n_shadow, tot, 100. * (float) n_shadow / tot,
                    n_user, tot, 100. * (float) n_user / tot);
  }

/* Clean up & skidattle */
  for(y = 0; y < 3; ++y) {
    FREE(localRadDemLines[y]);
    FREE(localGeoDemLines[y]);
    FREE(localbackconvertedDemLines[y]);
  }

  for (y = 0; y < inSarMeta->general->band_count; y++) {
    FREE(bands[y]);
  }
  FREE(bands);

  for(y = 0; y < 3; ++y) {
    if (localVectors[y]) {
      for(x = 0; x < ns; ++x) {
        vector_free(localVectors[y][x]);
      }
    }
    FREE(localVectors[y]);
  }

  if (inSarFlag) {
    FREE (inSarLine);
    FCLOSE (inSarFp);
    meta_free (inSarMeta);
  }
  FCLOSE (inDemSlantFp);
  FCLOSE (inDemGroundFp);
  FCLOSE (outFp);
  meta_free (metaDEMslant);
  if (metaDEMground)
    meta_free (metaDEMground);
  meta_free (outMeta);
  FREE (d.slantGR);
  FREE (d.groundSR);
  FREE (d.heightShiftSR);
  FREE (d.heightShiftGR);
  FREE (d.slantRange);
  FREE (d.slantRangeSqr);
  FREE (d.incidAng);
  FREE (d.sinIncidAng);
  FREE (d.cosIncidAng);
  if (d.cosineScale)
    FREE (d.cosineScale);
  FREE(sideProductsImg);
  FREE(sideProductsMeta);

  return TRUE;
}
