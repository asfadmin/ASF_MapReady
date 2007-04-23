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

static float SR2GR(struct deskew_dem_data *d, float srX, float height)
{
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

    for (inX=0;inX<ns;inX++)
    {
        float height=inBuf[inX];
        outX=(int)SR2GR(d,(float)inX,height);

        if ((height!=badDEMht)/*&&(height!=-1)*/&&(outX>=0)&&(outX<ns))
        {
            if (lastOutValue!=badDEMht &&
                (fill_holes || outX-lastOutX<maxBreakLen))
            {
                float curr=lastOutValue;
                float delt=(height-lastOutValue)/(outX-lastOutX);
                curr+=delt;
                for (xInterp=lastOutX+1;xInterp<=outX;xInterp++)
                {
                    outBuf[xInterp]=curr;
                    curr+=delt;
                }
            } else {
                for (xInterp=lastOutX+1;xInterp<=outX;xInterp++)
                    outBuf[xInterp]=badDEMht;
            }
            lastOutValue=height;
            lastOutX=outX;
        }
        //lastOutValue=height;
        //lastOutX=outX;
    }
    for (outX=lastOutX+1;outX<ns;outX++) {
        outBuf[outX]=badDEMht;
    }
}

static double calc_ranges(struct deskew_dem_data *d,meta_parameters *meta)
{
    int x;
    double slantFirst,slantPer;
    double er=meta_get_earth_radius(meta, meta->general->line_count/2, meta->general->sample_count/2);
    //double er=meta_get_earth_radius_pp(meta);
    double satHt=meta_get_sat_height(meta, meta->general->line_count/2, meta->general->sample_count/2);
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
    
    d->maxPhi=acos((satHt*satHt+er*er-d->slantRangeSqr[ns-1])/
                   (2.0*satHt*er));
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

static int eq(float a, float b, float tol)
{
    return fabs(a-b)<tol;
}

static void mask_float_line(int ns, int fill_value, float *in, float *inMask,
                            float *grDEM, struct deskew_dem_data *d)
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
        if (eq(grDEM[x],NO_DEM_DATA,.000001)) {
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
    const int num_hits_required_for_layover=3;
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

            if (srX >= 0 && srX < ns-1)
            {
                int x=floor(srX);
                double dx=srX-x;
                if (doInterp) {
                    /* bilinear interp */
                    out[grX]=(1-dx)*in[x] + dx*in[x+1];
                } else {
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
        if (height > -900 && height!=badDEMht)
        {
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
        
        for (i=max_valid_grX; i<ns; ++i)
            mask[i] = MASK_INVALID_DATA;
        for (i=min_valid_grX; i>=0; --i)
            mask[i] = MASK_INVALID_DATA;

        free(sr_hits);
    }
}

static void radio_compensate(struct deskew_dem_data *d,float *grDEM,
                             float *grDEMprev,float *inout,int ns,
                             int line, int form)
{

/*
Here's what it looked like before optimization:
		double dx,dy,dz,vecLen,ix,iy,iz,cosAng;
	  Find terrain normal.
		dx=(grDEM[x-1]-grDEM[x])/grPixelSize;
		dy=(grDEMprev[x]-grDEM[x])/grPixelSize;
		dz=1.0;
	  Make the normal a unit vector.
		vecLen=1.0/sqrt(dx*dx+dy*dy+dz*dz);
		dx*=vecLen;
		dy*=vecLen;
		dz*=vecLen;
	  Find the incidence angle normal.
	  	ix=-sin(incidAng[x]);
	  	iy=0;
	  	iz=cos(incidAng[x]);
	  Take dot product of these two vectors.
		cosAng=dx*ix+dy*iy+dz*iz;
		if (cosAng<0)
		{ 
	Shadowed area.
			inout[x]=(1&(x/10))?200:100;
		} else {
		Ordinary diffuse radar reflection--
	scale by phong equation.
			double cosScaled=pow(cosAng,10);
			double normFact=1.0-0.99*cosScaled;
			inout[x]*=normFact;
		}

*/

/*** FIXME
     For 3.1, we force the user to use correction formula #5
     That's the only one that's been tested... */
    if (form > 0) form = 5;
    //if (strncmp(CONVERT_PACKAGE_VERSION_STRING, "3.1", 3) != 0)
    //    asfPrintWarning("Radiometric terrain correction formula "
    //                    "is forced to #5!\n");

    int x;
    for (x=1;x<ns;x++)
    {
        double dx,dy,grX,vecLen,cosAng;

        /*Find terrain normal.*/
        grX=grDEM[x];
        if ((grX==badDEMht)||
            (grDEMprev[x]==badDEMht)||
            (grDEM[x-1]==badDEMht)) 
        {
            inout[x]=0;
            continue;
        }

        dx=(grX-grDEM[x-1])/d->grPixelSize;
        dy=(grDEMprev[x]-grX)/d->grPixelSize;

        /*Make the normal a unit vector.*/
        vecLen=sqrt(dx*dx+dy*dy+1);

        /*Take dot product of this vector and the incidence vector.*/
        cosAng=(-dx*d->sinIncidAng[x]+d->cosIncidAng[x])/vecLen;

        double li = acos(cosAng);
        double gi = meta_incid(d->meta, line, x);

        double tanphie = tan(gi);
        double cosphi = cosAng;
        double sinphir = fabs(gi+asin(dy/sqrt(dy*dy+1)));
        double cosphia = 1/sqrt(dx*dx+1);

        if (cosAng>=0) {
            switch (form) {
                default:
                case 0:
                    /* should not be in here... */
                    asfPrintError("Bad radiometric correction formula: %d\n",
                                  form);
                    return;
                case 1:
                    /* From the old terrcorr: ftcli */
                    inout[x] *= tan(li) / tanphie;
                    break;
                case 2:
                    /* From the old terrcorr: ftcgo */
                    inout[x] *= (sinphir * cosphia) / (tanphie * cosphi);
                    break;
                case 3:
                    /* From the old terrcorr: ftcsq */
                    inout[x] *= (sinphir * cosphia) / (tanphie * sqrt(cosphi));
                    break;
                case 4:
                    /* From the old terrcorr: ftcvx */
                    inout[x] *= (sinphir * cosphia) / sin(gi);
                    break;
                case 5:
                    /* Ordinary diffuse radar reflection */
                    /* This is the formula that was previously in deskew_dem */
                    inout[x] *= 1-.7*pow(cosAng,7);
                    break;
                case 22:
                    /* Secret test mode */
                    inout[x] = 4+(float)x/ns;
                    break;
            }
        }
    }
}

/* inSarName can be NULL, in this case doRadiometric is ignored */
/* inMaskName can be NULL, in this case outMaskName is ignored */
int deskew_dem(char *inDemName, char *outName, char *inSarName,
	       int doRadiometric, char *inMaskName, char *outMaskName,
               int fill_holes, int fill_value)
{
	float *srDEMline,*grDEMline,*grDEMlast,*inSarLine,*outLine,*maskLine;
	FILE *inDemFp,*inSarFp,*outFp,*inMaskFp=NULL,*outMaskFp=NULL;
	meta_parameters *inDemMeta, *outMeta, *inSarMeta, *inMaskMeta=NULL;
	char msg[256];
	int inSarFlag,inMaskFlag,outMaskFlag;
	int dem_is_ground_range=FALSE;
	register int x,y,b;
        struct deskew_dem_data d;

	inSarFlag = inSarName != NULL;
        inMaskFlag = inMaskName != NULL;
        outMaskFlag = outMaskName != NULL;

	inSarFp = NULL;
	inSarMeta = NULL;

/*Extract metadata*/
	inDemMeta = meta_read(inDemName);
	outMeta = meta_read(inDemName);

        if (doRadiometric)
            asfPrintWarning("Radiometric terrain correction is still "
                            "experimental.\n");

        if (inDemMeta->sar->image_type == 'G')
	   dem_is_ground_range=TRUE;

	if (inDemMeta->sar->image_type=='P') {
		asfPrintError("DEM cannot be map projected for this program to work!\n");
		return FALSE;
	}
	if (inSarFlag) {
	   inSarMeta = meta_read(inSarName);
           d.meta = inSarMeta;
	   if (inSarMeta->sar->image_type=='P') {
	      asfPrintError("SAR image cannot be map projected for this program to work!\n");
	      return FALSE;
	   }
	   outMeta->general->data_type = inSarMeta->general->data_type;
        } else {
            d.meta = NULL;
        }

        d.numLines = inDemMeta->general->line_count;
        d.numSamples = inDemMeta->general->sample_count;

/*Allocate vectors.*/
	d.slantGR       = (double*)MALLOC(sizeof(double)*d.numSamples);
	d.groundSR      = (double*)MALLOC(sizeof(double)*d.numSamples);
	d.heightShiftSR = (double*)MALLOC(sizeof(double)*d.numSamples);
	d.heightShiftGR = (double*)MALLOC(sizeof(double)*d.numSamples);
	d.slantRange    = (double*)MALLOC(sizeof(double)*d.numSamples);
	d.slantRangeSqr = (double*)MALLOC(sizeof(double)*d.numSamples);
	d.incidAng      = (double*)MALLOC(sizeof(double)*d.numSamples);
	d.sinIncidAng   = (double*)MALLOC(sizeof(double)*d.numSamples);
	d.cosIncidAng   = (double*)MALLOC(sizeof(double)*d.numSamples);
        d.cosineScale   = NULL;

/*Set up the output meta file.*/
	d.grPixelSize = calc_ranges(&d,inDemMeta);
	outMeta->sar->image_type='G';
	outMeta->general->x_pixel_size = d.grPixelSize;

/*Open files.*/
	inDemFp = fopenImage(inDemName,"rb");
	outFp   = fopenImage(outName,"wb");
	if (inSarFlag) {
          inSarFp = fopenImage(inSarName,"rb");
          outMeta->general->band_count = inSarMeta->general->band_count;
          strcpy(outMeta->general->bands, inSarMeta->general->bands);
        }
        if (inMaskFlag) {
            if (!inSarFlag)
                asfPrintError("Cannot produce a mask without a SAR!\n");
            inMaskMeta = meta_read(inMaskName);
            if ((inSarMeta->general->line_count != inMaskMeta->general->line_count) ||
                (inSarMeta->general->sample_count != inMaskMeta->general->sample_count))
            {
                asfPrintStatus(" SAR Image: %dx%d LxS.\n"
                               "Mask Image: %dx%d LxS.\n",
                               inSarMeta->general->line_count,
                               inSarMeta->general->sample_count,
                               inMaskMeta->general->line_count,
                               inMaskMeta->general->sample_count);

                asfPrintError("The mask and the SAR image must be the "
                              "same size.\n");
            }
        }
        
        // output file's metadata is all set, now
	meta_write(outMeta, outName);

/* Blather at user about what is going on */
	strcpy(msg,"");
	if (dem_is_ground_range)
	  sprintf(msg,"%sDEM is in ground range.\n",msg);
	else
	  sprintf(msg,"%sDEM in slant range, but will be corrected.\n",msg);

	if (inSarFlag)
	  sprintf(msg,"%sCorrecting image",msg);
	else
	  sprintf(msg,"%sCorrecting DEM",msg);

	if (doRadiometric)
	  sprintf(msg,"%s geometrically and radiometrically.\n",msg);
	else
	  sprintf(msg,"%s geometrically.\n",msg);

	asfPrintStatus(msg);

/*Allocate input buffers.*/
	if (inSarFlag) {
	   inSarLine = (float *)MALLOC(sizeof(float)*d.numSamples);
        } else {
	   inSarLine = NULL;
        }

	outLine   = (float *)MALLOC(sizeof(float)*d.numSamples);
	srDEMline = (float *)MALLOC(sizeof(float)*d.numSamples);
        grDEMline = (float *)MALLOC(sizeof(float)*d.numSamples);
        grDEMlast = (float *)MALLOC(sizeof(float)*d.numSamples);
        maskLine  = (float *)MALLOC(sizeof(float)*d.numSamples);

        n_layover = n_shadow = n_user = 0;

/*Open the mask, if we have one*/
        if (inMaskFlag)
            inMaskFp = fopenImage(inMaskName, "rb");
        if (outMaskFlag)
            outMaskFp = fopenImage(outMaskName, "wb");

/* Make an empty mask */
        for (x=0; x<d.numSamples; ++x)
            maskLine[x] = 1;

/*Rectify data.*/
	for (y=0;y<d.numLines;y++) {

            /*Read in DEM line-by-line (keeping two lines buffered)*/
            float *tmp=grDEMline;
            grDEMline=grDEMlast;
            grDEMline=tmp;
            if (dem_is_ground_range) {
                get_float_line(inDemFp,inDemMeta,y,grDEMline);
            } else {
                get_float_line(inDemFp,inDemMeta,y,srDEMline);
                dem_sr2gr(&d,srDEMline,grDEMline,d.numSamples,fill_holes);
            }
            
            if (inMaskFlag) {
                /* Read in the next line of the mask, update the values */
                get_float_line(inMaskFp,inMaskMeta,y,maskLine);
                for (x=0; x<d.numSamples; ++x) {
                    if (maskLine[x]==2.0)
                        maskLine[x] = MASK_INVALID_DATA;
                    else if (is_masked(maskLine[x]))
                        maskLine[x] = MASK_USER_MASK;
                }

                geo_compensate(&d,grDEMline,maskLine,outLine,
                               d.numSamples,0,NULL,y);

                for (x=0; x<d.numSamples; ++x)
                    maskLine[x] = outLine[x];
            }

            // do this line in all of the bands
            for (b=0; b<inSarMeta->general->band_count; ++b) {
                get_band_float_line(inSarFp,inSarMeta,b,y,inSarLine);

                geo_compensate(&d,grDEMline,inSarLine,outLine,
                               d.numSamples,1,maskLine,y);

                if (y>0&&doRadiometric)
                  radio_compensate(&d,grDEMline,grDEMlast,outLine,
                                   d.numSamples,y,doRadiometric);

                // subtract away the masked region
                mask_float_line(d.numSamples,fill_value,outLine,
                                maskLine,grDEMline,&d);

                put_band_float_line(outFp,outMeta,b,y,outLine);
            }
            if (outMaskFlag)
                put_float_line(outMaskFp,outMeta,y,maskLine);

            asfLineMeter(y,d.numLines);
	}

        if (inMaskFlag) {
            FCLOSE(inMaskFp);
            meta_free(inMaskMeta);
        }

/*Write the updated mask*/
        if (outMaskFlag) {
            FCLOSE(outMaskFp);

            // the mask has just 1 band, regardless of how many input has
            outMeta->general->band_count = 1;
            strcpy(outMeta->general->bands, "");

            // write the mask's metadata, then print mask stats
            meta_write(outMeta, outMaskName);
            int tot=d.numSamples*d.numLines;
            printf("Mask Statistics:\n"
                   "    Layover Pixels: %9d/%d (%f%%)\n"
                   "     Shadow Pixels: %9d/%d (%f%%)\n"
                   "User Masked Pixels: %9d/%d (%f%%)\n",
                   n_layover, tot, 100*(float)n_layover/tot, 
                   n_shadow, tot, 100*(float)n_shadow/tot,
                   n_user, tot, 100*(float)n_user/tot);
        }

/* Clean up & skidattle */
	if (inSarFlag) {
	   FREE(inSarLine);
	   FCLOSE(inSarFp);
	   meta_free(inSarMeta);
	}
	FREE(srDEMline);
	FREE(grDEMlast);
	FREE(grDEMline);
	FREE(outLine);
	FCLOSE(inDemFp);
	FCLOSE(outFp);
	meta_free(inDemMeta);
	meta_free(outMeta);
	FREE(d.slantGR);
	FREE(d.groundSR);
	FREE(d.heightShiftSR);
	FREE(d.heightShiftGR);
	FREE(d.slantRange);
	FREE(d.slantRangeSqr);
	FREE(d.incidAng);
	FREE(d.sinIncidAng);
	FREE(d.cosIncidAng);
        if (d.cosineScale) FREE(d.cosineScale);

	return TRUE;
}
