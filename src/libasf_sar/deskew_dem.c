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
#include "asf_reporting.h"

static int numLines, numSamples;
static double grPixelSize;

static double *slantGR;/*Slant range pixel #*/
static double *heightShiftGR;
static double *heightShiftSR;
static double *groundSR;/*Ground range pixel #*/
static double *slantRangeSqr,*slantRange;
static double *incidAng,*sinIncidAng,*cosIncidAng;
static double minPhi, maxPhi, phiMul;
static float badDEMht=0.0;
static int maxBreakLen=20;

#define phi2grX(phi) (((phi)-minPhi)*phiMul)
#define grX2phi(gr) (minPhi+(gr)/phiMul)

static float sr2gr(float srX,float height)
{
	double dx,srXSeaLevel=srX-height*heightShiftSR[(int)srX];
	int ix;
    /*Prevent ix index (and ix+1) from leaving the bounds of allotted memory*/
	if (srXSeaLevel<0) srXSeaLevel=0;
	if (srXSeaLevel>=numSamples-1)  srXSeaLevel=numSamples-2;
	ix=(int)srXSeaLevel;
	dx=srXSeaLevel-ix;
    /*Linear interpolation on groundSR array*/
	return groundSR[ix]+dx*(groundSR[ix+1]-groundSR[ix]);
}

static float gr2sr(float grX,float height)
{
	double dx,grXSeaLevel=grX-height*heightShiftGR[(int)grX];
	int ix;
    /*Prevent ix index (and ix+1) from leaving the bounds of allotted memory*/
	if (grXSeaLevel<0) grXSeaLevel=0;
	if (grXSeaLevel>=numSamples-1)  grXSeaLevel=numSamples-2;
	ix=(int)grXSeaLevel;
	dx=grXSeaLevel-ix;
    /*Linear interpolation on slantGR array*/
	return slantGR[ix]+dx*(slantGR[ix+1]-slantGR[ix]);
}

static void dem_sr2gr(float *inBuf,float *outBuf,int ns)
{
	int outX=0,inX,xInterp;
	int lastOutX=-1;
	float lastOutValue=badDEMht;
	for (inX=0;inX<ns;inX++)
	{
		float height=inBuf[inX];
		outX=(int)sr2gr((float)inX,height);
		if ((height!=badDEMht)&&(outX>=0)&&(outX<ns))
		{
		        /*if ((outX-lastOutX<maxBreakLen)&&(lastOutValue!=badDEMht))*/
		        if (lastOutValue!=badDEMht)
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
	}
	for (outX=lastOutX+1;outX<ns;outX++)
		outBuf[outX]=badDEMht;
}

static void dem_interp_col(float *demLine,int ns,int nl)
{
#define buf(y) (demLine[ns*(y)])
  int y,lastOutY=0;
  float lastOutVal=badDEMht;
  for (y=0;y<nl;y++)
  {
    float height=buf(y);
    if (height!=badDEMht)
     {
	if (lastOutY!=(y-1))
  	  {
	   int yInterp;
	   if (lastOutY&&(y-lastOutY<maxBreakLen))
	     {
	        float curr=lastOutVal;
		float delt=(height-lastOutVal)/(y-lastOutY);
		curr+=delt;
		for (yInterp=lastOutY+1;yInterp<=y;yInterp++)
	 	 {
		   buf(yInterp)=curr;
		   curr+=delt;
		 }
	     }	
	   }
	lastOutY=y;
	lastOutVal=height;
     }
  }
#undef buf
}

static double calc_ranges(meta_parameters *meta)
{
	int x;
	double slantFirst,slantPer;
	double er=meta_get_earth_radius(meta, meta->general->line_count/2, meta->general->sample_count/2);
	double satHt=meta_get_sat_height(meta, meta->general->line_count/2, meta->general->sample_count/2);
	double saved_ER=er;
	double er2her2,phi,phiAtSeaLevel,slantRng;
	int ns = meta->general->sample_count;

	meta_get_slants(meta,&slantFirst,&slantPer);
	slantFirst+=slantPer*meta->general->start_sample+1;
	slantPer*=meta->sar->sample_increment;
	er2her2=er*er-satHt*satHt;
	minPhi=acos((satHt*satHt+er*er-slantFirst*slantFirst)/(2.0*satHt*er));

/*Compute arrays indexed by slant range pixel:*/
	for (x=0;x<ns;x++)
	{
	/*Precompute slant range for SR pixel x.*/
		slantRange[x]=slantFirst+x*slantPer;
		slantRangeSqr[x]=slantRange[x]*slantRange[x];
	/*Compute incidence angle for SR pixel x.*/
		incidAng[x]=M_PI-acos((slantRangeSqr[x]+er2her2)/(2.0*er*slantRange[x]));
		sinIncidAng[x]=sin(incidAng[x]);
		cosIncidAng[x]=cos(incidAng[x]);
	}

	maxPhi=acos((satHt*satHt+er*er-slantRangeSqr[ns-1])/(2.0*satHt*er));
	phiMul=(ns-1)/(maxPhi-minPhi);

/*Compute arrays indexed by ground range pixel: slantGR and heightShiftGR*/
	for (x=0;x<ns;x++)
	{
		er=saved_ER;
		phiAtSeaLevel=grX2phi(x);
		slantRng=sqrt(satHt*satHt+er*er-2.0*satHt*er*cos(phiAtSeaLevel));
		slantGR[x]=(slantRng-slantFirst)/slantPer;
		er+=1000.0;
		phi=acos((satHt*satHt+er*er-slantRng*slantRng)/(2*satHt*er));

		heightShiftGR[x]=(phi2grX(phi)-x)/1000.0;
	}
/*Compute arrays indexed by slant range pixel: groundSR and heightShiftSR*/
	for (x=0;x<ns;x++)
	{
		er=saved_ER;
		phiAtSeaLevel=acos((satHt*satHt+er*er-slantRangeSqr[x])/(2*satHt*er));
		groundSR[x]=phi2grX(phiAtSeaLevel);
		er+=1000.0;
		slantRng=sqrt(satHt*satHt+er*er-2.0*satHt*er*cos(phiAtSeaLevel));

		heightShiftSR[x]=((slantRng-slantFirst)/slantPer-x)/1000.0;
	}
	er=saved_ER;
	return er/phiMul;
}

static void geo_compensate(float *grDEM, float *in, float *out,
                           int ns, int doInterp)
{
	int outX;
	for (outX=0;outX<ns;outX++)
	{
		double height=grDEM[outX];
		double inX=gr2sr(outX,height);
		
		if ((height!=badDEMht)&&(inX>=0)&&(inX<(ns-1)))
		{
			int x=floor(inX);
                        double dx=inX-x;
                        if (doInterp) {
                            /* bilinear interp */
                            out[outX]=(1-dx)*in[x]+dx*in[x+1];
                        } else {
                            /* nearest neighbor */
                            out[outX]= dx <= 0.5 ? in[x] : in[x+1];
                        }
		}
		else
			out[outX]=0.0;
	}
}

static void radio_compensate(float *grDEM, float *grDEMprev,float *inout,
			     int ns)
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

	static double *cosineScale=NULL;
	int x;
	if (cosineScale==NULL)
	{
		int i;
		double cosAng;
		cosineScale=(double *)MALLOC(sizeof(double)*512);
		for (i=0;i<512;i++)
		{
			cosAng=i/511.0;
			cosineScale[(int)(cosAng*511)]=1.00-0.70*pow(cosAng,7.0);
		}
	}
	for (x=1;x<ns;x++)
	{
		double dx,dy,grX,vecLen,cosAng;
	  /*Find terrain normal.*/
	  	grX=grDEM[x];
		if ((grX==badDEMht)||
		    (grDEMprev[x]==badDEMht)||
		    (grDEM[x-1]==badDEMht)) 
			{inout[x]=0;continue;}
		dx=(grX-grDEM[x-1])/grPixelSize;
		dy=(grDEMprev[x]-grX)/grPixelSize;
	  /*Make the normal a unit vector.*/
		vecLen=sqrt(dx*dx+dy*dy+1);
	  /*Take dot product of this vector and the incidence vector.*/
		cosAng=(dx*sinIncidAng[x]+cosIncidAng[x])/vecLen;
		if (cosAng>0)/* Ordinary diffuse radar reflection */
			inout[x]*=cosineScale[(int)(cosAng*511)];/*=cosScaled*255;*/
	}
}


/* inSarName can be NULL, in this case doRadiometric is ignored */
/* inMaskName can be NULL, in this case outMaskName is ignored */
int deskew_dem(char *inDemName, char *outName, char *inSarName,
	       int doRadiometric, char *inMaskName, char *outMaskName)
{
	float *srDEMline,*grDEM,*grDEMline,*grDEMlast,*inSarLine,*outLine;
	char *ext=NULL;
	FILE *inDemFp,*inSarFp,*outFp,*inMaskFp,*outMaskFp;
	meta_parameters *inDemMeta, *outMeta, *inSarMeta, *inMaskMeta;
	char msg[256];
	int inSarFlag=FALSE;
        int inMaskFlag=FALSE;
	int dem_is_ground_range=FALSE;
	register int x,y;

	inSarFlag = inSarName != NULL;
        inMaskFlag = inMaskName != NULL;

	inSarFp = NULL;
	inSarMeta = NULL;

        inMaskFp = NULL;
        outMaskFp = NULL;

	ext=findExt(inDemName);
	if (0==strcmp(ext,".dem"))
	   dem_is_ground_range=TRUE;

/*Extract metadata*/
	inDemMeta = meta_read(inDemName);
	outMeta = meta_read(inDemName);

	if (inDemMeta->sar->image_type=='P') {
		asfPrintError("DEM cannot be map projected for this program to work!\n");
		return FALSE;
	}
	if (inSarFlag) {
	   inSarMeta = meta_read(inSarName);
	   if (inSarMeta->sar->image_type=='P') {
	      asfPrintError("SAR image cannot be map projected for this program to work!\n");
	      return FALSE;
	   }
	   outMeta->general->data_type = inSarMeta->general->data_type;

	   if ((inSarMeta->general->line_count != inDemMeta->general->line_count) &&
	       (inSarMeta->general->sample_count != inDemMeta->general->sample_count))
	   {
	      asfPrintError("ERROR: The DEM and the SAR image must be the same size.\n");
	   }
	}
	numLines = inDemMeta->general->line_count;
	numSamples = inDemMeta->general->sample_count;
  	asfPrintStatus("Images are %i lines by %i samples.\n",
		       numLines,numSamples);

/*Allocate vectors.*/
	slantGR       = (double*)MALLOC(sizeof(double)*numSamples);
	groundSR      = (double*)MALLOC(sizeof(double)*numSamples);
	heightShiftSR = (double*)MALLOC(sizeof(double)*numSamples);
	heightShiftGR = (double*)MALLOC(sizeof(double)*numSamples);
	slantRange    = (double*)MALLOC(sizeof(double)*numSamples);
	slantRangeSqr = (double*)MALLOC(sizeof(double)*numSamples);
	incidAng      = (double*)MALLOC(sizeof(double)*numSamples);
	sinIncidAng   = (double*)MALLOC(sizeof(double)*numSamples);
	cosIncidAng   = (double*)MALLOC(sizeof(double)*numSamples);

/*Set up the output meta file.*/
	grPixelSize = calc_ranges(inDemMeta);
	outMeta->sar->image_type='G';
	outMeta->general->x_pixel_size = grPixelSize;
	meta_write(outMeta, outName);

/*Open files.*/
	inDemFp = fopenImage(inDemName,"rb");
	outFp   = fopenImage(outName,"wb");
	if (inSarFlag) inSarFp = fopenImage(inSarName,"rb");
        if (inMaskFlag) {
            if (!inSarFlag)
                asfPrintError("Cannot produce a mask without a SAR!\n");
            inMaskMeta = meta_read(inMaskName);
            if ((inSarMeta->general->line_count != inMaskMeta->general->line_count) &&
                (inSarMeta->general->sample_count != inMaskMeta->general->sample_count))
            {
                asfPrintError("ERROR: The mask and the SAR image must be the same size.\n");
            }
            inMaskFp = fopenImage(inMaskName,"rb");
            outMaskFp = fopenImage(outMaskName,"wb");
        }

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
	if (inSarFlag) 
	   inSarLine = (float *)MALLOC(sizeof(float)*numSamples);
	else
	   inSarLine = NULL;
	outLine   = (float *)MALLOC(sizeof(float)*numSamples);
	srDEMline = (float *)MALLOC(sizeof(float)*numSamples);

/*Map DEM to ground range if necessary.*/
	/*It's much simpler if the DEM is already in ground range.*/
	if (dem_is_ground_range)
		grDEM=(float *)MALLOC(sizeof(float)*numSamples);
	/*If the dem is slant range, then we need to map it to ground range,
	 *all at once-- we have to read it ALL in to interpolate the columns.*/
	else {
		grDEM=(float *)MALLOC(sizeof(float)*numSamples*numLines);
		for (y=0;y<numLines;y++)
		{
			get_float_line(inDemFp,inDemMeta,y,srDEMline);
			dem_sr2gr(srDEMline,&grDEM[y*numSamples],numSamples);
		}
		/*Close gaps in y direction.*/
		for (x=0;x<numSamples;x++)
			dem_interp_col(&grDEM[x],numSamples,numLines);
	}

/*Rectify data.*/
	for (y=0;y<numLines;y++) {
		if (inSarFlag) {
                    /*Read in DEM line-by-line (keeping two lines buffered)*/
                    if (dem_is_ground_range) {
                        float *tmp=srDEMline;
                        srDEMline=grDEM;
                        grDEM=tmp;
                        get_float_line(inDemFp,inDemMeta,y,grDEM);
                        grDEMline=grDEM;
                        grDEMlast=srDEMline;
                    }
                    /*Fetch the appropriate lines from the big buffer.*/
                    else {
                        grDEMline=&grDEM[y*numSamples];
                        grDEMlast=&grDEM[(y-1)*numSamples];
                    }
                    get_float_line(inSarFp,inSarMeta,y,inSarLine);
                    geo_compensate(grDEMline,inSarLine,outLine,numSamples,1);
                    if (y>0&&doRadiometric)
                        radio_compensate(grDEMline,grDEMlast,outLine,
                                         numSamples);
                    put_float_line(outFp,outMeta,y,outLine);

                    if (inMaskFp) {
                        get_float_line(inMaskFp,inMaskMeta,y,inSarLine);
                        geo_compensate(grDEMline,inSarLine,outLine,numSamples,0);
                        put_float_line(outMaskFp,outMeta,y,outLine);
                    }
		}
		else
                    put_float_line(outFp,outMeta,y,&grDEM[y*numSamples]);
	}

	asfPrintStatus("Wrote %lld bytes of data\n",
		       (long long)(numLines*numSamples*4));

/* Clean up & skidattle */
	if (inSarFlag) {
	   FREE(inSarLine);
	   FCLOSE(inSarFp);
	   meta_free(inSarMeta);
	}
        if (inMaskFlag) {
            FCLOSE(inMaskFp);
            FCLOSE(outMaskFp);
            meta_write(outMeta, outMaskName);
            meta_free(inMaskMeta);
        }
	FREE(srDEMline);
	FREE(outLine);
	FCLOSE(inDemFp);
	FCLOSE(outFp);
	meta_free(inDemMeta);
	meta_free(outMeta);
	FREE(slantGR);
	FREE(groundSR);
	FREE(heightShiftSR);
	FREE(heightShiftGR);
	FREE(slantRange);
	FREE(slantRangeSqr);
	FREE(incidAng);
	FREE(sinIncidAng);
	FREE(cosIncidAng);

	return TRUE;
}
