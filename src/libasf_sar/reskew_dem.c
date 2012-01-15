/****************************************************************
NAME:  reskew_dem

SYNOPSIS:  reskew_dem [-log <file>] <inGR_DEMfile> <outSR_DEM> <outSR_sim_amp>

DESCRIPTION:
	Reskew_dem maps an input, ground range DEM into slant range, and
	creates a simulated SAR image.  The input DEM must already be lined up
	with the image, but need not be precisely co-registered. In fact, the
	amplitude image is generated only so the images can be co-registered.

	This program is called by the dem2seeds script.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0      8/97        O. Lawlor   Reskew USGS DEMs for Interferometry/
    1.1      6/97        O. Lawlor   Made more consistent with deskew_dem.
    1.3     12/98        O. Lawlor   Allow ground and slant ranges to differ in
                                      length.
    1.31     7/01        R. Gens     Added logfile switch
    1.5     12/03        P. Denny    Update commandline parsing. Use meta 1.1
                                      instead of DDRs. This program is loaded
                                      with unnecessary globals. Yuk! Needs to
                                      be fixed sometime.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   reskew_dem -- this program remaps the input DEM to slant range,         *
*		  and creates a simulated slant-range amplitude image 	    *
*		  from it.						    *
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
#include <assert.h>

/* current earth radius, meters (FIXME: update with azimuth, range) */
static double earth_radius;

/* satellite height from center of earth, meters */
static double satHt; 

/* slant ranges, meters (FIXME: split out DEM and SAR slant ranges) */
static double slant_to_first, slant_per; 

static int gr_ns,sr_ns;

static float badDEMht=BAD_DEM_HEIGHT;
static float unInitDEM=-1.0;
static int maxBreakLen=2000;

static float srE2srH(float srEpix,float height)
{
	double er;
	double srE;
	double cosPhi;
	double srH;
	er = earth_radius;
	srE=slant_to_first+slant_per*srEpix;
	/* Calculate ground angle to this slant range */
	cosPhi=(satHt*satHt + er*er - srE*srE)/(2*satHt*er);
	/* Calculate slant range with new earth height */
	er+=height;
	srH=sqrt(satHt*satHt + er*er - 2 * satHt * er * cosPhi);
	return (srH-slant_to_first)/slant_per;
}

static float unitRand(void)
{
	return (float)(0x0ffff&rand())/(0x0ffff);
}

static float gaussRand(int num)
{
	int i;
	float ret=0;
	for (i=0;i<num;i++)
            ret+=unitRand();
	return 2*ret/num;
}

/*Note: it may seem stupid to add speckle to our simulated
SAR images, but the speckle has an important statistical 
contribution to the image.*/
static float *createSpeckle(int add_speckle)
{
	int i;
#define speckleLen 0x0fff
#define getSpeckle speckle[specklePtr=((specklePtr+7)&speckleLen)]
	float *speckle=(float *)MALLOC(sizeof(float)*(speckleLen+1));
        for (i=0;i<=speckleLen;i++)
          speckle[i]=add_speckle ? gaussRand(4) : 1;
        
	return speckle;
}

static int eq(double a, double b, double tol) {
  return fabs(a-b)<tol;
}

/*dem_gr2sr: map one line of a ground range DEM into
one line of a slant range DEM && one line of simulated amplitude image.*/
static void dem_gr2sr(float *grDEM, float *srDEM, float *amp, float *inMask,
                      int add_speckle)
{
    int x,grX;
    double lastSrX=-1;/*Slant range pixels up to (and including) here 
                        have been filled.*/
    int OsriX=0;
    int iX;
    float lastOutValue=badDEMht;
    int *outmask;

    static float *speckle=NULL;
    int specklePtr=rand();
    if (speckle==NULL)
        speckle=createSpeckle(add_speckle);

    outmask=MALLOC(sizeof(int)*sr_ns);

/*Initialize amplitude to zero, and DEM to -1.*/
    for (x=0;x<sr_ns;x++)
    {
        amp[x]=0;
        srDEM[x]=unInitDEM;
	outmask[x]=MASK_NORMAL;
    }

/*Step through the ground range line using grX.
Convert each grX to an srX.  Update amplitude and height images.*/
    for (grX=0;grX<gr_ns;grX++)
    {
        double height=grDEM[grX];

        // This little kludge prevents DEMs with lots of zeros (badDEMht is
        // currently 0) from blanking out big parts of the sar image.  Ideally
        // we would change badDEMht to something that could never occur in
        // a real DEM, however there is a problem with that (see asf_sar.h)
        if (height == badDEMht) height = badDEMht + 0.01;

        // SRTM DEMs use inconsistent values for "no data here" -- generally
        // large negative values.  This maps all those to our "no data" value
        if (height < -900) height = badDEMht;

	/*srX: float slant range pixel position.*/
        double srX=srE2srH(grX,height); 
        int sriX=(int)srX;
        if (is_masked(inMask[grX]) && srX>=0 && srX<sr_ns)
        {
            //	height = badDEMht; // simple check for mask
            for (iX=OsriX; iX<=sriX; ++iX)
                outmask[iX] = MASK_USER_MASK;
            OsriX = sriX;
        }
        
	if ((height!=badDEMht)&&(srX>=0)&&(srX<sr_ns)) 
	{
            double runLen=srX-lastSrX;
            int intRun=(int)runLen;

            if (eq(lastOutValue,NO_DEM_DATA,.0001) ||
                eq(height,NO_DEM_DATA,.0001))
            {
              for (x=lastSrX+1;x<=sriX;x++) {
                srDEM[x]=NO_DEM_DATA;
              }
            }
            else if ((runLen<maxBreakLen)&&(lastOutValue!=badDEMht))
            {
                double currAmp;
                /*Update the amplitude image.*/
                if (runLen<0) 
                    runLen=-runLen;
                currAmp=50.0/(runLen*runLen*5+0.1);
                for (x=lastSrX+1;x<=sriX;x++)
                    //if (outmask[x]!=MASK_USER_MASK)
                        amp[x]+=currAmp*getSpeckle;

                /*Then, update the height image.*/
                if (intRun!=0)
                {
                    float maxval=height>lastOutValue ? height : lastOutValue;
                    for (x=lastSrX+1;x<=sriX;x++)
                    {
                        if (srDEM[x]==unInitDEM || maxval > srDEM[x]) {
                            srDEM[x]=maxval;
                        }
                    }
                } else {
                    int ind = (int)lastSrX+1;
                    if (ind < sr_ns &&
                        (eq(srDEM[ind],unInitDEM,.0001) ||
                         eq(srDEM[ind],NO_DEM_DATA,.0001) ||
                         eq(srDEM[ind],badDEMht,.0001)))
                    {
                        srDEM[ind]=height;
                    }
                }
            } else {
                for (x=lastSrX+1;x<=sriX;x++) {                  
                    srDEM[x]=badDEMht;
                }
            }
            lastOutValue=height;
            lastSrX=srX;
        }
    }

/* Just plug all the holes and see what happens */
/* First, handle the left edge "holes" */
    if (srDEM[0] == badDEMht) {
        for (x=1; x<5; ++x) { // 5 because we don't want to get carried away
            if (srDEM[x] != badDEMht) {
                for (iX=x-1; iX>=0; --iX)
                    srDEM[iX] = srDEM[x];
                break;
            }
        }
    }

/* Right edge "holes" -- must also check for "unInitDEM" on this edge */
    if (srDEM[sr_ns-1] == badDEMht || srDEM[sr_ns-1] == unInitDEM) {
        for (x=sr_ns-2; x>sr_ns-6; --x) {
            if (srDEM[x] != badDEMht && srDEM[x] != unInitDEM) {
                for (iX=x+1; iX<sr_ns; ++iX)
                    srDEM[iX] = srDEM[x];
                break;
            }
        }
    }

/* Attempt to plug one-pixel holes, by interpolating over them. */
    for (x=1;x<(sr_ns-2);x++)
    {
        if (srDEM[x]==badDEMht)
            srDEM[x]=(srDEM[x-1]+srDEM[x+1])/2;

        if (srDEM[x]==unInitDEM)
            srDEM[x]=badDEMht;
    }

    FREE(outmask);
}

/*
Diffuse (lambertian) reflection:
	reflPower=cosIncidAng[sriX]+
		sinIncidAng[sriX]*(grDEM[grX]-grDEM[grX-1])/grPixelSize;
   	if (reflPower<0) reflPower=0;Radar Shadow.
   	currAmp=reflPower/runLen;
*/
/*
static const char *radiometry_toString(radiometry_t rad)
{
  switch (rad)
  {
    case r_AMP:
      return "Amplitude";

    case r_POWER:
      return "Power";

    case r_SIGMA:
      return "Sigma";

    case r_SIGMA_DB:
      return "Sigma (db)";

    case r_BETA:
      return "Beta";

    case r_BETA_DB:
      return "Beta (db)";

    case r_GAMMA:
      return "Gamma";

    case r_GAMMA_DB:
      return "Gamma (db)";

    default:
      return MAGIC_UNSET_STRING;
  }
}

static void handle_radiometry(meta_parameters *meta, float *amp, int n,
                              int line, radiometry_t rad)
{
  // converts the given amplitude data to the specified radiometry, in place
  int i;
  double incid,sigma;

  switch (rad)
  {
    case r_AMP:
      // we already have amplitude, don't need to do anything
      return;

    case r_POWER:
      for (i=0; i<n; ++i)
        amp[i] = amp[i]*amp[i];
      return;

    case r_SIGMA:
    case r_SIGMA_DB:
      for (i=0; i<n; ++i)
        amp[i] = amp[i]*amp[i];
      break;

    case r_BETA:
    case r_BETA_DB:
      for (i=0; i<n; ++i) {
        incid = meta_incid(meta, line, i);
        sigma = amp[i]*amp[i];
        amp[i] = sigma/sin(incid);
      }
      break;

    case r_GAMMA:
    case r_GAMMA_DB:
      for (i=0; i<n; ++i) {
        incid = meta_incid(meta, line, i);
        sigma = amp[i]*amp[i];
        amp[i] = sigma/cos(incid);
      }
      break;

    default:
      printf("Invalid radiometry: %d\n", rad);
      assert(0);
      break;
  }

  // calculate db if necessary
  if (rad==r_SIGMA_DB || rad==r_BETA_DB || rad==r_GAMMA_DB) {
    for (i=0; i<n; ++i) {
      if (amp[i] > .001)
        amp[i] = 10. * log10(amp[i]);
      else
        amp[i] = -30.;
    }
  }
}
*/
int reskew_dem(char *inMetafile, char *inDEMfile, char *outDEMfile,
               char *outAmpFile, char *inMaskFile, int add_speckle)
{
  return reskew_dem_rad(inMetafile, inDEMfile, outDEMfile, NULL, outAmpFile,
                        inMaskFile, r_AMP, add_speckle);
}

int reskew_dem_rad(char *inMetafile, char *inDEMfile, char *outDEMslant,
                   char *outDEMground, char *outAmpFile, char *inMaskFile,
                   radiometry_t rad, int add_speckle)
{
	float *grDEMline,*srDEMline,*outAmpLine,*inMaskLine;
	register int line,nl;
	FILE *inDEM,*outDEMsr,*outDEMgr,*outAmp,*inMask=NULL;
	meta_parameters *metaIn, *metaDEM, *metaInMask=NULL, *metaGR=NULL;

/* Get metadata */
	metaIn = meta_read(inMetafile);
        if (outDEMground)
          metaGR = meta_read(inMetafile);
	metaDEM = meta_read(inDEMfile);
	nl = metaDEM->general->line_count;
	gr_ns = metaDEM->general->sample_count;
	sr_ns = metaIn->general->sample_count;
	earth_radius = meta_get_earth_radius(metaIn, nl/2, 0);
	satHt = meta_get_sat_height(metaIn, nl/2, 0);
	meta_get_slants(metaIn, &slant_to_first, &slant_per);

/*Update ground range metadata*/
        if (outDEMground) {
          metaGR->sar->image_type = 'G';
          metaGR->general->sample_count = gr_ns;
          metaGR->general->line_count = nl;
          double grPixSize = metaIn->general->y_pixel_size;
          metaGR->general->x_pixel_size = grPixSize;
          metaGR->general->y_pixel_size = grPixSize;
          metaGR->general->start_line = metaGR->general->start_sample = 0;
          metaGR->general->band_count = 1;
	  metaGR->general->image_data_type = DEM;
          strcpy(metaGR->general->bands, "");
        }

/*Open files.*/
	inDEM  = fopenImage(inDEMfile,"rb");
	outDEMsr = fopenImage(outDEMslant,"wb");
        if (outDEMground)
          outDEMgr = fopenImage(outDEMground,"wb");
	outAmp = fopenImage(outAmpFile,"wb");
	
	inMaskLine = (float *)MALLOC(sizeof(float)*gr_ns);
	if (inMaskFile)
        {
            // read_mask(inMaskFile,&inMask,&metaInMask); for byte masks
            metaInMask = meta_read(inMaskFile);
            asfPrintStatus("Read in User Maskfile: %s "
                           "%dx%d LxS (needed %dx%d).\n",
                           inMaskFile, metaInMask->general->line_count,
                           metaInMask->general->sample_count,nl,gr_ns);
            inMask = fopenImage(inMaskFile,"rb");
        }
        else
        {
            // make a blank mask
            float val = unmasked_value();
            for (line=0; line<gr_ns; ++line)
                inMaskLine[line] = val;
        }
					
/*Allocate more memory (this time for data lines*/
	grDEMline  = (float *)MALLOC(sizeof(float)*gr_ns);
	srDEMline  = (float *)MALLOC(sizeof(float)*sr_ns);
	outAmpLine = (float *)MALLOC(sizeof(float)*sr_ns);

/* Read deskewed data, write out reskewed data */
	for (line=0; line<nl; ++line)
	{
            get_float_line(inDEM,metaDEM,line,grDEMline);
            if (inMaskFile)
                get_float_line(inMask,metaInMask,line,inMaskLine);

            dem_gr2sr(grDEMline,srDEMline,outAmpLine,inMaskLine,add_speckle);

            // write out slant/ground range DEM lines
            put_float_line(outDEMsr,metaIn,line,srDEMline);
            if (outDEMground)
              put_float_line(outDEMgr,metaGR,line,grDEMline);

            // convert amplitude data to desired radiometry, then
            // write out the simulated sar image line
            put_float_line(outAmp,metaIn,line,outAmpLine);	
	}

/* Write meta files */
        // we only put in one band into the DEM & Sim Amp
        metaIn->general->band_count = 1; 
        strcpy(metaIn->general->bands, "");

	meta_write(metaIn, outDEMslant);
	metaIn->general->image_data_type = SIMULATED_IMAGE;
	meta_write(metaIn, outAmpFile);

/* Free memory, close files, & exit */
	meta_free(metaDEM);
	meta_free(metaIn);

	FREE(grDEMline);
	FREE(srDEMline);
	FREE(outAmpLine);
	FREE(inMaskLine);
	FCLOSE(inDEM);
	FCLOSE(outDEMsr);
	FCLOSE(outAmp);
        if (outDEMground) {
          FCLOSE(outDEMgr);
          meta_write(metaGR, outDEMground);
          meta_free(metaGR);
        }
	if (inMaskFile) {
            FCLOSE(inMask);
            meta_free(metaInMask);
	}
	return TRUE;
}
