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
#include "asf_vector.h"

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
static float *createSpeckle(void)
{
	int i;
#define speckleLen 0x0fff
#define getSpeckle speckle[specklePtr=((specklePtr+7)&speckleLen)]
	float *speckle=(float *)MALLOC(sizeof(float)*(speckleLen+1));
	for (i=0;i<=speckleLen;i++)
            speckle[i]=gaussRand(4);
	return speckle;
}

static int eq(double a, double b, double tol) {
  return fabs(a-b)<tol;
}

/*dem_gr2sr: map one line of a ground range DEM into
one line of a slant range DEM && one line of simulated amplitude image.*/
static void dem_gr2sr(float *grDEM, float *srDEM, float *amp, float *inMask)
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
        speckle=createSpeckle();

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
                         eq(srDEM[ind],badDEMht,.0001))) {
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
            /* &&
               srDEM[x-1]!=badDEMht &&
               srDEM[x+1]!=badDEMht)*/
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

int reskew_dem(char *inMetafile, char *inDEMfile, char *outDEMfile,
               char *outAmpFile, char *inMaskFile)
{
	float *grDEMline,*srDEMline,*outAmpLine,*inMaskLine;
	register int line,nl;
	FILE *inDEM,*outDEM,*outAmp,*inMask=NULL;
	meta_parameters *metaIn, *metaDEM, *metaInMask=NULL;

        //printf("%s -> %s\n", inDEMfile, outDEMfile);
/* Get metadata */
	metaIn = meta_read(inMetafile);
	metaDEM = meta_read(inDEMfile);
	nl = metaDEM->general->line_count;
	gr_ns = metaDEM->general->sample_count;
	sr_ns = metaIn->general->sample_count;
	earth_radius = meta_get_earth_radius(metaIn, nl/2, 0);
	satHt = meta_get_sat_height(metaIn, nl/2, 0);
	meta_get_slants(metaIn, &slant_to_first, &slant_per);

/*Open files.*/
	inDEM  = fopenImage(inDEMfile,"rb");
	outDEM = fopenImage(outDEMfile,"wb");
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
            int i;
            float val = unmasked_value();
            for (i=0; i<gr_ns; i++)
                inMaskLine[i] = val;
        }
					
/*Allocate more memory (this time for data lines*/
	grDEMline  = (float *)MALLOC(sizeof(float)*gr_ns);
	srDEMline  = (float *)MALLOC(sizeof(float)*sr_ns);
	outAmpLine = (float *)MALLOC(sizeof(float)*sr_ns);

/* Read deskewed data, write out reskewed data */
	for (line=0; line<nl; line++)
	{
            get_float_line(inDEM,metaDEM,line,grDEMline);
            if (inMaskFile)
                get_float_line(inMask,metaInMask,line,inMaskLine);

            dem_gr2sr(grDEMline,srDEMline,outAmpLine,inMaskLine);

            put_float_line(outDEM,metaIn,line,srDEMline);
            put_float_line(outAmp,metaIn,line,outAmpLine);	
	}

/* Write meta files */
        // we only put in one band into the DEM & Sim Amp
        metaIn->general->band_count = 1; 
        strcpy(metaIn->general->bands, "");

	meta_write(metaIn, outDEMfile);
	meta_write(metaIn, outAmpFile);

/* Free memory, close files, & exit */
	meta_free(metaDEM);
	meta_free(metaIn);
	FREE(grDEMline);
	FREE(srDEMline);
	FREE(outAmpLine);
	FREE(inMaskLine);
	FCLOSE(inDEM);
	FCLOSE(outDEM);
	FCLOSE(outAmp);
	if (inMaskFile)
	{
            FCLOSE(inMask);
            meta_free(metaInMask);
	}
	return TRUE;
}
