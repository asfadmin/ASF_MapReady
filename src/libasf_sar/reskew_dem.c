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

static float badDEMht=0.0;
static float unInitDEM=-1.0;
static int maxBreakLen=20;


static int n_lay;
static int n_shad;
static int n_usermask;
static int n_invalid;

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

/*dem_gr2sr: map one line of a ground range DEM into
one line of a slant range DEM && one line of simulated amplitude image.*/
static void dem_gr2sr(float *grDEM, float *srDEM,float *amp,float *outmask,float *inMask)
{
    int x,grX;
    double lastSrX=-1;/*Slant range pixels up to (and including) here 
                        have been filled.*/
    int OsriX;
    int iX;
    float lastOutValue=badDEMht;

    static float *speckle=NULL;
    int specklePtr=rand();
    if (speckle==NULL)
        speckle=createSpeckle();
	
/*Initialize amplitude to zero, and DEM to -1.*/
    for (x=0;x<sr_ns;x++)
    {
        amp[x]=0;
        srDEM[x]=unInitDEM;
	outmask[x]=MASK_NORMAL;
    }
   
/*Step through the ground range line using grX.
Convert each grX to an srX.  Update amplitude and height images.*/
    for (grX=1;grX<gr_ns;grX++)
    {
        double height=grDEM[grX];
	/*srX: float slant range pixel position.*/
        double srX=srE2srH(grX,height);
        int sriX=(int)srX;
	if ((inMask[grX] != 0 ) && (srX>=0)&&(srX<sr_ns))
	 	{
			//	height = badDEMht; // simple check for mask
	 		++n_usermask;
	 		for (iX=OsriX; iX <= sriX; iX++)
				 		outmask[iX] = MASK_USER_MASK;
			OsriX = sriX;
		 }
			
	if   ((height!=badDEMht)&&(srX>=0)&&(srX<sr_ns)) 
	{
            double runLen=srX-lastSrX;
            int intRun=(int)runLen;
            if ((runLen<maxBreakLen)&&(lastOutValue!=badDEMht))
            {
                double currAmp;
                /*Update the amplitude image.*/
                if (runLen<0) 
                    runLen=-runLen;
                currAmp=50.0/(runLen*runLen*5+0.1);
                for (x=lastSrX+1;x<=sriX;x++)
			if (outmask[x]!=MASK_USER_MASK)
		            	amp[x]+=currAmp*getSpeckle;
		
                /*Then, update the height and mask images.*/
                if (intRun!=0)
                {
                    float curr=lastOutValue;
                    float delt=(height-lastOutValue)/intRun;
                    float maxval=height>lastOutValue ? height : lastOutValue;
                    curr+=delt;
                    for (x=lastSrX+1;x<=sriX;x++)
                    {
                        if (srDEM[x]==unInitDEM) {
                            /*Only write on fresh pixels.*/
                            srDEM[x]=maxval;
			   // mask[x]=MASK_NORMAL;
                            //srDEM[x]=curr;
                        } else {
                            /*Hit this pixel a 2nd time ==> layover*/
				if (maxval > srDEM[x]) 
					{
					srDEM[x] = maxval;
					}
					
					if (outmask[x]==MASK_NORMAL)	
			    			outmask[x] = MASK_LAYOVER;
			    
			    //    if(presentationMode == 1) srDEM[x]+=curr*getSpeckle; 
					
                            ++n_lay;
                        }
                        curr+=delt;
                    }
                    /* jumping backwards => shadow */
                    if (sriX < lastSrX-1) {
                        for (x=sriX;x<lastSrX;++x) {
				if (outmask[x]==MASK_NORMAL)
						outmask[x] = MASK_SHADOW;
                            ++n_shad;
                        }
                    }
                } else {
                    if (srDEM[(int)lastSrX+1]==unInitDEM) {
                        srDEM[(int)lastSrX+1]=height;
                    }
                }
            } else {
                for (x=lastSrX+1;x<=sriX;x++) {
                    srDEM[x]=badDEMht;
		    if (outmask[x]==MASK_NORMAL)
                    		outmask[x]=MASK_INVALID_DATA;
		    
                }
            }
            lastOutValue=height;
            lastSrX=srX;
        }
    }
/*Fill to end of line with zeros.*/
    for (x=lastSrX+1;x<sr_ns;x++)
    	{
        srDEM[x]=badDEMht;
	if (outmask[x]==MASK_NORMAL)
			outmask[x]=MASK_INVALID_DATA;
	}
/* Just plug all the holes and see what happens */
/*Attempt to plug one-pixel holes, by interpolating over them.*/
    for (x=1;x<(sr_ns-2);x++)
    {
        if (srDEM[x]==badDEMht)
            /* &&
               srDEM[x-1]!=badDEMht &&
               srDEM[x+1]!=badDEMht)*/
            srDEM[x]=(srDEM[x-1]+srDEM[x+1])/2;

        if (outmask[x-1] == outmask[x+1])
            outmask[x] = outmask[x-1];
    }
}

/*
Diffuse (lambertian) reflection:
	reflPower=cosIncidAng[sriX]+
		sinIncidAng[sriX]*(grDEM[grX]-grDEM[grX-1])/grPixelSize;
   	if (reflPower<0) reflPower=0;Radar Shadow.
   	currAmp=reflPower/runLen;
*/

int reskew_dem(char *inMetafile, char *inDEMfile, char *outDEMfile,
	       char *outAmpFile,  char *outMaskFile, char *inMaskFile)
{
	float *grDEMline,*srDEMline,*outAmpLine,*outMaskLine;
	float *inMask, *inMaskLine;
	register int line,nl;
	FILE *inDEM,*outDEM,*outAmp,*outMask;
	meta_parameters *metaIn, *metaDEM, *metaInMask;;

/* Get metadata */
	metaIn = meta_read(inMetafile);
	metaDEM = meta_read(inDEMfile);
	nl = metaDEM->general->line_count;
	gr_ns = metaDEM->general->sample_count;
	sr_ns = metaIn->general->sample_count;
	earth_radius = meta_get_earth_radius(metaIn, nl/2, 0);
	satHt = meta_get_sat_height(metaIn, nl/2, 0);
	meta_get_slants(metaIn, &slant_to_first, &slant_per);

        n_lay = n_shad = 0;
	n_usermask =0;
	n_invalid = 0;
	
/*Open files.*/
	inDEM  = fopenImage(inDEMfile,"rb");
	outDEM = fopenImage(outDEMfile,"wb");
	outAmp = fopenImage(outAmpFile,"wb");
	
        if (outMaskFile)
            	{
		 		outMask = fopenImage(outMaskFile,"wb");
		 		asfPrintStatus(" Opened  Mask file for output %s \n",outMaskFile);
		}
	int i;
	long j=0;
	inMaskLine = (float *)MALLOC(sizeof(float)*gr_ns);
	if (inMaskFile)
		{
			// read_mask(inMaskFile,&inMask,&metaInMask); for byte masks
			metaInMask = meta_read(inMaskFile);
			asfPrintStatus(" Read in User Maskfile: %s   %d x %d lines/samples  needed %d x %d \n\n",
			inMaskFile, metaInMask->general->line_count,metaInMask->general->sample_count,nl,gr_ns);
			inMask = fopenImage(inMaskFile,"rb");
		}
		else
		{
		for (i=0; i<gr_ns; i++)
				inMaskLine[i] = 0; // put our mask pointer into right place
		}
			// make a blank mask
					
/*Allocate more memory (this time for data lines*/
	grDEMline  = (float *)MALLOC(sizeof(float)*gr_ns);
	srDEMline  = (float *)MALLOC(sizeof(float)*sr_ns);
	outAmpLine = (float *)MALLOC(sizeof(float)*sr_ns);
	outMaskLine= (float *)MALLOC(sizeof(float)*sr_ns);

/* Read deskewed data, write out reskewed data */
	for (line=0; line<nl; line++)
	{
		get_float_line(inDEM,metaDEM,line,grDEMline);
		if (inMaskFile)
			get_float_line(inMask,metaInMask,line,inMaskLine);
		
		dem_gr2sr(grDEMline,srDEMline,outAmpLine,outMaskLine,inMaskLine);
		put_float_line(outDEM,metaIn,line,srDEMline);
		put_float_line(outAmp,metaIn,line,outAmpLine);
                if (outMaskFile)
                    put_float_line(outMask,metaIn,line,outMaskLine);
	
	}

        int total_pixels = nl * sr_ns;
	asfPrintStatus("Approximate Mask Statistics \n");
        asfPrintStatus("Layover pixels: %7d/%d (%f%%)\n",
                       n_lay, total_pixels, 100*(float)n_lay/total_pixels);
        asfPrintStatus(" Shadow pixels: %7d/%d (%f%%)\n",
                       n_shad, total_pixels, 100*(float)n_shad/total_pixels);
	asfPrintStatus("  Invalid Data pixels : %7d/%d (%f%%)\n",
		      n_invalid, total_pixels, 100*(float)n_invalid/total_pixels);
	asfPrintStatus("   User Masked pixels : %7d/%d (%f%%)\n",
		      n_usermask, total_pixels, 100*(float)n_usermask/total_pixels);

/* Write meta files */
	meta_write(metaIn, outDEMfile);
	meta_write(metaIn, outAmpFile);
        if (outMaskFile)
            meta_write(metaIn, outMaskFile);

/* Free memory, close files, & exit */
	meta_free(metaDEM);
	meta_free(metaIn);
	FREE(grDEMline);
	FREE(srDEMline);
	FREE(outAmpLine);
        FREE(outMaskLine);
	FREE(inMaskLine);
	FCLOSE(inDEM);
	FCLOSE(outDEM);
	FCLOSE(outAmp);
        if (outMaskFile)
            FCLOSE(outMask);
	if (inMaskFile)
	{
		FCLOSE(inMask);
		meta_free(metaInMask);
		//FREE(inMaskLine);
	}
	return TRUE;
}
