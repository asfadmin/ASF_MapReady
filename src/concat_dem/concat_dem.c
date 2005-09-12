/******************************************************************************
NAME: concat_dem -- lays several input DEMs on top of one another.

SYNOPSIS:  concat_dem <out img.ext> <in img1.ext> [ <in img2.ext> [...]]

                <out img.ext>  Name of LAS 6.0 image to be created.
                <in img...>  single-banded LAS images.

DESCRIPTION:  

	Concat_dem combines the input images to create an output image,
	respecting the geolocations of each image.  It will translate the 
	images so they line up with one another, but not rotate or flip them
	(for that, use the geocode(1) program).

        Where several input images overlap, concat_dem combines them
	by finding the trimmed average of their values, as described below.

        Unlike concat(1), concat_dem did not begin life as a LAS tool.
	Concat_dem works best with Digital Elevation Models (DEMs), while
	concat is aimed at SAR images.


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    6/98	O. Lawlor	Create concatenated DEMs for IGARSS.
    1.1	    5/99	O. Lawlor	Cleaned up for general use.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   concat_dem lays several input DEMs on top of one another.		    *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include "asf.h"
#include "worgen.h"
#include "ddr.h"
#include "dem.h"

const float VERSION=1.1;

int main(int argc,char *argv[])
{
#	define maxImgs 1000
/*Input files:*/
	float *inBuf[maxImgs],*inErr[maxImgs];
	dem *in[maxImgs];
/*Output files: output (heights) and byte (index of written pixel).*/
	float *outBuf;
	char *outfile;
	FILE *outF;
	struct DDR outDDR;
/*General Variables:*/
	int i,x,y,ns;
	extents outExt;
	double pStartX,pStartY,pDistX,pDistY;
	int nInputs,optInd=1;
	float background=0.0;/*Value of background pixels*/
	float invalidPix=0.0;/*Value of an invalid pixel*/
/*Statistics Variables*/
	/*long nOutliers=0,num=0;*/
	#define N_INV 1000
	double sum=0,sumSq=0,inv[N_INV];
	
/*Check CLA's.*/
	if (argc<3)
	{
		printf("\nUsage:\n"
		"\tconcat_dem <out img.ext> <in img1> [ <in img2> [...] ]\n"
		"\n\t\t<out img.ext>  LAS 6.0 image to be created.\n"
		"\t\t<in img...>    single-banded LAS images.\n\n"
		"concat_dem lays several input DEMs on top of one another.\n"
		"Inputs are LAS images, with or without extensions.\n"
		"\nVersion %.2f, ASF SAR TOOLS\n\n",VERSION);
		exit(1);
	}
	outfile=argv[optInd++];
	nInputs=argc-optInd;
/*Open input files, accumulating the extent they span.*/
	outExt=nullExtents();
	for (i=0;i<nInputs;i++)
	{
		in[i]=open_dem(argv[optInd+i]);
		/*if (i!=0)*/
			outExt=sumExtents(outExt,in[i]->height->ext);
	}

/*Create output file big enough for the entire set of input images*/
	outF=fopenImage(outfile,"wb");
	create_outDDR(outExt,&(in[0]->height->ddr),&pStartX,&pStartY,&pDistX,&pDistY,&outDDR);
	ns=outDDR.ns;
	c_putddr(outfile,&outDDR);

/*Create input buffers.*/
	for (i=0;i<N_INV;i++)
		inv[i]=1.0/i;
	inv[0]=0.0;
	outBuf=(float *)MALLOC(sizeof(float)*outDDR.ns);
	for (i=0;i<nInputs;i++)
	{
		inBuf[i]=(float *)MALLOC(sizeof(float)*outDDR.ns);
		inErr[i]=(float *)MALLOC(sizeof(float)*outDDR.ns);
	}
	
/*Do work: loop over output space, reading lines
from the input images and assembling them into the output (somehow).*/
	for (y=0;y<outDDR.nl;y++)
	{
		double line_sum=0,line_sumSq=0;
		/*Read each line in each source image*/
		for (i=0;i<nInputs;i++)
			getLineByProj(pStartX+pDistX*0,pStartY+pDistY*y,ns,
				in[i],inBuf[i],inErr[i]);
		
		for (x=0;x<ns;x++)
		{
		#define OVERLAY_METHOD 5
		#if OVERLAY_METHOD==0 /*Front-to-back overlay:*/
			outBuf[x]=0;
			for (i=0;i<nInputs;i++)
				if (inBuf[i][x]!=0.0)
					outBuf[x]=inBuf[i][x];
			
		#elif OVERLAY_METHOD==1 /*Average images.*/
			double sum=0.0;
			int num=0;
			for (i=0;i<nInputs;i++)
				if (inBuf[i][x]!=0.0)/*Bad DEM height*/
					{sum+=inBuf[i][x];num++;}
			outBuf[x]=sum*inv[num];
			byteBuf[x]=num;
			
		#elif OVERLAY_METHOD==2 /*Take image with least error.*/
			double leastErr=100000.0,leastVal=0;
			int leastInd=0;
			for (i=0;i<nInputs;i++)
				if (inBuf[i][x]!=0.0)
				 if (in[i]->err!=NULL)
				  if (inErr[i][x]<leastErr)
					{
						leastInd=i+1;
						leastVal=inBuf[i][x];
						leastErr=inErr[i][x];
					}
			outBuf[x]=leastVal;
			byteBuf[x]=leastInd;
			
		#elif OVERLAY_METHOD==3 /*Statistically average images:
		Take the "error" to represent a one-sigma deviation
		from a normal distribution.*/
			double weightSum=0.0,sum=0.0;
			int num=0;
			for (i=0;i<nInputs;i++)
				if (inBuf[i][x]!=0.0)
				{
					double weight=1.0/(3.0+inErr[i][x]);
					sum+=weight*inBuf[i][x];
					weightSum+=weight;
					num++;
				}
			if (num)
				outBuf[x]=sum/weightSum;
			else
				outBuf[x]=0.0;
			
			
		#elif OVERLAY_METHOD==4 /*Difference Image:*/
			if (inBuf[0][x]!=0 && inBuf[1][x]!=0)
			{
				double del=inBuf[1][x]-inBuf[0][x];
				if (del>200.0)
					{del=200.0;nOutliers++;}
				else if (del<-200.0)
					{del=-200.0;nOutliers++;}
				else
				{
					line_sum+=del;
					line_sumSq+=del*del;
					num++;
				}
				outBuf[x]=del;
			}
			else
				outBuf[x]=0;
			
		#elif OVERLAY_METHOD==5 /*Remove outliers & average images.*/
			double ave=0.0;
			int num=0;
			/*Find average value of input images*/
			for (i=0;i<nInputs;i++)
				if (inBuf[i][x]!=invalidPix)
				{
					ave+=inBuf[i][x];
					num++;
				}
			if (num)
			{
				/*Re-average, removing pixels far from the mean*/
				double ave_outliers=0.0;
				ave*=inv[num];
				num=0;/*Reset number of pixels.*/
				#define MAX_DIFF 60.0 /*Pixels further
				than this from the mean are called outliers and ignored.*/
				for (i=0;i<nInputs;i++)
					if ( (inBuf[i][x]!=invalidPix) &&
						((inBuf[i][x]-ave)<MAX_DIFF) &&
					     ((inBuf[i][x]-ave)>-MAX_DIFF) )
					{
						ave_outliers+=inBuf[i][x];
						num++;
					}
				outBuf[x]=ave_outliers*inv[num];
			}
			else
				outBuf[x]=background;
		#endif
		}
		
		putFloatLine(outF,&outDDR,y,outBuf);
		sum+=line_sum;
		sumSq+=line_sumSq;
		if (y%50==0)
			printf("Processing line %i\n",y);
	}
#if OVERLAY_METHOD==4
	printf("Stats:\n");
	printf("With %ld points, ignoring %ld outliers,\n"
		"Ave_Diff= %f RMS_Diff= %f\n",
		num,nOutliers,
		(double)(sum/num),
		(double)sqrt((sumSq-sum/num)/num));
#endif

	return 0;
}
