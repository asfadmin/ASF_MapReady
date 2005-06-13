/******************************************************************************
NAME:  remap

SYNOPSIS: 

    remap <infile> <outfile>
          [-rotate <deg>][-scale <sX> <sY>][-translate <tX> <tY>]
          [-matrix <matrixFile>][-ppf <ppfFile>][-deltas <deltaFile>]
                     [-quadratic <quadFile>][-warp <warpImages>]
          [-nearest|-bilinear|-sinc
                     |-kernel <sizeX> <sizeY>|-fileKernel <kernelFile>]
          [-background <fill>]
          [-byte [-map <min> <max>] |-char|-short|-int|-float|-double]
          [-width <width>][-height <height>][-sameSize][-asDDR <ddr>]
          [-log <file>]

GENERAL DESCRIPTION:

	Remap works with LAS 6.0 images.  Pass it two filenames.
A DDR must exist for the input image, and a DDR will be created for the output.

	Remap will perform a remapping and a resampling of the input image to the output
image.  Remapping changes the LOCATION of pixels (e.g. translation); resampling
changes the VALUE of pixels (e.g. a 3x3 kernel).  Remap will only work with
one-band (i.e. greyscale) byte, short, long, or float LAS 6.0 images.

	The other command line options can be entered in any order.  Although 
case is significant, the unique start of each option is always sufficient 
to identify it (e.g. "-rot 45" instead of "-rotation 45").  Specifing no 
parameters (other than the file names) will result an output image
which is in every way identical to the first.

	For details on the command line parameters, see the man page.
	
EXTERNAL ASSOCIATES:
	process_CLAs in CLA.c,
	perform_mapping in mapping.c,
	various Matrix2D utility routines in Matrix2D.c,
	and LAS DDR/BDDR I/O routines.

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
        VERS:   DATE:   AUTHOR:         PURPOSE:
    ---------------------------------------------------------------
	0.5	5/21/97	Orion Lawlor	Initial Development- needed
					  system to remap ISAR float/complex data.
	0.7	5/22/97	Orion Lawlor	Make project bigger+more general
					  (Probably sheer masochism).  It now
					  applies a
					  general 2x3 matrix tranform to char,
					  short, long,
					  and float data of any size.
	1.0	5/30/97	Orion Lawlor	Is now actually useful.  Maintains DDR
					  correctly.
					 Many bugs lost their buggy lives.
					 Includes #option to do forward and
					  reverse FFT.
	1.2	6/19/97 Orion Lawlor	Works on .cpx data, component -by-
					  component.
	2.0     5/21/98 Orion Lawlor    Put mapping and sampling functions into
					  their own files.
	2.1     3/14/99 Orion Lawlor    Added -quadratic option and -asDDR
					  options.
	2.11    7/16/01 Rudi Gens	Added logfile switch
	2.3     3/02    P. Denny        Updated Command line parsing


HARDWARE/SOFTWARE LIMITATIONS: none

ALGORITHM DESCRIPTION:

	This program, in an attempt to be general, uses a "Mapping Function,"
"Sampling Function," and data (as void *'s) for each.  The mapping function defines
the spatial transformation between output image space and input image space, and
is designed to not necessarily be linear.  The sampling function defines how 
pixels from the input image are changed into pixels in the output image.

	Each mapping or sampling function is responsible for allocating
and maintaining its own data in mapData and sampData.  Since there can
only be one mapping function and one sampling function, any function
has unfettered rights to this variable, which is carried along between
each function.

	There are currently two supported mapping functions- a 2x3 matrix (matrixMap),
and a quadratic 2D polynomial (quadraticMap).
To add others, you should add your mapping function to the mappingFunction
enumerated type in remap.h, then change calc_outDDR, process_CLAs, and
perform_mapping to handle the new mapping.  But you probably won't need to
do this, because a matrix tranformation is quite general.  It handles
any combination of translation, scaling, rotation, and shearing.

	It's probably more likely that you'll need to either add support
for another sampling function.  The currently defined sampling functions
are nearest neighbor (nearestSamp), bilinear interpolation (bilinearSamp),
and uniform or nonuniform image convolution kernels (kernelSamp).  To add
these, change the enum in remap.h, then add your parameters in process_CLAs,
and finally change perform_mapping.

BUGS: 
	none (but could be faster)  
	Also, remap is a large program, and may take while to learn.
	But I've found it quite useful in many graphics-related tasks,
	and ASF currently uses it for their complex interferometry,
	and soon will use it for DEM geocoding.  I hope you enjoy it.


******************************************************************************/
/****************************************************************************
*								            *
*   Remap will perform a remapping and a resampling of the input image to   *
*	  the output image. 						    *
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
#include "las.h"
#include "Matrix2D.h"
#include "remap.h"

float VERSION = 2.3;

float minFlt=-1, maxFlt=-1,backgroundFill=0.0;

/* Prototypes */
void grow(fPoint *min,fPoint *max,fPoint addThis);
fPoint makePoint(float x,float y);
void calc_outDDR(struct DDR *inDDR, mappingFunction map, struct DDR *outDDR);
float getProjection(float x,float y,struct DDR *ddr,int val);
void update_projection(struct DDR *inDDR, mappingFunction map, struct DDR *outDDR);


/*grow: Increases the size of an axis-aligned box defined by two points min and max.*/
void grow(fPoint *min,fPoint *max,fPoint addThis)
{
	if (addThis.x<min->x)
		min->x=addThis.x;
	if (addThis.x>max->x)
		max->x=addThis.x;
	if (addThis.y<min->y)
		min->y=addThis.y;
	if (addThis.y>max->y)
		max->y=addThis.y;
}
fPoint makePoint(float x,float y)
{
	fPoint ret;
	ret.x=x;
	ret.y=y;
	return ret;
}
/*Compute the values for the output DDR, based on the input DDR.*/
void calc_outDDR(struct DDR *inDDR, mappingFunction map, struct DDR *outDDR)
{
/*Depending on the mapping function, we'll calculate the output extents differently.
  In general, we're going to make the output image as big as it needs to be to hold
  the positive X and Y part of the image, BUT we'll truncate the image if it crosses
  the X or Y axis into negative coordinates.*/
	fPoint max={-1000000000,-1000000000},min={1000000000,1000000000},cur;
	printf("   Input dimensions: %ix%i (LxS).\n",inDDR->nl,inDDR->ns);
	if (logflag) {
	  sprintf(logbuf,"   Input dimensions: %ix%i (LxS).\n",inDDR->nl,inDDR->ns);
	  printLog(logbuf);
	}
  	*outDDR=*inDDR;
	updateDDR(map,inDDR,outDDR);
	forwardMap(map,makePoint(0,0),&cur); grow(&min,&max,cur);
	forwardMap(map,makePoint(0,inDDR->nl),&cur); grow(&min,&max,cur);
	forwardMap(map,makePoint(inDDR->ns,0),&cur); grow(&min,&max,cur);
	forwardMap(map,makePoint(inDDR->ns,inDDR->nl),&cur); grow(&min,&max,cur);
	outDDR->ns=ceil(max.x);
	outDDR->nl=ceil(max.y); 
	
}
/*GetProjection:
	Interpolates the projection coordinates of the given DDR 
to find the projection coordinate of the given (pixel) point (x,y).
If val==0, Northing coordinates are returned.  
If val==1, Easting coordinates are returned.*/
float getProjection(float x,float y,struct DDR *ddr,int val)
{
	float dx=x/ddr->ns,dy=y/ddr->nl;
	float upint=ddr->upleft[val]+(ddr->upright[val]-ddr->upleft[val])*dx;
	float loint=ddr->loleft[val]+(ddr->loright[val]-ddr->loleft[val])*dx;
	return upint+(loint-upint)*dy;
}
/*UpdateProjection:
	Updates the projection corner coordinates for the new 
DDR.  It does so by reverse-projecting the corners of the new, output DDR
into the old, trusted, input DDR space.  The coordinates of the corners of the
output DDR are computed in input space using getProjection.	
*/
void update_projection(struct DDR *inDDR, mappingFunction map, struct DDR *outDDR)
{
	fPoint inPt,outPt;
	inPt.x=inPt.y=0; map->doMap((void *)map,inPt,&outPt);
	outDDR->upleft[0]=getProjection(outPt.x,outPt.y,inDDR,0);
	outDDR->upleft[1]=getProjection(outPt.x,outPt.y,inDDR,1);
	
	inPt.x=outDDR->ns;inPt.y=0; map->doMap((void *)map,inPt,&outPt);
	outDDR->upright[0]=getProjection(outPt.x,outPt.y,inDDR,0);
	outDDR->upright[1]=getProjection(outPt.x,outPt.y,inDDR,1);
	
	inPt.x=0;inPt.y=outDDR->nl; map->doMap((void *)map,inPt,&outPt);
	outDDR->loleft[0]=getProjection(outPt.x,outPt.y,inDDR,0);
	outDDR->loleft[1]=getProjection(outPt.x,outPt.y,inDDR,1);
	
	inPt.x=outDDR->ns;inPt.y=outDDR->nl; map->doMap((void *)map,inPt,&outPt);
	outDDR->loright[0]=getProjection(outPt.x,outPt.y,inDDR,0);
	outDDR->loright[1]=getProjection(outPt.x,outPt.y,inDDR,1);
}

int main(int argc, char *argv[])
{
	int outPixelType,outWidth=0,outHeight=0;
	int bandNo;
	struct DDR inDDR,outDDR;
	struct DDR *asDDR=NULL;/*DDR to copy projection info & size from.*/
	meta_parameters *meta;
	FILE *in,*out=NULL;
	mappingFunction map;
	sampleFunction samp;
	int inputIsComplex=0;
  
  /* First, we process the filename arguments.*/
	char infile[255],outfile[255];
	if (argc<3) usage(argv[0]);
	strcpy(infile,argv[argc-2]);
	strcpy(outfile,argv[argc-1]);
	if (findExt(infile)!=NULL&&(0==strcmp(".cpx",findExt(infile))))
		inputIsComplex=1;
	if (findExt(outfile)!=NULL&&(0==strcmp(".cpx",findExt(outfile))))
		if (inputIsComplex==0)
			printf("   WARNING: Remapping complex to non-complex data has undefined results!\n");
	in=fopenImage(infile,"rb");
	out=fopenImage(outfile,"wb");FCLOSE(out);/*Create the image*/
	out=fopenImage(outfile,"r+b");/*Re-Open for append (this lets us read & write).*/
/*	printf("Remap input image: '%s'.  Output image: '%s'.\n",infile,outfile);*/

	system("date");
	printf("Program: remap\n\n");
	logflag=0;
	
  /*Next, we process the rest of the arguments.*/
	outPixelType=process_CLAs(argc,argv,&map,&samp,&outWidth,&outHeight,&asDDR);
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: remap\n\n");
	}
  
  /*Now we read in the input DDR.*/
	meta = meta_read(infile);
	meta2ddr(meta,&inDDR); /* keep the current DDR implementation happy - should be gone in a while */
  
  /*We now calculate the correct values for the output DDR.*/
  	if (asDDR!=NULL)
  	{/*We copy all the projection, etc. info from the given DDR*/
  		outDDR=*asDDR;
  		outDDR.dtype=inDDR.dtype;
  		outDDR.nbands=inDDR.nbands;
  	} else {
  	/*We copy some fields from the input DDR & update the rest*/
		calc_outDDR(&inDDR,map,&outDDR);
		if (outWidth<0&&outHeight<0)
			{outDDR.ns=inDDR.ns;outDDR.nl=inDDR.nl;}
		if (outWidth>0)
			outDDR.ns=outWidth;
		if (outHeight>0)
			outDDR.nl=outHeight;
		update_projection(&inDDR,map,&outDDR);
	}
	printf("   Output dimensions: %ix%i (LxS).\n",outDDR.nl,outDDR.ns);
	if (logflag) {
	  sprintf(logbuf,"   Output dimensions: %ix%i (LxS).\n",outDDR.nl,outDDR.ns);
	  printLog(logbuf);
	}
	if (outPixelType)/*Set the output pixel type*/ {
	  outDDR.dtype=outPixelType;
	  switch (outPixelType) {
	  case 1:
	    meta->general->data_type = BYTE;
	    break;
	  case 2:
	    meta->general->data_type = INTEGER16;
	    break;
	  case 3:
	    meta->general->data_type = INTEGER32;
	    break;
	  case 4:
	    meta->general->data_type = REAL32;
	    break;
	  }
	}
	
   /*Now we write out the new DDR.
  	c_putddr(outfile,&outDDR); */
	/* Write a metadata file for output */
	meta->general->line_count = outDDR.nl;
	meta->general->sample_count = outDDR.ns;
	proj2meta(&outDDR,meta); /* temporary fix to populate the projection fields in the metadata file with DDR information */
	meta_write(meta,outfile);
	
/*Now we just call Perform_mapping, which does the actual I/O and the remapping.*/
	for (bandNo=0;bandNo<outDDR.nbands;bandNo++)
	{
		if (outDDR.nbands>1)
			printf("   Remapping band %d of %d...\n",bandNo+1,outDDR.nbands);
		if (inputIsComplex)
		{
			inDDR.dtype=DTYPE_COMPLEX;
			outDDR.dtype=DTYPE_COMPLEXREAL;
			printf("   Remapping complex data-- real portion:\n");
		}
		perform_mapping(in,&inDDR,out,&outDDR,map,samp,bandNo);
		if (inputIsComplex)
		{
			outDDR.dtype=DTYPE_COMPLEXIMAG;
			printf("   Remapping complex data-- imaginary portion:\n");
			perform_mapping(in,&inDDR,out,&outDDR,map,samp,bandNo);
		}
	}
/*	printf("Remap completed sucessfully!\n\n");*/
	if (logflag) StopWatchLog(fLog);
  
  	killMap(map);
	killSamp(samp);
	return(0);
}




