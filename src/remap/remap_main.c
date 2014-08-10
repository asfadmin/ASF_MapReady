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

#include "asf_remap.h"

int main(int argc, char *argv[])
{
	asfSplashScreen(argc,argv);
	char args[256];
	strcpy(args,"");
	int ii;
	for (ii=1; ii<argc; ++ii) {
		strcat(args, argv[ii]);
		if (ii < argc-1) strcat(args, " ");
	}
        return remap(args);
}

