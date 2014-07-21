/******************************************************************************
NAME: fit_warp

SYNOPSIS: fit_warp <in> <image> <out>

DESCRIPTION:
    	
    	fit_warp finds a weighted combination of the point offsets
    from fico to generate two output translation LAS images--
    out.horiz and out.vert.  These can be fed to the "-warp" option
    in remap.
    
       The resulting warping images can represent a non-linear 
    image-image shift, which may result in much higher phase
    coherence for interferometry over moving and deforming surfaces,
    such as glaciers or ice sheets.
    

OPTIONS:
    	<in> an fico offset point file
    	<image> is a LAS image, used to set the size of the offset files.
    	     Otherwise, it is unused.
    	<out> the base name for the warp image files.
    	     out.horiz will the be the horizontal warping offset image;
    	     out.vert will be the vertical warping offset image.
    		
EXTERNAL ASSOCIATES:none

FILE REFERENCES:none

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0     5/99   O. Lawlor	Modified from fit_plane, for mesh-based
    				antarctic ice sheet interferometry.
    1.1     7/05   R. Gens      Removed DDR dependency. Took care of endianess

HARDWARE/SOFTWARE LIMITATIONS: none


ALGORITHM REFERENCES:

BUGS: none known

******************************************************************************/
/****************************************************************************
*								            *
*   Fit_warp takes, as an input, correlation points from fico and 	    *
*   produces as output a horizontal and vertical warping offset image	    *
*   for use with remap.							    *
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
#include "asf_license.h"
#include "asf_raster.h"
#define ASF_NAME_STRING "fit_warp"

void usage(char *name)
{
		printf("\n\
Usage: %s <in> <image> <out>\n\
\t<in> input: a correlation point file (from coregister_fine)\n\
\t<image> input: an image (to set output size)\n\
\t<out> output: a remap-compatible set of warping files.\n\
\n\
Fit_warp takes, as an input, correlation points from coregister_fine and \n\
produces as output a horizontal and vertical warping offset image\n\
for use with remap.\n\
\nVersion: %s\n\n",name,version_string(name));
		exit(1);
}

int main(int argc,char **argv)
{
        handle_common_asf_args(&argc, &argv, ASF_NAME_STRING);
        asfSplashScreen(argc, argv);

	if (argc!=4) usage(ASF_NAME_STRING);
 	return fit_warp(argv[1], argv[2], argv[3]);
}

