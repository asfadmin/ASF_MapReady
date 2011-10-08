/*********************************************************************
NAME:     resample.c -- resamples ASF SAR images to desired resolution

SYNOPSIS: resample infile outfile pixsiz

DESCRIPTION:
        Resamples the input file to a desired output pixel resolution
    (assumed to be in meters).  Images are downsized by a factor equal
    to the desired resolution divided by the true resolution of the
    image (as given in the image's metadata).  This is the size used
    for the kernel processing.  Resampling proceeds by selecting the
    pixel in the input image that closest represents the middle of the
    new pixel in the output image.  The output pixel is given the value
    of the average of the kernel around the choosen input pixel.  This
    program combines the programs filter.c and subsample.c from the
    original EDC Terrain Correction software (LAS Modules).
        The image used as input is single-banded byte valued
    LAS 6.0 .img file.
        The output file produced by resample is a flat file (as per
    LAS 6.0 image standard) and a LAS ddr file is also created or updated
    to provide image metadata (size, etc).

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    filter()         finds a kernel average at a given point
                           -- moved to libasf_raster in 02/06

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     3/17/94      combined LAS modules filter and subsample on YMP
    2.0     3/94         Ported to T3D
    2.1     3/94         Began T3D I/O optimizations
    2.2     10/2/94      T3D Optimizations, Clean Up, & Commenting
    3.0     4/95     Ported to Sun workstations.  Allowed input to be
             an ASF SAR image or a 1-band byte LAS 6.0 image.
    3.1     10/4/95      Bug fix - trying to seek line -1 tested for.
    3.2     10/30/95     Changed call to create_ddr and added timer calls.
    3.3     9/96         Changed to handle RADARSAT era data
    3.4     6/98         Fixed non-ANSI timer routine.
    4.0     12/98        Paradoxically, removed ASF capability to make
                         cross-dataset capability easier to implement.
    4.5     07/05        Removed DDR dependency and made the tool to work
                         with floating point values
    5.0     02/06        Moved resampling code to libasf_raster

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:
    Parse command line parameters
    Read input image metadata from ddr file
    Establish kernel processing parameters
    Open input and output files
    for each output line
       calculate file read position
       read next kernel from input file
       if (!LAS) strip ceos wrapper from input data
       for each output pixel
     apply kernel to input data at appropriate position to get output value
       write output line to file
    if (!LAS) create a ddr for output file
    else      copy input ddr to output ddr (with update)
    Close input and output files

ALGORITHM REFERENCES:

BUGS:

AUTHOR:   T. Logan
*********************************************************************/
/****************************************************************************
*                                           *
*   resample.c -- resamples ASF SAR images to desired resolution        *
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
#include "ceos.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "resample_help.h"
#include "asf_license.h"

#define VERSION 5.0

int main(argc,argv)
    int      argc;
    char     **argv;
{
    char *infile;
    char *outfile;
    meta_parameters *metaIn;
    double  xscalfact=1;              /* x scale factor                  */
    double  yscalfact=1;              /* y scale factor                  */

    handle_common_asf_args(&argc, &argv, "resample");

    int use_nn = extract_flag_options(&argc, &argv,
                                      "-nearest_neighbor", "-nn",
                                      "--nearest_neighbor", "--nn",
                                      NULL);
    int is_square_pixsiz = extract_flag_options(&argc, &argv, "-square", NULL);
    int is_scaling = extract_flag_options(&argc, &argv, "-scale", NULL);
    int is_scalex = extract_double_options(&argc, &argv, &xscalfact,
                                          "-scalex", NULL);
    int is_scaley = extract_double_options(&argc, &argv, &yscalfact,
                                          "-scaley", NULL);

    if ((is_square_pixsiz + is_scaling + is_scalex > 1) ||
        (is_square_pixsiz + is_scaling + is_scaley > 1) ||
        (is_scalex == 1 && is_scaley == 0) ||
        (is_scaley == 1 && is_scalex == 0))
    {
       asfPrintStatus("*** Invalid combination of arguments.\n");
       usage();
       return 1;
    }

   /*--------  Process Command Line Inputs -------------*/
    if (argc>1) {
        check_for_help(argc, argv);
        handle_license_and_version_args(argc, argv, TOOL_NAME);
    }

    int args_required;
    if (is_square_pixsiz)
        args_required = 4; // resample <pixel_size> infile outfile
    else if (is_scaling)
        args_required = 4; // resample <scale_factor> infile outfile
    else if (is_scalex || is_scaley)
        args_required = 3; // resample infile outfile
    else
        args_required = 5; // resample <x pixsiz> <y pixsiz> infile outfile 

    if (argc < args_required) {
        asfPrintStatus("*** Not enough arguments.\n");
        usage();
        return 1;
    }
    if (argc > args_required) {
        asfPrintStatus("*** Too many arguments.\n");
        usage();
        return 1;
    }

    infile = argv[args_required-2];
    outfile = argv[args_required-1];

    // read metadata
    metaIn = meta_read(infile);
    
    // calculate the xscalfact, yscalfact
    if (is_square_pixsiz) {
        double pixsiz = atof(argv[1]);
        if (pixsiz<=0)
            asfPrintError("Invalid pixel size: %f", pixsiz);
        xscalfact = 1.0/(pixsiz/metaIn->general->x_pixel_size);
        yscalfact = 1.0/(pixsiz/metaIn->general->y_pixel_size);
        asfPrintStatus("Scaling to square pixels: %fm\n", pixsiz);
    }
    else if (is_scaling) {
        double scale = atof(argv[1]);
        if (scale<=0)
            asfPrintError("Invalid scale factor: %s\n", argv[1]);
        asfPrintStatus("Scaling in x and y by: %f\n", scale);
        //xscalfact = yscalfact = 1.0/scale;
        xscalfact = yscalfact = scale;
    }
    else if (is_scalex || is_scaley) { // should always be both
        asfPrintStatus("Scaling in x by: %f\n", xscalfact);
        asfPrintStatus("Scaling in y by: %f\n", yscalfact);
        if (xscalfact<=0)
            asfPrintError("Invalid x scale factor: %f\n", xscalfact);
        //xscalfact = 1.0/xscalfact;
        if (yscalfact<=0)
            asfPrintError("Invalid y scale factor: %f\n", yscalfact);
        //yscalfact = 1.0/yscalfact;
    }
    else {
        double xpixsiz = atof(argv[1]);
        if (xpixsiz<=0)
            asfPrintError("Invalid x pixel size: %s\n", argv[1]);
        xscalfact = 1.0/(xpixsiz/metaIn->general->x_pixel_size);

        double ypixsiz = atof(argv[2]);
        if (ypixsiz<=0)
            asfPrintError("Invalid y pixel size: %s\n", argv[2]);
        yscalfact = 1.0/(ypixsiz/metaIn->general->y_pixel_size);

        asfPrintStatus("Scaling to: %fm pixels in x.\n", xpixsiz);
        asfPrintStatus("            %fm pixels in y.\n", ypixsiz);
    }

    // finally ready
    resample_ext(infile, outfile, xscalfact, yscalfact, use_nn);
    
    meta_free(metaIn);
    return(0);
}
