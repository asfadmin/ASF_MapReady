/******************************************************************************
NAME: convert2geotiff - To convert an ASF tools image to a geoTIFF file.

SYNOPSIS:  convert2geotiff <inASF> <outGeoTIFF>

DESCRIPTION:
        Convert2geotiff converts an ASF tools image to a geoTIFF file.  Works
        with byte, short, integer and float data, 1 or 3 banded as well.
        Convert2geotiff will add all extensions.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0     8/01   tae          Original tae code to handle byte data.
                                Source code given to ASF from ..???tae
    1.0     8/01   S. Watts     Converted code from tae to ASF.  Changed
                                program to use short, integer, and float data.
    1.1     2/04   P. Denny     New license, replaced references to "LAS" with
                                "ASF tools", renamed from las2geotiff to
                                convert2geotiff.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/******************************************************************************
*                                                                             *
* convert2geotiff - To convert an ASF tools image to a geoTIFF file.          *
*                                                                             *
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
#include "asf_meta.h"
#include "ifm.h"
#include "proj.h"
#include "convert2geotiff_IO.h"
#include "geotiffio.h"
#include "xtiffio.h"
#include "protos.h"

#define VERSION 1.1

static void usage (char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <inASF> <outGeoTIFF>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <inASF>       Base name for (input) ASF tools image with metadata file.\n"
	"   <outGeoTIFF>  Base name for (output) geotiff image.\n"
	"                 (Do not add extension.  Program will.)\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts an ASF tools image into a geotiff file.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n", VERSION);
  exit(EXIT_FAILURE);
}

int main (int argc, char *argv[])
{
  struct DDR ddr;
  char infile[256];
  char outfile[256];
  TIFF *out=(TIFF*)0;
  GTIF *gtif=(GTIF*)0;          /* GeoKey-level descriptor */
  FILE *fpin;
  double tiepoints[4][6];       /* coordinate array for GEOTIEPOINTS */
  double pixelscale[3];         /* coordinate array for PIXELSCALE */
  uint16 bitspersample = 0;     /* Number of bits per sample */
  int dtype=0;                  /* ddr data type */
  int nbands=0;                 /* # of actual bands in output image */
  int imagelength;              /* Length of image line  */
  int imagewidth;               /* width of image */
  long size;                   /* # of array elements in tiepoints */
  int sampleformat_variable;   /* datatype for output tiff tag */
  int test_size;

/***Usage***/
if (argc != 3) {usage(argv[0]);exit(0);}

strcpy(infile, argv[1]);
strcpy(outfile, argv[2]);

/******Get ddr information and initialize values*********/
c_getddr(infile, &ddr);
imagewidth=ddr.ns;
imagelength=ddr.nl;
nbands = ddr.nbands;
dtype = ddr.dtype;

/*****Open Image******/
fpin=fopenImage(infile, "rb");

/*****Determine data type for output bitspersample********/
switch (dtype)
  {
    case 1: bitspersample = 8;
            sampleformat_variable = SAMPLEFORMAT_INT;
            break;
    case 2: bitspersample = 16;
            sampleformat_variable = SAMPLEFORMAT_UINT;
            break;
    case 3: bitspersample = 32;
            sampleformat_variable = SAMPLEFORMAT_UINT;
            break;
    case 4: bitspersample = 32;
            sampleformat_variable = SAMPLEFORMAT_IEEEFP;
            break;
    default:
        printf("\n\nData type of input image is not supported.\n\n"
                        "Exiting...\n\n"); exit(0);
  }


/*****Open Tiff file and GeoKey level descriptor file ********/
out = XTIFFOpen(appendExt(outfile,".tif"),"w");
if (out == NULL)
{printf("\n\nExiting...\n\n"); exit(0);}

gtif = GTIFNew(out);
  if (!gtif)
  {
    TIFFClose(out);
    printf("\n\nError creating GTIF\n\n");
  }

/********Set the normal TIFF image fields **********/
  TIFFSetField(out, TIFFTAG_SUBFILETYPE, 0);
  TIFFSetField(out, TIFFTAG_IMAGEWIDTH, imagewidth);
  TIFFSetField(out, TIFFTAG_IMAGELENGTH, imagelength);
  TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, bitspersample);
  TIFFSetField(out, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
  if (nbands==1)
    TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  else if (nbands==3)
    TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
  else
    TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_SEPARATED);
  TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, nbands);
  TIFFSetField(out, TIFFTAG_ROWSPERSTRIP,1);
  TIFFSetField(out, TIFFTAG_XRESOLUTION,1);
  TIFFSetField(out, TIFFTAG_YRESOLUTION,1);
  TIFFSetField(out, TIFFTAG_RESOLUTIONUNIT, RESUNIT_NONE);
  TIFFSetField(out, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(out, TIFFTAG_SAMPLEFORMAT, sampleformat_variable);
  TIFFSetField(out, TIFFTAG_DATATYPE, sampleformat_variable );

/*******Set normal geotiff tags*******************/
if (ddr.valid[6])
  {
    /* the corner coordinates in the ddr are valid, so need to
       put them in the geoTIFF
       ------------------------------------------------------- */
    get_tiepoints(ddr,tiepoints,&size);

    /* Some applications (e.g., ArcView) won't handle geoTIFF images with
       more than one tie point pair.  Therefore, only the upper left corner
       is being written to the geoTIFF file.  In order to write all computed
       tie points to the geoTIFF, change the 6 to size in the line below.
    ------------------------------------------------------------------------*/
  TIFFSetField(out, TIFFTAG_GEOTIEPOINTS, 6, tiepoints);

  }
  else
   printf("\n\nImage does not contain valid corner coordinates\n\n");

if (ddr.valid[5])
  {
    /* the pixel scales in the ddr are valid,
       so need to put them in the geoTIFF
       -------------------------------------- */
    pixelscale[0] = ddr.pdist_x;       /* x-axis scale    */
    pixelscale[1] = ddr.pdist_y;       /* y-axis scale    */
    pixelscale[2] = 0;                 /* z-axis scale    */
    TIFFSetField(out, TIFFTAG_GEOPIXELSCALE, 3, pixelscale);
  }
  else
  {
    printf("\n\nImage does not contain valid projection distance\n\n");
  }
/* all the tags that apply to any geoTIFF file are
     done so time to add the projection specific tags
     ------------------------------------------------ */
  if (ddr.valid[0])
  {
    switch (ddr.proj_code)
    {
      case GEO:
        geokeyset(ddr,gtif);
        break;
      case UTM:
        utmkeyset(ddr,gtif);
        break;
      case SPCS:
        spcskeyset(ddr,gtif);
        break;
      case ALBERS:
        alberskeyset(ddr,gtif);
        break;
      case LAMCC:
        lamcckeyset(ddr,gtif);
        break;
      case MERCAT:
        mercatkeyset(ddr,gtif);
        break;
      case PS:
        pskeyset(ddr,gtif);
        break;
      case POLYC:
        polyckeyset(ddr,gtif);
        break;
      case EQUIDC:
        equidckeyset(ddr,gtif);
        break;
      case TM:
        tmkeyset(ddr,gtif);
        break;
      case STEREO:
        stereokeyset(ddr,gtif);
        break;
      case LAMAZ:
        lamazkeyset(ddr,gtif);
        break;
      case AZMEQD:
        azmeqdkeyset(ddr,gtif);
        break;
      case GNOMON:
        gnomonkeyset(ddr,gtif);
        break;
      case ORTHO:
        orthokeyset(ddr,gtif);
        break;
      case GVNSP:
        gvnspkeyset(ddr,gtif);
        break;
      case SNSOID:
        snsoidkeyset(ddr,gtif);
        break;
      case EQRECT:
        eqrectkeyset(ddr,gtif);
        break;
      case MILLER:
        millerkeyset(ddr,gtif);
        break;
      case VGRINT:
        vgrintkeyset(ddr,gtif);
        break;
      case HOM:
        homkeyset(ddr,gtif);
        break;
      case ROBIN:
        robinkeyset(ddr,gtif);
        break;
      case SOM:
        somkeyset(ddr,gtif);
        break;
      case ALASKA:
        alaskakeyset(ddr,gtif);
        break;
      case GOOD:
        goodkeyset(ddr,gtif);
        break;
      case MOLL:
        mollkeyset(ddr,gtif);
        break;
      case IMOLL:
        imollkeyset(ddr,gtif);
        break;
      case HAMMER:
        hammerkeyset(ddr,gtif);
        break;
      case WAGIV:
        wagivkeyset(ddr,gtif);
        break;
      case WAGVII:
        wagviikeyset(ddr,gtif);
        break;
      case OBEQA:
        obeqakeyset(ddr,gtif);
        break;
    }
  }
  else
    printf("\nImage does not contain valid projection code\n");

/***********I/O PROCEDURE FOR ALL MULTI-BANDS IMAGES*************/
if(nbands!=1)
  {
  /****CHECK TO SEE IF THERE WILL BE ENOUGH MEMORY FOR IMAGE*******/
  test_size=(imagelength*imagewidth*nbands)/1000000;
  if (test_size > MEM_SPACE)
    {
    printf("\nInput image is too large for this program.\n"
           "Image must be less than %d.   Exiting program\n",MEM_SPACE);
    exit(0);
    }
  if (dtype ==1)
  three_band_byte(out,fpin,gtif,imagewidth,imagelength,outfile,nbands);
  else
    {
    printf("\nERROR: 3-banded images must be byte data only\n"
           "Exiting...\n");
    exit(0);
    }
  }/**END OF MULTIPLE BANDS I/O PROCEDURE***/

else { /**FOR ALL 1-BAND IMAGES****/
  /**********Call functions that perform appro. I/O *************/
   if (dtype == 1)
     byte_IO(out,fpin, gtif,imagewidth, imagelength, outfile);
   else if (dtype == 2)
     short_IO(out,fpin, gtif,imagewidth, imagelength, outfile);
   else if (dtype == 3)
     integer_IO(out,fpin, gtif,imagewidth, imagelength, outfile);
   else if (dtype == 4)
     float_IO(out,fpin, gtif,imagewidth, imagelength, outfile);
   else {
     printf("\n"
            "Data type of input image is not supported in this program\n"
            "Exiting...\n");
     exit(EXIT_FAILURE);
   }
}

return(0);
}
