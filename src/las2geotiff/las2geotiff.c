/******************************************************************************
NAME: las2geotiff - To convert a LAS image to a geoTIFF file.

SYNOPSIS:  las2geotiff <inLAS> <outGeoTIFF>

DESCRIPTION:  

	Las2geotiff converts a LAS image to a geoTIFF file.  Works with byte,
	short, integer and float data, 1 or 3 banded as well.  Las2geotiff
	will add all extensions.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0	    8/01   tae		Original tae code to handle byte data.
				Source code given to ASF from ..???tae
    1.0     8/01   S. Watts     Converted code from tae to ASF.  Changed 
				program to use short, integer, and float data.

HARDWARE/SOFTWARE LIMITATIONS:
	
ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   las2geotiff - To convert a LAS image to a geoTIFF file.		    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
#include "ddr.h"
#include "ifm.h"
#include "proj.h"
#include "las2geotiff_IO.h"
#include "geotiffio.h"
#include "xtiffio.h"
#include "protos.h"

#define VERSION 1.0


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
  uint16 bitspersample = 0;	/* Number of bits per sample */
  int dtype=0;			/* ddr data type */
  int nbands=0;               	/* # of actual bands in output image */
  int imagelength;	 	/* Length of image line	 */
  int imagewidth;		/* width of image */
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
   printf("\n\nLAS image does not contain valid corner coordinates\n\n");
  
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
    printf("\n\nLAS image does not contain valid projection distance\n\n");
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
    printf("\nLAS image does not contain valid projection code\n");

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

else  /**FOR ALL 1-BAND IMAGES****/
  {
  /**********Call functions that perform appro. I/O *************/
if (dtype == 1)
  byte_IO(out,fpin, gtif,imagewidth, imagelength, outfile);
else if (dtype == 2)
  short_IO(out,fpin, gtif,imagewidth, imagelength, outfile);
else if (dtype == 3)
  integer_IO(out,fpin, gtif,imagewidth, imagelength, outfile);
else if (dtype == 4)
  float_IO(out,fpin, gtif,imagewidth, imagelength, outfile);
else
  {printf("\nData type of input image is not supported in this program\n"
	"Exiting...\n");exit(0);}
  }

return(0);
}

void usage (char *name)
{
printf(
  "\nUsage:  %s <inLAS> <outGeoTIFF>\n\n"
  "\t<inLAS>       Base name for (input) LAS image with valid ddr file.\n"
  "\t<outGeoTIFF>  Base name for (output) geotiff image.\n"
  "\t              (Do not add extension.  Program will.)\n\n"
  "las2geotiff:  converts a LAS image into a geotiff file.\n"
  "Version %.2f, ASF SAR Tools\n\n", name, VERSION);  
  exit(0);
}

