/******************************************************************************
NAME:  deskew -- remove squint induced skew in imagery

SYNOPSIS:  deskew <infile> <inmeta> <outfile> <outmeta>

DESCRIPTION:

	Deskew uses the squint angle of an image along with the
        look angle to determine the amount of parallelogram shift
        skew that has been introduced in an image due to the doppler
        centroid choosen during image processing.  It then remaps
        the image (using bi-linear interpolation) to remove this
        skew.
 

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0		   Tom Logan

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/

/******************************************************************************
*                                                                             *
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
#include "geolocate.h"
#include "ddr.h"

#define VERSION 1.0
#define RTD	(180.0/M_PI)
#define DTR 	(M_PI/180.0)

int main(int argc,char *argv[])
{
  double	yaw, look, fac;
  meta_parameters *meta_img;
  int      np, nl,                /* in number of pixels,lines      */
    i, line, out_line;
  /*char     tfile[256];*/
  FILE        *fpi, *fpo;
  float *ibuf, *obuf;
  float	*skew;
  int *lower;
  char infile[255], inmeta[255], outfile[255], outmeta[255];
  
  if (argc != 3)
    { 
      printf("\nUsage: %s <infile> <outfile>\n",argv[0]);
      printf("       <infile> 	input file name (.img,.meta)\n");
      printf("       <outfile>  output file name (.img,.meta)\n");
      printf("\n"
	     "Removes doppler skew from image.\n");
      printf("Version %.2f, ASF SAR TOOLS\n\n",VERSION);
      exit(1);
    }

  create_name(infile, argv[1], ".img");
  create_name(outfile, argv[2], ".img");
  meta_img = meta_read(infile); 
  nl = meta_img->general->line_count;
  np = meta_img->general->sample_count;
  
  {
    meta_parameters *meta=meta_init(infile);
    GEOLOCATE_REC *g;
    stateVector stVec=meta_get_stVec(meta,0.0);
    if (meta->sar->deskewed!=0)
      {
	printf("The image %s is already deskewed!\n", infile);
	exit(30);
      }
    
    g=init_geolocate_meta(&stVec,meta);
    getLookYaw(g,
	       meta_get_slant(meta,0,0),
	       meta_get_dop(meta,0,0),
	       &look,&yaw);
    free_geolocate(g);
    
    meta->sar->deskewed = 1;
    meta_write(meta, outfile);
  }
  
  fac = sin(look)*sin(yaw);
  
  skew =  (float *) MALLOC (np * sizeof(float));
  lower =  (int *) MALLOC (np * sizeof(int));
  
  for (i=0; i<np; i++)
    {
      skew[i] = (float)i * fac;
      lower[i] = (int) skew[i];
    }
  printf("Maximum Skew = %f\n", skew[np-1]);
  
  ibuf =  (float *) MALLOC (np * sizeof(float));
  obuf =  (float *) MALLOC (nl*np * sizeof(float));
  
  fpi=fopenImage(infile,"rb");
  
  for (line = 0; line < nl; line++)  
    {
      get_float_line(fpi, meta_img, line, ibuf);
      for (i=0; i<np; i++)
	{
	  out_line = line + lower[i];
	  if (out_line >= 0 && out_line < nl) 
	    obuf[out_line*np+i] = ibuf[i];
	}
      if (line%1000==0) printf("Reading Line %i\n",line);
    }
  FCLOSE(fpi);
  
  fpo=fopenImage(outfile,"wb");
  put_float_lines(fpo, meta_img, 0, nl, obuf);
  FCLOSE(fpo);

  return 0;
}
