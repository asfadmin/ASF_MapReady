/******************************************************************************** 
 *  Mode 0: Create a color image from two bands of FBD data using a modified
 *  Kellndorfer method:
 *
 *   Use 0 as background 
 *   Scale to dB (10*log10)
 *   HH: -30 dB is DN 1, -1 dB is DN 255
 *   HV: -30 dB is DN 1, -10 dB is DN 255
 *   DIV:  HH-HV, scale from -25 to -10
 *
 *  Mode 1: Create color image from three bands of PLR data using 3sigma stretch
 *
 *   Use 0 as background 
 *   Scale to dB (10*log10)
 *   scale each band separately by 3-sigma
 *
 *  Written by Tom Logan, March 2014
 *  Part of the RTC Project
 *
 *******************************************************************************/
#include "asf_nan.h"
#include "asf.h"
#include "asf_meta.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <asf_license.h>
#include <asf_contact.h>

typedef enum {
  TRUNCATE=1,
  MINMAX,
  MINMAX_MEDIAN,
  SIGMA,
  SIGMA3,
  HISTOGRAM_EQUALIZE,
  NONE
} scale_t3;


unsigned char *my_floats_to_bytes (float *data, long long pixel_count, float mask,
				scale_t3 scaling,float min, float max);

int main(int argc,char *argv[])
{
  char  infile1[256], infile2[256], infile3[256];  // Input file name                         
  char  outfile[256];         			   // Output file name
  int   mode;
  int   i,j;
  int   sample_count;


  if (argc != 4 && argc !=5) {
      printf("Usage:  %s <infile1> <infile2> [infile3] <outfile>\n",argv[0]);
      printf("\n%s : **Not enough arguments (need 3, got %d).\n",argv[0],argc);
      return(1);
  }

  if (argc == 4) mode = 0;  /* FBD data */
  if (argc == 5) mode = 1;  /* PLR data */

  printf("Creating colorized browse images from");
  if (mode == 0) printf(" FBD data\n");
  else printf(" PLR data\n");

  if (!quietflag) asfSplashScreen(argc, argv);

  if (mode == 0) {
    create_name(infile1,argv[1],".img");
    create_name(infile2,argv[2],".img");
    create_name(outfile,argv[3],".img");

    meta_parameters *meta1 = meta_read(infile1);
    meta_parameters *meta2 = meta_read(infile2);

    if (meta1->general->line_count != meta2->general->line_count ||
        meta1->general->sample_count != meta2->general->sample_count)
      {
        asfPrintError("Images must be the same size!!!\n");
        exit(1);
      }    
    strcpy(meta1->general->bands,"HH");
    strcpy(meta2->general->bands,"HV");

    int pixel_count = meta1->general->line_count*meta1->general->sample_count;
    float *buf1 = MALLOC(pixel_count * sizeof(float));
    float *buf2 = MALLOC(pixel_count * sizeof(float));
    float *buf3 = MALLOC(pixel_count * sizeof(float));
    unsigned char *cbuf1, *cbuf2, *cbuf3;
    FILE *fp1 = FOPEN(infile1, "r");
    FILE *fp2 = FOPEN(infile2, "r");
    FILE *ofp = FOPEN(outfile, "w");
    char ofile1[256];
    char ofile2[256];

    strcpy(ofile1,argv[1]);
    strcat(ofile1,"_DB.img");
    strcpy(ofile2,argv[2]);
    strcat(ofile2,"_DB.img");
 
    printf("Creating output DB files %s and %s\n",ofile1,ofile2);
    FILE *ofp1 = FOPEN(ofile1, "w");
    FILE *ofp2 = FOPEN(ofile2, "w");

    get_float_lines(fp1,meta1,0,meta1->general->line_count, buf1);
    get_float_lines(fp2,meta2,0,meta2->general->line_count, buf2);

    /* Convert data from sigma0 to dB */
    sample_count = 0;
    for (i=0; i<meta1->general->line_count; ++i) {
      for (j=0; j<meta1->general->sample_count; ++j) {
        if (meta_is_valid_double(buf1[sample_count])) {
          if (buf1[sample_count] != 0)
            buf1[sample_count] = 10.0 * log10f(buf1[sample_count]);
          if (buf2[sample_count] != 0)
            buf2[sample_count] = 10.0 * log10f(buf2[sample_count]);
        }
  	sample_count++;
      }
    }
    put_float_lines(ofp1, meta1, 0,meta1->general->line_count,buf1);
    put_float_lines(ofp2, meta2, 0,meta1->general->line_count,buf2);

    meta_write(meta1, ofile1);
    meta_write(meta2, ofile2);

    fclose(fp1);
    fclose(fp2);
    fclose(ofp1);
    fclose(ofp2);

    /* Scale the data to a byte range using given min/max values */
    cbuf1 = my_floats_to_bytes(buf1,(long long) pixel_count, 0.0,MINMAX,-30.0,-1.0); 
    cbuf2 = my_floats_to_bytes(buf2,(long long) pixel_count, 0.0,MINMAX,-30.0,-10.0); 
   
    /* 
    cbuf1 = my_floats_to_bytes(buf1,(long long) pixel_count, 0.0,MINMAX,-31.0,7.1); 
    cbuf2 = my_floats_to_bytes(buf2,(long long) pixel_count, 0.0,MINMAX,-31.0,7.1);
    */

    strcpy(ofile1,argv[1]);
    strcat(ofile1,"_byte.img");
    strcpy(ofile2,argv[2]);
    strcat(ofile2,"_byte.img");
 
    printf("Creating output byte files %s and %s\n",ofile1,ofile2);
    ofp1 = FOPEN(ofile1, "w");
    ofp2 = FOPEN(ofile2, "w");

    meta1->general->data_type=ASF_BYTE;
    meta2->general->data_type=ASF_BYTE;

    sample_count = 0;
    for (i=0; i<meta1->general->line_count; ++i) {
      for (j=0; j<meta1->general->sample_count; ++j) {
          buf1[sample_count] = (float) cbuf1[sample_count];
          buf2[sample_count] = (float) cbuf2[sample_count];
  	  sample_count++;
      }
    }

    put_float_lines(ofp1,meta1,0,meta1->general->line_count,buf1); 
    put_float_lines(ofp2,meta2,0,meta2->general->line_count,buf2); 

    meta_write(meta1, ofile1);
    meta_write(meta2, ofile2);

    fclose(ofp1);
    fclose(ofp2);

    /* Create the third band for the color image */
    sample_count = 0;
    for (i=0; i<meta1->general->line_count; ++i) {
      for (j=0; j<meta1->general->sample_count; ++j) {
 	 if (buf2[sample_count] != 0) {
           /*
           buf3[sample_count] = (buf1[sample_count] / buf2[sample_count]); 
           */

           buf3[sample_count] = (buf1[sample_count] - buf2[sample_count]);
           if (buf3[sample_count] < 1) buf3[sample_count] = 1;
           else if (buf3[sample_count] > 255) buf3[sample_count] = 255;
         } else buf3[sample_count] = 0;
         sample_count++;
       }
    }

    cbuf3 = my_floats_to_bytes(buf3,(long long) pixel_count, 0.0,SIGMA ,-25.0,-10.0); 
    sample_count = 0;
    for (i=0; i<meta1->general->line_count; ++i) {
      for (j=0; j<meta1->general->sample_count; ++j) {
          buf3[sample_count] = (float) cbuf3[sample_count];
  	  sample_count++;
      }
    }

    /* Finally, create the 3 banded image we were looking for */
    strcpy(meta1->general->bands,"HH,HV,DIV");
    meta1->general->band_count=3;
    put_band_float_lines(ofp,meta1,0,0,meta1->general->line_count,buf1);
    put_band_float_lines(ofp,meta1,1,0,meta1->general->line_count,buf2);
    put_band_float_lines(ofp,meta1,2,0,meta1->general->line_count,buf3);

    meta_write(meta1,outfile);

  } else {

    /* Mode 1 - Create Color Browse from 3 bands using 3sigma stretch */
    create_name(infile1,argv[1],".img");
    create_name(infile2,argv[2],".img");
    create_name(infile3,argv[3],".img");
    create_name(outfile,argv[4],".img");

    meta_parameters *meta1 = meta_read(infile1);
    meta_parameters *meta2 = meta_read(infile2);
    meta_parameters *meta3 = meta_read(infile3);

    if (meta1->general->line_count != meta2->general->line_count ||
        meta1->general->sample_count != meta2->general->sample_count)
      {
        asfPrintError("Images must be the same size!!!\n");
        exit(1);
      }

    if (meta3->general->line_count != meta2->general->line_count ||
        meta3->general->sample_count != meta2->general->sample_count)
      {
        asfPrintError("Images must be the same size!!!\n");
        exit(1);
      }

    int pixel_count = meta1->general->line_count*meta1->general->sample_count;
    float *buf1 = MALLOC(pixel_count * sizeof(float));
    float *buf2 = MALLOC(pixel_count * sizeof(float));
    float *buf3 = MALLOC(pixel_count * sizeof(float));
    float *buf4 = MALLOC(pixel_count * sizeof(float));
    unsigned char *cbuf1, *cbuf2, *cbuf3, *cbuf4;
    FILE *fp1 = FOPEN(infile1, "r");
    FILE *fp2 = FOPEN(infile2, "r");
    FILE *fp3 = FOPEN(infile3, "r");
    FILE *ofp = FOPEN(outfile, "w");

    get_float_lines(fp1,meta1,0,meta1->general->line_count, buf1);
    get_float_lines(fp2,meta2,0,meta2->general->line_count, buf2);
    get_float_lines(fp3,meta3,0,meta3->general->line_count, buf3);

    /* Convert data from sigma0 to dB */
    sample_count = 0;
    for (i=0; i<meta1->general->line_count; ++i) {
      for (j=0; j<meta1->general->sample_count; ++j) {
        if (meta_is_valid_double(buf1[sample_count])) {
          if (buf1[sample_count] != 0)
            buf1[sample_count] = 10.0 * log10f(buf1[sample_count]);
          if (buf2[sample_count] != 0)
            buf2[sample_count] = 10.0 * log10f(buf2[sample_count]);
          if (buf3[sample_count] != 0)
            buf3[sample_count] = 10.0 * log10f(buf3[sample_count]);
        }
  	sample_count++;
      }
    }
    /* Scale the data to a byte range using 3-sigma stretch values */
    cbuf1 = my_floats_to_bytes(buf1,(long long) pixel_count, 0.0,SIGMA3,-30.0,-1.0);
    cbuf2 = my_floats_to_bytes(buf2,(long long) pixel_count, 0.0,SIGMA3,-30.0,-10.0);
    cbuf3 = my_floats_to_bytes(buf3,(long long) pixel_count, 0.0,SIGMA3,-30.0,-10.0);

    meta1->general->data_type=ASF_BYTE;
    meta2->general->data_type=ASF_BYTE;
    meta3->general->data_type=ASF_BYTE;

    sample_count = 0;
    for (i=0; i<meta1->general->line_count; ++i) {
      for (j=0; j<meta1->general->sample_count; ++j) {
          buf1[sample_count] = (float) cbuf1[sample_count];
          buf2[sample_count] = (float) cbuf2[sample_count];
          buf3[sample_count] = (float) cbuf3[sample_count];
          sample_count++;
      }
    }
 
    /* Finally, create the 3 banded image we were looking for */
    strcpy(meta1->general->bands,"HH,HV,VV");
    meta1->general->band_count=3;
    put_band_float_lines(ofp,meta1,0,0,meta1->general->line_count,buf1);
    put_band_float_lines(ofp,meta1,1,0,meta1->general->line_count,buf2);
    put_band_float_lines(ofp,meta1,2,0,meta1->general->line_count,buf3);

    meta_write(meta1,outfile);
  }


  asfPrintStatus("Done.\n");
  exit(EXIT_SUCCESS);
}

unsigned char *my_floats_to_bytes (float *data, long long pixel_count, float mask,
				scale_t3 scaling,float inmin, float inmax)
{
  long long ii;
  double imin=99999, imax=-99999, imean=0, isdev=0;
  double diff_min, diff_max, omin, omax, slope, offset;
  unsigned char *pixels = malloc (pixel_count * sizeof (unsigned char));

  switch (scaling) 
    {
    case TRUNCATE:
      /* Compute all the output pixels truncating the input values */
      for (ii=0; ii<pixel_count; ii++) 
	if (data[ii] < 0) {
	  pixels[ii] = 0;
	}
	else if (data[ii] > 255) {
	  pixels[ii] = 255;
	}
	else
	  pixels[ii] = data[ii] + 0.5;
      break;

    case MINMAX:
      /* Determine the minimum and maximum values for the image. Exclude the 
       * mask value for this calculation */
      printf("USING MINMAX MAPPING...\n");
/*
      for (ii=0; ii<pixel_count; ii++) {
        if (!ISNAN(mask)) {  
	  if (data[ii] < imin && !FLOAT_EQUIVALENT(data[ii], mask)) 
	    imin = data[ii];
	  if (data[ii] > imax && !FLOAT_EQUIVALENT(data[ii], mask)) 
	    imax = data[ii];
	}
	else {
	  if (data[ii] < imin) imin = data[ii];
	  if (data[ii] > imax) imax = data[ii];
	} 
      }
      printf("Calculated min/max as %lf/%lf\n",imin,imax);
*/

      imin = inmin;
      imax = inmax;
    
      omin = imin;
      omax = imax;

      /* Compute all the output pixels stretching the minimum and maximum value
       * into the byte range */
      slope = 255 / (omax-omin);
      offset = -slope * omin;
      printf("slope = %lf, offset %lf\n",slope,offset);
      for (ii=0; ii<pixel_count; ii++) {
        if (data[ii] == mask)
          pixels[ii] = mask;
	else if ((slope * data[ii] + offset) < 1)
	  pixels[ii] = 1;
	else if ((slope * data[ii] + offset) > 255)
	  pixels[ii] = 255;
	else
	  pixels[ii] = slope * data[ii] + offset;
      }
      break;

    case SIGMA:
      /* Determine the minimum, maximum, mean, and standard deviation for the
       * image. Exclude the mask value from the calculation */
      calc_stats(data, pixel_count, 0.0, &imin, &imax, &imean, &isdev);
      printf("imin = %f; imax = %f; imean = %f; isdev = %f\n",imin,imax,imean,isdev);

      /* Apply 2 sigma to calculate new minimum and maximum */
      diff_min = imean - 2*isdev;
      diff_max = imean + 2*isdev;

      omin = diff_min;
      omax = diff_max;
      
      if (diff_min < imin) omin = imin;
      if (diff_max > imax) omax = imax;
      
      /* Computing output pixels applying the sigma mapping */
      slope = 255 / (omax-omin);
      offset = -slope * omin;
      for (ii=0; ii<pixel_count; ii++) {
        if (data[ii] == mask) {
  	  pixels[ii] = mask;
        } else {
	  if ((slope * data[ii] + offset) < 0)
  	    pixels[ii] = 0;
	  else if ((slope * data[ii] + offset) > 255)
	    pixels[ii] = 255;
	  else 
	    pixels[ii] = slope * data[ii] + offset;
        }
      }
      break;

    case SIGMA3:
      /* Determine the minimum, maximum, mean, and standard deviation for the
       * image. Exclude the mask value from the calculation */
      calc_stats(data, pixel_count, 0.0, &imin, &imax, &imean, &isdev);
      printf("imin = %f; imax = %f; imean = %f; isdev = %f\n",imin,imax,imean,isdev);

      /* Apply 3 sigma to calculate new minimum and maximum */
      diff_min = imean - 3*isdev;
      diff_max = imean + 3*isdev;

      omin = diff_min;
      omax = diff_max;
      
      if (diff_min < imin) omin = imin;
      if (diff_max > imax) omax = imax;
      
      /* Computing output pixels applying the sigma mapping */
      slope = 255 / (omax-omin);
      offset = -slope * omin;
      for (ii=0; ii<pixel_count; ii++) {
        if (data[ii] == mask) {
  	  pixels[ii] = mask;
        } else {
	  if ((slope * data[ii] + offset) < 0)
  	    pixels[ii] = 0;
	  else if ((slope * data[ii] + offset) > 255)
	    pixels[ii] = 255;
	  else 
	    pixels[ii] = slope * data[ii] + offset;
        }
      }
      break;


    default:
      asfPrintError("Undefined scaling mechanism!");
      break;
      
    }
  
  return pixels;
}

