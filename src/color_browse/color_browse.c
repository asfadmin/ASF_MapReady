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
 *  Extended by Rudi Gens for Sentinel dual-pol browse images (March 2015)
 *
 *******************************************************************************/
#include "asf_nan.h"
#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_export.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <asf_license.h>
#include <asf_contact.h>

typedef enum {
  PALSAR_FBD,
  PALSAR_PLR,
  SENTINEL_DUAL,
  NOTYPE
} browse_type_t;

unsigned char *my_floats_to_bytes (float *data, long long pixel_count, float mask,
				scale_t scaling,float min, float max);

#define VERSION 1.1

void usage(char *name)
{
  printf("\n"
   "USAGE:\n"
   "   color_browse [-sentinel <scale>] <inFile1> <inFile2> [<inFile3>] "
   "<outFile\n");
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   inFile1   Name of an ASF internal file (HH or VV)\n"
   "   inFile2   Name of an ASF internal file (HV or VH)\n"
   "   outFile   Name of the output browse image\n");
  printf("\n"
   "OPTIONAL ARGUMENTS:\n"
   "  -sentinel  Input data is from Sentinel instead of ALOS PALSAR (default)\n"
   "   inFile3   Name of an ASF internal file (VV for PLR data)\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program generates browse images for RTC products and Sentinel data.\n"
   "   For Sentinel data they are create with associated world and auxiliary"
   " files for ease of use in a GIS environment.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

int strmatches(const char *key, ...)
{
    va_list ap;
    char *arg = NULL;
    int found = FALSE;

    va_start(ap, key);
    do {
        arg = va_arg(ap, char *);
        if (arg) {
            if (strcmp(key, arg) == 0) {
                found = TRUE;
                break;
            }
        }
    } while (arg);

    return found;
}

static int strindex(char s[], char t[])
{
  int i, j, k;

  for (i=0; s[i]!='\0'; i++) {
    for (j=i, k=0; t[k]!='\0' && s[j]==t[k]; j++, k++)
      ;
    if (k>0 && t[k]=='\0')
      return i;
  }
  return -1;
}

static char *read_param(char *line)
{
  int i, k;
  char *value=(char *)CALLOC(256, sizeof(char));

  strcpy(value, "");
  i=strindex(line, "]");
  k=strindex(line, "=");
  if (i>0) strncpy(value, line, i+1);
  if (k>0) strncpy(value, line, k);
  return value;
}

static char *read_str(char *line, char *param)
{
  static char value[255];
  char *p = strchr(line, '=');

  // skip past the '=' sign, and eat up any whitespace
  ++p;
  while (isspace(*p))
      ++p;

  strcpy(value, p);

  // eat up trailing whitespace, too
  p = value + strlen(value) - 1;
  while (isspace(*p))
      *p-- = '\0';

  return value;
}

int main(int argc,char *argv[])
{
  char  infile1[256], infile2[256], infile3[256];  // Input file name                         
  char  outfile[256];         			   // Output file name
  browse_type_t mode = NOTYPE;
  int   i,j;
  int   sample_count;
  double scale=-1;
  extern int currArg;

  // Parse command line
  if ((argc-currArg)<1) {
    printf("Insufficient arguments.\n"); 
    usage("");
  }

  while (currArg < (argc-2)) {
    char *key = argv[currArg++];
    if (strmatches(key, "-sentinel", "--sentinel", NULL)) {
      CHECK_ARG(1);
      scale = atof(GET_ARG(1));
      mode = SENTINEL_DUAL;
    }
    else if (strmatches(key, "-log", "--log", NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key, "-quiet", "--quiet", "-q", NULL))
      quietflag = TRUE;
    else {
      --currArg;
      break;
    }
  }
  if ((argc-currArg) < 2) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }
  if (mode == NOTYPE && argc == 4)
    mode = PALSAR_FBD;
  else if (mode == NOTYPE && argc == 5) 
    mode = PALSAR_PLR;

  if (!quietflag) 
    asfSplashScreen(argc, argv);

  if (mode == PALSAR_FBD) {
  
    asfPrintStatus("Creating colorized browse image from PALSAR FBD data\n");
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

    meta1->general->data_type=REAL32;
    meta2->general->data_type=REAL32;

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

  } 
  else if (mode == PALSAR_PLR) {
  
    /* Mode 1 - Create Color Browse from 3 bands using 3sigma stretch */
    asfPrintStatus("Creating colorized browse image from PALSAR PLR data\n");
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

    meta1->general->data_type=REAL32;
    //meta2->general->data_type=ASF_BYTE;
    //meta3->general->data_type=ASF_BYTE;

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
  else if (mode == SENTINEL_DUAL) {
  
    // Read temporary directory from MapReady settings file
    char settingsFile[1024], line[1024], params[25], tmpPath[512]="", *test;
    
    sprintf(settingsFile, "%s%cmapready_settings.cfg", 
      get_asf_share_dir(), DIR_SEPARATOR);
    FILE *fp = FOPEN(settingsFile, "r");
    if (fp) {
      while (fgets(line, 1024, fp) != NULL) {
        if (strncmp(line, "[General]", 9) == 0)
          strcpy(params, "general");
        if (strncmp(params, "general", 7) == 0) {
          test = read_param(line);
          if (strncmp(test, "temporary directory", 19) == 0)
            strcpy(tmpPath, read_str(line, "temporary directory"));
          FREE(test);
        }
      }
    }
    FCLOSE(fp);
  
    asfPrintStatus("Creating colorized browse image from Sentinel dual-pol "
      "data\n");
    create_name(infile1,argv[3],".img");
    create_name(infile2,argv[4],".img");
    create_name(outfile,argv[5],".tif");

    // Create temporary directory
    char tmpDir[1024];
    if (strlen(tmpPath) > 0)
      sprintf(tmpDir, "%s%cbrowse-", tmpPath, DIR_SEPARATOR);
    else
      strcpy(tmpDir, "browse-");
    strcat(tmpDir, time_stamp_dir());
    create_clean_dir(tmpDir);
    asfPrintStatus("Temp dir is: %s\n", tmpDir);
 
    // Calculate ratio image
    char tmpRatio[512], tmpRed[512], tmpGreen[512], tmpBlue[512], tmpIn[512];
    char *inFiles[2]; 
    inFiles[0] = (char *) MALLOC(sizeof(char)*255);
    inFiles[1] = (char *) MALLOC(sizeof(char)*255);
    strcpy(inFiles[0], infile1);
    strcpy(inFiles[1], infile2);
    sprintf(tmpRatio, "%s%cdiv.img", tmpDir, DIR_SEPARATOR);
    raster_calc(tmpRatio, "a/b", 2, inFiles);
   
    if (scale<0) asfPrintError("Invalid scale value: %f\n", scale);
 
    // Resample all three bands and scale to byte
    meta_parameters *metaIn = meta_read(tmpRatio);
    double scaleFactor = 1.0/(scale/metaIn->general->x_pixel_size);
    meta_free(metaIn);
    sprintf(tmpIn, "%s%cred.img", tmpDir, DIR_SEPARATOR);
    resample(infile1, tmpIn, scaleFactor, scaleFactor);
    sprintf(tmpRed, "%s%cred_byte.img", tmpDir, DIR_SEPARATOR);
    floats_to_bytes_from_file(tmpIn, tmpRed, NULL, 0.0, SIGMA);
    sprintf(tmpIn, "%s%cgreen.img", tmpDir, DIR_SEPARATOR);
    resample(infile2, tmpIn, scaleFactor, scaleFactor);
    sprintf(tmpGreen, "%s%cgreen_byte.img", tmpDir, DIR_SEPARATOR);
    floats_to_bytes_from_file(tmpIn, tmpGreen, NULL, 0.0, SIGMA);
    sprintf(tmpIn, "%s%cblue.img", tmpDir, DIR_SEPARATOR);
    resample(tmpRatio, tmpIn, scaleFactor, scaleFactor);    
    sprintf(tmpBlue, "%s%cblue_byte.img", tmpDir, DIR_SEPARATOR);
    floats_to_bytes_from_file(tmpIn, tmpBlue, NULL, 0.0, SIGMA);

    // Layer stack the bands
    char tmpBrowse[512];
    sprintf(tmpBrowse, "%s%cbrowse.img", tmpDir, DIR_SEPARATOR);
    FILE *fpOut = FOPEN(tmpBrowse, "w");    
    meta_parameters *metaOut = meta_read(tmpRed);
    metaOut->general->band_count = 3;
    metaIn = meta_read(tmpRed);
    int line_count = metaIn->general->line_count;
    int sample_count = metaIn->general->sample_count;
    
    float *buf = (float *) MALLOC(sizeof(float)*line_count*sample_count);
    FILE *fpIn = FOPEN(tmpBlue, "r");
    get_float_lines(fpIn, metaIn, 0, line_count, buf);
    put_band_float_lines(fpOut, metaOut, 0, 0, line_count, buf);
    FCLOSE(fpIn);
    fpIn = FOPEN(tmpGreen, "r");
    get_float_lines(fpIn, metaIn, 0, line_count, buf);
    put_band_float_lines(fpOut, metaOut, 1, 0, line_count, buf);
    FCLOSE(fpIn);
    fpIn = FOPEN(tmpRed, "r");
    get_float_lines(fpIn, metaIn, 0, line_count, buf);
    put_band_float_lines(fpOut, metaOut, 2, 0, line_count, buf);
    FCLOSE(fpIn);
    FCLOSE(fpOut);
    FREE(buf);

    strcpy(metaOut->general->bands, "red,green,blue");
    meta_write(metaOut, tmpBrowse);
    
    // Export to GeoTIFF
    char *band_names[3] = { "blue", "green", "red" };
    asf_export_bands(GEOTIFF, TRUNCATE , TRUE, FALSE, FALSE, FALSE, FALSE, 
      tmpBrowse, outfile, band_names, NULL, NULL);

    // Clean up
    asfPrintStatus("Removing temporary directory: %s\n", tmpDir);
    remove_dir(tmpDir);
    meta_free(metaIn);
    meta_free(metaOut);
  }
  else
    asfPrintError("Mode is not defined!\n");

  asfPrintStatus("Done.\n");
  exit(EXIT_SUCCESS);
}

unsigned char *my_floats_to_bytes (float *data, long long pixel_count, float mask,
				scale_t scaling,float inmin, float inmax)
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

