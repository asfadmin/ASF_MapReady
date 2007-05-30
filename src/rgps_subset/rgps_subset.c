#include "asf_import.h"
#include "asf_export.h"
#include "asf_raster.h"
#include "asf.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "envi.h"

#define VERSION 1.0

void usage()
{
  printf("\n"
   "USAGE:\n"
   "   rgps_subset [-startY <start line] [-startX <start sample>] "
    "[-lines <lines>]\n"
   "         [-samples <samples>] [-filter <type> <size>] [-geotiff]\n"
   "         <source image> <target image>\n");
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   sar_name   Name of input image file (including extension)\n");
  printf("\n"
   "OPTIONAL ARGUMENTS:\n"
   "   -startY     Start line of the subset in the source image.\n"
   "   -startX     Start sample of the subset in the source image.\n"
   "   -lines      Number of lines of the subset.\n"
   "   -samples    Number of samples of the subset.\n"
   "   -filter     Filter the subset for noise reduction:\n"
   "               AVERAGE - simple mean filter\n"
   "               SOBEL - edge detection filter\n"
   "               FROST, LEE, GAMMA_MAP - speckle reduction filters\n"
   "   -geotiff    Save the final result in GeoTIFF format\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program ingests an RGPS source and target image (CEOS format)\n"
   "   and adjusts the brightness of the subsets.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

static void read_subset(FILE *in, meta_parameters *meta,
			int startX, int startY, int samples, int lines,
			float add, float *mean, float *dest)
{
  int i;
  float sum;

  // Read subset
  get_partial_float_lines(in, meta, startY, lines, startX, samples, dest);

  // Determine mean value or adjust brightness
  for (i=0; i<lines*samples; i++) {
    if (mean == NULL)
      dest[i] += add;
    else {
      sum += dest[i];
      dest[i] += add;
    }
  }
  if (mean != NULL)
    *mean = sum / lines / samples;
}

static void filter_image(float *inbuf, float *outbuf, filter_type_t filter, 
			 int size, int inLines, int inSamples)
{
  float half = (size - 1) / 2;
  int ii, jj;

  // Set upper margin of image to input pixel values
  for (ii=0; ii<half; ii++)
    for (jj=0; jj<inSamples; jj++) 
      outbuf[ii*inSamples+jj] = 0.0;
  
  // Filtering the 'regular' lines
  for (ii=half; ii<inLines-half; ii++) {
    
    for (jj=0; jj<half; jj++) outbuf[jj] = 0.0;
    for (jj=half; jj<inSamples-half; jj++) 
      outbuf[ii*inSamples+jj] = kernel(filter, inbuf, inLines, inSamples, 
				       ii, jj, size, 1, 4);
    for (jj=inSamples-half; jj<inSamples; jj++) outbuf[jj] = 0.0;
    
  }
  
  // Set lower margin of image to input pixel values
  for (ii=inLines-half; ii<inLines; ii++)
    for (jj=0; jj<inSamples; jj++) 
      outbuf[ii*inSamples+jj] = 0.0;
}

int main(int argc, char **argv)
{
  FILE *fp;
  meta_parameters *metaSrc, *metaTrg;
  envi_header *envi;
  extern int currArg;          // Pre-initialized to 1
  radiometry_t radiometry=r_AMP;
  filter_type_t filter_type;
  char srcImage[255], trgImage[255], *inFile, outFile[255], filter_str[25];
  int startX_src, startY_src, startX_trg, startY_trg, lines, samples, size; 
  int subset=FALSE, filter=FALSE, geotiff=FALSE, line_count, sample_count;
  double lat_UL, lon_UL, lat_LR, lon_LR, yLine, xSample;
  float mean, scale;
  register float *img, *filtered_img=NULL;
  register int x, y, l;

  /* parse command line */
  logflag=quietflag=FALSE;
  while (currArg < (argc-2)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-startX")) {
      CHECK_ARG(1);
      startX_src = atoi(GET_ARG(1));
      subset = TRUE;
    }
    else if (strmatch(key,"-startY")) {
      CHECK_ARG(1);
      startY_src = atoi(GET_ARG(1));
      subset = TRUE;
    }
    else if (strmatch(key,"-lines")) {
      CHECK_ARG(1);
      lines = atoi(GET_ARG(1));
      subset = TRUE;
    }
    else if (strmatch(key,"-samples")) {
      CHECK_ARG(1);
      samples = atoi(GET_ARG(1));
      subset = TRUE;
    }
    else if (strmatch(key,"-filter")) {
      CHECK_ARG(2);
      strcpy(filter_str, GET_ARG(2));
      size = atoi(GET_ARG(1));
      filter = TRUE;
    }
    else if (strmatch(key,"-geotiff")) {
      geotiff = TRUE;
    }
    else {
      printf( "\n**Invalid option:  %s\n", argv[currArg-1]); 
      usage(argv[0]);
    }
  }

  if ((argc-currArg)<2) {printf("Insufficient arguments.\n"); usage(argv[0]);}

  strcpy (srcImage, argv[currArg]);
  strcpy (trgImage, argv[currArg+1]);

  asfSplashScreen(argc, argv);

  // Ingesting CEOS files into ASF internal format
  asfPrintStatus("Ingesting source image: %s ...\n", srcImage);
  asf_import(radiometry, FALSE, "CEOS", "", "geocoded_image", NULL, NULL,
	     -99.0, -99.0, NULL, NULL, NULL, NULL, srcImage, srcImage);
  metaSrc = meta_read(srcImage);

  asfPrintStatus("Ingesting target image: %s ...\n", trgImage);
  asf_import(radiometry, FALSE, "CEOS", "", "geocoded_image", NULL, NULL,
	     -99.0, -99.0, NULL, NULL, NULL, NULL, trgImage, trgImage);
  metaTrg = meta_read(trgImage);

  // Check subset values for source image
  line_count = metaSrc->general->line_count;
  sample_count = metaSrc->general->sample_count;
  if (subset) {
    if (startX_src < 0 || startX_src > sample_count)
      startX_src = 0;
    if (startY_src < 0 || startY_src > line_count)
      startY_src = 0;
    if (lines < 0 || (lines-startY_src) > line_count)
      lines = line_count - startY_src;
    if (samples < 0 || (samples-startX_src) > sample_count)
      samples = sample_count - startX_src;
  }
  else {
    startX_src = startY_src = 0;
    lines = line_count;
    samples = sample_count;
  }

  // Assign filter
  if (filter) {
    if (strcmp(uc(filter_str), "AVERAGE") == 0)
      filter_type = AVERAGE;
    else if (strcmp(uc(filter_str), "SOBEL") == 0)
      filter_type = SOBEL;
    else if (strcmp(uc(filter_str), "FROST") == 0)
      filter_type = FROST;
    else if (strcmp(uc(filter_str), "LEE") == 0)
      filter_type = LEE;
    else if (strcmp(uc(filter_str), "GAMMA_MAP") == 0)
      filter_type = GAMMA_MAP;
    else {
      asfPrintWarning("Unsupported filter type '%s'- ignoring the filter"
		      " settings\n", filter_str);
      filter = FALSE;
    }
    if (size%2 == 0 && filter_type != AVERAGE) {
      size--;
      asfPrintWarning("Filter kernel must have an odd number of lines and"
		      "samples!\n");
    }
  }

  // Allocate some memory for the subsets
  line_count = metaTrg->general->line_count;
  sample_count = metaTrg->general->sample_count;
  img = (float *) MALLOC(sizeof(float)*lines*samples);
  if (filter)
    filtered_img = (float *) MALLOC(sizeof(float)*lines*samples);

  // Determine geographic location of subset
  meta_get_latLon(metaSrc, startY_src, startX_src, 0.0, &lat_UL, &lon_UL);
  meta_get_latLon(metaSrc, startY_src+lines, startX_src+samples, 0.0, 
		  &lat_LR, &lon_LR);
  meta_get_lineSamp(metaTrg, lat_UL, lon_UL, 0.0, &yLine, &xSample);
  startX_trg = (int) (xSample + 0.5);
  startY_trg = (int) (yLine + 0.5);

  // READ IN SUBSETS
  // Read target image subset first to determine average brightness
  asfPrintStatus("\nGenerating subset for target image ...\n");
  asfPrintStatus("start line: %d, start sample: %d, lines: %d, samples: %d\n",
		 startY_trg, startX_trg, lines, samples);
  inFile = appendExt(trgImage, ".img");
  sprintf(outFile, "%s_sub.img", trgImage);
  fp = FOPEN(inFile, "rb");
  read_subset(fp, metaTrg, startX_trg, startY_trg, samples, lines, 0.0, 
	      &mean, img);
  FCLOSE(fp);

  // Compute scale factor
  mean *= -1;
  scale = 1.0 / (lines * samples);

  // Subtract this average off of target image
  for (y=0; y<lines; y++) {
    l = samples * y;
    for (x=0; x<samples; x++)
      img[l+x] = (img[l+x] + mean) * scale;
  }

  if (filter) {
    asfPrintStatus("\nFiltering target image subset with %s (%dx%d) ...\n", 
		   uc(filter_str), size, size);
    filter_image(img, filtered_img, filter_type, size, lines, samples);
  }

  // Update metadata and write target subset to file
  metaTrg->general->line_count = lines;
  metaTrg->general->sample_count = samples;
  metaTrg->general->start_line = startY_trg;
  metaTrg->general->start_sample = startX_trg;
  meta_write(metaTrg, outFile);
  envi = meta2envi(metaTrg);
  write_envi_header(outFile, metaTrg, envi);

  fp = FOPEN(outFile, "wb");
  if (filter)
    put_float_lines(fp, metaTrg, 0, lines, filtered_img);
  else
    put_float_lines(fp, metaTrg, 0, lines, img);
  FCLOSE(fp);

  // Read source image subset applying for brightness
  asfPrintStatus("\nGenerating subset for source image ...\n");
  asfPrintStatus("start line: %d, start sample: %d, lines: %d, samples: %d\n",
		 startY_src, startX_src, lines, samples);
  inFile = appendExt(srcImage, ".img");
  sprintf(outFile, "%s_sub.img", srcImage);
  fp = FOPEN(inFile, "rb");
  read_subset(fp, metaSrc, startX_src, startY_src, samples, lines, mean, 
	      NULL, img);
  FCLOSE(fp);

  if (filter) {
    asfPrintStatus("\nFiltering source image subset with %s (%dx%d) ...\n", 
		   uc(filter_str), size, size);
    filter_image(img, filtered_img, filter_type, size, lines, samples);
  }

  // Update metadata and write source subset to file
  metaSrc->general->line_count = lines;
  metaSrc->general->sample_count = samples;
  metaSrc->general->start_line = startY_src;
  metaSrc->general->start_sample = startX_src;
  meta_write(metaSrc, outFile);
  envi = meta2envi(metaSrc);
  write_envi_header(outFile, metaSrc, envi);

  fp = FOPEN(outFile, "wb");
  if (filter)
    put_float_lines(fp, metaSrc, 0, lines, filtered_img);
  else
    put_float_lines(fp, metaSrc, 0, lines, img);
  FCLOSE(fp);

  // Clean up
  FREE(img);
  meta_free(metaSrc);
  meta_free(metaTrg);

  // Exporting subsets to GeoTIFF
  if (geotiff) {
    output_format_t output_format=GEOTIFF;
    scale_t sample_mapping=SIGMA;

    asfPrintStatus("\nExporting source image subset to GeoTIFF ...\n");
    sprintf(outFile, "%s_sub", srcImage);
    asf_export(output_format, sample_mapping, outFile, outFile);
    asfPrintStatus("\nExporting target image subset to GeoTIFF ...\n");
    sprintf(outFile, "%s_sub", trgImage);
    asf_export(output_format, sample_mapping, outFile, outFile);
  }

  return (0);
}
