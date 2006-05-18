#include "asf.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "asf_reporting.h"
#include "asf_raster.h"
#include "libasf_proj.h"
#include "proj.h"

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [ -refine ] [ -polynomial <order> ] [ -log <logfile> ]\n"
	 "      <sarName> <mapDemName> <offset> <simAmpName> [ <sarDemName> ]\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   <sarName>        SAR image\n"
	 "   <mapDemName>     map projected reference DEM\n"
	 "   <offset>         offset file\n"
	 "   <simAmpName>     simulated amplitude image\n"
	 "   All filenames require extensions.\n");
  printf("\n"
	 "OPTIONS:\n"
	 "   -refine               applies the offset determined to improve the\n"
	 "                         the geolocation.\n"
	 "   -polynomial <order>   uses a polynomial fit of given order to\n"
	 "                         reproject DEM into SAR geometry (only applies\n"
	 "                         to non-projected imagea).\n"
	 "   <sarDemName>          reference DEM in SAR geometry (only applies to\n"
	 "                         non-projected images\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   For images in a SAR geometry (ground range or slant range) it reprojects\n"
	 "   a DEM and simulates an amplitude image into SAR \n"
	 "   geometry using a map projected reference DEM. The mapping can be done\n"
	 "   using a polynomial or a least-square (default) fitting approach.\n"
	 "   For map projected SAR images it simulates an amplitude image for the\n"
	 "   reference DEM in its map projection.\n"
	 "   Matching the real and simulated amplitude image \n"
	 "   in frequency domain is used to determine the offsets to correct\n"
	 "   the geolocation.\n");
  printf("\n"
	 "Version %.2f, ASF SAR TOOLS\n"
	 "\n",VERSION);
  exit(1);
}


int main(int argc, char *argv[])
{
  char sarName[255]="", mapDemName[255]="", sarDemName[255]="", simAmpName[255]="";
  char gridName[255]="", sarByteName[255]="", simByteName[255]="", cmd[255];
  char option[25]="-least-square", offsetName[255]="", polyName[25]="";
  char sarRefinedName[255]="", line;
  int ii, nSamples, nLines, order, refine_flag=0;
  double min, max, mean, stdDev;
  double azimuth_offset, range_offset, time_offset, elev;
  float *float_data;
  unsigned char *byte_data;
  meta_parameters *metaSAR, *metaDEM;
  FILE *fpIn, *fpOut;

  currArg=1;      /* from cla.h in asf.h, points to current argv string */
  
  printf("%s\n",date_time_stamp());
  printf("Program: asf_check_geolocation\n\n");  

  // Parse command line
  logflag = FALSE;
  quietflag = TRUE;
  while (currArg < (argc-5)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-refine")) {
      refine_flag=TRUE;
    }
    else if (strmatch(key,"-polynomial")) {
      sprintf(option, "-polynomial");
      CHECK_ARG(1);
      order = atoi(GET_ARG(1));
    }
    else if (strmatch(key,"-log")) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag=TRUE;
    }
    else if (strmatch(key,"-quiet")) {
      quietflag=TRUE;
    }
    else {printf( "\n   **Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }
  if ((argc-currArg) < 5) {printf("   Insufficient arguments.\n"); usage(argv[0]);}

  // Fetch required arguments
  strcpy(sarName, argv[currArg++]);
  strcpy(mapDemName, argv[currArg++]);
  strcpy(offsetName, argv[currArg++]);
  strcpy(simAmpName, argv[currArg++]);
  metaSAR = meta_read(sarName);
  if (metaSAR->sar->image_type=='S' || metaSAR->sar->image_type=='G')
    strcpy(sarDemName, argv[currArg]);

  /* There is scope to generalize this tool to allow the ingest of ortho-rectified
     optical data and the works. The principle remains the same, getting some data
     from a map projection into the SAR geometry. */
  
  // Image in slant range or ground range geometry
  if (metaSAR->sar->image_type=='S' || metaSAR->sar->image_type=='G') {

    if (metaSAR->sar->image_type=='S') {
      printf("   Detected slant range SAR image\n");
      if (logflag)
	printLog("   Detected slant range SAR image\n");
    }
    else if (metaSAR->sar->image_type=='G') {
      printf("   Detected ground range SAR image\n");
      if (logflag)
	printLog("   Detected ground range SAR image\n");
    }

    // Convert slant/ground range image to byte.
    nSamples = metaSAR->general->sample_count;
    nLines = metaSAR->general->line_count;
    create_name(sarByteName, sarName, "_byte.img");
    float_data = (float *) MALLOC(sizeof(float)*nLines*nSamples);
    byte_data = (unsigned char *) MALLOC(sizeof(char)*nLines*nSamples);
    fpIn = FOPEN(sarName,"rb");
    fpOut = FOPEN(sarByteName, "wb");
    get_float_lines(fpIn, metaSAR, 0, nLines, float_data);
    /* Chose the two sigma method (SIGMA) for conversion to byte because the minimum/
       maximum stretch (MINMAX) sometimes leads to dark images. Interestingly, the
       confidence level of sigma versus minmax for the test case was lower. -RG */
    byte_data = floats_to_bytes(float_data, nLines*nSamples, NAN, SIGMA);
    FWRITE(byte_data, 1, nLines*nSamples, fpOut);
    FCLOSE(fpIn);
    FCLOSE(fpOut);
    FREE(float_data);
    FREE(byte_data);
    metaSAR->general->data_type = BYTE;
    meta_write(metaSAR, sarByteName);
    
    // Create a grid of points to map DEM into slant range
    if (metaSAR->general->average_height)    
      elev = metaSAR->general->average_height;
    else
      elev = 0.0;
    meta_free(metaSAR);
    create_name(gridName, mapDemName, "_initial.grid");
    create_dem_grid(mapDemName, sarByteName, gridName, elev);
    
    if (strcmp(option, "-polynomial")==0) {
      // Generate polynomial mapping function for remap
      sprintf(polyName, "poly.%i", order); 
      fit_poly(gridName, order, polyName);
      // Cut out the DEM subset we need
      sprintf(cmd, "remap -width %i -height %i -poly %s -float %s dem_big.dem", 
	      nSamples, nLines, polyName, mapDemName);
      system(cmd);
    }
    else if (strcmp(option, "-least-square")==0) {
      // Generate least-square mapping function for reamp
      fit_plane(gridName, "dem_plane");
      // Cut out the DEM subset we need
      sprintf(cmd, "remap -width %i -height %i -matrix dem_plane -float %s dem_big.dem",
	      nSamples, nLines, mapDemName);
      system(cmd);
    }
    else 
      asfPrintError("   Fitting option '%s' not supported!", option);
    
    // Simulate an amplitude image
    printf("   Simulating amplitude image for DEM\n");
    if (logflag)
      printLog("   Simulating amplitude image for DEM\n");
    reskew_dem(sarName, "dem_big.dem", sarDemName, simAmpName);

    // Convert simulated amplitude image to byte
    metaSAR = meta_read(sarName);
    create_name(simByteName, simAmpName, "_byte.img");
    float_data = (float *) MALLOC(sizeof(float)*nLines*nSamples);
    byte_data = (unsigned char *) MALLOC(sizeof(char)*nLines*nSamples);
    fpIn = FOPEN(simAmpName,"rb");
    fpOut = FOPEN(simByteName, "wb");
    get_float_lines(fpIn, metaSAR, 0, nLines, float_data);
    // Same as initial byte conversion. MINMAX is the alternative.
    byte_data = floats_to_bytes(float_data, nLines*nSamples, NAN, SIGMA);
    FWRITE(byte_data, 1, nLines*nSamples, fpOut);
    FCLOSE(fpIn);
    FCLOSE(fpOut);
    FREE(float_data);
    FREE(byte_data);
    metaSAR->general->data_type = BYTE;
    meta_write(metaSAR, simByteName);
    meta_free(metaSAR);
    
    // Determine initial offset 
    printf("\n   Determining initial offset\n");
    if (logflag)
      printLog("\n   Determining initial offset\n");
    fftMatch_withOffsetFile(sarByteName, simByteName, NULL, offsetName);

    // Applying height correction
    printf("\n   Applying height correction\n");
    metaSAR = meta_read(sarName);
    float_data = (float *) MALLOC(sizeof(float)*nLines*nSamples);
    fpIn = FOPEN("dem_big.dem","rb");
    get_float_lines(fpIn, metaSAR, 0, nLines, float_data);
    calc_stats(float_data, nLines*nSamples, 0.0, &min, &max, &mean, &stdDev);
    printf("   Initial average height: %.1lf\n", mean);
    FCLOSE(fpIn);
    FREE(float_data);
    meta_free(metaSAR);
    create_name(gridName, mapDemName, "_final.grid");
    // Create a new grid corrected with the average height out of the intial test
    create_dem_grid(mapDemName, sarByteName, gridName, -mean);

    if (strcmp(option, "-polynomial")==0) {
      // Generate polynomial mapping function for remap
      sprintf(polyName, "poly.%i", order); 
      fit_poly(gridName, order, polyName);
      // Cut out the DEM subset we need
      sprintf(cmd, "remap -width %i -height %i -poly %s -float %s dem_big.dem", 
	      nSamples, nLines, polyName, mapDemName);
      system(cmd);
    }
    else if (strcmp(option, "-least-square")==0) {
      // Generate least-square mapping function for reamp
      fit_plane(gridName, "dem_plane");
      // Cut out the DEM subset we need
      sprintf(cmd, "remap -width %i -height %i -matrix dem_plane -float %s dem_big.dem",
	      nSamples, nLines, mapDemName);
      system(cmd);
    }
    else 
      asfPrintError("   Fitting option '%s' not supported!", option);

    // Simulate an amplitude image
    printf("   Simulating amplitude image for DEM\n");
    if (logflag)
      printLog("   Simulating amplitude image for DEM\n");
    reskew_dem(sarName, "dem_big.dem", sarDemName, simAmpName);

    // Convert simulated amplitude image to byte
    metaSAR = meta_read(sarName);
    create_name(simByteName, simAmpName, "_byte.img");
    float_data = (float *) MALLOC(sizeof(float)*nLines*nSamples);
    byte_data = (unsigned char *) MALLOC(sizeof(char)*nLines*nSamples);
    fpIn = FOPEN(simAmpName,"rb");
    fpOut = FOPEN(simByteName, "wb");
    get_float_lines(fpIn, metaSAR, 0, nLines, float_data);
    // Same as initial byte conversion. MINMAX is the alternative.
    byte_data = floats_to_bytes(float_data, nLines*nSamples, NAN, SIGMA);
    FWRITE(byte_data, 1, nLines*nSamples, fpOut);
    FCLOSE(fpIn);
    FCLOSE(fpOut);
    FREE(float_data);
    FREE(byte_data);
    metaSAR->general->data_type = BYTE;
    meta_write(metaSAR, simByteName);
    meta_free(metaSAR);
    
    // Determine final offset
    printf("\n   Determining final offset\n");
    if (logflag)
      printLog("\n   Determining final offset\n");
    fftMatch_withOffsetFile(sarByteName, simByteName, NULL, offsetName);

    // Determine height for metadata
    metaSAR = meta_read(sarName);
    float_data = (float *) MALLOC(sizeof(float)*nLines*nSamples);
    fpIn = FOPEN("dem_big.dem","rb");
    get_float_lines(fpIn, metaSAR, 0, nLines, float_data);
    calc_stats(float_data, nLines*nSamples, 0.0, &min, &max, &mean, &stdDev);
    printf("   Adjusted average height: %.1lf\n", mean);
    FCLOSE(fpIn);
    FREE(float_data);
    metaSAR->general->average_height = ((int)(mean*10))/10.0;
    meta_write(metaSAR, sarName);

    // Refine geolocation
    if (refine_flag) {
      printf(logbuf, "   Refining geolocation of '%s'\n", sarName);
      if (logflag) {
	sprintf(logbuf, "   Refining geolocation of '%s'\n", sarName);
	printLog(logbuf);
      }
      
      // Read offset file
      fpIn = FOPEN(offsetName,"r");
      fscanf(fpIn, "%lf %lf", &range_offset, &azimuth_offset);
      FCLOSE(fpIn);

      // Determine time offset
      metaSAR = meta_read(sarName);
      create_name(sarRefinedName, sarName, "_refined.img");
      sprintf(cmd, "ln -s %s %s", sarName, sarRefinedName);
      system(cmd);
      meta_write(metaSAR, sarRefinedName);
      time_offset = azimuth_offset * metaSAR->sar->azimuth_time_per_pixel;
      metaSAR->sar->time_shift -= time_offset;
      metaSAR->sar->slant_shift = range_offset * metaSAR->general->x_pixel_size;
      meta_write(metaSAR, sarName);
      printf("   Time offset: %.12lf\n\n", time_offset);
    }
  }

  // Map projected images
  if (metaSAR->sar->image_type=='P') {

    printf("   Detected map projected SAR image\n");
    if (logflag)
      printLog("   Detected map projected SAR image\n");

    // Simulate an amplitude image
    printf("   Simulating amplitude image for DEM\n");
    if (logflag)
      printLog("   Simulating amplitude image for DEM\n");


    // Convert simulated amplitude image to byte
    nSamples = metaSAR->general->sample_count;
    nLines = metaSAR->general->line_count;
    create_name(sarByteName, sarName, "_byte.img");
    float_data = (float *) MALLOC(sizeof(float)*nLines*nSamples);
    byte_data = (unsigned char *) MALLOC(sizeof(char)*nLines*nSamples);
    fpIn = FOPEN(sarName,"rb");
    fpOut = FOPEN(sarByteName, "wb");
    get_float_lines(fpIn, metaSAR, 0, nLines, float_data);
    /* Chose the two sigma method (SIGMA) for conversion to byte because the minimum/
       maximum stretch (MINMAX) sometimes leads to dark images. Interestingly, the
       confidence level of sigma versus minmax for the test case was lower. -RG */
    byte_data = floats_to_bytes(float_data, nLines*nSamples, NAN, SIGMA);
    FWRITE(byte_data, 1, nLines*nSamples, fpOut);
    FCLOSE(fpIn);
    FCLOSE(fpOut);
    FREE(float_data);
    FREE(byte_data);
    metaSAR->general->data_type = BYTE;
    meta_write(metaSAR, sarByteName);
    meta_free(metaSAR);

    // Determine the offset
    printf("   Determining offset\n");
    printLog("   Determining offset\n");
    fftMatch_withOffsetFile(sarByteName, simByteName, NULL, offsetName);

    // Refine geolocation
    if (refine_flag) {
      printf("\n   Refining geolocation of '%s'\n", sarName);
      if (logflag) {
	sprintf(logbuf, "   Refining geolocation of '%s'\n", sarName);
	printLog(logbuf);
      }
      
      // Read offset file
      fpIn = FOPEN(offsetName,"r");
      fscanf(fpIn, "%lf %lf", &range_offset, &azimuth_offset);
      FCLOSE(fpIn);

      // Apply offsets by shifting the SAR image to DEM location
      create_name(sarRefinedName, sarName, "_refined.img");
      sprintf(cmd, "remap -translate %.4f %.4f -sameSize %s %s", -range_offset, 
	      -azimuth_offset, sarName, sarRefinedName);
      system(cmd);
    }
  }
    meta_free(metaSAR);
  exit(0);
}
