/******************************************************************************
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

#include "asf.h"
#include "asf_nan.h"
#include "asf_endian.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_export.h"
#include "asf_license.h"
#include "dateUtil.h"

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [ -log <logFile> ] <measures> <netcdf>\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   measures   list of MEaSUREs data files\n"
	 "   netcdf     time series data\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s converts RGPS MEaSUREs gridded files into a netCDF file.\n"
	 "   The tool assumes that parameters in the list are covered for all "
	 "time steps in the collection.\n",
	 name);
  printf("\n"
	 "Version %.2f, ASF SAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  FILE *fpIn, *fpOut, *fpMask, *fpInList, *fpOutList;
  extern int currArg; /* from cla.h in asf.h... initialized to 1 */
  logflag = 0;
  
  // Parse command line args
  while (currArg < (argc-2)) {
    char *key=argv[currArg++];
    if (strmatch(key,"-log")) {
      sprintf(logFile, "%s", argv[currArg]);
      logflag = 1;
    }
    else {
      printf("\n   ***Invalid option:  %s\n\n",
	     argv[currArg-1]);
      usage(argv[0]);
    }
  }
  if ((argc-currArg) < 2) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }
  
  asfSplashScreen(argc, argv);
  
  char *listInFile = (char *) MALLOC(sizeof(char)*(strlen(argv[1])+1));
  strcpy(listInFile, argv[1]);
  char *outFile = (char *) MALLOC(sizeof(char)*(strlen(argv[2])+1));
  strcpy(outFile, argv[2]);
  
  // Setup file names
  char *tmpDir = (char *) MALLOC(sizeof(char)*512);
  sprintf(tmpDir, "measures-");
  strcat(tmpDir, time_stamp_dir());
  create_clean_dir(tmpDir);  
  char *inFile = (char *) MALLOC(sizeof(char)*512);
  char *imgFile = (char *) MALLOC(sizeof(char)*768);
  char *metaFile = (char *) MALLOC(sizeof(char)*768);
  char *maskFile = (char *) MALLOC(sizeof(char)*768);
  sprintf(maskFile, "%s%cwater_mask.img", tmpDir, DIR_SEPARATOR);
  char *listOutFile = (char *) MALLOC(sizeof(char)*768);
  sprintf(listOutFile, "%s%clist", tmpDir, DIR_SEPARATOR);

  // Read header information
  char header[109], *ext;
  float x_pix, y_pix, x_map_ll, y_map_ll, x_map_ur, y_map_ur, cat;
  int ii, kk, ll, nFiles=0, num, size, sample_count, line_count;
  image_data_type_t image_data_type;
  int ageFlag = FALSE, thkFlag = FALSE, bshFlag = FALSE, myrFlag = FALSE;
  int divFlag = FALSE, vrtFlag = FALSE, shrFlag = FALSE;
  int nAge = 0, nThk = 0, nBsh = 0, nMyr = 0, nDiv = 0, nVrt = 0, nShr = 0;
  int bAge = 0, bThk = 0, bBsh = 0, bMyr = 0, bDiv = 0, bVrt = 0, bShr = 0;

  fpInList = FOPEN(listInFile, "r");
  fpOutList = FOPEN(listOutFile, "w");
  while (fgets(inFile, 512, fpInList)) {

    num = 1;
    chomp(inFile);
    sprintf(imgFile, "%s%cimage_%d.img", tmpDir, DIR_SEPARATOR, nFiles);
    sprintf(metaFile, "%s%cimage_%d.meta", tmpDir, DIR_SEPARATOR, nFiles);

    // Check extension
    ii = strlen(inFile) - 1;
    while (inFile[ii] != '.')
      ii--;
    ext = (char *) &inFile[ii];
    if (strcmp_case(ext, ".AGE") == 0 ||
	    strcmp_case(ext, ".THK") == 0)
      size = 109;
    else if (strcmp_case(ext, ".BSH") == 0)
      size = 104;
    else
      size = 96;
    
    // Read header file
    fpIn = FOPEN(inFile, "r");
    FREAD(header, size, 1, fpIn);
    
    if (size == 109) {
      sscanf(header, "%f %f %f %f %f %f %f %d %d", &x_pix, &y_pix, &x_map_ll, 
	     &y_map_ll, &x_map_ur, &y_map_ur, &cat, &sample_count, &line_count);
      num = (int) cat;
      /*
	printf("x_pix: %f, y_pix: %f\n", x_pix, y_pix);
	printf("x_map_ll: %f, y_map_ll: %f\n", x_map_ll, y_map_ll);
	printf("x_map_ur: %f, y_map_ur: %f\n", x_map_ur, y_map_ur);
	printf("num: %f, sample_count: %d, line_count: %d\n",
	cat, sample_count, line_count);
      */
    }
    else if (size == 104) {
      sscanf(header, "%f %f %f %f %f %f %d %d %d", &x_pix, &y_pix, &x_map_ll, 
	     &y_map_ll, &x_map_ur, &y_map_ur, &num, &sample_count, &line_count);
      /*
	printf("x_pix: %f, y_pix: %f\n", x_pix, y_pix);
	printf("x_map_ll: %f, y_map_ll: %f\n", x_map_ll, y_map_ll);
	printf("x_map_ur: %f, y_map_ur: %f\n", x_map_ur, y_map_ur);
	printf("num: %d, sample_count: %d, line_count: %d\n",
	num, sample_count, line_count);
      */
    }
    else if (size == 96) {
      sscanf(header, "%f %f %f %f %f %f %d %d", &x_pix, &y_pix, &x_map_ll, 
	     &y_map_ll, &x_map_ur, &y_map_ur, &sample_count, &line_count);
      /*
	printf("x_pix: %f, y_pix: %f\n", x_pix, y_pix);
	printf("x_map_ll: %f, y_map_ll: %f\n", x_map_ll, y_map_ll);
	printf("x_map_ur: %f, y_map_ur: %f\n", x_map_ur, y_map_ur);
	printf("sample_count: %d, line_count: %d\n", sample_count, line_count);
      */
    }

    // Generate some rudimentary metadata
    project_parameters_t pps;
    projection_type_t proj_type;
    datum_type_t datum;
    spheroid_type_t spheroid;
    meta_parameters *meta = raw_init();
    meta->general->line_count = line_count;
    meta->general->sample_count = sample_count;
    meta->general->band_count = num;
    meta->general->data_type = REAL32;
    if (strcmp_case(ext, ".AGE") == 0) {
      image_data_type = ICE_AGE;
      ageFlag = TRUE;
      nAge++;
      bAge = num;
    }
    else if (strcmp_case(ext, ".THK") == 0) {
      image_data_type = ICE_THICKNESS;
      thkFlag = TRUE;
      nThk++;
      bThk = num;
    }
    else if (strcmp_case(ext, ".BSH") == 0) {
      image_data_type = BACKSCATTER_HISTOGRAM;
      bshFlag = TRUE;
      nBsh++;
      bBsh = num;
    }
    else if (strcmp_case(ext, ".MYR") == 0) {
      image_data_type = MULTIYEAR_ICE_FRACTION;
      myrFlag = TRUE;
      nMyr++;
      bMyr = num;
    }
    else if (strcmp_case(ext, ".DIV") == 0) {
      image_data_type = DIVERGENCE;
      divFlag = TRUE;
      nDiv++;
      bDiv = num;
    }
    else if (strcmp_case(ext, ".VRT") == 0) {
      image_data_type = VORTICITY;
      vrtFlag = TRUE;
      nVrt++;
      bVrt = num;
    }
    else if (strcmp_case(ext, ".SHR") == 0) {
      image_data_type = SHEAR;
      shrFlag = TRUE;
      nShr++;
      bShr = num;
    }
    meta->general->image_data_type = image_data_type;
    strcpy(meta->general->basename, inFile);
    inFile[ii] = '\0';
    meta->general->x_pixel_size = x_pix*1000.0;
    meta->general->y_pixel_size = y_pix*1000.0;
    meta->general->start_line = 0;
    meta->general->start_sample = 0;
    meta->general->no_data = MAGIC_UNSET_DOUBLE;
    strcpy(meta->general->sensor, "RGPS MEaSUREs");
    sprintf(meta->general->bands, "%s", 
	    lc(image_data_type2str(meta->general->image_data_type)));
    sprintf(meta->general->acquisition_date, "%s", inFile);
    
    // Sort out map projection
    read_proj_file("polar_stereographic_north_ssmi.proj", 
		   &pps, &proj_type, &datum, &spheroid);
    pps.ps.false_easting = 0.0;
    pps.ps.false_northing = 0.0;
    meta_projection *proj = meta_projection_init();
    proj->type = proj_type;
    proj->datum = HUGHES_DATUM;
    proj->spheroid = spheroid;
    proj->param = pps;
    strcpy(proj->units, "meters");
    proj->hem = 'N';
    spheroid_axes_lengths(spheroid, &proj->re_major, &proj->re_minor);
    proj->startX = x_map_ll*1000.0 - x_pix*1000.0;
    proj->startY = y_map_ur*1000.0 - y_pix*1000.0;
    proj->perX = x_pix*1000.0;
    proj->perY = -y_pix*1000.0;
    meta->projection = proj;
    meta_write(meta, metaFile);
    meta_parameters *metaByte = meta_read(metaFile);
    metaByte->general->data_type = ASF_BYTE;
    strcpy(metaByte->general->bands, "water mask");
    sprintf(metaFile, "%s%cwater_mask.meta", tmpDir, DIR_SEPARATOR);
    meta_write(metaByte, metaFile);  
    sprintf(metaFile, "%s%cimage_%d.meta", tmpDir, DIR_SEPARATOR, nFiles);
    
    float *floatBuf = (float *) MALLOC(sizeof(float)*sample_count*num);
    float *floatBand = (float *) MALLOC(sizeof(float)*sample_count);
    float *maskBuf = (float *) MALLOC(sizeof(float)*sample_count);
    
    // Write gridded data to ASF internal format
    // Write separate water mask file
    fpOut = FOPEN(imgFile, "wb");
    fpMask = FOPEN(maskFile, "wb");
    for (ii=0; ii<line_count; ii++) {
      for (kk=0; kk<sample_count*num; kk++) {
	      FREAD(&floatBuf[kk], sizeof(float), 1, fpIn);
	      ieee_big32(floatBuf[kk]);
      }
      for (ll=0; ll<num; ll++) {
        for (kk=0; kk<sample_count; kk++) {
          if (floatBuf[kk*num+ll] < 10000000000.0) {
            floatBand[kk] = floatBuf[kk*num+ll];
            maskBuf[kk] = 1.0;
          }
          else if (floatBuf[kk*num+ll] > 10000000000.0) {
            maskBuf[kk] = 1.0;
            floatBand[kk] = MAGIC_UNSET_DOUBLE;
          }
          else {
            maskBuf[kk] = 0.0;
            floatBand[kk] = MAGIC_UNSET_DOUBLE;
          }
        }
        put_band_float_line(fpOut, meta, ll, line_count-ii-1, floatBand);
        if (ll == 0)
          put_float_line(fpMask, metaByte, line_count-ii-1, maskBuf);
      }
    }
    FCLOSE(fpOut);
    FCLOSE(fpMask);
    fprintf(fpOutList, "%s\n", imgFile);
    meta_free(meta);
    meta_free(metaByte);
    nFiles++;
  }
  fprintf(fpOutList, "%s\n", maskFile);
  FCLOSE(fpInList);
  FCLOSE(fpOutList);
  int band_count = ageFlag + thkFlag + bshFlag + myrFlag + divFlag + vrtFlag;
  band_count += shrFlag;

  // Define bands
  band_t *band = (band_t *) MALLOC(sizeof(band_t)*(band_count+7));
  
  // RGPS MEaSUREs products
  for (ii=0; ii<band_count; ii++) {
    if (ageFlag) {
      strcpy(band[ii].name, "ice_age");
      ageFlag = FALSE;
    }
    else if (thkFlag) {
      strcpy(band[ii].name, "ice_thickness");
      thkFlag = FALSE;
    }
    else if (bshFlag) {
      strcpy(band[ii].name, "backscatter_histogram");
      bshFlag = FALSE;
    }
    else if (myrFlag) {
      strcpy(band[ii].name, "multiyear_ice_fraction");
      myrFlag = FALSE;
    }
    else if (divFlag) {
      strcpy(band[ii].name, "divergence");
      divFlag = FALSE;
    }
    else if (vrtFlag) {
      strcpy(band[ii].name, "vorticity");
      vrtFlag = FALSE;
    }
    else if (shrFlag) {
      strcpy(band[ii].name, "shear");
      shrFlag = FALSE;
    }
    strcpy(band[ii].axis, "");
    sprintf(band[ii].cell_methods, "area: %s value", band[ii].name);
    strcpy(band[ii].coordinates, "longitude latitude");
    strcpy(band[ii].grid_mapping, "projection");
    sprintf(band[ii].long_name, "RGPS MEaSUREs %s", band[ii].name);
    strcpy(band[ii].references, "");
    strcpy(band[ii].standard_name, "");
    strcpy(band[ii].units, "1");
    strcpy(band[ii].units_description, "unitless");
    strcpy(band[ii].bounds, "");
    band[ii].fill_value = MAGIC_UNSET_DOUBLE;
    if (strcmp_case(band[ii].name, "backscatter_histogram") == 0) {
      band[ii].valid_range[0] = 0.0;
      band[ii].valid_range[1] = 1000.0;
      band[ii].time_count = nBsh;
      band[ii].cat_count = bBsh;
    }
    else if (strcmp_case(band[ii].name, "divergence") == 0) {
      band[ii].valid_range[0] = -0.04;
      band[ii].valid_range[1] = 0.04;
      band[ii].time_count = nDiv;
      band[ii].cat_count = bDiv;
    }
    else if (strcmp_case(band[ii].name, "vorticity") == 0) {
      band[ii].valid_range[0] = -0.06;
      band[ii].valid_range[1] = 0.06;
      band[ii].time_count = nVrt;
      band[ii].cat_count = bVrt;
    }
    else if (strcmp_case(band[ii].name, "shear") == 0) {
      band[ii].valid_range[0] = 0.0;
      band[ii].valid_range[1] = 0.08;
      band[ii].time_count = nShr;
      band[ii].cat_count = bShr;
    }
    else {
      if (strcmp_case(band[ii].name, "ice_age") == 0) {
	      band[ii].time_count = nAge;
	      band[ii].cat_count = bAge;
      }
      else if (strcmp_case(band[ii].name, "ice_thickness") == 0) {
	      band[ii].time_count = nThk;
	      band[ii].cat_count = bThk;
      }
      else if (strcmp_case(band[ii].name, "multiyear_ice_fraction") == 0) {
	      band[ii].time_count = nMyr;
	      band[ii].cat_count = bMyr;
      }
      band[ii].valid_range[0] = MAGIC_UNSET_DOUBLE;
      band[ii].valid_range[1] = MAGIC_UNSET_DOUBLE;
    }
    if (image_data_type == ICE_AGE ||
	    image_data_type == ICE_THICKNESS ||
	    image_data_type == BACKSCATTER_HISTOGRAM)
      band[ii].dim_count = 4;
    else
      band[ii].dim_count = 3;
    band[ii].datatype = NC_FLOAT;
  }

  // Water mask
  strcpy(band[band_count].name, "water_mask");
  strcpy(band[band_count].axis, "");
  strcpy(band[band_count].cell_methods, "");
  strcpy(band[band_count].coordinates, "longitude latitude");
  strcpy(band[band_count].grid_mapping, "projection");
  sprintf(band[band_count].long_name, "water mask for gridded products");
  strcpy(band[band_count].references, "");
  strcpy(band[band_count].standard_name, "");
  strcpy(band[band_count].units, "1");
  strcpy(band[band_count].units_description, "unitless");
  strcpy(band[band_count].bounds, "");
  band[band_count].fill_value = 0.0;
  band[band_count].valid_range[0] = MAGIC_UNSET_DOUBLE;
  band[band_count].valid_range[1] = MAGIC_UNSET_DOUBLE;
  band[band_count].time_count = 1;
  band[band_count].cat_count = 1;
  band[band_count].dim_count = 2;
  band[band_count].datatype = NC_INT;

  // Time
  strcpy(band[band_count+1].name, "time");
  strcpy(band[band_count+1].axis, "T");
  strcpy(band[band_count+1].cell_methods, "");
  strcpy(band[band_count+1].coordinates, "");
  strcpy(band[band_count+1].grid_mapping, "");
  strcpy(band[band_count+1].long_name, "serial date");
  strcpy(band[band_count+1].references, "center time of 3-day average");
  strcpy(band[band_count+1].standard_name, "time");
  strcpy(band[band_count+1].units, "seconds since 1995-01-01T00:00:00Z");
  strcpy(band[band_count+1].units_description, "");
  strcpy(band[band_count+1].bounds, "time_bnds");
  band[band_count+1].fill_value = 0.0;
  band[band_count+1].valid_range[0] = MAGIC_UNSET_DOUBLE;
  band[band_count+1].valid_range[1] = MAGIC_UNSET_DOUBLE;
  band[band_count+1].time_count = nFiles/band_count;
  band[band_count+1].cat_count = 1;
  band[band_count+1].dim_count = 1;
  band[band_count+1].datatype = NC_FLOAT;

  // Time bounds
  strcpy(band[band_count+2].name, "time_bnds");
  strcpy(band[band_count+2].axis, "");
  strcpy(band[band_count+2].cell_methods, "");
  strcpy(band[band_count+2].coordinates, "");
  strcpy(band[band_count+2].grid_mapping, "");
  strcpy(band[band_count+2].long_name, "serial date");
  strcpy(band[band_count+2].references, "start and end time of 3-day average");
  strcpy(band[band_count+2].standard_name, "time");
  strcpy(band[band_count+2].units, "seconds since 1995-01-01T00:00:00Z");
  strcpy(band[band_count+2].units_description, "");
  strcpy(band[band_count+2].bounds, "");
  band[band_count+2].fill_value = 0.0;
  band[band_count+2].valid_range[0] = MAGIC_UNSET_DOUBLE;
  band[band_count+2].valid_range[1] = MAGIC_UNSET_DOUBLE;
  band[band_count+2].time_count = nFiles/band_count;
  band[band_count+2].cat_count = 1;
  band[band_count+2].dim_count = 2;
  band[band_count+2].datatype = NC_FLOAT;

  // Longitude
  strcpy(band[band_count+3].name, "longitude");
  strcpy(band[band_count+3].axis, "");
  strcpy(band[band_count+3].cell_methods, "");
  strcpy(band[band_count+3].coordinates, "");
  strcpy(band[band_count+3].grid_mapping, "");
  strcpy(band[band_count+3].long_name, "longitude");
  strcpy(band[band_count+3].references, "");
  strcpy(band[band_count+3].standard_name, "longitude");
  strcpy(band[band_count+3].units, "degrees_east");
  strcpy(band[band_count+3].units_description, "");
  strcpy(band[band_count+3].bounds, "");
  band[band_count+3].fill_value = -999.0;
  band[band_count+3].valid_range[0] = -180.0;
  band[band_count+3].valid_range[1] = 180.0;
  band[band_count+3].time_count = 1;
  band[band_count+3].cat_count = 1;
  band[band_count+3].dim_count = 2;
  band[band_count+3].datatype = NC_FLOAT;

  // Latitude
  strcpy(band[band_count+4].name, "latitude");
  strcpy(band[band_count+4].axis, "");
  strcpy(band[band_count+4].cell_methods, "");
  strcpy(band[band_count+4].coordinates, "");
  strcpy(band[band_count+4].grid_mapping, "");
  strcpy(band[band_count+4].long_name, "latitude");
  strcpy(band[band_count+4].references, "");
  strcpy(band[band_count+4].standard_name, "latitude");
  strcpy(band[band_count+4].units, "degrees_north");
  strcpy(band[band_count+4].units_description, "");
  strcpy(band[band_count+4].bounds, "");
  band[band_count+4].fill_value = -999.0;
  band[band_count+4].valid_range[0] = -90.0;
  band[band_count+4].valid_range[1] = 90.0;
  band[band_count+4].time_count = 1;
  band[band_count+4].cat_count = 1;
  band[band_count+4].dim_count = 2;
  band[band_count+4].datatype = NC_FLOAT;

  // XGrid
  strcpy(band[band_count+5].name, "xgrid");
  strcpy(band[band_count+5].axis, "X");
  strcpy(band[band_count+5].cell_methods, "");
  strcpy(band[band_count+5].coordinates, "");
  strcpy(band[band_count+5].grid_mapping, "");
  strcpy(band[band_count+5].long_name, "projection_grid_x_center");
  strcpy(band[band_count+5].references, "");
  strcpy(band[band_count+5].standard_name, "projection_x_coordinate");
  strcpy(band[band_count+5].units, "meters");
  strcpy(band[band_count+5].units_description, "");
  strcpy(band[band_count+5].bounds, "");
  band[band_count+5].fill_value = MAGIC_UNSET_DOUBLE;
  band[band_count+5].valid_range[0] = MAGIC_UNSET_DOUBLE;
  band[band_count+5].valid_range[1] = MAGIC_UNSET_DOUBLE;
  band[band_count+5].time_count = 1;
  band[band_count+5].cat_count = 1;
  band[band_count+5].dim_count = 2;
  band[band_count+5].datatype = NC_FLOAT;

  // YGrid
  strcpy(band[band_count+6].name, "ygrid");
  strcpy(band[band_count+6].axis, "Y");
  strcpy(band[band_count+6].cell_methods, "");
  strcpy(band[band_count+6].coordinates, "");
  strcpy(band[band_count+6].grid_mapping, "");
  strcpy(band[band_count+6].long_name, "projection_grid_y_center");
  strcpy(band[band_count+6].references, "");
  strcpy(band[band_count+6].standard_name, "projection_y_coordinate");
  strcpy(band[band_count+6].units, "meters");
  strcpy(band[band_count+6].units_description, "");
  strcpy(band[band_count+6].bounds, "");
  band[band_count+6].fill_value = MAGIC_UNSET_DOUBLE;
  band[band_count+6].valid_range[0] = MAGIC_UNSET_DOUBLE;
  band[band_count+6].valid_range[1] = MAGIC_UNSET_DOUBLE;
  band[band_count+6].time_count = 1;
  band[band_count+6].cat_count = 1;
  band[band_count+6].dim_count = 2;
  band[band_count+6].datatype = NC_FLOAT;

  // Export to netCDF
  export_netcdf_list(listOutFile, band, band_count+7, outFile);

  // Clean up
  FREE(inFile);
  FREE(outFile);
  FREE(imgFile);
  FREE(metaFile);
  remove_dir(tmpDir);
  FREE(tmpDir);

  return 0;
}
