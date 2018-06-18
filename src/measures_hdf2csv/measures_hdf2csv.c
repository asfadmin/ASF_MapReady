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
#include "asf_license.h"
#include "dateUtil.h"
#include "time.h"
#include "spheroids.h"
#include "libasf_proj.h"
#include "hdf5.h"
#include <hdf5_hl.h>

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [ -log <logFile> ] <measures> <csv>\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   measures   MEaSUREs HDF data file\n"
	 "   csv        data dump\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s converts an RGPS MEaSUREs file into a csv file.\n",
	 name);
  printf("\n"
	 "Version %.2f, ASF SAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}

static float minValue(float x1, float x2, float x3, float x4)
{
  float min = x1 + 360.0;
  if ((x2+360.0) < min)
    min = x2 + 360.0;
  if ((x3+360.0) < min)
    min = x3 + 360.0;
  if ((x4+360.0) < min)
    min = x4 + 360.0;
  min -= 360.0;
  
  return min;
}

static float maxValue(float x1, float x2, float x3, float x4)
{
  float max = x1 + 360.0;
  if ((x2+360.0) > max)
    max = x2 + 360.0;
  if ((x3+360.0) > max)
    max = x3 + 360.0;
  if ((x4+360.0) > max)
    max = x4 + 360.0;
  max -= 360.0;
  
  return max;
}

static void rgps2iso_date(int year, double day, char *isoStr)
{
  julian_date jd;
  hms_time time;
  ymd_date date;
  
  jd.year = year;
  jd.jd = (int) day;
  date_jd2ymd(&jd, &date);
  double sec = 86400 * (day - jd.jd);
  date_sec2hms(sec, &time);
  sprintf(isoStr, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ",
        date.year, date.month, date.day, time.hour, time.min, time.sec);
}

void h5_att_str(hid_t file_id, char *group, char *name, char *value)
{
  int ii, kk;
  void *buf = NULL;
  hid_t attr_id;
  size_t data_size;
  H5O_info_t object_info;
  char *attr_name = (char *) MALLOC(sizeof(char)*50);

  H5Oget_info_by_name(file_id, group, &object_info, H5P_DEFAULT);
  for (ii=0; ii<object_info.num_attrs; ii++) {
    attr_id = H5Aopen_by_idx(file_id, group, H5_INDEX_NAME, H5_ITER_NATIVE, ii,
      H5P_DEFAULT, H5P_DEFAULT);
    H5Aget_name(attr_id, 50, attr_name);
    if (strcmp(group, "/") == 0) {
      char *global = strstr(attr_name, "_GLOSDS");
      if (global != NULL) {
        printf("triggered!\n");
        attr_name[strlen(attr_name)-7] = '\n';
      }
    }
    if (strcmp_case(name, attr_name) == 0) {
      hid_t attr_type = H5Aget_type(attr_id);
      H5T_class_t data_type = H5Tget_native_type(attr_type, H5T_DIR_DEFAULT);
      data_size = H5Tget_size(data_type);
      hid_t attr_space = H5Aget_space(attr_id);
      hsize_t dims[H5S_MAX_RANK];
      int rank = H5Sget_simple_extent_dims(attr_space, dims, NULL);
      hsize_t elements = 1;
      for (kk=0; kk<rank; kk++)
        elements *= dims[kk];
      buf = (void *) MALLOC((unsigned)(elements*data_size));
      H5Aread(attr_id, attr_type, buf);
      H5Tclose(attr_type);
      H5Tclose(data_type);
      H5Sclose(attr_space);
    }
    else {
      H5Aclose(attr_id);
      continue;
    }
    H5Aclose(attr_id);
  }
  if (buf) {
    strcpy(value, buf);
    value[(unsigned)data_size] = '\0';
    FREE(buf);
  }
  else
    strcpy(value, "???");
  FREE(attr_name);
}

void h5_att_int(hid_t file_id, char *group, char *name, int *nValue)
{
  char value[255];
  h5_att_str(file_id, group, name, value);
  *nValue = atoi(value);
}

void h5_att_float(hid_t file_id, char *group, char *name, float *fValue)
{
  char value[255];
  h5_att_str(file_id, group, name, value);
  *fValue = (float) atof(value);
}

void h5_att_double(hid_t file_id, char *group, char *name, double *fValue)
{
  char value[255];
  h5_att_str(file_id, group, name, value);
  *fValue = atof(value);
}

void h5_value_shorts(hid_t file_id, char *group, char *name, short int **values,
	int *numValues)
{
  int kk;
  hid_t group_id = H5Gopen(file_id, group, H5P_DEFAULT);
  hid_t data_id = H5Dopen(group_id, name, H5P_DEFAULT);
  hid_t data_space = H5Dget_space(data_id);
  int rank = H5Sget_simple_extent_ndims(data_space);
  hsize_t dims[H5S_MAX_RANK], maxdim[H5S_MAX_RANK];
  H5Sget_simple_extent_dims(data_space, dims, maxdim);
  hid_t data_type = H5Dget_type(data_id);
  H5T_class_t data_class = H5Tget_native_type(data_type, H5T_DIR_DEFAULT);
  size_t data_size = H5Tget_size(data_class);  
  hsize_t elements = 1;
  for (kk=0; kk<rank; kk++)
    elements *= dims[kk];
  void *buf = (void *) MALLOC((size_t)(elements*data_size));
  H5Dread(data_id, data_class, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
  *values = buf;
  *numValues = elements;
  H5Tclose(data_type);
  H5Tclose(data_class);
  H5Sclose(data_space);
  H5Dclose(data_id);
  H5Gclose(group_id); 
}

void h5_value_doubles(hid_t file_id, char *group, char *name, double **values,
	int *numValues)
{
  int kk;
  hid_t group_id = H5Gopen(file_id, group, H5P_DEFAULT);
  hid_t data_id = H5Dopen(group_id, name, H5P_DEFAULT);
  hid_t data_space = H5Dget_space(data_id);
  int rank = H5Sget_simple_extent_ndims(data_space);
  hsize_t dims[H5S_MAX_RANK], maxdim[H5S_MAX_RANK];
  H5Sget_simple_extent_dims(data_space, dims, maxdim);
  hid_t data_type = H5Dget_type(data_id);
  H5T_class_t data_class = H5Tget_native_type(data_type, H5T_DIR_DEFAULT);
  size_t data_size = H5Tget_size(data_class);  
  hsize_t elements = 1;
  for (kk=0; kk<rank; kk++)
    elements *= dims[kk];
  void *buf = (void *) MALLOC((size_t)(elements*data_size));
  H5Dread(data_id, data_class, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
  *values = buf;
  *numValues = elements;
  H5Tclose(data_type);
  H5Tclose(data_class);
  H5Sclose(data_space);
  H5Dclose(data_id);
  H5Gclose(group_id); 
}

int main(int argc, char **argv)
{
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
  
  char *inFile = (char *) MALLOC(sizeof(char)*(strlen(argv[1])+1));
  strcpy(inFile, argv[1]);
  char *outFile = (char *) MALLOC(sizeof(char)*(strlen(argv[2])+1));
  strcpy(outFile, argv[2]);

  char *path = (char *) MALLOC(sizeof(char)*strlen(outFile));
  char *csvFile = (char *) MALLOC(sizeof(char)*strlen(outFile));
  split_dir_and_file(outFile, path, csvFile);
  
  char isoStr[30], dateStr[30], citation[255], sw_version[25], description[50];
  char data_region[50], ref_id[30], source_image[50], target_image[50];
  char product_id[50];
  int create_year, source_year, target_year, num_grids;
  double create_time, source_time, target_time;
  float srcUL_lat, srcUL_lon, srcUR_lat, srcUR_lon, srcLL_lat, srcLL_lon;
  float srcLR_lat, srcLR_lon, trgUL_lat, trgUL_lon, trgUR_lat, trgUR_lon;
  float trgLL_lat, trgLL_lon, trgLR_lat, trgLR_lon, time_diff, pixel_size;
  float grid_spacing, avg_disp_x, avg_disp_y;

  // Open HDF file
  hid_t file_id = H5Fopen(inFile, H5F_ACC_RDONLY, H5P_DEFAULT);
  if (file_id < 0)
    asfPrintError("Cannot open file (%s)!\n", inFile);
    
  // Read global attributes
  h5_att_float(file_id, "/", "AVG_DISP_X(km):", &avg_disp_x);
  h5_att_float(file_id, "/", "AVG_DISP_Y(km):", &avg_disp_y);
  h5_att_double(file_id, "/", "CREATE_TIME:", &create_time);
  h5_att_int(file_id, "/", "CREATE_YEAR:", &create_year);
  h5_att_float(file_id, "/", "D_TIME between images(day):", &time_diff);
  h5_att_str(file_id, "/", "Data Region:", data_region);
  h5_att_float(file_id, "/", "Grid Spacing(km):", &grid_spacing);
  h5_att_str(file_id, "/", "IMV Tracking REQID:", ref_id);
  h5_att_float(file_id, "/", "Image Pixel Size(m):", &pixel_size);
  h5_att_int(file_id, "/", "NGRID With Obs:", &num_grids);
  h5_att_str(file_id, "/", "Product Description:", description);
  h5_att_str(file_id, "/", "Product Identifier:", product_id);
  h5_att_str(file_id, "/", "SW_VERSION:", sw_version);
  h5_att_double(file_id, "/", "Source Img Center Time:", &source_time);
  h5_att_str(file_id, "/", "Source Img ID:", source_image);
  h5_att_float(file_id, "/", "Source Img Lower left lat:", &srcLL_lat);
  h5_att_float(file_id, "/", "Source Img Lower left lon:", &srcLL_lon);
  h5_att_float(file_id, "/", "Source Img Lower right lat:", &srcLR_lat);
  h5_att_float(file_id, "/", "Source Img Lower right lon:", &srcLR_lon);
  h5_att_float(file_id, "/", "Source Img Upper left lat:", &srcUL_lat);
  h5_att_float(file_id, "/", "Source Img Upper left lon:", &srcUL_lon);
  h5_att_float(file_id, "/", "Source Img Upper right lat:", &srcUR_lat);
  h5_att_float(file_id, "/", "Source Img Upper right lon:", &srcUR_lon);
  h5_att_int(file_id, "/", "Source Img Year:", &source_year);
  h5_att_double(file_id, "/", "Target Img Center Time:", &target_time);
  h5_att_str(file_id, "/", "Target Img ID:", target_image);
  h5_att_float(file_id, "/", "Target Img Lower left lat:", &trgLL_lat);
  h5_att_float(file_id, "/", "Target Img Lower left lon:", &trgLL_lon);
  h5_att_float(file_id, "/", "Target Img Lower right lat:", &trgLR_lat);
  h5_att_float(file_id, "/", "Target Img Lower right lon:", &trgLR_lon);
  h5_att_float(file_id, "/", "Target Img Upper left lat:", &trgUL_lat);
  h5_att_float(file_id, "/", "Target Img Upper left lon:", &trgUL_lon);
  h5_att_float(file_id, "/", "Target Img Upper right lat:", &trgUR_lat);
  h5_att_float(file_id, "/", "Target Img Upper right lon:", &trgUR_lon);
  h5_att_int(file_id, "/", "Target Img Year:", &target_year);
  
  // Read data
  int ii, num_values;
  short int *grid_qfg;
  double *x_grid, *y_grid, *src_lat, *src_lon, *trg_lat, *trg_lon;
  h5_value_doubles(file_id, "/", "grid_dx(km)", &x_grid, &num_values);
  h5_value_doubles(file_id, "/", "grid_dy(km)", &y_grid, &num_values);
  h5_value_shorts(file_id, "/", "grid_qfg", &grid_qfg, &num_values);
  h5_value_doubles(file_id, "/", "src_grid_lat(deg)", &src_lat, &num_values);
  h5_value_doubles(file_id, "/", "src_grid_lon(deg)", &src_lon, &num_values);
  h5_value_doubles(file_id, "/", "trg_grid_lat(deg)", &trg_lat, &num_values);
  h5_value_doubles(file_id, "/", "trg_grid_lon(deg)", &trg_lon, &num_values);
  
  H5Fclose(file_id);

  // Write CSV file
  double x, y;
  char projFile[50];
  quietflag = TRUE;
  strcpy(projFile, "polar_stereographic_north_ssmi.proj");
  FILE *fp = FOPEN(outFile, "w");
  fprintf(fp, "source_lat,source_lon,source_x,source_y,target_lat,target_lon,"
    "target_x,target_y,x_grid,y_grid,quality_flag\n");
  for (ii=0; ii<num_values; ii++) {
    latLon2proj(src_lat[ii], src_lon[ii], 0.0, projFile, &x, &y);
    fprintf(fp, "%.4f,%.4f,%.4f,%.4f,", src_lat[ii], src_lon[ii], x, y);
    latLon2proj(trg_lat[ii], trg_lon[ii], 0.0, projFile, &x, &y);
    fprintf(fp, "%.4f,%.4f,%.4f,%.4f,%.3f,%.3f,%d\n", trg_lat[ii], trg_lon[ii],
      x, y, x_grid[ii]*1000.0, y_grid[ii]*1000.0, grid_qfg[ii]);
  }
  FCLOSE(fp);
  
  float srcMinLat = minValue(srcLL_lat, srcLR_lat, srcUL_lat, srcUR_lat);
  float srcMaxLat = maxValue(srcLL_lat, srcLR_lat, srcUL_lat, srcUR_lat);
  float srcMinLon = minValue(srcLL_lon, srcLR_lon, srcUL_lon, srcUR_lon);
  float srcMaxLon = maxValue(srcLL_lon, srcLR_lon, srcUL_lon, srcUR_lon);
  float trgMinLat = minValue(trgLL_lat, trgLR_lat, trgUL_lat, trgUR_lat);
  float trgMaxLat = maxValue(trgLL_lat, trgLR_lat, trgUL_lat, trgUR_lat);
  float trgMinLon = minValue(trgLL_lon, trgLR_lon, trgUL_lon, trgUR_lon);
  float trgMaxLon = maxValue(trgLL_lon, trgLR_lon, trgUL_lon, trgUR_lon);
  double westBoundLon = minValue(srcMinLon, srcMaxLon, trgMinLon, trgMaxLon);
  double eastBoundLon = maxValue(srcMinLon, srcMaxLon, trgMinLon, trgMaxLon);
  double northBoundLat = maxValue(srcMinLat, srcMaxLat, trgMinLat, trgMaxLat);
  double southBoundLat = minValue(srcMinLat, srcMaxLat, trgMinLat, trgMaxLat);
  
  // Generate the XML metadata
  sprintf(isoStr, "%s", iso_date());
  char *xmlFile = appendExt(outFile, ".xml");
  FILE *fpXml = FOPEN(xmlFile, "w");
  fprintf(fpXml, "<rgps>\n");
  fprintf(fpXml, "  <granule>%s</granule>\n", stripExt(product_id));
  fprintf(fpXml, "  <metadata_creation>%s</metadata_creation>\n", isoStr);
  fprintf(fpXml, "  <metadata>\n");
  fprintf(fpXml, "    <product>\n");
  fprintf(fpXml, "      <file type=\"string\" definition=\"product identifier\""
    ">%s</file>\n", product_id);
  fprintf(fpXml, "      <format type=\"string\" definition=\"name of the data "
    "format\">CSV</format>\n");
  fprintf(fpXml, "      <description type=\"string\" definition=\"product "
    "description\">%s</description>\n", description);
  fprintf(fpXml, "      <duration type=\"float\" definition=\"time between "
    "source and target image acquisition\" units=\"days\">%.8f</duration>\n", 
    time_diff);
  fprintf(fpXml, "      <data_region type=\"string\" definition=\"region data "
    "was acquired in\">%s</data_region>\n", data_region);
  fprintf(fpXml, "      <ref_id type=\"string\" definition=\"IMV tracking REQID"
    "\">%s</ref_id>\n", ref_id);
  fprintf(fpXml, "      <num_grids type=\"int\" definition=\"number of grid "
    "points with observations\">%d</num_grids>\n", num_grids);
  fprintf(fpXml, "      <pixel_size type=\"float\" definition=\"image pixel "
    "size [m]\" units=\"m\">%.3f</pixel_size>\n", pixel_size);
  fprintf(fpXml, "      <grid_spacing type=\"float\" definition=\"grid spacing "
    "[m]\" units=\"m\">%.3f</grid_spacing>\n", grid_spacing*1000.0);
  fprintf(fpXml, "      <average_disp_x type=\"float\" definition=\"average "
    "displacement in x direction [m]\" units=\"m\">%.3f</average_disp_x>\n",
    avg_disp_x*1000.0);
  fprintf(fpXml, "      <average_disp_y type=\"float\" definition=\"average "
    "displacement in y direction [m]\" units=\"m\">%.3f</average_disp_y>\n",
    avg_disp_y*1000.0);
  fprintf(fpXml, "      <projection_string type=\"string\" definition=\"map "
    "projection information as well known text\">%s</projection_string>\n", 
    meta2esri_proj(NULL, NULL));
  fprintf(fpXml, "    </product>\n");
  fprintf(fpXml, "    <source>\n");
  fprintf(fpXml, "      <file type=\"string\" definition=\"source image "
    "identifier\">%s</file>\n", source_image);
  rgps2iso_date(source_year, source_time, dateStr);
  fprintf(fpXml, "      <acquisition type=\"string\" definition=\"source image "
    "acquisition\">%s</acquisition>\n", dateStr);
  fprintf(fpXml, "      <upper_left_lat type=\"float\" definition=\"latitude of"
    " upper left corner pixel\" units=\"degrees\">%.5f</upper_left_lat>\n", 
    srcUL_lat);
  fprintf(fpXml, "      <upper_left_lon type=\"float\" definition=\"longitude "
    "of upper left corner pixel\" units=\"degrees\">%.5f</upper_left_lon>\n", 
    srcUL_lon);
  fprintf(fpXml, "      <upper_right_lat type=\"float\" definition=\"latitude "
    "of upper right corner pixel\" units=\"degrees\">%.5f</upper_right_lat>\n", 
    srcUR_lat);
  fprintf(fpXml, "      <upper_right_lon type=\"float\" definition=\"longitude "
    "of upper right corner pixel\" units=\"degrees\">%.5f</upper_right_lon>\n", 
    srcUR_lon);
  fprintf(fpXml, "      <lower_left_lat type=\"float\" definition=\"latitude of"
    " lower left corner pixel\" units=\"degrees\">%.5f</lower_left_lat>\n", 
    srcLL_lat);
  fprintf(fpXml, "      <lower_left_lon type=\"float\" definition=\"longitude "
    "of lower left corner pixel\" units=\"degrees\">%.5f</lower_left_lon>\n", 
    srcLL_lon);
  fprintf(fpXml, "      <lower_right_lat type=\"float\" definition=\"latitude "
    "of lower right corner pixel\" units=\"degrees\">%.5f</lower_right_lat>\n", 
    srcLR_lat);
  fprintf(fpXml, "      <lower_right_lon type=\"float\" definition=\"longitude "
    "of lower right corner pixel\" units=\"degrees\">%.5f</lower_right_lon>\n", 
    srcLR_lon);
  fprintf(fpXml, "    </source>\n");
  fprintf(fpXml, "    <target>\n");
  fprintf(fpXml, "      <file type=\"string\" definition=\"target image "
    "identifier\">%s</file>\n", target_image);
  rgps2iso_date(target_year, target_time, dateStr);
  fprintf(fpXml, "      <acquisition type=\"string\" definition=\"source image "
    "acquisition\">%s</acquisition>\n", dateStr);
  fprintf(fpXml, "      <upper_left_lat type=\"float\" definition=\"latitude of"
    " upper left corner pixel\" units=\"degrees\">%.5f</upper_left_lat>\n", 
    trgUL_lat);
  fprintf(fpXml, "      <upper_left_lon type=\"float\" definition=\"longitude "
    "of upper left corner pixel\" units=\"degrees\">%.5f</upper_left_lon>\n", 
    trgUL_lon);
  fprintf(fpXml, "      <upper_right_lat type=\"float\" definition=\"latitude "
    "of upper right corner pixel\" units=\"degrees\">%.5f</upper_right_lat>\n", 
    trgUR_lat);
  fprintf(fpXml, "      <upper_right_lon type=\"float\" definition=\"longitude "
    "of upper right corner pixel\" units=\"degrees\">%.5f</upper_right_lon>\n", 
    trgUR_lon);
  fprintf(fpXml, "      <lower_left_lat type=\"float\" definition=\"latitude of"
    " lower left corner pixel\" units=\"degrees\">%.5f</lower_left_lat>\n", 
    trgLL_lat);
  fprintf(fpXml, "      <lower_left_lon type=\"float\" definition=\"longitude "
    "of lower left corner pixel\" units=\"degrees\">%.5f</lower_left_lon>\n", 
    trgLL_lon);
  fprintf(fpXml, "      <lower_right_lat type=\"float\" definition=\"latitude "
    "of lower right corner pixel\" units=\"degrees\">%.5f</lower_right_lat>\n", 
    trgLR_lat);
  fprintf(fpXml, "      <lower_right_lon type=\"float\" definition=\"longitude "
    "of lower right corner pixel\" units=\"degrees\">%.5f</lower_right_lon>\n", 
    trgLR_lon);
  fprintf(fpXml, "    </target>\n");
  fprintf(fpXml, "  </metadata>\n");
  fprintf(fpXml, "  <extent>\n");
  fprintf(fpXml, "    <product>\n");
  fprintf(fpXml, "      <westBoundLongitude>%.5f</westBoundLongitude>\n",
    westBoundLon);
  fprintf(fpXml, "      <eastBoundLongitude>%.5f</eastBoundLongitude>\n",
    eastBoundLon);
  fprintf(fpXml, "      <northBoundLatitude>%.5f</northBoundLatitude>\n",
    northBoundLat);
  fprintf(fpXml, "      <southBoundLatitude>%.5f</southBoundLatitude>\n",
    southBoundLat);
  rgps2iso_date(source_year, source_time, dateStr);
  snprintf(citation, 11, "%s", dateStr);
  strcat(citation, " to ");
  fprintf(fpXml, "      <start_datetime>%s</start_datetime>\n", dateStr);
  rgps2iso_date(target_year, target_time, dateStr);
  strcat(citation, dateStr);
  citation[24] = '\0';
  fprintf(fpXml, "      <end_datetime>%s</end_datetime>\n", dateStr);
  fprintf(fpXml, "    </product>\n");
  fprintf(fpXml, "  </extent>\n");
  fprintf(fpXml, "  <processing>\n");
  rgps2iso_date(create_year, create_time, dateStr);
  fprintf(fpXml, "    <creation_time>%s</creation_time>\n", dateStr);
  fprintf(fpXml, "    <software_version>%s</software_version>\n", sw_version);
  fprintf(fpXml, "  </processing>\n");
  fprintf(fpXml, "  <root>\n");
  fprintf(fpXml, "    <institution>Alaska Satellite Facility</institution>\n");
  fprintf(fpXml, "    <title>Kwok, Ron. 2008. MEaSUREs Small-Scale Kinematics of"
    " Arctic Ocean Sea Ice, Version 01, %s. Jet Propulsion Laboratory "
    "Pasadena, CA USA and Alaska Satellite Facility Fairbanks, AK USA. Digital "
    "media.</title>\n", citation);
  fprintf(fpXml, "    <source>Products derived from ENVISAT imagery at "
    "100 m resolution</source>\n");
  fprintf(fpXml, "    <comment>Imagery the products are derived from: Copyright "
    "European Space Agency (2002 to 2012)</comment>\n");
  fprintf(fpXml, "    <reference>Documentation available at: www.asf.alaska.edu"
    "</reference>\n");
  fprintf(fpXml, "    <history>%s: CSV file created.</history>\n", isoStr);
  fprintf(fpXml, "  </root>\n");
  fprintf(fpXml, "</rgps>\n");
  FCLOSE(fpXml);
  
  FREE(inFile);
  FREE(outFile);
  FREE(path);
  FREE(csvFile);
  FREE(xmlFile);
  
  return 0;
}
