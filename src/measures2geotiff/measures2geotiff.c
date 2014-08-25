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
	 "   %s [ -log <logFile> ] <inFile> <outFile>\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   inFile    MEaSUREs data product\n"
	 "   outFile   name of the GeoTIFF file\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s converts RGPS MEaSUREs gridded files into a GeoTIFF file.\n",
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

static int subInt(char *inStr, int start, int len)
{ 
  char buf[100];
  strncpy(buf, &inStr[start], len);
  buf[len]=0;
  int ret = atoi(buf);
  return ret;
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

  // Setup file names
  char inDirName[512], outDirName[512], inFileName[512], outFileName[512];
  split_dir_and_file(inFile, inDirName, inFileName);
  split_dir_and_file(outFile, outDirName, outFileName);
  char *tmpDir = (char *) MALLOC(sizeof(char)*512);
  sprintf(tmpDir, "%smeasures-", outDirName);
  char *tsdir = time_stamp_dir();
  strcat(tmpDir, tsdir);
  FREE(tsdir);
  create_clean_dir(tmpDir);
  char *isoStr = iso_date();

  // Read header information
  char imgFile[768], metaFile[768], xmlFile[768], citation[50], start[30];
  char end[30], header[120], baseName[512], ext[5];
  float x_pix = 0.0, y_pix = 0.0, x_map_ll = 0.0, y_map_ll = 0.0;
  float x_map_ur = 0.0, y_map_ur = 0.0, inc, cat;
  int ii, kk, ll, mm, num = 1, sample_count = 0, line_count = 0;
  image_data_type_t image_data_type = UNKNOWN_IMAGE_DATA_TYPE;

  hms_time hms;
  hms.hour = 0;
  hms.min = 0;
  hms.sec = 0.0;

  int ageFlag = FALSE, thkFlag = FALSE, bshFlag = FALSE, myrFlag = FALSE; 
  int divFlag = FALSE, vrtFlag = FALSE, shrFlag = FALSE;

  // Sort out dates
  int startYear = subInt(inFileName, 0, 4);
  int startDay = subInt(inFileName, 4, 3);
  int endYear = subInt(inFileName, 8, 4);
  int endDay = subInt(inFileName, 12, 3);
  sprintf(citation, "%d%03d to %d%03d", startYear, startDay, endYear, endDay);
  rgps2iso_date(startYear, (double) startDay, start);
  rgps2iso_date(endYear, (double) endDay, end);
  
  // Check extension
  split_base_and_ext(outFileName, 1, '.', baseName, ext);
  sprintf(xmlFile, "%s%s.xml", outDirName, baseName);
  split_base_and_ext(inFileName, 1, '.', baseName, ext);
  FILE *fpXml = FOPEN(xmlFile, "w");
  fprintf(fpXml, "<rgps>\n");
  fprintf(fpXml, "  <granule>%s</granule>\n", inFileName);
  fprintf(fpXml, "  <metadata_creation>%s</metadata_creation>\n", isoStr);
  fprintf(fpXml, "  <metadata>\n");
  fprintf(fpXml, "    <product>\n");
  fprintf(fpXml, "      <file type=\"string\" definition=\"name of product "
    "file\">%s_%s.tif</file>\n", baseName, &ext[1]);
  
  // Read header information
  FILE *fpIn = FOPEN(inFile, "r");
  fgets(header, 100, fpIn);
  sscanf(header, "%f %f %f %f %f %f", &x_pix, &y_pix, &x_map_ll, &y_map_ll, 
    &x_map_ur, &y_map_ur);
  fgets(header, 100, fpIn);
  int params = sscanf(header, "%f %f %d %d", 
    &inc, &cat, &sample_count, &line_count);
  if (params == 3) {
    sscanf(header, "%f %d %d", &cat, &sample_count, &line_count);
    inc = 0;
  }
  else if (params == 2) {
    sscanf(header, "%d %d", &sample_count, &line_count);
    inc = 0;
    cat = 1;
  }
  num = (int) cat;
  
  printf("x_pix: %f, y_pix: %f\n", x_pix, y_pix);
  printf("x_map_ll: %f, y_map_ll: %f\n", x_map_ll, y_map_ll);
  printf("x_map_ur: %f, y_map_ur: %f\n", x_map_ur, y_map_ur);
  printf("inc: %f, num: %d, sample_count: %d, line_count: %d\n\n",
    inc, num, sample_count, line_count);
  
  if (strcmp_case(ext, ".AGE") == 0) {
    fprintf(fpXml, "      <type type=\"string\" definition=\"product type\">"
      "ice age</type>\n");
    image_data_type = ICE_AGE;
    ageFlag= TRUE;
  }
  else if (strcmp_case(ext, ".THK") == 0) {
    fprintf(fpXml, "      <type type=\"string\" definition=\"product type\">"
      "ice thickness</type>\n");
    image_data_type = ICE_THICKNESS;
    thkFlag = TRUE;
  }
  else if (strcmp_case(ext, ".BSH") == 0) {
    fprintf(fpXml, "      <type type=\"string\" definition=\"product type\">"
      "backscatter histogram</type>\n");
    image_data_type = BACKSCATTER_HISTOGRAM;
    bshFlag = TRUE;
  }
  else if (strcmp_case(ext, ".MYR") == 0) {
    fprintf(fpXml, "      <type type=\"string\" definition=\"product type\">"
      "multiyear ice fraction</type>\n");
    image_data_type = MULTIYEAR_ICE_FRACTION;
    myrFlag = TRUE;
  }
  else if (strcmp_case(ext, ".DIV") == 0) {
    fprintf(fpXml, "      <type type=\"string\" definition=\"product type\">"
      "divergence</type>\n");
    image_data_type = DIVERGENCE;
    divFlag = TRUE;
  }
  else if (strcmp_case(ext, ".VRT") == 0) {
    fprintf(fpXml, "      <type type=\"string\" definition=\"product type\">"
      "vorticity</type>\n");
    image_data_type = VORTICITY;
    vrtFlag = TRUE;
  }
  else if (strcmp_case(ext, ".SHR") == 0) {
    fprintf(fpXml, "      <type type=\"string\" definition=\"product type\">"
      "shear</type>\n");
    image_data_type = SHEAR;
    shrFlag = TRUE;
  }
  
  // Read the data
  long pixel_count = line_count*sample_count;
  float *floatBuf = (float *) MALLOC(sizeof(float)*pixel_count*num);
  FREAD(&floatBuf[0], sizeof(float), pixel_count*num, fpIn);
  FCLOSE(fpIn);
  
  // Work the metadata
  fprintf(fpXml, "      <format type=\"string\" definition=\"name of the data "
    "format\">GeoTIFF</format>\n");
  fprintf(fpXml, "      <cell_size_x type=\"double\" definition=\"cell size "
    "in x direction\" units=\"m\">%.2f</cell_size_x>\n", x_pix*1000.0);
  fprintf(fpXml, "      <cell_size_y type=\"double\" definition=\"cell size "
    "in y direction\" units=\"m\">%.2f</cell_size_y>\n", y_pix*1000.0);
  fprintf(fpXml, "      <map_x_lower_left type=\"double\" definition=\"x "
    "coordinate of lower left corner\" units=\"m\">%.6f</map_x_lower_left>\n",
    x_map_ll*1000.0);
  fprintf(fpXml, "      <map_y_lower_left type=\"double\" definition=\"y "
    "coordinate of lower left corner\" units=\"m\">%.6f</map_y_lower_left>\n",
    y_map_ll*1000.0);
  fprintf(fpXml, "      <map_x_upper_right type=\"double\" definition=\"x "
    "coordinate of upper right corner\" units=\"m\">%.6f</map_x_upper_right>"
    "\n", x_map_ur*1000.0);
  fprintf(fpXml, "      <map_y_upper_right type=\"double\" definition=\"y "
    "coordinate of upper right corner\" units=\"m\">%.6f</map_y_upper_right>"
    "\n", y_map_ur*1000.0);
  if (num > 1) {
    if (image_data_type == ICE_AGE)
      fprintf(fpXml, "      <number_ice_age_categories type=\"int\" definition="
      "\"number of ice age categories\">%d</number_ice_categories>\n", num);
    else if (image_data_type == ICE_THICKNESS) {
      if (inc > 0)
        fprintf(fpXml, "      <ice_thickness_increment type=\"float\" "
          "definition=\"ice thickness increment\" unit=\"m\">%.3f"
          "</ice_thickness_increment>\n", inc*0.01);
      fprintf(fpXml, "      <number_ice_thickness_categories type=\"int\" "
        "definition=\"number of ice thickness categories\">%d"
        "</number_ice_thickness_categories>\n", num);
    }
    else if (image_data_type == BACKSCATTER_HISTOGRAM) {
      fprintf(fpXml, "      <number_ice_thickness_categories type=\"int\" "
        "definition=\"number of ice thickness categories\">%d"
        "</number_ice_thickness_categories>\n", num);
    }
  }
  fprintf(fpXml, "      <cell_dimension_x type=\"int\" definition=\"cell "
    "dimension in x direction\">%d</cell_dimension_x>\n", 
    sample_count);
  fprintf(fpXml, "      <cell_dimension_y type=\"int\" definition=\"cell "
    "dimension in y direction\">%d</cell_dimension_y>\n",
    line_count);

  // Generate basic metadata
  meta_parameters *meta = raw_init();
  meta->general->line_count = line_count;
  meta->general->sample_count = sample_count;
  meta->general->band_count = 1;
  meta->general->data_type = REAL32;
  meta->general->image_data_type = image_data_type;
  strcpy(meta->general->basename, inFile);
  meta->general->x_pixel_size = x_pix*1000.0;
  meta->general->y_pixel_size = y_pix*1000.0;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->no_data = MAGIC_UNSET_DOUBLE;
  strcpy(meta->general->sensor, "RGPS MEaSUREs");
  char *tmp = image_data_type2str(meta->general->image_data_type);
  sprintf(meta->general->bands, "%s", lc(tmp));
  FREE(tmp);
  sprintf(meta->general->acquisition_date, "%s", baseName);
  
  // Preparing map projection information
  project_parameters_t pps;
  projection_type_t proj_type;
  datum_type_t datum;
  spheroid_type_t spheroid;
  read_proj_file("polar_stereographic_north_ssmi.proj", 
    &pps, &proj_type, &datum, &spheroid);
  pps.ps.false_easting = 0.0;
  pps.ps.false_northing = 0.0;
  meta_projection *proj = meta_projection_init();
  proj->type = proj_type;
  proj->datum = HUGHES_DATUM;
  proj->spheroid = HUGHES_SPHEROID;
  proj->param = pps;
  strcpy(proj->units, "meters");
  proj->hem = 'N';
  spheroid_axes_lengths(spheroid, &proj->re_major, &proj->re_minor);
  proj->startX = x_map_ll*1000.0;
  proj->startY = y_map_ur*1000.0;
  proj->perX = x_pix*1000.0;
  proj->perY = -y_pix*1000.0;
  meta->projection = proj;
  
  // Set up intermediate file list
  char *listFile = (char *) MALLOC(sizeof(char)*(strlen(tmpDir)+10));
  sprintf(listFile, "%s%clist", tmpDir, DIR_SEPARATOR);
  FILE *fpList = FOPEN(listFile, "w");

  // Write gridded data to ASF internal format
  float *floatBand = (float *) MALLOC(sizeof(float)*sample_count);
  long index;
  for (ll=0; ll<num; ll++) {
    sprintf(imgFile, "%s%c%s_%s_%03d.img", tmpDir, DIR_SEPARATOR, baseName,
      &ext[1], ll);
    sprintf(metaFile, "%s%c%s_%s_%03d.meta", tmpDir, DIR_SEPARATOR, baseName, 
      &ext[1], ll);
    fprintf(fpList, "%s\n", imgFile);
    meta_write(meta, metaFile);
    FILE *fpOut = FOPEN(imgFile, "wb");
    for (ii=0; ii<line_count; ii++) {
      for (kk=0; kk<sample_count; kk++) {
        for (mm=0; mm<num; mm++) {
          index = ii*num*sample_count + kk*num + mm;
          ieee_big32(floatBuf[index]);
          if (floatBuf[index] > 10000000000.0 || 
            FLOAT_EQUIVALENT(floatBuf[index], 10000000000.0))
            floatBand[kk] = MAGIC_UNSET_DOUBLE;
          else
            floatBand[kk] = floatBuf[index];
        }
      }
      put_float_line(fpOut, meta, line_count-ii-1, floatBand);
    }
    FCLOSE(fpOut);
  }
  FREE(floatBuf);
  FREE(floatBand);
  FCLOSE(fpList);

  // Adding map projection information to metadata    
  fprintf(fpXml, "      <projection_string type=\"string\" definition=\"map "
    "projection information as well known text\">%s</projection_string>\n", 
  meta2esri_proj(meta, NULL));
  fprintf(fpXml, "    </product>\n");
  fprintf(fpXml, "  </metadata>\n");

  // Calculate geographic extent
  double lat1, lon1, lat2, lon2, lat3, lon3, lat4, lon4, height;
  proj_to_latlon(proj, x_map_ll*1000.0, y_map_ll*1000.0, 0.0, 
    &lat1, &lon1, &height);
  proj_to_latlon(proj, x_map_ll*1000.0, y_map_ur*1000.0, 0.0, 
    &lat2, &lon2, &height);
  proj_to_latlon(proj, x_map_ur*1000.0, y_map_ur*1000.0, 0.0, 
    &lat3, &lon3, &height);
  proj_to_latlon(proj, x_map_ur*1000.0, y_map_ll*1000.0, 0.0, 
    &lat4, &lon4, &height);
  double westBoundLon = minValue(lon1*R2D, lon2*R2D, lon3*R2D, lon4*R2D);
  double eastBoundLon = maxValue(lon1*R2D, lon2*R2D, lon3*R2D, lon4*R2D);
  double northBoundLat = maxValue(lat1*R2D, lat2*R2D, lat3*R2D, lat4*R2D);
  double southBoundLat = minValue(lat1*R2D, lat2*R2D, lat3*R2D, lat4*R2D);
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
  fprintf(fpXml, "      <start_datetime>%s</start_datetime>\n", start);
  fprintf(fpXml, "      <end_datetime>%s</end_datetime>\n", end);
  fprintf(fpXml, "    </product>\n");
  fprintf(fpXml, "  </extent>\n");
  fprintf(fpXml, "</rgps>\n");
  FCLOSE(fpXml);
  meta_free(meta);
  
  // Export product to GeoTIFF
  export_geotiff(listFile, outFile);
  
  // Clean up
  remove_dir(tmpDir);
  FREE(tmpDir);
  FREE(inFile);
  FREE(outFile);
  FREE(isoStr);
  FREE(listFile);

  return 0;
}
