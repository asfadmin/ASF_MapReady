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

static void jd2date(julian_date *jd, char *buf)
{
  char mon[][5]= 
    {"","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
  ymd_date ymd;
  date_jd2ymd(jd, &ymd);
  sprintf(buf, "%02d-%s-%4d", ymd.day, mon[ymd.month], ymd.year);
}

int main(int argc, char **argv)
{
  FILE *fpIn, *fpOut, *fpInList, *fpOutList, *fpXml;
  meta_parameters *meta;
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
  char outDirName[512], outFileName[512];
  split_dir_and_file(outFile, outDirName, outFileName);
  char *tmpDir = (char *) MALLOC(sizeof(char)*512);
  sprintf(tmpDir, "%smeasures-", outDirName);
  char *tsdir = time_stamp_dir();
  strcat(tmpDir, tsdir);
  FREE(tsdir);
  create_clean_dir(tmpDir);
  char *isoStr = iso_date();

  // Read header information
  char inFile[512], imgFile[768], metaFile[768];
  char listOutFile[768], citation[50], start[30], end[30], first[30];
  char header[120], baseName[512], dirName[512], ext[5];
  float x_pix, y_pix, x_map_ll, y_map_ll, x_map_ur, y_map_ur, inc, cat;
  double lat, lon, height, x, y, z;
  int ii, kk, nFiles=0, num = 1, sample_count, line_count;
  image_data_type_t image_data_type;
  sprintf(listOutFile, "%s%crgps.xml", tmpDir, DIR_SEPARATOR);

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
  FREE(proj);

  // Set up supplemental file names: water mask, lat/lon, x/y grids
  char maskFile[768], latFile[768], lonFile[768], xFile[768], yFile[768]; 
  sprintf(maskFile, "%s%cwater_mask.img", tmpDir, DIR_SEPARATOR);
  sprintf(latFile, "%s%clatitude.img", tmpDir, DIR_SEPARATOR);
  sprintf(lonFile, "%s%clongitude.img", tmpDir, DIR_SEPARATOR);
  sprintf(xFile, "%s%cxgrid.img", tmpDir, DIR_SEPARATOR);
  sprintf(yFile, "%s%cygrid.img", tmpDir, DIR_SEPARATOR);

  // Generating output XML file
  fpInList = FOPEN(listInFile, "r");
  fpOutList = FOPEN(listOutFile, "w");
  fprintf(fpOutList, "<netcdf>\n");
  fprintf(fpOutList, "  <data>\n");
  fprintf(fpOutList, "    <latitude>%s</latitude>\n", latFile);
  fprintf(fpOutList, "    <longitude>%s</longitude>\n", lonFile);
  fprintf(fpOutList, "    <xgrid>%s</xgrid>\n", xFile);
  fprintf(fpOutList, "    <ygrid>%s</ygrid>\n", yFile);
  fprintf(fpOutList, "    <mask>%s</mask>\n", maskFile);
  
  julian_date jdStart, jdEnd, jdRef;
  hms_time hms;
  hms.hour = 0;
  hms.min = 0;
  hms.sec = 0.0;

  asfPrintStatus("Working through the file list:\n");
  int myrFlag=FALSE, divFlag=FALSE, vrtFlag=FALSE, shrFlag=FALSE;
  int firstYear, firstDay, startYear, startDay, endYear, endDay;
  double westBoundLon, eastBoundLon, northBoundLat, southBoundLat;
  double minLat=90.0, maxLat=-90.0, minLon=180.0, maxLon=-180.0;

  while (fgets(inFile, 512, fpInList)) {

    chomp(inFile);
    char inDirName[512], inFileName[512];
    split_dir_and_file(inFile, inDirName, inFileName);

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

    // Sort out dates
    startYear = subInt(inFileName, 0, 4);
    startDay = subInt(inFileName, 4, 3);
    endYear = subInt(inFileName, 8, 4);
    endDay = subInt(inFileName, 12, 3);
    if (nFiles == 0) {
      firstYear = startYear;
      firstDay = startDay;
    }
    sprintf(citation, "%d%03d to %d%03d", startYear, startDay, endYear, endDay);
    rgps2iso_date(startYear, (double) startDay, start);
    rgps2iso_date(endYear, (double) endDay, end);
    rgps2iso_date(firstYear, (double) firstDay, first);
    
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
    if (num > 1)
      asfPrintError("Multiband imagery (%s) not supported for netCDF "
        "generation!\n", inFile);

    /*  
    printf("x_pix: %f, y_pix: %f\n", x_pix, y_pix);
    printf("x_map_ll: %f, y_map_ll: %f\n", x_map_ll, y_map_ll);
    printf("x_map_ur: %f, y_map_ur: %f\n", x_map_ur, y_map_ur);
    printf("sample_count: %d, line_count: %d\n\n", sample_count, line_count);
    */
      
    // Check extension
    split_base_and_ext(inFileName, 1, '.', baseName, ext);
    asfPrintStatus("Processing %s ...\n", inFileName);
    sprintf(imgFile, "%s%c%s_%s.img", tmpDir, DIR_SEPARATOR, baseName, &ext[1]);
    sprintf(metaFile, "%s%c%s_%s.meta", tmpDir, DIR_SEPARATOR, baseName, 
      &ext[1]);
    
    jdRef.year = firstYear;
    jdRef.jd = 1;
    jdStart.year = startYear;
    jdStart.jd = startDay;
    jdEnd.year = endYear;
    jdEnd.jd = endDay;
    double startSec = date2sec(&jdStart, &hms) - date2sec(&jdRef, &hms);
    double endSec = date2sec(&jdEnd, &hms) - date2sec(&jdRef, &hms);
    if (strcmp_case(ext, ".MYR") == 0) {
      fprintf(fpOutList, "    <multiyear_ice_fraction start=\"%.0f\" end=\"%.0f"
        "\">%s</multiyear_ice_fraction>\n", startSec, endSec, imgFile);
      image_data_type = MULTIYEAR_ICE_FRACTION;
      myrFlag = TRUE;
    }
    else if (strcmp_case(ext, ".DIV") == 0) {
      fprintf(fpOutList, "    <divergence start=\"%.0f\" end=\"%.0f\">%s"
        "</divergence>\n", startSec, endSec, imgFile);
      image_data_type = DIVERGENCE;
      divFlag = TRUE;
    }
    else if (strcmp_case(ext, ".VRT") == 0) {
      fprintf(fpOutList, "    <vorticity start=\"%.0f\" end=\"%.0f\">%s"
        "</vorticity>\n", startSec, endSec, imgFile);
      image_data_type = VORTICITY;
      vrtFlag = TRUE;
    }
    else if (strcmp_case(ext, ".SHR") == 0) {
      fprintf(fpOutList, "    <shear start=\"%.0f\" end=\"%.0f\">%s</shear>", 
        startSec, endSec, imgFile);
      image_data_type = SHEAR;
      shrFlag = TRUE;
    }

    // Generate basic metadata
    meta = raw_init();
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
    
    // Sort out map projection
    proj->startX = x_map_ll*1000.0;
    proj->startY = y_map_ur*1000.0;
    proj->perX = x_pix*1000.0;
    proj->perY = -y_pix*1000.0;
    meta->projection = proj;
    meta_write(meta, metaFile);
    strcpy(meta->general->bands, "water mask");
    sprintf(metaFile, "%s%cwater_mask.meta", tmpDir, DIR_SEPARATOR);
    meta_write(meta, metaFile);  
    sprintf(metaFile, "%s%c%s_%s.meta", tmpDir, DIR_SEPARATOR, baseName, 
      &ext[1]);
    
    float *floatBuf = (float *) MALLOC(sizeof(float)*sample_count);

    // Write gridded data to ASF internal format
    fpOut = FOPEN(imgFile, "wb");
    for (ii=0; ii<line_count; ii++) {
      for (kk=0; kk<sample_count; kk++) {
	      FREAD(&floatBuf[kk], sizeof(float), 1, fpIn);
	      ieee_big32(floatBuf[kk]);
        if (floatBuf[kk] > 10000000000.0 || 
          FLOAT_EQUIVALENT(floatBuf[kk], 10000000000.0))
          floatBuf[kk] = MAGIC_UNSET_DOUBLE;
      }
      put_float_line(fpOut, meta, line_count-ii-1, floatBuf);
    }
    FCLOSE(fpOut);
    FREE(floatBuf);
    
    double lat1, lon1, lat2, lon2, lat3, lon3, lat4, lon4;
    proj_to_latlon(proj, x_map_ll*1000.0, y_map_ll*1000.0, 0.0, 
      &lat1, &lon1, &height);
    proj_to_latlon(proj, x_map_ll*1000.0, y_map_ur*1000.0, 0.0, 
      &lat2, &lon2, &height);
    proj_to_latlon(proj, x_map_ur*1000.0, y_map_ur*1000.0, 0.0, 
      &lat3, &lon3, &height);
    proj_to_latlon(proj, x_map_ur*1000.0, y_map_ll*1000.0, 0.0, 
      &lat4, &lon4, &height);
    westBoundLon = minValue(lon1*R2D, lon2*R2D, lon3*R2D, lon4*R2D);
    eastBoundLon = maxValue(lon1*R2D, lon2*R2D, lon3*R2D, lon4*R2D);
    northBoundLat = maxValue(lat1*R2D, lat2*R2D, lat3*R2D, lat4*R2D);
    southBoundLat = minValue(lat1*R2D, lat2*R2D, lat3*R2D, lat4*R2D);
    if (westBoundLon < minLon)
      minLon = westBoundLon;
    if (eastBoundLon > maxLon)
      maxLon = eastBoundLon;
    if (southBoundLat < minLat)
      minLat = southBoundLat;
    if (northBoundLat > maxLat)
      maxLat = northBoundLat;

    meta_free(meta);
    nFiles++;
  }
  FCLOSE(fpInList);
  
  fprintf(fpOutList, "  </data>\n");
  fprintf(fpOutList, "  <metadata>\n");
  fprintf(fpOutList, "    <time>\n");
  fprintf(fpOutList, "      <axis type=\"string\" definition=\"name of axis\">T"
    "</axis>\n");
  fprintf(fpOutList, "      <long_name type=\"string\" definition=\"long "
    "descriptive name\">serial date</long_name>\n");
  fprintf(fpOutList, "      <references type=\"string\" definition=\"reference "
    "of the value\">start time of 3-day average</references>\n");
  fprintf(fpOutList, "      <standard_name type=\"string\" definition=\"name "
    "used to identify the physical quantity\">time</standard_name>\n");
  fprintf(fpOutList, "      <units type=\"string\" definition=\"unit of "
    "dimensional quantity\">seconds since %d-01-01T00:00:00Z</units>\n",
    firstYear);
  fprintf(fpOutList, "      <bounds type=\"string\" definition=\"variable "
    "containing data range\">time_bounds</bounds>\n");
  fprintf(fpOutList, "      <FillValue type=\"double\" definition=\"default "
    "value\">0</FillValue>\n");
  fprintf(fpOutList, "    </time>\n");
  fprintf(fpOutList, "    <time_bounds>\n");
  fprintf(fpOutList, "      <long_name type=\"string\" definition=\"long "
    "descriptive name\">serial date</long_name>\n");
  fprintf(fpOutList, "      <references type=\"string\" definition=\"reference "
    "of the value\">start and end time of 3-day average</references>\n");
  fprintf(fpOutList, "      <standard_name type=\"string\" definition=\"name "
    "used to identify the physical quantity\">time</standard_name>\n");
  fprintf(fpOutList, "      <units type=\"string\" definition=\"unit of "
    "dimensional quantity\">seconds since %d-01-01T00:00:00Z</units>\n",
    firstYear);
  fprintf(fpOutList, "      <FillValue type=\"double\" definition=\"default "
    "value\">0</FillValue>\n");
  fprintf(fpOutList, "    </time_bounds>\n");
  fprintf(fpOutList, "    <latitude>\n");
  fprintf(fpOutList, "      <long_name type=\"string\" definition=\"long "
    "descriptive name\">latitude</long_name>\n");
  fprintf(fpOutList, "      <standard_name type=\"string\" definition=\"name "
    "used to identify the physical quantity\">latitude</standard_name>\n");
  fprintf(fpOutList, "      <units type=\"string\" definition=\"unit of "
    "dimensional quantity\">degrees_north</units>\n");
  fprintf(fpOutList, "      <FillValue type=\"float\" definition=\"default "
    "value\">-999</FillValue>\n");
  fprintf(fpOutList, "      <valid_min type=\"float\" definition=\"minimum "
    "valid value\">-90.0</valid_min>\n");
  fprintf(fpOutList, "      <valid_max type=\"float\" definition=\"minimum "
    "valid value\">90.0</valid_max>\n");
  fprintf(fpOutList, "    </latitude>\n");
  fprintf(fpOutList, "    <longitude>\n");
  fprintf(fpOutList, "      <long_name type=\"string\" definition=\"long "
    "descriptive name\">longitude</long_name>\n");
  fprintf(fpOutList, "      <standard_name type=\"string\" definition=\"name "
    "used to identify the physical quantity\">longitude</standard_name>\n");
  fprintf(fpOutList, "      <units type=\"string\" definition=\"unit of "
    "dimensional quantity\">degrees_east</units>\n");
  fprintf(fpOutList, "      <FillValue type=\"float\" definition=\"default "
    "value\">-999</FillValue>\n");
  fprintf(fpOutList, "      <valid_min type=\"float\" definition=\"minimum "
    "valid value\">-180.0</valid_min>\n");
  fprintf(fpOutList, "      <valid_max type=\"float\" definition=\"minimum "
    "valid value\">180.0</valid_max>\n");
  fprintf(fpOutList, "    </longitude>\n");
  fprintf(fpOutList, "    <xgrid>\n");
  fprintf(fpOutList, "      <axis type=\"string\" definition=\"name of axis\">X"
    "</axis>\n");
  fprintf(fpOutList, "      <long_name type=\"string\" definition=\"long "
    "descriptive name\">projection_grid_x_center</long_name>\n");
  fprintf(fpOutList, "      <standard_name type=\"string\" definition=\"name "
    "used to identify the physical quantity\">projection_x_coordinate"
    "</standard_name>\n");
  fprintf(fpOutList, "      <units type=\"string\" definition=\"unit of "
    "dimensional quantity\">meters</units>\n");
  fprintf(fpOutList, "      <FillValue type=\"float\" definition=\"default "
    "value\">NaN</FillValue>\n");
  fprintf(fpOutList, "    </xgrid>\n");
  fprintf(fpOutList, "    <ygrid>\n");
  fprintf(fpOutList, "      <axis type=\"string\" definition=\"name of axis\">Y"
    "</axis>\n");
  fprintf(fpOutList, "      <long_name type=\"string\" definition=\"long "
    "descriptive name\">projection_grid_y_center</long_name>\n");
  fprintf(fpOutList, "      <standard_name type=\"string\" definition=\"name "
    "used to identify the physical quantity\">projection_y_coordinate"
    "</standard_name>\n");
  fprintf(fpOutList, "      <units type=\"string\" definition=\"unit of "
    "dimensional quantity\">meters</units>\n");
  fprintf(fpOutList, "      <FillValue type=\"float\" definition=\"default "
    "value\">NaN</FillValue>\n");
  fprintf(fpOutList, "    </ygrid>\n");
  fprintf(fpOutList, "    <Polar_Stereographic>\n");
  fprintf(fpOutList, "      <grid_mapping_name>polar_stereographic"
    "</grid_mapping_name>\n");
  fprintf(fpOutList, "      <straight_vertical_longitude_from_pole>%.1f"
    "</straight_vertical_longitude_from_pole>\n", pps.ps.slon);
  fprintf(fpOutList, "      <longitude_of_central_meridian>90.0"
    "</longitude_of_central_meridian>\n");
  fprintf(fpOutList, "      <standard_parallel>%.1f</standard_parallel>\n", 
    pps.ps.slat);
  fprintf(fpOutList, "      <false_easting>%.1f</false_easting>\n", 
    pps.ps.false_easting);
  fprintf(fpOutList, "      <false_northing>%.1f</false_northing>\n",
    pps.ps.false_northing);
  fprintf(fpOutList, "      <projection_x_coordinate>xgrid"
    "</projection_x_coordinate>\n");
  fprintf(fpOutList, "      <projection_y_coordinate>ygrid"
    "</projection_y_coordinate>\n");
  fprintf(fpOutList, "      <units>meters</units>\n");
  fprintf(fpOutList, "    </Polar_Stereographic>\n");
  fprintf(fpOutList, "    <mask>\n");
  fprintf(fpOutList, "      <coordinates type=\"string\" definition=\""
    "coordinate reference\">ygrid xgrid</coordinates>\n");
  fprintf(fpOutList, "      <grid_mapping type=\"string\" definition=\"\">"
    "Polar_Stereographic</grid_mapping>\n");
  fprintf(fpOutList, "      <long_name type=\"string\" definition=\"long "
    "descriptive name\">projection_grid_y_center</long_name>\n");
  fprintf(fpOutList, "      <units type=\"string\" definition=\"unit of "
    "dimensional quantity\">1</units>\n");
  fprintf(fpOutList, "      <units_description type=\"string\" definition=\""
    "descriptive information about dimensionless quantity\">unitless"
    "</units_description>\n");
  fprintf(fpOutList, "      <FillValue type=\"int\" definition=\"default "
    "value\">0</FillValue>\n");
  fprintf(fpOutList, "    </mask>\n");
  if (myrFlag) {
    fprintf(fpOutList, "    <multiyear_ice_fraction>\n");
    fprintf(fpOutList, "      <cell_methods type=\"string\" definition=\""
      "characteristic of a field that is represented by cell values\">area: "
      "multiyear ice fraction value</cell_methods>\n");
    fprintf(fpOutList, "      <coordinates type=\"string\" definition=\""
      "coordinate reference\">ygrid xgrid</coordinates>\n");
    fprintf(fpOutList, "      <grid_mapping type=\"string\" definition=\"\">"
      "Polar_Stereographic</grid_mapping>\n");
    fprintf(fpOutList, "      <long_name type=\"string\" definition=\"long "
      "descriptive name\">RGPS MEaSUREs multiyear ice fraction</long_name>\n");
    fprintf(fpOutList, "      <units type=\"string\" definition=\"unit of "
      "dimensional quantity\">1</units>\n");
    fprintf(fpOutList, "      <units_description type=\"string\" definition=\""
      "descriptive information about dimensionless quantity\">unitless"
      "</units_description>\n");
    fprintf(fpOutList, "      <FillValue type=\"float\" definition=\"default "
      "value\">NaN</FillValue>\n");
    fprintf(fpOutList, "    </multiyear_ice_fraction>\n");
  }
  if (divFlag) {
    fprintf(fpOutList, "    <divergence>\n");
    fprintf(fpOutList, "      <cell_methods type=\"string\" definition=\""
      "characteristic of a field that is represented by cell values\">area: "
      "divergence value</cell_methods>\n");
    fprintf(fpOutList, "      <coordinates type=\"string\" definition=\""
      "coordinate reference\">ygrid xgrid</coordinates>\n");
    fprintf(fpOutList, "      <grid_mapping type=\"string\" definition=\"\">"
      "Polar_Stereographic</grid_mapping>\n");
    fprintf(fpOutList, "      <long_name type=\"string\" definition=\"long "
      "descriptive name\">RGPS MEaSUREs divergence</long_name>\n");
    fprintf(fpOutList, "      <units type=\"string\" definition=\"unit of "
      "dimensional quantity\">1</units>\n");
    fprintf(fpOutList, "      <units_description type=\"string\" definition=\""
      "descriptive information about dimensionless quantity\">unitless"
      "</units_description>\n");
    fprintf(fpOutList, "      <FillValue type=\"float\" definition=\"default "
      "value\">NaN</FillValue>\n");
    fprintf(fpOutList, "    </divergence>\n");
  }
  if (vrtFlag) {
    fprintf(fpOutList, "    <vorticity>\n");
    fprintf(fpOutList, "      <cell_methods type=\"string\" definition=\""
      "characteristic of a field that is represented by cell values\">area: "
      "vorticity value</cell_methods>\n");
    fprintf(fpOutList, "      <coordinates type=\"string\" definition=\""
      "coordinate reference\">ygrid xgrid</coordinates>\n");
    fprintf(fpOutList, "      <grid_mapping type=\"string\" definition=\"\">"
      "Polar_Stereographic</grid_mapping>\n");
    fprintf(fpOutList, "      <long_name type=\"string\" definition=\"long "
      "descriptive name\">RGPS MEaSUREs vorticity</long_name>\n");
    fprintf(fpOutList, "      <units type=\"string\" definition=\"unit of "
      "dimensional quantity\">1</units>\n");
    fprintf(fpOutList, "      <units_description type=\"string\" definition=\""
      "descriptive information about dimensionless quantity\">unitless"
      "</units_description>\n");
    fprintf(fpOutList, "      <FillValue type=\"float\" definition=\"default "
      "value\">NaN</FillValue>\n");
    fprintf(fpOutList, "    </vorticity>\n");
  }
  if (shrFlag) {
    fprintf(fpOutList, "    <shear>\n");
    fprintf(fpOutList, "      <cell_methods type=\"string\" definition=\""
      "characteristic of a field that is represented by cell values\">area: "
      "shear value</cell_methods>\n");
    fprintf(fpOutList, "      <coordinates type=\"string\" definition=\""
      "coordinate reference\">ygrid xgrid</coordinates>\n");
    fprintf(fpOutList, "      <grid_mapping type=\"string\" definition=\"\">"
      "Polar_Stereographic</grid_mapping>\n");
    fprintf(fpOutList, "      <long_name type=\"string\" definition=\"long "
      "descriptive name\">RGPS MEaSUREs shear</long_name>\n");
    fprintf(fpOutList, "      <units type=\"string\" definition=\"unit of "
      "dimensional quantity\">1</units>\n");
    fprintf(fpOutList, "      <units_description type=\"string\" definition=\""
      "descriptive information about dimensionless quantity\">unitless"
      "</units_description>\n");
    fprintf(fpOutList, "      <FillValue type=\"float\" definition=\"default "
      "value\">NaN</FillValue>\n");
    fprintf(fpOutList, "    </shear>\n");
  }
  fprintf(fpOutList, "  </metadata>\n");
  fprintf(fpOutList, "  <parameter>\n");
  if (myrFlag)
    fprintf(fpOutList, "    <multiyear_ice_fraction type=\"float\"/>\n");
  if (divFlag)
    fprintf(fpOutList, "    <divergence type=\"float\"/>\n");
  if (vrtFlag)
    fprintf(fpOutList, "    <vorticity type=\"float\"/>\n");
  if (shrFlag)
    fprintf(fpOutList, "    <shear type=\"float\"/>\n");
  fprintf(fpOutList, "  </parameter>\n");
  
  char startStr[15], endStr[15];
  jdStart.year = firstYear;
  jdStart.jd = firstDay;
  jdEnd.year = endYear;
  jdEnd.jd = endDay;
  jd2date(&jdStart, startStr);
  jd2date(&jdEnd, endStr);
  if (firstYear != endYear || firstDay != endDay)
    sprintf(citation, "%s to %s", startStr, endStr);
  else
    sprintf(citation, "%s", startStr);
  fprintf(fpOutList, "  <root>\n");
  fprintf(fpOutList, "    <Conventions>CF-1.6</Conventions>\n");
  fprintf(fpOutList, "    <institution>Alaska Satellite Facility</institution>\n");
  fprintf(fpOutList, "    <title>Kwok, Ron. 2008. MEaSUREs Small-Scale Kinematics"
    " of Arctic Ocean Sea Ice, Version 01, %s. Jet Propulsion Laboratory "
    "Pasadena, CA USA and Alaska Satellite Facility Fairbanks, AK USA. "
    "Digital media.</title>\n", citation);
  fprintf(fpOutList, "    <source>Products derived from RADARSAT-1 SWB imagery at "
    "100 m resolution</source>\n");
  fprintf(fpOutList, "    <comment>Imagery the products are derived from: Copyright "
    "Canadian Space Agency (1996 to 2008)</comment>\n");
  fprintf(fpOutList, "    <reference>Documentation available at: www.asf.alaska.edu"
    "</reference>\n");
  fprintf(fpOutList, "    <history>%s: netCDF file created.</history>\n", isoStr);
  fprintf(fpOutList, "  </root>\n");
  fprintf(fpOutList, "</netcdf>\n");
  FCLOSE(fpOutList);

  // Generate supplemental files: water mask, lat/lon, x/y grids
  asfPrintStatus("Generating supplemental files ...\n");
  float *floatBuf = (float *) MALLOC(sizeof(float)*sample_count);
  float *maskBuf = (float *) MALLOC(sizeof(float)*sample_count);
  float *latBuf = (float *) MALLOC(sizeof(float)*sample_count);
  float *lonBuf = (float *) MALLOC(sizeof(float)*sample_count);
  float *xBuf = (float *) MALLOC(sizeof(float)*sample_count);
  float *yBuf = (float *) MALLOC(sizeof(float)*sample_count);
  meta = meta_read(metaFile);
  
  fpIn = FOPEN(inFile, "r");
  fgets(header, 100, fpIn);
  sscanf(header, "%f %f %f %f %f %f", &x_pix, &y_pix, &x_map_ll, &y_map_ll, 
    &x_map_ur, &y_map_ur);
  fgets(header, 100, fpIn);
  sscanf(header, "%d %d", &sample_count, &line_count);
  
  FILE *fpMask = FOPEN(maskFile, "wb");
  FILE *fpLat = FOPEN(latFile, "wb");
  FILE *fpLon = FOPEN(lonFile, "wb");
  FILE *fpXgrid = FOPEN(xFile, "wb");
  FILE *fpYgrid = FOPEN(yFile, "wb");
  for (ii=0; ii<line_count; ii++) {
    for (kk=0; kk<sample_count; kk++) {
      FREAD(&floatBuf[kk], sizeof(float), 1, fpIn);
      ieee_big32(floatBuf[kk]);
    }
    for (kk=0; kk<sample_count; kk++) {
      meta_get_latLon(meta, line_count-ii-1, kk, 0.0, &lat, &lon);
      latlon_to_proj(meta->projection, 'R', lat*D2R, lon*D2R, 0.0, &x, &y, &z);
      latBuf[kk] = lat;
      lonBuf[kk] = lon;
      xBuf[kk] = x;
      yBuf[kk] = y;
      if (floatBuf[kk] < 10000000000.0) {
        maskBuf[kk] = 1.0;
      }
      else if (floatBuf[kk] > 10000000000.0) {
        maskBuf[kk] = 1.0;
      }
      else {
        maskBuf[kk] = 0.0;
      }
    }
    put_float_line(fpMask, meta, line_count-ii-1, maskBuf);
    put_float_line(fpLat, meta, line_count-ii-1, latBuf);
    put_float_line(fpLon, meta, line_count-ii-1, lonBuf);
    put_float_line(fpXgrid, meta, line_count-ii-1, xBuf);
    put_float_line(fpYgrid, meta, line_count-ii-1, yBuf);
  }
  FCLOSE(fpIn);
  FCLOSE(fpMask);
  FCLOSE(fpLat);
  FCLOSE(fpLon);
  FREE(floatBuf);
  FREE(maskBuf);
  FREE(latBuf);
  FREE(lonBuf);
  FREE(xBuf);
  FREE(yBuf);
  meta_write(meta, latFile);
  meta_write(meta, lonFile);
  meta_write(meta, xFile);
  meta_write(meta, yFile);

  // Write ISO meatadata for netCDF
  asfPrintStatus("Generating metadata for netCDF file ...\n");

  char *ncXmlBase = get_basename(outFile);
  char *ncXmlFile = appendExt(outFile, ".xml");
  fpXml = FOPEN(ncXmlFile, "w");
  fprintf(fpXml, "<rgps>\n");
  fprintf(fpXml, "  <granule>%s</granule>\n", ncXmlBase);
  fprintf(fpXml, "  <metadata_creation>%s</metadata_creation>\n", isoStr);
  fprintf(fpXml, "  <metadata>\n");
  fprintf(fpXml, "    <product>\n");
  fprintf(fpXml, "      <file type=\"string\" definition=\"name of product "
    "file\">%s.nc</file>\n", ncXmlBase);
  if (divFlag && vrtFlag && shrFlag)
    fprintf(fpXml, "      <type type=\"string\" definition=\"product type\">"
    "divergence, vorticity, shear</type>\n");
  else if (myrFlag)
    fprintf(fpXml, "      <type type=\"string\" definition=\"product type\">"
    "multiyear ice fraction</type>\n");
  fprintf(fpXml, "      <format type=\"string\" definition=\"name of the data "
    "format\">netCDF</format>\n");

  fpInList = FOPEN(listInFile, "r");
  while (fgets(inFile, 512, fpInList)) {
    chomp(inFile);
    split_dir_and_file(inFile, dirName, baseName);
    fprintf(fpXml, "      <source type=\"string\" definition=\"name of the data"
    " source\">%s</source>\n", baseName);
  }
  FCLOSE(fpInList);

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
  fprintf(fpXml, "      <cell_dimension_x type=\"int\" definition=\"cell "
    "dimension in x direction\">%d</cell_dimension_x>\n", 
    sample_count);
  fprintf(fpXml, "      <cell_dimension_y type=\"int\" definition=\"cell "
    "dimension in y direction\">%d</cell_dimension_y>\n",
      line_count);
  fprintf(fpXml, "      <projection_string type=\"string\" definition=\"map "
    "projection information as well known text\">%s</projection_string>\n", 
  meta2esri_proj(meta, NULL));
  fprintf(fpXml, "    </product>\n");
  fprintf(fpXml, "  </metadata>\n");
  fprintf(fpXml, "  <extent>\n");
  fprintf(fpXml, "    <product>\n");
  fprintf(fpXml, "      <westBoundLongitude>%.5f</westBoundLongitude>\n",
    minLon);
  fprintf(fpXml, "      <eastBoundLongitude>%.5f</eastBoundLongitude>\n",
    maxLon);
  fprintf(fpXml, "      <northBoundLatitude>%.5f</northBoundLatitude>\n",
    maxLat);
  fprintf(fpXml, "      <southBoundLatitude>%.5f</southBoundLatitude>\n",
    minLat);
  fprintf(fpXml, "      <start_datetime>%s</start_datetime>\n", first);
  fprintf(fpXml, "      <end_datetime>%s</end_datetime>\n", end);
  fprintf(fpXml, "    </product>\n");
  fprintf(fpXml, "  </extent>\n");
  fprintf(fpXml, "</rgps>\n");
  FCLOSE(fpXml);
  FREE(ncXmlBase);
  FREE(ncXmlFile);
  meta_free(meta);

  // Export to netCDF
  asfPrintStatus("Exporting to netCDF file ...\n");
  export_netcdf_xml(listOutFile, outFile);

  // Clean up
  remove_dir(tmpDir);
  FREE(tmpDir);
  FREE(outFile);
  FREE(listInFile);
  FREE(isoStr);

  return 0;
}
