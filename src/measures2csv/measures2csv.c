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


#define VERSION 1.0

typedef struct {
  char id[17];
  short year;
  double time;
  double map_x;
  double map_y;
} image_t;

typedef struct {
  short year;
  double time;
  double map_x;
  double map_y;
  short q_flag;
} ice_motion_obs_t;

typedef struct {
  short year;
  double time;
  double map_x;
  double map_y;
  double disp_x;
  double disp_y;
  float cell_area;
  float diff_area;
  float dtp;
  float dudx;
  float dudy;
  float dvdx;
  float dvdy;
} ice_deform_obs_t;

typedef struct {
  short year;
  double time;
  double map_x;
  double map_y;
  float center_temp;
  float fdd;
  float cell_area;
  short n_age;
  short n_thick;
  short n_ridge;
  short far_fyr;
  short far_my;
} ice_age_obs_t;

typedef struct {
  float r_age;
  float i_age;
  short frac_area;
  float r_fdd;
  float i_fdd;
} ice_age_t ;

typedef struct {
  float r_ridge_ar;
  float i_ridge_ar;
  float r_ridge_tr;
  float i_ridge_tr;
  short ridge_far;
  float r_ridge_fdd;
  float i_ridge_fdd;
  short ridge_flag;
} ice_ridge_t;

typedef struct {
  short year;
  double time;
  double map_x;
  double map_y;
  float center_temp;
  float cell_area;
  short multi_year;
  short open_water;
  short frac_back[25];
  float inc_ang;
} backscatter_t;

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [ -log <logFile> ] <measures> <csv>\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   measures   MEaSUREs data file\n"
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

static char *iso_date(void)
{
  time_t t;
  char *t_stamp;

  t_stamp = (char*) MALLOC(30*sizeof(char));
  t = time(NULL);
  strftime(t_stamp, 30, "%Y-%m-%dT%H:%M:%S.000000Z", gmtime(&t));

  return t_stamp;
}

static char *meta2esri_proj(meta_parameters *meta, char *projFile)
{
  FILE *fpIn;
  char projcsStr[100], geogcsStr[200], projStr[500], datumStr[150];
  char spheroidStr[100], *error;
  static char out[1024];

  project_parameters_t pps;
  projection_type_t proj_type;
  datum_type_t datum;
  spheroid_type_t spheroid;
  double semimajor;
  double inv_flattening ;

  // Get projection information
  if (meta &&
      (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR ||
       meta->projection->type == POLAR_STEREOGRAPHIC ||
       meta->projection->type == ALBERS_EQUAL_AREA ||
       meta->projection->type == LAMBERT_CONFORMAL_CONIC ||
       meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)) {
    
    pps = meta->projection->param;
    proj_type = meta->projection->type;
    datum = meta->projection->datum;
    spheroid = meta->projection->spheroid;
    semimajor = meta->projection->re_major;
    inv_flattening = 
      semimajor / (semimajor - meta->projection->re_minor);
  }
  else if (projFile) {
    parse_proj_args_file(projFile, &pps, &proj_type, &datum, &spheroid, &error);
    fpIn = FOPEN(projFile, "r");
    //spheroid = get_spheroid(fpIn);
    FCLOSE(fpIn);

    switch (spheroid)
      {
      case BESSEL_SPHEROID:
      	semimajor = BESSEL_SEMIMAJOR;
      	inv_flattening = BESSEL_INV_FLATTENING;
    	  break;
      case CLARKE1866_SPHEROID:
      	semimajor = CLARKE1866_SEMIMAJOR;
      	inv_flattening = CLARKE1866_INV_FLATTENING;
    	  break;
      case GEM6_SPHEROID:
      	semimajor = GEM6_SEMIMAJOR;
      	inv_flattening = GEM6_INV_FLATTENING;
    	  break;
      case GEM10C_SPHEROID:
      	semimajor = GEM10C_SEMIMAJOR;
      	inv_flattening = GEM10C_INV_FLATTENING;			\
    	  break;
      case GRS1980_SPHEROID:
      	semimajor = GRS1980_SEMIMAJOR;
      	inv_flattening = GRS1980_INV_FLATTENING;
    	  break;
      case INTERNATIONAL1924_SPHEROID:
      	semimajor = INTERNATIONAL1924_SEMIMAJOR;
      	inv_flattening = INTERNATIONAL1924_INV_FLATTENING;
    	  break;
      case INTERNATIONAL1967_SPHEROID:
      	semimajor = INTERNATIONAL1967_SEMIMAJOR;
      	inv_flattening = INTERNATIONAL1967_INV_FLATTENING;
    	  break;
      case WGS72_SPHEROID:
      	semimajor = WGS72_SEMIMAJOR;
      	inv_flattening = WGS72_INV_FLATTENING;
    	  break;
      case WGS84_SPHEROID:
      	semimajor = WGS84_SEMIMAJOR;
      	inv_flattening = WGS84_INV_FLATTENING;
    	  break;
      case HUGHES_SPHEROID:
      	semimajor = HUGHES_SEMIMAJOR;
      	inv_flattening = HUGHES_INV_FLATTENING;
    	  break;
      default:
        asfPrintError("Unknown spheroid: %d\n", spheroid);
        break;
      }
  }

  // Convert the projection information into ESRI projection format
  if ((meta &&
       (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR ||
        meta->projection->type == POLAR_STEREOGRAPHIC ||
        meta->projection->type == ALBERS_EQUAL_AREA ||
        meta->projection->type == LAMBERT_CONFORMAL_CONIC ||
        meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)) || projFile) {

    // Construct geographic coordinate system string
    sprintf(geogcsStr, "GEOGCS[");
    sprintf(datumStr, "DATUM[");
    switch (datum)
      {
      case EGM96_DATUM:
      	break;
      case ED50_DATUM:
      	strcat(geogcsStr, "\"GCS_European_1950\",");
      	strcat(datumStr, "\"D_European_1950\",");
    	  break;
      case ETRF89_DATUM:
      	strcat(geogcsStr, "\"GCS_ETRS_1989\",");
      	strcat(datumStr, "\"D_ETRS_1989\",");
    	  break;
      case ITRF97_DATUM:
    	  break;
      case NAD27_DATUM:
      	strcat(geogcsStr, "\"GCS_North_American_1927\",");
      	strcat(datumStr, "\"D_North_American_1927\",");
    	  break;
      case NAD83_DATUM:
      	strcat(geogcsStr, "\"GCS_North_American_1983\",");
      	strcat(datumStr, "\"D_North_American_1983\",");
    	  break;
      case WGS72_DATUM:
      	strcat(geogcsStr, "\"GCS_WGS_1972\",");
      	strcat(datumStr, "\"D_WGS_1972\",");
    	  break;
      case WGS84_DATUM:
      	strcat(geogcsStr, "\"GCS_WGS_1984\",");
      	strcat(datumStr, "\"D_WGS_1984\",");
    	  break;
      case HUGHES_DATUM:
      	strcat(geogcsStr, "\"GCS_HUGHES\",");
      	strcat(datumStr, "\"D_HUGHES\",");
    	  break;
      default:
        asfPrintError("Unknown datum: %d\n", datum);
        break;
      }
    strcat(geogcsStr, datumStr);
    switch (spheroid)
      {
      case BESSEL_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"BESSEL\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case CLARKE1866_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"CLARKE_1866\",%.0f,%.9f]]",
    	  	semimajor, inv_flattening);
    	  break;
      case GEM6_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"GEM6\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case GEM10C_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"GEM10C\",%.0f,%.9f]]",
		      semimajor, inv_flattening);
    	  break;
      case GRS1980_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"GRS_1980\",%.0f,%.9f]]", 
      		semimajor, inv_flattening);
    	  break;
      case INTERNATIONAL1924_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"International_1924\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case INTERNATIONAL1967_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"International_1967\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case WGS72_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"WGS_1972\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case WGS84_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"WGS_1984\",%.0f,%.9f]]",
      		semimajor, inv_flattening);
    	  break;
      case HUGHES_SPHEROID:
      	sprintf(spheroidStr, "SPHEROID[\"HUGHES\",%.0f,%9f]]",
      		semimajor, inv_flattening);
    	  break;
      default:
        asfPrintError("Unknown spheroid: %d\n", spheroid);
        break;
      }
    strcat(geogcsStr, spheroidStr);  
    
    // Construct projection string
    switch (proj_type)
      {
      default:
        asfPrintError("Unknown proj_type: %d\n", proj_type);
        break;
      case LAT_LONG_PSEUDO_PROJECTION:
    	  break;
      case UNIVERSAL_TRANSVERSE_MERCATOR:
      	sprintf(projcsStr, "PROJCS[\"Universal_Transverse_Mercator\"");
      	sprintf(projStr, "PROJECTION[\"Transverse_Mercator\"],PARAMETER[\""
      		"False_Easting\",%.1f],PARAMETER[\"False_Northing\",%.1f],"
      		"PARAMETER[\"Central_Meridian\",%.1f],PARAMETER["
      		"\"Scale_Factor\",%.4f],PARAMETER[\"Latitude_Of_Origin\",%.1f],"
      		"UNIT[\"Meter\",1.0]",
    		pps.utm.false_easting, pps.utm.false_northing, 
		    pps.utm.lon0, pps.utm.scale_factor, pps.utm.lat0);
    	  break;
      case POLAR_STEREOGRAPHIC:
      	if (!isfinite(pps.ps.false_easting))
	        pps.ps.false_easting = 0.0;
      	if (!isfinite(pps.ps.false_northing))
      	  pps.ps.false_northing = 0.0;
      	sprintf(projcsStr, "PROJCS[\"Polar_Stereographic\"");
      	sprintf(projStr, "PROJECTION[\"Stereographic\"],PARAMETER["
      		"\"False_Easting\",%.1f],PARAMETER[\"False_Northing\",%.1f],"
      		"PARAMETER[\"Central_Meridian\",%.1f],PARAMETER["
      		"\"Scale_Factor\",1.0],PARAMETER[\"Latitude_Of_Origin\",%.1f],"
      		"UNIT[\"Meter\",1.0]",
    		pps.ps.false_easting, pps.ps.false_northing, pps.ps.slat, 
		    pps.ps.slon);
    	  break;
      case ALBERS_EQUAL_AREA:
      	sprintf(projcsStr, "PROJCS[\"Albers_Equal_Area_Conic\"");
      	sprintf(projStr, "PROJECTION[\"Albers\"],PARAMETER[\"False_Easting\","
      		"%.1f],PARAMETER[\"False_Northing\",%.1f],PARAMETER["
      		"\"Central_Meridian\",%.1f],PARAMETER[\"Standard_Parallel_1\","
      		"%.1f],PARAMETER[\"Standard_Parallel_2\",%.1f],PARAMETER["
      		"\"Latitude_Of_Origin\",%.1f],UNIT[\"Meter\",1.0]",
      		pps.albers.false_easting, pps.albers.false_northing, 
      		pps.albers.center_meridian, pps.albers.std_parallel1, 
      		pps.albers.std_parallel2, pps.albers.orig_latitude);
    	  break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      	sprintf(projcsStr, "PROJCS[\"Lambert_Azimuthal_Equal_Area\"");
      	sprintf(projStr, "PROJECTION[\"\"],PARAMETER[\"False_Easting\","
      		"%.1f],PARAMETER[\"False_Northing\",%.1f],PARAMETER["
      		"\"Central_Meridian\",%.1f],PARAMETER["
      		"\"Latitude_Of_Origin\",%.1f],UNIT[\"Meter\",1.0]",
      		pps.lamaz.false_easting, pps.lamaz.false_northing, 
      		pps.lamaz.center_lat, pps.lamaz.center_lon);
    	  break;
      case LAMBERT_CONFORMAL_CONIC:
      	sprintf(projcsStr, "PROJCS[\"Lambert_Conformal_Conic\"");
      	sprintf(projStr, "PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER["
      		"\"False_Easting\",%.1f],PARAMETER[\"False_Northing\",%.1f],"
      		"PARAMETER[\"Central_Meridian\",%.1f],PARAMETER["
      		"\"Standard_Parallel_1\",%.1f],PARAMETER["
      		"\"Standard_Parallel_2\",%.1f],PARAMETER["
      		"\"Latitude_Of_Origin\",%.1f],UNIT[\"Meter\",1.0]",
      		pps.lamcc.false_easting, pps.lamcc.false_northing, 
      		pps.lamcc.lon0, pps.lamcc.plat1, pps.lamcc.plat2, 
      		pps.lamcc.lat0);
    	  break;
      case MERCATOR:
        break;
      case EQUI_RECTANGULAR:
    	  break;
      }
    
    sprintf(out, "%s,%s,PRIMEM[\"Greenwich\",0],UNIT[\"Degree\","
	    "0.0174532925199432955]],%s]\n", projcsStr, geogcsStr, projStr);
  }
  else
    sprintf(out,
	    "GEOGCS[\"GCS_WGS_1984\","
	    "DATUM[\"D_WGS_1984\","
	    "SPHEROID[\"WGS_1984\",6378137,298.257223563]],"
	    "PRIMEM[\"Greenwich\",0],"
	    "UNIT[\"Degree\",0.017453292519943295]]");

  return out;
}

int main(int argc, char **argv)
{
  FILE *fpIn, *fpOut=NULL;
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
  char *csvFile = (char *) MALLOC(sizeof(char)*(strlen(argv[2])+1));
  strcpy(csvFile, argv[2]);
  char *xmlFile = (char *) MALLOC(sizeof(char)*(strlen(csvFile)+10));
  create_name(xmlFile, csvFile, ".xml");
  
  // General header information
  char pid[25];	             // RGPS Product Identifier
  char product_id[20];
  char prod_description[50]; // Description of this product
  short	n_images=0;	 // Number of images used in the create of this product
  int n_trajectories=0;      // Number of trajectories
  int n_cells;               // Number of cells
  char prod_type[15];        // Product type
  short	create_year=0;	     // Product creation year
  double create_time=0.0;	   // Product creation time
  short prod_start_year=0;   // Product start year
  double prod_start_time=0;  // Product start time
  short	prod_end_year=0;     // Product end year
  double prod_end_time=0;	   // Product end time
  char sw_version[12];       // Software version used to create this product
  char sw_ver[5];
  float	n_w_lat;             // North West Latitude of initial datatake
  float	n_w_lon;             // North West Longitude of inital datatake
  float	n_e_lat;             // North East Latitude of initial datatake
  float	n_e_lon;             // North East Longitude of initial datatake
  float	s_w_lat;             // South West Latitude of initial datatake
  float	s_w_lon;             // South West Longitude of initial datatake
  float	s_e_lat;             // South West Latitude of initial datatake
  float	s_e_lon;             // South West Longitude of initial datatake
  float thick_step;          // Interval of each thickness category
  float westBoundLon=0, eastBoundLon=0, northBoundLat=0, southBoundLat=0;
  char citation[100], isoStr[30];
  int ii, kk, ll;
  image_t *image;

  int gpid;          // Grid point identifier
  int cell_id;       // Cell identifier
  short birth_year;  // Birth year of grid point
  double birth_time; // Birth time of grid point
  short death_year;  // Death year of grid point
  double death_time; // Dearth time of grid point
  float init_area;   // Initial cell area
  int n_int_obs;     // Number of observations (int)
  short n_short_obs; // Number of observations (short)

  // Time and projection
  double lat, lon, height;
  float bsr_low[25], bsr_high[25];
  meta_parameters *meta=NULL;
  project_parameters_t pps;
  projection_type_t proj_type;
  datum_type_t datum;
  spheroid_type_t spheroid;
  meta_projection *proj;
  char dateStr[30];
  
  fpIn = FOPEN(inFile, "rb");

  FREAD(&pid, 24, 1, fpIn);
  FREAD(&prod_description, 40, 1, fpIn);  
  snprintf(product_id, 18, "%s", pid);
  printf("PID: %s\n", product_id);
  printf("Product description: %s\n", prod_description);
  
  if (strncmp_case(prod_description, "Lagrangian Ice Motion", 21) == 0) {
    FREAD(&n_images, 2, 1, fpIn);
    ieee_big16(n_images);
    FREAD(&n_trajectories, 4, 1, fpIn);
    ieee_big32(n_trajectories);
    FREAD(&prod_type, 8, 1, fpIn);
    FREAD(&create_year, 2, 1, fpIn);
    ieee_big16(create_year);
    FREAD(&create_time, 8, 1, fpIn);
    ieee_big64(create_time);
    FREAD(&prod_start_year, 2, 1, fpIn);
    ieee_big16(prod_start_year);
    FREAD(&prod_start_time, 8, 1, fpIn);
    ieee_big64(prod_start_time);
    FREAD(&prod_end_year, 2, 1, fpIn);
    ieee_big16(prod_end_year);
    FREAD(&prod_end_time, 8, 1, fpIn);
    ieee_big64(prod_end_time);
    FREAD(&sw_version, 12, 1, fpIn);
    FREAD(&n_w_lat, 4, 1, fpIn);
    ieee_big32(n_w_lat);
    FREAD(&n_w_lon, 4, 1, fpIn);
    ieee_big32(n_w_lon);
    FREAD(&n_e_lat, 4, 1, fpIn);
    ieee_big32(n_e_lat);
    FREAD(&n_e_lon, 4, 1, fpIn);
    ieee_big32(n_e_lon);
    FREAD(&s_w_lat, 4, 1, fpIn);
    ieee_big32(s_w_lat);
    FREAD(&s_w_lon, 4, 1, fpIn);
    ieee_big32(s_w_lon);
    FREAD(&s_e_lat, 4, 1, fpIn);
    ieee_big32(s_e_lat);
    FREAD(&s_e_lon, 4, 1, fpIn);
    ieee_big32(s_e_lon);
    
    printf("Number images: %d\n", n_images);
    printf("Number trajectories: %d\n", n_trajectories);
    printf("Product type: %s\n", prod_type);
    printf("Product creation year: %d\n", create_year);
    printf("Product creation time: %.6f\n", create_time);
    printf("Product start year: %d\n", prod_start_year);
    printf("Product start time: %.6f\n", prod_start_time);
    printf("Product end year: %d\n", prod_end_year);
    printf("Product end time: %.6f\n", prod_end_time);
    snprintf(sw_ver, 5, "%s", sw_version);
    printf("Software version: %s\n", sw_ver);
    printf("NW: %.6f %.6f\n", n_w_lat, n_w_lon);
    printf("NE: %.6f %.6f\n", n_e_lat, n_e_lon);
    printf("SW: %.6f %.6f\n", s_w_lat, s_w_lon);
    printf("SE: %.6f %.6f\n", s_e_lat, s_e_lon);
    westBoundLon = minValue(n_w_lon, n_e_lon, s_w_lon, s_e_lon);
    eastBoundLon = maxValue(n_w_lon, n_e_lon, s_w_lon, s_e_lon);
    northBoundLat = maxValue(n_w_lat, n_e_lat, s_w_lat, s_e_lat);
    southBoundLat = minValue(n_w_lat, n_e_lat, s_w_lat, s_e_lat);
        
    image = (image_t *) MALLOC(sizeof(image_t)*n_images);
    for (ii=0; ii<n_images; ii++) {
      FREAD(&image[ii].id, 16, 1, fpIn);
      FREAD(&image[ii].year, 2, 1, fpIn);
      ieee_big16(image[ii].year);
      FREAD(&image[ii].time, 8, 1, fpIn);
      ieee_big64(image[ii].time);
      FREAD(&image[ii].map_x, 8, 1, fpIn);
      ieee_big64(image[ii].map_x);
      FREAD(&image[ii].map_y, 8, 1, fpIn);
      ieee_big64(image[ii].map_y);
      /*
      printf("Image (%d) ID: %s\n", ii+1, image[ii].id);
      printf("Image center: %d, %.6f\n", image[ii].year, image[ii].time);
      printf("Map: %.6f %.6f\n", image[ii].map_x, image[ii].map_y);
      */
    }

    fpOut = FOPEN(csvFile, "w");
    fprintf(fpOut, "IMAGE_ID,IMAGE_YEAR,IMAGE_TIME,MAP_X,MAP_Y,GPID,BIRTH_YEAR,"
      "BIRTH_TIME,DEATH_YEAR,DEATH_TIME,N_OBS,OBS_YEAR,OBS_TIME,X_MAP,Y_MAP,"
      "LAT,LON,Q_FLAG\n");
    for (ii=0; ii<n_trajectories; ii++) {
      FREAD(&gpid, 4, 1, fpIn);
      ieee_big32(gpid);
      FREAD(&birth_year, 2, 1, fpIn);
      ieee_big16(birth_year);
      FREAD(&birth_time, 8, 1, fpIn);
      ieee_big64(birth_time);
      FREAD(&death_year, 2, 1, fpIn);
      ieee_big16(death_year);
      FREAD(&death_time, 8, 1, fpIn);
      ieee_big64(death_time);
      FREAD(&n_int_obs, 4, 1, fpIn);
      ieee_big32(n_int_obs);
      
      printf("GPID: %d\r", gpid);
      /*
      printf("Birth: %d, %.6f\n", birth_year, birth_time);
      printf("Death: %d, %.6f\n", death_year, death_time);
      printf("Number of observations: %d\n", n_obs);
      */
      ice_motion_obs_t *obs = 
      	(ice_motion_obs_t *) MALLOC(sizeof(ice_motion_obs_t)*n_int_obs);
      for (kk=0; kk<n_int_obs; kk++) {
        FREAD(&obs[kk].year, 2, 1, fpIn);
        ieee_big16(obs[kk].year);
        FREAD(&obs[kk].time, 8, 1, fpIn);
        ieee_big64(obs[kk].time);
        FREAD(&obs[kk].map_x, 8, 1, fpIn);
        ieee_big64(obs[kk].map_x);
        FREAD(&obs[kk].map_y, 8, 1, fpIn);
        ieee_big64(obs[kk].map_y);
        FREAD(&obs[kk].q_flag, 2, 1, fpIn);
        ieee_big16(obs[kk].q_flag);
        /*
        printf("Observation: %d, %.6f\n", obs[kk].year, obs[kk].time);
        printf("Map: %.6f %.6f\n", obs[kk].map_x, obs[kk].map_y);
        printf("Quality flag: %d\n", obs[kk].q_flag);
        */
        double map_x = obs[kk].map_x * 1000.0;
        double map_y = obs[kk].map_y * 1000.0;
        char *image_id;
        int image_year;
        double image_time, image_map_x, image_map_y;

        // Define projection block
        read_proj_file("polar_stereographic_north_ssmi.proj", 
          &pps, &proj_type, &datum, &spheroid);
        pps.ps.false_easting = 0.0;
        pps.ps.false_northing = 0.0;
        proj = meta_projection_init();
        proj->type = proj_type;
        proj->datum = HUGHES_DATUM; //datum;
        proj->spheroid = spheroid;
        proj->param = pps;
        strcpy(proj->units, "meters");
        proj->hem = 'N';
        spheroid_axes_lengths(spheroid, &proj->re_major, &proj->re_minor);
        proj_to_latlon(proj, map_x, map_y, 0.0, &lat, &lon, &height);
        meta = raw_init();
        meta->projection = proj;
        for (ll=0; ll<n_images; ll++) {
          if ((obs[kk].year == image[ll].year) &&
              FLOAT_COMPARE_TOLERANCE(obs[kk].time, image[ll].time, 0.000001)) {
            image_id = STRDUP(image[ll].id);
            image_year = image[ll].year;
            image_time = image[ll].time;
            image_map_x = image[ll].map_x*1000.0;
            image_map_y = image[ll].map_y*1000.0;
          }
        }
        fprintf(fpOut,"%s,%d,%.6f,%.4f,%.4f,%d,%d,%.6f,%d,%.6f,%d,", image_id, 
          image_year, image_time, image_map_x, image_map_y, gpid, birth_year, 
          birth_time, death_year, death_time, n_int_obs);
        fprintf(fpOut, "%d,%.6f,%.4f,%.4f,%.5f,%.5f,%d\n", obs[kk].year, 
          obs[kk].time, map_x, map_y, lat*R2D, lon*R2D, obs[kk].q_flag);
      }
      FREE(obs);
    }
    FREE(image);
  }
  else if (strncmp_case(prod_description, "Ice Deformation", 15) == 0) {
    FREAD(&n_cells, 4, 1, fpIn);
    ieee_big32(n_cells);
    FREAD(&create_year, 2, 1, fpIn);
    ieee_big16(create_year);
    FREAD(&create_time, 8, 1, fpIn);
    ieee_big64(create_time);
    FREAD(&prod_start_year, 2, 1, fpIn);
    ieee_big16(prod_start_year);
    FREAD(&prod_start_time, 8, 1, fpIn);
    ieee_big64(prod_start_time);
    FREAD(&prod_end_year, 2, 1, fpIn);
    ieee_big16(prod_end_year);
    FREAD(&prod_end_time, 8, 1, fpIn);
    ieee_big64(prod_end_time);
    FREAD(&sw_version, 12, 1, fpIn);
    FREAD(&n_w_lat, 4, 1, fpIn);
    ieee_big32(n_w_lat);
    FREAD(&n_w_lon, 4, 1, fpIn);
    ieee_big32(n_w_lon);
    FREAD(&n_e_lat, 4, 1, fpIn);
    ieee_big32(n_e_lat);
    FREAD(&n_e_lon, 4, 1, fpIn);
    ieee_big32(n_e_lon);
    FREAD(&s_w_lat, 4, 1, fpIn);
    ieee_big32(s_w_lat);
    FREAD(&s_w_lon, 4, 1, fpIn);
    ieee_big32(s_w_lon);
    FREAD(&s_e_lat, 4, 1, fpIn);
    ieee_big32(s_e_lat);
    FREAD(&s_e_lon, 4, 1, fpIn);
    ieee_big32(s_e_lon);
        
    printf("Number cells: %d\n", n_cells);
    printf("Product creation year: %d\n", create_year);
    printf("Product creation time: %.6f\n", create_time);
    printf("Product start year: %d\n", prod_start_year);
    printf("Product start time: %.6f\n", prod_start_time);
    printf("Product end year: %d\n", prod_end_year);
    printf("Product end time: %.6f\n", prod_end_time);
    snprintf(sw_ver, 5, "%s", sw_version);
    printf("Software version: %s\n", sw_ver);
    printf("NW: %.6f %.6f\n", n_w_lat, n_w_lon);
    printf("NE: %.6f %.6f\n", n_e_lat, n_e_lon);
    printf("SW: %.6f %.6f\n", s_w_lat, s_w_lon);
    printf("SE: %.6f %.6f\n", s_e_lat, s_e_lon);
    westBoundLon = minValue(n_w_lon, n_e_lon, s_w_lon, s_e_lon);
    eastBoundLon = maxValue(n_w_lon, n_e_lon, s_w_lon, s_e_lon);
    northBoundLat = maxValue(n_w_lat, n_e_lat, s_w_lat, s_e_lat);
    southBoundLat = minValue(n_w_lat, n_e_lat, s_w_lat, s_e_lat);
    
    fpOut = FOPEN(csvFile, "w");
    fprintf(fpOut, "CELL_ID,BIRTH_YEAR,BIRTH_TIME,N_OBS,OBS_YEAR,OBS_TIME,"
      "X_MAP,Y_MAP,LAT,LON,X_DISP,Y_DISP,C_AREA,D_AREA,DTP,DUDX,DUDY,DVDX,DVDY"
      "\n");
    for (ii=0; ii<n_cells; ii++) {
      FREAD(&cell_id, 4, 1, fpIn);
      ieee_big32(cell_id);
      FREAD(&birth_year, 2, 1, fpIn);
      ieee_big16(birth_year);
      FREAD(&birth_time, 8, 1, fpIn);
      ieee_big64(birth_time);
      FREAD(&n_short_obs, 2, 1, fpIn);
      ieee_big16(n_short_obs);
      
      printf("Cell ID: %d\r", cell_id);
      /*      
      printf("Birth: %d, %.6f\n", birth_year, birth_time);
      printf("Number of observations: %d\n", n_short_obs);
      */
      ice_deform_obs_t *obs = 
      	(ice_deform_obs_t *) MALLOC(sizeof(ice_deform_obs_t)*n_short_obs);
      for (kk=0; kk<n_short_obs; kk++) {
        FREAD(&obs[kk].year, 2, 1, fpIn);
        ieee_big16(obs[kk].year);
        FREAD(&obs[kk].time, 8, 1, fpIn);
        ieee_big64(obs[kk].time);
        FREAD(&obs[kk].map_x, 8, 1, fpIn);
        ieee_big64(obs[kk].map_x);
        FREAD(&obs[kk].map_y, 8, 1, fpIn);
        ieee_big64(obs[kk].map_y);
        FREAD(&obs[kk].disp_x, 8, 1, fpIn);
        ieee_big64(obs[kk].disp_x);
        FREAD(&obs[kk].disp_y, 8, 1, fpIn);
        ieee_big64(obs[kk].disp_y);
        FREAD(&obs[kk].cell_area, 4, 1, fpIn);
        ieee_big32(obs[kk].cell_area);
        FREAD(&obs[kk].diff_area, 4, 1, fpIn);
        ieee_big32(obs[kk].diff_area);
        FREAD(&obs[kk].dtp, 4, 1, fpIn);
        ieee_big32(obs[kk].dtp);
        FREAD(&obs[kk].dudx, 4, 1, fpIn);
        ieee_big32(obs[kk].dudx);
        FREAD(&obs[kk].dudy, 4, 1, fpIn);
        ieee_big32(obs[kk].dudy);
        FREAD(&obs[kk].dvdx, 4, 1, fpIn);
        ieee_big32(obs[kk].dvdx);
        FREAD(&obs[kk].dvdy, 4, 1, fpIn);
        ieee_big32(obs[kk].dvdy);
        /*
        printf("\nObservation: %d, %.6f\n", obs[kk].year, obs[kk].time);
        printf("Map: %.6f %.6f\n", obs[kk].map_x, obs[kk].map_y);
        printf("Displacement: %.6f %.6f\n", obs[kk].disp_x, obs[kk].disp_y);
        printf("Area: %.6f %.6f\n", obs[kk].cell_area, obs[kk].diff_area);
        printf("dtp: %.6f\n", obs[kk].dtp);
        printf("dudx: %.6f, dudy: %.6f\n", obs[kk].dudx, obs[kk].dudy);
        printf("dvdx: %.6f, dvdy: %.6f\n", obs[kk].dvdx, obs[kk].dvdy);
        */
        double map_x = obs[kk].map_x * 1000.0;
        double map_y = obs[kk].map_y * 1000.0;

        // Define projection block
        read_proj_file("polar_stereographic_north_ssmi.proj", 
                 &pps, &proj_type, &datum, &spheroid);
        pps.ps.false_easting = 0.0;
        pps.ps.false_northing = 0.0;
        proj = meta_projection_init();
        proj->type = proj_type;
        proj->datum = HUGHES_DATUM;
        proj->spheroid = spheroid;
        proj->param = pps;
        strcpy(proj->units, "meters");
        proj->hem = 'N';
        spheroid_axes_lengths(spheroid, &proj->re_major, &proj->re_minor);
        proj_to_latlon(proj, map_x, map_y, 0.0, &lat, &lon, &height);
        meta = raw_init();
        meta->projection = proj;
        fprintf(fpOut, "%d,%d,%.6f,", cell_id, birth_year, birth_time);
        fprintf(fpOut, "%d,%d,%.6f,", n_short_obs, obs[kk].year, obs[kk].time);
        fprintf(fpOut, "%.4f,%.4f,%.5f,%.5f,", map_x, map_y, lat*R2D, lon*R2D);
        fprintf(fpOut, "%.6f,%.6f,", obs[kk].disp_x, obs[kk].disp_y);
        fprintf(fpOut, "%.6f,%.6f,", obs[kk].cell_area, obs[kk].diff_area);
        fprintf(fpOut, "%.6f,", obs[kk].dtp);
        fprintf(fpOut, "%.6f,%.6f,", obs[kk].dudx, obs[kk].dudy);
        fprintf(fpOut, "%.6f,%.6f\n", obs[kk].dvdx, obs[kk].dvdy);
      }
      FREE(obs);
    }
  }  
  else if (strncmp_case(prod_description, "Ice Age", 7) == 0) {
    FREAD(&n_cells, 4, 1, fpIn);
    ieee_big32(n_cells);
    FREAD(&create_year, 2, 1, fpIn);
    ieee_big16(create_year);
    FREAD(&create_time, 8, 1, fpIn);
    ieee_big64(create_time);
    FREAD(&prod_start_year, 2, 1, fpIn);
    ieee_big16(prod_start_year);
    FREAD(&prod_start_time, 8, 1, fpIn);
    ieee_big64(prod_start_time);
    FREAD(&prod_end_year, 2, 1, fpIn);
    ieee_big16(prod_end_year);
    FREAD(&prod_end_time, 8, 1, fpIn);
    ieee_big64(prod_end_time);
    FREAD(&sw_version, 12, 1, fpIn);
    FREAD(&n_w_lat, 4, 1, fpIn);
    ieee_big32(n_w_lat);
    FREAD(&n_w_lon, 4, 1, fpIn);
    ieee_big32(n_w_lon);
    FREAD(&n_e_lat, 4, 1, fpIn);
    ieee_big32(n_e_lat);
    FREAD(&n_e_lon, 4, 1, fpIn);
    ieee_big32(n_e_lon);
    FREAD(&s_w_lat, 4, 1, fpIn);
    ieee_big32(s_w_lat);
    FREAD(&s_w_lon, 4, 1, fpIn);
    ieee_big32(s_w_lon);
    FREAD(&s_e_lat, 4, 1, fpIn);
    ieee_big32(s_e_lat);
    FREAD(&s_e_lon, 4, 1, fpIn);
    ieee_big32(s_e_lon);
    FREAD(&thick_step, 4, 1, fpIn);
    ieee_big32(thick_step);
    
    printf("Number cells: %d\n", n_cells);
    printf("Product creation year: %d\n", create_year);
    printf("Product creation time: %.6f\n", create_time);
    printf("Product start year: %d\n", prod_start_year);
    printf("Product start time: %.6f\n", prod_start_time);
    printf("Product end year: %d\n", prod_end_year);
    printf("Product end time: %.6f\n", prod_end_time);
    snprintf(sw_ver, 5, "%s", sw_version);
    printf("Software version: %s\n", sw_ver);
    printf("NW: %.6f %.6f\n", n_w_lat, n_w_lon);
    printf("NE: %.6f %.6f\n", n_e_lat, n_e_lon);
    printf("SW: %.6f %.6f\n", s_w_lat, s_w_lon);
    printf("SE: %.6f %.6f\n", s_e_lat, s_e_lon);
    printf("Thickness interval: %.6f\n", thick_step);
    westBoundLon = minValue(n_w_lon, n_e_lon, s_w_lon, s_e_lon);
    eastBoundLon = maxValue(n_w_lon, n_e_lon, s_w_lon, s_e_lon);
    northBoundLat = maxValue(n_w_lat, n_e_lat, s_w_lat, s_e_lat);
    southBoundLat = minValue(n_w_lat, n_e_lat, s_w_lat, s_e_lat);
    
    fpOut = FOPEN(csvFile, "w");
    fprintf(fpOut, "CELL_ID,BIRTH_YEAR,BIRTH_TIME,I_AREA,N_OBS,OBS_YEAR,"
      "OBS_TIME,X_MAP,Y_MAP,LAT,LON,C_TEMP,FDD,C_AREA,N_AGE,R_AR,I_AR,AGE_FAR,"
      "R_FDD,I_FDD,N_THICK,THICK_FAR,FAR_FYR,FAR_MY,N_RIDGE,R_RIDGE_AR,"
      "I_RIDGE_AR,R_RIDGE_TR,I_RIDGE_TR,RIDGE_FAR,R_RIDGE_FDD,I_RIDGE_FDD,"
      "RIDGE_FLAG\n");

    for (ii=0; ii<n_cells; ii++) {
      FREAD(&cell_id, 4, 1, fpIn);
      ieee_big32(cell_id);
      FREAD(&birth_year, 2, 1, fpIn);
      ieee_big16(birth_year);
      FREAD(&birth_time, 8, 1, fpIn);
      ieee_big64(birth_time);
      FREAD(&init_area, 4, 1, fpIn);
      ieee_big32(init_area);
      FREAD(&n_int_obs, 4, 1, fpIn);
      ieee_big32(n_int_obs);
      
      printf("Cell ID: %d\r", cell_id);
      /*
      printf("Birth: %d, %.6f\n", birth_year, birth_time);
      printf("Initial cell area: %.6f\n", init_area);
      printf("Number of observations: %d\n", n_int_obs);
      */
      ice_age_obs_t *obs = 
      	(ice_age_obs_t *) MALLOC(sizeof(ice_age_obs_t)*n_int_obs);
      for (kk=0; kk<n_int_obs; kk++) {
        FREAD(&obs[kk].year, 2, 1, fpIn);
        ieee_big16(obs[kk].year);
        FREAD(&obs[kk].time, 8, 1, fpIn);
        ieee_big64(obs[kk].time);
        FREAD(&obs[kk].map_x, 8, 1, fpIn);
        ieee_big64(obs[kk].map_x);
        FREAD(&obs[kk].map_y, 8, 1, fpIn);
        ieee_big64(obs[kk].map_y);
        FREAD(&obs[kk].center_temp, 4, 1, fpIn);
        ieee_big32(obs[kk].center_temp);
        FREAD(&obs[kk].fdd, 4, 1, fpIn);
        ieee_big32(obs[kk].fdd);
        FREAD(&obs[kk].cell_area, 4, 1, fpIn);
        ieee_big32(obs[kk].cell_area);
        FREAD(&obs[kk].n_age, 2, 1, fpIn);
        ieee_big16(obs[kk].n_age);
        /*
        printf("\nObservation: %d, %.6f\n", obs[kk].year, obs[kk].time);
        printf("Map: %.6f %.6f\n", obs[kk].map_x, obs[kk].map_y);
        printf("Center temperate: %.6f\n", obs[kk].center_temp);
        printf("Freezing degree days: %.6f\n", obs[kk].fdd);
        printf("Current cell area: %.6f\n", obs[kk].cell_area);
        printf("Number of age categories: %d\n", obs[kk].n_age);
        */
        double map_x = obs[kk].map_x * 1000.0;
        double map_y = obs[kk].map_y * 1000.0;

        // Define projection block
        read_proj_file("polar_stereographic_north_ssmi.proj", 
                 &pps, &proj_type, &datum, &spheroid);
        pps.ps.false_easting = 0.0;
        pps.ps.false_northing = 0.0;
        proj = meta_projection_init();
        proj->type = proj_type;
        proj->datum = HUGHES_DATUM;
        proj->spheroid = spheroid;
        proj->param = pps;
        strcpy(proj->units, "meters");
        proj->hem = 'N';
        spheroid_axes_lengths(spheroid, &proj->re_major, &proj->re_minor);
        proj_to_latlon(proj, map_x, map_y, 0.0, &lat, &lon, &height);
        meta = raw_init();
        meta->projection = proj;
      
        // Read ice age information
        int n_max = obs[kk].n_age;
        ice_age_t *age=NULL;
        if (obs[kk].n_age > 0) {
          age = (ice_age_t *) MALLOC(sizeof(ice_age_t)*obs[kk].n_age);
          for (ll=0; ll<obs[kk].n_age; ll++) {
            FREAD(&age[ll].r_age, 4, 1, fpIn);
            ieee_big32(age[ll].r_age);
            FREAD(&age[ll].i_age, 4, 1, fpIn);
            ieee_big32(age[ll].i_age);
            FREAD(&age[ll].frac_area, 2, 1, fpIn);
            ieee_big16(age[ll].frac_area);
            FREAD(&age[ll].r_fdd, 4, 1, fpIn);
            ieee_big32(age[ll].r_fdd);
            FREAD(&age[ll].i_fdd, 4, 1, fpIn);
            ieee_big32(age[ll].i_fdd);
            /*
            printf("Bin: %d\n", ll+1);
            printf("Age range: %.6f, %.6f\n", age[ll].r_age, age[ll].i_age);
            printf("Fractional area: %.6f\n", (age[ll].frac_area)*0.001);
            printf("Freezing degree days: %.6f, %.6f\n", 
             age[ll].r_fdd, age[ll].i_fdd);
            */
          }
        }
        // Read ice thickness information
        FREAD(&obs[kk].n_thick, 2, 1, fpIn);
        ieee_big16(obs[kk].n_thick);
        //printf("Number of thickness categories: %d\n", obs[kk].n_thick);
        if (obs[kk].n_thick > n_max)
          n_max = obs[kk].n_thick;
        short *thick_far=NULL;
        if (obs[kk].n_thick > 0) {
          thick_far = (short *) MALLOC(sizeof(short)*obs[kk].n_thick);
          for (ll=0; ll<obs[kk].n_thick; ll++) {
            FREAD(&thick_far[ll], 2, 1, fpIn);
            ieee_big16(thick_far[ll]);
            /*
            printf("Fractional area in thickness: %.6f\n", 
             (thick_far[ll])*0.001);
            */
          }
        }
        // Read general fractional area information
        FREAD(&obs[kk].far_fyr, 2, 1, fpIn);
        ieee_big16(obs[kk].far_fyr);
        FREAD(&obs[kk].far_my, 2, 1, fpIn);
        ieee_big16(obs[kk].far_my);
        /*
        printf("Area first-year ice: %.6f\n", (obs[kk].far_fyr)*0.001);
        printf("Area multi-year ice: %.6f\n", (obs[kk].far_my)*0.001);
        */
        // Read ice ridge information
        FREAD(&obs[kk].n_ridge, 2, 1, fpIn);
        ieee_big16(obs[kk].n_ridge);
        //printf("Number of ridging events: %d\n", obs[kk].n_ridge);
        if (obs[kk].n_ridge > n_max)
          n_max = obs[kk].n_ridge;
        ice_ridge_t *ridge=NULL;
        if (obs[kk].n_ridge > 0) {
          ridge = (ice_ridge_t *) MALLOC(sizeof(ice_ridge_t)*obs[kk].n_ridge);
          for (ll=0; ll<obs[kk].n_ridge; ll++) {
            FREAD(&ridge[ll].r_ridge_ar, 4, 1, fpIn);
            ieee_big32(ridge[ll].r_ridge_ar);
            FREAD(&ridge[ll].i_ridge_ar, 4, 1, fpIn);
            ieee_big32(ridge[ll].i_ridge_ar);
            FREAD(&ridge[ll].r_ridge_tr, 4, 1, fpIn);
            ieee_big32(ridge[ll].r_ridge_tr);
            FREAD(&ridge[ll].i_ridge_tr, 4, 1, fpIn);
            ieee_big32(ridge[ll].i_ridge_tr);
            FREAD(&ridge[ll].ridge_far, 2, 1, fpIn);
            ieee_big16(ridge[ll].ridge_far);
            FREAD(&ridge[ll].r_ridge_fdd, 4, 1, fpIn);
            ieee_big32(ridge[ll].r_ridge_fdd);
            FREAD(&ridge[ll].i_ridge_fdd, 4, 1, fpIn);
            ieee_big32(ridge[ll].i_ridge_fdd);
            FREAD(&ridge[ll].ridge_flag, 2, 1, fpIn);
            ieee_big16(ridge[ll].ridge_flag);
            /*
            printf("Ridge: %d\n", ll+1);
            printf("Ridge age range: %.6f, %.6f\n", 
             ridge[ll].r_ridge_ar, ridge[ll].i_ridge_ar);
            printf("Ridge thickness range: %.6f, %.6f\n", 
             ridge[ll].r_ridge_tr, ridge[ll].i_ridge_tr);
            printf("Fractional ridge area: %d\n", ridge[ll].ridge_far);
            printf("Freezing degree days: %.6f, %.6f\n", 
             ridge[ll].r_ridge_fdd, ridge[ll].i_ridge_fdd);
            printf("Ridge flag: %d\n", ridge[ll].ridge_flag);
            */
          }
        }
        for (ll=0; ll<n_max; ll++) {
          fprintf(fpOut, "%d,%d,%.6f,%.6f,%d,", cell_id, birth_year, birth_time,
            init_area, n_int_obs);
          fprintf(fpOut, "%d,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%d,", 
            obs[kk].year, obs[kk].time, map_x, map_y, lat*R2D, lon*R2D,
            obs[kk].center_temp, obs[kk].fdd, obs[kk].cell_area, obs[kk].n_age);
          if (ll < obs[kk].n_age) {
            fprintf(fpOut, "%.6f,%.6f,", age[ll].r_age, age[ll].i_age);
            fprintf(fpOut, "%.6f,", (age[ll].frac_area*0.001));
            fprintf(fpOut, "%.6f,%.6f,", age[ll].r_fdd, age[ll].i_fdd);
          }
          else
            fprintf(fpOut, ",,,,,");
          fprintf(fpOut, "%d,", obs[kk].n_thick);
          if (ll < obs[kk].n_thick)
            fprintf(fpOut, "%.6f,", (thick_far[ll]*0.001));
          else
            fprintf(fpOut, ",");            
          fprintf(fpOut, "%.6f,%.6f,", (obs[kk].far_fyr*0.001), 
            (obs[kk].far_my*0.001));
          fprintf(fpOut, "%d,", obs[kk].n_ridge);
          if (ll < obs[kk].n_ridge) {
            fprintf(fpOut, "%.6f,%.6f,", 
             ridge[ll].r_ridge_ar, ridge[ll].i_ridge_ar);
            fprintf(fpOut, "%.6f,%.6f,", 
             ridge[ll].r_ridge_tr, ridge[ll].i_ridge_tr);
            fprintf(fpOut, "%.6f,", (ridge[ll].ridge_far*0.001));
            fprintf(fpOut, "%.6f,%.6f,", 
              ridge[ll].r_ridge_fdd, ridge[ll].i_ridge_fdd);
            fprintf(fpOut, "%d\n", ridge[ll].ridge_flag);
          }
          else
            fprintf(fpOut, ",,,,,,,\n");
        }
        FREE(age);
        FREE(thick_far);
        FREE(ridge);
      }
      FREE(obs);
    }
  }
  else if (strncmp_case(prod_description, "Backscatter Histogram", 21) == 0) {
    FREAD(&n_cells, 4, 1, fpIn);
    ieee_big32(n_cells);
    FREAD(&create_year, 2, 1, fpIn);
    ieee_big16(create_year);
    FREAD(&create_time, 8, 1, fpIn);
    ieee_big64(create_time);
    FREAD(&prod_start_year, 2, 1, fpIn);
    ieee_big16(prod_start_year);
    FREAD(&prod_start_time, 8, 1, fpIn);
    ieee_big64(prod_start_time);
    FREAD(&prod_end_year, 2, 1, fpIn);
    ieee_big16(prod_end_year);
    FREAD(&prod_end_time, 8, 1, fpIn);
    ieee_big64(prod_end_time);
    FREAD(&sw_version, 12, 1, fpIn);
    FREAD(&n_w_lat, 4, 1, fpIn);
    ieee_big32(n_w_lat);
    FREAD(&n_w_lon, 4, 1, fpIn);
    ieee_big32(n_w_lon);
    FREAD(&n_e_lat, 4, 1, fpIn);
    ieee_big32(n_e_lat);
    FREAD(&n_e_lon, 4, 1, fpIn);
    ieee_big32(n_e_lon);
    FREAD(&s_w_lat, 4, 1, fpIn);
    ieee_big32(s_w_lat);
    FREAD(&s_w_lon, 4, 1, fpIn);
    ieee_big32(s_w_lon);
    FREAD(&s_e_lat, 4, 1, fpIn);
    ieee_big32(s_e_lat);
    FREAD(&s_e_lon, 4, 1, fpIn);
    ieee_big32(s_e_lon);
    
    printf("Number cells: %d\n", n_cells);
    printf("Product creation year: %d\n", create_year);
    printf("Product creation time: %.6f\n", create_time);
    printf("Product start year: %d\n", prod_start_year);
    printf("Product start time: %.6f\n", prod_start_time);
    printf("Product end year: %d\n", prod_end_year);
    printf("Product end time: %.6f\n", prod_end_time);
    snprintf(sw_ver, 5, "%s", sw_version);
    printf("Software version: %s\n", sw_ver);
    printf("NW: %.6f %.6f\n", n_w_lat, n_w_lon);
    printf("NE: %.6f %.6f\n", n_e_lat, n_e_lon);
    printf("SW: %.6f %.6f\n", s_w_lat, s_w_lon);
    printf("SE: %.6f %.6f\n", s_e_lat, s_e_lon);
    westBoundLon = minValue(n_w_lon, n_e_lon, s_w_lon, s_e_lon);
    eastBoundLon = maxValue(n_w_lon, n_e_lon, s_w_lon, s_e_lon);
    northBoundLat = maxValue(n_w_lat, n_e_lat, s_w_lat, s_e_lat);
    southBoundLat = minValue(n_w_lat, n_e_lat, s_w_lat, s_e_lat);
    
    // Backscatter range record
    for (ii=0; ii<25; ii++) {
      FREAD(&bsr_low[ii], 4, 1, fpIn);
      ieee_big32(bsr_low[ii]);
      FREAD(&bsr_high[ii], 4, 1, fpIn);
      ieee_big32(bsr_high[ii]);
      printf("BSR[%2d]: %.6f, %.6f\n", ii+1, bsr_low[ii], bsr_high[ii]);
    }
    // Backscatter histogram data
    fpOut = FOPEN(csvFile, "w");
    fprintf(fpOut, "CELL_ID,BIRTH_YEAR,BIRTH_TIME,I_AREA,N_OBS,OBS_YEAR,"
      "OBS_TIME,MAP_X,MAP_Y,LAT,LON,CELL_TEMP,C_AREA,MYFRAC,OWFRAC,");
    for (ll=0; ll<25; ll++)
      fprintf(fpOut, "FBSR_%d,", ll+1);
    fprintf(fpOut, "INC_ANG\n");
    for (ii=0; ii<n_cells; ii++) {
      FREAD(&cell_id, 4, 1, fpIn);
      ieee_big32(cell_id);
      FREAD(&birth_year, 2, 1, fpIn);
      ieee_big16(birth_year);
      FREAD(&birth_time, 8, 1, fpIn);
      ieee_big64(birth_time);
      FREAD(&init_area, 4, 1, fpIn);
      ieee_big32(init_area);
      FREAD(&n_int_obs, 4, 1, fpIn);
      ieee_big32(n_int_obs);
      
      printf("Cell ID: %d\r", cell_id);
      /*
      printf("Birth: %d, %.6f\n", birth_year, birth_time);
      printf("Initial area: %.6f\n", init_area);
      printf("Number of observations: %d\n", n_obs);
      */
      backscatter_t *obs = 
      	(backscatter_t *) MALLOC(sizeof(backscatter_t)*n_int_obs);
      for (kk=0; kk<n_int_obs; kk++) {
        FREAD(&obs[kk].year, 2, 1, fpIn);
        ieee_big16(obs[kk].year);
        FREAD(&obs[kk].time, 8, 1, fpIn);
        ieee_big64(obs[kk].time);
        FREAD(&obs[kk].map_x, 8, 1, fpIn);
        ieee_big64(obs[kk].map_x);
        FREAD(&obs[kk].map_y, 8, 1, fpIn);
        ieee_big64(obs[kk].map_y);
        FREAD(&obs[kk].center_temp, 4, 1, fpIn);
        ieee_big32(obs[kk].center_temp);
        FREAD(&obs[kk].cell_area, 4, 1, fpIn);
        ieee_big32(obs[kk].cell_area);
        FREAD(&obs[kk].multi_year, 2, 1, fpIn);
        ieee_big16(obs[kk].multi_year);
        FREAD(&obs[kk].open_water, 2, 1, fpIn);
        ieee_big16(obs[kk].open_water);
        for (ll=0; ll<25; ll++) {
          FREAD(&obs[kk].frac_back[ll], 2, 1, fpIn);
          ieee_big16(obs[kk].frac_back[ll]);
        }
        FREAD(&obs[kk].inc_ang, 4, 1, fpIn);
        ieee_big32(obs[kk].inc_ang);
        /*
        printf("\nObservation: %d, %.6f\n", obs[kk].year, obs[kk].time);
        printf("Map: %.6f %.6f\n", obs[kk].map_x, obs[kk].map_y);
        printf("Temperature cell center: %.6f\n", obs[kk].center_temp);
        printf("Area: %.6f\n", obs[kk].cell_area);
        printf("Multiyear ice: %.6f\n", (obs[kk].multi_year)*0.001);
        printf("Open water: %.6f\n", (obs[kk].open_water)*0.001);
        for (ll=0; ll<25; ll++)
          printf("FBSR[%2d]: %.6f\n", ll+1, (obs[kk].frac_back[ll])*0.001);
        printf("Incidence angle: %.6f\n", obs[kk].inc_ang);	
        */
        double map_x = obs[kk].map_x * 1000.0;
        double map_y = obs[kk].map_y * 1000.0;

        // Define projection block
        read_proj_file("polar_stereographic_north_ssmi.proj", 
                 &pps, &proj_type, &datum, &spheroid);
        pps.ps.false_easting = 0.0;
        pps.ps.false_northing = 0.0;
        proj = meta_projection_init();
        proj->type = proj_type;
        proj->datum = HUGHES_DATUM;
        proj->spheroid = spheroid;
        proj->param = pps;
        strcpy(proj->units, "meters");
        proj->hem = 'N';
        spheroid_axes_lengths(spheroid, &proj->re_major, &proj->re_minor);
        proj_to_latlon(proj, map_x, map_y, 0.0, &lat, &lon, &height);
        meta = raw_init();
        meta->projection = proj;
        fprintf(fpOut, "%d,%d,%.6f,", cell_id, birth_year, birth_time);
        fprintf(fpOut, "%.6f,%d,", init_area, n_int_obs);
        fprintf(fpOut, "%d,%.6f,", obs[kk].year, obs[kk].time);
        fprintf(fpOut, "%.4f,%.4f,", map_x, map_y);
        fprintf(fpOut, "%.5f,%.5f,", lat*R2D, lon*R2D);
        fprintf(fpOut, "%.6f,%.6f,", obs[kk].center_temp, obs[kk].cell_area);
        fprintf(fpOut, "%.6f,%.6f,", (obs[kk].multi_year*0.001), 
          (obs[kk].open_water*0.001));
        for (ll=0; ll<25; ll++)
          fprintf(fpOut, "%.6f,", (obs[kk].frac_back[ll]*0.001));
        fprintf(fpOut, "%.6f\n", obs[kk].inc_ang);	
      }
      FREE(obs);
    }
  }
  
  // Generate metadata
  sprintf(isoStr, "%s", iso_date());
  FILE *fpXml = FOPEN(xmlFile, "w");
  fprintf(fpXml, "<rgps>\n");
  fprintf(fpXml, "  <granule>%s</granule>\n", stripExt(csvFile));
  fprintf(fpXml, "  <metadata_creation>%s</metadata_creation>\n", isoStr);
  fprintf(fpXml, "  <metadata>\n");
  fprintf(fpXml, "    <product>\n");
  fprintf(fpXml, "      <file>%s</file>\n", product_id);
  if (strncmp_case(prod_description, "Lagrangian Ice Motion", 21) == 0) {
    fprintf(fpXml, "      <description type=\"string\" definition=\"description"
      " of this product\">Lagrangian Ice Motion</description>\n");
    fprintf(fpXml, "      <type type=\"string\" definition=\"product type "
      "('winter' or 'summer')\">%s</type>\n", prod_type);
    fprintf(fpXml, "      <number_images type=\"int\" definition=\"number of "
      "images used in the creation of this product\">%d</number_images>\n", 
      n_images);
    fprintf(fpXml, "      <number_trajectories type=\"int\" definition=\"number"
      " of trajectories\">%d</number_trajectories>\n", n_trajectories);
  }
  else if (strncmp_case(prod_description, "Ice Deformation", 15) == 0) {
    fprintf(fpXml, "      <description type=\"string\" definition=\"description"
      " of this product\">Ice Deformation</description>\n");
    fprintf(fpXml, "      <number_cells type=\"int\" definition=\"number of "
      "cells\">%d</number_cells>\n", n_cells);
  }
  else if (strncmp_case(prod_description, "Ice Age", 7) == 0) {
    fprintf(fpXml, "      <description type=\"string\" definition=\"description"
      " of this product\">Ice Age/Thickness Histogram</description>\n");
    fprintf(fpXml, "      <number_cells type=\"int\" definition=\"number of "
      "cells\">%d</number_cells>\n", n_cells);
    fprintf(fpXml, "      <thickness_interval>%.2f</thickness_interval>\n",
      thick_step);
  }
  else if (strncmp_case(prod_description, "Backscatter Histogram", 21) == 0) {
    fprintf(fpXml, "      <description type=\"string\" definition=\"description"
      " of this product\">Backscatter Histogram</description>\n");
    fprintf(fpXml, "      <number_cells type=\"int\" definition=\"number of "
      "cells\">%d</number_cells>\n", n_cells);
    fprintf(fpXml, "      <backscatter_histogram>\n");
    for (ii=0; ii<25; ii++) {
      fprintf(fpXml, "        <range id=\"%d\" units=\"dB\">\n", ii+1);
      fprintf(fpXml, "          <minimum>%.2f</minimum>\n", bsr_low[ii]);
      fprintf(fpXml, "          <maximum>%.2f</maximum>\n", bsr_high[ii]);
      fprintf(fpXml, "        </range>\n");
    }
    fprintf(fpXml, "      </backscatter_histogram>\n");
  }
  rgps2iso_date(prod_start_year, prod_start_time, dateStr);
  snprintf(citation, 11, "%s", dateStr);
  strcat(citation, " to ");
  fprintf(fpXml, "      <start_datetime type=\"string\" definition=\"start "
    "time of the image acquisition for the product \">%s</start_datetime>\n", 
    dateStr);
  rgps2iso_date(prod_end_year, prod_end_time, dateStr);
  strcat(citation, dateStr);
  citation[24] = '\0';
  fprintf(fpXml, "      <end_datetime type=\"string\" definition=\"end time of"
    " the image acquisition for the product\">%s</end_datetime>\n", dateStr);
  fprintf(fpXml, "      <start_year type=\"int\" definition=\"product start "
    "year\">%d</start_year>\n", prod_start_year);
  fprintf(fpXml, "      <start_day type=\"double\" definition=\"product start "
    "fractional day of the year\">%.6f</start_day>\n", prod_start_time);
  fprintf(fpXml, "      <end_year type=\"int\" definition=\"product end "
    "year\">%d</end_year>\n", prod_end_year);
  fprintf(fpXml, "      <end_day type=\"double\" definition=\"product end "
    "fractional day of the year\">%.6f</end_day>\n", prod_end_time);
  fprintf(fpXml, "      <projection_string type=\"string\" definition=\"map "
    "projection information as well known text\">%s</projection_string>\n", 
    meta2esri_proj(meta, NULL));
  fprintf(fpXml, "    </product>\n");
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
  fprintf(fpXml, "    </product>\n");
  fprintf(fpXml, "  </extent>\n");
  fprintf(fpXml, "  <processing>\n");
  rgps2iso_date(create_year, create_time, dateStr);
  fprintf(fpXml, "    <creation_time>%s</creation_time>\n", dateStr);
  fprintf(fpXml, "    <software_version>%s</software_version>\n", sw_ver);
  fprintf(fpXml, "  </processing>\n");
  fprintf(fpXml, "  <root>\n");
  fprintf(fpXml, "    <institution>Alaska Satellite Facility</institution>\n");
  fprintf(fpXml, "    <title>Kwok, Ron. 2008. MEaSUREs Small-Scale Kinematics of"
    " Arctic Ocean Sea Ice, Version 01, %s. Jet Propulsion Laboratory "
    "Pasadena, CA USA and Alaska Satellite Facility Fairbanks, AK USA. Digital "
    "media.</title>\n", citation);
  fprintf(fpXml, "    <source>Products derived from RADARSAT-1 SWB imagery at "
    "100 m resolution</source>\n");
  fprintf(fpXml, "    <comment>Imagery the products are derived from: Copyright "
    "Canadian Space Agency (1996 to 2008)</comment>\n");
  fprintf(fpXml, "    <reference>Documentation available at: www.asf.alaska.edu"
    "</reference>\n");
  fprintf(fpXml, "    <history>%s: CSV file created.</history>\n", isoStr);
  fprintf(fpXml, "  </root>\n");
  fprintf(fpXml, "</rgps>\n");
  FCLOSE(fpXml);
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(inFile);
  FREE(csvFile);
  FREE(xmlFile);
  meta_free(meta);
  
  return 0;
}
