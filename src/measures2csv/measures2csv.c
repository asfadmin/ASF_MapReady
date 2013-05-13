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

int main(int argc, char **argv)
{
  FILE *fpIn, *fpOut, *fpOut2, *fpOut3;
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
  
  // General header information
  char pid[24];	             // RGPS Product Identifier
  char prod_description[40]; // Description of this product
  short	n_images;	 // Number of images used in the create of this product
  int n_trajectories;        // Number of trajectories
  int n_cells;               // Number of cells
  char prod_type[8];         // Product type
  short	create_year;	     // Product creation year
  double create_time;	     // Product creation time
  short prod_start_year;     // Product start year
  double prod_start_time;    // Product start time
  short	prod_end_year;       // Product end year
  double prod_end_time;	     // Product end time
  char sw_version[12];       // Software version used to create this product
  float	n_w_lat;             // North West Latitude of initial datatake
  float	n_w_lon;             // North West Longitude of inital datatake
  float	n_e_lat;             // North East Latitude of initial datatake
  float	n_e_lon;             // North East Longitude of initial datatake
  float	s_w_lat;             // South West Latitude of initial datatake
  float	s_w_lon;             // South West Longitude of initial datatake
  float	s_e_lat;             // South West Latitude of initial datatake
  float	s_e_lon;             // South West Longitude of initial datatake
  float thick_step;          // Interval of each thickness category
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
  julian_date jd;
  ymd_date ymd;
  hms_time hms;
  double sec;
  project_parameters_t pps;
  projection_type_t proj_type;
  datum_type_t datum;
  spheroid_type_t spheroid;
  meta_projection *proj;
  
  fpIn = FOPEN(inFile, "rb");

  FREAD(&pid, 24, 1, fpIn);
  FREAD(&prod_description, 40, 1, fpIn);
  
  printf("PID: %s\n", pid);
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
    printf("Software version: %s\n", sw_version);
    printf("NW: %.6f %.6f\n", n_w_lat, n_w_lon);
    printf("NE: %.6f %.6f\n", n_e_lat, n_e_lon);
    printf("SW: %.6f %.6f\n", s_w_lat, s_w_lon);
    printf("SE: %.6f %.6f\n", s_e_lat, s_e_lon);
    
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
    fprintf(fpOut, "GPID, OBJ_JUL, OBS_DATE, N_OBS, IMAGE, MAP_X, MAP_Y, LAT, "
	    "LON, Q_FLAG\n");
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
	char date_str[25], *image_id;
	double map_x = obs[kk].map_x * 1000.0;
	double map_y = obs[kk].map_y * 1000.0;

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
	jd.year = obs[kk].year;
	jd.jd = (int) obs[kk].time;
	sec = 86400 * (obs[kk].time - jd.jd);
	date_jd2ymd(&jd, &ymd);
	date_sec2hms(sec, &hms);
	sprintf(date_str, "%4d-%02d-%02d %02d:%02d:%06.3f",
		ymd.year, ymd.month, ymd.day, hms.hour, hms.min, hms.sec);
	for (ll=0; ll<n_images; ll++) {
	  if ((obs[kk].year == image[ll].year) &&
	      FLOAT_COMPARE_TOLERANCE(obs[kk].time, image[ll].time, 0.000001))
	    image_id = STRDUP(image[ll].id);
	}
	fprintf(fpOut, "%d, %.6f, %s, %d,", gpid, obs[kk].time, date_str, kk+1);
	fprintf(fpOut, " %s, %.3f, %.3f, ", image_id, map_x, map_y);
	fprintf(fpOut, "%.5f, %.5f, ", lat*R2D, lon*R2D);
	fprintf(fpOut, "%d\n", obs[kk].q_flag);
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
    printf("Software version: %s\n", sw_version);
    printf("NW: %.6f %.6f\n", n_w_lat, n_w_lon);
    printf("NE: %.6f %.6f\n", n_e_lat, n_e_lon);
    printf("SW: %.6f %.6f\n", s_w_lat, s_w_lon);
    printf("SE: %.6f %.6f\n", s_e_lat, s_e_lon);
    
    fpOut = FOPEN(csvFile, "w");
    fprintf(fpOut, "CELL_ID, OBS_JUL, OBS_DATE, N_OBS, MAP_X, MAP_Y, LAT, LON, "
	    "DISP_X, DISP_Y, C_AREA, D_AREA, DTP, DUDX, DUDY, DVDX, DVDY\n");
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
	char date_str[25];
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
	jd.year = obs[kk].year;
	jd.jd = (int) obs[kk].time;
	sec = 86400 * (obs[kk].time - jd.jd);
	date_jd2ymd(&jd, &ymd);
	date_sec2hms(sec, &hms);
	sprintf(date_str, "%4d-%02d-%02d %02d:%02d:%06.3f",
		ymd.year, ymd.month, ymd.day, hms.hour, hms.min, hms.sec);
	fprintf(fpOut, "%d, %.6f, %s, ", cell_id, obs[kk].time, date_str);
	fprintf(fpOut, "%d, %.3f, %.3f, ", kk+1, map_x, map_y);
	fprintf(fpOut, "%.5f, %.5f, ", lat*R2D, lon*R2D);
	fprintf(fpOut, "%.6f, %.6f, ", obs[kk].disp_x, obs[kk].disp_y);
	fprintf(fpOut, "%.6f, %.6f, ", obs[kk].cell_area, obs[kk].diff_area);
	fprintf(fpOut, "%.6f, ", obs[kk].dtp);
	fprintf(fpOut, "%.6f, %.6f, ", obs[kk].dudx, obs[kk].dudy);
	fprintf(fpOut, "%.6f, %.6f\n", obs[kk].dvdx, obs[kk].dvdy);
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
    printf("Software version: %s\n", sw_version);
    printf("NW: %.6f %.6f\n", n_w_lat, n_w_lon);
    printf("NE: %.6f %.6f\n", n_e_lat, n_e_lon);
    printf("SW: %.6f %.6f\n", s_w_lat, s_w_lon);
    printf("SE: %.6f %.6f\n", s_e_lat, s_e_lon);
    printf("Thickness interval: %.6f\n", thick_step);
    
    char *ageFile = (char *) MALLOC(sizeof(char)*(strlen(csvFile)+10));
    create_name(ageFile, csvFile, "_age.csv");
    char *thickFile = (char *) MALLOC(sizeof(char)*(strlen(csvFile)+10));
    create_name(thickFile, csvFile, "_thick.csv");
    char *ridgeFile = (char *) MALLOC(sizeof(char)*(strlen(csvFile)+10));
    create_name(ridgeFile, csvFile, "_ridge.csv");

    fpOut = FOPEN(ageFile, "w");
    fprintf(fpOut, "CELL_ID, OBS_JUL, OBS_DATE, N_OBS, MAP_X, MAP_Y, LAT, LON, "
	    "C_TEMP, FDD, C_AREA, FAR_FYR, FAR_MY, BIN, R_AGE, I_AGE, AGE_FAR, "
	    "R_FDD, I_FDD\n");
    fpOut2 = FOPEN(thickFile, "w");
    fprintf(fpOut2, "CELL_ID, OBS_JUL, OBS_DATE, N_OBS, MAP_X, MAP_Y, LAT, LON,"
	    " C_TEMP, FDD, C_AREA, FAR_FYR, FAR_MY, N_THICK, THICK_FAR\n");
    fpOut3 = FOPEN(ridgeFile, "w");
    fprintf(fpOut3, "CELL_ID, OBS_JUL, OBS_DATE, N_OBS, MAP_X, MAP_Y, LAT, LON,"
	    " C_TEMP, FDD, C_AREA, FAR_FYR, FAR_MY, R_RIDGE_AR, I_RIDGE_AR, "
	    "R_RIDGE_TR, I_RIDGE_TR, RIDGE_FAR, R_RIDGE_FDD, I_RIDGE_FDD, "
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
      printf("Number of observations: %d\n", n_obs);
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
	char date_str[25];
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
	jd.year = obs[kk].year;
	jd.jd = (int) obs[kk].time;
	sec = 86400 * (obs[kk].time - jd.jd);
	date_jd2ymd(&jd, &ymd);
	date_sec2hms(sec, &hms);
	sprintf(date_str, "%4d-%02d-%02d %02d:%02d:%06.3f",
		ymd.year, ymd.month, ymd.day, hms.hour, hms.min, hms.sec);
      
	// Read ice age information
	ice_age_t *age;
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
	short *thick_far;
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
	ice_ridge_t *ridge;
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
	if (obs[kk].n_age > 0) {
	  for (ll=0; ll<obs[kk].n_age; ll++) {
	    fprintf(fpOut, "%d, %.6f, %s, ", cell_id, obs[kk].time, date_str);
	    fprintf(fpOut, "%d, %.3f, %.3f, ", ll+1, map_x, map_y);
	    fprintf(fpOut, "%.5f, %.5f, ", lat*R2D, lon*R2D);
	    fprintf(fpOut, "%.6f, %.6f, ", obs[kk].center_temp, obs[kk].fdd);
	    fprintf(fpOut, "%.6f, ", obs[kk].cell_area);
	    fprintf(fpOut, "%.6f, %.6f, ", 
		    (obs[kk].far_fyr*0.001), (obs[kk].far_my*0.001));
	    fprintf(fpOut, "%d, ", ll+1);
	    fprintf(fpOut, "%.6f, %.6f, ", age[ll].r_age, age[ll].i_age);
	    fprintf(fpOut, "%.6f, ", (age[ll].frac_area*0.001));
	    fprintf(fpOut, "%.6f, %.6f\n", age[ll].r_fdd, age[ll].i_fdd);
	  }
	  FREE(age);
	}
	if (obs[kk].n_thick > 0) {
	  for (ll=0; ll<obs[kk].n_thick; ll++) {
	    fprintf(fpOut2, "%d, %.6f, %s, ", cell_id, obs[kk].time, date_str);
	    fprintf(fpOut2, "%d, %.3f, %.3f, ", ll+1, map_x, map_y);
	    fprintf(fpOut2, "%.5f, %.5f, ", lat*R2D, lon*R2D);
	    fprintf(fpOut2, "%.6f, %.6f, ", obs[kk].center_temp, obs[kk].fdd);
	    fprintf(fpOut2, "%.6f, ", obs[kk].cell_area);
	    fprintf(fpOut2, "%.6f, %.6f, ", 
		    (obs[kk].far_fyr*0.001), (obs[kk].far_my*0.001));
	    fprintf(fpOut2, "%d, %.6f", ll+1, (thick_far[ll]*0.001));
	  }
	  FREE(thick_far);
	}
	if (obs[kk].n_ridge > 0) {
	  for (ll=0; ll<obs[kk].n_ridge; ll++) {
	    fprintf(fpOut3, "%d, %.6f, %s, ", cell_id, obs[kk].time, date_str);
	    fprintf(fpOut3, "%d, %.3f, %.3f, ", ll+1, map_x, map_y);
	    fprintf(fpOut3, "%.5f, %.5f, ", lat*R2D, lon*R2D);
	    fprintf(fpOut3, "%.6f, %.6f, ", obs[kk].center_temp, obs[kk].fdd);
	    fprintf(fpOut3, "%.6f, ", obs[kk].cell_area);
	    fprintf(fpOut3, "%.6f, %.6f, ", 
		    (obs[kk].far_fyr*0.001), (obs[kk].far_my*0.001));
	    fprintf(fpOut3, "%d, ", ll+1);
	    fprintf(fpOut3, "%.6f, %.6f, ", 
		   ridge[ll].r_ridge_ar, ridge[ll].i_ridge_ar);
	    fprintf(fpOut3, "%.6f, %.6f, ", 
		   ridge[ll].r_ridge_tr, ridge[ll].i_ridge_tr);
	    fprintf(fpOut3, "%d, ", ridge[ll].ridge_far);
	    fprintf(fpOut3, "%.6f, %.6f, ", 
		    ridge[ll].r_ridge_fdd, ridge[ll].i_ridge_fdd);
	    fprintf(fpOut3, "%d\n", ridge[ll].ridge_flag);
	  }
	  FREE(ridge);
	}
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
    printf("Software version: %s\n", sw_version);
    printf("NW: %.6f %.6f\n", n_w_lat, n_w_lon);
    printf("NE: %.6f %.6f\n", n_e_lat, n_e_lon);
    printf("SW: %.6f %.6f\n", s_w_lat, s_w_lon);
    printf("SE: %.6f %.6f\n", s_e_lat, s_e_lon);
    
    // Backscatter range record
    float bsr_low[25], bsr_high[25];
    for (ii=0; ii<25; ii++) {
      FREAD(&bsr_low[ii], 4, 1, fpIn);
      ieee_big32(bsr_low[ii]);
      FREAD(&bsr_high[ii], 4, 1, fpIn);
      ieee_big32(bsr_high[ii]);

      //printf("BSR[%2d]: %.6f, %.6f\n", ii+1, bsr_low[ii], bsr_high[ii]);
    }
    // Backscatter histogram data
    fpOut = FOPEN(csvFile, "w");
    fprintf(fpOut, "CELL_ID, OBS_JUL, OBS_DATE, N_OBS, MAP_X, MAP_Y, LAT, LON, "
	    "CELL_TEMP, C_AREA, MYFRAC, OWFRAC, ");
    for (ll=0; ll<25; ll++)
      fprintf(fpOut, "FBSR_%d, ", ll+1);
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
	char date_str[25];
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
	jd.year = obs[kk].year;
	jd.jd = (int) obs[kk].time;
	sec = 86400 * (obs[kk].time - jd.jd);
	date_jd2ymd(&jd, &ymd);
	date_sec2hms(sec, &hms);
	sprintf(date_str, "%4d-%02d-%02d %02d:%02d:%06.3f",
		ymd.year, ymd.month, ymd.day, hms.hour, hms.min, hms.sec);
	fprintf(fpOut, "%d, %.6f, %s, ", cell_id, obs[kk].time, date_str);
	fprintf(fpOut, "%d, %.3f, %.3f, ", kk+1, map_x, map_y);
	fprintf(fpOut, "%.5f, %.5f, ", lat*R2D, lon*R2D);
	fprintf(fpOut, "%.6f, %.6f, ", obs[kk].center_temp, obs[kk].cell_area);
	fprintf(fpOut, "%.6f, %.6f, ", (obs[kk].multi_year*0.001), 
		(obs[kk].open_water*0.001));
	for (ll=0; ll<25; ll++)
	  fprintf(fpOut, "%.6f, ", (obs[kk].frac_back[ll]*0.001));
	fprintf(fpOut, "%.6f\n", obs[kk].inc_ang);	
      }
      FREE(obs);
    }
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(inFile);
  FREE(csvFile);
  
  return 0;
}
