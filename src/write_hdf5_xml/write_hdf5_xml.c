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
#include "asf_meta.h"
#include "asf_license.h"
#include "dateUtil.h"
#include "time.h"
#include <string.h>
#include <hdf5.h>

#define ASF_NAME_STRING "write_hdf5_xml"
#define VERSION 1.0

typedef struct {
	char *granule;
	char *master_meta_file;
	char *slave_meta_file;
	char *master_metadata;
	char *slave_metadata;
	char *doppler;
	char *processing_log;
	char *baseline;
	char *wrapped_interferogram;
	char *unwrapped_interferogram;
	char *correlation;
	char *browse_amplitude;
	char *browse_wrapped_interferogram;
	char *browse_unwrapped_interferogram;
	char *browse_correlation;
	char *incidence_angle;
	char *digital_elevation_model;
	char *dem_url;
	char *troposphere;
	char *troposphere_url;
  double master_faraday_rotation;
	double slave_faraday_rotation;
} params_t;

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [ -log <logFile> ] <parameter_list> <xml_name>\n", name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   parameter_list  List of parameters.\n"
	 "   xml_name        Base name of the XML file.\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s converts a file list to an .xml file for HDF5 exporting.\n",
	 name);
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

/*
static void meta2iso_date(meta_parameters *meta, 
	char *begin, char *center, char *end)
{
  julian_date julian;
  hms_time time;
  ymd_date date;
  double begin_time, center_time, end_time;

  julian.year = meta->state_vectors->year;
  julian.jd = meta->state_vectors->julDay;
	date_jd2ymd(&julian, &date);
	// ALOS Palsar is different
	if (strcmp_case(meta->general->sensor, "ALOS") != 0) {
  	begin_time = 
  		meta->state_vectors->second + meta->state_vectors->vecs[0].time;
  	date_sec2hms(begin_time, &time);
  	sprintf(begin, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ", 
  		date.year, date.month, date.day, time.hour, time.min, time.sec);
  	center_time = 
  		meta->state_vectors->second + meta->state_vectors->vecs[1].time;
  	date_sec2hms(center_time, &time);
  	sprintf(center, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ", 
  		date.year, date.month, date.day, time.hour, time.min, time.sec);
  	end_time = 
  		meta->state_vectors->second + meta->state_vectors->vecs[2].time;
  	date_sec2hms(end_time, &time);
  	sprintf(end, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ", 
  		date.year, date.month, date.day, time.hour, time.min, time.sec);
  }
  else {
  	center_time = meta_get_time(meta, meta->general->line_count/2, 0.0);
  	center_time += meta->state_vectors->second;
  	center_time += meta->state_vectors->vecs[0].time;
  	date_sec2hms(center_time, &time);
  	sprintf(center, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ", 
  		date.year, date.month, date.day, time.hour, time.min, time.sec);
  }
}

static void line2iso_date(char *line, char *time)
{
  char buf[100];
#define subStr(start,len,dest) strncpy(buf,&line[start],len);buf[len]=0;sscanf(buf,"%d",dest);
	int year, month, day, hour, min, sec; 
	subStr(0,2,&month);
	subStr(3,2,&day);
	subStr(6,4,&year);
	subStr(11,2,&hour);
	subStr(14,2,&min);
	subStr(17,2,&sec);
	strncpy(buf, &line[20], 2);
	if (strncmp_case(buf, "PM", 2) == 0)
		hour += 12;
	sprintf(time, "%4d-%02d-%02dT%02d:%02d:%02d.000000Z", 
		year, month, day, hour, min, sec);
}
*/

static void ymd_hms2iso_date(ymd_date *date, hms_time *time, char *isoStr)
{
  sprintf(isoStr, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ", 
  	date->year, date->month, date->day, time->hour, time->min, time->sec);
}

static char *xml_encode(char *xml_str)
{
  static char buf[1024];
  strcpy(buf, "");

  if (xml_str)
  {
    char *a = asf_strReplace(xml_str, "&", "&amp;");
    char *b = asf_strReplace(a, "<", "&lt;");
    char *c = asf_strReplace(b, ">", "&gt;");
    char *d = asf_strReplace(c, "\"", "&quot;");
    char *e = asf_strReplace(d, "'", "&apos;");
    FREE(a); FREE(b); FREE(c); FREE(d);

    strcpy(buf, e);
    FREE(e);
  }

  return buf;
}

static void roi2iso_date(char *line, char *time)
{
  char buf[100];
#define subStr(start,len,dest) strncpy(buf,&line[start],len);buf[len]=0;sscanf(buf,"%d",dest);
	int year, month, day, hour, min, sec; 
	subStr(0,4,&year);
	subStr(4,2,&month);
	subStr(6,2,&day);
	subStr(9,2,&hour);
	subStr(11,2,&min);
	subStr(13,2,&sec);
	sprintf(time, "%4d-%02d-%02dT%02d:%02d:%02d.000000Z", 
		year, month, day, hour, min, sec);
}

params_t *params_init(char *type) {

	params_t *params = (params_t *) MALLOC(sizeof(params_t));
	params->granule = NULL;
	params->master_meta_file = NULL;
	params->slave_meta_file = NULL;
	params->master_metadata = NULL;
	params->slave_metadata = NULL;
	params->doppler = NULL;
	params->processing_log = NULL;
	params->baseline = NULL;
	params->wrapped_interferogram = NULL;
	params->unwrapped_interferogram = NULL;
	params->correlation = NULL;
	params->incidence_angle = NULL;
	params->digital_elevation_model = NULL;
	params->dem_url = NULL;
	params->troposphere = NULL;
	params->troposphere_url = NULL;
	params->browse_amplitude = NULL;
	params->browse_wrapped_interferogram = NULL;
	params->browse_unwrapped_interferogram = NULL;
	params->browse_correlation = NULL;
	params->master_faraday_rotation = MAGIC_UNSET_DOUBLE;
	params->slave_faraday_rotation = MAGIC_UNSET_DOUBLE;
	
	return params;
}

void params_free(params_t *params) {
	if (params->granule)
		FREE(params->granule);
	if (params->master_meta_file)
		FREE(params->master_meta_file);
	if (params->slave_meta_file)
		FREE(params->slave_meta_file);
	if (params->master_metadata)
		FREE(params->master_metadata);
	if (params->slave_metadata)
		FREE(params->slave_metadata);
	if (params->doppler)
		FREE(params->doppler);
	if (params->processing_log)
		FREE(params->processing_log);
	if (params->baseline)
		FREE(params->baseline);
	if (params->wrapped_interferogram)
		FREE(params->wrapped_interferogram);
	if (params->unwrapped_interferogram)
		FREE(params->unwrapped_interferogram);
	if (params->correlation)
		FREE(params->correlation);
	if (params->incidence_angle)
		FREE(params->incidence_angle);
	if (params->digital_elevation_model)
		FREE(params->digital_elevation_model);
	if (params->dem_url)
	  FREE(params->dem_url);
	if (params->troposphere)
		FREE(params->troposphere);
	if (params->troposphere_url)
	  FREE(params->troposphere_url);
	if (params->browse_amplitude)
		FREE(params->browse_amplitude);
	if (params->browse_wrapped_interferogram)
		FREE(params->browse_wrapped_interferogram);
	if (params->browse_unwrapped_interferogram)
		FREE(params->browse_unwrapped_interferogram);
	if (params->browse_correlation)
		FREE(params->browse_correlation);
	FREE(params);
}

void read_filenames(const char * file, params_t **params, char *type)
{
  int n = 512;
  char line[n], *p, value[n];
  params_t *list = NULL;
  sprintf(type, "%s", MAGIC_UNSET_STRING);
  FILE *fp = FOPEN(file, "rt");
  if (!fp)
    asfPrintError("Couldn't open file list: %s\n", file);
  fgets(line, n, fp);
  if (strncmp_case(line, "[InSAR]", 7) == 0) {
  	list = params_init("INSAR");
  	sprintf(type, "InSAR");
		while (NULL != fgets(line, n, fp)) {
			p = strchr(line, '=');
			sprintf(value, "%s", p+1);
			if (strncmp_case(line, "granule", 7) == 0) {
				list->granule = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->granule, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "master meta file", 16) == 0) {
				list->master_meta_file = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->master_meta_file, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "slave meta file", 15) == 0) {
				list->slave_meta_file = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->slave_meta_file, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "master metadata", 15) == 0) {
				list->master_metadata = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->master_metadata, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "slave metadata", 14) == 0) {
				list->slave_metadata = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->slave_metadata, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "doppler", 7) == 0) {
				list->doppler = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->doppler, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "processing log", 14) == 0) {
				list->processing_log = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->processing_log, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "baseline", 8) == 0) {
				list->baseline = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->baseline, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "wrapped interferogram", 21) == 0) {
				list->wrapped_interferogram = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->wrapped_interferogram, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "unwrapped interferogram", 23) == 0) {
				list->unwrapped_interferogram = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->unwrapped_interferogram, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "correlation", 11) == 0) {
				list->correlation = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->correlation, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "incidence angle", 15) == 0) {
				list->incidence_angle = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->incidence_angle, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "digital elevation model", 23) == 0) {
				list->digital_elevation_model = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->digital_elevation_model, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "dem url", 7) == 0) {
				list->dem_url = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->dem_url, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "troposphere file", 16) == 0) {
				list->troposphere = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->troposphere, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "troposphere url", 15) == 0) {
				list->troposphere_url = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->troposphere_url, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "browse amplitude", 16) == 0) {
				list->browse_amplitude = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->browse_amplitude, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "browse wrapped interferogram", 28) == 0) {
				list->browse_wrapped_interferogram = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->browse_wrapped_interferogram, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "browse unwrapped interferogram", 30) == 0) {
				list->browse_unwrapped_interferogram = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->browse_unwrapped_interferogram, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "browse correlation", 18) == 0) {
				list->browse_correlation = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->browse_correlation, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "master faraday rotation", 23) == 0)
				list->master_faraday_rotation = atof(trim_spaces(value));
			if (strncmp_case(line, "slave faraday rotation", 22) == 0)
				list->slave_faraday_rotation = atof(trim_spaces(value));
		}
  }
  *params = list;
  
  FCLOSE(fp);
}

int main(int argc, char **argv)
{
  char file_list[1024], file_name[512], xml_name[1024], *logFileName=NULL;
  int currArg = 1;
  int NUM_ARGS = 2;

  if (argc<=1)
      usage(ASF_NAME_STRING);

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-log","--log",NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
      quietflag = TRUE;
    }
    else {
      --currArg;
      break;
    }
  }

  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    usage(argv[0]);
  }

  sprintf(file_list, "%s", argv[currArg]);
  sprintf(xml_name, "%s", argv[currArg+1]);

  asfSplashScreen(argc, argv);

  FILE *fpFiles = NULL;
  params_t *params = NULL;
  char line[255], str[50], isoStr[50], rscFile[512], logFile[512];
  char directory[25], filename[30], creation[30], map_projection[50];
  char *type = (char *) MALLOC(sizeof(char)*10);
  double seconds;
  int browse = FALSE, dem = FALSE, tropo = FALSE, stats = FALSE;
  hms_time hms, midnight;
  ymd_date ymd, start;
  midnight.hour = 0;
  midnight.min = 0;
  midnight.sec = 0.0;

  // Read file list information
  sprintf(creation, "%s", iso_date());
  read_filenames(file_list, &params, type);
  if (strcmp_case(type, "INSAR") == 0) {

    // Let's check the files first
    // The only ones we require are master/slave metadata and the geocoded
    // products. All other missing files would lead to incomplete records,
    // but should not generate errors in the process.
    if (params->master_meta_file && !fileExists(params->master_meta_file))
      asfPrintError("Master metadata file (%s) is missing!\n", 
        params->master_meta_file);
    if (params->slave_meta_file && !fileExists(params->slave_meta_file))
      asfPrintError("Slave metadata file (%s) is missing!\n", 
        params->slave_meta_file);
    if (params->master_metadata && !fileExists(params->master_metadata))
      asfPrintError("Master metadata file (%s) is missing!\n", 
        params->master_metadata);
    if (params->slave_metadata && !fileExists(params->slave_metadata))
      asfPrintError("Slave metadata file (%s) is missing!\n", 
        params->slave_metadata);
    if (params->wrapped_interferogram) {    
      sprintf(file_name, "%s.rsc", params->wrapped_interferogram);
      if (!fileExists(file_name))
        asfPrintError("Wrapped interferogram (%s) is missing!\n", file_name);
    }
    if (params->unwrapped_interferogram) {
      sprintf(file_name, "%s.rsc", params->unwrapped_interferogram);
      if (!fileExists(file_name))
        asfPrintError("Unwrapped interferogram (%s) is missing!\n", file_name);
    }
    if (params->correlation) {
      sprintf(file_name, "%s.rsc", params->correlation);
      if (!fileExists(file_name))
        asfPrintError("Correlation file (%s) is missing!\n", file_name);
    }
    if (params->incidence_angle) {
      sprintf(file_name, "%s.rsc", params->incidence_angle);
      if (!fileExists(file_name))
        asfPrintError("Incidence angle map (%s) is missing!\n", file_name);
    }
    if ((params->browse_amplitude && fileExists(params->browse_amplitude)) || 
        (params->browse_wrapped_interferogram && 
          fileExists(params->browse_wrapped_interferogram)) || 
        (params->browse_unwrapped_interferogram &&
          fileExists(params->browse_unwrapped_interferogram)) || 
        (params->browse_correlation && fileExists(params->browse_correlation)))
      browse = TRUE;
    if (params->browse_amplitude && !fileExists(params->browse_amplitude))
      asfPrintError("Browse image of amplitude (%s) is missing!\n",
        params->browse_amplitude);
    if (params->browse_wrapped_interferogram && 
          !fileExists(params->browse_wrapped_interferogram))
      asfPrintError("Browse image of wrapped interferogram (%s) is missing!\n",
        params->browse_wrapped_interferogram);
    if (params->browse_unwrapped_interferogram &&
          !fileExists(params->browse_unwrapped_interferogram))
      asfPrintError("Browse image of unwrapped interferogram (%s) is missing!"
        "\n", params->browse_unwrapped_interferogram);
    if (params->browse_correlation && !fileExists(params->browse_correlation))
      asfPrintError("Browse image of correlation (%s) is missing!\n",
        params->browse_correlation);

    meta_parameters *meta = NULL;
    FILE *fp = FOPEN(xml_name, "wt");
    fprintf(fp, "<hdf5>\n");
    fprintf(fp, "  <granule>%s</granule>\n", params->granule);
    fprintf(fp, "  <metadata_creation>%s</metadata_creation>\n", creation);

    // Fill in the data section
    fprintf(fp, "  <data>\n");
    fprintf(fp, "    <wrapped_interferogram>%s</wrapped_interferogram>\n",
      params->wrapped_interferogram);
    fprintf(fp, "    <unwrapped_interferogram>%s</unwrapped_interferogram>\n", 
      params->unwrapped_interferogram);
    fprintf(fp, "    <correlation>%s</correlation>\n", params->correlation);
    fprintf(fp, "    <incidence_angle>%s</incidence_angle>\n", 
      params->incidence_angle);
    if (params->digital_elevation_model) {
      sprintf(file_name, "%s.img", params->digital_elevation_model);
      if (fileExists(file_name))
        fprintf(fp, "    <digital_elevation_model>%s</digital_elevation_model>\n",
          params->digital_elevation_model);
    }
    if (params->troposphere) {
      sprintf(file_name, "%s.img", params->troposphere);
      if (fileExists(file_name))
        fprintf(fp, "    <troposphere>%s</troposphere>\n", params->troposphere);
    }
    fprintf(fp, "  </data>\n");
    
    fprintf(fp, "  <metadata>\n");
    if (params->master_meta_file) {
      fprintf(fp, "    <master_image>\n");

      // Read ASF master metadata
      meta = meta_read(params->master_meta_file);
      fprintf(fp, "      <file type=\"string\" definition=\"file name of the "
        "master image\">%s</file>\n", meta->general->basename);
      fprintf(fp, "      <platform type=\"string\" definition=\"name of the "
        "platform\">%s</platform>\n", meta->general->sensor);
      if (strcmp_case(meta->general->sensor, "ALOS") == 0 && 
      strcmp_case(meta->general->sensor_name, "SAR") == 0)
        fprintf(fp, "      <sensor type=\"string\" definition=\"name of the "
          "sensor\">PALSAR</sensor>\n");
      else
        fprintf(fp, "      <sensor type=\"string\" definition=\"name of the "
          "sensor\">%s</sensor>\n", meta->general->sensor_name);
      fprintf(fp, "      <wavelength type=\"double\" definition=\"wavelength of "
        "the sensor\" units=\"m\">%g</wavelength>\n", meta->sar->wavelength);
      fprintf(fp, "      <beam_mode type=\"string\" definition=\"beam mode of the"
        " sensor\">%s</beam_mode>\n", meta->general->mode);
      fprintf(fp, "      <absolute_orbit type=\"int\" definition=\"absolute orbit"
        " of the image\">%d</absolute_orbit>\n", meta->general->orbit);
      fprintf(fp, "      <frame type=\"int\" definition=\"frame number of the "
        "image\">%d</frame>\n",  meta->general->frame);
      if (meta->general->orbit_direction == 'A')
        fprintf(fp, "      <flight_direction type=\"string\" definition=\"flight "
        "direction of the sensor\">ascending</flight_direction>\n");
      else if (meta->general->orbit_direction == 'D')
        fprintf(fp, "      <flight_direction type=\"string\" definition=\"flight "
        "direction of the sensor\">descending</flight_direction>\n");
      fprintf(fp, "      <polarization type=\"string\" definition=\"polarization "
        "of the transmitted and received signal\">%s</polarization>\n", 
        meta->sar->polarization);
      fprintf(fp, "      <data_processing_level type=\"string\" definition=\""
        "processing level of the input data\">raw</data_processing_level>\n");
      fprintf(fp, "      <data_format type=\"string\" definition=\"data format "
        "of input data\">CEOS</data_format>\n");
      fprintf(fp, "      <prf type=\"double\" definition=\"pulse repetition "
        "frequency\" units=\"Hz\">%g</prf>\n", meta->sar->prf);
      meta_free(meta);

      // Read ROI_PAC master metadata
      fpFiles = FOPEN(params->master_metadata, "rt");
      while (NULL != fgets(line, 255, fpFiles)) {
        sprintf(str, "%s", line+40);
        if (strncmp_case(line, "FIRST_LINE_YEAR", 15) == 0)
          start.year = atoi(trim_spaces(str));
        if (strncmp_case(line, "FIRST_LINE_MONTH_OF_YEAR", 24) == 0)
          start.month = atoi(trim_spaces(str));
        if (strncmp_case(line, "FIRST_LINE_DAY_OF_MONTH", 23) == 0)
          start.day = atoi(trim_spaces(str));
        if (strncmp_case(line, "FIRST_LINE_UTC", 14) == 0) {
          fprintf(fp, "      <start_seconds_day type=\"double\" definition=\""
            "seconds of the day at the start of the image\" units=\"s\">%s"
            "</start_seconds_day>\n", trim_spaces(str));
          ymd = start;
          hms = midnight;
          seconds = atof(trim_spaces(str));
          add_time(seconds, &ymd, &hms);
          ymd_hms2iso_date(&ymd, &hms, isoStr);
          fprintf(fp, "      <start_datetime type=\"string\" definition=\"UTC "
            "time at the start of the image\">%s</start_datetime>\n", isoStr); 	
        }
        if (strncmp_case(line, "CENTER_LINE_UTC", 15) == 0) {
          fprintf(fp, "      <center_seconds_day type=\"double\" definition=\""
            "seconds of the day at the center of the image\" units=\"s\">%s"
            "</center_seconds_day>\n", trim_spaces(str));
          ymd = start;
          hms = midnight;
          seconds = atof(trim_spaces(str));
          add_time(seconds, &ymd, &hms);
          ymd_hms2iso_date(&ymd, &hms, isoStr);
          fprintf(fp, "      <center_datetime type=\"string\" definition=\"UTC "
            "time at the center of the image\">%s</center_datetime>\n", isoStr);
        }
        if (strncmp_case(line, "LAST_LINE_UTC", 13) == 0) {
          fprintf(fp, "      <end_seconds_day type=\"double\" definition=\""
            "seconds of the day at the end of the image\" units=\"s\">%s"
            "</end_seconds_day>\n", trim_spaces(str));
          ymd = start;
          hms = midnight;
          seconds = atof(trim_spaces(str));
          add_time(seconds, &ymd, &hms);
          ymd_hms2iso_date(&ymd, &hms, isoStr);
          fprintf(fp, "      <end_datetime type=\"string\" definition=\"UTC "
            "time at the end of the image\">%s</end_datetime>\n", isoStr);
        }
        if (strncmp_case(line, "LATITUDE", 8) == 0)
          fprintf(fp, "      <peg_latitude type=\"double\" definition=\"latitude "
            "of the peg point [degrees]\" units=\"degrees_north\">%s"
            "</peg_latitude>\n", trim_spaces(str));
        if (strncmp_case(line, "LONGITUDE", 9) == 0)
          fprintf(fp, "      <peg_longitude type=\"double\" definition=\""
            "longitude of the peg point [degrees]\" units=\"degrees_east\">%s"
            "</peg_longitude>\n", trim_spaces(str));
        if (strncmp_case(line, "HEADING", 7) == 0)
          fprintf(fp, "      <peg_heading type=\"double\" definition=\"heading "
            "of the peg point [degrees]\" units=\"degrees_north\">%s"
            "</peg_heading>\n", trim_spaces(str));
        if (strncmp_case(line, "HEIGHT ", 7) == 0)
          fprintf(fp, "      <height type=\"double\" definition=\"height of the "
            "platform at the image center [m]\" units=\"m\">%s</height>\n", 
            trim_spaces(str));
        if (strncmp_case(line, "EARTH_RADIUS", 12) == 0)
          fprintf(fp, "      <earth_radius type=\"double\" definition=\"earth "
            "radius at the image center\" units=\"m\">%s</earth_radius>\n",
            trim_spaces(str));
      }
      FCLOSE(fpFiles);
      fprintf(fp, "      <data_source type=\"string\" definition=\"source "
        "providing the data\">Alaska Satellite Facility</data_source>\n");
      if (meta_is_valid_double(params->master_faraday_rotation))
        fprintf(fp, "      <faraday_rotation type=\"double\" definition=\""
          "Faraday rotation [degrees]\" units=\"degrees\">%g</faraday_rotation>"
          "\n", params->master_faraday_rotation);
      fprintf(fp, "    </master_image>\n");
    }
    if (params->slave_meta_file) {
      fprintf(fp, "    <slave_image>\n");

      // Read ASF master metadata
      meta = meta_read(params->slave_meta_file);
      fprintf(fp, "      <file type=\"string\" definition=\"file name of the "
        "slave image\">%s</file>\n", meta->general->basename);
      fprintf(fp, "      <platform type=\"string\" definition=\"name of the "
        "platform\">%s</platform>\n", meta->general->sensor);
      if (strcmp_case(meta->general->sensor, "ALOS") == 0 && 
      strcmp_case(meta->general->sensor_name, "SAR") == 0)
        fprintf(fp, "      <sensor type=\"string\" definition=\"name of the "
          "sensor\">PALSAR</sensor>\n");
      else
        fprintf(fp, "      <sensor type=\"string\" definition=\"name of the "
          "sensor\">%s</sensor>\n", meta->general->sensor_name);
      fprintf(fp, "      <wavelength type=\"double\" definition=\"wavelength of "
        "the sensor\" units=\"m\">%g</wavelength>\n", meta->sar->wavelength);
      fprintf(fp, "      <beam_mode type=\"string\" definition=\"beam mode of the"
        " sensor\">%s</beam_mode>\n", meta->general->mode);
      fprintf(fp, "      <absolute_orbit type=\"int\" definition=\"absolute orbit"
        " of the image\">%d</absolute_orbit>\n", meta->general->orbit);
      fprintf(fp, "      <frame type=\"int\" definition=\"frame number of the "
        "image\">%d</frame>\n",  meta->general->frame);
      if (meta->general->orbit_direction == 'A')
        fprintf(fp, "      <flight_direction type=\"string\" definition=\"flight "
        "direction of the sensor\">ascending</flight_direction>\n");
      else if (meta->general->orbit_direction == 'D')
        fprintf(fp, "      <flight_direction type=\"string\" definition=\"flight "
        "direction of the sensor\">descending</flight_direction>\n");
      fprintf(fp, "      <polarization type=\"string\" definition=\"polarization "
        "of the transmitted and received signal\">%s</polarization>\n", 
        meta->sar->polarization);
      fprintf(fp, "      <data_processing_level type=\"string\" definition=\""
        "processing level of the input data\">raw</data_processing_level>\n");
      fprintf(fp, "      <data_format type=\"string\" definition=\"data format "
        "of input data\">CEOS</data_format>\n");
      fprintf(fp, "      <prf type=\"double\" definition=\"pulse repetition "
        "frequency\" units=\"Hz\">%g</prf>\n", meta->sar->prf);
      meta_free(meta);

      // Read ROI_PAC slave metadata
      fpFiles = FOPEN(params->slave_metadata, "rt");
      while (NULL != fgets(line, 255, fpFiles)) {
        sprintf(str, "%s", line+40);
        if (strncmp_case(line, "FIRST_LINE_YEAR", 15) == 0)
          start.year = atoi(trim_spaces(str));
        if (strncmp_case(line, "FIRST_LINE_MONTH_OF_YEAR", 24) == 0)
          start.month = atoi(trim_spaces(str));
        if (strncmp_case(line, "FIRST_LINE_DAY_OF_MONTH", 23) == 0)
          start.day = atoi(trim_spaces(str));
        if (strncmp_case(line, "FIRST_LINE_UTC", 14) == 0) {
          fprintf(fp, "      <start_seconds_day type=\"double\" definition=\""
            "seconds of the day at the start of the image\" units=\"s\">%s"
            "</start_seconds_day>\n", trim_spaces(str));
          ymd = start;
          hms = midnight;
          seconds = atof(trim_spaces(str));
          add_time(seconds, &ymd, &hms);
          ymd_hms2iso_date(&ymd, &hms, isoStr);
          fprintf(fp, "      <start_datetime type=\"string\" definition=\"UTC "
            "time at the start of the image\">%s</start_datetime>\n", isoStr); 	
        }
        if (strncmp_case(line, "CENTER_LINE_UTC", 15) == 0) {
          fprintf(fp, "      <center_seconds_day type=\"double\" definition=\""
            "seconds of the day at the center of the image\" units=\"s\">%s"
            "</center_seconds_day>\n", trim_spaces(str));
          ymd = start;
          hms = midnight;
          seconds = atof(trim_spaces(str));
          add_time(seconds, &ymd, &hms);
          ymd_hms2iso_date(&ymd, &hms, isoStr);
          fprintf(fp, "      <center_datetime type=\"string\" definition=\"UTC "
            "time at the center of the image\">%s</center_datetime>\n", isoStr);
        }
        if (strncmp_case(line, "LAST_LINE_UTC", 13) == 0) {
          fprintf(fp, "      <end_seconds_day type=\"double\" definition=\""
            "seconds of the day at the end of the image\" units=\"s\">%s"
            "</end_seconds_day>\n", trim_spaces(str));
          ymd = start;
          hms = midnight;
          seconds = atof(trim_spaces(str));
          add_time(seconds, &ymd, &hms);
          ymd_hms2iso_date(&ymd, &hms, isoStr);
          fprintf(fp, "      <end_datetime type=\"string\" definition=\"UTC "
            "time at the end of the image\">%s</end_datetime>\n", isoStr);
        }
        if (strncmp_case(line, "LATITUDE", 8) == 0)
          fprintf(fp, "      <peg_latitude type=\"double\" definition=\"latitude "
            "of the peg point [degrees]\" units=\"degrees_north\">%s"
            "</peg_latitude>\n", trim_spaces(str));
        if (strncmp_case(line, "LONGITUDE", 9) == 0)
          fprintf(fp, "      <peg_longitude type=\"double\" definition=\""
            "longitude of the peg point [degrees]\" units=\"degrees_east\">%s"
            "</peg_longitude>\n", trim_spaces(str));
        if (strncmp_case(line, "HEADING", 7) == 0)
          fprintf(fp, "      <peg_heading type=\"double\" definition=\"heading "
            "of the peg point [degrees]\" units=\"degrees_north\">%s"
            "</peg_heading>\n", trim_spaces(str));
        if (strncmp_case(line, "HEIGHT ", 7) == 0)
          fprintf(fp, "      <height type=\"double\" definition=\"height of the "
            "platform at the image center [m]\" units=\"m\">%s</height>\n", 
            trim_spaces(str));
        if (strncmp_case(line, "EARTH_RADIUS", 12) == 0)
          fprintf(fp, "      <earth_radius type=\"double\" definition=\"earth "
            "radius at the image center\" units=\"m\">%s</earth_radius>\n",
            trim_spaces(str));
      }
      FCLOSE(fpFiles);
      fprintf(fp, "      <data_source type=\"string\" definition=\"source "
        "providing the data\">Alaska Satellite Facility</data_source>\n");
      if (meta_is_valid_double(params->slave_faraday_rotation))
        fprintf(fp, "      <faraday_rotation type=\"double\" definition=\""
          "Faraday rotation [degrees]\" units=\"degrees\">%g</faraday_rotation>"
          "\n", params->slave_faraday_rotation);
      fprintf(fp, "    </slave_image>\n");
    }
    
    // Read Doppler information
    double doppler0, doppler1, doppler2, slant_range_azimuth_resolution;
    if (params->doppler) {
      fpFiles = FOPEN(params->doppler, "rt");
      while (NULL != fgets(line, 255, fpFiles)) {
        sprintf(str, "%s", line+40);
        if (strncmp_case(line, "DOPPLER_RANGE0", 14) == 0)
          doppler0 = atof(trim_spaces(str));
        if (strncmp_case(line, "DOPPLER_RANGE1", 14) == 0)
          doppler1 = atof(trim_spaces(str));
        if (strncmp_case(line, "DOPPLER_RANGE2", 14) == 0)
          doppler2 = atof(trim_spaces(str));
        if (strncmp_case(line, "SL_AZIMUT_RESOL", 15) == 0)
          slant_range_azimuth_resolution = atof(trim_spaces(str));
      }
      FCLOSE(fpFiles);
    }

    // Read baseline information
    double vertical_baseline, horizontal_baseline, vertical_baseline_rate;
    double horizontal_baseline_rate, vertical_baseline_acceleration;
    double horizontal_baseline_acceleration;
    if (params->baseline) {
      fpFiles = FOPEN(params->baseline, "rt");
      while (NULL != fgets(line, 255, fpFiles)) {
        sprintf(str, "%s", line+40);
        if (strncmp_case(line, "V_BASELINE_TOP_SIM", 18) == 0)
          vertical_baseline = atof(trim_spaces(str));
        if (strncmp_case(line, "V_BASELINE_RATE_SIM", 19) == 0)
          vertical_baseline_rate = atof(trim_spaces(str));
        if (strncmp_case(line, "V_BASELINE_ACC_SIM", 18) == 0)
          vertical_baseline_acceleration = atof(trim_spaces(str));
        if (strncmp_case(line, "H_BASELINE_TOP_SIM", 18) == 0)
          horizontal_baseline = atof(trim_spaces(str));
        if (strncmp_case(line, "H_BASELINE_RATE_SIM", 19) == 0)
          horizontal_baseline_rate = atof(trim_spaces(str));
        if (strncmp_case(line, "H_BASELINE_ACC_SIM", 18) == 0)
          horizontal_baseline_acceleration = atof(trim_spaces(str));
      }
      FCLOSE(fpFiles);
    }

    // Determine orbit type
    char orbit_type[25], *p = NULL;
    split_dir_and_file(params->master_metadata, directory, filename);
    sprintf(logFile, "%s%clog", directory, DIR_SEPARATOR);
    fpFiles = FOPEN(logFile, "rt");
    while (NULL != fgets(line, 255, fpFiles)) {
      p = strstr(line, "roi_prep.pl");
      if (p) {
        sprintf(str, "roi_prep.pl %s", directory);
        sprintf(orbit_type, "%s", trim_spaces(p+strlen(str)));
      }
    }
    FCLOSE(fpFiles);    

    // Read information for wrapped interferogram
    if (params->wrapped_interferogram) {
      sprintf(rscFile, "%s.rsc", params->wrapped_interferogram);
      fpFiles = FOPEN(rscFile, "rt");
      fprintf(fp, "    <wrapped_interferogram>\n");
      split_dir_and_file(params->wrapped_interferogram, directory, filename);
      fprintf(fp, "      <file type=\"string\" definition=\"file name of "
        "wrapped interferogram\">%s</file>\n", filename);
      fprintf(fp, "      <source type=\"string\" definition=\"source of the "
        "data\">Alaska Satellite Facility</source>\n");				
      fprintf(fp, "      <map_projection type=\"string\" definition=\"map "
        "projection of the data\">geographic</map_projection>\n");
      while (NULL != fgets(line, 255, fpFiles)) {
        sprintf(str, "%s", line+40);
        if (strncmp_case(line, "WIDTH", 5) == 0)
          fprintf(fp, "      <width type=\"int\" definition=\"width of the "
            "image\">%s</width>\n", trim_spaces(str));
        if (strncmp_case(line, "FILE_LENGTH", 11) == 0)
          fprintf(fp, "      <height type=\"int\" definition=\"height of the "
          "image\">%s</height>\n", trim_spaces(str));
        if (strncmp_case(line, "Y_FIRST", 7) == 0)
          fprintf(fp, "      <start_lat type=\"double\" definition=\"starting "
            "latitude of the image [degrees]\" units=\"degrees_north\">%s"
            "</start_lat>\n", trim_spaces(str));
        if (strncmp_case(line, "X_FIRST", 7) == 0)
          fprintf(fp, "      <start_lon type=\"double\" definition=\"starting "
            "longitude of the image [degrees]\" units=\"degrees_east\">%s"
            "</start_lon>\n", trim_spaces(str));
        if (strncmp_case(line, "Y_STEP", 6) == 0)
          fprintf(fp, "      <spacing_lat type=\"double\" definition=\"pixel "
            "spacing in latitude direction [degrees]\" units=\"degrees_north\""
            ">%s</spacing_lat>\n", trim_spaces(str));
        if (strncmp_case(line, "X_STEP", 6) == 0)
          fprintf(fp, "      <spacing_lon type=\"double\" definition=\"pixel "
            "spacing in longitude direction [degrees]\" units=\"degrees_east\""
            ">%s</spacing_lon>\n", trim_spaces(str));
        // range looks
        // azimuth looks
      }
      if (params->baseline) {
        fprintf(fp, "      <vertical_baseline type=\"double\" definition=\""
          "vertical baseline component [m]\" units=\"m\">%g"
          "</vertical_baseline>\n", vertical_baseline);
        fprintf(fp, "      <vertical_baseline_rate type=\"double\" definition"
          "=\"vertical baseline velocity [m/m]\" units=\"m/m\">%g"
          "</vertical_baseline_rate>\n", vertical_baseline_rate);
        fprintf(fp, "      <vertical_baseline_acceleration type=\"double\" "
          "definition=\"vertical baseline acceleration [m/m^2]\" units=\""
          "m/m^2\">%g</vertical_baseline_acceleration>\n", 
          vertical_baseline_acceleration);
        fprintf(fp, "      <horizontal_baseline type=\"double\" definition=\""
          "horizontal baseline component [m]\" units=\"m\">%g"
          "</horizontal_baseline>\n", horizontal_baseline);
        fprintf(fp, "      <horizontal_baseline_rate type=\"double\" "
          "definition=\"horizontal baseline velocity [m/m]\" units=\"m/m\">%g"
          "</horizontal_baseline_rate>\n", horizontal_baseline_rate);
        fprintf(fp, "      <horizontal_baseline_acceleration type=\"double\" "
          "definition=\"horizontal baseline acceleration [m/m^2]\" units=\""
          "m/m^2\">%g</horizontal_baseline_acceleration>\n", 
          horizontal_baseline_acceleration);
      }
      if (params->doppler) {
        fprintf(fp, "      <doppler0 type=\"double\" definition=\"constant "
          "Doppler term [Hz/PRF]\" units=\"Hz/PRF\">%g</doppler0>\n", 
          doppler0);
        fprintf(fp, "      <doppler1 type=\"double\" definition=\"linear "
          "Doppler term [Hz/(PRF*pixel)]\" units=\"Hz/(PRF*pixel)\">%g"
          "</doppler1>\n", doppler1);
        fprintf(fp, "      <doppler2 type=\"double\" definition=\"quadratic "
          "Doppler term [Hz/(PRF*pixel^2)]\" units=\"Hz/(PRF*pixel^2)\">%g"
          "</doppler2>\n", doppler2); 
        fprintf(fp, "      <slant_range_azimuth_resolution type=\"double\" "
          "definition=\"azimuth resolution in slant range [m]\" units=\"m\">"
          "%g</slant_range_azimuth_resolution>\n", 
          slant_range_azimuth_resolution);
      }
      fprintf(fp, "      <baseline_method type=\"string\" definition=\"baseline "
        "refinement using unwrapped phase and topography\">topo</baseline_method>"
        "\n");
      fprintf(fp, "      <orbit_type type=\"string\" definition=\"orbit type: "
        "HDR (header information) or PRC (precision state vectors)\">%s"
        "</orbit_type>\n", orbit_type);
      // filter
      // filter_strength
      fprintf(fp, "    </wrapped_interferogram>\n");
      FCLOSE(fpFiles);
    }

    // Read information for unwrapped interferogram
    int insar_width = 0, insar_height = 0;
    double insar_start_lat = -99, insar_start_lon = -199;
    double insar_spacing_lat = -99, insar_spacing_lon = -199;
    if (params->unwrapped_interferogram) {
      sprintf(rscFile, "%s.rsc", params->unwrapped_interferogram);
      fpFiles = FOPEN(rscFile, "rt");
      fprintf(fp, "    <unwrapped_interferogram>\n");
      split_dir_and_file(params->unwrapped_interferogram, directory, filename);
      fprintf(fp, "      <file type=\"string\" definition=\"file name of "
        "unwrapped interferogram\">%s</file>\n", filename);
      fprintf(fp, "      <source type=\"string\" definition=\"source of the "
        "data\">Alaska Satellite Facility</source>\n");				
      fprintf(fp, "      <map_projection type=\"string\" definition=\"map "
        "projection of the data\">geographic</map_projection>\n");
      while (NULL != fgets(line, 255, fpFiles)) {
        sprintf(str, "%s", line+40);
        if (strncmp_case(line, "WIDTH", 5) == 0) {
          insar_width = atoi(trim_spaces(str));
          fprintf(fp, "      <width type=\"int\" definition=\"width of the "
            "image\">%d</width>\n", insar_width);
        }
        if (strncmp_case(line, "FILE_LENGTH", 11) == 0) {
          insar_height = atoi(trim_spaces(str));
          fprintf(fp, "      <height type=\"int\" definition=\"height of the "
            "image\">%d</height>\n", insar_height);
        }
        if (strncmp_case(line, "Y_FIRST", 7) == 0) {
          insar_start_lat = atof(trim_spaces(str));
          fprintf(fp, "      <start_lat type=\"double\" definition=\"starting "
            "latitude of the image [degrees]\" units=\"degrees_north\">%g"
            "</start_lat>\n", insar_start_lat);
        }
        if (strncmp_case(line, "X_FIRST", 7) == 0) {
          insar_start_lon = atof(trim_spaces(str));
          fprintf(fp, "      <start_lon type=\"double\" definition=\"starting "
            "longitude of the image [degrees]\" units=\"degrees_east\">%g"
            "</start_lon>\n", insar_start_lon);
        }
        if (strncmp_case(line, "Y_STEP", 6) == 0) {
          insar_spacing_lat = atof(trim_spaces(str));
          fprintf(fp, "      <spacing_lat type=\"double\" definition=\"pixel "
            "spacing in latitude direction [degrees]\" units=\"degrees_north\""
            ">%g</spacing_lat>\n", insar_spacing_lat);
        }
        if (strncmp_case(line, "X_STEP", 6) == 0) {
          insar_spacing_lon = atof(trim_spaces(str));
          fprintf(fp, "      <spacing_lon type=\"double\" definition=\"pixel "
            "spacing in longitude direction [degrees]\" units=\"degrees_east\""
            ">%g</spacing_lon>\n", insar_spacing_lon);
        }
        // range looks
        // azimuth looks
      }
      if (params->baseline) {
        fprintf(fp, "      <vertical_baseline type=\"double\" definition=\""
          "vertical baseline component [m]\" units=\"m\">%g"
          "</vertical_baseline>\n", vertical_baseline);
        fprintf(fp, "      <vertical_baseline_rate type=\"double\" definition"
          "=\"vertical baseline velocity [m/m]\" units=\"m/m\">%g"
          "</vertical_baseline_rate>\n", vertical_baseline_rate);
        fprintf(fp, "      <vertical_baseline_acceleration type=\"double\" "
          "definition=\"vertical baseline acceleration [m/m^2]\" units=\""
          "m/m^2\">%g</vertical_baseline_acceleration>\n", 
          vertical_baseline_acceleration);
        fprintf(fp, "      <horizontal_baseline type=\"double\" definition=\""
          "horizontal baseline component [m]\" units=\"m\">%g"
          "</horizontal_baseline>\n", horizontal_baseline);
        fprintf(fp, "      <horizontal_baseline_rate type=\"double\" "
          "definition=\"horizontal baseline velocity [m/m]\" units=\"m/m\">%g"
          "</horizontal_baseline_rate>\n", horizontal_baseline_rate);
        fprintf(fp, "      <horizontal_baseline_acceleration type=\"double\" "
          "definition=\"horizontal baseline acceleration [m/m^2]\" units=\""
          "m/m^2\">%g</horizontal_baseline_acceleration>\n", 
          horizontal_baseline_acceleration);
      }
      if (params->doppler) {
        fprintf(fp, "      <doppler0 type=\"double\" definition=\"constant "
          "Doppler term [Hz/PRF]\" units=\"Hz/PRF\">%g</doppler0>\n", 
          doppler0);
        fprintf(fp, "      <doppler1 type=\"double\" definition=\"linear "
          "Doppler term [Hz/(PRF*pixel)]\" units=\"Hz/(PRF*pixel)\">%g"
          "</doppler1>\n", doppler1);
        fprintf(fp, "      <doppler2 type=\"double\" definition=\"quadratic "
          "Doppler term [Hz/(PRF*pixel^2)]\" units=\"Hz/(PRF*pixel^2)\">%g"
          "</doppler2>\n", doppler2); 
        fprintf(fp, "      <slant_range_azimuth_resolution type=\"double\" "
          "definition=\"azimuth resolution in slant range [m]\" units=\"m\">"
          "%g</slant_range_azimuth_resolution>\n", 
          slant_range_azimuth_resolution);
      }
      fprintf(fp, "      <baseline_method type=\"string\" definition=\"baseline "
        "refinement using unwrapped phase and topography\">topo</baseline_method>"
        "\n");
      fprintf(fp, "      <orbit_type type=\"string\" definition=\"orbit type: "
        "HDR (header information) or PRC (precision state vectors)\">%s"
        "</orbit_type>\n", orbit_type);
      meta = meta_read(params->unwrapped_interferogram);
      if (meta->stats) {
        fprintf(fp, "      <phase_minimum>%g</phase_minimum>\n",
          meta->stats->band_stats[0].min);
        fprintf(fp, "      <phase_maximum>%g</phase_maximum>\n",
          meta->stats->band_stats[0].max);
        fprintf(fp, "      <percent_unwrapped>%g</percent_unwrapped>\n",
          meta->stats->band_stats[0].percent_valid);
      }
      meta_free(meta);
      // filter
      // filter_strength
      // unwrapped_method
      // unwrapping_parameters
      fprintf(fp, "    </unwrapped_interferogram>\n");
      FCLOSE(fpFiles);
    }

    // Read information for coherence image
    if (params->correlation) {
      sprintf(rscFile, "%s.rsc", params->correlation);
      fpFiles = FOPEN(rscFile, "rt");
      fprintf(fp, "    <correlation>\n");
      split_dir_and_file(params->correlation, directory, filename);
      fprintf(fp, "      <file type=\"string\" definition=\"file name of "
        "correlation image\">%s</file>\n", filename);
      fprintf(fp, "      <source type=\"string\" definition=\"source of the "
        "data\">Alaska Satellite Facility</source>\n");				
      fprintf(fp, "      <map_projection type=\"string\" definition=\"map "
        "projection of the data\">geographic</map_projection>\n");
      while (NULL != fgets(line, 255, fpFiles)) {
        sprintf(str, "%s", line+40);
        if (strncmp_case(line, "WIDTH", 5) == 0)
          fprintf(fp, "      <width type=\"int\" definition=\"width of the "
            "image\">%s</width>\n", trim_spaces(str));
        if (strncmp_case(line, "FILE_LENGTH", 11) == 0)
          fprintf(fp, "      <height type=\"int\" definition=\"height of the "
            "image\">%s</height>\n", trim_spaces(str));
        if (strncmp_case(line, "Y_FIRST", 7) == 0)
          fprintf(fp, "      <start_lat type=\"double\" definition=\"starting "
            "latitude of the image [degrees]\" units=\"degrees_north\">%s"
            "</start_lat>\n", trim_spaces(str));
        if (strncmp_case(line, "X_FIRST", 7) == 0)
          fprintf(fp, "      <start_lon type=\"double\" definition=\"starting "
            "longitude of the image [degrees]\" units=\"degrees_east\">%s"
            "</start_lon>\n", trim_spaces(str));
        if (strncmp_case(line, "Y_STEP", 6) == 0)
          fprintf(fp, "      <spacing_lat type=\"double\" definition=\"pixel "
            "spacing in latitude direction [degrees]\" units=\"degrees_north\""
            ">%s</spacing_lat>\n", trim_spaces(str));
        if (strncmp_case(line, "X_STEP", 6) == 0)
          fprintf(fp, "      <spacing_lon type=\"double\" definition=\"pixel "
            "spacing in longitude direction [degrees]\" units=\"degrees_east\""
            ">%s</spacing_lon>\n", trim_spaces(str));
        // range looks
        // azimuth looks
      }
      if (params->baseline) {
        fprintf(fp, "      <vertical_baseline type=\"double\" definition=\""
          "vertical baseline component [m]\" units=\"m\">%g"
          "</vertical_baseline>\n", vertical_baseline);
        fprintf(fp, "      <vertical_baseline_rate type=\"double\" definition"
          "=\"vertical baseline velocity [m/m]\" units=\"m/m\">%g"
          "</vertical_baseline_rate>\n", vertical_baseline_rate);
        fprintf(fp, "      <vertical_baseline_acceleration type=\"double\" "
          "definition=\"vertical baseline acceleration [m/m^2]\" units=\""
          "m/m^2\">%g</vertical_baseline_acceleration>\n", 
          vertical_baseline_acceleration);
        fprintf(fp, "      <horizontal_baseline type=\"double\" definition=\""
          "horizontal baseline component [m]\" units=\"m\">%g"
          "</horizontal_baseline>\n", horizontal_baseline);
        fprintf(fp, "      <horizontal_baseline_rate type=\"double\" "
          "definition=\"horizontal baseline velocity [m/m]\" units=\"m/m\">%g"
          "</horizontal_baseline_rate>\n", horizontal_baseline_rate);
        fprintf(fp, "      <horizontal_baseline_acceleration type=\"double\" "
          "definition=\"horizontal baseline acceleration [m/m^2]\" units=\""
          "m/m^2\">%g</horizontal_baseline_acceleration>\n", 
          horizontal_baseline_acceleration);
      }
      if (params->doppler) {
        fprintf(fp, "      <doppler0 type=\"double\" definition=\"constant "
          "Doppler term [Hz/PRF]\" units=\"Hz/PRF\">%g</doppler0>\n", 
          doppler0);
        fprintf(fp, "      <doppler1 type=\"double\" definition=\"linear "
          "Doppler term [Hz/(PRF*pixel)]\" units=\"Hz/(PRF*pixel)\">%g"
          "</doppler1>\n", doppler1);
        fprintf(fp, "      <doppler2 type=\"double\" definition=\"quadratic "
          "Doppler term [Hz/(PRF*pixel^2)]\" units=\"Hz/(PRF*pixel^2)\">%g"
          "</doppler2>\n", doppler2); 
        fprintf(fp, "      <slant_range_azimuth_resolution type=\"double\" "
          "definition=\"azimuth resolution in slant range [m]\" units=\"m\">"
          "%g</slant_range_azimuth_resolution>\n", 
          slant_range_azimuth_resolution);
      }
      fprintf(fp, "      <baseline_method type=\"string\" definition=\"baseline "
        "refinement using unwrapped phase and topography\">topo</baseline_method>"
        "\n");
      fprintf(fp, "      <orbit_type type=\"string\" definition=\"orbit type: "
        "HDR (header information) or PRC (precision state vectors)\">%s"
        "</orbit_type>\n", orbit_type);
      meta = meta_read(params->correlation);
      if (meta->stats) {
        fprintf(fp, "      <average_coherence>%g</average_coherence>\n",
          meta->stats->band_stats[0].mean);
      }
      meta_free(meta);
      fprintf(fp, "    </correlation>\n");
      FCLOSE(fpFiles);
    }

    // Read information for incidence angle map
    if (params->incidence_angle) {
      sprintf(rscFile, "%s.rsc", params->incidence_angle);
      fpFiles = FOPEN(rscFile, "rt");
      fprintf(fp, "    <incidence_angle>\n");
      split_dir_and_file(params->incidence_angle, directory, filename);
      fprintf(fp, "      <file type=\"string\" definition=\"file name of "
        "incidence angle map\">%s</file>\n", filename);
      fprintf(fp, "      <source type=\"string\" definition=\"source of the "
        "data\">Alaska Satellite Facility</source>\n");				
      fprintf(fp, "      <map_projection type=\"string\" definition=\"map "
        "projection of the data\">geographic</map_projection>\n");
      while (NULL != fgets(line, 255, fpFiles)) {
        sprintf(str, "%s", line+40);
        if (strncmp_case(line, "WIDTH", 5) == 0)
          fprintf(fp, "      <width type=\"int\" definition=\"width of the "
            "image\">%s</width>\n", trim_spaces(str));
        if (strncmp_case(line, "FILE_LENGTH", 11) == 0)
          fprintf(fp, "      <height type=\"int\" definition=\"height of the "
            "image\">%s</height>\n", trim_spaces(str));
        if (strncmp_case(line, "Y_FIRST", 7) == 0)
          fprintf(fp, "      <start_lat type=\"double\" definition=\"starting "
            "latitude of the image [degrees]\" units=\"degrees_north\">%s"
            "</start_lat>\n", trim_spaces(str));
        if (strncmp_case(line, "X_FIRST", 7) == 0)
          fprintf(fp, "      <start_lon type=\"double\" definition=\"starting "
            "longitude of the image [degrees]\" units=\"degrees_east\">%s"
            "</start_lon>\n", trim_spaces(str));
        if (strncmp_case(line, "Y_STEP", 6) == 0)
          fprintf(fp, "      <spacing_lat type=\"double\" definition=\"pixel "
            "spacing in latitude direction [degrees]\" units=\"degrees_north\""
            ">%s</spacing_lat>\n", trim_spaces(str));
        if (strncmp_case(line, "X_STEP", 6) == 0)
          fprintf(fp, "      <spacing_lon type=\"double\" definition=\"pixel "
            "spacing in longitude direction [degrees]\" units=\"degrees_east\""
            ">%s</spacing_lon>\n", trim_spaces(str));
      }			
      fprintf(fp, "    </incidence_angle>\n");
      FCLOSE(fpFiles);
    }

    // Read information for digital elevation model
    int dem_width = 0, dem_height = 0;
    double dem_start_lat = -99, dem_start_lon = -199;
    double dem_spacing_lat = -99, dem_spacing_lon = -199;
    if (params->digital_elevation_model) {
      sprintf(rscFile, "%s.rsc", params->digital_elevation_model);
      if (fileExists(rscFile)) {
        dem = TRUE;
        fpFiles = FOPEN(rscFile, "rt");
        fprintf(fp, "    <digital_elevation_model>\n");
        split_dir_and_file(params->digital_elevation_model, directory, filename);
        fprintf(fp, "      <file type=\"string\" definition=\"file name of "
          "digital elevation model\">%s</file>\n", filename);
        fprintf(fp, "      <height_units type=\"string\" definition=\"units of "
          "the height values\">meters</height_units>\n");
        fprintf(fp, "      <source type=\"string\" definition=\"source of the "
          "data\">OpenTopo</source>\n");				
        while (NULL != fgets(line, 255, fpFiles)) {
          sprintf(str, "%s", line+13);
          if (strncmp_case(line, "PROJECTION", 10) == 0) {
            sprintf(map_projection, "%s", trim_spaces(str));
            if (strcmp_case(map_projection, "LATLON") == 0)
              fprintf(fp, "      <map_projection type=\"string\" definition=\""
              "map projection of the data\">geographic</map_projection>\n");
          }
          if (strncmp_case(line, "WIDTH", 5) == 0) {
            dem_width = atoi(trim_spaces(str));
            fprintf(fp, "      <width type=\"int\" definition=\"width of the "
              "image\">%d</width>\n", dem_width);
          }
          if (strncmp_case(line, "FILE_LENGTH", 11) == 0) {
            dem_height = atoi(trim_spaces(str));
            fprintf(fp, "      <height type=\"int\" definition=\"height of the "
              "image\">%d</height>\n", dem_height);
          }
          if (strncmp_case(line, "Y_FIRST", 7) == 0) {
            dem_start_lat = atof(trim_spaces(str));
            fprintf(fp, "      <start_lat type=\"double\" definition=\"starting "
              "latitude of the image [degrees]\" units=\"degrees_north\">%g"
              "</start_lat>\n", dem_start_lat);
          }
          if (strncmp_case(line, "X_FIRST", 7) == 0) {
            dem_start_lon = atof(trim_spaces(str));
            fprintf(fp, "      <start_lon type=\"double\" definition=\"starting "
              "longitude of the image [degrees]\" units=\"degrees_east\">%g"
              "</start_lon>\n", dem_start_lon);
          }
          if (strncmp_case(line, "Y_STEP", 6) == 0) {
            dem_spacing_lat = atof(trim_spaces(str));
            fprintf(fp, "      <spacing_lat type=\"double\" definition=\"pixel "
              "spacing in latitude direction [degrees]\" units=\"degrees_north\""
              ">%g</spacing_lat>\n", dem_spacing_lat);
          }
          if (strncmp_case(line, "X_STEP", 6) == 0) {
            dem_spacing_lon = atof(trim_spaces(str));
            fprintf(fp, "      <spacing_lon type=\"double\" definition=\"pixel "
              "spacing in longitude direction [degrees]\" units=\"degrees_east\""
              ">%g</spacing_lon>\n", dem_spacing_lon);
          }
        }
        if (params->dem_url)
          fprintf(fp, "      <url type=\"string\" definition=\"URL used to download"
            " digital elevation model\">%s</url>\n", xml_encode(params->dem_url));
        fprintf(fp, "    </digital_elevation_model>\n");
        FCLOSE(fpFiles);
      }
    }

    // Read information for troposphere
    int tropo_width = 0, tropo_height = 0;
    double tropo_start_lat = -99, tropo_start_lon = -199;
    double tropo_spacing_lat = -99, tropo_spacing_lon = -199;
    if (params->troposphere) {
      sprintf(rscFile, "%s.rsc", params->troposphere);
      if (fileExists(rscFile)) {
        tropo = TRUE;
        fpFiles = FOPEN(rscFile, "rt");
        fprintf(fp, "    <troposphere>\n");
        split_dir_and_file(params->troposphere, directory, filename);
        fprintf(fp, "      <file type=\"string\" definition=\"file name of "
          "tropospheric information\">%s</file>\n", filename);
        fprintf(fp, "      <source type=\"string\" definition=\"source of the "
          "data\">Jet Propulsion Laboratory</source>\n");				
        while (NULL != fgets(line, 255, fpFiles)) {
          sprintf(str, "%s", line+15);
          if (strncmp_case(line, "PROJECTION", 10) == 0) {
            sprintf(map_projection, "%s", trim_spaces(str));
            if (strcmp_case(map_projection, "LATLON") == 0)
              fprintf(fp, "      <map_projection type=\"string\" definition=\""
              "map projection of the data\">geographic</map_projection>\n");
          }
          if (strncmp_case(line, "WIDTH", 5) == 0) {
            tropo_width = atoi(trim_spaces(str));
            fprintf(fp, "      <width type=\"int\" definition=\"width of the "
              "image\">%d</width>\n", tropo_width);
          }
          if (strncmp_case(line, "FILE_LENGTH", 11) == 0) {
            tropo_height = atoi(trim_spaces(str));
            fprintf(fp, "      <height type=\"int\" definition=\"height of the "
              "image\">%d</height>\n", tropo_height);
          }
          if (strncmp_case(line, "Y_FIRST", 7) == 0) {
            tropo_start_lat = atof(trim_spaces(str));
            fprintf(fp, "      <start_lat type=\"double\" definition=\"starting "
              "latitude of the image [degrees]\" units=\"degrees_north\">%g"
              "</start_lat>\n", tropo_start_lat);
          }
          if (strncmp_case(line, "X_FIRST", 7) == 0) {
            tropo_start_lon = atof(trim_spaces(str));
            fprintf(fp, "      <start_lon type=\"double\" definition=\"starting "
              "longitude of the image [degrees]\" units=\"degrees_east\">%g"
              "</start_lon>\n", tropo_start_lon);
          }
          if (strncmp_case(line, "Y_STEP", 6) == 0) {
            tropo_spacing_lat = atof(trim_spaces(str));
            fprintf(fp, "      <spacing_lat type=\"double\" definition=\"pixel "
              "spacing in latitude direction [degrees]\" units=\"degrees_north\""
              ">%g</spacing_lat>\n", tropo_spacing_lat);
          }
          if (strncmp_case(line, "X_STEP", 6) == 0) {
            tropo_spacing_lon = atof(trim_spaces(str));
            fprintf(fp, "      <spacing_lon type=\"double\" definition=\"pixel "
              "spacing in longitude direction [degrees]\" units=\"degrees_east\""
              ">%g</spacing_lon>\n", tropo_spacing_lon);
          }
        }
        if (params->troposphere_url)
          fprintf(fp, "      <url type=\"string\" definition=\"URL used to download"
            " tropospheric correction\">%s</url>\n", xml_encode(params->troposphere_url));
        meta = meta_read(params->troposphere);
        if (meta->stats) {
          fprintf(fp, "      <correction_minimum>%g</correction_minimum>\n",
            meta->stats->band_stats[0].min);
          fprintf(fp, "      <correction_maximum>%g</correction_maximum>\n",
            meta->stats->band_stats[0].max);
          fprintf(fp, "      <correction_coverage>%g</correction_coverage>\n",
            meta->stats->band_stats[0].percent_valid);
          fprintf(fp, "      <correction.variation>%g</correction_variation>\n",
            meta->stats->band_stats[0].std_deviation);
        }
        meta_free(meta);
        fprintf(fp, "    </troposphere>\n");
        FCLOSE(fpFiles);
      }
    }
    fprintf(fp, "  </metadata>\n");

    // Browse image information
    if (browse) {
      fprintf(fp, "  <browse>\n");
      if (params->browse_amplitude && fileExists(params->browse_amplitude))
        fprintf(fp, "    <amplitude>%s</amplitude>\n", params->browse_amplitude);
      if (params->browse_wrapped_interferogram && 
        fileExists(params->browse_wrapped_interferogram))
        fprintf(fp, "    <wrapped_interferogram>%s</wrapped_interferogram>\n",
          params->browse_wrapped_interferogram);
      if (params->browse_unwrapped_interferogram &&
        fileExists(params->browse_unwrapped_interferogram))
        fprintf(fp, "    <unwrapped_interferogram>%s</unwrapped_interferogram>"
          "\n", params->browse_unwrapped_interferogram);
      if (params->browse_correlation && fileExists(params->browse_correlation))
        fprintf(fp, "    <correlation>%s</correlation>\n", 
          params->browse_correlation);
      fprintf(fp, "  </browse>\n");
    }

    // Data extent
    fprintf(fp, "  <extent>\n");
    fprintf(fp, "    <insar>\n");
    fprintf(fp, "      <westBoundLongitude>%.5f</westBoundLongitude>\n",
      insar_start_lon);
    fprintf(fp, "      <eastBoundLongitude>%.5f</eastBoundLongitude>\n",
      (insar_start_lon + insar_spacing_lon*insar_width));
    fprintf(fp, "      <northBoundLatitude>%.5f</northBoundLatitude>\n",
      insar_start_lat);
    fprintf(fp, "      <southBoundLatitude>%.5f</southBoundLatitude>\n",
      (insar_start_lat + insar_spacing_lat*insar_height));
    fprintf(fp, "    </insar>\n");
    if (dem) {
      fprintf(fp, "    <digital_elevation_model>\n");
      fprintf(fp, "      <westBoundLongitude>%.5f</westBoundLongitude>\n",
        dem_start_lon);
      fprintf(fp, "      <eastBoundLongitude>%.5f</eastBoundLongitude>\n",
        (dem_start_lon + dem_spacing_lon*dem_width));
      fprintf(fp, "      <northBoundLatitude>%.5f</northBoundLatitude>\n",
        dem_start_lat);
      fprintf(fp, "      <southBoundLatitude>%.5f</southBoundLatitude>\n",
        (dem_start_lat + dem_spacing_lat*dem_height));
      fprintf(fp, "    </digital_elevation_model>\n");    
    }
    if (tropo) {
      fprintf(fp, "    <troposphere>\n");
      fprintf(fp, "      <westBoundLongitude>%.5f</westBoundLongitude>\n",
        tropo_start_lon);
      fprintf(fp, "      <eastBoundLongitude>%.5f</eastBoundLongitude>\n",
        (tropo_start_lon + tropo_spacing_lon*tropo_width));
      fprintf(fp, "      <northBoundLatitude>%.5f</northBoundLatitude>\n",
        tropo_start_lat);
      fprintf(fp, "      <southBoundLatitude>%.5f</southBoundLatitude>\n",
        (tropo_start_lat + tropo_spacing_lat*tropo_height));
      fprintf(fp, "    </troposphere>\n");    
    }
    fprintf(fp, "  </extent>\n");

    // Statistics
    meta = meta_read(params->wrapped_interferogram);
    if (meta->stats)
      stats = TRUE;
    meta_free(meta);
    meta = meta_read(params->unwrapped_interferogram);
    if (meta->stats)
      stats = TRUE;
    meta_free(meta);
    meta = meta_read(params->correlation);
    if (meta->stats)
      stats = TRUE;
    meta_free(meta);
    if (params->digital_elevation_model) {
      char metaFile[512];
      sprintf(metaFile, "%s.meta", params->digital_elevation_model);
      meta = meta_read(metaFile);
      if (meta->stats)
        stats = TRUE;
      meta_free(meta);
    }
    if (params->troposphere) {
      meta = meta_read(params->troposphere);
      if (meta->stats)
        stats = TRUE;
      meta_free(meta);
    }    
    if (stats) {
      fprintf(fp, "  <statistics>\n");
      meta = meta_read(params->wrapped_interferogram);
      if (meta->stats) {
        fprintf(fp, "    <wrapped_interferogram>\n");
        fprintf(fp, "      <minimum_value>%.11g</minimum_value>\n",
          meta->stats->band_stats[0].min);
        fprintf(fp, "      <maximum_value>%.11g</maximum_value>\n",
          meta->stats->band_stats[0].max);
        fprintf(fp, "      <mean_value>%.11g</mean_value>\n",
          meta->stats->band_stats[0].mean);
        fprintf(fp, "      <standard_deviation>%.11g</standard_deviation>\n",
          meta->stats->band_stats[0].std_deviation);
        fprintf(fp, "      <percent_valid_values>%.3f</percent_valid_values>\n",
          meta->stats->band_stats[0].percent_valid);
        fprintf(fp, "    </wrapped_interferogram>\n");
      }
      meta_free(meta);
      meta = meta_read(params->unwrapped_interferogram);
      if (meta->stats) {
        fprintf(fp, "    <unwrapped_interferogram>\n");
        fprintf(fp, "      <minimum_value>%.11g</minimum_value>\n",
          meta->stats->band_stats[0].min);
        fprintf(fp, "      <maximum_value>%.11g</maximum_value>\n",
          meta->stats->band_stats[0].max);
        fprintf(fp, "      <mean_value>%.11g</mean_value>\n",
          meta->stats->band_stats[0].mean);
        fprintf(fp, "      <standard_deviation>%.11g</standard_deviation>\n",
          meta->stats->band_stats[0].std_deviation);
        fprintf(fp, "      <percent_valid_values>%.3f</percent_valid_values>\n",
          meta->stats->band_stats[0].percent_valid);
        fprintf(fp, "    </unwrapped_interferogram>\n");
      }
      meta_free(meta);
      meta = meta_read(params->correlation);
      if (meta->stats) {
        fprintf(fp, "    <correlation>\n");
        fprintf(fp, "      <minimum_value>%.11g</minimum_value>\n",
          meta->stats->band_stats[0].min);
        fprintf(fp, "      <maximum_value>%.11g</maximum_value>\n",
          meta->stats->band_stats[0].max);
        fprintf(fp, "      <mean_value>%.11g</mean_value>\n",
          meta->stats->band_stats[0].mean);
        fprintf(fp, "      <standard_deviation>%.11g</standard_deviation>\n",
          meta->stats->band_stats[0].std_deviation);
        fprintf(fp, "      <percent_valid_values>%.3f</percent_valid_values>\n",
          meta->stats->band_stats[0].percent_valid);
        fprintf(fp, "    </correlation>\n");
      }
      meta_free(meta);
      if (params->digital_elevation_model) {
        char metaFile[512];
        sprintf(metaFile, "%s.meta", params->digital_elevation_model);
        meta = meta_read(metaFile);
        if (meta->stats) {
          fprintf(fp, "    <digital_elevation_model>\n");
          fprintf(fp, "      <minimum_value>%.11g</minimum_value>\n",
            meta->stats->band_stats[0].min);
          fprintf(fp, "      <maximum_value>%.11g</maximum_value>\n",
            meta->stats->band_stats[0].max);
          fprintf(fp, "      <mean_value>%.11g</mean_value>\n",
            meta->stats->band_stats[0].mean);
          fprintf(fp, "      <standard_deviation>%.11g</standard_deviation>\n",
            meta->stats->band_stats[0].std_deviation);
          fprintf(fp, "      <percent_valid_values>%.3f</percent_valid_values>"
            "\n", meta->stats->band_stats[0].percent_valid);
          fprintf(fp, "    </digital_elevation_model>\n");
        }
        meta_free(meta);
      }
      if (params->troposphere) {
        meta = meta_read(params->troposphere);
        if (meta->stats) {
          fprintf(fp, "    <troposphere>\n");
          fprintf(fp, "      <minimum_value>%.11g</minimum_value>\n",
            meta->stats->band_stats[0].min);
          fprintf(fp, "      <maximum_value>%.11g</maximum_value>\n",
            meta->stats->band_stats[0].max);
          fprintf(fp, "      <mean_value>%.11g</mean_value>\n",
            meta->stats->band_stats[0].mean);
          fprintf(fp, "      <standard_deviation>%.11g</standard_deviation>\n",
            meta->stats->band_stats[0].std_deviation);
          fprintf(fp, "      <percent_valid_values>%.3f</percent_valid_values>"
            "\n", meta->stats->band_stats[0].percent_valid);
          fprintf(fp, "    </troposphere>\n");
        }
        meta_free(meta);
      }
      fprintf(fp, "  </statistics>\n");
    }

    // Working the various log files
    fprintf(fp, "  <processing>\n");
    
    // Master image processing
    split_dir_and_file(params->master_metadata, directory, filename);
    sprintf(logFile, "%s%clog", directory, DIR_SEPARATOR);
    fpFiles = FOPEN(logFile, "rt");
    while (NULL != fgets(line, 255, fpFiles)) {
      if (strstr(line, "make_raw_alos.pl")) {
        roi2iso_date(line, str);
        fprintf(fp, "    <master_data_ingest>%s</master_data_ingest>\n", 
          str);
      }
      if (strstr(line, "roi.pl")) {
        roi2iso_date(line, str);
        fprintf(fp, "    <master_sar_processing>%s</master_sar_processing>"
          "\n", str);
      }
    }
    FCLOSE(fpFiles);
    
    // Slave image processing
    split_dir_and_file(params->slave_metadata, directory, filename);
    sprintf(logFile, "%s%clog", directory, DIR_SEPARATOR);
    fpFiles = FOPEN(logFile, "rt");
    while (NULL != fgets(line, 255, fpFiles)) {
      if (strstr(line, "make_raw_alos.pl")) {
        roi2iso_date(line, str);
        fprintf(fp, "    <slave_data_ingest>%s</slave_data_ingest>\n", 
          str);
      }
      if (strstr(line, "roi.pl")) {
        roi2iso_date(line, str);
        fprintf(fp, "    <slave_sar_processing>%s</slave_sar_processing>\n",
          str);
      }
    }
    FCLOSE(fpFiles);
    
    // Interferometric processing
    split_dir_and_file(params->processing_log, directory, filename);
    sprintf(logFile, "%s%clog", directory, DIR_SEPARATOR);
    fpFiles = FOPEN(logFile, "rt");
    while (NULL != fgets(line, 255, fpFiles)) {
      if (strstr(line, "resamp.pl")) {
        roi2iso_date(line, str);
        fprintf(fp, "    <create_interferogram>%s</create_interferogram>\n", 
          str);
      }
      if (strstr(line, "make_cor.pl")) {
        roi2iso_date(line, str);
        fprintf(fp, "    <create_correlation>%s</create_correlation>\n", str);
      }
      if (strstr(line, "make_sim.pl")) {
        roi2iso_date(line, str);
        fprintf(fp, "    <remove_topography>%s</remove_topography>\n", str);
      }
      if (strstr(line, "int2filtmaskunwrapNew.pl")) {
        roi2iso_date(line, str);
        fprintf(fp, "    <phase_unwrapping>%s</phase_unwrapping>\n", str);
      }
      if (strstr(line, "make_geomap.pl")) {
        roi2iso_date(line, str);
        fprintf(fp, "    <geocoding>%s</geocoding>\n", str);
      }
    }
    FCLOSE(fpFiles);    
    
    fprintf(fp, "  </processing>\n");
    
    // Root metadata
    char data_source[25], processing[100], copyright[50], original_file[100];
    char version[25];
    sprintf(version, "v31 r172");
    meta = meta_read(params->master_meta_file);
    if (strcmp_case(meta->general->sensor, "ALOS") == 0 && 
        strcmp_case(meta->general->sensor_name, "SAR") == 0) {
      sprintf(data_source, "ALOS PALSAR");
      sprintf(copyright, "JAXA, METI (%d)", start.year);
    }
    meta_free(meta);
    sprintf(processing, "SAR interferometric product, processed by ROI_PAC "
      "(%s)", version);
    if (params->digital_elevation_model &&
        fileExists(params->digital_elevation_model))
      strcat(processing, ", corrected for terrain");
    if (params->troposphere && fileExists(params->troposphere))
      strcat(processing, " and troposphere");
    split_dir_and_file(params->master_meta_file, directory, filename);
    sprintf(original_file, "master: %s, slave: ", stripExt(filename));
    split_dir_and_file(params->slave_meta_file, directory, filename);
    strcat(original_file, stripExt(filename));      

    fprintf(fp, "  <root>\n");
    fprintf(fp, "    <institution>Alaska Satellite Facility</institution>\n");
    fprintf(fp, "    <title>%s</title>\n", processing);
    fprintf(fp, "    <source>InSAR pair from %s data</source>\n", data_source);
    fprintf(fp, "    <original_file>%s</original_file>\n", original_file);
    fprintf(fp, "    <comment>Copyright %s</comment>\n", copyright);
    fprintf(fp, "    <reference>Documentation available at: www.asf.alaska.edu"
      "</reference>\n");
    fprintf(fp, "    <history>%s: H5 file created.</history>\n", creation);
    fprintf(fp, "  </root>\n");
    
    fprintf(fp, "</hdf5>\n");
    FCLOSE(fp);
  }
  // FIXME: This is the Seasat metadata that needs to be stored new style.
  /*
  char piFile[1024];
  FILE *fpLog=NULL;
  char granule[50];
  if (meta->general->image_data_type == RAW_IMAGE)
    sprintf(granule, "%s", get_basename(get_basename(meta->general->basename)));
  else
    sprintf(granule, "%s", get_basename(meta->general->basename));
  sprintf(piFile, "%s.000.pi", granule);
  char *begin = (char *) MALLOC(sizeof(char)*30);
  char *center = (char *) MALLOC(sizeof(char)*30);
  char *end = (char *) MALLOC(sizeof(char)*30);
  if (meta->general->image_data_type == RAW_IMAGE && fileExists(piFile)) {
    fprintf(fp, "  <quality>\n");
    char line[1024];
    float fValue;
    fpLog = FOPEN(piFile, "rt");
    while (fgets(line, 1024, fpLog) != NULL) {
      if (strstr(line, "missing_lines")) {
        sscanf(line, "  missing_lines: %f", &fValue);
        fprintf(fp, "    <missing_lines>%g</missing_lines>\n", fValue);
      }
      if (strstr(line, "bit_error_rate")) {
        sscanf(line, "  bit_error_rate: %f", &fValue);
        fprintf(fp, "    <bit_error_rate>%g</bit_error_rate>\n", fValue);
      }
    }  		
    FCLOSE(fpLog);
    fprintf(fp, "  </quality>\n");
  }	
  fprintf(fp, "  <iso>\n");
  fprintf(fp, "    <granule>%s</granule>\n", granule);
  fprintf(fp, "    <metadata_creation>%s</metadata_creation>\n", iso_date());
  meta2iso_date(meta, begin, center, end);
  fprintf(fp, "    <time>\n");
  fprintf(fp, "      <begin>%s</begin>\n", begin); 	
  fprintf(fp, "      <center>%s</center>\n", center);
  fprintf(fp, "      <end>%s</end>\n", end);
  fprintf(fp, "    </time>\n");
  fprintf(fp, "    <files>\n");
  if (logFile)
    fprintf(fp, "      <source>%s.down</source>\n", stripExt(logFile));
  if (strncmp_case(meta->general->sensor, "ERS", 3) == 0 &&
      meta->general->image_data_type != RAW_IMAGE) {
    fprintf(fp, "      <ceos_data_file>%s.D</ceos_data_file>\n", granule);
    fprintf(fp, "      <ceos_leader_file>%s.L</ceos_leader_file>\n", 
      granule);
    fprintf(fp, "      <ceos_parameter_file>%s.P</ceos_parameter_file>\n", 
      granule);
    fprintf(fp, "      <ceos_leader_text>%s.L.txt</ceos_leader_text>\n", 
      granule);
    fprintf(fp, "      <browse_image>%s.jpg</browse_image>\n", granule);
    fprintf(fp, "      <kml_file>%s.kml</kml_file>\n", granule);
  }
  else if (strncmp_case(meta->general->sensor, "ERS", 3) == 0) {
    fprintf(fp, "      <ceos_data_file>%s.000.raw</ceos_data_file>\n", 
      granule);
    fprintf(fp, "      <ceos_leader_file>%s.000.ldr</ceos_leader_file>\n", 
      granule);
    fprintf(fp, "      <ceos_volume_file>%s.000.vol</ceos_volume_file>\n",
      granule);
    fprintf(fp, "      <ceos_null_file>%s.000.nul</ceos_null_file>\n",
      granule);
    fprintf(fp, "      <ceos_pi_file>%s.000.pi</ceos_pi_file>\n", granule);
    fprintf(fp, "      <ceos_meta_file>%s.000.meta</ceos_meta_file\n", granule);
  }  	
  fprintf(fp, "    </files>\n");
  if (logFile) {
    fprintf(fp, "    <logs>\n");
    char line[1024], *p=NULL;
    char *time = (char *) MALLOC(sizeof(char)*30);
    // Read general log file first
    fpLog = FOPEN(logFile, "rt");
    while (fgets(line, 1024, fpLog) != NULL) {
      p = strstr(line, "bin/rds");
      if (p && strstr(p , granule)) {
        line2iso_date(line, time);
        fprintf(fp, "      <rds>%s</rds>\n", time);
      }
      p = strstr(line, "run.pp_ceos completed");
      if (p && strstr(p , granule)) {
        line2iso_date(line, time);
        fprintf(fp, "      <precision_processor>%s</precision_processor>\n",
          time);
      }
      p = strstr(line, "convert2vector");
      if (p && strstr(p , granule)) {
        line2iso_date(line, time);
        fprintf(fp, "      <convert2vector>%s</convert2vector>\n", time);
      }
      p = strstr(line, "metadata");
      if (p && strstr(p , granule)) {
        line2iso_date(line, time);
        fprintf(fp, "      <metadata>%s</metadata>\n", time);
      }
      p = strstr(line, "create_thumbs");
      if (p && strstr(p , granule)) {
        line2iso_date(line, time);
        fprintf(fp, "      <create_thumbs>%s</create_thumbs>\n", time);
      }
    }
    FCLOSE(fpLog);
    
    // Read SyncPrep log next
    char logFile2[1024];
    sprintf(logFile2, "%s_sp.log", get_basename(logFile));
    if (!fileExists)
      asfPrintError("SyncPrep log file does not exist!\n");
    printf("%s\n", logFile2);
    fpLog = FOPEN(logFile2, "rt");
    while (fgets(line, 1024, fpLog) != NULL) {
      if (strncmp_case(line, "98", 2) == 0) {
        line2iso_date(line, time);
        fprintf(fp, "      <syncPrep>\n");
        fprintf(fp, "        <time>%s</time>\n");
        fprintf(fp, "        <revision>%s</revision>\n");
        fprintf(fp, "        <edition>%s</edition>\n");
        fprintf(fp, "      </syncPrep>\n");
      }

      p = strstr(line, "zip -q -r");
      if (p && strstr(p, granule)) {
        line2iso_date(line, time);
        fprintf(fp, "      <superCeos>%s</superCeos>\n", time);
      }
      
    }
    FCLOSE(fpLog);
    
    FREE(time);
    fprintf(fp, "    </logs>\n");
  }
  if (meta->sar) {
    fprintf(fp, "    <polarization>\n");
    if (strcmp_case(meta->sar->polarization, "HH") == 0) {
      fprintf(fp, "      <transmitted>horizontal</transmitted>\n");
      fprintf(fp, "      <received>horizontal</received>\n");
    }
    if (strcmp_case(meta->sar->polarization, "VV") == 0) {
      fprintf(fp, "      <transmitted>vertical</transmitted>\n");
      fprintf(fp, "      <received>vertical</received>\n");
    }
    fprintf(fp, "    </polarization>\n");
  }
  fprintf(fp, "  </iso>\n");
  FREE(begin);
  FREE(center);
  FREE(end);
  */

  // Clean and report
  if (logFileName)
  	FREE(logFileName);
  asfPrintStatus("   Converted file list (%s) to XML file (%s)\n\n",
		 file_list, xml_name);

  return 0;
}

