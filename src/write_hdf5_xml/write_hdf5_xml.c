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
#include "asf_import.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_license.h"
#include "dateUtil.h"
#include "time.h"
#include <string.h>
#include <hdf5.h>

#define ASF_NAME_STRING "write_hdf5_xml"
#define VERSION 1.0

typedef struct {
  char *format;
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
	char *browse_overview;
	char *browse_amplitude;
	char *browse_wrapped_interferogram;
	char *browse_unwrapped_interferogram;
	char *browse_correlation;
	char *kml_overlay;
	char *incidence_angle;
	char *layover_shadow_mask;
	char *layover_shadow_stats;
	char *dem_file;
	char *dem_metadata;
	char *original_dem;
	char *dem_url;
	char *troposphere;
	char *troposphere_url;
  double master_faraday_rotation;
	double slave_faraday_rotation;
	char *data;
	char *metadata;
	char *input_HH_file;
	char *input_HV_file;
	char *input_VH_file;
	char *input_VV_file;
	char *rtc_HH_metadata;
	char *rtc_HV_metadata;
	char *rtc_VH_metadata;
	char *rtc_VV_metadata;
	char *rtc_HH_file;
	char *rtc_HV_file;
	char *rtc_VH_file;
	char *rtc_VV_file;
	char *rtc_log;
	char *main_log;
	char *mk_geo_radcal_0_log;
	char *mk_geo_radcal_1_log;
	char *mk_geo_radcal_2_log;
	char *mk_geo_radcal_3_log;
	char *coreg_check_log;
	char *mli_par_file;
	char *gamma_version;
	char *gap_rtc;
        char *hyp3_rtc;
        char *ipf;
	char *dem_source;
	char *incidence_angle_file;
	char *incidence_angle_meta;
	char *xml;
	char *latitude;
	char *longitude;
	char *sigma0_hh_fore;
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
  begin_time = meta_get_time(meta, 0.0, 0.0);
  begin_time += meta->state_vectors->second;
  begin_time -= meta->state_vectors->vecs[0].time;
  date_sec2hms(begin_time, &time);
  sprintf(begin, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ", 
    date.year, date.month, date.day, time.hour, time.min, time.sec);
  center_time = meta_get_time(meta, meta->general->line_count/2, 0.0);
  center_time += meta->state_vectors->second;
  center_time -= meta->state_vectors->vecs[0].time;
  date_sec2hms(center_time, &time);
  sprintf(center, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ", 
    date.year, date.month, date.day, time.hour, time.min, time.sec);
  end_time = meta_get_time(meta, meta->general->line_count, 0.0);
  end_time += meta->state_vectors->second;
  end_time -= meta->state_vectors->vecs[0].time;
  date_sec2hms(end_time, &time);
  sprintf(end, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ", 
    date.year, date.month, date.day, time.hour, time.min, time.sec);
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

static void gamma2iso_date(char *line, char *time)
{
  char mon[][5]=
    {"","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
  char buf[100];
#define subStr(start,len,dest) strncpy(buf,&line[start],len);buf[len]=0;sscanf(buf,"%d",dest);
	int i, year, month, day, hour, min, sec; 
  for (i=0; i<13; i++) {
    strncpy(buf, &line[4], 3);
    buf[3] = 0;
    if (strcmp_case(buf, mon[i]) == 0)
      month = i;
  }
	subStr(8,2,&day);
	subStr(11,2,&hour);
	subStr(14,2,&min);
	subStr(17,2,&sec);
	subStr(20,4,&year);
	sprintf(time, "%4d-%02d-%02dT%02d:%02d:%02d.000000Z", 
		year, month, day, hour, min, sec);
}

params_t *params_init(char *type) {

	params_t *params = (params_t *) MALLOC(sizeof(params_t));
	if (strcmp_case(type, "InSAR") == 0) {
	  params->format = (char *) MALLOC(sizeof(char)*10);
	  strcpy(params->format, "HDF5");
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
    params->dem_file = NULL;
    params->dem_url = NULL;
    params->troposphere = NULL;
    params->troposphere_url = NULL;
    params->browse_overview = NULL;
    params->browse_amplitude = NULL;
    params->browse_wrapped_interferogram = NULL;
    params->browse_unwrapped_interferogram = NULL;
    params->browse_correlation = NULL;
    params->master_faraday_rotation = MAGIC_UNSET_DOUBLE;
    params->slave_faraday_rotation = MAGIC_UNSET_DOUBLE;
	}
	else if (strcmp_case(type, "GAMMA RTC") == 0) {
	  params->format = (char *) MALLOC(sizeof(char)*10);
	  strcpy(params->format, "GeoTIFF");
	  params->granule = NULL;
    params->metadata = NULL;
    params->layover_shadow_mask = NULL;
    params->layover_shadow_stats = NULL;
    params->dem_file = NULL;
    params->dem_metadata = NULL;
    params->original_dem = NULL;
    params->input_HH_file = NULL;
    params->input_HV_file = NULL;
    params->input_VH_file = NULL;
    params->input_VV_file = NULL;
    params->rtc_HH_metadata = NULL;
    params->rtc_HV_metadata = NULL;
    params->rtc_VH_metadata = NULL;
    params->rtc_VV_metadata = NULL;
    params->rtc_HH_file = NULL;
    params->rtc_HV_file = NULL;
    params->rtc_VH_file = NULL;
    params->rtc_VV_file = NULL;
    params->rtc_log = NULL;
    params->mk_geo_radcal_0_log = NULL;
    params->mk_geo_radcal_1_log = NULL;
    params->mk_geo_radcal_2_log = NULL;
    params->mk_geo_radcal_3_log = NULL;
    params->coreg_check_log = NULL;
    params->mli_par_file = NULL;
    params->processing_log = NULL;
    params->main_log = NULL;
    params->browse_amplitude = NULL;
    params->kml_overlay = NULL;
    params->gamma_version = NULL;
    params->gap_rtc = NULL;
    params->hyp3_rtc = NULL;
    params->ipf = NULL;
    params->dem_source = NULL;
    params->incidence_angle_file = NULL;
    params->incidence_angle_meta = NULL;
	}
	else if (strcmp_case(type, "SMAP") == 0) {
	  params->format = (char *) MALLOC(sizeof(char)*10);
	  strcpy(params->format, "netCDF");
	  params->data = NULL;
	  params->metadata = NULL;
	  params->xml = NULL;
	  params->latitude = NULL;
	  params->longitude = NULL;
	  params->sigma0_hh_fore = NULL;
  }
	
	return params;
}

void params_free(params_t *params) {
  FREE(params->format);
  FREE(params->granule);
  FREE(params->master_meta_file);
  FREE(params->slave_meta_file);
  FREE(params->master_metadata);
  FREE(params->slave_metadata);
  FREE(params->doppler);
  FREE(params->processing_log);
  FREE(params->baseline);
  FREE(params->wrapped_interferogram);
  FREE(params->unwrapped_interferogram);
  FREE(params->correlation);
  FREE(params->incidence_angle);
  FREE(params->layover_shadow_mask);
  FREE(params->layover_shadow_stats);
  FREE(params->dem_file);
  FREE(params->dem_metadata);
  FREE(params->original_dem);
  FREE(params->dem_url);
  FREE(params->troposphere);
  FREE(params->troposphere_url);
  FREE(params->browse_amplitude);
  FREE(params->browse_wrapped_interferogram);
  FREE(params->browse_unwrapped_interferogram);
  FREE(params->browse_correlation);
  FREE(params->kml_overlay);
  FREE(params->data);
  FREE(params->metadata);
  FREE(params->input_HH_file);
  FREE(params->input_HV_file);
  FREE(params->input_VH_file);
  FREE(params->input_VV_file);
  FREE(params->rtc_HH_metadata);
  FREE(params->rtc_HV_metadata);
  FREE(params->rtc_VH_metadata);
  FREE(params->rtc_VV_metadata);
  FREE(params->rtc_HH_file);
  FREE(params->rtc_HV_file);
  FREE(params->rtc_VH_file);
  FREE(params->rtc_VV_file);
  FREE(params->rtc_log);
  FREE(params->mk_geo_radcal_0_log);
  FREE(params->mk_geo_radcal_1_log);
  FREE(params->mk_geo_radcal_2_log);
  FREE(params->mk_geo_radcal_3_log);
  FREE(params->coreg_check_log);
  FREE(params->mli_par_file);
  FREE(params->main_log);
  FREE(params->gamma_version);
  FREE(params->gap_rtc);
  FREE(params->hyp3_rtc);
  FREE(params->ipf);
  FREE(params->dem_source);
  FREE(params->incidence_angle_file);
  FREE(params->incidence_angle_meta);
  FREE(params->xml);
  FREE(params->latitude);
  FREE(params->longitude);
  FREE(params->sigma0_hh_fore);
  FREE(params);
}

static int matches_key(const char *line, const char *key)
{
  return strncmp(line, key, strlen(key)) == 0;
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
			if (matches_key(line, "format"))
				list->format = trim_spaces(value);
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
				list->dem_file = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->dem_file, "%s", trim_spaces(value));
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
			if (strncmp_case(line, "browse overview", 15) == 0) {
				list->browse_overview = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->browse_overview, "%s", trim_spaces(value));
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
  else if (strncmp_case(line, "[GAMMA RTC]", 11) == 0) {
  	list = params_init("GAMMA RTC");
  	sprintf(type, "GAMMA RTC");
		while (NULL != fgets(line, n, fp)) {
			p = strchr(line, '=');
			sprintf(value, "%s", p+1);
			if (matches_key(line, "format"))
				list->format = trim_spaces(value);
			if (strncmp_case(line, "granule", 7) == 0) {
				list->granule = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->granule, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "metadata", 8) == 0) {
				list->metadata = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->metadata, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "oversampled dem file", 20) == 0) {
				list->dem_file = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->dem_file, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "oversampled dem metadata", 24) == 0) {
				list->dem_metadata = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->dem_metadata, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "original dem", 12) == 0) {
				list->original_dem = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->original_dem, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "layover shadow mask", 19) == 0) {
			  list->layover_shadow_mask = (char *) MALLOC(sizeof(char)*n);
			  sprintf(list->layover_shadow_mask, "%s", trim_spaces(value));
      }			
			if (strncmp_case(line, "layover shadow stats", 20) == 0) {
			  list->layover_shadow_stats = (char *) MALLOC(sizeof(char)*n);
			  sprintf(list->layover_shadow_stats, "%s", trim_spaces(value));
      }			
			if (strncmp_case(line, "input HH file", 13) == 0) {
			  list->input_HH_file = (char *) MALLOC(sizeof(char)*n);
			  sprintf(list->input_HH_file, "%s", trim_spaces(value));
      }			
			if (strncmp_case(line, "input HV file", 13) == 0) {
			  list->input_HV_file = (char *) MALLOC(sizeof(char)*n);
			  sprintf(list->input_HV_file, "%s", trim_spaces(value));
      }			
			if (strncmp_case(line, "input VH file", 13) == 0) {
			  list->input_VH_file = (char *) MALLOC(sizeof(char)*n);
			  sprintf(list->input_VH_file, "%s", trim_spaces(value));
      }			
			if (strncmp_case(line, "input VV file", 13) == 0) {
			  list->input_VV_file = (char *) MALLOC(sizeof(char)*n);
			  sprintf(list->input_VV_file, "%s", trim_spaces(value));
      }			
			if (strncmp_case(line, "terrain corrected HH metadata", 29) == 0) {
				list->rtc_HH_metadata = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->rtc_HH_metadata, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "terrain corrected HV metadata", 29) == 0) {
				list->rtc_HV_metadata = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->rtc_HV_metadata, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "terrain corrected VH metadata", 29) == 0) {
				list->rtc_VH_metadata = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->rtc_VH_metadata, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "terrain corrected VV metadata", 29) == 0) {
				list->rtc_VV_metadata = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->rtc_VV_metadata, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "terrain corrected HH file", 25) == 0) {
				list->rtc_HH_file = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->rtc_HH_file, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "terrain corrected HV file ", 25) == 0) {
				list->rtc_HV_file = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->rtc_HV_file, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "terrain corrected VH file", 25) == 0) {
				list->rtc_VH_file = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->rtc_VH_file, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "terrain corrected VV file", 25) == 0) {
				list->rtc_VV_file = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->rtc_VV_file, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "initial processing log", 22) == 0) {
				list->processing_log = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->processing_log, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "terrain correction log", 22) == 0) {
				list->rtc_log = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->rtc_log, "%s", trim_spaces(value));
			}
			if (matches_key(line, "mk_geo_radcal_0 log"))
				list->mk_geo_radcal_0_log = trim_spaces(value);
			if (matches_key(line, "mk_geo_radcal_1 log"))
				list->mk_geo_radcal_1_log = trim_spaces(value);
			if (matches_key(line, "mk_geo_radcal_2 log"))
				list->mk_geo_radcal_2_log = trim_spaces(value);
			if (matches_key(line, "mk_geo_radcal_3 log"))
				list->mk_geo_radcal_3_log = trim_spaces(value);
			if (matches_key(line, "coreg_check log"))
				list->coreg_check_log = trim_spaces(value);
			if (matches_key(line, "mli.par file"))
				list->mli_par_file = trim_spaces(value);
			if (strncmp_case(line, "main log", 8) == 0) {
				list->main_log = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->main_log, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "gamma version", 13) == 0) {
				list->gamma_version = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->gamma_version, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "gap_rtc version", 15) == 0) {
			  list->gap_rtc = (char *) MALLOC(sizeof(char)*n);
			  sprintf(list->gap_rtc, "%s", trim_spaces(value));
			}
                        if (strncmp_case(line, "hyp3_rtc version", 16) == 0) {
                          list->hyp3_rtc = (char *) MALLOC(sizeof(char)*10);
                          sprintf(list->hyp3_rtc, "%s", trim_spaces(value));
                        }
                        if (strncmp_case(line, "ipf version", 11) == 0) {
                          list->ipf = (char *) MALLOC(sizeof(char)*10);
                          sprintf(list->ipf, "%s", trim_spaces(value));
                        }
			if (strncmp_case(line, "dem source", 10) == 0) {
				list->dem_source = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->dem_source, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "browse image", 12) == 0) {
				list->browse_amplitude = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->browse_amplitude, "%s", trim_spaces(value));
			}
			if (strncmp_case(line, "kml overlay", 11) == 0) {
				list->kml_overlay = (char *) MALLOC(sizeof(char)*n);
				sprintf(list->kml_overlay, "%s", trim_spaces(value));
			}
			if (matches_key(line, "incidence angle file"))
				list->incidence_angle_file = trim_spaces(value);
			if (matches_key(line, "incidence angle meta"))
				list->incidence_angle_meta = trim_spaces(value);
		}
  }
  else if (matches_key(line, "[SMAP]")) {
  	list = params_init("SMAP");
  	sprintf(type, "SMAP");
		while (NULL != fgets(line, n, fp)) {
			p = strchr(line, '=');
			sprintf(value, "%s", p+1);
			if (matches_key(line, "format"))
				list->format = trim_spaces(value);
			if (matches_key(line, "metadata"))
			  list->metadata = trim_spaces(value);
			if (matches_key(line, "xml"))
			  list->xml = trim_spaces(value);
			if (matches_key(line, "latitude"))
			  list->latitude = trim_spaces(value);
			if (matches_key(line, "longitude"))
			  list->longitude = trim_spaces(value);
			if (matches_key(line, "sigma0_hh_fore"))
			  list->sigma0_hh_fore = trim_spaces(value);
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

  FILE *fp=NULL, *fpFiles = NULL;
  meta_parameters *meta = NULL;
  params_t *params = NULL;
  char line[255], str[50], isoStr[50], rscFile[512], logFile[512];
  char directory[768], filename[768], creation[30], map_projection[50];
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

    meta = NULL;
    fp = FOPEN(xml_name, "wt");
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
    if (params->dem_file) {
      sprintf(file_name, "%s.img", params->dem_file);
      if (fileExists(file_name))
        fprintf(fp, "    <digital_elevation_model>%s</digital_elevation_model>\n",
          params->dem_file);
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
      if (strcmp_case(meta->sar->polarization, "HH") == 0) {
        fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
          "\"polarization of transmitted signal\">horizontal</"
          "transmitted_polarization>\n");
        fprintf(fp, "      <received_polarization type=\"string\" definition=\""
          "polarization of received signal\">horizontal</received_polarization>"
          "\n");
      }
      else if (strcmp_case(meta->sar->polarization, "HV") == 0) {
        fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
          "\"polarization of transmitted signal\">horizontal</"
          "transmitted_polarization>\n");
        fprintf(fp, "      <received_polarization type=\"string\" definition=\""
          "polarization of received signal\">vertical</received_polarization>\n");
      }
      else if (strcmp_case(meta->sar->polarization, "VH") == 0) {
        fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
          "\"polarization of transmitted signal\">vertical</"
          "transmitted_polarization>\n");
        fprintf(fp, "      <received_polarization type=\"string\" definition=\""
          "polarization of received signal\">horizontal</received_polarization>"
          "\n");
      }
      else if (strcmp_case(meta->sar->polarization, "VV") == 0) {
        fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
          "\"polarization of transmitted signal\">vertical</"
          "transmitted_polarization>\n");
        fprintf(fp, "      <received_polarization type=\"string\" definition=\""
          "polarization of received signal\">vertical</received_polarization>\n");
      }
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
      if (strcmp_case(meta->sar->polarization, "HH") == 0) {
        fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
          "\"polarization of transmitted signal\">horizontal</"
          "transmitted_polarization>\n");
        fprintf(fp, "      <received_polarization type=\"string\" definition=\""
          "polarization of received signal\">horizontal</received_polarization>"
          "\n");
      }
      else if (strcmp_case(meta->sar->polarization, "HV") == 0) {
        fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
          "\"polarization of transmitted signal\">horizontal</"
          "transmitted_polarization>\n");
        fprintf(fp, "      <received_polarization type=\"string\" definition=\""
          "polarization of received signal\">vertical</received_polarization>\n");
      }
      else if (strcmp_case(meta->sar->polarization, "VH") == 0) {
        fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
          "\"polarization of transmitted signal\">vertical</"
          "transmitted_polarization>\n");
        fprintf(fp, "      <received_polarization type=\"string\" definition=\""
          "polarization of received signal\">horizontal</received_polarization>"
          "\n");
      }
      else if (strcmp_case(meta->sar->polarization, "VV") == 0) {
        fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
          "\"polarization of transmitted signal\">vertical</"
          "transmitted_polarization>\n");
        fprintf(fp, "      <received_polarization type=\"string\" definition=\""
          "polarization of received signal\">vertical</received_polarization>\n");
      }
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
      fprintf(fp, "      <projection_string type=\"string\" definition=\"map "
      "projection as well known text\">GEOGCS[\"GCS_WGS_1984\",DATUM[\""
      "D_WGS_1984\",SPHEROID[\"WGS_1984\",6378137,298.257223563]],PRIMEM[\""
      "Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]]"
      "</projection_string>\n");
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
      fprintf(fp, "      <projection_string type=\"string\" definition=\"map "
      "projection as well known text\">GEOGCS[\"GCS_WGS_1984\",DATUM[\""
      "D_WGS_1984\",SPHEROID[\"WGS_1984\",6378137,298.257223563]],PRIMEM[\""
      "Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]]"
      "</projection_string>\n");
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
        fprintf(fp, "      <phase_minimum type=\"double\" definition=\"minimum "
          "phase value\">%g</phase_minimum>\n", meta->stats->band_stats[0].min);
        fprintf(fp, "      <phase_maximum type=\"double\" definition=\"maximum "
          "phase value\">%g</phase_maximum>\n", meta->stats->band_stats[0].max);
        fprintf(fp, "      <percent_unwrapped type=\"double\" definition=\""
          "percentage of unwrapped phase\" units=\"percent\">%g</percent_unwrapped>\n",
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
      fprintf(fp, "      <projection_string type=\"string\" definition=\"map "
      "projection as well known text\">GEOGCS[\"GCS_WGS_1984\",DATUM[\""
      "D_WGS_1984\",SPHEROID[\"WGS_1984\",6378137,298.257223563]],PRIMEM[\""
      "Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]]"
      "</projection_string>\n");
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
        fprintf(fp, "      <average_coherence type=\"double\" definition=\""
          "average coherence\">%g</average_coherence>\n",
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
      fprintf(fp, "      <projection_string type=\"string\" definition=\"map "
      "projection as well known text\">GEOGCS[\"GCS_WGS_1984\",DATUM[\""
      "D_WGS_1984\",SPHEROID[\"WGS_1984\",6378137,298.257223563]],PRIMEM[\""
      "Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]]"
      "</projection_string>\n");
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
    if (params->dem_file) {
      sprintf(rscFile, "%s.rsc", params->dem_file);
      if (fileExists(rscFile)) {
        dem = TRUE;
        fpFiles = FOPEN(rscFile, "rt");
        fprintf(fp, "    <digital_elevation_model>\n");
        split_dir_and_file(params->dem_file, directory, filename);
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
              fprintf(fp, "      <projection_string type=\"string\" definition"
              "=\"map projection as well known text\">GEOGCS[\"GCS_WGS_1984\","
              "DATUM[\"D_WGS_1984\",SPHEROID[\"WGS_1984\",6378137,298.257223563"
              "]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.017453292519943295"
              "]]</projection_string>\n");
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
              fprintf(fp, "      <projection_string type=\"string\" definition"
              "=\"map projection as well known text\">GEOGCS[\"GCS_WGS_1984\","
              "DATUM[\"D_WGS_1984\",SPHEROID[\"WGS_1984\",6378137,298.257223563"
              "]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.017453292519943295"
              "]]</projection_string>\n");
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
    if (params->dem_file) {
      char metaFile[512];
      sprintf(metaFile, "%s.meta", params->dem_file);
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
      if (params->dem_file) {
        char metaFile[512];
        sprintf(metaFile, "%s.meta", params->dem_file);
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
    char data_source[25], processing[100], copyright[50], original_file[200];
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
    if (params->dem_file && fileExists(params->dem_file))
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
    fprintf(fp, "    <history>%s: %s file created.</history>\n", creation,
      params->format);
    fprintf(fp, "  </root>\n");
    
    fprintf(fp, "</hdf5>\n");
    FCLOSE(fp);
  }
  else if (strcmp_case(type, "GAMMA RTC") == 0) {

    meta = NULL;
    fp = FOPEN(xml_name, "wt");
    fprintf(fp, "<hdf5>\n");
    fprintf(fp, "  <granule>%s</granule>\n", params->granule);
    fprintf(fp, "  <metadata_creation>%s</metadata_creation>\n", creation);

    // Let's check for the files first
    if (params->metadata && !fileExists(params->metadata))
      asfPrintError("Metadata file (%s) is missing!\n", params->metadata);
    if (params->dem_file && !fileExists(params->dem_file))
      asfPrintError("Oversampled digital elevation model (%s) is missing!\n",
        params->dem_file);
    if (params->original_dem && !fileExists(params->original_dem))
      asfPrintError("Original digital elevation model (%s) is missing!\n",
        params->original_dem);
    if (params->rtc_HH_metadata && !fileExists(params->rtc_HH_metadata))
      asfPrintError("Metadata for terrain corrected file (%s) is missing!\n",
        params->rtc_HH_metadata);
    if (params->rtc_HH_file && !fileExists(params->rtc_HH_file))
      asfPrintError("Terrain corrected file (%s) is missing!\n", 
        params->rtc_HH_file);
    if (params->rtc_VV_metadata && !fileExists(params->rtc_VV_metadata))
      asfPrintError("Metadata for terrain corrected file (%s) is missing!\n",
        params->rtc_VV_metadata);
    if (params->rtc_VV_file && !fileExists(params->rtc_VV_file))
      asfPrintError("Terrain corrected file (%s) is missing!\n", 
        params->rtc_VV_file);
    if (params->incidence_angle_file && 
      !fileExists(params->incidence_angle_file))
      asfPrintError("Incidence angle map (%s) is missing!\n", 
        params->incidence_angle_file);
    if ((params->browse_amplitude && fileExists(params->browse_amplitude)) ||
      (params->kml_overlay && fileExists(params->kml_overlay)))
        browse = TRUE;
    if (params->browse_amplitude && !fileExists(params->browse_amplitude))
      asfPrintError("Browse image file (%s) is missing!\n", 
        params->browse_amplitude);
    if (params->kml_overlay && !fileExists(params->kml_overlay))
      asfPrintError("KML overlay file (%s) is missing!\n", params->kml_overlay);

    // Fill in the data section
    fprintf(fp, "  <data>\n");
    if (params->rtc_HH_file)
      fprintf(fp, "    <terrain_corrected_HH>%s</terrain_corrected_HH>\n",
        params->rtc_HH_file);
    if (params->rtc_HV_file)
      fprintf(fp, "    <terrain_corrected_HV>%s</terrain_corrected_HV>\n",
        params->rtc_HV_file);
    if (params->rtc_VH_file)
      fprintf(fp, "    <terrain_corrected_VH>%s</terrain_corrected_VH>\n",
        params->rtc_VH_file);
    if (params->rtc_VV_file)
      fprintf(fp, "    <terrain_corrected_VV>%s</terrain_corrected_VV>\n",
        params->rtc_VV_file);
    fprintf(fp, "    <digital_elevation_model>%s</digital_elevation_model>\n",
      params->original_dem);
    fprintf(fp, "    <layover_shadow_mask>%s</layover_shadow_mask>\n",
      params->layover_shadow_mask);
    fprintf(fp, "    <incidence_angle_map>%s</incidence_angle_map>\n",
      params->incidence_angle_file);
    fprintf(fp, "  </data>\n");

    // Add metadata
    char original_file[200];
    char *begin = (char *) MALLOC(sizeof(char)*30);
    char *center = (char *) MALLOC(sizeof(char)*30);
    char *end = (char *) MALLOC(sizeof(char)*30);
    int year=0, month=0, day=0;
    hms_time time;
    fprintf(fp, "  <metadata>\n");
   
    int range_looks=0, azimuth_looks=0;
    double slant_spacing=0, azimuth_spacing=0;
    double dop0=0, dop1=0, dop2=0, ddop0=0, ddop1=0, ddop2=0;
    double slant_first=0, slant_center=0, slant_last=0, prf=0;
    double frequency, wavelength=0, start_sec=0, center_sec=0, end_sec=0;
    double speedOfLight = 299792458.0;
    char platform[15], beam_mode[15];
    if (params->mli_par_file && fileExists(params->mli_par_file)) {
      fpFiles = FOPEN(params->mli_par_file, "r");
      while (NULL != fgets(line, 255, fpFiles)) {
        char *key, *value;
        split2(line, ':', &key, &value);
        if (strcmp(key, "sensor") == 0) {
          if (strstr(value, "ERS1"))
            strcpy(platform, "ERS-1");
          else if (strstr(value, "ERS2"))
            strcpy(platform, "ERS-2");
          else if (strstr(value, "PALSAR"))
            strcpy(platform, "ALOS");
          else if (strstr(value, "S1A"))
            strcpy(platform, "Sentinel-1A");
          else if (strstr(value, "S1B"))
            strcpy(platform, "Sentinel-1B");
          else
            asfPrintError("Could not identify sensor!\n");
        }
        else if (strcmp(key, "date") == 0)
          sscanf(value, "%d %d %d", &year, &month, &day);
        else if (strcmp(key, "start_time") == 0)
          sscanf(value, "%lf", &start_sec);
        else if (strcmp(key, "center_time") == 0)
          sscanf(value, "%lf", &center_sec);
        else if (strcmp(key, "end_time") == 0)
          sscanf(value, "%lf", &end_sec);
        else if (strcmp(key, "doppler_polynomial") == 0)
          sscanf(value, "%lf %lf %lf", &dop0, &dop1, &dop2);
        else if (strcmp(key, "doppler_poly_dot") == 0)
          sscanf(value, "%lf %lf %lf", &ddop0, &ddop1, &ddop2);
        else if (strcmp(key, "near_range_slc") == 0)
          sscanf(value, "%lf", &slant_first);
        else if (strcmp(key, "center_range_slc") == 0)
          sscanf(value, "%lf", &slant_center);
        else if (strcmp(key, "far_range_slc") == 0)
          sscanf(value, "%lf", &slant_last);
        else if (strcmp(key, "prf") == 0)
          sscanf(value, "%lf", &prf);
        else if (strcmp(key, "radar_frequency") == 0) {
          sscanf(value, "%lf", &frequency);
          wavelength = speedOfLight / frequency;
        }
        else if (strcmp(key, "range_looks") == 0)
          sscanf(value, "%d", &range_looks);
        else if (strcmp(key, "azimuth_looks") == 0)
          sscanf(value, "%d", &azimuth_looks);
        else if (strcmp(key, "range_pixel_spacing") == 0)
          sscanf(value, "%lf", &slant_spacing);
        else if (strcmp(key, "azimuth_pixel_spacing") == 0)
          sscanf(value, "%lf", &azimuth_spacing);
      }
      FCLOSE(fpFiles);
      date_sec2hms(start_sec, &time);
      sprintf(begin, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ", 
        year, month, day, time.hour, time.min, time.sec);
      date_sec2hms(center_sec, &time);
      sprintf(center, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ", 
        year, month, day, time.hour, time.min, time.sec);
      date_sec2hms(end_sec, &time);
      sprintf(end, "%4d-%02d-%02dT%02d:%02d:%09.6lfZ", 
        year, month, day, time.hour, time.min, time.sec);
    }
    else
      asfPrintError("Could not fine MLI parameter file!\n");

    double range_offset=0, azimuth_offset=0;
    double range_stddev=0, azimuth_stddev=0;
    int patches_accepted=0, patches_attempted=0;
    if (params->mk_geo_radcal_2_log && fileExists(params->mk_geo_radcal_2_log)) {
      fpFiles = FOPEN(params->mk_geo_radcal_2_log, "r");
      while (NULL != fgets(line, 255, fpFiles)) {
        char *key, *value;
        split2(line, ':', &key, &value);
        if (strcmp(key, "final range offset poly. coeff.") == 0)
          range_offset = atof(value);
        else if (strcmp(key, "final azimuth offset poly. coeff.") == 0)
          azimuth_offset = atof(value);
        else if (strcmp(key, "final model fit std. dev. (samples) range") == 0)
          sscanf(value, "%lf azimuth: %lf", &range_stddev, &azimuth_stddev);
        else if (strcmp(key, "final solution") == 0 && strstr(value, "offset estimates accepted out of"))
          sscanf(value, "%d offset estimates accepted out of %d samples", &patches_accepted, &patches_attempted);
      }
      FCLOSE(fpFiles);
    }

    int passed;
    if (fileExists(params->coreg_check_log)) {
      passed = -1;
      fpFiles = FOPEN(params->coreg_check_log, "r");
      while (NULL != fgets(line, 255, fpFiles)) {
        if (strstr(line, "passed coregistration")) {
          passed = TRUE;
        }
        else if (strstr(line, "failed coregistration")) {
          passed = FALSE;
        }
      }
      FCLOSE(fpFiles);
      if (passed == -1)
        asfPrintError("Could not determine if this granule passed coregistration!");
    }
    else {
      passed = FALSE;
    }

    // Original input image
    fprintf(fp, "    <input_image>\n");

    int orbit=0, frame=0;
    char orbitStr[10], frameStr[10];
    if (params->metadata && fileExists(params->metadata))
      meta = meta_read(params->metadata);
    else
      asfPrintError("Metadata file does not exist!\n");
    if (strcmp_case(platform, "ALOS") == 0) {
      strcpy(beam_mode, meta->general->mode);
      strcpy(original_file, params->input_HH_file);
      if (params->input_HV_file) {
        sprintf(original_file, "%s,%s", params->input_HH_file, 
          params->input_HV_file);
      }
      if (params->input_VH_file && params->input_VV_file) {
        sprintf(original_file, "%s,%s,%s,%s", params->input_HH_file, 
          params->input_HV_file, params->input_VH_file, params->input_VV_file);
      }
      char *fileName = (char *) MALLOC(sizeof(char)*strlen(original_file));
      strcpy(fileName, original_file);
      strncpy(orbitStr, &fileName[3], 5);
      orbitStr[6] = '\0';
      orbit = atoi(orbitStr);
      strncpy(frameStr, &fileName[14], 4);
      frameStr[5] = '\0';
      frame = atoi(frameStr);
      FREE(fileName);      
    }
    else if (strncmp_case(platform, "SENTINEL", 8) == 0) {
      if (params->input_HH_file)
        strcpy(original_file, params->input_HH_file);
      else if (params->input_VV_file)
        strcpy(original_file, params->input_VV_file);
      else
        asfPrintError("Could not find metadata file\n");
      strcpy(beam_mode, meta->general->mode);
      strncpy(orbitStr, &meta->general->basename[49], 6);
      orbit = atoi(orbitStr); 
    }
    else {
      strcpy(beam_mode, "STD");
      if (params->input_HH_file)
        strcpy(original_file, params->input_HH_file);
      else if (params->input_VV_file)
        strcpy(original_file, params->input_VV_file);
      else
        asfPrintError("Could not find metadata file\n");
      if (strncmp_case(platform, "ERS", 3) == 0) {
        char *fileName = (char *) MALLOC(sizeof(char)*strlen(original_file));
        strcpy(fileName, original_file);
        strncpy(orbitStr, &fileName[3], 5);
        orbitStr[6] = '\0';
        orbit = atoi(orbitStr);
        strncpy(frameStr, &fileName[15], 3);
        frameStr[4] = '\0';
        frame = atoi(frameStr);
        FREE(fileName);
      }
    }
    fprintf(fp, "      <file type=\"string\" definition=\"file name(s) of the "
      "input image\">%s</file>\n", original_file);
    fprintf(fp, "      <platform type=\"string\" definition=\"name of the "
      "platform\">%s</platform>\n", platform);
    if (strcmp_case(platform, "ALOS") == 0)
      fprintf(fp, "      <sensor type=\"string\" definition=\"name of the "
        "sensor\">PALSAR</sensor>\n");
    else
      fprintf(fp, "      <sensor type=\"string\" definition=\"name of the "
        "sensor\">SAR</sensor>\n");
    fprintf(fp, "      <wavelength type=\"double\" definition=\"wavelength of "
      "the sensor\" units=\"m\">%g</wavelength>\n", wavelength);
    fprintf(fp, "      <beam_mode type=\"string\" definition=\"beam mode of the"
      " sensor\">%s</beam_mode>\n", beam_mode);
    fprintf(fp, "      <absolute_orbit type=\"int\" definition=\"absolute orbit"
      " of the image\">%d</absolute_orbit>\n", orbit);
    if (frame > 0)
      fprintf(fp, "      <frame type=\"int\" definition=\"frame number of the "
        "image\">%d</frame>\n",  frame);
    if (meta->general->orbit_direction == 'A')
      fprintf(fp, "      <flight_direction type=\"string\" definition=\"flight "
      "direction of the sensor\">ascending</flight_direction>\n");
    else if (meta->general->orbit_direction == 'D')
      fprintf(fp, "      <flight_direction type=\"string\" definition=\"flight "
      "direction of the sensor\">descending</flight_direction>\n");
    if (params->input_HH_file) {
      fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
        "\"polarization of transmitted signal\">horizontal</"
        "transmitted_polarization>\n");
      fprintf(fp, "      <received_polarization type=\"string\" definition=\""
        "polarization of received signal\">horizontal</received_polarization>"
        "\n");
    }
    else if (params->input_HV_file) {
      fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
        "\"polarization of transmitted signal\">horizontal</"
        "transmitted_polarization>\n");
      fprintf(fp, "      <received_polarization type=\"string\" definition=\""
        "polarization of received signal\">vertical</received_polarization>\n");
    }
    else if (params->input_VH_file) {
      fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
        "\"polarization of transmitted signal\">vertical</"
        "transmitted_polarization>\n");
      fprintf(fp, "      <received_polarization type=\"string\" definition=\""
        "polarization of received signal\">horizontal</received_polarization>"
        "\n");
    }
    else if (params->input_HH_file) {
      fprintf(fp, "      <transmitted_polarization type=\"string\" definition="
        "\"polarization of transmitted signal\">vertical</"
        "transmitted_polarization>\n");
      fprintf(fp, "      <received_polarization type=\"string\" definition=\""
        "polarization of received signal\">vertical</received_polarization>\n");
    }
    if (meta->general->image_data_type == COMPLEX_IMAGE)
      fprintf(fp, "      <data_processing_level type=\"string\" definition=\""
        "processing level of the input data\">single look complex"
        "</data_processing_level>\n");
    if (strncmp_case(platform, "SENTINEL", 8) == 0)
      fprintf(fp, "      <data_format type=\"string\" definition=\"data format "
        "of input data\">SAFE</data_format>\n");
    else
      fprintf(fp, "      <data_format type=\"string\" definition=\"data format "
        "of input data\">CEOS</data_format>\n");
    fprintf(fp, "      <prf type=\"double\" definition=\"pulse repetition "
      "frequency\" units=\"Hz\">%g</prf>\n", prf);
    fprintf(fp, "      <start_datetime type=\"string\" definition=\"UTC "
      "time at the start of the image\">%s</start_datetime>\n", begin); 	
    fprintf(fp, "      <center_datetime type=\"string\" definition=\"UTC "
      "time at the center of the image\">%s</center_datetime>\n", center);
    fprintf(fp, "      <end_datetime type=\"string\" definition=\"UTC "
      "time at the end of the image\">%s</end_datetime>\n", end);
    fprintf(fp, "      <doppler_poly_0 type=\"double\" definition=\"Doppler "
      "polynomial\" units=\"Hz\">%g</doppler_poly_0>\n", dop0);
    fprintf(fp, "      <doppler_poly_1 type=\"double\" definition=\"Doppler "
      "polynomial\" units=\"Hz/m\">%g</doppler_poly_1>\n", dop1);
    fprintf(fp, "      <doppler_poly_2 type=\"double\" definition=\"Doppler "
      "polynomial\" units=\"Hz/m^2\">%g</doppler_poly_2>\n", dop2);
    fprintf(fp, "      <doppler_poly_dot_0 type=\"double\" definition=\"Doppler "
      "polynomial dot\" units=\"Hz/s\">%g</doppler_poly_dot_0>\n", ddop0);
    fprintf(fp, "      <doppler_poly_dot_1 type=\"double\" definition=\"Doppler "
      "polynomial dot\" units=\"Hz/s/m\">%g</doppler_poly_dot_1>\n", ddop1);
    fprintf(fp, "      <doppler_poly_dot_2 type=\"double\" definition=\"Doppler "
      "polynomial dot\" units=\"Hz/s/m^2\">%g</doppler_poly_dot_2>\n", ddop2);
    fprintf(fp, "      <range_looks type=\"int\" definition=\"Number of range "
      "looks\">%d</range_looks>\n", range_looks);
    fprintf(fp, "      <azimuth_looks type=\"int\" definition=\"Number of azimuth "
      "looks\">%d</azimuth_looks>\n", azimuth_looks);
    fprintf(fp, "      <slant_spacing type=\"double\" definition=\"Pixel size in "
      "slant range\" units=\"m\">%g</slant_spacing>\n", slant_spacing);
    fprintf(fp, "      <azimuth_spacing type=\"double\" definition=\"Pixel size in "
      "azimuth\" units=\"m\">%g</azimuth_spacing>\n", azimuth_spacing);
    fprintf(fp, "      <slant_to_first type=\"double\" definition=\"Slant range "
      "distance to the near-range edge of the image\" units=\"m\">%.3f"
      "</slant_to_first>\n", slant_first);
    fprintf(fp, "      <slant_to_center type=\"double\" definition=\"Slant range "
      "distance to the center of the image\" units=\"m\">%.3f"
      "</slant_to_center>\n", slant_center);
    fprintf(fp, "      <slant_to_last type=\"double\" definition=\"Slant range "
      "distance to the far-range edge of the image\" units=\"m\">%.3f"
      "</slant_to_last>\n", slant_last);
    strncpy(str, center, 4);
    year = atoi(str);
    FREE(begin);
    FREE(center);
    FREE(end);
    meta_free(meta);
    fprintf(fp, "      <data_source type=\"string\" definition=\"source "
      "providing the data\">Alaska Satellite Facility</data_source>\n");
    if (params->ipf)
      fprintf(fp, "      <ipf_version type=\"string\" definition=\"version "
        "number of the ESA processor used for the processing of the Sentinel-1 "
        "data\">%s</ipf_version>\n", params->ipf);
    fprintf(fp, "    </input_image>\n");
    
    // Terrain corrected result
    char hh[256]="", hv[256]="", vh[256]="", vv[256]="";
    if (params->rtc_HH_file)
      split_dir_and_file(params->rtc_HH_file, directory, filename);
    else if (params->rtc_VV_file)
      split_dir_and_file(params->rtc_VV_file, directory, filename);
    else
      asfPrintError("Could not find terrain corrected product!\n");
    sprintf(hh, "%s", filename);
    if (strcmp_case(beam_mode, "FBD") == 0 || 
        strcmp_case(beam_mode, "PLR") == 0) {
      split_dir_and_file(params->rtc_HV_file, directory, filename);
      sprintf(hv, ",%s", filename);
    }
    if (strcmp_case(beam_mode, "PLR") == 0) {
      split_dir_and_file(params->rtc_VH_file, directory, filename);
      sprintf(vh, ",%s", filename);
      split_dir_and_file(params->rtc_VV_file, directory, filename);
      sprintf(vv, ",%s", filename);
    }
    sprintf(filename, "%s%s%s%s", hh, hv, vh, vv);
    fprintf(fp, "    <terrain_corrected_image>\n");
    if (params->rtc_HH_metadata)
      meta = meta_read(params->rtc_HH_metadata);
    else if (params->rtc_VV_metadata)
      meta = meta_read(params->rtc_VV_metadata);
    else
      asfPrintError("Could not find metadata for terrain corrected product\n");
    fprintf(fp, "      <file type=\"string\" definition=\"file name(s) of the "
      "terrain corrected image\">%s</file>\n", filename);
    fprintf(fp, "      <width type=\"int\" definition=\"width of the image\">"
      "%d</width>\n", meta->general->sample_count);
    fprintf(fp, "      <height type=\"int\" definition=\"height of the image\">"
      "%d</height>\n", meta->general->line_count);
    fprintf(fp, "      <x_spacing type=\"double\" definition=\"spacing in x "
      "direction\">%g</x_spacing>\n", meta->general->x_pixel_size);
    fprintf(fp, "      <y_spacing type=\"double\" definition=\"spacing in y "
      "direction\">%g</y_spacing>\n", meta->general->y_pixel_size);
    fprintf(fp, "      <software type=\"string\" definition=\"version of the "
      "software used for terrain correction\">%s</software>\n", 
      params->gamma_version);
    fprintf(fp, "      <projection_string type=\"string\" definition=\"map "
      "projection information as well known text\">%s</projection_string>\n", 
      meta2esri_proj(meta, NULL));
    fprintf(fp, "    </terrain_corrected_image>\n");
    meta_free(meta);

    // Digital elevation model
    split_dir_and_file(params->original_dem, directory, filename);
    int ii, ignore[MAX_BANDS];
    for (ii=0; ii<MAX_BANDS; ii++)
      ignore[ii] = 0;
    meta_parameters *dem_meta = 
      read_generic_geotiff_metadata(params->original_dem, ignore, "DEM");
    fprintf(fp, "    <digital_elevation_model>\n");
    fprintf(fp, "      <file type=\"string\" definition=\"file name of the "
      "digital elevation model\">%s</file>\n", filename);    
    fprintf(fp, "      <width type=\"int\" definition=\"width of the image\">"
      "%d</width>\n", dem_meta->general->sample_count);
    fprintf(fp, "      <height type=\"int\" definition=\"height of the image\">"
      "%d</height>\n", dem_meta->general->line_count);
    fprintf(fp, "      <x_spacing type=\"double\" definition=\"spacing in x "
      "direction\">%g</x_spacing>\n", dem_meta->general->x_pixel_size);
    fprintf(fp, "      <y_spacing type=\"double\" definition=\"spacing in y "
      "direction\">%g</y_spacing>\n", dem_meta->general->y_pixel_size);
    fprintf(fp, "      <source type=\"string\" definition=\"source where the "
      "digital elevation model came from\">%s</source>\n", params->dem_source);
    fprintf(fp, "      <projection_string type=\"string\" definition=\"map "
      "projection information as well known text\">%s</projection_string>\n", 
      meta2esri_proj(dem_meta, NULL));
    fprintf(fp, "    </digital_elevation_model>\n");
    meta_free(dem_meta);

    // Layover shadow mask
    split_dir_and_file(params->layover_shadow_mask, directory, filename);
    if (params->rtc_HH_metadata)
      meta = meta_read(params->rtc_HH_metadata);
    else if (params->rtc_VV_metadata)
      meta = meta_read(params->rtc_VV_metadata);
    else 
      asfPrintError("Could not find metadata for layover shadow mask\n");
    fprintf(fp, "    <layover_shadow_mask>\n");
    fprintf(fp, "      <file type=\"string\" definition=\"file name of the "
      "layover shadow mask\">%s</file>\n", filename);
    fprintf(fp, "      <width type=\"int\" definition=\"width of the image\">"
      "%d</width>\n", meta->general->sample_count);
    fprintf(fp, "      <height type=\"int\" definition=\"height of the image\">"
      "%d</height>\n", meta->general->line_count);
    fprintf(fp, "      <x_spacing type=\"double\" definition=\"spacing in x "
      "direction\">%g</x_spacing>\n", meta->general->x_pixel_size);
    fprintf(fp, "      <y_spacing type=\"double\" definition=\"spacing in y "
      "direction\">%g</y_spacing>\n", meta->general->y_pixel_size);
    fprintf(fp, "    </layover_shadow_mask>\n");
    meta_free(meta);

    // Incidence angle map
    split_dir_and_file(params->incidence_angle_file, directory, filename);
    if (params->rtc_HH_metadata)
      meta = meta_read(params->rtc_HH_metadata);
    else if (params->rtc_VV_metadata)
      meta = meta_read(params->rtc_VV_metadata);
    else
      asfPrintError("Could not find metdata for incidence angle map\n");
    fprintf(fp, "    <incidence_angle_map>\n");
    fprintf(fp, "      <file type=\"string\" definition=\"file name of the "
      "incidence angle map\">%s</file>\n", filename);
    fprintf(fp, "      <width type=\"int\" definition=\"width of the image\">"
      "%d</width>\n", meta->general->sample_count);
    fprintf(fp, "      <height type=\"int\" definition=\"height of the image\">"
      "%d</height>\n", meta->general->line_count);
    fprintf(fp, "      <x_spacing type=\"double\" definition=\"spacing in x "
      "direction\">%g</x_spacing>\n", meta->general->x_pixel_size);
    fprintf(fp, "      <y_spacing type=\"double\" definition=\"spacing in y "
      "direction\">%g</y_spacing>\n", meta->general->y_pixel_size);
    fprintf(fp, "    </incidence_angle_map>\n");
    meta_free(meta);

    // Terrain correction processing parameters
    fprintf(fp, "    <terrain_correction>\n");
    fprintf(fp, "      <gamma_version type=\"string\" definition=\"version "
      "number of the GAMMA software used for processing\">%s</gamma_version>\n",
      params->gamma_version);
    if (params->gap_rtc)
      fprintf(fp, "      <gap_rtc_version type=\"string\" definition=\"version "
        "number of gap_rtc script used for terrain correction processing\">%s"
        "</gap_rtc_version>\n", params->gap_rtc);
    if (params->hyp3_rtc)
      fprintf(fp, "      <hyp3_rtc_version type=\"string\" definition=\"version "
        "number of hyp3_rtc script used for terrain correction processing\">%s"
        "</hyp3_rtc_version>\n", params->hyp3_rtc);
    fprintf(fp, "      <patches_attempted type=\"int\" definition=\"number of patches used to coregister\""
      ">%d</patches_attempted>\n", patches_attempted);
    fprintf(fp, "      <patches_accepted type=\"int\" definition=\"number of patches successfully coregistered\""
      ">%d</patches_accepted>\n", patches_accepted);
    fprintf(fp, "      <coregistration_success type=\"string\" definition=\"Coregistration "
      "success flag\">%s</coregistration_success>\n", passed ? "Y" : "N");
    fprintf(fp, "      <offset_x type=\"double\" definition=\"Coregistration range offset\""
      " units=\"pixels\">%g</offset_x>\n", range_offset);
    fprintf(fp, "      <offset_y type=\"double\" definition=\"Coregistration azimuth offset\""
      " units=\"pixels\">%g</offset_y>\n", azimuth_offset);
    fprintf(fp, "      <residual_range_offset_stddev type=\"double\" definition=\"final model fit std. dev. range\""
      " units=\"pixels\">%g</residual_range_offset_stddev>\n", range_stddev);
    fprintf(fp, "      <residual_azimuth_offset_stddev type=\"double\" definition=\"final model fit std. dev. azimuth\""
      " units=\"pixels\">%g</residual_azimuth_offset_stddev>\n", azimuth_stddev);
    fprintf(fp, "    </terrain_correction>\n");
    fprintf(fp, "  </metadata>\n");

    // Browse image information
    if (browse) {
      fprintf(fp, "  <browse>\n");
      if (params->browse_amplitude && fileExists(params->browse_amplitude)) {
        split_dir_and_file(params->browse_amplitude, directory, filename);
        fprintf(fp, "    <amplitude>%s</amplitude>\n", filename);
      }
      if (params->kml_overlay && fileExists(params->kml_overlay)) {
        split_dir_and_file(params->kml_overlay, directory, filename);
        fprintf(fp, "    <kml_overlay>%s</kml_overlay>\n", filename);
      }
      fprintf(fp, "  </browse>\n");
    }

    // Data extent
    double plat_min, plat_max, plon_min, plon_max;
    fprintf(fp, "  <extent>\n");
    if (params->rtc_HH_metadata)
      meta = meta_read(params->rtc_HH_metadata);
    else if (params->rtc_VV_metadata)
      meta = meta_read(params->rtc_VV_metadata);
    else
      asfPrintError("Could not find metadata for determining extent\n");
    meta_get_bounding_box(meta, &plat_min, &plat_max, &plon_min, &plon_max);
    fprintf(fp, "    <terrain_corrected_image>\n");
    fprintf(fp, "      <westBoundLongitude>%.5f</westBoundLongitude>\n",
      plon_min);
    fprintf(fp, "      <eastBoundLongitude>%.5f</eastBoundLongitude>\n",
      plon_max);
    fprintf(fp, "      <northBoundLatitude>%.5f</northBoundLatitude>\n",
      plat_max);
    fprintf(fp, "      <southBoundLatitude>%.5f</southBoundLatitude>\n",
      plat_min);
    fprintf(fp, "    </terrain_corrected_image>\n");
    meta_free(meta);
    dem_meta = 
      gamma_dem2meta(params->dem_file, params->dem_metadata);
    meta_get_bounding_box(dem_meta, &plat_min, &plat_max, &plon_min, &plon_max);
    fprintf(fp, "    <digital_elevation_model>\n");
    fprintf(fp, "      <westBoundLongitude>%.5f</westBoundLongitude>\n",
      plon_min);
    fprintf(fp, "      <eastBoundLongitude>%.5f</eastBoundLongitude>\n",
      plon_max);
    fprintf(fp, "      <northBoundLatitude>%.5f</northBoundLatitude>\n",
      plat_max);
    fprintf(fp, "      <southBoundLatitude>%.5f</southBoundLatitude>\n",
      plat_min);
    fprintf(fp, "    </digital_elevation_model>\n");
    fprintf(fp, "  </extent>\n");

    // Statistics
    if (params->rtc_HH_metadata) {
      meta = meta_read(params->rtc_HH_metadata);
      if (meta->stats)
        stats = TRUE;
      meta_free(meta);
    }
    else if (params->rtc_VV_metadata) {
      meta = meta_read(params->rtc_VV_metadata);
      if (meta->stats)
        stats = TRUE;
      meta_free(meta);
    }
    if (params->dem_file || params->layover_shadow_stats)
      stats = TRUE;    
    if (stats) {
      long long pixel_count = 0;
      fprintf(fp, "  <statistics>\n");
      if (params->rtc_HH_metadata) {
        meta = meta_read(params->rtc_HH_metadata);
        if (meta->stats) {
          fprintf(fp, "    <terrain_corrected_HH>\n");
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
          fprintf(fp, "    </terrain_corrected_HH>\n");
        }
        pixel_count = meta->general->sample_count * meta->general->line_count;
        meta_free(meta);
      }
      if (params->rtc_HV_metadata) {
        meta = meta_read(params->rtc_HV_metadata);
        if (meta->stats) {
          fprintf(fp, "    <terrain_corrected_HV>\n");
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
          fprintf(fp, "    </terrain_corrected_HV>\n");
        }
        meta_free(meta);
      }
      if (params->rtc_VH_metadata) {
        meta = meta_read(params->rtc_VH_metadata);
        if (meta->stats) {
          fprintf(fp, "    <terrain_corrected_VH>\n");
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
          fprintf(fp, "    </terrain_corrected_VH>\n");
        }
        meta_free(meta);
      }
      if (params->rtc_VV_metadata) {
        meta = meta_read(params->rtc_VV_metadata);
        if (meta->stats) {
          fprintf(fp, "    <terrain_corrected_VV>\n");
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
          fprintf(fp, "    </terrain_corrected_VV>\n");
        }
        meta_free(meta);
      }
      if (params->dem_file) {
        long long pixel_count = 
          dem_meta->general->line_count * dem_meta->general->sample_count;
        float *data = (float *) MALLOC(sizeof(float)*pixel_count);
        FILE *fpDEM = FOPEN(params->dem_file, "rb");
        ASF_FREAD(data, sizeof(float), pixel_count, fpDEM);
        FCLOSE(fpDEM);
        long long ii;
        for (ii=0; ii<pixel_count; ii++)
          ieee_big32(data[ii]);
        double min, max, mean, stdDev, percentValid;
        calc_stats_ext(data, pixel_count, dem_meta->general->no_data, FALSE,
          &min, &max, &mean, &stdDev, &percentValid);
        fprintf(fp, "    <digital_elevation_model>\n");
        fprintf(fp, "      <minimum_value>%.11g</minimum_value>\n", min);
        fprintf(fp, "      <maximum_value>%.11g</maximum_value>\n", max);
        fprintf(fp, "      <mean_value>%.11g</mean_value>\n", mean);
        fprintf(fp, "      <standard_deviation>%.11g</standard_deviation>\n",
          stdDev);
        fprintf(fp, "      <percent_valid_values>%.3f</percent_valid_values>\n",
          percentValid);
        fprintf(fp, "    </digital_elevation_model>\n");
      }
      if (params->layover_shadow_stats) {
        long long h[32];
        FILE *fpStats = FOPEN(params->layover_shadow_stats, "r");
        while (NULL != fgets(line, 255, fpStats)) {
          if (strstr(line, "  0-  7:"))
            sscanf(line+8, "%lld %lld %lld %lld %lld %lld %lld %lld", 
              &h[0], &h[1], &h[2], &h[3], &h[4], &h[5], &h[6], &h[7]);
          if (strstr(line, "  8- 15:"))
            sscanf(line+8, "%lld %lld %lld %lld %lld %lld %lld %lld", 
              &h[8], &h[9], &h[10], &h[11], &h[12], &h[13], &h[14], &h[15]);
          if (strstr(line, " 16- 23:"))
            sscanf(line+8, "%lld %lld %lld %lld %lld %lld %lld %lld", 
              &h[16], &h[17], &h[18], &h[19], &h[20], &h[21], &h[22], &h[23]);
          if (strstr(line, " 24- 31:"))
            sscanf(line+8, "%lld %lld %lld %lld %lld %lld %lld %lld", 
              &h[24], &h[25], &h[26], &h[27], &h[28], &h[29], &h[30], &h[31]);
        }
        FCLOSE(fpStats);
        if (params->rtc_HH_metadata)
          meta = meta_read(params->rtc_HH_metadata);
        else if (params->rtc_VV_metadata)
          meta = meta_read(params->rtc_VV_metadata);
        else
          asfPrintError("Could not find metadata for layover statistics\n");
        float valid_values = (pixel_count - h[0]) / (float)pixel_count;
        pixel_count = meta->general->line_count * meta->general->sample_count;
        pixel_count -= h[0];
        meta_free(meta);
        float no_layover_shadow = 0;
        float true_layover = 0, layover = 0, true_shadow = 0, shadow = 0;
        int ii;
        for (ii=0; ii<32; ii++) {
          if (ii & 1)
            no_layover_shadow += h[ii] / (float)pixel_count;
          if (ii & 2)
            true_layover += h[ii] / (float)pixel_count;
          if (ii & 4)
            layover += h[ii] / (float)pixel_count;
          if (ii & 8)
            true_shadow += h[ii] / (float)pixel_count;
          if (ii & 16)
            shadow += h[ii] / (float)pixel_count;
        }
        fprintf(fp, "    <layover_shadow_mask>\n");
        fprintf(fp, "      <percent_no_layover_shadow>%.1lf"
          "</percent_no_layover_shadow>\n", no_layover_shadow*100.0);
        fprintf(fp, "      <percent_true_layover>%.1lf"
          "</percent_true_layover>\n", true_layover*100.0);
        fprintf(fp, "      <percent_layover>%.1lf</percent_layover>\n", 
          layover*100.0);
        fprintf(fp, "      <percent_true_shadow>%.1lf</percent_true_shadow>\n", 
          true_shadow*100.0);
        fprintf(fp, "      <percent_shadow>%.1lf</percent_shadow>\n", 
          shadow*100.0);
        fprintf(fp, "      <percent_valid_values>%.3f</percent_valid_values>\n",
          valid_values*100.0);
        fprintf(fp, "    </layover_shadow_mask>\n");
      }
      if (params->incidence_angle_meta) {
        meta = meta_read(params->incidence_angle_meta);
        if (meta->stats) {
          fprintf(fp, "    <incidence_angle_map>\n");
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
          fprintf(fp, "    </incidence_angle_map>\n");
        }
      }
      fprintf(fp, "  </statistics>\n");
      meta_free(dem_meta);
    }

    // Working the various log files
    fprintf(fp, "  <processing>\n");

    // Initial processing
    /*
    fpFiles = FOPEN(params->processing_log, "rt");
    while (NULL != fgets(line, 255, fpFiles)) {
      if (strstr(line, "PROCESSING START:")) {
        p = strstr(line, "PROCESSING START:");
        gamma2iso_date(p+18, str);
        fprintf(fp, "    <data_ingest>%s</data_ingest>\n", str);
      }
    }
    FCLOSE(fpFiles);
    */

    // Terrain correction
    char *p;
    fpFiles = FOPEN(params->mk_geo_radcal_0_log, "r");
    while (NULL != fgets(line, 255, fpFiles)) {
      if (strstr(line, "mk_geo_radcal") && strstr(line, "processing start:") &&
          strstr(line, "mode: 0")) {
        p = strstr(line, "processing start:");
        gamma2iso_date(p+18, str);
        fprintf(fp, "    <data_ingest>%s</data_ingest>\n", str);
        fprintf(fp, "    <simulate_sar>%s</simulate_sar>\n", str);
      }
    }
    FCLOSE(fpFiles);
    if (fileExists(params->mk_geo_radcal_1_log)) {
      fpFiles = FOPEN(params->mk_geo_radcal_1_log, "r");
      while (NULL != fgets(line, 255, fpFiles)) {
        if (strstr(line, "mk_geo_radcal") && strstr(line, "processing start:") &&
            strstr(line, "mode: 1")) {
          p = strstr(line, "processing start:");
          gamma2iso_date(p+18, str);
          fprintf(fp, "    <initial_offset>%s</initial_offset>\n", str);
        }
      }
      FCLOSE(fpFiles);
    }
    if (fileExists(params->mk_geo_radcal_2_log)) {
      fpFiles = FOPEN(params->mk_geo_radcal_2_log, "r");
      while (NULL != fgets(line, 255, fpFiles)) {
        if (strstr(line, "mk_geo_radcal") && strstr(line, "processing start:") &&
            strstr(line, "mode: 2")) {
          p = strstr(line, "processing start:");
          gamma2iso_date(p+18, str);
          fprintf(fp, "    <refined_offset>%s</refined_offset>\n", str);
        }
      }
      FCLOSE(fpFiles);
    }
    fpFiles = FOPEN(params->mk_geo_radcal_3_log, "r");
    while (NULL != fgets(line, 255, fpFiles)) {
      if (strstr(line, "mk_geo_radcal") && strstr(line, "processing start:") &&
          strstr(line, "mode: 3")) {
        p = strstr(line, "processing start:");
        gamma2iso_date(p+18, str);
        fprintf(fp, "    <terrain_correction>%s</terrain_correction>\n", str);
      }
    }
    FCLOSE(fpFiles);

    fprintf(fp, "  </processing>\n");
    
    // Root metadata
    char data_source[25], processing[100], copyright[50];
    if (strcmp_case(platform, "ALOS") == 0) {
      sprintf(data_source, "ALOS PALSAR");
      sprintf(copyright, "JAXA, METI (%d)", year);
    }
    else if (strncmp_case(platform, "ERS", 3) == 0) {
      sprintf(data_source, "%s SAR", platform);
      sprintf(copyright, "ESA (%d)", year);
    }
    else if (strcmp_case(platform, "Sentinel-1A") == 0) {
      sprintf(data_source, "Copernicus Sentinel-1A");
      sprintf(copyright, "ESA (%d)", year);
    }
    else if (strcmp_case(platform, "Sentinel-1B") == 0) {
      sprintf(data_source, "Copernicus Sentinel-1B");
      sprintf(copyright, "ESA (%d)", year);
    }
    else
      asfPrintError("Could not set data source or copyright for %s!\n", 
        platform);
    sprintf(processing, "Terrain corrected product, processed by GAMMA");
    if (params->gamma_version) {
      sprintf(str, " (%s)", params->gamma_version);
      strcat(processing, str);
    }
    if (params->dem_source) {
      sprintf(str, " and corrected with a %s DEM", params->dem_source);
      strcat(processing, str);
    }

    fprintf(fp, "  <root>\n");
    fprintf(fp, "    <institution>Alaska Satellite Facility</institution>\n");
    fprintf(fp, "    <title>%s</title>\n", processing);
    fprintf(fp, "    <source>Single look complex %s data</source>\n", 
      data_source);
    fprintf(fp, "    <original_file>%s</original_file>\n", original_file);
    fprintf(fp, "    <comment>Copyright %s</comment>\n", copyright);
    fprintf(fp, "    <reference>Documentation available at: www.asf.alaska.edu"
      "</reference>\n");
    fprintf(fp, "    <history>%s: %s file created.</history>\n", creation,
      params->format);
    fprintf(fp, "  </root>\n");
    fprintf(fp, "</hdf5>\n");
    FCLOSE(fp);
  }
  else if (strcmp_case(type, "SMAP") == 0) {

    // check data entries in parameter list
    int found_data = FALSE;
    if (params->latitude || params->longitude || params->sigma0_hh_fore)
      found_data = TRUE;

    // update output XML file
    FILE *fpXml = FOPEN(params->xml, "r");
    fp = FOPEN(xml_name, "wt");
    while (fgets(line, 1024, fpXml) != NULL) {
      if (strstr(line, "<data/>")) {
        if (found_data) {
          fprintf(fp, "  <format>%s</format>\n", params->format);
          fprintf(fp, "  <data>\n");          
          if (params->latitude) 
            fprintf(fp, "    <latitude>%s</latitude>\n", params->latitude);
          if (params->longitude)
            fprintf(fp, "    <longitude>%s</longitude>\n", params->longitude);
          if (params->sigma0_hh_fore)
            fprintf(fp, "    <sigma0_hh_fore>%s</sigma0_hh_fore>\n", 
              params->sigma0_hh_fore);
          fprintf(fp, "  </data>\n");
        }
        else {
          meta = meta_read(params->metadata);
          int band_count = meta->general->band_count;
          char **bands = extract_band_names(meta->general->bands, band_count);
          fprintf(fp, "  <format>%s</format>\n", params->format);
          fprintf(fp, "  <data>\n");
          int ii;
          for (ii=0; ii<band_count; ii++) {
            fprintf(fp, "    <%s>%s:%s</%s>\n", 
              bands[ii], params->data, bands[ii], bands[ii]);
          }
          fprintf(fp, "  </data>\n");
          meta_free(meta);
        }
      }
      else if (strstr(line, "<projection/>")) {
        meta = meta_read(params->metadata);
        meta_projection *mp = meta->projection;
        if (mp && mp->type != SCANSAR_PROJECTION) {
          fprintf(fp, "  <projection>\n");
          if (mp->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
            fprintf(fp, "    <grid_mapping_name>transverse_mercator"
              "</grid_mapping_name>\n");
            fprintf(fp, "    <scale_factor_at_central_meridian>%.6f</scale_"
              "factor_at_central_meridian>\n", mp->param.utm.scale_factor);
            fprintf(fp, "    <longitude_of_central_meridian>%.1f</longitude_"
              "of_central_meridian>\n", mp->param.utm.lon0);
            fprintf(fp, "    <latitude_of_projection_origin>%.1f</latitude_"
              "of_projection_origin>\n", mp->param.utm.lat0);
            fprintf(fp, "    <false_easting>%.0f</false_easting>\n",
              mp->param.utm.false_easting);
            fprintf(fp, "    <false_northing>%.0f</false_northing>\n",
              mp->param.utm.false_northing);
          }
          else if (mp->type == POLAR_STEREOGRAPHIC) {
            double lon_pole;
            fprintf(fp, "    <grid_mapping_name>polar_stereographic"
              "</grid_mapping_name>\n");
            fprintf(fp, "    <straight_vertical_longitude_from_pole>%.1f"
              "</straight_vertical_longitude_from_pole>\n", mp->param.ps.slon);
            if (mp->param.ps.slat > 0)
              lon_pole = 90.0;
            else
              lon_pole = -90.0;
            fprintf(fp, "    <latitude_of_projection_origin>%.1f</latitude_"
              "of_projection_origin>\n", lon_pole);
            fprintf(fp, "    <standard_parallel>%.1f</standard_parallel>\n",
              mp->param.ps.slat);
            fprintf(fp, "    <false_easting>%.0f</false_easting>\n",
              mp->param.ps.false_easting);
            fprintf(fp, "    <false_northing>%.0f</false_northing>\n",
              mp->param.ps.false_northing);
          }
          else if (mp->type == ALBERS_EQUAL_AREA) {
            fprintf(fp, "    <grid_mapping_name>albers_conical_equal_area"
              "</grid_mapping_name>\n");
            fprintf(fp, "    <standard_parallel_1>%.1f</standard_parallel_1>\n",
              mp->param.albers.std_parallel1);
            fprintf(fp, "    <standard_parallel_2>%.1f</standard_parallel_2>\n",
              mp->param.albers.std_parallel2);
            fprintf(fp, "    <longitude_of_central_meridian>%.1f</longitude_of_"
              "central_meridian>\n", mp->param.albers.center_meridian);
            fprintf(fp, "    <latitude_of_projection_origin>%.1f</latitude_of_"
              "projection_origin>\n", mp->param.albers.orig_latitude);
            fprintf(fp, "    <false_easting>%.0f</false_easting>\n",
              mp->param.albers.false_easting);
            fprintf(fp, "    <false_northing>%.0f</false_northing>\n",
              mp->param.albers.false_northing);
          }
          else if (mp->type == LAMBERT_CONFORMAL_CONIC) {
            fprintf(fp, "    <grid_mapping_name>lambert_conformal_conic"
              "</grid_mapping_name>\n");
            fprintf(fp, "    <standard_parallel_1>%.1f</standard_parallel_1>\n",
              mp->param.lamcc.plat1);
            fprintf(fp, "    <standard_parallel_2>%.1f</standard_parallel_2>\n",
              mp->param.lamcc.plat2);
            fprintf(fp, "    <longitude_of_central_meridian>%.1f</longitude_of_"
              "central_meridian>\n", mp->param.lamcc.lon0);
            fprintf(fp, "    <latitude_of_projection_origin>%.1f</latitude_of_"
              "projection_origin>\n", mp->param.lamcc.lat0);
            fprintf(fp, "    <false_easting>%.0f</false_easting>\n",
              mp->param.lamcc.false_easting);
            fprintf(fp, "    <false_northing>%.0f</false_northing>\n",
              mp->param.lamcc.false_northing);
          }
          else if (mp->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
            fprintf(fp, "    <grid_mapping_name>lambert_azimuthal_equal_area"
              "</grid_mapping_name>\n");
            fprintf(fp, "    <longitude_of_projection_origin>%.1f</longitude_"
              "of_projection_origin>\n", mp->param.lamaz.center_lon);
            fprintf(fp, "    <latitude_of_projection_origin>%.1f</latitude_of_"
              "projection_origin>\n", mp->param.lamaz.center_lat);
            fprintf(fp, "    <false_easting>%.0f</false_easting>\n",
              mp->param.lamaz.false_easting);
            fprintf(fp, "    <false_northing>%.0f</false_northing>\n",
              mp->param.lamaz.false_northing);
          }
          fprintf(fp, "    <semi_major_axis>%.3f</semi_major_axis>\n",
            mp->re_major);
          fprintf(fp, "    <semi_minor_axis>%.3f</semi_minor_axis>\n",
            mp->re_minor);
          fprintf(fp, "  </projection>\n");
        }
        meta_free(meta);
      }
      else if (strstr(line, "<extent/>")) {
        double plat_min, plat_max, plon_min, plon_max;
        fprintf(fp, "  <extent>\n");
        meta = meta_read(params->metadata);
        meta_get_bounding_box(meta, &plat_min, &plat_max, &plon_min, &plon_max);
        fprintf(fp, "    <smap_data>\n");
        fprintf(fp, "      <westBoundLongitude>%.5f</westBoundLongitude>\n",
          plon_min);
        fprintf(fp, "      <eastBoundLongitude>%.5f</eastBoundLongitude>\n",
          plon_max);
        fprintf(fp, "      <northBoundLatitude>%.5f</northBoundLatitude>\n",
          plat_max);
        fprintf(fp, "      <southBoundLatitude>%.5f</southBoundLatitude>\n",
          plat_min);
        fprintf(fp, "    </smap_data>\n");
        fprintf(fp, "  </extent>\n");
        meta_free(meta);
      }
      else if (strstr(line, "<statistics/>")) {
        meta = meta_read(params->metadata);
        if (meta->stats) {
          int ii;
          fprintf(fp, "  <statistics>\n");
          for (ii=0; ii<meta->stats->band_count; ii++) {
            fprintf(fp, "    <%s>\n", meta->stats->band_stats[ii].band_id);
            fprintf(fp, "      <minimum_value>%.11g</minimum_value>\n",
              meta->stats->band_stats[ii].min);
            fprintf(fp, "      <maximum_value>%.11g</maximum_value>\n",
              meta->stats->band_stats[ii].max);
            fprintf(fp, "      <mean_value>%.11g</mean_value>\n",
              meta->stats->band_stats[ii].mean);
            fprintf(fp, "      <standard_deviation>%.11g</standard_deviation>\n",
              meta->stats->band_stats[ii].std_deviation);
            fprintf(fp, "      <percent_valid_values>%.3f</percent_valid_values>\n",
              meta->stats->band_stats[ii].percent_valid);
            fprintf(fp, "    </%s>\n", meta->stats->band_stats[ii].band_id);
          }
          fprintf(fp, "  </statistics>\n");
        }
        meta_free(meta);
      }
      else
        fprintf(fp, "%s", line);
    }
    FCLOSE(fp);
    FCLOSE(fpXml);
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

