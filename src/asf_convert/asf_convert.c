/*******************************************************************************
<documentation>
<name>
   asf_convert
</name>

<synopsis>
asf_convert [-format <format>] <in_base_name> <out_full_name> |
            -config <config_file> |
            -init_config <config_file>
</synopsis>

<description>
   This program ingests level one CEOS data, geocodes and resamples them
   (both optional) and exports them to a variety of output formats. It can
   handle input from a configuration file as well as directly from the command
   line. asf_convert is able to run in batch mode. Running the program with a
   configuration file allows for a more detailed processing flow, including
   resampling and geocoding as intermediate processing steps.
</description>

<input>
   The basic configuration file is defined as follows:

   asf_convert configuration file

   [General]
   input data file = < name of input data file (with extension) >
   input metadata file = < name of input metadata file (with extension) >
   input format = < CEOS | ASF >
   data type = < amplitude | power | sigma | gamma | beta >
   output file = < basename of the output file >
   output format = < ASF | GEOTIFF | JPEG | ENVI | ESRI | PPM >
   resampling = < flag for subsampling: 0 | 1 >
   browse image = < flag for subsampling to browse image size: 0 | 1 >
   geocoding = < flag for geocoding: 0 | 1 >
   batch mode = < flag for batch mode processing: 0 | 1 >
   batch file = < batch file name >
   log file = < log file name >

   For command line input see OPTIONS.
</input>

<output>
   For configuration file output see INPUT.
   For command line output see OPTIONS.
</output>

<options>
   -config        runs asf_convert from a configuration file.
   -init_config   creates only a configuration file and exits.
   -input         input format and input data and metadata files.
   -output        output format and output filename.
</options>

<examples>
   To convert the CEOS format file1 to a jpeg format in file2.jpg
      asf_convert -input CEOS file1.img file1.meta -output jpeg file2.jpg
   To run the program from a configuration file:
      asf_convert -config configFile
</examples>

<limitations>
   None known.
</limitations>

<see_also>
   asf_import, asf_export
</see_also>

<copyright>
*******************************************************************************
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
*******************************************************************************
</copyright>
</documentation>

PROGRAM HISTORY:

    VERS:   DATE:   AUTHOR:   PURPOSE:
    ----------------------------------------------------------------------
    0.1     10/02   R. Gens   Original development
    0.2     10/03   R. Gens   Extended output formats
    0.3     05/04   R. Gens   Overhaul for new metadata, added resampling
    0.4     05/04   R. Gens   Added batch mode
    0.5     06/04   R. Gens   Renamed to asf_convert, added alternative
                              command line

*******************************************************************************/

#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "functions.h"
#include "proj.h"
#include <unistd.h>

#define VERSION 0.5
#define REQUIRED_ARGS 1

void usage()
{
 printf("\n"
	"USAGE:\n"
	"   asf_convert [-format <format>] <in_base_name> <out_full_name> |\n"
	"               -config <config_file> |\n"
        "               -init_config <config_file>\n");
  exit(EXIT_FAILURE);
}

void check_return(int ret, char *msg)
{
  if (ret!=0) {
    sprintf(errbuf, "\n   ERROR: %s\n\n", msg);
    printErr(errbuf);
  }
}

int main(int argc, char *argv[])
{
  FILE *fBatch;
  s_config *cfg;
  char configFile[255], cmd[255], options[255], in[25], out[25];
  char proj[25], line[255], fileName[255], batchConfig[255];
  char format_in[255], format_out[255];
  char out_file[255], data_file[255], meta_file[255];
  int inFlag=FALSE, outFlag=FALSE, configFlag=FALSE, configInitFlag=FALSE;
  int id, ii;
  file_format_t type_in, type_out;


  /* Parse command line args */
  while (currArg < (argc-REQUIRED_ARGS)) {
    char *key=argv[currArg++];

    if (strmatch(key,"-config")) {
      CHECK_ARG(1);
      strcpy(configFile,GET_ARG(1));
      configFlag = TRUE;
      logflag = TRUE;
    }
    else if (strmatch(key,"-init_config")) {
      CHECK_ARG(1);
      strcpy(configFile,GET_ARG(1));
      configFlag = TRUE;
      configInitFlag = TRUE;
    }
    else if (strmatch(key,"-input")) {
      CHECK_ARG(3);
      strcpy(format_in,GET_ARG(3));
      strcpy(data_file,GET_ARG(2));
      strcpy(meta_file,GET_ARG(1));
      inFlag = TRUE;
    }
    else if (strmatch(key,"-output")) {
      CHECK_ARG(2);
      strcpy(format_out,GET_ARG(2));
      strcpy(out_file,GET_ARG(1));
      outFlag = TRUE;
    }
    else if (strmatch(key,"-log")) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      logflag = TRUE;
    }
  }

  if (!configFlag && (!inFlag && outFlag)) {
    check_return(1, "No input files defined!");
    exit(0);
  }
  else if (!configFlag && (inFlag && !outFlag)) {
    check_return(1, "No output file defined!");
    exit(0);
  }
  else if (argc<3)
    usage();

  quietflag = TRUE;

  sprintf(logbuf, "\nCommand line: asf_convert");
  for (ii=1; ii<argc; ii++) {
    sprintf(cmd, " %s",argv[ii]);
    strcat(logbuf, cmd);
  }
  sprintf(cmd, "\nDate: %s\nProgram: asf_convert\n\n", date_time_stamp());
  strcat(logbuf, cmd);
  printf(logbuf);

  /* Get process ID. */
  id = (int)getpid();

  /* Read configuration file */
  if (configFlag) {
    if (!fileExists(configFile)) {
      check_return(init_config(configFile),
		   "basic configuration file could not be initialized");
      exit(0);
    }
    else if (check_resample_flag(configFile) && configInitFlag) {
      check_return(init_resample_config(configFile),
		   "extended resampling configuration file "
		   "could not be initialized");
      exit(0);
    }
    else if (check_geocode_flag(configFile) && configInitFlag) {
      check_return(init_projection_config(configFile),
		   "extended geocoding configuration file "
		   "could not be initialized");
      exit(0);
    }
    else cfg = read_config(configFile);
    strcpy(format_in, uc(cfg->general->in_format));
    strcpy(format_out, uc(cfg->general->out_format));
    sprintf(logFile, "%s", cfg->general->logFile);
    if (strcmp(logFile, "")==0) sprintf(logFile, "tmp%d.log", id);
    fLog = FOPEN(logFile, "a");
    if (logflag)
      printLog(logbuf);
  }
  /* Get input from command line arguments */
  else {
    sprintf(configFile, "tmp%d.config", id);
    cfg = init_cfg();
    strcpy(cfg->comment, "asf_convert configuration file");
    strcpy(cfg->general->in_data_name, data_file);
    strcpy(cfg->general->in_meta_name, meta_file);
    strcpy(cfg->general->in_format, uc(format_in));
    strcpy(cfg->general->data_type, "amplitude");
    strcpy(cfg->general->out_name, out_file);
    strcpy(cfg->general->out_format, uc(format_out));
    cfg->general->resample = 0;
    cfg->general->browse = 0;
    cfg->general->batch = 0;
    strcpy(cfg->general->batchFile, "");
    if (!logflag)
      sprintf(logFile, "tmp%d.log", id);
    strcpy(cfg->general->logFile, logFile);
    check_return(write_config(configFile, cfg),
		 "writing a temporary configuration file");
  }

  /* Handle data types */
  if (strncmp(uc(format_in),"CEOS",4)==0) type_in = CEOS;
  else if (strncmp(uc(format_in),"ASF",3)==0) type_in = ASF;
  if (strncmp(uc(format_out),"ASF",3)==0) type_out = ASF;
  else if (strncmp(uc(format_out),"CEOS",4)==0) type_out = CEOS;
  else if (strncmp(uc(format_out),"GEOTIFF",3)==0) type_out = GEOTIFF;
  else if (strncmp(uc(format_out),"JPEG",3)==0) type_out = JPEG;
  else if (strncmp(uc(format_out),"ENVI",3)==0) type_out = ENVI;
  else if (strncmp(uc(format_out),"ESRI",3)==0) type_out = ESRI;
  else if (strncmp(uc(format_out),"PPM",3)==0) type_out = PPM;
  else if (strncmp(uc(format_out),"PNG",3)==0) type_out = PNG;
  else if (strncmp(uc(format_out),"LAS",3)==0) type_out = LAS;

  /* Batch mode processing */
  if (cfg->general->batch) {
    cfg->general->batch = 0;
    logflag = 0;
    fBatch = FOPEN(cfg->general->batchFile, "r");
    while (fgets(line, 255, fBatch) != NULL) {
      sscanf(line, "%s", fileName);
      sprintf(cfg->general->in_data_name, "%s.D", fileName);
      sprintf(cfg->general->in_meta_name, "%s.L", fileName);
      strcpy(cfg->general->out_name, fileName);
      sprintf(batchConfig, "%s.config", fileName);
      check_return(write_config(batchConfig, cfg),
		   "Could not write individual configuration file "
		   "for batch mode processing");
      check_return(asf_convert(batchConfig),
		   "Processing image in batch mode (asf_convert)");
    }
  }
  else {

    /* Prepare processing */
    if (!configFlag) {
      if (!fileExists(cfg->general->in_data_name))
	check_return(1, "input data file does not exist");
      if (!fileExists(cfg->general->in_meta_name))
	check_return(1, "input metadata file does not exist");
    }

    /* Ingest CEOS image */
    switch (type_in) {
    case CEOS:
      sprintf(in, "tmp%d", id);
      if (strncmp(cfg->general->data_type, "amplitude", 9)==0) {
	check_return(asf_import(cfg->general->in_data_name,
				cfg->general->in_meta_name, "-amplitude", in),
		     "Importing CEOS data (asf_import)");
	sprintf(out, "tmp%d_amp", id);
      }
      else if (strncmp(cfg->general->data_type, "power", 5)==0) {
	check_return(asf_import(cfg->general->in_data_name,
				cfg->general->in_meta_name, "-power", in),
		     "Importing CEOS data (asf_import)");
	sprintf(out, "tmp%d_power", id);
      }
      else if (strncmp(cfg->general->data_type, "sigma", 5)==0) {
	check_return(asf_import(cfg->general->in_data_name,
				cfg->general->in_meta_name, "-sigma", in),
		     "Importing CEOS data (asf_import)");
	sprintf(out, "tmp%d_sigma", id);
      }
      else if (strncmp(cfg->general->data_type, "gamma", 5)==0) {
	check_return(asf_import(cfg->general->in_data_name,
				cfg->general->in_meta_name, "-gamma", in),
		     "Importing CEOS data (asf_import)");
	sprintf(out, "tmp%d_gamma", id);
      }
      else if (strncmp(cfg->general->data_type, "beta", 4)==0) {
	check_return(asf_import(cfg->general->in_data_name,
				cfg->general->in_meta_name, "-beta", in),
		     "Importing CEOS data (asf_import)");
	sprintf(out, "tmp%d_beta", id);
      }
      sprintf(cmd, "cp %s.meta %s.meta", out, cfg->general->out_name);
      system(cmd);
      break;
    case ASF:
      strcpy(out, cfg->general->in_data_name);
      break;
    default: check_return(1, "Unsupported input format type!");
      break;
    }

    /* Determine the corner coordinates of the image */
    if (cfg->general->browse) {
      check_return(corner_coords(out),
		   "determining geographic coordinates of corner points "
		   "(corner_coords)");
      sprintf(cmd, "mv %s.corners %s.corners", out, cfg->general->out_name);
      system(cmd);
    }

    /* Resampling */
    if (cfg->general->resample || cfg->general->browse) {
      sprintf(in, "%s", out);
      sprintf(out, "tmp%d_small", id);
      if (cfg->general->browse)
	sprintf(options, "-browse");
      else
	sprintf(options, "-resample %d", cfg->resampling->kernel);
      check_return(filter(options, in, out),
		   "subsampling image (filter)");
    }

    /* Geocoding */
    if (cfg->general->geocoding) {

      /* Creating projection parameter file */
      sprintf(proj, "tmp%d.proj", id);

      /*** Polar Stereographic ***/
      if (strncmp(uc(cfg->geocoding->projection), "POLAR", 5)==0) {
	sprintf(options, "-l %lf -p %lf -g %s -d %d",
		cfg->polar->center_lon, cfg->polar->center_lat,
		cfg->polar->units, cfg->polar->datum);
	check_return(projprm("plstereo", "key", proj, options),
		     "generating projection parameter file (projprm)");
      }
      /*** Universal Transverse Mercator ***/
      if (strncmp(uc(cfg->geocoding->projection), "UTM", 3)==0) {
	sprintf(options, "-d %i -z %i", cfg->utm->datum, cfg->utm->zone);
	check_return(projprm("utm", "key", proj, options),
		     "generating projection parameter file (projprm)");
      }

      /*** Albers Conic Equal Area ***/
      if (strncmp(uc(cfg->geocoding->projection), "ALBERS", 6)==0) {
	sprintf(options, "-a %lf -b %lf -c %lf -o %lf -g %s -d %d",
		cfg->albers->first_parallel, cfg->albers->second_parallel,
		cfg->albers->center_meridian, cfg->albers->orig_latitude,
                cfg->albers->units, cfg->albers->datum);
	check_return(projprm("albers", "key", proj, options),
		     "generating projection parameter file (projprm)");
      }

      /*** Lambert Conformal Conic ***
	   if (strncmp(uc(cfg->geocoding->projection), "LAMBERT_CC", )==0) {
	   sprintf(options, "-x %lf -y %lf -g %s -d %d",
	   cfg->lambert1->latitude, cfg->lambert1->longitude,
	   cfg->lambert1->units, cfg->lambert1->datum);
	   check_return(projprm("lambert", "key", proj, options),
	   "generating projection parameter file (projprm)");
	   } ***/

      /*** Lambert Azimuthal Equal Area ***
	   if (strncmp(uc(cfg->geocoding->projection), "LAMBERT2", 8)==0) {
	   *** still needs to be figured out ***
	   } ***/

      sprintf(in, "%s", out);
      sprintf(out, "tmp%d_geo", id);
      check_return(geocode(in, proj, "key", cfg->geocoding->pixel,
			   cfg->geocoding->height, out),
		   "geocoding image (geocode)");
    }

    /* Exporting image */
    switch (type_out) {
    case CEOS: break;
    case ASF:
      sprintf(cmd, "mv %s.img %s.img", out, cfg->general->out_name);
      system(cmd);
      sprintf(cmd, "mv %s.meta %s.meta", out, cfg->general->out_name);
      break;
    case GEOTIFF:
      check_return(asf_export(type_out, out, cfg->general->out_name),
		   "exporting image to GEOTIFF format (asf_export)");
      break;
    case JPEG:
      check_return(asf_export(type_out, out, cfg->general->out_name),
		   "exporting image to JPEG format (asf_export)");
      break;
    case ENVI:
      check_return(asf_export(type_out, out, cfg->general->out_name),
		   "exporting image to ENVI format (asf_export)");
      break;
    case ESRI:
      check_return(asf_export(type_out, out, cfg->general->out_name),
		   "exporting image to ESRI format (asf_export)");
      break;
    case PPM:
      check_return(asf_export(type_out, out, cfg->general->out_name),
		   "exporting image to PPM format (asf_export)");
      break;
    case PNG:
      /*
      check_return(asf_export(type_out, out, cfg->general->out_name),
		   "exporting image to PNG format (asf_export)");
      */
      break;
    case LAS: break;
    default:
      check_return(1, "Unsupported output format!");
      break;
    }

    /* Remove temporary files */
    sprintf(cmd, "rm -f tmp%d*", id);
    system(cmd);

  }
  return(0);
}


