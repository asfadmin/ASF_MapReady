/*==================BEGIN ASF AUTO-GENERATED DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to change the
following defines. For the short ones (like ASF_NAME_STRING) this is no big
deal. However, for some of the longer ones, such as ASF_COPYRIGHT_STRING, it
can be a daunting task to get all the newlines in correctly, etc. In order to
help you with this task, there is a tool, edit_man_header.pl. The tool *only*
works with this portion of the code, so fear not. It will scan in defines of
the format #define ASF_<something>_STRING between the two auto-generated
documentation markers, format them for a text editor, run that editor, allow
you to edit the text in a clean manner, and then automatically generate these
defines, formatted appropriately. The only warning is that any text between
those two markers and not part of one of those defines will not be preserved,
and that all of this auto-generated code will be at the top of the source
file. Save yourself the time and trouble, and use edit_man_header.pl. :)
*/

#define ASF_NAME_STRING \
"   asf_convert"

#define ASF_USAGE_STRING \
"[-format <output_format>] [-size <output_size>] [-log <logFile>]\n"\
"               [-quiet] [-help] <inBaseName> <outBaseName>"

#define ASF_DESCRIPTION_STRING \
"   This program ingests level one CEOS data and exports it to a variety of\n"\
"   output formats."

#define ASF_INPUT_STRING \
"   inBaseName\n"\
"        A CEOS data/metadata set."

#define ASF_OUTPUT_STRING \
"   outBaseName\n"\
"        The CEOS data in the specified format (If no format is specified,\n"\
"        it will default to geotiff)."

#define ASF_OPTIONS_STRING \
"   -size <size>\n"\
"        Scale image so that its largest dimension is, at most,\n"\
"        <size> pixels.  Not supported for geotiff. \n"\
"   -help\n"\
"        Displays the documentation for this tool\n"\
"   -format <format>\n"\
"        Specifies the output format. Must be one of the following:\n"\
"        jpeg, tiff, or geotiff. (Default is geotiff).\n"\
"   -log <logFile>\n"\
"        Set the name and location of the log file. Default behavior is to\n"\
"        log to tmp<processIDnumber>.log\n"\
"   -quiet\n"\
"        Suppresses all non-essential output to stdout"

#define ASF_EXAMPLES_STRING \
"   To convert CEOS format file1 to geotiff format file2.tif (this is the\n"\
"   default behavior), do this:\n"\
"      example> asf_convert file1 file2\n"\
"\n"\
"   To convert the CEOS format file1 to a jpeg format in file2.jpg\n"\
"      example> asf_convert -format jpeg file1 file2"

#define ASF_LIMITATIONS_STRING \
"   None known."

#define ASF_SEE_ALSO_STRING \
"   asf_import, asf_export"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks\n"\
"All rights reserved.\n"\
"\n"\
"Redistribution and use in source and binary forms, with or without\n"\
"modification, are permitted provided that the following conditions are met:\n"\
"\n"\
"    * Redistributions of source code must retain the above copyright notice,\n"\
"      this list of conditions and the following disclaimer.\n"\
"    * Redistributions in binary form must reproduce the above copyright\n"\
"      notice, this list of conditions and the following disclaimer in the\n"\
"      documentation and/or other materials provided with the distribution.\n"\
"    * Neither the name of the Geophysical Institute nor the names of its\n"\
"      contributors may be used to endorse or promote products derived from\n"\
"      this software without specific prior written permission.\n"\
"\n"\
"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\n"\
"AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"\
"IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n"\
"ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE\n"\
"LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"\
"CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF\n"\
"SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS\n"\
"INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN\n"\
"CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)\n"\
"ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n"\
"POSSIBILITY OF SUCH DAMAGE.\n"\
"\n"\
"       For more information contact us at:\n"\
"\n"\
"       Alaska Satellite Facility\n"\
"       Geophysical Institute\n"\
"       University of Alaska Fairbanks\n"\
"       P.O. Box 757320\n"\
"       Fairbanks, AK 99775-7320\n"\
"\n"\
"       http://www.asf.alaska.edu\n"\
"       uso@asf.alaska.edu"

#define ASF_PROGRAM_HISTORY_STRING \
"    VERS:   DATE:   AUTHOR:   PURPOSE:\n"\
"    ----------------------------------------------------------------------\n"\
"    0.1     10/02   R. Gens   Original development\n"\
"    0.2     10/03   R. Gens   Extended output formats\n"\
"    0.3     05/04   R. Gens   Overhaul for new metadata, added resampling\n"\
"    0.4     05/04   R. Gens   Added batch mode\n"\
"    0.5     06/04   R. Gens   Renamed to asf_convert, added alternative\n"\
"                              command line"

/*
#define ASF_VERSION_MAJOR_STRING \
"   0.30"

#define VERSION 0.3
*/

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/


#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "functions.h"
#include "proj.h"
#include "asf_reporting.h"
#include <unistd.h>


#define REQUIRED_ARGS 1
#define FLAG_NOT_SET -1


void usage()
{
  printf ("\n"
    "USAGE:\n"
    ASF_NAME_STRING
    " "
    ASF_USAGE_STRING
    "\n\n");
  exit (EXIT_FAILURE);
}

void config_usage()
{
  printf(
    "BATCH USAGE:\n"
    "   asf_convert -config <config_file> |\n"
    "               -init_config <config_file>\n");
}


void help_page()
{
  char happy_string[4066];
  char command[4096];

  /* What to print out for help */
  sprintf(happy_string,
          "\n\n\n"
          "Tool name:\n" ASF_NAME_STRING "\n\n\n"
          "Usage:\n" ASF_NAME_STRING " " ASF_USAGE_STRING "\n\n\n"
          "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
          "Input:\n" ASF_INPUT_STRING "\n\n\n"
          "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
          "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
          "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
          "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
          "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
          "Version:\n" CONVERT_PACKAGE_VERSION_STRING "\n\n\n"
          "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n");

  /* If we can, use less */
  sprintf (command, "echo '%s' | less --prompt='Type q to quit help, h for "
	   "help with help browser'", happy_string);
  if ( system(command) == 0 )
    exit (EXIT_SUCCESS);

  /* Hmmm, less didn't work cause we got here, try using more */
  sprintf (command,"echo '%s' | more", happy_string);
  if(system(command) == 0)
    exit(EXIT_SUCCESS);

  /* Okay, neither less or more work (obviously if we made it here),
   * just print the info straight to stdout and exit */
  printf(happy_string);
  exit(EXIT_SUCCESS);
}


void check_return(int ret, char *msg)
{
  if (ret != 0)
    asfPrintError(msg);
}

/*Check to see if an option was supplied or not
If it was found, return its argument number
Otherwise, return FLAG_NOT_SET*/
int checkForOption(char* key, int argc, char* argv[])
{
  int ii = 0;
  while(ii < argc)
  {
    if(strmatch(key, argv[ii]))
      return(ii);
    ++ii;
  }
  return(FLAG_NOT_SET);
}


void replace_backslashes(char *msg)
{
  char temp[256];
  int ii,jj;
  /*Replace all \'s in msg with \\ in temp*/
  for(ii = 0, jj = 0; ii < 256 && jj < 256; ++ii, ++jj)
  {
    if(msg[ii] == '\\')
    {
      temp[jj] = '\\';
      ++jj;
      temp[jj] = '\\';
    }
    else
    {
      temp[jj] = msg[ii];
    }
  }
  temp[jj] = '\0';
  /*Done replacing...now copy temp into msg*/
  for(ii = 0; ii < 256; ++ii)
  {
    msg[ii] = temp[ii];
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
  int flags[NUM_FLAGS];
  int size_out;
  int found;
  file_format_t type_in, type_out;

/**********************BEGIN COMMAND LINE PARSING STUFF**********************/
  /*Super-secret hidden options :)*/
  if(checkForOption("-batch", argc, argv) != FLAG_NOT_SET)
  {
    config_usage();
    exit(EXIT_SUCCESS);
  }

  /*Normal options*/
  if(checkForOption("-help", argc, argv) != -1 ||
    checkForOption("--help", argc, argv) != -1)
    help_page();
  flags[f_FORMAT] = checkForOption("-format", argc, argv);
  flags[f_SIZE] = checkForOption("-size", argc, argv);
  flags[f_CONFIG] = checkForOption("-config", argc, argv);
  flags[f_CONFIG_INIT] = checkForOption("-init_config", argc, argv);
  flags[f_LOG] = checkForOption("-log", argc, argv);
  flags[f_QUIET] = checkForOption("-quiet", argc, argv);

  /* Set the old school quietflag accoringly (for library use) */
  quietflag = (flags[f_QUIET]!=FLAG_NOT_SET) ? TRUE : FALSE;
  /* old school logflag must be FALSE until we've opened the log file */
  logflag = FALSE;

  if(flags[f_CONFIG] != FLAG_NOT_SET && flags[f_CONFIG_INIT] != FLAG_NOT_SET)/*One or the other is fine, but not both*/
    usage();/*This exits with a failure*/
  if((flags[f_CONFIG] != FLAG_NOT_SET || flags[f_CONFIG_INIT] != FLAG_NOT_SET) && /*These...*/
    (flags[f_FORMAT] != FLAG_NOT_SET || flags[f_SIZE] != FLAG_NOT_SET))/*...are mutually exclusive with these*/
    usage();/*This exits with a failure*/
  {
    /*So, at this point, we know our options don't conflict...now we need
    to know how many arguments we ought to have.*/
    int neededArgs = 1;/*command*/
    if(flags[f_CONFIG] != FLAG_NOT_SET || flags[f_CONFIG_INIT] != FLAG_NOT_SET)
      neededArgs += 2;/*option & parameter*/
    else
      neededArgs += 2;/*in_data_file, in_meta_file (no longer!), out_file*/
    if(flags[f_LOG] != FLAG_NOT_SET)
      neededArgs += 2;/*option & parameter*/
    if(flags[f_FORMAT] != FLAG_NOT_SET)
      neededArgs += 2;/*option & parameter*/
    if(flags[f_SIZE] != FLAG_NOT_SET)
      neededArgs += 2;/*option & parameter*/
    if(flags[f_QUIET] != FLAG_NOT_SET)
      neededArgs += 1;/*option*/

    /*make sure we've got enough arguments*/
    if(argc != neededArgs)
      usage();/*This exits with a failure*/
  }

  /*We also need to make sure the last three options are close to what we expect*/
  if(flags[f_CONFIG] != FLAG_NOT_SET || flags[f_CONFIG_INIT] != FLAG_NOT_SET)
  {
    if(argv[argc - 1][0] == '-')
      usage();
  }
  else
  {
    if(argv[argc - 1][0] == '-' || argv[argc - 2][0] == '-')
      usage();/*This exits with a failure*/
  }

  /*Next we're going to make sure the options are specified according to the usage
  That is, -option <parameter> -option <parameter> and so on...if an option requires
  a parameter, we need to make sure it's followed by a parameter! Also make sure
  an option's parameters don't bleed into the command's required arguments*/
  if(flags[f_LOG] != FLAG_NOT_SET)
  {
    if(flags[f_CONFIG] != FLAG_NOT_SET || flags[f_CONFIG_INIT] != FLAG_NOT_SET) {
      if(argv[flags[f_LOG] + 1][0] == '-' || flags[f_LOG] >= argc - 3)
        usage();
    }
    else {
      if(argv[flags[f_LOG] + 1][0] == '-' || flags[f_LOG] >= argc - 3)
        usage();/*This exits with a failure*/
    }
  }
  if(flags[f_LOG] != FLAG_NOT_SET)
    strcpy(logFile, argv[flags[f_LOG] + 1]);
  else
    sprintf(logFile, "tmp%i.log", (int)getpid());/*default behavior: log to tmp<pid>.log*/

  if(flags[f_FORMAT] != FLAG_NOT_SET)
    if(argv[flags[f_FORMAT] + 1][0] == '-' || flags[f_FORMAT] >= argc - 3)
      usage();/*This exits with a failure*/
  if(flags[f_SIZE] != FLAG_NOT_SET)
    if(argv[flags[f_SIZE] + 1][0] == '-' || flags[f_SIZE] >= argc - 3)
      usage();/*This exits with a failure*/

  if(flags[f_FORMAT] != FLAG_NOT_SET)
    strcpy(format_out, argv[flags[f_FORMAT] + 1]);
  else
    strcpy(format_out, "geotiff");/*default behavior is geotiff*/
  str2upper(format_out);
  if(flags[f_SIZE] != FLAG_NOT_SET)
    size_out = atol(argv[flags[f_SIZE] + 1]);

  if(flags[f_CONFIG] != FLAG_NOT_SET)
    strcpy(configFile, argv[flags[f_CONFIG] + 1]);
  else
    sprintf(configFile, "tmp%i.config", (int)getpid());/*default behavior: config to tmp<pid>.config*/

  if(flags[f_CONFIG_INIT] != FLAG_NOT_SET)
    strcpy(configFile, argv[flags[f_CONFIG_INIT] + 1]);

  /*Get files from command line if we're not doing a config file*/
  if(flags[f_CONFIG] == FLAG_NOT_SET && flags[f_CONFIG_INIT] == FLAG_NOT_SET)
  {
    strcpy(data_file, argv[argc - 2]);/*The first of the last two arguments*/
    /*strcpy(meta_file, argv[argc - 2]);*/ /*The second of the last three arguments*/
    strcpy(out_file, argv[argc - 1]);/*The second of the last two arguments*/
    /*Replace any \'s with \\'s*/
    replace_backslashes(data_file);
    /*replace_backslashes(meta_file);*/
    replace_backslashes(out_file);

    found = FALSE;
    if(strchr(data_file, '.') != NULL)/* If file has an extension, see if we know it*/
    {
      if(!strcmp(".D", strrchr(data_file, '.')))/*If the file ends in .D*/
      {                                         /*It must be a CEOS image*/
        strcpy(format_in, "CEOS");
        strcpy(meta_file, data_file);
        meta_file[ strlen(meta_file) - 1 ] = 'L';
        found = TRUE;
      }
      else if(!strcmp(".img", strrchr(data_file, '.')))/*If the file ends in .img*/
      {                                  /*It must be an ASF internal format file*/
              char *ext;
        strcpy(format_in, "ASF");
        strcpy(meta_file, data_file);
        ext = strrchr(meta_file, '.');
        strcpy(ext + 1, "meta");
        found = TRUE;
        }
    }

    if (!found)
    {
      /* see if <basename>.D exists */
      char file_test[255];
      strcpy(file_test, data_file);
      strcat(file_test, ".D");
      if (fileExists(file_test))
      {
          strcpy(format_in, "CEOS");
          strcpy(data_file, file_test);
          strcpy(meta_file, data_file);
          meta_file[ strlen(meta_file) - 1 ] = 'L';
      }
      else
      {
        /* try <basename>.img */
        strcpy(file_test, data_file);
        strcat(file_test, ".img");
        if (fileExists(file_test))
        {
          char *ext;
          strcpy(format_in, "ASF");
          strcpy(data_file, file_test);
          strcpy(meta_file, data_file);
          ext = strrchr(meta_file, '.');
          strcpy(ext + 1, "meta");
        }
        else
        { /* can't find the file! */
          snprintf(file_test, sizeof(file_test),"Couldn't find input file %s.D",
                   data_file);
          asfPrintError(file_test); /*Otherwise, we have no idea*/
        }
      }
    }
  }
  if(flags[f_CONFIG_INIT] != FLAG_NOT_SET)
  {
    if(fileExists(configFile))
      asfPrintError("config file already exits");
    else
      check_return(init_config(configFile),
                   "Basic configuration file could not be initialized.\n");
    exit(EXIT_SUCCESS);
  }

  /* If we're working from a config file, read configuration file */
  if (flags[f_CONFIG] != FLAG_NOT_SET)/*flags[f_CONFIG] has been set*/
  {
    /*Does the specific config file already exist? If not...*/
    if (!fileExists(configFile))
    {
      check_return(init_config(configFile),
                   "Basic configuration file could not be initialized.\n");
      exit(EXIT_FAILURE);
    }
    /*config init has been set*/
    else if (check_resample_flag(configFile) && (flags[f_CONFIG_INIT]!=FLAG_NOT_SET))
    {
      check_return(init_resample_config(configFile),
                   "Extended resampling configuration file could not be initialized.\n");
      exit(EXIT_FAILURE);
    }
    /*config init has been set*/
    else if (check_geocode_flag(configFile) && flags[f_CONFIG_INIT] != FLAG_NOT_SET)
    {
      check_return(init_projection_config(configFile),
                   "Extended geocoding configuration file could not be initialized.\n");
      exit(EXIT_FAILURE);
    }
    /*Nab stuff out of the config File*/
    cfg = read_config(configFile);
    strcpy(format_in, str2upper(cfg->general->in_format));
    strcpy(format_out, str2upper(cfg->general->out_format));
    if (strcmp(cfg->general->logFile, "") != 0) {
      flags[f_LOG] = (flags[f_LOG]==FLAG_NOT_SET)
                     ? !FLAG_NOT_SET : flags[f_LOG];
      strcpy(logFile, cfg->general->logFile);
    }
  }
  /* If there's not a config file, get input from command line arguments */
  else
  {
    cfg = init_cfg();
    strcpy(cfg->comment, "asf_convert temporary configuration file");
    strcpy(cfg->general->in_data_name, data_file);
    strcpy(cfg->general->in_meta_name, meta_file);
    strcpy(cfg->general->in_format, str2upper(format_in));
    strcpy(cfg->general->data_type, "amplitude");
    strcpy(cfg->general->out_name, out_file);
    strcpy(cfg->general->out_format, str2upper(format_out));
    cfg->general->resample = 0;
    cfg->general->browse = 0;
    cfg->general->batch = 0;
    strcpy(cfg->general->batchFile, "");
    strcpy(cfg->general->logFile, logFile);
    check_return(write_config(configFile, cfg),
                 "Writing a temporary configuration file.\n");
  }

  /* At long last we are sure that logFile has been specified open it & set
     old school logflag to true */
  fLog = fopen (logFile, "a");
  if ( fLog == NULL ) {
    /* Failed to open the log file, oh well.  Just set logflag to
       FALSE and go on.  */
    logflag = FALSE;		
  } else {
    logflag = TRUE;
  }

  /* We are FINALLY good enough at this point... log & quiet flags set */
  asfSplashScreen(argc, argv);/*display splash screen if not quiet*/


/***********************END COMMAND LINE PARSING STUFF***********************/

  /* Handle data types */
  /*Input types:*/
  if (strncmp(str2upper(format_in),"CEOS",4) == 0) type_in = CEOS;
  else if (strncmp(str2upper(format_in),"ASF",3) == 0) type_in = ASF;
  else asfPrintError("Unrecognized input format");
  /*Output types:*/
  if (strncmp(str2upper(format_out),"ASF",3) == 0) type_out = ASF;
  else if (strncmp(str2upper(format_out),"CEOS",4) == 0) type_out = CEOS;
  else if (strncmp(str2upper(format_out),"GEOTIFF",3) == 0) type_out = GEOTIFF;
  else if (strncmp(str2upper(format_out),"TIFF",3) == 0) type_out = TIFF;
  else if (strncmp(str2upper(format_out),"JPEG",4) == 0) type_out = JPEG;
  else if (strncmp(str2upper(format_out),"JPG",3) == 0) type_out = JPEG;
  else if (strncmp(str2upper(format_out),"ENVI",4) == 0) type_out = ENVI;
  else if (strncmp(str2upper(format_out),"ESRI",4) == 0) type_out = ESRI;
  else if (strncmp(str2upper(format_out),"PPM",3) == 0) type_out = PPM;
  else if (strncmp(str2upper(format_out),"PNG",3) == 0) type_out = PNG;
  else if (strncmp(str2upper(format_out),"LAS",3) == 0) type_out = LAS;
  else asfPrintError("Unrecognized output format");

  /* Batch mode processing */
  if (cfg->general->batch != 0)
  {
    fBatch = FOPEN(cfg->general->batchFile, "r");
    while (fgets(line, 255, fBatch) != NULL)
    {
      sscanf(line, "%s", fileName);
      sprintf(cfg->general->in_data_name, "%s.D", fileName);
      sprintf(cfg->general->in_meta_name, "%s.L", fileName);
      strcpy(cfg->general->out_name, fileName);
      sprintf(batchConfig, "%s.config", fileName);
      check_return(write_config(batchConfig, cfg),
                   "Could not write individual configuration file for batch mode processing.\n");
      check_return(asf_convert(batchConfig),
                   "Processing image in batch mode (asf_convert).\n");
    }
  }
  else
  {
    char command[255];
    char temp[255];
    char asf_internal_basename[255];

    /* Prepare processing */
    if (flags[f_CONFIG] == FLAG_NOT_SET)
    {
      if (!fileExists(cfg->general->in_data_name))
        asfPrintError("Input data file does not exist");
      if (!fileExists(cfg->general->in_meta_name))
        asfPrintError("Input metadata file does not exist");
    }


    /* skip asf_import if given an img file */
    if (strcmp(str2upper(format_in), "ASF"))
    {
      sprintf(asf_internal_basename, "tmp%i", (int)getpid());
      sprintf(command, "asf_import -log %s -format %s", logFile, format_in);
      if(flags[f_QUIET] != FLAG_NOT_SET)
        strcat(command, " -quiet");
      sprintf(temp, " -%s %s %s",
        cfg->general->data_type,
        cfg->general->in_data_name,
        /* cfg->general->in_meta_name, */
        asf_internal_basename);
      strcat(command, temp);
      check_return(system(command), "Importing data (asf_import).\n");
    }
    else
    {
      char *p;
      strcpy(asf_internal_basename, cfg->general->in_data_name);
      p = strrchr(asf_internal_basename, '.');
      if (strcmp(p, ".img") == 0)
        *p = '\0';
    }

    /* Determine the corner coordinates of the image */
    if (cfg->general->browse)
    {
      check_return(corner_coords(out),
        "Determining geographic coordinates of corner points (corner_coords).\n");
      sprintf(cmd, "mv %s.corners %s.corners", out, cfg->general->out_name);
      system(cmd);
    }

    /* Resampling */
    if (cfg->general->resample || cfg->general->browse)
    {
      sprintf(in, "%s", out);
      sprintf(out, "tmp%i_small", (int)getpid());
      if (cfg->general->browse)
        sprintf(options, "-browse");
      else
        sprintf(options, "-resample %d", cfg->resampling->kernel);
      check_return(filter(options, in, out), "Subsampling image (filter)\n.");
    }

    /* Geocoding */
    if (cfg->general->geocoding) {

      /* Creating projection parameter file */
      sprintf(proj, "tmp%i.proj", (int)getpid());

      /*** Polar Stereographic ***/
      if (strncmp(str2upper(cfg->geocoding->projection), "POLAR", 5) == 0)
      {
        sprintf(options, "-l %lf -p %lf -g %s -d %d",
                cfg->polar->center_lon, cfg->polar->center_lat,
                cfg->polar->units, cfg->polar->datum);
        check_return(projprm("plstereo", "key", proj, options),
                     "Generating projection parameter file (projprm).\n");
      }
      /*** Universal Transverse Mercator ***/
      if (strncmp(str2upper(cfg->geocoding->projection), "UTM", 3) == 0)
      {
        sprintf(options, "-d %i -z %i", cfg->utm->datum, cfg->utm->zone);
        check_return(projprm("utm", "key", proj, options),
                     "Generating projection parameter file (projprm).\n");
      }

      /*** Albers Conic Equal Area ***/
      if (strncmp(str2upper(cfg->geocoding->projection), "ALBERS", 6) == 0)
      {
        sprintf(options, "-a %lf -b %lf -c %lf -o %lf -g %s -d %d",
                cfg->albers->first_parallel, cfg->albers->second_parallel,
                cfg->albers->center_meridian, cfg->albers->orig_latitude,
                cfg->albers->units, cfg->albers->datum);
        check_return(projprm("albers", "key", proj, options),
                     "Generating projection parameter file (projprm).\n");
      }

      /*** Lambert Conformal Conic ***
      if (strncmp(uc(cfg->geocoding->projection), "LAMBERT_CC", ) == 0)
      {
        sprintf(options, "-x %lf -y %lf -g %s -d %d",
                cfg->lambert1->latitude, cfg->lambert1->longitude,
                cfg->lambert1->units, cfg->lambert1->datum);
        check_return(projprm("lambert", "key", proj, options),
                     "Generating projection parameter file (projprm).\n");
      } ***/

      /*** Lambert Azimuthal Equal Area ***
      if (strncmp(uc(cfg->geocoding->projection), "LAMBERT2", 8) == 0)
      {
      *** still needs to be figured out ***
      } ***/

      sprintf(in, "%s", out);
      sprintf(out, "tmp%i_geo", (int)getpid());
      check_return(geocode(in, proj, "key", cfg->geocoding->pixel,
                   cfg->geocoding->height, out),
                   "Geocoding image (geocode).\n");
    }


    sprintf(command, "asf_export -log %s", logFile);
    if(flags[f_QUIET] != FLAG_NOT_SET)
      strcat(command, " -quiet");
    if(flags[f_SIZE] != FLAG_NOT_SET)
    {
      sprintf(temp, " -size %i", size_out);
      strcat(command, temp);
    }
    sprintf(temp, " -format %s %s %s",
    format_out,
    asf_internal_basename,
    cfg->general->out_name);
    strcat(command, temp);
    check_return(system(command), "Exporting data (asf_export).\n");
  }

  {
    char temp[256];
    /* Remove temporary files */
    if(flags[f_LOG] == FLAG_NOT_SET)
      remove(logFile);
    sprintf(temp, "tmp%i.config", (int)getpid());
    remove(temp);
    sprintf(temp, "tmp%i.img", (int)getpid());
    remove(temp);
    sprintf(temp, "tmp%i.meta", (int)getpid());
    remove(temp);
    sprintf(temp, "tmp%i_small", (int)getpid());
    remove(temp);
    sprintf(temp, "tmp%i.proj", (int)getpid());
    remove(temp);
    sprintf(temp, "tmp%i_geo", (int)getpid());
    remove(temp);
  }
  return(0);
}
