#include <asf_contact.h>
#include <asf_copyright.h>
#include <asf_license.h>
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
"asf_convert"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-create] [-log <logFile>] [-quiet] [-license]\n"\
"               [-version] [-help]\n"\
"               <config_file>\n"

#define ASF_DESCRIPTION_STRING \
"   This program can ingest level one CEOS data, geocode it, and export it to\n"\
"   a variety of imagery formats. The user is able to control how "ASF_NAME_STRING"\n"\
"   dictates the processiong flow by creating and/or editting a configuration\n"\
"   file which is fed into "ASF_NAME_STRING" when it is called.\n"

#define ASF_REQUIRED_ARGUMENTS_STRING \
"   config_file\n"\
"        A configuration file that "ASF_NAME_STRING" uses to find which files to\n"\
"        use for input and output, what options to use, and how the data should\n"\
"        be processed. It is either read or created based on whether or not\n"\
"        the -create option is specified.\n"

#define ASF_OPTIONS_STRING \
"   -create\n"\
"        Create <config_file> instead of reading it.\n"\
"   -log <logFile>\n"\
"        Set the name and location of the log file. Default behavior is to\n"\
"        log to tmp<processIDnumber>.log\n"\
"   -quiet\n"\
"        Suppresses all non-essential output.\n"\
"   -license\n"\
"        Print copyright and license for this software then exit.\n"\
"   -version\n"\
"        Print version and copyright then exit.\n"\
"   -help\n"\
"        Print a help page and exit.\n"

#define ASF_EXAMPLES_STRING \
"   To create an editable configuration file named 'convert.conf' with some\n"\
"   example values, do this:\n"\
"      example> "ASF_NAME_STRING" -create convert.conf\n"\
"\n"\
"   To process level 1 CEOS data using an "ASF_NAME_STRING" style configuration\n"\
"   file named 'config', do this:\n"\
"      example> "ASF_NAME_STRING" config\n"

#define ASF_LIMITATIONS_STRING \
"   None known.\n"

#define ASF_SEE_ALSO_STRING \
"   asf_import, asf_geocode, asf_export\n"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) "ASF_COPYRIGHT_YEAR_STRING", University of Alaska Fairbanks, Alaska Satellite Facility.\n"\
"All rights reserved.\n"

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/


#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "functions.h"
#include "proj.h"
#include "asf_reporting.h"
#include "asf_contact.h"
#include <unistd.h>


#define REQUIRED_ARGS 1
#define FLAG_NOT_SET -1


static int log_f;
// If the user didn't ask for a log file then we can nuke the one that
// we've been keeping since we've finished everything */
static int exit_with_success(void) {
  if ((log_f==FLAG_NOT_SET) && (logflag==TRUE)){
    fclose (fLog);
    remove(logFile);
  }
  exit(EXIT_SUCCESS);
}

// Print minimalistic usage info & exit
static void print_usage(void)
{
  asfPrintStatus("\n"
      "Usage:\n"
      ASF_USAGE_STRING
      "\n");
  exit(EXIT_FAILURE);
}

// Print the help info & exit
static void print_help(void)
{
  asfPrintStatus(
      "\n"
      "Tool name:\n   " ASF_NAME_STRING "\n\n"
      "Usage:\n" ASF_USAGE_STRING "\n"
      "Description:\n" ASF_DESCRIPTION_STRING "\n"
      "Required Arguments:\n" ASF_REQUIRED_ARGUMENTS_STRING "\n"
      "Options:\n" ASF_OPTIONS_STRING "\n"
      "Examples:\n" ASF_EXAMPLES_STRING "\n"
      "Limitations:\n" ASF_LIMITATIONS_STRING "\n"
      "See also:\n" ASF_SEE_ALSO_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " CONVERT_PACKAGE_VERSION_STRING "\n\n");
  exit(EXIT_SUCCESS);
}

// Print version and copyright & exit
static void print_version(void)
{
  asfPrintStatus(
    ASF_NAME_STRING", version "CONVERT_PACKAGE_VERSION_STRING"\n"
    ASF_COPYRIGHT_STRING);
  exit(EXIT_SUCCESS);
}

// Print our copyright and license notice & exit
static void print_license(int license_id)
{
  asfPrintStatus("\n"ASF_COPYRIGHT_STRING"\n");

  switch (license_id) {
    case ASF_BSD_ID:
      asfPrintStatus(ASF_BSD_LICENSE_STRING"\n");
      break;
    default:
      printf("License not found.\n");
      break;
  }
  exit(EXIT_SUCCESS);
}


/* Check to see if an option was supplied or not. If it was found, return its
   argument number. Otherwise, return FLAG_NOT_SET. STOLEN FROM ASF_IMPORT */
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


void check_return(int ret, char *msg)
{
  if (ret != 0)
    asfPrintError(msg);
}

char *uc(char *string)
{
  char *out=(char *)MALLOC(sizeof(char)*strlen(string));
  int i;

  for (i=0; i<strlen(string); i++) out[i]=toupper(string[i]);
  out[i]='\0';

  return out;
}


#define REQUIRED_ARGS 1

int main(int argc, char *argv[])
{
  FILE *fBatch, *fConfig;
  convert_config *cfg;
  char configFileName[255], cmd[1024], options[255], line[255], batchConfig[255];
  char inFile[255], outFile[255], fileName[255];
  char format[255], radiometry[255], projection[255], datum[255], resampling[255];
  char scale[255], values[255], prefix[30], suffix[30];
  const int pid = getpid();
  int createflag;
  extern int logflag, quietflag;
  int create_f, quiet_f;  /* log_f is a static global */

  createflag = logflag = quietflag = FALSE;
  create_f = log_f = quiet_f = FLAG_NOT_SET;

  // Begin command line parsing ***********************************************
  if (   (checkForOption("--help", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-h", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-help", argc, argv) != FLAG_NOT_SET) ) {
      print_help();
  }
  if ( checkForOption("-license", argc, argv) != FLAG_NOT_SET ) {
      print_license(ASF_BSD_ID);
  }
  if ( checkForOption("-version", argc, argv) != FLAG_NOT_SET ) {
      print_version();
  }

  // Check which options were provided
  create_f = checkForOption("-create", argc, argv);
  log_f    = checkForOption("-log", argc, argv);
  quiet_f  = checkForOption("-quiet", argc, argv);

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 1 + REQUIRED_ARGS;               // command & REQUIRED_ARGS
  if (create_f != FLAG_NOT_SET)  needed_args += 1; // option
  if (log_f    != FLAG_NOT_SET)  needed_args += 2; // option & param
  if (quiet_f  != FLAG_NOT_SET)  needed_args += 1; // option

  // Make sure we have the right number of args
  if(argc != needed_args) {
    print_usage();
  }

  // Make sure argument for each flag (that requires an arg) is not another
  // option flag & is not a required argument
  if (log_f != FLAG_NOT_SET) {
    if ( (argv[log_f+1][0]=='-') || (log_f>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }

  // Do the actual flagging & such for each flag
  if (create_f != FLAG_NOT_SET) {
    createflag = TRUE;
  }
  if (log_f != FLAG_NOT_SET) {
    strcpy(logFile, argv[log_f+1]);
  }
  else {
    // default behavior: log to tmp<pid>.log
    sprintf(logFile, "tmp%i.log", pid);
  }
  logflag = TRUE;
  fLog = FOPEN(logFile, "a");
  // Set old school quiet flag (for use in our libraries)
  quietflag = (quiet_f!=FLAG_NOT_SET) ? TRUE : FALSE;

  // Fetch required arguments
  strcpy(configFileName, argv[argc-1]);

  // Report the command line
  asfSplashScreen(argc, argv);

  // End command line parsing *************************************************

  // If requested, create a config file and exit (if the file does not exist),
  // otherwise read it
  if ( createflag==TRUE && !fileExists(configFileName) ) {
    init_config(configFileName);
    exit_with_success();
  }
  // Extend the configuration file if the file already exist
  else if ( createflag==TRUE && fileExists(configFileName) ) {
    cfg = read_config(configFileName);
    check_return(write_config(configFileName, cfg), "Could not update configuration file");
    asfPrintStatus("   Initialized complete configuration file\n\n");
    FCLOSE(fLog);
    exit(EXIT_SUCCESS);
  }
  else {
    cfg = read_config(configFileName);
  }

  // Batch mode processing
  if (strcmp(cfg->general->batchFile, "") != 0) {
    fBatch = FOPEN(cfg->general->batchFile, "r");
    while (fgets(line, 255, fBatch) != NULL)
    {
      sscanf(line, "%s", fileName);

      // Create temporary configuration file
      sprintf(batchConfig, "%s.config", fileName);
      fConfig = FOPEN(batchConfig, "w");
      fprintf(fConfig, "asf_convert temporary configuration file\n\n");
      fprintf(fConfig, "[General]\n");
      fprintf(fConfig, "default values = %s\n", cfg->general->defaults);
      fprintf(fConfig, "input file = %s\n", fileName);
      if (strcmp(cfg->general->prefix, "") == 0)
	strcpy(prefix, "");
      else
	sprintf(prefix, "%s_", cfg->general->prefix);
      if (strcmp(cfg->general->suffix, "") == 0)
	strcpy(suffix, "");
      else
	sprintf(suffix, "_%s", cfg->general->suffix);
      fprintf(fConfig, "output file = %s%s%s\n", prefix, fileName, suffix);
      FCLOSE(fConfig);

      // Run asf_convert for temporary configuration file
      asfPrintStatus("\nProcessing %s ...\n", fileName);
      check_return(asf_convert(batchConfig),
                   "Processing image in batch mode (asf_convert).\n");

      // Clean up
      sprintf(cmd, "rm -f %s", batchConfig);
      system(cmd);
    }
  }
  // Regular processing
  else {

    // Check whether everything in the [Import] block is reasonable
    if (cfg->general->import) {

      // Import format: ASF, CEOS or STF
      if (strncmp(uc(cfg->import->format), "ASF", 3) != 0 &&
          strncmp(uc(cfg->import->format), "CEOS", 4) != 0 &&
          strncmp(uc(cfg->import->format), "STF", 3) != 0) {
        asfPrintError("Chosen import format not supported\n");
      }

      // Radiometry
      if (strncmp(uc(cfg->import->radiometry), "AMPLITUDE_IMAGE", 15) != 0 &&
          strncmp(uc(cfg->import->radiometry), "POWER_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) != 0) {
        asfPrintError("Chosen radiometry not supported\n");
      }

      // Look up table file existence check
      if (strcmp(cfg->import->lut, "") != 0) {
        if (!fileExists(cfg->import->lut)) {
          asfPrintError("Look up table file does not exist\n");
        }
      }

      // Precision state vector file check can only be done
      // from within asf_import

      // Assign appropriate file name
      if (strncmp(uc(cfg->import->format), "ASF", 3) == 0) {
        create_name(inFile, cfg->general->in_name, ".img");
        cfg->general->import = 0; // does not need import
      }
      else {
        create_name(inFile, cfg->general->in_name, ".D");
      }

      // Data file existence check
      if (!fileExists(inFile))
        asfPrintError("Import data file does not exist\n");
    }

    // Check whether everything in the [Geocoding] block is reasonable
    if (cfg->general->geocoding) {

      // Projection file existence check
      if (!fileExists(cfg->geocoding->projection)) {
        asfPrintError("Projection parameter file '%s' does not exist\n",
		      cfg->geocoding->projection);
      }

      // Check for pixel size smaller than threshold ???

      // Datum
      if (strncmp(cfg->geocoding->datum, "WGS84", 5) != 0 &&
          strncmp(cfg->geocoding->datum, "NAD27", 5) != 0 &&
          strncmp(cfg->geocoding->datum, "NAD83", 5) != 0) {
        asfPrintError("Chosen datum not supported\n");
      }

      // Resampling
      if (strncmp(cfg->geocoding->resampling, "NEAREST NEIGHBOR", 16) != 0 &&
          strncmp(cfg->geocoding->resampling, "BILINEAR", 8) != 0 &&
          strncmp(cfg->geocoding->resampling, "BICUBIC", 7) != 0) {
        asfPrintError("Chosen resampling method not supported\n");
      }
    }

    // Check whether everything in the [Image stats] block is reasonable
    if (cfg->general->geocoding) {

      // Values
      if (strncmp(cfg->image_stats->values, "LOOK", 4) != 0 &&
          strncmp(cfg->image_stats->values, "INCIDENCE", 9) != 0 &&
          strncmp(cfg->image_stats->values, "RANGE", 5) != 0) {
        asfPrintError("Chosen values not supported\n");
      }
    }

    // Check whether everything in the [Export] block is reasonable
    if (cfg->general->export) {

      // Export format: ASF, TIFF, GEOTIFF, JPEG, PPM
      if (strncmp(uc(cfg->export->format), "ASF", 3) != 0 &&
          strncmp(uc(cfg->export->format), "TIFF", 4) != 0 &&
          strncmp(uc(cfg->export->format), "GEOTIFF", 7) != 0 &&
          strncmp(uc(cfg->export->format), "JPEG", 4) != 0 &&
          strncmp(uc(cfg->export->format), "PPM", 3) != 0) {
        asfPrintError("Chosen export format not supported\n");
      }

      // Unset export flag when export format is ASF
      if (strncmp(uc(cfg->export->format), "ASF", 3) == 0) {
        cfg->general->export = 0;
      }

      // Scaling method for floating point GeoTIFFs
      if (strncmp(uc(cfg->export->format), "GEOTIFF", 7) == 0) {
        if (strncmp(uc(cfg->export->byte), "SIGMA", 5) != 0 &&
            strncmp(uc(cfg->export->byte), "MINMAX", 6) != 0 &&
            strncmp(uc(cfg->export->byte), "TRUNCATE", 8) != 0 &&
            strncmp(uc(cfg->export->byte), "HISTOGRAM_EQUALIZE", 18) != 0) {
          asfPrintError("Chosen scaling method not supported\n");
        }
      }
    }

    if (!cfg->general->import && !cfg->general->geocoding && !cfg->general->export) {
      asfPrintError("Nothing to be done\n");
    }

    // Let's finally get to work
    sprintf(outFile, "%s", cfg->general->in_name);
    if (strcmp(cfg->general->out_name, "") == 0) {
      sprintf(cfg->general->out_name, "%s", cfg->general->in_name);
    }

    if (cfg->general->import) {

      // Radiometry
      if (strncmp(uc(cfg->import->radiometry), "AMPLITUDE_IMAGE", 15) == 0) {
        sprintf(radiometry, "-amplitude ");
      } else if (strncmp(uc(cfg->import->radiometry), "POWER_IMAGE", 11) == 0) {
        sprintf(radiometry, "-power ");
      } else if (strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) == 0) {
        sprintf(radiometry, "-sigma ");
      } else if (strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) == 0) {
        sprintf(radiometry, "-gamma ");
      } else if (strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) == 0) {
        sprintf(radiometry, "-beta ");
      }

      // Pass in command line
      if (cfg->general->image_stats || cfg->general->detect_cr ||
	  cfg->general->geocoding || cfg->general->export) {
        sprintf(outFile, "tmp%i_import", pid);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }
      check_return(asf_import(cfg->general->in_name, outFile, cfg->import->format,
                              radiometry, NULL, cfg->import->lat_begin,
                              cfg->import->lat_end),
                   "ingesting data file (asf_import)\n");
    }

    if (cfg->general->image_stats) {

      // Values for statistics
      if (strncmp(uc(cfg->image_stats->values), "LOOK", 4) == 0) {
        sprintf(values, "-look");
      } else if (strncmp(uc(cfg->image_stats->values), "INCIDENCE", 9) == 0) {
        sprintf(values, "-incidence");
      } else if (strncmp(uc(cfg->image_stats->values), "RANGE", 5) == 0) {
        sprintf(values, "-range");
      }

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      if (cfg->general->geocoding || cfg->general->export) {
        sprintf(outFile, "tmp%i_image_stats", pid);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }
      check_return(image_stats(inFile, outFile, values, cfg->image_stats->bins,
                               cfg->image_stats->interval),
                   "running statistics on data file (image_stats)\n");
    }

    if (cfg->general->detect_cr) {

      // Intermediate results
      if (cfg->general->intermediates) {
	cfg->detect_cr->chips = 1;
	cfg->detect_cr->text = 1;
      }

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      if (cfg->general->geocoding || cfg->general->export) {
	sprintf(outFile, "tmp%i_detect_cr", pid);
      }
      else {
	sprintf(outFile, "%s", cfg->general->out_name);
      }
      check_return(detect_cr(inFile, cfg->detect_cr->cr_location, outFile,
			     cfg->detect_cr->chips, cfg->detect_cr->text),
		   "detecting corner reflectors (detect_cr)\n");
    }

    if (cfg->general->geocoding) {

      // Projection
      sprintf(projection, "--read-proj-file %s", cfg->geocoding->projection);

      // Datum
      sprintf(datum, "--datum %s", uc(cfg->geocoding->datum));

      // Resampling method
      if (strncmp(uc(cfg->geocoding->resampling), "NEAREST_NEIGHBOR", 16) == 0) {
        sprintf(resampling, "--resample-method nearest_neighbor");
      }
      if (strncmp(uc(cfg->geocoding->resampling), "BILINEAR", 8) == 0) {
        sprintf(resampling, "--resample-method bilinear");
      }
      if (strncmp(uc(cfg->geocoding->resampling), "BICUBIC", 7) == 0) {
        sprintf(resampling, "--resample-method bicubic");
      }

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      if (cfg->general->export) {
        sprintf(outFile, "tmp%i_geocoding", pid);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }
      if (cfg->geocoding->force) {
        sprintf(options, "--height %.1lf --pixel-size %.2lf %s %s %s --force",
                cfg->geocoding->height, cfg->geocoding->pixel,
                projection, datum, resampling);
      }
      else {
        sprintf(options, "--height %.1lf --pixel-size %.2lf %s %s %s",
                cfg->geocoding->height, cfg->geocoding->pixel,
                projection, datum, resampling);
      }
      check_return(asf_geocode(options, inFile, outFile),
                   "geocoding data file (asf_geocode)\n");
      sprintf(cmd, "cp %s.meta %s.meta", outFile, cfg->general->in_name);
      system(cmd);
    }

    if (cfg->general->export) {

      // Format
      if (strncmp(uc(cfg->export->format), "TIFF", 4) == 0) {
        sprintf(format, "-format tiff");
      } else if (strncmp(uc(cfg->export->format), "GEOTIFF", 7) == 0) {
        sprintf(format, "-format geotiff");
      } else if (strncmp(uc(cfg->export->format), "JPEG", 4) == 0) {
        sprintf(format, "-format jpeg");
      } else if (strncmp(uc(cfg->export->format), "PPM", 3) == 0) {
        sprintf(format, "-format ppm");
      }

      // Byte scaling
      if (strncmp(uc(cfg->export->byte), "TRUNCATE", 8) == 0) {
        sprintf(scale, "-byte truncate");
      } else if (strncmp(uc(cfg->export->byte), "MINMAX", 6) == 0) {
        sprintf(scale, "-byte minmax");
      } else if (strncmp(uc(cfg->export->byte), "SIGMA", 5) == 0) {
        sprintf(scale, "-byte sigma");
      } else if (strncmp(uc(cfg->export->byte), "HISTOGRAM_EQUALIZE", 18) == 0) {
        sprintf(scale, "-byte histogram_equalize");
      }

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      sprintf(outFile, "%s", cfg->general->out_name);
      sprintf(options, "%s %s", format, scale);
      check_return(asf_export(options, inFile, outFile),
                   "exporting data file (asf_export)\n");
    }

    if (!cfg->general->intermediates) {
      sprintf(cmd, "rm -f tmp%i*", pid);
      system(cmd);
    }
  }

  exit_with_success();
  return 0; // just to quiet a compiler warning; will never get here
}
