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
"[ -config ] <configuration file>"

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
"   asf_import, asf_geocode, asf_export"

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

char *uc(char *string)
{
  char *out=(char *)MALLOC(sizeof(char)*strlen(string));
  int i;
  
  for (i=0; i<strlen(string); i++) out[i]=toupper(string[i]);
  out[i]='\0';
  
  return out;
}


int main(int argc, char *argv[])
{
  FILE *fBatch, *fConfig;
  convert_config *cfg;
  char configFile[255], cmd[1024], options[255], line[255], batchConfig[255];
  char inFile[255], outFile[255], fileName[255];
  char format[255], radiometry[255], projection[255], datum[255], resampling[255];
  char scale[255];
  int cFlag=0, pid;

  if (argc<2 || argc>3) usage();
  if (strcmp(argv[1], "-config")==0) {
    cFlag = 1;
    strcpy(configFile, argv[2]);
  }
  else 
    strcpy(configFile, argv[1]);
  
  //Does the specific config file already exist? If not...
  if (!fileExists(configFile)) {
    check_return(init_config(configFile),
		 "Configuration file could not be initialized.\n");
    exit(EXIT_FAILURE);
  }
  else 
    cfg = read_config(configFile, cFlag);

  // Print status
  logflag = 1;
  pid = (int) getpid();
  sprintf(logFile, "%s.log", cfg->general->in_name);
  fLog = FOPEN(logFile, "w");
  if (argc == 3) 
    asfPrintStatus("\nCommand line: asf_convert -config %s\n", configFile);
  else 
    asfPrintStatus("\nCommand line: asf_convert %s\n", configFile);
  asfPrintStatus("Date: %s\n",date_time_stamp());
  asfPrintStatus("PID: %i\n", pid);
  asfPrintStatus("Program: asf_convert\n\n");

  // Update configuration file
  check_return(write_config(configFile, cfg), "Could not update configuration file");
  if (cFlag) {
    asfPrintStatus("   Initialized complete configuration file\n\n");
    FCLOSE(fLog);
    exit(0);
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
	  strncmp(uc(cfg->import->format), "STF", 3) != 0)
	asfPrintError("Chosen import format not supported\n");

      // Radiometry
      if (strncmp(uc(cfg->import->radiometry), "AMPLITUDE_IMAGE", 15) != 0 &&
	  strncmp(uc(cfg->import->radiometry), "POWER_IMAGE", 11) != 0 &&
	  strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) != 0 &&
	  strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) != 0 &&
	  strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) != 0)
	asfPrintError("Chosen radiometry not supported\n");

      // Look up table file existence check
      if (strcmp(cfg->import->lut, "") != 0)
	if (!fileExists(cfg->import->lut))
	  asfPrintError("Look up table file does not exist\n");

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
      if (!fileExists(cfg->geocoding->projection))
	asfPrintError("Projection parameter file does not exist\n");

      // Check for pixel size smaller than threshold ??? 

      // Datum
      if (strncmp(cfg->geocoding->datum, "WGS84", 5) != 0 &&
	  strncmp(cfg->geocoding->datum, "NAD27", 5) != 0 &&
	  strncmp(cfg->geocoding->datum, "NAD83", 5) != 0)
	asfPrintError("Chosen datum not supported\n");

      // Resampling
      if (strncmp(cfg->geocoding->resampling, "NEAREST NEIGHBOR", 16) != 0 &&
	  strncmp(cfg->geocoding->resampling, "BILINEAR", 8) != 0 &&
	  strncmp(cfg->geocoding->resampling, "BICUBIC", 7) != 0)
	asfPrintError("Chosen resampling method not supported\n");
    }

    // Check whether everything in the [Export] block is reasonable
    if (cfg->general->export) {

      // Export format: ASF, TIFF, GEOTIFF, JPEG, PPM
      if (strncmp(uc(cfg->export->format), "ASF", 3) != 0 &&
	  strncmp(uc(cfg->export->format), "TIFF", 4) != 0 &&
	  strncmp(uc(cfg->export->format), "GEOTIFF", 7) != 0 &&
	  strncmp(uc(cfg->export->format), "JPEG", 4) != 0 &&
	  strncmp(uc(cfg->export->format), "PPM", 3) != 0)
	asfPrintError("Chosen export format not supported\n");
      
      // Unset export flag when export format is ASF
      if (strncmp(uc(cfg->export->format), "ASF", 3) == 0)
	cfg->general->export = 0;

      // Scaling method for floating point GeoTIFFs
      if (strncmp(uc(cfg->export->format), "GEOTIFF", 7) == 0) {
	if (strncmp(uc(cfg->export->byte), "SIGMA", 5) != 0 &&
	    strncmp(uc(cfg->export->byte), "MINMAX", 6) != 0 &&
	    strncmp(uc(cfg->export->byte), "TRUNCATE", 8) != 0 &&
	    strncmp(uc(cfg->export->byte), "HISTOGRAM_EQUALIZE", 18) != 0)
	  asfPrintError("Chosen scaling method not supported\n");
      }
    }

    if (!cfg->general->import && !cfg->general->geocoding && !cfg->general->export)
      asfPrintError("Nothing to be done\n");

    // Update configuration file
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
    
    // Let's finally get to work
    sprintf(outFile, "%s", cfg->general->in_name);
    if (strcmp(cfg->general->out_name, "") == 0)
      sprintf(cfg->general->out_name, "%s", cfg->general->in_name);

    if (cfg->general->import) {

      // Radiometry
      if (strncmp(uc(cfg->import->radiometry), "AMPLITUDE_IMAGE", 15) == 0)
	sprintf(radiometry, "-amplitude ");
      else if (strncmp(uc(cfg->import->radiometry), "POWER_IMAGE", 11) == 0)
	sprintf(radiometry, "-power ");
      else if (strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) == 0)
	sprintf(radiometry, "-sigma ");
      else if (strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) == 0)
	sprintf(radiometry, "-gamma ");
      else if (strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) == 0)
	sprintf(radiometry, "-beta ");

      // Pass in command line
      if (cfg->general->geocoding || cfg->general->export)
	sprintf(outFile, "tmp%i_import", pid);
      else
	sprintf(outFile, "%s", cfg->general->out_name);
      check_return(asf_import(cfg->general->in_name, outFile, cfg->import->format, 
			      radiometry, NULL, cfg->import->lat_begin, 
			      cfg->import->lat_end), 
		   "ingesting data file (asf_import)\n");
      if (!cfg->general->geocoding) {
	sprintf("cp %s.meta %s.meta", outFile, cfg->general->in_name);
	system(cmd);
      }
    }

    if (cfg->general->geocoding) {

      // Projection
      sprintf(projection, "--read-proj-file %s", cfg->geocoding->projection);
      
      // Datum
      sprintf(datum, "--datum %s", uc(cfg->geocoding->datum));

      // Resampling method
      if (strncmp(uc(cfg->geocoding->resampling), "NEAREST_NEIGHBOR", 16) == 0)
	sprintf(resampling, "--resample-method nearest_neighbor");
      if (strncmp(uc(cfg->geocoding->resampling), "BILINEAR", 8) == 0)
	sprintf(resampling, "--resample-method bilinear");
      if (strncmp(uc(cfg->geocoding->resampling), "BICUBIC", 7) == 0)
	sprintf(resampling, "--resample-method bicubic");

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      if (cfg->general->export)
	sprintf(outFile, "tmp%i_geocoding", pid);
      else
	sprintf(outFile, "%s", cfg->general->out_name);
      if (cfg->geocoding->force)
	sprintf(options, "--height %.1lf --pixel-size %.2lf %s %s %s --force", 
		cfg->geocoding->height, cfg->geocoding->pixel, 
		projection, datum, resampling);
      else
	sprintf(options, "--height %.1lf --pixel-size %.2lf %s %s %s", 
		cfg->geocoding->height, cfg->geocoding->pixel, 
		projection, datum, resampling);
      check_return(asf_geocode(options, inFile, outFile), 
		   "geocoding data file (asf_geocode)\n");
      sprintf(cmd, "cp %s.meta %s.meta", outFile, cfg->general->in_name);
      system(cmd);
    }

    if (cfg->general->export) {

      // Format
      if (strncmp(uc(cfg->export->format), "TIFF", 4) == 0)
	sprintf(format, "-format tiff");
      else if (strncmp(uc(cfg->export->format), "GEOTIFF", 7) == 0)
	sprintf(format, "-format geotiff");
      else if (strncmp(uc(cfg->export->format), "JPEG", 4) == 0)
	sprintf(format, "-format jpeg");
      else if (strncmp(uc(cfg->export->format), "PPM", 3) == 0)
	sprintf(format, "-format ppm");

      // Byte scaling
      if (strncmp(uc(cfg->export->byte), "TRUNCATE", 8) == 0)
	sprintf(scale, "-byte truncate");
      else if (strncmp(uc(cfg->export->byte), "MINMAX", 6) == 0)
	sprintf(scale, "-byte minmax");
      else if (strncmp(uc(cfg->export->byte), "SIGMA", 5) == 0)
	sprintf(scale, "-byte sigma");
      else if (strncmp(uc(cfg->export->byte), "HISTOGRAM_EQUALIZE", 18) == 0)
	sprintf(scale, "-byte histogram_equalize");

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

  // Update status
  check_return(write_config(configFile, cfg), 
	       "Could not update configuration file");

  return(0);
}
