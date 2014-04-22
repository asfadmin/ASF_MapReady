#include "asf.h"
#include "asf_meta.h"
#include "asf_vector.h"
#include "asf_endian.h"
#include "asf_license.h"
#include "convert2vector_help.h"
#include <stdio.h>
#include <ctype.h>

#define VERSION 3.0

void usage()
{
  printf("\n"
   "USAGE:\n"
   "   convert2vector [-list] [-input-format <format>] [-output-format "
   "<format>]\n" \
   "                  [-config <configuration file>]\n" \
   "                  [-log <filename>] [-license] [-version] [-help "
   "[<input format>]]\n" \
   "                  <input file> <output file>\n");
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   snapshot      Basename of the output file.\n"
   "                 cells: cells_<snapshot>, grid points: grid_<snapshot>\n");
  printf("\n"
   "OPTIONAL ARGUMENTS:\n"
   "   -cells        Generates a data layer with the cell information.\n"
   "   grid points   File name with grid point table.\n"
   "   attributes    File name with cell attribute table.\n"
   "   connectvity   File name with cell connectivity table.\n"
   "   cell definition    File name with cell definition table.\n"
   "   grid definition    File name with grid definition table.\n"
   "   input type    input files can database files or backup text files.\n"
   "                 database: 'db' or text file: 'txt'.\n"
   "   -weather      Generates a data layer with weather information.\n"
   "   table         File name with weather table.\n"
   "   date          Date for which to extract the weather data\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program converts information out of the RGPS database into\n"
   "   ArcGIS shape files and KML files.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  char inFormat[25], outFormat[25], configFile[255];
  int listFlag=0;
  int stackFlag=0;
  int timeFlag=0;
  int testFlag=0;
  int inputFormatFlag=0;
  int outputFormatFlag=0;
  int configFlag=0;
  int overlayFlag=0;
  int transparencyFlag=0;
  int northFlag=0, southFlag=0, eastFlag=0, westFlag=0;
  int needed_args=3;
  c2v_config *cfg=NULL;

  if (argc > 1) {
      check_for_help(argc, argv);
      handle_common_asf_args(&argc, &argv, TOOL_NAME);
  }

  // parse command line
  configFlag =  
    checkForOption("-config", argc, argv) ?
    getStringOption("-config", argc, argv, configFile, NULL) :
    checkForOption("--config", argc, argv) ?
    getStringOption("--config", argc, argv, configFile, NULL) :
    checkForOption("-c", argc, argv) ?
    getStringOption("-c", argc, argv, configFile, NULL) : 0;
  if (!configFlag) {
    sprintf(configFile, "%s/convert2vector.config", get_asf_share_dir());
    asfPrintStatus("\nReading parameters from default configuration file:\n"
		   "%s\n", configFile);
    cfg = read_c2v_config(configFile);
  }
  listFlag = checkForOption("-list", argc, argv)    ?
               checkForOption("-list", argc, argv)  :
             checkForOption("--list", argc, argv)   ?
               checkForOption("--list", argc, argv) :
             checkForOption("-l", argc, argv)       ?
               checkForOption("-l", argc, argv)     :
             0;
  if (listFlag)
    cfg->list = TRUE;
  stackFlag = checkForOption("-stack", argc, argv)    ?
    checkForOption("-stack", argc, argv)  :
    checkForOption("--stack", argc, argv)   ?
    checkForOption("--stack", argc, argv) :
    checkForOption("-s", argc, argv)       ?
    checkForOption("-s", argc, argv)     :
             0;
  if (stackFlag)
    cfg->stack = TRUE;
  timeFlag = checkForOption("-time", argc, argv);
  if (timeFlag)
    cfg->time = TRUE;
  testFlag = checkForOption("-test", argc, argv)    ?
               checkForOption("-test", argc, argv)  :
             checkForOption("--test", argc, argv)   ?
               checkForOption("--test", argc, argv) : 0;
  overlayFlag =
    checkForOption("-png", argc, argv) ?
    getStringOption("-png", argc, argv, cfg->overlay, NULL) :
    checkForOption("--png", argc, argv) ?
    getStringOption("--png", argc, argv, cfg->overlay, NULL) : 0;
  transparencyFlag =
    checkForOption("-transparency", argc, argv) ?
    getIntegerOption("-transparency", argc, argv, &cfg->transparency, 50) :
    checkForOption("--transparency", argc, argv) ?
    getIntegerOption("--transparency", argc, argv, &cfg->transparency, 50) : 0;
  northFlag =  
    checkForOption("-north", argc, argv) ?
    getDoubleOption("-north", argc, argv, &cfg->north, MAGIC_UNSET_DOUBLE) :
    checkForOption("--north", argc, argv) ?
    getDoubleOption("--north", argc, argv, &cfg->north, MAGIC_UNSET_DOUBLE) :
    checkForOption("-n", argc, argv) ?
    getDoubleOption("-n", argc, argv, &cfg->north, MAGIC_UNSET_DOUBLE) : 0;
  southFlag =  
    checkForOption("-south", argc, argv) ?
    getDoubleOption("-south", argc, argv, &cfg->south, MAGIC_UNSET_DOUBLE) :
    checkForOption("--south", argc, argv) ?
    getDoubleOption("--south", argc, argv, &cfg->south, MAGIC_UNSET_DOUBLE) :
    checkForOption("-s", argc, argv) ?
    getDoubleOption("-s", argc, argv, &cfg->south, MAGIC_UNSET_DOUBLE) : 0;
  eastFlag =  
    checkForOption("-east", argc, argv) ?
    getDoubleOption("-east", argc, argv, &cfg->east, MAGIC_UNSET_DOUBLE) :
    checkForOption("--east", argc, argv) ?
    getDoubleOption("--east", argc, argv, &cfg->east, MAGIC_UNSET_DOUBLE) :
    checkForOption("-e", argc, argv) ?
    getDoubleOption("-e", argc, argv, &cfg->east, MAGIC_UNSET_DOUBLE) : 0;
  westFlag =  
    checkForOption("-west", argc, argv) ?
    getDoubleOption("-west", argc, argv, &cfg->west, MAGIC_UNSET_DOUBLE) :
    checkForOption("--west", argc, argv) ?
    getDoubleOption("--west", argc, argv, &cfg->west, MAGIC_UNSET_DOUBLE) :
    checkForOption("-w", argc, argv) ?
    getDoubleOption("-w", argc, argv, &cfg->west, MAGIC_UNSET_DOUBLE) : 0;
  inputFormatFlag =  
    checkForOption("-input-format", argc, argv) ?
    getStringOption("-input-format", argc, argv, cfg->input_format, NULL) :
    checkForOption("--input-format", argc, argv) ?
    getStringOption("--input-format", argc, argv, cfg->input_format, NULL) :
    checkForOption("-i", argc, argv) ?
    getStringOption("-i", argc, argv, cfg->input_format, NULL) : 0;
  outputFormatFlag =  
    checkForOption("-output-format", argc, argv) ?
    getStringOption("-output-format", argc, argv, cfg->output_format, NULL) :
    checkForOption("--output-format", argc, argv) ?
    getStringOption("--output-format", argc, argv, cfg->output_format, NULL) :
    checkForOption("-o", argc, argv) ?
    getStringOption("-o", argc, argv, cfg->output_format, NULL) : 0;
  needed_args += listFlag         ? 1 : 0; // No argument
  needed_args += stackFlag        ? 1 : 0; // No argument
  needed_args += testFlag         ? 1 : 0; // No argument
  needed_args += timeFlag         ? 1 : 0; // No argument
  needed_args += inputFormatFlag  ? 2 : 0; // w/Argument
  needed_args += outputFormatFlag ? 2 : 0; // w/Argument
  needed_args += overlayFlag      ? 2 : 0; // w/Argument
  needed_args += transparencyFlag ? 2 : 0; // w/Argument
  needed_args += northFlag        ? 2 : 0; // w/Argument
  needed_args += southFlag        ? 2 : 0; // w/Argument
  needed_args += eastFlag         ? 2 : 0; // w/Argument
  needed_args += westFlag         ? 2 : 0; // w/Argument
  if (argc < needed_args) {
      usage("Insufficient arguments.");
      exit(1);
  }
  if (argc > needed_args) {
      usage("Too many arguments.");
      exit(1);
  }
  if (listFlag         >= argc - 1  ||
      stackFlag        >= argc - 1  ||
      timeFlag         >= argc - 1  ||
      inputFormatFlag  >= argc - 2  ||
      outputFormatFlag >= argc - 2)
  {
      // Options other than -help, -version, and -license must precede the input and
      // output filenames
      usage(NULL);
      exit(1);
  }
  if (!configFlag) {
    sprintf(cfg->input_file, "%s", argv[argc - 2]);
    sprintf(cfg->output_file, "%s", argv[argc - 1]);
  }

  asfSplashScreen (argc, argv);

  // Read configuration file
  if (configFlag) {
    cfg = read_c2v_config(configFile);
    if (cfg) {
      asfPrintStatus("Reading parameters from configuration file.\n"
		     "Input and output formats as well as the list flag, "
		     "given on the command line,\nsupersede configuration file "
		     "information.\n\n");
      if (cfg->list)
	    listFlag = TRUE;
      // fix me: take care of directory
    }
    else
      asfPrintError("Could not read configuration file '%s'.\n"
		    "There is a sample configuration file located at\n"
		    "'%s/convert2vector.config'.\n", 
		    configFile, get_asf_share_dir());
  }

  // Check the lat/lon extents of the overlay
  if (cfg->north < -90.0 || cfg->north > 90.0)
    asfPrintError("Northern extent (%.4lf) outside the valid value range"
		  " (-90 to 90)\n", cfg->north);
  if (cfg->south < -90.0 || cfg->south > 90.0)
    asfPrintError("Southern extent (%.4lf) outside the valid value range"
		  " (-90 to 90)\n", cfg->south);
  if (cfg->east < -180.0 || cfg->east > 180.0)
    asfPrintError("Eastern extent (%.4lf) outside the valid value range"
		  " (-180 to 180)\n", cfg->east);
  if (cfg->west < -180.0 || cfg->west > 180.0)
    asfPrintError("Western extent (%.4lf) outside the valid value range"
		  " (-180 to 180)\n", cfg->west);
  
  // Check the opacity
  if (cfg->transparency < 0 || cfg->transparency > 100) {
    asfPrintWarning("Transparency (%d) outside the valid value range (0 to 100)\n"
		    "Setting it to the default value of 50\n", cfg->transparency);
    cfg->transparency = 50;
  }

  sprintf(inFormat, "%s", uc(cfg->input_format));
  sprintf(outFormat, "%s", uc(cfg->output_format));
  
  // Check whether you can find information about the format in the header
  // list file in the share directory
  dbf_header_t *dbf;
  int nCols;
  char shape_type[25];
  if (strcmp_case(inFormat, "CSV") == 0 ||
    read_header_config(inFormat, &dbf, &nCols, shape_type))
    asfPrintStatus("   Converting a %s format file to %s\n", 
      inFormat, outFormat);
  else
    asfPrintError("   Unsupported input format (%s)\n", inFormat);
  
  // Set output directory as the temporary directory -- where all temp files
  // created during import should be put
  char *tmpdir = get_dirname(cfg->output_file);
  if (tmpdir && strlen(tmpdir) > 0)
    set_asf_tmp_dir(tmpdir);

  convert2vector(cfg);

  asfPrintStatus("Done.\n\n");

  return(0);
}

