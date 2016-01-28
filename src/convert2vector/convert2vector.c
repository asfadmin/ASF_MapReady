#include "asf.h"
#include "asf_meta.h"
#include "asf_vector.h"
#include "asf_endian.h"
#include "asf_license.h"
#include <stdio.h>
#include <ctype.h>

#define VERSION 3.0

void usage(char *name)
{
  printf("\n"
   "USAGE:\n"
   "   %s [-list] [-input-format <format>] [-output-format "
   "<format>]\n"
   "                   [-config <configuration file>] [-nosplit]\n"
   "                   [-wrapdateline <tolerance>]\n"
   "                   [-log <filename>] [-help [<input format>]]\n"
   "                   <input file> <output file>\n", name);
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   input file      Name of the input file.\n"
   "   output file     Name of the output file.\n"
   "   If the -config option is used these two files are not required arguments"
   ".\n");
  printf("\n"
   "OPTIONAL ARGUMENTS:\n"
   "   -config         Name of the configuration file.\n"
   "   -input-format   META - ASF internal .meta file, CEOS leader file,\n"
   "                          GeoTIFF file, TerraSAR-X XML file, Radarsat-2 XML"
   " file\n"
   "                   SMAP - SMAP file in HDF format\n"
   "                   SENTINEL_RAW - SENTINEL (raw data) XML metadata file\n"
   "                   SENTINEL_SLC - SENTINEL (SLC data) XML metadata file\n"
   "                   SENTINEL_GRD - SENTINEL (GRD data) XML metadata file\n"
   "                   GEOTIFF - GeoTIFF file\n"
   "                   CSV - generic CSV file\n"
   "                   POINT - point CSV file (ID,LAT,LON)\n"
   "                   POLYGON - polygon CSV file (ID,LAT,LON)\n"
   "                   LATLON - geographic polygon CSV file (ID,LAT/LON pairs)\n"
   "                   URSA - URSA search output CSV file\n"
   "                   DATAPOOL - DATAPOOL search results CSV file\n"
   "                   GRANULE - ASF internal database table CSV file\n" 
   "   -output-format  Two output format are supported:\n"
   "                   SHAPE - ArcGIS shapefile\n"
   "                   KML - Keyhole Markup Language file\n"
   "   -nosplit        No splitting of vectors at the dateline (SMAP only)\n"
   "   -wrapdateline   Splits the polygon is the coordinate range is within \n"
   "                   the tolerance\n"
   "   -log            Name of the logfile.\n"
   "   -help           Returns the usage of the tool. If a known format is "
   "defined,\n"
   "                   the contents of the data dictionary is returned as well."
   "\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program converts geospatial information into standard vector "
   "formats.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  //exit(EXIT_FAILURE);
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

int main(int argc, char **argv)
{
  char inFormat[25], outFormat[25], configFile[255];
  int currArg = 1, NUM_ARGS = 1, configFlag = FALSE;
  c2v_config *cfg=NULL;

  if (argc < 3) {
    usage(argv[0]);
    exit(1);
  }
    
  // Check for configuration file option first
  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key, "-help", "--help", NULL)) {
      usage(argv[0]);
      char format[25], data_dictionary[512];
      CHECK_ARG(1);
      strcpy(format, GET_ARG(1));
      sprintf(data_dictionary, "%s%cdata_dictionaries%c%s_data_dictionary.csv", 
        get_asf_share_dir(), DIR_SEPARATOR, DIR_SEPARATOR, format);
      if (fileExists(data_dictionary)) {
        asfPrintStatus("\nFormat defined in %s_data_dictionary.csv\n\n", 
        format);
        catFile(data_dictionary);
        asfPrintStatus("\n\n");
      }
      else
        asfPrintWarning("Could not find a data dictionary for format (%s)!\n\n", 
          format);
      exit(1);
    }
    else if (strmatches(key, "-config", "--config", "-c", NULL)) {
      CHECK_ARG(1);
      strcpy(configFile, GET_ARG(1));
      cfg = read_c2v_config(configFile);
      configFlag = TRUE;
    }
  }
  if (!configFlag) {
    sprintf(configFile, "%s%cconvert2vector.config", 
      get_asf_share_dir(), DIR_SEPARATOR);
    asfPrintStatus("\nReading parameters from default configuration file:\n"
		   "%s\n", configFile);
    cfg = read_c2v_config(configFile);
  }

  // Pick up the rest of the arguments
  currArg = 1;
  NUM_ARGS = 2;
  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key, "-config", "--config", "-c", NULL)) { ; }
    else if (strmatches(key, "-log", "--log", NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key, "-quiet", "--quiet", "-q", NULL))
      quietflag = TRUE;
    else if (strmatches(key, "-list", "--list", NULL))
      cfg->list = TRUE;
    else if (strmatches(key, "-nosplit", "--nosplit", "-ns", NULL))
      cfg->nosplit = TRUE;
    else if (strmatches(key, "-wrapdateline", "--wrapdateline", NULL)) {
      CHECK_ARG(1);
      cfg->wrapdateline = atof(GET_ARG(1));
    }
    else if (strmatches(key, "-input-format", "--input-format", "-i", NULL)) {
      CHECK_ARG(1);
      strcpy(cfg->input_format, GET_ARG(1));
    }
    else if (strmatches(key, "-output-format", "--output-format", "-o", NULL)) {
      CHECK_ARG(1);
      strcpy(cfg->output_format, GET_ARG(1));
    }
    else {
      --currArg;
      break;
    }
  }
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }

  if (!configFlag) {
    sprintf(cfg->input_file, "%s", argv[currArg++]);
    sprintf(cfg->output_file, "%s", argv[currArg]);
  }

  asfSplashScreen (argc, argv);

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

