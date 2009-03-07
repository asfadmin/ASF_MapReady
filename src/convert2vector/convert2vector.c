#include "asf.h"
#include "asf_meta.h"
#include "asf_vector.h"
#include "asf_endian.h"
#include "asf_license.h"
#include "convert2vector_help.h"
#include <stdio.h>
#include <ctype.h>

static int iscsv(char *file)
{
  char *ext = findExt(file);
  if (!ext) {
    char *csvfile = appendExt(file, ".csv");
    int ret = fileExists(csvfile);
    free(csvfile);
    return ret;
  } else {
    return strcmp_case(ext,".csv")==0;
  }
}

int main(int argc, char **argv)
{
  char informat_str[25], outformat_str[25], *inFile, *outFile;
  int listFlag=0;
  int testFlag=0;
  int inputFormatFlag=0;
  int outputFormatFlag=0;
  int needed_args=3;
  format_type_t inFormat, outFormat;

  if (argc > 1) {
      check_for_help(argc, argv);
      handle_common_asf_args(&argc, &argv, TOOL_NAME);
  }

  // allocate some memory
  inFile = (char *) MALLOC(sizeof(char)*512);
  outFile = (char *) MALLOC(sizeof(char)*512);

  /* parse command line */
  strcpy(informat_str, "GEOTIFF"); // Default value
  strcpy(outformat_str, "KML"); // Default value
  strcpy(inFile, MAGIC_UNSET_STRING);
  strcpy(outFile, MAGIC_UNSET_STRING);
  listFlag = checkForOption("-list", argc, argv)    ?
               checkForOption("-list", argc, argv)  :
             checkForOption("--list", argc, argv)   ?
               checkForOption("--list", argc, argv) :
             checkForOption("-l", argc, argv)       ?
               checkForOption("-l", argc, argv)     :
             0;
  testFlag = checkForOption("-test", argc, argv)    ?
               checkForOption("-test", argc, argv)  :
             checkForOption("--test", argc, argv)   ?
               checkForOption("--test", argc, argv) :
             checkForOption("-t", argc, argv)       ?
               checkForOption("-t", argc, argv)     :
             0;
  inputFormatFlag =  
    checkForOption("-input-format", argc, argv) ?
    getStringOption("-input-format", argc, argv, informat_str, NULL) :
    checkForOption("--input-format", argc, argv) ?
    getStringOption("--input-format", argc, argv, informat_str, NULL) :
    checkForOption("-i", argc, argv) ?
    getStringOption("-i", argc, argv, informat_str, NULL) : 0;
  outputFormatFlag =  
    checkForOption("-output-format", argc, argv) ?
    getStringOption("-output-format", argc, argv, outformat_str, NULL) :
    checkForOption("--output-format", argc, argv) ?
    getStringOption("--output-format", argc, argv, outformat_str, NULL) :
    checkForOption("-o", argc, argv) ?
    getStringOption("-o", argc, argv, outformat_str, NULL) : 0;
  needed_args += listFlag         ? 1 : 0; // No argument
  needed_args += testFlag         ? 1 : 0; // No argument
  needed_args += inputFormatFlag  ? 2 : 0; // w/Argument
  needed_args += outputFormatFlag ? 2 : 0; // w/Argument
  if (argc < needed_args) {
      usage("Insufficient arguments.");
      exit(1);
  }
  if (argc > needed_args) {
      usage("Too many arguments.");
      exit(1);
  }
  if (listFlag         >= argc - 1  ||
      inputFormatFlag  >= argc - 2  ||
      outputFormatFlag >= argc - 2  )
  {
      // Options other than -help, -version, and -license must precede the input and
      // output filenames
      usage(NULL);
      exit(1);
  }
  sprintf(inFile, "%s", argv[argc - 2]);
  sprintf(outFile, "%s", argv[argc - 1]);

  asfSplashScreen (argc, argv);

  /*
  int meta_found    = ismetadata(inFile);
  int leader_found  = isleader(inFile);
  int point_found   = ispoint(inFile);
  int polygon_found = ispolygon(inFile);
  int shape_found   = isshape(inFile);
  int geotiff_found = isgeotiff(inFile);
  int csv_found     = iscsv(inFile);
  if (point_found || polygon_found) csv_found = FALSE;
  int types_found = meta_found    +
                    leader_found  +
                    point_found   +
                    polygon_found +
                    shape_found   +
                    geotiff_found +
                    csv_found     ;
  if (types_found > 1) {
      char *type_list = (char *)MALLOC(sizeof(char)*types_found*1024);
      char msg[7168];

      *type_list = '\0';
      if (meta_found) {
          sprintf(msg, "    %s.meta\n", inFile);
          strcat(type_list, msg);
      }
      if (leader_found) {
          sprintf(msg, "    %s.L (or LED-%s etcetera)\n", inFile, inFile);
          strcat(type_list, msg);
      }
      if (point_found || polygon_found) {
          sprintf(msg, "    %s.csv\n", inFile);
          strcat(type_list, msg);
      }
      if (shape_found) {
          sprintf(msg, "    %s.shp (etcetera)\n", inFile);
          strcat(type_list, msg);
      }
      if (geotiff_found) {
          sprintf(msg, "    %s.tif or %s.tiff\n", inFile, inFile);
          strcat(type_list, msg);
      }
      if (csv_found) {
          sprintf(msg, "    %s.csv\n", inFile);
          strcat(type_list, msg);
      }
      sprintf(msg, "More than one file sharing the given basename, \"%s\", "
	      "was found\nTry running convert2vector again but provide the "
	      "full file name\nfor the input file.  The following files were "
	      "found:\n\n%s\n", inFile, type_list);
      FREE(type_list);
      asfPrintError(msg);
  }
  */

  inFormat = str2format(informat_str);
  outFormat = str2format(outformat_str);

  /*
  if (types_found == 1 && inputFormatFlag) {
    if ((meta_found    && inFormat != META) ||
	(leader_found  && inFormat != LEADER) ||
	(point_found   && inFormat != POINT) ||
	(polygon_found && inFormat != POLYGON) ||
	(shape_found   && inFormat != SHAPEFILE) ||
	(geotiff_found && inFormat != GEOTIFF_META) ||
	(csv_found     && inFormat != AUIG && inFormat != CSV)) {
      asfPrintWarning("\nData type found in input file does not agree with\n"
		      "the data type specified with the -input-format flag "
		      "(%s).\n\n"
		      "Defaulting to the data type which was automatically "
		      "found\nin the input file (%s)\n\n",
                  uc(informat_str),
                  meta_found    ? "ASF Metadata (META)"                  :
                  leader_found  ? "CEOS Leader data (LEADER)"            :
                  point_found   ? "Point data in a CSV file (POINT)"     :
                  polygon_found ? "Polygon data in a CSV file (POLYGON)" :
                  shape_found   ? "Shape file (SHAPE)"                   :
                  geotiff_found ? "GeoTIFF file (GEOTIFF)"               :
                  csv_found     ? "CSV file (CSV)"                       :
                                  "UNKNOWN FILE TYPE");
      strcpy(informat_str,
	     meta_found    ? "META"    :
	     leader_found  ? "LEADER"  :
	     point_found   ? "POINT"   :
	     polygon_found ? "POLYGON" :
	     shape_found   ? "SHAPE"   :
	     geotiff_found ? "GEOTIFF" :
	     csv_found     ? "CSV"     :
	     MAGIC_UNSET_STRING);
    }
  }
  */

  if (!inputFormatFlag) {
    // If the input format option was not used, try to determine the input 
    // format from the file itself
    if (ismetadata(inFile))
      inFormat = META;
    else if (isleader(inFile))
      inFormat = LEADER;
    else if (ispoint(inFile))
      inFormat = POINT;
    else if (ispolygon(inFile))
      inFormat = POLYGON;
    else if (isshape(inFile))
      inFormat = SHAPEFILE;
    else if (isgeotiff(inFile))
      inFormat = GEOTIFF_META;
    else if (iscsv(inFile))
      inFormat = CSV;
    else if (isparfile(inFile))
      inFormat = STF_META;
    else 
      asfPrintError("Could not automatically determine input file format "
		    "for %s.\nPlease use the -input-format option to "
		    "explicitly select an input\nformat type.  Try "
		    "convert2vector -help.\n", inFile);
  }
  else if (inFormat == GEOTIFF_META && !isgeotiff(inFile))
    asfPrintError("Input TIFF file is not a GeoTIFF.\n");

  // Input formats
  if (inFormat == META)
    asfPrintStatus("   Converting a metadata file ");
  else if (inFormat == LEADER) 
    asfPrintStatus("   Converting a leader file ");
  else if (inFormat == STF_META)
    asfPrintStatus("   Converting an STF file ");
  else if (inFormat == CSV)
    asfPrintStatus("   Converting a generic csv file ");
  else if (inFormat == AUIG)
    asfPrintStatus("   Converting an AUIG file ");
  else if (inFormat == POINT) 
    asfPrintStatus("   Converting a point file ");
  else if (inFormat == POLYGON) 
    asfPrintStatus("   Converting a polygon file ");
  else if (inFormat == SHAPEFILE) 
    asfPrintStatus("   Converting shape file ");
  else if (inFormat == KMLFILE) 
    asfPrintStatus("   Converting a KML file ");
  else if (inFormat == GEOTIFF_META) 
    asfPrintStatus("   Converting a geotiff file ");
  else if (inFormat == TERRASAR_META)
    asfPrintStatus("   Converting a Terrasar metadata file ");
  else if (inFormat == URSA)
    asfPrintStatus("   Converting a generic URSA CSV file ");
  else {
    dbf_header_t *dbf;
    int nCols;

    // Check whether you can find information about the format in the header
    // list file in the share directory
    if (read_header_config(uc(informat_str), &dbf, &nCols))
      asfPrintStatus("   Converting a %s format file ", uc(informat_str));
    else
      asfPrintError("   Unsupported input format (%s)\n", uc(informat_str));
  }
  
  // Output formats
  if (outFormat == SHAPEFILE)
    asfPrintStatus("into a shape file ...\n\n");
  else if (outFormat == KMLFILE) 
    asfPrintStatus("into a KML file ...\n\n");
  else if (outFormat == CSV) 
    asfPrintStatus("into a generic CSV file ...\n\n");
  else if (outFormat == META)
    asfPrintStatus("into a metatdata file ...\n\n");
  else if (outFormat == POLYGON) 
    asfPrintStatus("into a polygon file ...\n\n");
  else if (outFormat == POINT)
    asfPrintStatus("into a list of points ...\n\n");
  else if (outFormat == AUIG) 
    asfPrintStatus("into an AUIG CSV file ...\n");
  else if (outFormat == URSA)
    asfPrintStatus("into an URSA file ...\n");
  else {
    dbf_header_t *dbf;
    int nCols;

    // Check whether you can find information about the format in the header
    // list file in the share directory
    if (read_header_config(uc(outformat_str), &dbf, &nCols))
      asfPrintStatus("into a %s format file ...\n\n", uc(outformat_str));
    else
      asfPrintError("   Unsupported output format (%s)\n", uc(outformat_str));
  }

  if (testFlag)
    test_c2v(inFile, informat_str, outFile, outformat_str);
  else
    convert2vector(inFile, informat_str, outFile, outformat_str, listFlag);

  asfPrintStatus("Done.\n\n");

  // Clean up
  FREE(inFile);
  FREE(outFile);

  return(0);
}

