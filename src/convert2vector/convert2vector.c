#include "asf.h"
#include "asf_meta.h"
#include "asf_vector.h"
#include "asf_endian.h"
#include "asf_license.h"
#include "convert2vector_help.h"
#include <stdio.h>
#include <ctype.h>

#define DEFAULT_OUTPUT_FORMAT   "KML"

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
  char informat[25], outformat[25], infile[255], outfile[255];
  int listFlag=0;
  int inputFormatFlag=0;
  int outputFormatFlag=0;
  int needed_args=3;

  if (argc > 1) {
      check_for_help(argc, argv);
      handle_license_and_version_args(argc, argv, TOOL_NAME);
  }

  /* parse command line */
  strcpy(informat, MAGIC_UNSET_STRING);
  strcpy(outformat, MAGIC_UNSET_STRING);
  strcpy(infile, MAGIC_UNSET_STRING);
  strcpy(outfile, MAGIC_UNSET_STRING);
  strcpy(outformat, DEFAULT_OUTPUT_FORMAT);
  listFlag = checkForOption("-list", argc, argv)    ?
               checkForOption("-list", argc, argv)  :
             checkForOption("--list", argc, argv)   ?
               checkForOption("--list", argc, argv) :
             checkForOption("-l", argc, argv)       ?
               checkForOption("-l", argc, argv)     :
             0;
  inputFormatFlag =  checkForOption("-input-format", argc, argv)                        ?
                       getStringOption("-input-format", argc, argv, informat, NULL)     :
                     checkForOption("--input-format", argc, argv)                       ?
                       getStringOption("--input-format", argc, argv, informat, NULL)    :
                     checkForOption("-i", argc, argv)                                   ?
                       getStringOption("-i", argc, argv, informat, NULL)                :
                     0;
  outputFormatFlag =  checkForOption("-output-format", argc, argv)                      ?
                        getStringOption("-output-format", argc, argv, outformat, NULL)  :
                      checkForOption("--output-format", argc, argv)                     ?
                        getStringOption("--output-format", argc, argv, outformat, NULL) :
                      checkForOption("-i", argc, argv)                                  ?
                        getStringOption("-i", argc, argv, outformat, NULL)              :
                      0;
  needed_args += listFlag         ? 1 : 0; // No argument
  needed_args += inputFormatFlag  ? 2 : 0; // w/Argument
  needed_args += outputFormatFlag ? 2 : 0; // w/Argument
  if (argc < needed_args) {;
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
  strcpy(infile, argv[argc - 2]);
  strcpy(outfile, argv[argc - 1]);

  asfSplashScreen (argc, argv);

  int meta_found    = ismetadata(infile);
  int leader_found  = isleader(infile);
  int point_found   = ispoint(infile);
  int polygon_found = ispolygon(infile);
  int shape_found   = isshape(infile);
  int geotiff_found = isgeotiff(infile);
  int rgps_found    = isrgps(infile);
  int csv_found     = iscsv(infile);
  if (point_found || polygon_found) csv_found = FALSE;
  int types_found = meta_found    +
                    leader_found  +
                    point_found   +
                    polygon_found +
                    shape_found   +
                    geotiff_found +
                    csv_found     +
                    rgps_found    ;
  if (types_found > 1) {
      char *type_list = (char *)MALLOC(sizeof(char)*types_found*1024);
      char msg[7168];

      *type_list = '\0';
      if (meta_found) {
          sprintf(msg, "    %s.meta\n", infile);
          strcat(type_list, msg);
      }
      if (leader_found) {
          sprintf(msg, "    %s.L (or LED-%s etcetera)\n", infile, infile);
          strcat(type_list, msg);
      }
      if (point_found || polygon_found) {
          sprintf(msg, "    %s.csv\n", infile);
          strcat(type_list, msg);
      }
      if (shape_found) {
          sprintf(msg, "    %s.shp (etcetera)\n", infile);
          strcat(type_list, msg);
      }
      if (geotiff_found) {
          sprintf(msg, "    %s.tif or %s.tiff\n", infile, infile);
          strcat(type_list, msg);
      }
      if (csv_found) {
          sprintf(msg, "    %s.csv\n", infile);
          strcat(type_list, msg);
      }
      if (rgps_found) {
          sprintf(msg, "    %s\n", infile);
          strcat(type_list, msg);
      }
      sprintf(msg, "More than one file sharing the given basename, \"%s\", was found\n"
                   "Try running convert2vector again but provide the full file name\n"
                   "for the input file.  The following files were found:\n\n%s\n", infile, type_list);
      FREE(type_list);
      asfPrintError(msg);
  }

  if (types_found == 1 && inputFormatFlag) {
      if ((meta_found    && strncmp_case(informat, "META", 4)    != 0) ||
          (leader_found  && strncmp_case(informat, "LEADER", 6)  != 0) ||
          (point_found   && strncmp_case(informat, "POINT", 5)   != 0) ||
          (polygon_found && strncmp_case(informat, "POLYGON", 7) != 0) ||
          (shape_found   && strncmp_case(informat, "SHAPE", 5)   != 0) ||
          (geotiff_found && strncmp_case(informat, "GEOTIFF", 7) != 0) ||
          (csv_found     && strncmp_case(informat, "CSV", 3)     != 0) ||
          (rgps_found    && strncmp_case(informat, "RGPS", 4)    != 0))
      {
          asfPrintWarning("\nData type found in input file does not agree with\n"
                  "the data type specified with the -input-format flag (%s).\n\n"
                  "Defaulting to the data type which was automatically found\n"
                  "in the input file (%s)\n\n",
                  uc(informat),
                  meta_found    ? "ASF Metadata (META)"                  :
                  leader_found  ? "CEOS Leader data (LEADER)"            :
                  point_found   ? "Point data in a CSV file (POINT)"     :
                  polygon_found ? "Polygon data in a CSV file (POLYGON)" :
                  shape_found   ? "Shape file (SHAPE)"                   :
                  geotiff_found ? "GeoTIFF file (GEOTIFF)"               :
                  csv_found     ? "CSV file (CSV)"               :
                  rgps_found    ? "RGPS file (RGPS)"                     :
                                  "UNKNOWN FILE TYPE");
          strcpy(informat,
                 meta_found    ? "META"    :
                 leader_found  ? "LEADER"  :
                 point_found   ? "POINT"   :
                 polygon_found ? "POLYGON" :
                 shape_found   ? "SHAPE"   :
                 geotiff_found ? "GEOTIFF" :
                 csv_found     ? "CSV"     :
                 rgps_found    ? "RGPS"    :
                                  MAGIC_UNSET_STRING);
      }
  }

  if (!inputFormatFlag) {
      // If the input format option was not used, try to determine the input format from the
      // file itself
      if (ismetadata(infile)) {
          strcpy(informat, "META");
      }
      else if (isleader(infile)) {
          strcpy(informat, "LEADER");
      }
      else if (ispoint(infile)) {
          strcpy(informat, "POINT");
      }
      else if (ispolygon(infile)) {
          strcpy(informat, "POLYGON");
      }
      else if (isshape(infile)) {
          strcpy(informat, "SHAPE");
      }
      else if (isgeotiff(infile)) {
          strcpy(informat, "GEOTIFF");
      }
      else if (iscsv(infile)) {
          strcpy(informat, "CSV");
      }
      else if (isrgps(infile)) {
          strcpy(informat, "RGPS");
      }
      else {
          asfPrintError("Could not automatically determine input file format for %s.\n"
                        "Please use the -input-format option to explicitly select an input\n"
                        "format type.  Try convert2vector -help.\n", infile);
          strcpy(informat, "UNSUPPORTED_TYPE"); // Should never reach here
      }
  }
  else if (strcmp(uc(informat), "GEOTIFF")==0 && !isgeotiff(infile)) {
      asfPrintError("Input TIFF file is not a GeoTIFF.\n");
  }

  // Call library functions that get the work done
  if (listFlag) {
      if (strcmp(uc(informat), "META")==0 && strcmp(uc(outformat), "SHAPE")==0) {
          asfPrintStatus("   Converting a list of metadata files into a shape file ...\n\n");
          write_shape(infile, outfile, META, 1);
      }
      else if (strcmp(uc(informat), "META")==0 && strcmp(uc(outformat), "KML")==0) {
          asfPrintStatus("   Converting a list of metadata files into a kml file ...\n\n");
          write_kml(infile, outfile, META, 1);
      }
      else if (strcmp(uc(informat), "META")==0 && strcmp(uc(outformat), "TEXT")==0) {
          asfPrintStatus("   Converting a list of metadata files into a CSV polygon text file ...\n\n");
          write_text(infile, outfile, META, 1);
      }
      else if
      (strcmp(uc(informat), "LEADER")==0 && strcmp(uc(outformat), "SHAPE")==0) {
          asfPrintStatus("   Converting a list of leader files into a shape file ...\n\n");
          write_shape(infile, outfile, META, 1);
      }
      else if
      (strcmp(uc(informat), "LEADER")==0 && strcmp(uc(outformat), "KML")==0) {
          asfPrintStatus("   Converting a list of leader files into a kml file ...\n\n");
          write_kml(infile, outfile, META, 1);
      }
      else if
      (strcmp(uc(informat), "LEADER")==0 && strcmp(uc(outformat), "TEXT")==0) {
          asfPrintStatus("   Converting a list of leader files into a CSV polygon text file ...\n\n");
          write_text(infile, outfile, META, 1);
      }
      else if
      (strcmp(uc(informat), "POINT")==0 && strcmp(uc(outformat), "SHAPE")==0) {
          asfPrintStatus("   Converting a list of point files into a shape file ...\n\n");
          write_shape(infile, outfile, POINT, 1);
      }
      else if
      (strcmp(uc(informat), "POINT")==0 && strcmp(uc(outformat), "KML")==0) {
          asfPrintStatus("   Converting list of point files into a kml file ...\n\n");
          write_kml(infile, outfile, POINT, 1);
      }
      else if
      (strcmp(uc(informat), "POLYGON")==0 && strcmp(uc(outformat), "SHAPE")==0) {
          asfPrintStatus("   Converting a list of polygon files into a shape file ...\n\n");
          polygon2shape_new(infile, outfile);
      }
      else if
      (strcmp(uc(informat), "POLYGON")==0 && strcmp(uc(outformat), "KML")==0) {
          asfPrintStatus("   Converting list of polygon files into a kml file ...\n\n");
          write_kml(infile, outfile, POLYGON, 1);
      }
      else if
      (strcmp(uc(informat), "SHAPE")==0 && strcmp(uc(outformat), "KML")==0) {
          asfPrintStatus("   Converting a list of shape files into a kml file ...\n\n");
          read_shape(infile, outfile, KMLFILE, 1);
      }
      else if
      (strcmp(uc(informat), "SHAPE")==0 && strcmp(uc(outformat), "TEXT")==0) {
          asfPrintStatus("   Converting a list of shape files into a CSV polygon text file ...\n\n");
          write_text(infile, outfile, TEXT, 1);
          //read_shape(infile, outfile, TEXT, 0); // Use 0?  See branch
      }
      else if
      (strcmp(uc(informat), "GEOTIFF")==0 && strcmp(uc(outformat), "SHAPE")==0) {
          asfPrintStatus("   Converting a list of geotiff files into a shape file ...\n\n");
          write_shape(infile, outfile, GEOTIFF_META, 1);
      }
      else if
      (strcmp(uc(informat), "GEOTIFF")==0 && strcmp(uc(outformat), "KML")==0) {
          asfPrintStatus("   Converting a list of geotiff files into a kml file ...\n\n");
          write_kml(infile, outfile, GEOTIFF_META, 1);
      }
      else if
      (strcmp(uc(informat), "GEOTIFF")==0 && strcmp(uc(outformat), "TEXT")==0) {
          asfPrintStatus("   Converting a list of geotiff files into a CSV polygon text file ...\n\n");
          write_text(infile, outfile, GEOTIFF_META, 1);
      }
      else if
      (strcmp(uc(informat), "RGPS")==0) {
          asfPrintError("-list option is not supported for RGPS format.\n");
      }
      else {
          asfPrintStatus("\nUnsupported conversion.\n");
      }
  }
  else {
    if (strcmp(uc(informat), "CSV")==0 && strcmp(uc(outformat), "SHAPE")==0) {
      asfPrintStatus("   Converting a generic csv file into a shape file ...\n\n");
      csv2shape(infile, outfile);
    }
    else if (strcmp(uc(informat), "CSV")==0 && strcmp(uc(outformat), "KML")==0) {
      asfPrintStatus("   Converting a generic csv file into a kml file ...\n\n");
      csv2kml(infile, outfile);
    }
    else if (strcmp(uc(informat), "META")==0 && strcmp(uc(outformat), "SHAPE")==0) {
      asfPrintStatus("   Converting a metadata file into a shape file ...\n\n");
      write_shape(infile, outfile, META, 0);
    }
    else if (strcmp(uc(informat), "META")==0 && strcmp(uc(outformat), "KML")==0) {
      asfPrintStatus("   Converting a metadata file into a kml file ...\n\n");
      write_kml(infile, outfile, META, 0);
    }
    else if (strcmp(uc(informat), "META")==0 && strcmp(uc(outformat), "TEXT")==0) {
        asfPrintStatus("   Converting a metadata file into a CSV polygon text file ...\n\n");
        write_text(infile, outfile, META, 0);
    }
    else if
      (strcmp(uc(informat), "LEADER")==0 && strcmp(uc(outformat), "SHAPE")==0) {
      asfPrintStatus("   Converting a leader file into a shape file ...\n\n");
      write_shape(infile, outfile, META, 0);
    }
    else if
      (strcmp(uc(informat), "LEADER")==0 && strcmp(uc(outformat), "KML")==0) {
      asfPrintStatus("   Converting a leader file into a kml file ...\n\n");
      write_kml(infile, outfile, META, 0);
    }
    else if
    (strcmp(uc(informat), "LEADER")==0 && strcmp(uc(outformat), "TEXT")==0) {
        asfPrintStatus("   Converting a leader file into a CSV polygon text file ...\n\n");
        write_text(infile, outfile, META, 0);
    }
    else if
      (strcmp(uc(informat), "POINT")==0 && strcmp(uc(outformat), "SHAPE")==0) {
      asfPrintStatus("   Converting a point file into a shape file ...\n\n");
      write_shape(infile, outfile, POINT, 0);
    }
    else if
    (strcmp(uc(informat), "POINT")==0 && strcmp(uc(outformat), "KML")==0) {
        asfPrintStatus("   Converting list of points into a kml file ...\n\n");
        write_kml(infile, outfile, POINT, 0);
    }
    else if
      (strcmp(uc(informat), "POLYGON")==0 && strcmp(uc(outformat), "SHAPE")==0) {
      asfPrintStatus("   Converting a polygon file into a shape file ...\n\n");
      polygon2shape_new(infile, outfile);
    }
    else if
      (strcmp(uc(informat), "POLYGON")==0 && strcmp(uc(outformat), "KML")==0) {
      asfPrintStatus("   Converting a polygon into a kml file ...\n\n");
      write_kml(infile, outfile, POLYGON, 0);
    }
    else if
    (strcmp(uc(informat), "SHAPE")==0 && strcmp(uc(outformat), "KML")==0) {
        asfPrintStatus("   Converting shape file into a kml file ...\n\n");
        read_shape(infile, outfile, KMLFILE, 0);
    }
    else if
    (strcmp(uc(informat), "SHAPE")==0 && strcmp(uc(outformat), "TEXT")==0) {
        asfPrintStatus("   Converting a shapefile into a text file ...\n\n");
        read_shape(infile, outfile, TEXT, 0);
    }
    else if
    (strcmp(uc(informat), "GEOTIFF")==0 && strcmp(uc(outformat), "SHAPE")==0) {
        asfPrintStatus("   Converting a geotiff file into a shape file ...\n\n");
        write_shape(infile, outfile, GEOTIFF_META, 0);
    }
    else if
    (strcmp(uc(informat), "GEOTIFF")==0 && strcmp(uc(outformat), "KML")==0) {
        asfPrintStatus("   Converting a geotiff file into a kml file ...\n\n");
        write_kml(infile, outfile, GEOTIFF_META, 0);
    }
    else if
    (strcmp(uc(informat), "GEOTIFF")==0 && strcmp(uc(outformat), "TEXT")==0) {
        asfPrintStatus("   Converting a geotiff file into a CSV polygon text file ...\n\n");
        write_text(infile, outfile, GEOTIFF_META, 0);
    }
    else if
      (strcmp(uc(informat), "RGPS")==0 && strcmp(uc(outformat), "SHAPE")==0) {
      asfPrintStatus("   Converting list of RGPS cells into a shape file ...\n\n");
      write_shape(infile, outfile, RGPS, 0);
    }
    else if
      (strcmp(uc(informat), "RGPS")==0 && strcmp(uc(outformat), "KML")==0) {
      asfPrintStatus("   Converting list of RGPS cells into a shape file ...\n\n");
      write_kml(infile, outfile, RGPS, 0);
    }
    else {
      asfPrintError("Unsupported conversion.\n");
    }
  }
  asfPrintStatus("\nDone.\n\n");

  return(0);
}

