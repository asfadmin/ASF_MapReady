#include "asf.h"
#include "asf_meta.h"
#include "asf_vector.h"
#include "asf_endian.h"
#include "asf_license.h"
#include "convert2vector_help.h"
#include <stdio.h>
#include <ctype.h>

#define DEFAULT_OUTPUT_FORMAT   "KML"

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
          asfPrintStatus("   Converting list of points into a kml file ...\n\n");
          write_kml(infile, outfile, POINT, 1);
      }
      else if
      (strcmp(uc(informat), "POLYGON")==0 && strcmp(uc(outformat), "SHAPE")==0) {
          asfPrintStatus("   Converting a list of point files into a shape file ...\n\n");
          polygon2shape_new(infile, outfile);
      }
      else if
      (strcmp(uc(informat), "POLYGON")==0 && strcmp(uc(outformat), "KML")==0) {
          asfPrintStatus("   Converting list of points into a kml file ...\n\n");
          write_kml(infile, outfile, POLYGON, 1);
      }
      else if
      (strcmp(uc(informat), "SHAPE")==0 && strcmp(uc(outformat), "KML")==0) {
          asfPrintStatus("   Converting a list of shape files into a kml file ...\n\n");
          read_shape(infile, outfile, KMLFILE, 1);
      }
      else if
      (strcmp(uc(informat), "SHAPE")==0 && strcmp(uc(outformat), "TEXT")==0) {
          asfPrintStatus("   Converting a list of shapefiles into a CSV polygon text file ...\n\n");
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
    if (strcmp(uc(informat), "META")==0 && strcmp(uc(outformat), "SHAPE")==0) {
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
      asfPrintStatus("   Converting a point file into a shape file ...\n\n");
      polygon2shape_new(infile, outfile); // Use write_shape(,,0)?  See branch...
    }
    else if
      (strcmp(uc(informat), "POLYGON")==0 && strcmp(uc(outformat), "KML")==0) {
      asfPrintStatus("   Converting list of points into a kml file ...\n\n");
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

