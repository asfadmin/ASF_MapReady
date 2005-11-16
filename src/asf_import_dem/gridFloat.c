#include <asf.h>
#include <asf_meta.h>
#include <asf_reporting.h>
#include <libasf_proj.h>
#include <asf_raster.h>
#include <spheroids.h>

#include <pcre.h>
#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_statistics_double.h>

#include "asf_import_dem.h"
#include "gridFloat.h"
#include "parse_options.h"
#include "seamless_meta.h"

#define COMP_ARGS   prce_compile_options,&error,&erroffset,NULL
#define EXEC_ARGS   NULL,line,line_length,0,0,ovector,OVECTOR_SIZE

#define LOAD_STRING(method) \
      if ( num_matches > 0) { \
        pcre_get_substring(line,ovector,num_matches,1,fetched_string); \
        (method)(smeta,*fetched_string); \
      }
#define LOAD_INT(method) \
      if ( num_matches > 0) { \
        pcre_get_substring(line,ovector,num_matches,1,fetched_string); \
        (method)(smeta,atoi(*fetched_string)); \
      }
#define LOAD_DOUBLE(method) \
      if ( num_matches > 0) { \
        pcre_get_substring(line,ovector,num_matches,1,fetched_string); \
        (method)(smeta,strtod(*fetched_string,NULL)); \
      }

// ----------------------------------------------------------------------------
// *.prj file format
// -----------------
// Projection    GEOGRAPHIC
// Datum         NAD27
// Spheroid      CLARKE1866
// Units         DD
// Zunits        METERS
// Parameters


seamless_meta_t *
gridFloat_metadata_to_seamless_meta( char *baseName)
{
  char inPrjName[256];
  create_name(inPrjName,baseName,".prj");
  int prj_exists = fileExists(inPrjName);

  char inHdrName[256];
  create_name(inHdrName,baseName,".hdr");
  int hdr_exists = fileExists(inHdrName);

  asfRequire (hdr_exists,"Unable to open gridFloat .hdr file %s.\n",inHdrName);

  // Our illustrious return structure
  seamless_meta_t *smeta = seamless_meta_new();

  // pcre variable needs for compiling the regex
  const char *error;
  int erroffset;
  int prce_compile_options = PCRE_CASELESS | PCRE_DOLLAR_ENDONLY;

  // pcre variable needs for execute the regex on given text
  char line[512];
  static int OVECTOR_SIZE = 511*3;
  int ovector[OVECTOR_SIZE];
  const char *fetched_string[512];
  int num_matches;

  // Nab metadata from the gridfloat .prj file if available
  if (prj_exists) {
    // Compile regex's for the prj file contents
    pcre *re_projection = pcre_compile("^\\s*projection\\s+(\\w+)\\s*$", COMP_ARGS);
    pcre *re_datum = pcre_compile("^\\s*datum\\s+(\\w+)\\s*$", COMP_ARGS);
    pcre *re_spheroid = pcre_compile("^\\s*spheroid\\s+(\\w+)\\s*$", COMP_ARGS);
    pcre *re_units = pcre_compile("^\\s*units\\s+(\\w+)\\s*$", COMP_ARGS);
    pcre *re_zunits = pcre_compile("^\\s*zunits\\s+(\\w+)\\s*$", COMP_ARGS);

    FILE *prj_fp = FOPEN( inPrjName, "rb");
    while (NULL != fgets(line,511,prj_fp)) {
      int line_length = strlen(line);
      // Projection of the data (usually "GEOGRAPHIC")
      num_matches = pcre_exec( re_projection, EXEC_ARGS);
      LOAD_STRING(seamless_meta_set_projection);
      // Datum name (AK=NAD27, everything else=NAD83)
      num_matches = pcre_exec( re_datum, EXEC_ARGS);
      LOAD_STRING(seamless_meta_set_datum);
      // Spheroid name
      num_matches = pcre_exec(re_spheroid, EXEC_ARGS);
      LOAD_STRING(seamless_meta_set_spheroid);
      // Units on the data plain (xy axize) (usually DD -- decimal degrees)
      num_matches = pcre_exec( re_units, EXEC_ARGS);
      LOAD_STRING(seamless_meta_set_units);
      // Height units (z axis) (usually meters)
      num_matches = pcre_exec( re_zunits ,EXEC_ARGS);
      LOAD_STRING(seamless_meta_set_zunits);
    }
    FCLOSE(prj_fp);
  }
  else {
    asfPrintWarning("Unable to open %s for metadata, continuing without it.\n",
                    inPrjName);
  }
  //printf("proj = %s\n",seamless_meta_get_projection(smeta));
  //printf("datum = %s\n", seamless_meta_get_datum(smeta));
  //printf("spheroid = %s\n", seamless_meta_get_spheroid(smeta));
  //printf("units = %s\n", seamless_meta_get_units(smeta));
  //printf("zunits = %s\n", seamless_meta_get_zunits(smeta));

  // This comparison has already been made, but just to be safe...
  if (hdr_exists) {
    pcre *re_ncols = pcre_compile("^\\s*ncols\\s+(\\d+)\\s*$", COMP_ARGS);
    pcre *re_nrows = pcre_compile("^\\s*nrows\\s+(\\d+)\\s*$", COMP_ARGS);
    pcre *re_xllcorner = pcre_compile("^\\s*xllcorner\\s+([+-]?\\d+\\.\\d+)\\s*$", COMP_ARGS);
    pcre *re_yllcorner = pcre_compile("^\\s*yllcorner\\s+([+-]?\\d+\\.\\d+)\\s*$", COMP_ARGS);
    pcre *re_cellsize = pcre_compile("^\\s*cellsize\\s+([+-]?\\d+\\.\\d+)\\s*$", COMP_ARGS);
    pcre *re_NODATA_value = pcre_compile("^\\s*NODATA_value\\s+([+-]?\\d+)\\s*$", COMP_ARGS);
    pcre *re_byteorder = pcre_compile("^\\s*byteorder\\s+([LM]SBFIRST)\\s*$", COMP_ARGS);

    FILE *hdr_fp = FOPEN( inHdrName, "rb");
    while (NULL != fgets(line,511,hdr_fp)) {
      int line_length = strlen(line);
      // Number of columns
      num_matches = pcre_exec(re_ncols, EXEC_ARGS);
      LOAD_INT(seamless_meta_set_ncols);
      // Number of rows
      num_matches = pcre_exec(re_nrows, EXEC_ARGS);
      LOAD_INT(seamless_meta_set_nrows);
      // Lower left corner x dimension
      num_matches = pcre_exec(re_xllcorner, EXEC_ARGS);
      LOAD_DOUBLE(seamless_meta_set_xllcorner);
      // Lower left corner y dimension
      num_matches = pcre_exec(re_yllcorner, EXEC_ARGS);
      LOAD_DOUBLE(seamless_meta_set_yllcorner);
      // Pixel size (both dimensions)
      num_matches = pcre_exec(re_cellsize, EXEC_ARGS);
      LOAD_DOUBLE(seamless_meta_set_cellsize);
      // Value indicating pixel is invalid (should be -9999)
      num_matches = pcre_exec(re_NODATA_value, EXEC_ARGS);
      LOAD_STRING(seamless_meta_set_NODATA_value);
      // Endianness
      num_matches = pcre_exec(re_byteorder, EXEC_ARGS);
      LOAD_STRING(seamless_meta_set_byteorder);
    }
    FCLOSE(hdr_fp);
  }
  else {
    asfPrintError("Unable to open %s for metadata; exiting...\n", inHdrName);
  }
  //printf("ncols = %d\n",seamless_meta_get_ncols(smeta));
  //printf("nrows = %d\n",seamless_meta_get_nrows(smeta));
  //printf("xllcorner = %.15lf\n",seamless_meta_get_xllcorner(smeta));
  //printf("yllcorner = %.15lf\n",seamless_meta_get_yllcorner(smeta));
  //printf("cellsize = %.15lf\n",seamless_meta_get_cellsize(smeta));
  //printf("NODATA_value = %d\n",seamless_meta_get_NODATA_value(smeta));
  //printf("byteorder = %s\n",seamless_meta_get_byteorder(smeta));

  return smeta;
}
