#include <asf_contact.h>
#include <asf_license.h>
#include "asf_import.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "ceos.h"
#include "decoder.h"
#include "find_geotiff_name.h"
#include "get_ceos_names.h"
#include "get_stf_names.h"
#include "asf_raster.h"
#include <ctype.h>

int asf_import(radiometry_t radiometry, int db_flag, int complex_flag,
         int multilook_flag, char *format_type, char *band_id,
         char *image_data_type, char *lutName, char *prcPath,
         double lowerLat, double upperLat,
         double *p_range_scale, double *p_azimuth_scale,
         double *p_correct_y_pixel_size, char *inMetaNameOption,
         char *inBaseName, char *outBaseName)
{
  char outDataName[256], outMetaName[256];

  asfPrintStatus("Importing: %s\n", inBaseName);

  strcpy(outDataName, outBaseName);
  strcpy(outMetaName, outBaseName);
  strcat(outMetaName, TOOLS_META_EXT);

  if ((radiometry == r_SIGMA ||
       radiometry == r_BETA  ||
       radiometry == r_GAMMA ||
       radiometry == r_POWER)   &&
       !(strncmp(uc(format_type), "CEOS", 4) == 0 ||
         strncmp(uc(format_type), "STF", 3)  == 0))
  {
    // A power flag is on, but the input file is not CEOS or STF format
    // so it will be ignored
    asfPrintWarning("Power flags %s%s for non-CEOS/non-STF datasets\n"
                    "will be ignored since other data formats do not indicate what\n"
                    "type of data is in the file.  Assuming the input data is an AMPLITUDE\n"
                    "image...\n",
                    radiometry == r_SIGMA ? "SIGMA" :
                    radiometry == r_BETA  ? "BETA"  :
                    radiometry == r_GAMMA ? "GAMMA" :
                    radiometry == r_POWER ? "POWER" : "UNKNOWN",
                    db_flag               ? " scaled to DECIBELS" : "");
  }

  // Ingest all sorts of flavors of CEOS data/
  if (strncmp(format_type, "CEOS", 4) == 0) {
    asfPrintStatus("   Data format: %s\n\n", format_type);
    import_ceos(inBaseName, outBaseName, format_type, band_id, lutName,
                p_range_scale, p_azimuth_scale, p_correct_y_pixel_size,
                inMetaNameOption, radiometry, db_flag, complex_flag,
                multilook_flag);
  }
  // Ingest Vexcel Sky Telemetry Format (STF) data
  else if (strncmp(format_type, "STF", 3) == 0) {
    asfPrintStatus("   Data format: %s\n", format_type);
    int lat_constrained = upperLat != -99 && lowerLat != -99;
    import_stf(inBaseName, outBaseName, radiometry, inMetaNameOption,
               lat_constrained, lowerLat, upperLat, prcPath);
  }
  else if ( strncmp (format_type, "GEOTIFF", 7) == 0 ) {
    asfPrintStatus("   Data format: %s\n", format_type);
    if (band_id != NULL &&
      strlen(band_id) > 0 &&
      strncmp(uc(band_id), "ALL", 3) != 0) {
      asfPrintWarning("The -band option is not supported for data files containing\n"
          "multiple bands within a single file (such as GeoTIFF or TIFF)\n"
          "rather than in individual band files (such as ALOS etc).\n"
          "\nThe import will continue, but all available bands will be\n"
          "imported into a single ASF-format file.  You may select any\n"
          "individual band for export however.\n");
    }
    char *ext = findExt(inBaseName);
    if (ext != NULL) {
      *ext = '\0';
    }
    GString *inGeotiffName = find_geotiff_name (inBaseName);
    if ( inGeotiffName == NULL ) {
      asfPrintError ("Couldn't find a GeoTIFF file (i.e. a file with "
         "extension '.tif', '.tiff',\n'.TIF', or '.TIFF') "
         "corresponding to specified inBaseName:\n"
         "%s\n", inBaseName);
    }
    if (strlen(image_data_type)               &&
        strncmp(image_data_type, MAGIC_UNSET_STRING, strlen(MAGIC_UNSET_STRING)) != 0)
    {
      import_generic_geotiff (inGeotiffName->str, outBaseName, image_data_type);
    }
    else {
      import_generic_geotiff (inGeotiffName->str, outBaseName, NULL);
    }
  }
  else if (strncmp(format_type, "BIL", 3) == 0) {
    asfPrintStatus("   Data format: %s\n", format_type);
    import_bil(inBaseName, outBaseName);
  }
  else if (strncmp(format_type, "GRIDFLOAT", 9) == 0) {
    asfPrintStatus("   Data format: %s\n", format_type);
    import_gridfloat(inBaseName, outBaseName);
  }
  else if (strncmp(format_type, "AIRSAR", 6) == 0) {
    asfPrintStatus("   Data format: %s\n", format_type);
    import_airsar(inBaseName, outBaseName);
  }
  else if (strncmp(format_type, "GAMMA_MSP", 9) == 0) {
    asfPrintStatus("   Data format: %s\n", format_type);
    import_gamma_msp(inBaseName, outBaseName);
  }
  else if (strncmp(format_type, "GAMMA_ISP", 9) == 0) {
    asfPrintStatus("   Data format: %s\n", format_type);
    import_gamma_isp(inBaseName, outBaseName);
  }
  // Don't recognize this data format; report & quit
  else {
    asfPrintError("Unrecognized data format: '%s'\n",format_type);
  }

  asfPrintStatus("Import complete.\n");
  return 0;
}
