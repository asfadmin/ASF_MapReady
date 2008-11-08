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

// FIXME: This is just a quick fix for AirSAR and ALOS mosaic ingest
// The db_flag for the asf_import function is obsolete since the radiometry
// in power scale as well as in dB is part of the radiometry type.
// Should be properly cleaned as part of regular code maintenance.
radiometry_t update(radiometry_t radiometry, int db_flag)
{
  if (radiometry == r_SIGMA && db_flag)
    return r_SIGMA_DB;
  else if (radiometry == r_BETA && db_flag)
    return r_BETA_DB;
  else if (radiometry == r_GAMMA && db_flag)
    return r_GAMMA_DB;
  else
    return radiometry;
}

int asf_import(radiometry_t radiometry, int db_flag, int complex_flag,
         int multilook_flag, int amp0_flag, input_format_t format_type,
         char *band_id, char *data_type, char *image_data_type, char *lutName,
         char *prcPath, double lowerLat, double upperLat,
         int line, int sample, int width, int height, int save_intermediates,
         double *p_range_scale, double *p_azimuth_scale,
         double *p_correct_y_pixel_size, int apply_ers2_gain_fix,
         char *inMetaNameOption,
         char *inBaseName, char *outBaseName)
{
  char outDataName[256], outMetaName[256];

  asfPrintStatus("   Importing: %s\n", inBaseName);

  /*
  printf("\nradiometry: %d\n", radiometry);
  printf("db_flag: %d\n", db_flag);
  printf("complex_flag: %d\n", complex_flag);
  printf("multilook_flag: %d\n", multilook_flag);
  printf("amp0_flag: %d\n", amp0_flag);
  printf("format_type: %d\n", format_type);
  printf("band_id: %s\n", band_id);
  printf("data_type: %s\n", data_type);
  printf("image_data_type: %s\n", image_data_type);
  printf("lutName: %s\n", lutName);
  printf("line: %d\n", line);
  printf("sample: %d\n", sample);
  printf("width: %d\n", width);
  printf("height: %d\n", height);
  printf("p_range_scale: %lf\n", &p_range_scale);
  printf("p_azimuth_scale: %lf\n", &p_azimuth_scale);
  printf("p_correct_y_pixel_size: %lf\n", &p_correct_y_pixel_size);
  printf("apply_ers2_gain_fix: %d\n", apply_ers2_gain_fix);
  printf("inMetaNameOption: %s\n", inMetaNameOption);
  printf("inBaseName: %s\n", inBaseName);
  printf("outBaseName: %s\n\n", outBaseName);
  */

  strcpy(outDataName, outBaseName);
  strcpy(outMetaName, outBaseName);
  strcat(outMetaName, TOOLS_META_EXT);

  if ((radiometry == r_SIGMA ||
       radiometry == r_BETA  ||
       radiometry == r_GAMMA ||
       radiometry == r_POWER)   &&
       !(format_type == CEOS || format_type == STF || format_type == AIRSAR ||
	 format_type == ALOS_MOSAIC))
  {
    // A power flag is on, but the input file is not CEOS or STF format
    // so it will be ignored
    asfPrintWarning("Power flags %s%s will be only accepted for the following\n"
		    "data formats since other formats do not indicate what "
                    "type of data is in the file:\n"
		    "CEOS, STF, AirSAR and ALOS mosaics.\n"
		    "Assuming the input data is an AMPLITUDE image...\n",
                    radiometry == r_SIGMA ? "SIGMA" :
                    radiometry == r_BETA  ? "BETA"  :
                    radiometry == r_GAMMA ? "GAMMA" :
                    radiometry == r_POWER ? "POWER" : "UNKNOWN",
                    db_flag               ? " scaled to DECIBELS" : "");
  }

  // Ingest all sorts of flavors of CEOS data/
  if (format_type == CEOS) {
    asfPrintStatus("   Data format: CEOS\n");
    import_ceos(inBaseName, outBaseName, band_id, lutName,
                p_range_scale, p_azimuth_scale, p_correct_y_pixel_size,
                line, sample, width, height, inMetaNameOption, radiometry,
                db_flag, complex_flag, multilook_flag, amp0_flag,
                apply_ers2_gain_fix);
  }
  // Ingest Vexcel Sky Telemetry Format (STF) data
  else if (format_type == STF) {
    asfPrintStatus("   Data format: STF\n");
    int lat_constrained = upperLat != -99 && lowerLat != -99;
    import_stf(inBaseName, outBaseName, radiometry, inMetaNameOption,
               lat_constrained, lowerLat, upperLat, prcPath);
  }
  else if (format_type == GENERIC_GEOTIFF) {
    asfPrintStatus("   Data format: GEOTIFF\n");
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
  else if (format_type == BIL) {
    asfPrintStatus("   Data format: BIL\n");
    import_bil(inBaseName, outBaseName);
  }
  else if (format_type == GRIDFLOAT) {
    asfPrintStatus("   Data format: GRIDFLOAT\n");
    import_gridfloat(inBaseName, outBaseName);
  }
  else if (format_type == AIRSAR) {
    asfPrintStatus("   Data format: AIRSAR\n");
    import_airsar(inBaseName, update(radiometry, db_flag), outBaseName);
  }
  else if (format_type == VP) {
    asfPrintStatus("   Data format: VP\n");
    import_vexcel_plain(inBaseName, outBaseName);
  }
  else if (format_type == JAXA_L0) {
      asfPrintStatus("   Data format: JAXA_L0 (ALOS AVNIR-2 Level 0)\n");
      import_jaxa_L0(inBaseName, outBaseName);
  }
  else if (format_type == ALOS_MOSAIC) {
    asfPrintStatus("   Data format: ALOS MOSAIC\n");
    import_alos_mosaic(inBaseName, update(radiometry, db_flag), outBaseName);
  }
  // Don't recognize this data format; report & quit
  else {
    asfPrintError("Unrecognized data format: '%d'\n",format_type);
  }

  asfPrintStatus("Import complete.\n\n");
  return 0;
}
