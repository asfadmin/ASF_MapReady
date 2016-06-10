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

static int is_PolSARpro(const char *infile)
{
  int found_bin = FALSE;
  int found_bin_hdr = FALSE;
  char *bin = NULL, *bin_hdr = NULL, *dupe = NULL, *ext = NULL;

  ext = findExt(infile);
  if (!ext) {
    // If no file extension exists, then maybe it has been stripped
    // off.  Guess .bin and check for existence...
    char *inFile = (char *)MALLOC(sizeof(char) * strlen(infile) + 5);
    sprintf(inFile, "%s.bin", infile);
    int ret = is_PolSARpro(inFile);
    FREE(inFile);
    return ret;
  }
  if (strcmp_case(ext, ".bin")==0) {
    bin = (char *)infile;
    bin_hdr = (char *)MALLOC(sizeof(char) * (strlen(infile) + 5));
    sprintf(bin_hdr, "%s.hdr", infile);
    found_bin = fileExists(bin);
    found_bin_hdr = fileExists(bin_hdr);
    FREE(bin_hdr);
  }
  else if (strcmp_case(ext, ".hdr")==0) {
    dupe = STRDUP(infile);
    bin_hdr = (char *)infile;
    ext = findExt(dupe);
    *ext = '\0';
    ext = findExt(dupe);
    if (ext && (strcmp_case(ext, ".bin")==0)) {
      bin = dupe;
    }
    found_bin = fileExists(bin);
    found_bin_hdr = fileExists(bin_hdr);
    FREE(dupe);
  }
  int ret = found_bin + found_bin_hdr;

  return ret;
}

void create_xml_meta(char *inBaseName, char *outBaseName,
		     input_format_t format_type);

int asf_import(radiometry_t radiometry, int db_flag, int complex_flag,
	       int multilook_flag, int azimuth_look_count, int range_look_count,
	       int amp0_flag, input_format_t format_type,
	       char *band_id, char *data_type, char *image_data_type, 
	       char *lutName, char *prcPath, double lowerLat, double upperLat, 
	       double lowerLon, double upperLon, int line, int sample, 
	       int width, int height, int save_intermediates,
	       double *p_range_scale, double *p_azimuth_scale,
	       double *p_correct_y_pixel_size, int apply_ers2_gain_fix,
	       char *inMetaNameOption, char *inBaseName, char *ancillary_file,
	       char *colormapName, char *slave_file, char *interferogram_file,
	       char *coherence_file, char *baseline_file, 
	       int complex_gamma, char *uavsar_type,
	       int metaonly, char *outBaseName)
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
  printf("p_range_scale: %f\n", &p_range_scale);
  printf("p_azimuth_scale: %f\n", &p_azimuth_scale);
  printf("p_correct_y_pixel_size: %f\n", &p_correct_y_pixel_size);
  printf("apply_ers2_gain_fix: %d\n", apply_ers2_gain_fix);
  printf("inMetaNameOption: %s\n", inMetaNameOption);
  printf("inBaseName: %s\n", inBaseName);
  printf("outBaseName: %s\n\n", outBaseName);
  */

  strcpy(outDataName, outBaseName);
  strcpy(outMetaName, outBaseName);
  strcat(outMetaName, TOOLS_META_EXT);

  if (metaonly) {
    create_xml_meta(inBaseName, outBaseName, format_type);
    return 0;
  }

  if ((radiometry == r_SIGMA ||
       radiometry == r_BETA  ||
       radiometry == r_GAMMA ||
       radiometry == r_POWER)   &&
       !(format_type == CEOS || format_type == STF || format_type == AIRSAR ||
	       format_type == ALOS_MOSAIC || format_type == TERRASAR ||
	       format_type == RADARSAT2 || format_type == UAVSAR ||
	       format_type == SENTINEL))
  {
    // A power flag is on, but the input file is not CEOS or STF format
    // so it will be ignored
    asfPrintWarning("Power flags %s%s will be only accepted for the following\n"
		    "data formats since other formats do not indicate what "
                    "type of data is in the file:\n"
		    "CEOS, TERRASAR, STF, AirSAR and ALOS mosaics.\n"
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
                db_flag, complex_flag, multilook_flag, azimuth_look_count,
		range_look_count, amp0_flag, apply_ers2_gain_fix);

    // Replace the restituted state vectors that comes with the image data with
    // precision state vectors from Delft
    update_state_vectors(outBaseName, prcPath);
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
    g_string_free(inGeotiffName,TRUE);
  }
  else if (format_type == BIL) {
    asfPrintStatus("   Data format: BIL\n");
    import_bil(inBaseName, outBaseName);
  }
  else if (format_type == GRIDFLOAT) {
    asfPrintStatus("   Data format: GRIDFLOAT\n");
    import_gridfloat(inBaseName, outBaseName);
  }
  else if (format_type == SEASAT_H5) {
    if (radiometry != r_AMP)
      asfPrintError("Calibration of SEASAT data is not supported.\n");
    asfPrintStatus("   Data format: SEASAT H5\n");
    import_seasat_h5(inBaseName, outBaseName);
  }
  else if (format_type == AIRSAR) {
    asfPrintStatus("   Data format: AIRSAR\n");
    import_airsar(inBaseName, update(radiometry, db_flag), outBaseName);
  }
  else if (format_type == UAVSAR) {
    asfPrintStatus("   Data format: UAVSAR\n");
    if (radiometry == r_BETA || radiometry == r_BETA_DB)
      asfPrintError("Calibration currently does not support BETA values!\n");
    else if (radiometry == r_SIGMA || radiometry == r_SIGMA_DB)
      asfPrintError("Calibration currently does not support SIGMA values!\n");
    import_uavsar(inBaseName, line, sample, width, height, 
		  update(radiometry, db_flag), uavsar_type, outBaseName);
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
  else if (format_type == TERRASAR) {
    asfPrintStatus("   Data format: TERRASAR\n");
    import_terrasar(inBaseName, update(radiometry, db_flag), outBaseName, 0);
  }
  else if (format_type == RADARSAT2) {
    asfPrintStatus("   Data format: RADARSAT2\n");
    import_radarsat2(inBaseName, update(radiometry, db_flag), outBaseName, 0);
  }
  else if (format_type == POLSARPRO) {
    asfPrintStatus("   Data format: POLSARPRO\n");
    import_polsarpro(inBaseName, ancillary_file, colormapName, image_data_type,
		     db_flag, outBaseName);
  }
  else if (format_type == GAMMA) {
    asfPrintStatus("   Data format: GAMMA\n");
    import_gamma(inBaseName, inMetaNameOption, slave_file, interferogram_file,
		 coherence_file, baseline_file, complex_gamma, 
		 outBaseName);
  }
  else if (format_type == ROIPAC_DEPRECATED) {
    asfPrintStatus("   Data format: ROI_PAC (deprecated)\n");
    import_roipac(inBaseName, outBaseName);
  }
  else if (format_type == ROIPAC) {
    asfPrintStatus("   Data format: ROI_PAC\n");
    import_roipac_new(inBaseName, outBaseName, inMetaNameOption, ancillary_file);
  }
  else if (format_type == SMAP) {
    asfPrintStatus("   Data format: SMAP\n");
    import_smap(inBaseName, outBaseName, 
		upperLat, upperLon, lowerLat, lowerLon);
  }
  else if (format_type == SENTINEL) {
    asfPrintStatus("   Data format: SENTINEL\n");
    import_sentinel(inBaseName, update(radiometry, db_flag), lutName, 
      outBaseName);
  }
  // Don't recognize this data format; report & quit
  else {
    asfPrintError("Unrecognized data format: '%d'\n",format_type);
  }
  int num_elements = 0;
  int is_jasc_palette = 0;
  if (format_type == AIRSAR && colormapName != NULL)
    asfPrintError("Colormaps not supported for AirSAR data.\n");
  if (format_type != AIRSAR && format_type != UAVSAR) {
    meta_parameters *meta = meta_read(outMetaName);
    if (meta->colormap) {
      if (!(colormapName != NULL && 
            (meta->general->data_type == ASF_BYTE || is_PolSARpro(inBaseName)))) {
        // Ooops.  Tried to apply a colormap to the wrong type of data
        asfPrintWarning(
          "Color map specified with the -colormap option (%s) not\n"
          "applied during ingest.  Color maps can only be applied to byte or\n"
          "PolSARpro data.  Data was imported as-is.\n", colormapName);
      }
      else {
        // Apply the user-selected color map to the metadata
        FILE *fp = NULL;
        unsigned char * lut_buffer;
        char *lut_basename = STRDUP(colormapName);
        char *ext = findExt(lut_basename);
        if (ext) *ext = '\0';
        
        // Check LUT file validity and allocate appropriately sized buffer
        // to read into
        char magic_str[1024];
        char version_s[1024];
        char num_elements_s[1024];
        char lut_path[1024];
        sprintf(lut_path, "%s%clook_up_tables%c%s.pal", get_asf_share_dir(),
                DIR_SEPARATOR, DIR_SEPARATOR, lut_basename);
        if (fileExists(lut_path)) {
          fp = (FILE*)FOPEN(lut_path, "rt");
          fgets(magic_str, 1024, fp);
          fgets(version_s, 1024, fp);
          fgets(num_elements_s, 1024, fp);
          FCLOSE(fp);
          int version = atoi(version_s);
          num_elements = atoi(num_elements_s);
          is_jasc_palette =
            (strncmp(magic_str, "JASC", 4) == 0 && version == 100) ? 1 : 0;
          if (is_jasc_palette && (num_elements <= 0 || num_elements > 512)) {
            asfPrintWarning(
              "Found invalid JASC-PAL type color map file (%s) ...color map\n"
              "not applied.\n", colormapName);
          }
          if (num_elements > 256) {
            asfPrintWarning(
              "PolSARpro look-up table contains more than 256 elements (%d).\n"
              "Only the first 256 will be read and mapped to data.\n",
              num_elements);
          }
        }
        else {
          sprintf(lut_path, "%s%clook_up_tables%c%s.lut", get_asf_share_dir(),
                  DIR_SEPARATOR, DIR_SEPARATOR, lut_basename);
          if (fileExists(lut_path)) {
            is_jasc_palette = 0; // Assume ASF format
          }
          else {
            strcpy(lut_path, "");
          }
        }
        lut_buffer = MALLOC(sizeof(unsigned char) * 3 * MAX_LUT_DN);
      
        // Read the LUT
        if (strlen(lut_path) > 0) {
          int max_dn = read_lut(lut_path, lut_buffer);
	
          // Populate the metadata colormap
          if (!meta->colormap) meta->colormap = meta_colormap_init();
          if (is_jasc_palette) {
            meta->colormap->num_elements =
              (num_elements <= 256) ? num_elements : 256;
            sprintf(meta->colormap->look_up_table, "%s.pal", lut_basename);
          }
          else {
            num_elements = max_dn + 1;
            meta->colormap->num_elements = num_elements;
            sprintf(meta->colormap->look_up_table, "%s.lut", lut_basename);
          }
          meta->colormap->rgb =
            (meta_rgb*)CALLOC(meta->colormap->num_elements, sizeof(meta_rgb));
          int i;
          for (i = 0; i < meta->colormap->num_elements; i++) {
            meta->colormap->rgb[i].red   = lut_buffer[i*3];
            meta->colormap->rgb[i].green = lut_buffer[i*3+1];
            meta->colormap->rgb[i].blue  = lut_buffer[i*3+2];
          }
        }
        FREE(lut_basename);
        meta_write(meta, outMetaName);
      }
    }
    
    meta_free(meta);
  }

  asfPrintStatus("Import complete.\n\n");
  return 0;
}

void create_xml_meta(char *inBaseName, char *outBaseName,
		     input_format_t format_type)
{
  meta_parameters *meta;

  if (format_type == CEOS) {
    asfPrintStatus("   Data format: CEOS\n\n");
    meta = meta_read_only(inBaseName);
    meta_write_xml(meta, outBaseName);
  }
  else if (format_type == STF) {
    asfPrintStatus("   Data format: STF\n\n");
    meta = meta_read_stf(inBaseName);
    meta_write_xml(meta, outBaseName);
  }
  else if (format_type == GENERIC_GEOTIFF) {
    asfPrintStatus("   Data format: GEOTIFF\n\n");
    int ignore[MAX_BANDS], i;
    for (i=0; i<MAX_BANDS; i++) 
      ignore[i]=0;
    meta = read_generic_geotiff_metadata(inBaseName, ignore, NULL);
    meta_write_xml(meta, outBaseName);
  }
  else if (format_type == BIL) {
    asfPrintStatus("   Data format: BIL\n\n");
    meta = read_meta_bil(inBaseName);
    meta_write_xml(meta, outBaseName);
  }
  else if (format_type == GRIDFLOAT) {
    asfPrintStatus("   Data format: GRIDFLOAT\n\n");
    meta = read_meta_gridfloat(inBaseName);
    meta_write_xml(meta, outBaseName);
  }
  else if (format_type == AIRSAR) {
    asfPrintStatus("   Data format: AIRSAR\n\n");
    read_meta_airsar(inBaseName, outBaseName);
  }
  else if (format_type == UAVSAR) {
    asfPrintStatus("   Data format: UAVSAR\n\n");
    read_meta_uavsar(inBaseName, outBaseName);
  }
  else if (format_type == VP) {
  }
  else if (format_type == JAXA_L0) {
  }
  else if (format_type == ALOS_MOSAIC) {
  }
  else if (format_type == TERRASAR) {
  }
  else if (format_type == RADARSAT2) {
  }
  else if (format_type == POLSARPRO) {
  }
  else if (format_type == GAMMA) {
  }
  else if (format_type == ROIPAC) {
  }
}
