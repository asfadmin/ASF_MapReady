#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "proj.h"
#include "asf_export.h"
#include "asf_import.h"
#include "asf_contact.h"
#include "asf_sar.h"
#include "asf_terrcorr.h"
#include "asf_geocode.h"
#include "asf_nan.h"
#include "ardop_defs.h"
#include <ctype.h>
#include <string.h>
#include <sys/types.h> /* 'DIR' structure (for opendir) */
#include <dirent.h>    /* for opendir itself            */

int findDemFile(char *fileName)
{
  int found = 0;

  // First check for the filename as-is
  if (fileExists(fileName)) {
    found = 1;
  }
  else {
    // Check for .img, .tif, .tiff, .TIF, and .TIFF extensions
    char *img_File = appendExt(fileName, ".img");
    char *tif_File = appendExt(fileName, ".tif");
    char *tiff_File = appendExt(fileName, ".tiff");
    char *TIF_File = appendExt(fileName, ".TIF");
    char *TIFF_File = appendExt(fileName, ".TIFF");

    if (fileExists(img_File)) {
      found = 1;
    }
    else if (fileExists(tif_File)) {
      found = 1;
      strcpy(fileName, tif_File);
    }
    else if (fileExists(tiff_File)) {
      found = 1;
      strcpy(fileName, tiff_File);
    }
    else if (fileExists(TIF_File)) {
      found = 1;
      strcpy(fileName, TIF_File);
    }
    else if (fileExists(TIFF_File)) {
      found = 1;
      strcpy(fileName, TIFF_File);
    }
    else {
      found = 0;
    }

    // Clean up
    FREE(img_File);
    FREE(tif_File);
    FREE(tiff_File);
    FREE(TIF_File);
    FREE(TIFF_File);
  }

  return found;
}

void check_return(int ret, char *msg)
{
  if (ret != 0)
    asfPrintError(msg);
}

// If a temporary directory has not been specified, create one using the time
// stamp as the name of the temporary directory
static void create_and_set_tmp_dir(char *basename, char *tmp_dir)
{
  int empty_name = strlen(tmp_dir)==0;

  if (empty_name) {
    strcpy(tmp_dir, basename);
    strcat(tmp_dir, "-");
    strcat(tmp_dir, time_stamp_dir());
    create_clean_dir(tmp_dir);
  }
  else {
    DIR *dirp = opendir(tmp_dir);
    if (!dirp)
      create_clean_dir(tmp_dir);
    else
      closedir(dirp);
  }

  set_asf_tmp_dir(tmp_dir);
}

/* Make a copy of the metdata file. */
static void copy_meta(char *src, char *dest)
{
  char *tmp1 = appendExt(src, ".meta");
  char *tmp2 = appendExt(dest, ".meta");
  fileCopy(tmp1, tmp2);
  free(tmp1);
  free(tmp2);
}

/* Returns TRUE if the filename has a TIFF extension */
static int has_tiff_ext(const char *f)
{
    char *ext = findExt(f);
    if (ext)
        return
            strcmp_case(ext, ".tif") == 0 ||
            strcmp_case(ext, ".tiff") == 0;
    else
        return FALSE;
}

static char *
convert_tiff(const char *tiff_file, char *what, convert_config *cfg,
             int save_converted)
{
    char *tiff_basename, imported[255], geocoded[255], status[255];
    tiff_basename = stripExt(tiff_file);
    sprintf(imported, "%s/imported_%s", cfg->general->tmp_dir, what);

    // want see "dem" as "DEM" in the status messages, and asf_import
    // requires an uppercase string for the image_data_type
    char *uc_what = MALLOC(strlen(what)+2);
    strcpy(uc_what, uc(what)); // uc() returns ptr to static mem => make copy

    // if user wants to save the converted file, do so, otherwise we
    // put it into the temporary directory along with everything else
    if (save_converted) {
        char *outfileDir = get_dirname(cfg->general->out_name);
        char *basename = get_basename(tiff_basename);
        // the naming scheme of the geocoded dem/mask must match what the
        // gui expects in settings_update_dem/mask.
        sprintf(geocoded, "%sgeocoded_%s_%s", outfileDir, what, basename);
        free(outfileDir);
        free(basename);
    }
    else {
        sprintf(geocoded, "%s/geocoded_%s", cfg->general->tmp_dir, what);
    }

    sprintf(status, "Importing %s...", uc_what);
    update_status(status);

    sprintf(status, "ingesting GeoTIFF %s (asf_import)\n", uc_what);
    check_return(
        asf_import(r_AMP, FALSE, "GEOTIFF", NULL, what, NULL,
                   NULL, 0, 0, NULL, NULL, NULL, NULL, tiff_basename,
                   imported), status);

    sprintf(status, "Geocoding %s...", uc_what);
    update_status(status);

    // Check to see if the TIFF is map-projected or not, and if not
    // then geocode it.  ALSO set the image_data_type field while
    // we're at it
    meta_parameters *mp = meta_read(imported);
    if (strncmp(uc_what, "DEM", 3) == 0) {
      mp->general->image_data_type = DEM;
    }
    else if (strncmp(uc_what, "MASK", 4) == 0) {
      mp->general->image_data_type = MASK;
    }
    meta_write(mp, imported);
    if (!is_map_projected(mp)) {
      if (cfg->general->geocoding) {
          // use the user's projection, if we have it
          sprintf(status, "geocoding GeoTIFF %s (asf_geocode)\n", uc_what);
          check_return(
              asf_geocode_from_proj_file(
                  cfg->geocoding->projection, cfg->geocoding->force,
                  RESAMPLE_NEAREST_NEIGHBOR, 0.0, WGS84_DATUM, NAN,
                  NULL, imported, geocoded, cfg->geocoding->background),
              status);
      }
      else {
          // use UTM if no geocoding specified
          sprintf(status, "geocoding GeoTIFF %s (asf_geocode)\n", uc_what);
          check_return(
              asf_geocode_utm(RESAMPLE_NEAREST_NEIGHBOR, 0.0, WGS84_DATUM,
                              NAN, NULL, imported, geocoded, 0.0), status);
      }
    }
    else { // is map projected, so copy the imported file to the geocoded filename for consistency
      char *infile;
      char *outfile;
      int ch;
      FILE *fin;
      FILE *fout;

      // Copy the .img file
      infile = appendExt(imported, ".img");
      outfile = appendExt(geocoded, ".img");
      fin = FOPEN(infile, "r");
      fout = FOPEN(outfile, "w");

      fread(&ch, 1, 1, fin);
      while (!feof(fin)) {
        FWRITE(&ch, 1, 1, fout);
        fread(&ch, 1, 1, fin);
      }
      FCLOSE(fin);
      FCLOSE(fout);

      // Copy the .meta file
      char metaStr[256];
      infile = appendExt(imported, ".meta");
      outfile = appendExt(geocoded, ".meta");
      fin = FOPEN(infile, "r");
      fout = FOPEN(outfile, "w");

      fgets(metaStr, 256, fin);
      while (!feof(fin)) {
        fputs(metaStr, fout);
        fgets(metaStr, 256, fin);
      }
      FCLOSE(fin);
      FCLOSE(fout);
    }

    meta_free(mp);
    free(uc_what);
    free(tiff_basename);

    return STRDUP(geocoded);
}

int asf_convert_ext(int createflag, char *configFileName, int saveDEM)
{
  convert_config *cfg;
  char inFile[255], outFile[255];

  // If requested, create a config file and exit (if the file does not exist),
  // otherwise read it
  if ( createflag==TRUE && !fileExists(configFileName) ) {
    init_convert_config(configFileName);
    return(EXIT_SUCCESS);
  }
  // Extend the configuration file if the file already exist
  else if ( createflag==TRUE && fileExists(configFileName) ) {
    cfg = read_convert_config(configFileName);
    check_return(write_convert_config(configFileName, cfg),
                 "Could not update configuration file");
    asfPrintStatus("   Initialized complete configuration file\n\n");
    FCLOSE(fLog);
    return(EXIT_SUCCESS);
  }
  else if (!fileExists(configFileName)) {
    asfPrintStatus("Couldn't find config file: %s\n", configFileName);
    FCLOSE(fLog);
    return EXIT_FAILURE;
  }
  else {
    cfg = read_convert_config(configFileName);
  }

  // Check for greyscale output versus selection of a color option
  if (strncmp(uc(cfg->export->format), "PGM", 3) == 0 &&
      (strlen(cfg->export->rgb) > 0 ||
      cfg->export->truecolor        ||
      cfg->export->falsecolor       ||
      cfg->export->pauli            ||
      cfg->export->sinclair)
     )
  {
    asfPrintWarning("Greyscale PGM output is not compatible with color options:\n"
        "(RGB, True Color, False Color, Pauli, or Sinclair) ...\n"
        "Defaulting to producing separate greyscale PGM files for each\n"
        "available band.\n");
    strcpy(cfg->export->rgb, "");
    cfg->export->truecolor = 0;
    cfg->export->falsecolor = 0;
    cfg->export->pauli = 0;
    cfg->export->sinclair = 0;
  }

  // Batch mode processing
  if (strlen(cfg->general->batchFile) > 0) {
    convert_config *tmp_cfg=NULL;
    char tmp_dir[255];
    char tmpCfgName[255];
    char line[255];
    int n_ok = 0, n_bad = 0;
    FILE *fBatch = FOPEN(cfg->general->batchFile, "r");

    strcpy(tmp_dir, cfg->general->tmp_dir);
    while (fgets(line, 255, fBatch) != NULL) {
      char batchItem[255], fileName[255], batchPreDir[255];
      sscanf(line, "%s", batchItem);
      
      // strip off known extensions
      char *p = findExt(batchItem);
      if (p) *p = '\0';

      split_dir_and_file(batchItem, batchPreDir, fileName);

      // Create temporary configuration file
      create_and_set_tmp_dir(fileName, tmp_dir);
      sprintf(tmpCfgName, "%s/%s.cfg", tmp_dir, fileName);
      FILE *fConfig = FOPEN(tmpCfgName, "w");
      fprintf(fConfig, "asf_convert temporary configuration file\n\n");
      fprintf(fConfig, "[General]\n");
      fprintf(fConfig, "default values = %s\n", cfg->general->defaults);
      fprintf(fConfig, "input file = %s\n", batchItem);
      fprintf(fConfig, "output file = %s%s%s\n", cfg->general->prefix,
                                                 fileName,
                                                 cfg->general->suffix);
      fprintf(fConfig, "tmp dir = %s\n", tmp_dir);
      FCLOSE(fConfig);

      // Extend the temporary configuration file
      tmp_cfg = read_convert_config(tmpCfgName);
      check_return(write_convert_config(tmpCfgName, tmp_cfg),
                   "Could not update configuration file");
      free_convert_config(tmp_cfg);

      // Run asf_convert for temporary configuration file

      // This is really quite a kludge-- we used to call the library
      // function here, now we shell out and run the tool directly, sort
      // of a step backwards, it seems.  Unfortunately, in order to keep
      // processing the batch even if an error occurs, we're stuck with
      // this method.  (Otherwise, we'd have to teach asfPrintError to
      // get us back here, to continue the loop.)
      asfPrintStatus("\nProcessing %s ...\n", batchItem);
      int ret = asfSystem("%sasf_convert%s -log %s.log %s",
          get_argv0(), bin_postfix(), batchItem, tmpCfgName);

      if (ret != 0) {
          asfPrintStatus("%s: failed\n", batchItem);
          ++n_bad;
      } else {
          asfPrintStatus("%s: ok\n", batchItem);
          ++n_ok;
      }

      strcpy(tmp_dir, cfg->general->tmp_dir);
    }
    FCLOSE(fBatch);

    asfPrintStatus("\n\nBatch Complete.\n");
    asfPrintStatus("Successfully processed %d/%d file%s.\n", n_ok,
        n_ok + n_bad, n_ok + n_bad == 1 ? "" : "s");

    if (n_bad > 0)
        asfPrintStatus("  *** %d file%s failed. ***\n", n_bad,
            n_bad==1 ? "" : "s");
  }
  // Regular processing
  else {

    if (cfg->general->status_file && strlen(cfg->general->status_file) > 0)
      set_status_file(cfg->general->status_file);

    update_status("Processing...");

    create_and_set_tmp_dir(cfg->general->in_name, cfg->general->tmp_dir);

    // Check that input name isn't the same as the output name
    // (This can happen with CEOS Level 0-- both use .raw)
    if (strcmp_case(cfg->import->format, "CEOS") == 0 &&
        strcmp_case(cfg->general->in_name, cfg->general->out_name) == 0) {
        // the names (actually, the basenames) can match for L1 data
        // it is only the ".RAW" ones we want to flag
        char *RAW_file = appendExt(cfg->general->in_name, ".RAW");
        char *raw_file = appendExt(cfg->general->in_name, ".raw");
        if (fileExists(raw_file) || fileExists(RAW_file))
            asfPrintError("Input and Output names are the same: %s\n",
                          cfg->general->in_name);
        free(raw_file); free(RAW_file);
        RAW_file = appendStr(cfg->general->in_name, ".RAW");
        raw_file = appendStr(cfg->general->in_name, ".raw");
        if (fileExists(raw_file) || fileExists(RAW_file))
            asfPrintError("Input and Output names are the same: %s\n",
                          cfg->general->in_name);
        free(raw_file); free(RAW_file);
    }

    // Check whether everything in the [Import] block is reasonable
    if (cfg->general->import) {

      // Import format: ASF, CEOS or STF, GeoTIFF
      if (strncmp(uc(cfg->import->format), "ASF", 3) != 0 &&
          strncmp(uc(cfg->import->format), "CEOS", 4) != 0 &&
          strncmp(uc(cfg->import->format), "STF", 3) != 0 &&
          strncmp(uc(cfg->import->format), "GEOTIFF", 7) != 0) {
        asfPrintError("Chosen import format not supported\n");
      }

      // Radiometry
      if (strncmp(uc(cfg->import->radiometry), "AMPLITUDE_IMAGE", 15) != 0 &&
          strncmp(uc(cfg->import->radiometry), "POWER_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) != 0) {
        asfPrintError("Chosen radiometry not supported\n");
      }

      // Look up table file existence check
      if (strlen(cfg->import->lut) > 0) {
        if (!fileExists(cfg->import->lut)) {
          asfPrintError("Look up table file does not exist\n");
        }
      }

      // Do not allow terrain correction with sigma, beta, gamma radiometry
      // and not using db.
      if (cfg->general->terrain_correct && !cfg->import->output_db &&
          (strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) == 0 ||
           strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) == 0 ||
           strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) == 0))
      {
          asfPrintError("When using Sigma, Beta, or Gamma radiometry and "
                        "applying terrain correction,\nyou should output "
                        "in dB.\n");
      }

      // Precision state vector file check can only be done
      // from within asf_import

      // Get input file name ready
      strcpy(inFile, cfg->general->in_name);
/*
 * Taking the extension-adding and existence check out on the input file
 *
 * asf_import already does all of this, and it does it better since it
 * handles prepension schemes (alos), and has better handling of basename
 * versus the extension-already-there case, etc.
 *
      if (strcmp_case(cfg->import->format, "ASF")) == 0) {
        create_name(inFile, cfg->general->in_name, ".img");
      }
      else if (strcmp_case(cfg->import->format, "CEOS")) == 0) {
        create_name(inFile, cfg->general->in_name, ".D");
        if (!fileExists(inFile)) {
            create_name(inFile, cfg->general->in_name, ".RAW");
        }
      }
      else if (strcmp_case(cfg->import->format, "STF")) == 0)
        strcpy(inFile, cfg->general->in_name);

      // Data file existence check
      if (!fileExists(inFile))
          asfPrintError("Import data file '%s' does not exist!\n", inFile);

 * ... end of the commented out input-file-existence-check code.
 */

      // Can skip import if the input is already asf internal.
      if (strncmp(uc(cfg->import->format), "ASF", 3) == 0) {
          cfg->general->import = 0;
      }
    }

    // Check whether everything in the [SAR processing] block is reasonable
    if (cfg->general->sar_processing) {

        // ADDED FOR 3.0 -- DO NOT SUPPORT L0 PROCESSING!
        // We actually think this is working in many cases, but it hasn't
        // been fully tested yet for all satellites/beams.
        asfPrintWarning("Processing from level 0 is not fully tested yet!\n");

      // Radiometry
      if (strncmp(uc(cfg->sar_processing->radiometry), "AMPLITUDE_IMAGE", 15) != 0 &&
          strncmp(uc(cfg->sar_processing->radiometry), "POWER_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->sar_processing->radiometry), "SIGMA_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->sar_processing->radiometry), "GAMMA_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->sar_processing->radiometry), "BETA_IMAGE", 10) != 0) {
        asfPrintError("Chosen radiometry not supported\n");
      }

    }

    // Check whether everything in the [C2P] block is reasonable
    if (cfg->general->c2p) {
        asfPrintWarning("SLC ingest with c2p is still underdevelopment.\n");
        // make sure input data has the extension .cpx
        char *ext = findExt(cfg->general->in_name);
        if (strcmp_case(ext, ".cpx") != 0) {
            asfPrintWarning("Input data is not complex.  c2p flag ignored.\n");
            cfg->general->c2p = 0;
        }
    }

    // Check whether everything in the [Image stats] block is reasonable
    if (cfg->general->image_stats) {

      // Values
      if (strncmp(cfg->image_stats->values, "LOOK", 4) != 0 &&
          strncmp(cfg->image_stats->values, "INCIDENCE", 9) != 0 &&
          strncmp(cfg->image_stats->values, "RANGE", 5) != 0) {
        asfPrintError("Chosen values not supported\n");
      }
    }

    // Check whether everything in the [Terrain correction] block is
    // reasonable
    if (cfg->general->terrain_correct) {
      // Reference DEM file existence check
      if (!findDemFile(cfg->terrain_correct->dem)) {
        asfPrintError("Reference DEM file '%s' does not exist\n",
                      cfg->terrain_correct->dem);
      }

      // Check for pixel size smaller than threshold ???

      // specified a mask and asked for an auto-mask
      int have_mask = cfg->terrain_correct->mask &&
          strlen(cfg->terrain_correct->mask) > 0;
      if (have_mask) {
          if (!findDemFile(cfg->terrain_correct->mask)) {
              asfPrintError("MASK file '%s' does not exist\n",
                            cfg->terrain_correct->mask);
          }

          if (cfg->terrain_correct->auto_mask_water) {
              asfPrintStatus("Mask File: %s\n", cfg->terrain_correct->mask);
              asfPrintStatus("Auto Water Mask: %d\n",
                             cfg->terrain_correct->auto_mask_water);
              asfPrintWarning(
                  "You cannot specify a mask for terrain correction "
                  "and also request an automatically generated mask.\n"
                  "Ignoring auto water mask -- will use the mask provided.\n");
          }
      }
    }

    // Check whether everything in the [Geocoding] block is reasonable
    if (cfg->general->geocoding) {

      // Projection file existence check
      if (!fileExists(cfg->geocoding->projection)) {
        asfPrintError("Projection parameter file '%s' does not exist\n",
                      cfg->geocoding->projection);
      }

      // Check for pixel size smaller than threshold ???

      // Datum
      if (strncmp(uc(cfg->geocoding->datum), "WGS84", 5) != 0 &&
          strncmp(uc(cfg->geocoding->datum), "NAD27", 5) != 0 &&
          strncmp(uc(cfg->geocoding->datum), "NAD83", 5) != 0) {
        asfPrintError("Chosen datum not supported\n");
      }

      // Resampling
      if (strncmp(uc(cfg->geocoding->resampling), "NEAREST_NEIGHBOR", 16) != 0 &&
          strncmp(uc(cfg->geocoding->resampling), "BILINEAR", 8) != 0 &&
          strncmp(uc(cfg->geocoding->resampling), "BICUBIC", 7) != 0) {
        asfPrintError("Chosen resampling method not supported\n");
      }

      // Check that the user didn't specify an average height, and
      // also is doing terrain correction
      if (cfg->general->terrain_correct &&
          !cfg->terrain_correct->refine_geolocation_only &&
          cfg->geocoding->height != 0)
      {
          asfPrintWarning("Since terrain correction is being applied, "
                          "asf_geocode will ignore the\nspecified "
                          "average height.\n");
      }
    }

    // Check whether everything in the [Terrain Correct] block is reasonable
    if (cfg->general->terrain_correct) {

        // specified a mask and asked for an auto-mask
        int have_mask = cfg->terrain_correct->mask &&
            strlen(cfg->terrain_correct->mask) > 0;

        if (have_mask && cfg->terrain_correct->auto_mask_water) {
            asfPrintStatus("Mask File: %s\n", cfg->terrain_correct->mask);
            asfPrintStatus("Auto Water Mask: %d\n",
                           cfg->terrain_correct->auto_mask_water);
            asfPrintWarning("You cannot specify a mask for terrain correction "
                          "and also request an automatically generated mask.\n"
                          "Ignoring auto water mask -- will use the mask "
                          "provided.\n");
        }

    }

    // Check whether everything in the [Export] block is reasonable
    if (cfg->general->export) {

      // Export format: ASF, TIFF, GEOTIFF, JPEG, PGM
      if (strncmp(uc(cfg->export->format), "ASF", 3) != 0 &&
          strncmp(uc(cfg->export->format), "TIFF", 4) != 0 &&
          strncmp(uc(cfg->export->format), "GEOTIFF", 7) != 0 &&
          strncmp(uc(cfg->export->format), "JPEG", 4) != 0 &&
          strncmp(uc(cfg->export->format), "PGM", 3) != 0) {
        asfPrintError("Chosen export format (%s) not supported\n",
                     cfg->export->format);
      }

      // Unset export flag when export format is ASF
      if (strncmp(uc(cfg->export->format), "ASF", 3) == 0) {
        cfg->general->export = 0;
      }

      // Scaling method for floating point GeoTIFFs
      if (strncmp(uc(cfg->export->format), "GEOTIFF", 7) == 0) {
        if (strncmp(uc(cfg->export->byte), "NONE", 4) != 0 &&
            strncmp(uc(cfg->export->byte), "SIGMA", 5) != 0 &&
            strncmp(uc(cfg->export->byte), "MINMAX", 6) != 0 &&
            strncmp(uc(cfg->export->byte), "TRUNCATE", 8) != 0 &&
            strncmp(uc(cfg->export->byte), "HISTOGRAM_EQUALIZE", 18) != 0) {
          asfPrintError("Chosen scaling method (%s) not supported\n",
                        cfg->export->byte);
        }
      }

      // If RGB Banding option is "ignore,ignore,ignore" then the
      // user has probably been using the gui, and didn't pick
      // anything for any of the RGB channels.
      int rgbBanding = strlen(cfg->export->rgb) > 0 ? 1 : 0;
      if (rgbBanding &&
          strcmp(uc(cfg->export->rgb), "IGNORE,IGNORE,IGNORE") == 0)
      {
          asfPrintError(
              "Exporting as RGB was selected, but no values for each RGB\n"
              "channels were selected.  Please choose which bands you wish\n"
              "to have placed into at least one of the RGB channels.\n");
      }

      // RGB Banding, True Color, and False Color are mutually-exclusive
      // options.  Check to make sure the config doesn't spec more than
      // one of these.
    }
    int rgbBanding = strlen(cfg->export->rgb) > 0 ? 1 : 0;
    int truecolor = cfg->export->truecolor == 0 ? 0 : 1;
    int falsecolor = cfg->export->falsecolor == 0 ? 0 : 1;
    int pauli = cfg->export->pauli == 0 ? 0 : 1;
    int sinclair = cfg->export->sinclair == 0 ? 0 : 1;
    if (rgbBanding + truecolor + falsecolor + pauli + sinclair > 1)
    {
      asfPrintError("More than one color export mode was selected (rgb banding, truecolor,\n"
          "falsecolor, pauli, or sinclair.)  Only one of these options may\n"
          "be selected at a time.\n");
    }

    if (!cfg->general->import && !cfg->general->sar_processing &&
        !cfg->general->terrain_correct && !cfg->general->geocoding &&
        !cfg->general->export) {
      asfPrintError("Nothing to be done\n");
    }

    //---------------------------------------------------------------
    // Let's finally get to work
    sprintf(outFile, "%s", cfg->general->in_name);
    if (strlen(cfg->general->out_name) == 0) {
      sprintf(cfg->general->out_name, "%s", cfg->general->in_name);
    }

    // global variable-- if set, tells meta_write to also dump .hdr (ENVI) files
    dump_envi_header = cfg->general->dump_envi;

    if (cfg->general->import) {

      update_status("Importing...");

      radiometry_t radiometry;
      int db_flag = FALSE;
      int lut_flag = FALSE;

      // Radiometry
      if (!cfg->general->sar_processing) {
          if (strncmp(uc(cfg->import->radiometry), "AMPLITUDE_IMAGE", 15) == 0) {
              radiometry = r_AMP;
          } else if (strncmp(uc(cfg->import->radiometry), "POWER_IMAGE", 11) == 0) {
              radiometry = r_POWER;
          } else if (strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) == 0) {
              radiometry = r_SIGMA;
          } else if (strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) == 0) {
              radiometry = r_GAMMA;
          } else if (strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) == 0) {
              radiometry = r_BETA;
          }
      }

      // LUT
      if (strlen(cfg->import->lut) > 0)
          lut_flag = TRUE;

      if (cfg->import->output_db)
          db_flag = TRUE;

      // Generate a temporary output filename
      if (cfg->general->image_stats || cfg->general->detect_cr ||
          cfg->general->sar_processing || cfg->general->terrain_correct ||
          cfg->general->geocoding || cfg->general->export) {
        sprintf(outFile, "%s/import", cfg->general->tmp_dir);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }

      // Call asf_import!
      check_return(asf_import(radiometry, db_flag,
                              uc(cfg->import->format),
                              NULL,
                              MAGIC_UNSET_STRING,
                              cfg->import->lut, cfg->import->prc,
                              cfg->import->lat_begin, cfg->import->lat_end,
                              NULL, NULL, NULL, NULL,
                              cfg->general->in_name, outFile),
                   "ingesting data file (asf_import)\n");
    }
    // Make sure truecolor/falsecolor are only specified for optical data
    meta_parameters *meta = meta_read(outFile);
    if (!meta->optical && (truecolor || falsecolor)) {
      asfPrintError("Cannot select True Color or False Color output with non-optical data\n");
    }
    meta_free(meta);

    if (cfg->general->sar_processing) {
      meta_parameters *meta;

      update_status("Running ArDop...");

      // Check whether the input file is a raw image.
      // If not, skip the SAR processing step
      meta = meta_read(outFile);
      if (meta->general->image_data_type == RAW_IMAGE) {

          sprintf(inFile, "%s", outFile);
          if (cfg->general->image_stats || cfg->general->detect_cr ||
              cfg->general->sar_processing || cfg->general->terrain_correct ||
              cfg->general->geocoding || cfg->general->export) {
              sprintf(outFile, "%s/sar_processing", cfg->general->tmp_dir);
          }
          else {
              sprintf(outFile, "%s", cfg->general->out_name);
          }

          struct INPUT_ARDOP_PARAMS *params_in;
          params_in = get_input_ardop_params_struct(inFile, outFile);
          int one = 1;

        // Radiometry
        if (strncmp(uc(cfg->sar_processing->radiometry), "POWER_IMAGE", 11) == 0) {
            params_in->pwrFlag = &one;
        } else if (strncmp(uc(cfg->sar_processing->radiometry), "SIGMA_IMAGE", 11) == 0) {
            params_in->sigmaFlag = &one;
        } else if (strncmp(uc(cfg->sar_processing->radiometry), "GAMMA_IMAGE", 11) == 0) {
            params_in->gammaFlag = &one;
        } else if (strncmp(uc(cfg->sar_processing->radiometry), "BETA_IMAGE", 10) == 0) {
            params_in->betaFlag = &one;
        }

        // When terrain correcting, add the deskew flag
        if (cfg->general->terrain_correct)
            params_in->deskew = &one;

        ardop(params_in);

        if (strcmp_case(cfg->sar_processing->radiometry, "AMPLITUDE_IMAGE") == 0)
          sprintf(outFile, "%s/sar_processing_amp", cfg->general->tmp_dir);
        else if (strcmp_case(cfg->sar_processing->radiometry, "POWER_IMAGE") == 0)
          sprintf(outFile, "%s/sar_processing_power", cfg->general->tmp_dir);
        else if (strcmp_case(cfg->sar_processing->radiometry, "SIGMA_IMAGE") == 0)
          sprintf(outFile, "%s/sar_processing_sigma", cfg->general->tmp_dir);
        else if (strcmp_case(cfg->sar_processing->radiometry, "GAMMA_IMAGE") == 0)
          sprintf(outFile, "%s/sar_processing_gamma", cfg->general->tmp_dir);
        else if (strcmp_case(cfg->sar_processing->radiometry, "BETA_IMAGE") == 0)
          sprintf(outFile, "%s/sar_processing_beta", cfg->general->tmp_dir);
        else
            asfPrintError("Unexpected radiometry: %s\n",
                          cfg->sar_processing->radiometry);

        // If we are not going to be terrain correcting, get the output
        // from ardop into ground range (terrain correction would do that
        // for us).
        if (!cfg->general->terrain_correct)
        {
            asfPrintStatus("Converting to ground range.\n");
            sprintf(inFile, "%s", outFile);
            sprintf(outFile, "%s_gr", inFile);
            sr2gr(inFile, outFile);
        }
      }
      else {
        asfPrintStatus("Image has already been processed - skipping SAR processing step");
      }
      meta_free(meta);
    }

    if (cfg->general->c2p) {

        update_status("Converting Complex to Polar...");

        sprintf(inFile, "%s", outFile);
        if (cfg->general->terrain_correct ||
            cfg->general->geocoding ||
            cfg->general->export)
        {
            sprintf(outFile, "%s/c2p", cfg->general->tmp_dir);
        }
        else 
        {
            sprintf(outFile, "%s", cfg->general->out_name);
        }

        c2p(inFile, outFile, cfg->c2p->multilook, TRUE);
    }

    if (cfg->general->image_stats) {
      char values[255];

      update_status("Running Image Stats...");

      // Values for statistics
      if (strncmp(uc(cfg->image_stats->values), "LOOK", 4) == 0) {
        sprintf(values, "-look");
      } else if (strncmp(uc(cfg->image_stats->values), "INCIDENCE", 9) == 0) {
        sprintf(values, "-incidence");
      } else if (strncmp(uc(cfg->image_stats->values), "RANGE", 5) == 0) {
        sprintf(values, "-range");
      }

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      if (cfg->general->terrain_correct ||
          cfg->general->geocoding ||
          cfg->general->export)
      {
        sprintf(outFile, "%s/image_stats", cfg->general->tmp_dir);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }
      check_return(image_stats(inFile, outFile, values, cfg->image_stats->bins,
                               cfg->image_stats->interval),
                   "running statistics on data file (image_stats)\n");
    }

    if (cfg->general->detect_cr) {

      update_status("Detecting Corner Reflectors...");

      // Intermediate results
      if (cfg->general->intermediates) {
        cfg->detect_cr->chips = 1;
        cfg->detect_cr->text = 1;
      }

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      if (cfg->general->geocoding || cfg->general->export) {
        sprintf(outFile, "%s/detect_cr", cfg->general->tmp_dir);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }
      check_return(detect_cr(inFile, cfg->detect_cr->cr_location, outFile,
                             cfg->detect_cr->chips, cfg->detect_cr->text),
                   "detecting corner reflectors (detect_cr)\n");
    }

    if (cfg->general->terrain_correct) {

      // If the DEM is a GeoTIFF, we need to import it, and geocode it.
      if (has_tiff_ext(cfg->terrain_correct->dem)) {
          char * converted_dem =
              convert_tiff(cfg->terrain_correct->dem, "dem", cfg, saveDEM);
          strcpy(cfg->terrain_correct->dem, converted_dem);
          free(converted_dem);
      }

      // If the Mask is a GeoTIFF, we need to import it, and geocode it.
      if (cfg->terrain_correct->mask &&
          strlen(cfg->terrain_correct->mask) > 0 &&
          has_tiff_ext(cfg->terrain_correct->mask))
      {
          char * converted_mask =
              convert_tiff(cfg->terrain_correct->mask, "mask", cfg, saveDEM);
          strcpy(cfg->terrain_correct->mask, converted_mask);
          free(converted_mask);
      }

      // Generate filenames
      sprintf(inFile, "%s", outFile);
      sprintf(outFile, "%s/terrain_correct", cfg->general->tmp_dir);

      // Call asf_terrcorr!  Or refine_geolocation!
      if (cfg->terrain_correct->refine_geolocation_only) {
          update_status("Refining Geolocation...");
          check_return(
              refine_geolocation(inFile, cfg->terrain_correct->dem,
                                 cfg->terrain_correct->mask, outFile, FALSE,
                                 cfg->terrain_correct->auto_mask_water,
                                 cfg->terrain_correct->water_height_cutoff,
                                 !cfg->general->intermediates, NULL),
            "refining geolocation of the data file (refine_geolocation)\n");
      }
      else {
          update_status("Terrain Correcting...");
          check_return(
            asf_terrcorr_ext(inFile, cfg->terrain_correct->dem,
                             cfg->terrain_correct->mask, outFile,
                             cfg->terrain_correct->pixel,
                             !cfg->general->intermediates,
                             TRUE, FALSE, cfg->terrain_correct->interp,
                             TRUE, 20, TRUE, cfg->terrain_correct->fill_value,
                             cfg->terrain_correct->auto_mask_water,
                             cfg->terrain_correct->save_terrcorr_dem, FALSE,
                             cfg->terrain_correct->water_height_cutoff,
                             cfg->terrain_correct->do_radiometric,
                             cfg->terrain_correct->smooth_dem_holes,
                             NULL),
            "terrain correcting data file (asf_terrcorr)\n");
      }

      if (!cfg->general->export && !cfg->general->geocoding) {
          // if this was the last step, get the terrain corrected output
          // to the output directory -- as well as any other needed files
          renameImgAndMeta(outFile, cfg->general->out_name);
      }
    }

    datum_type_t datum = WGS84_DATUM;

    if (cfg->general->geocoding) {

      update_status("Geocoding...");
      int force_flag = cfg->geocoding->force;
      resample_method_t resample_method = RESAMPLE_BILINEAR;
      double average_height = cfg->geocoding->height;
      double pixel_size = cfg->geocoding->pixel;
      float background_val = cfg->geocoding->background;

      // When terrain correcting, ignore average height -- the height
      // has already been corrected for.
      if (cfg->general->terrain_correct &&
          !cfg->terrain_correct->refine_geolocation_only &&
          cfg->geocoding->height != 0)
      {
          asfPrintWarning("Since terrain correction was applied, ignoring "
                          "average height specification\nfor geocoding.\n");
          average_height = 0.0;
      }

      // Datum
      if (strncmp(uc(cfg->geocoding->datum), "WGS84", 5) == 0) {
        datum = WGS84_DATUM;
      }
      if (strncmp(uc(cfg->geocoding->datum), "NAD27", 5) == 0) {
        datum = NAD27_DATUM;
      }
      if (strncmp(uc(cfg->geocoding->datum), "NAD83", 5) == 0) {
        datum = NAD83_DATUM;
      }

      // Resampling method
      if (strncmp(uc(cfg->geocoding->resampling), "NEAREST_NEIGHBOR", 16) == 0) {
        resample_method = RESAMPLE_NEAREST_NEIGHBOR;
      }
      if (strncmp(uc(cfg->geocoding->resampling), "BILINEAR", 8) == 0) {
        resample_method = RESAMPLE_BILINEAR;
      }
      if (strncmp(uc(cfg->geocoding->resampling), "BICUBIC", 7) == 0) {
        resample_method = RESAMPLE_BICUBIC;
      }

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      if (cfg->general->export) {
        sprintf(outFile, "%s/geocoding", cfg->general->tmp_dir);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }

      check_return(asf_geocode_from_proj_file(cfg->geocoding->projection,
                                              force_flag, resample_method,
                                              average_height, datum,
                                              pixel_size, NULL, inFile, outFile,
                                              background_val),
                   "geocoding data file (asf_geocode)\n");

      // Move the .meta file to be ready for export
      //   ... I don't think we need this now ??  causes problems if
      //       input file is on a read-only filesystem.
      //copy_meta(outFile, cfg->general->in_name);
    }

    output_format_t format = JPEG;

    if (cfg->general->export) {
      int true_color = cfg->export->truecolor == 0 ? 0 : 1;
      int false_color = cfg->export->falsecolor == 0 ? 0 : 1;

      scale_t scale = SIGMA;
      update_status("Exporting...");

      // Format
      if (strncmp(uc(cfg->export->format), "TIFF", 4) == 0) {
        format = TIF;
      } else if (strncmp(uc(cfg->export->format), "GEOTIFF", 7) == 0) {
        format = GEOTIFF;
      } else if (strncmp(uc(cfg->export->format), "JPEG", 4) == 0) {
        format = JPEG;
      } else if (strncmp(uc(cfg->export->format), "PGM", 3) == 0) {
        format = PGM;
      }

      // Byte scaling
      if (strncmp(uc(cfg->export->byte), "TRUNCATE", 8) == 0) {
        scale = TRUNCATE;
      } else if (strncmp(uc(cfg->export->byte), "MINMAX", 6) == 0) {
        scale = MINMAX;
      } else if (strncmp(uc(cfg->export->byte), "SIGMA", 5) == 0) {
        scale = SIGMA;
      } else if (strncmp(uc(cfg->export->byte), "HISTOGRAM_EQUALIZE", 18) == 0) {
        scale = HISTOGRAM_EQUALIZE;
      } else if (strncmp(uc(cfg->export->byte), "NONE", 4) == 0) {
        scale = NONE;
      }

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      sprintf(outFile, "%s", cfg->general->out_name);

      // Move the .meta file out of temporary status
      copy_meta(inFile, outFile);

      if (strlen(cfg->export->rgb) > 0) {
          // user has requested banding
          char *red,  *green, *blue;
          if (split3(cfg->export->rgb, &red, &green, &blue, ',')) {
              int num_found;
              char **bands = find_bands(inFile, TRUE, red, green, blue,
                                        &num_found);
              if (num_found > 0) {
                asfPrintStatus("\nExporting RGB bands into single color file:\n"
                    "Red band  : %s\n"
                    "Green band: %s\n"
                    "Blue band : %s\n\n",
                              red, green, blue);
		check_return(asf_export_bands(format, scale, TRUE, 0, 0, 0, 0, NULL,
                                                inFile, outFile, bands),
                               "export data file (asf_export), banded.\n");
                  int i;
                  for (i = 0; i<num_found; i++) {
                    FREE(bands[i]);
                  }
                  FREE(bands);
              } else {
                  asfPrintError("The requested bands (%s) "
                                "were not available in the file: %s\n",
                                cfg->export->rgb, inFile);
                  strcpy(cfg->export->rgb, "");
              }
              FREE(red); FREE(green); FREE(blue);
          } else {
              asfPrintError("Invalid listing of RGB bands: %s\n",
                            cfg->export->rgb);
              strcpy(cfg->export->rgb, "");
          }
      }

      if (strlen(cfg->export->rgb) == 0)
      {
        meta_parameters *meta = meta_read(inFile);
        if (cfg->export->pauli || cfg->export->sinclair) {
            asfPrintStatus("\nExporting %s-decomposition file...\n\n\n",
                           pauli ? "Pauli" : sinclair ? "Sinclair" : "Unknown");
          if (strstr(meta->general->bands, "HH") == NULL ||
              strstr(meta->general->bands, "VV") == NULL ||
              strstr(meta->general->bands, "VH") == NULL ||
              strstr(meta->general->bands, "VV") == NULL)
          {
            asfPrintError("Imported file does not contain required bands\n"
                "necessary for pauli or sinclair output (HH, VV, HV, VH)\n");
          }
          char **bands = extract_band_names(meta->general->bands, meta->general->band_count);
          if (meta->general->band_count >= 4 && bands != NULL) {
            check_return(asf_export_bands(format, SIGMA, TRUE,
                        0, 0, pauli, sinclair, NULL,
                        inFile, outFile, bands),
            "exporting data file (asf_export), color banded.\n");
            int i;
            for (i=0; i < meta->general->band_count; i++) {
              FREE (bands[i]);
            }
            FREE(bands);
          }
          else {
            asfPrintError("Cannot determine band names from imported metadata file:\n  %s\n",
                          inFile);
          }
        }
        else if (meta->optical && (true_color || false_color)) {
          // Multi-band optical data, exporting as true or false color single file
          asfPrintStatus("\nExporting %s file...\n\n\n",
                         true_color ? "True Color" : false_color ? "False Color" : "Unknown");
          if (true_color &&
              (strstr(meta->general->bands, "01") == NULL ||
               strstr(meta->general->bands, "02") == NULL ||
               strstr(meta->general->bands, "03") == NULL)
             )
          {
            asfPrintError("Imported file does not contain required color bands\n"
                "necessary for true color output (03, 02, 01)\n");
          }
          if (false_color &&
              (strstr(meta->general->bands, "02") == NULL ||
               strstr(meta->general->bands, "03") == NULL ||
               strstr(meta->general->bands, "04") == NULL)
             )
          {
            asfPrintError("Imported file does not contain required color bands\n"
                "necessary for false color output (04, 03, 02)\n");
          }
          if (scale != NONE) {
            asfPrintWarning("A byte conversion other than NONE was specified.  Since\n"
                "True Color or False Color output was selected, the byte conversion\n"
                "will be overridden with SIGMA, a 2-sigma contrast expansion.\n");
          }
          char **bands = extract_band_names(meta->general->bands, meta->general->band_count);
          if (meta->general->band_count >= 4 && bands != NULL) {
            // The imported file IS a multiband file with enough bands,
            // but the extract bands need to be ordered correctly
            if (true_color) {
              strcpy(bands[0], "03");
              strcpy(bands[1], "02");
              strcpy(bands[2], "01");
              strcpy(bands[3], "");
            }
            else {
              strcpy(bands[0], "04");
              strcpy(bands[1], "03");
              strcpy(bands[2], "02");
              strcpy(bands[3], "");
            }
            check_return(asf_export_bands(format, NONE, TRUE,
                        true_color, false_color, 0, 0, NULL,
                        inFile, outFile, bands),
            "exporting data file (asf_export), color banded.\n");
            int i;
            for (i=0; i < meta->general->band_count; i++) {
              FREE (bands[i]);
            }
            FREE(bands);
          }
          else {
            asfPrintError("Cannot determine band names from imported metadata file:\n  %s\n",
                          inFile);
          }
        }
        else {
          if (meta->general->band_count != 1) {
              // multi-band, exporting as separate greyscale files
              asfPrintStatus("Exporting %d bands as separate greyscale files...\n",
                             meta->general->band_count);
              check_return(asf_export_bands(format, scale, FALSE,
                                            0, 0, 0, 0, NULL,
                                            inFile, outFile, NULL),
                           "exporting data file (asf_export), greyscale bands.\n");
          }
          else {
              // single band
              check_return(asf_export(format, scale, inFile, outFile),
                           "exporting data file (asf_export), single band.\n");
          }
        }
        meta_free(meta);
      }
    }

    //---------------------------------------------------------------------
    // At this point the processing of the SAR image is done.
    // We'll now do some of the extra stuff the user may have asked for.

    // Generate a small thumbnail if requested.
    if (cfg->general->thumbnail) {
        asfPrintStatus("Generating Thumbnail image...\n");

        output_format_t format = JPEG;
        scale_t scale = SIGMA;
	meta_parameters *meta;
	double in_side_length, out_pixel_size;
	char *tmpFile;
        int i,n;

	tmpFile = (char *) MALLOC(sizeof(char)*512);

	// Calculate pixel size for generating right size thumbnail
	meta = meta_read(inFile);

	in_side_length = (meta->general->line_count > meta->general->sample_count) ?
	  meta->general->line_count : meta->general->sample_count;
	out_pixel_size =  meta->general->x_pixel_size * in_side_length / 512;

        // Pass in command line
        if (!cfg->general->export)
            sprintf(inFile, "%s", outFile);

        // Put the thumbnail in the intermediates directory, if it is
        // being kept, otherwise in the output directory.
        char *basename = get_basename(cfg->general->out_name);

        if (cfg->general->intermediates) {
            sprintf(outFile, "%s/%s_thumb.jpg",
                    cfg->general->tmp_dir, basename);
            sprintf(tmpFile, "%s/%s_thumb",
                    cfg->general->tmp_dir, basename);
        } else {
            char *tmp = appendToBasename(cfg->general->out_name, "_thumb");
            strcpy(tmpFile, tmp);
            strcpy(outFile, tmp);
            strcat(outFile, ".jpg");
            free(tmp);
        }

	check_return(resample_to_square_pixsiz(inFile, tmpFile, out_pixel_size),
		     "resampling data to thumbnail size (resample)\n");

        if (strlen(cfg->export->rgb) > 0) {
           char *red, *green, *blue;
           if (split3(cfg->export->rgb, &red, &green, &blue, ',')) {
               char **bands = find_bands(inFile, 1, red, green, blue, &n);
               if (n > 0) {
                   check_return(asf_export_bands(format, scale, TRUE, 0, 0, 0, 0, NULL,
                                                 tmpFile, outFile, bands),
                     "exporting thumbnail data file (asf_export), banded\n");
                   for (i=0; i<n; ++i) FREE(bands[i]);
                   FREE(bands);
               }
           }
        }
        else { // no rgb bands selected
          int true_color = cfg->export->truecolor == 0 ? 0 : 1;
          int false_color = cfg->export->falsecolor == 0 ? 0 : 1;
          if (cfg->export->pauli || cfg->export->sinclair) {
              char **bands = extract_band_names(meta->general->bands, meta->general->band_count);
              if (meta->general->band_count >= 4 && bands != NULL) {
                  check_return(asf_export_bands(format, SIGMA, TRUE,
                                                0, 0, cfg->export->pauli, cfg->export->sinclair, NULL,
                                                tmpFile, outFile, bands),
                               "exporting thumbnail data file (asf_export), color banded.\n");
                  int i;
                  for (i=0; i < meta->general->band_count; i++) {
                      FREE (bands[i]);
                  }
                  FREE(bands);
              }
          }
          else if (meta->optical && (true_color || false_color))
          {
            if (meta->optical && (true_color || false_color)) {
              // Multi-band optical data, exporting as true or false color single file
              char **bands = extract_band_names(meta->general->bands, meta->general->band_count);
              if (meta->general->band_count >= 4 && bands != NULL) {
              // The imported file IS a multiband file with enough bands,
              // but the extract bands need to be ordered correctly
                if (true_color) {
                  strcpy(bands[0], "03");
                  strcpy(bands[1], "02");
                  strcpy(bands[2], "01");
                  strcpy(bands[3], "");
                }
                else {
                  strcpy(bands[0], "04");
                  strcpy(bands[1], "03");
                  strcpy(bands[2], "02");
                  strcpy(bands[3], "");
                }
                check_return(asf_export_bands(format, NONE, TRUE,
                             true_color, false_color, 0, 0, NULL,
                             tmpFile, outFile, bands),
                             "exporting thumbnail data file (asf_export), color banded.\n");
                int i;
                for (i=0; i < meta->general->band_count; i++) {
                  FREE (bands[i]);
                }
                FREE(bands);
              }
            }
          }
          else { // not a true or false color optical image
            if (meta->general->band_count == 1) {
              check_return(asf_export(format, scale, tmpFile, outFile),
                           "exporting thumbnail data file (asf_export)\n");
            }
            else {
              // just the first band -- set the others to NULL
              char **bands = find_single_band(tmpFile, "all", &n);
              for (i=1; i<n; ++i) {
                FREE(bands[i]); bands[i] = NULL;
              }
              check_return(asf_export_bands(format, scale, FALSE, 0, 0,
                                            0, 0, NULL, tmpFile, outFile, bands),
                           "exporting thumbnail data file (asf_export)\n");
              // strip off the band name at the end!
              char *banded_name = MALLOC(sizeof(char)*(strlen(outFile)+10));
              if (cfg->general->intermediates) {
                sprintf(banded_name, "%s/%s_thumb_%s.jpg",
                        cfg->general->tmp_dir, basename, bands[0]);
                sprintf(outFile, "%s/%s_thumb.jpg",
                        cfg->general->tmp_dir, basename);
              }
              else {
                sprintf(banded_name, "%s_thumb_%s.jpg",
                        cfg->general->out_name, bands[0]);
                char *tmp =
                    appendToBasename(cfg->general->out_name, "_thumb");
                strcpy(outFile, tmp);
                strcat(outFile, ".jpg");
                free(tmp);
              }
              fileRename(banded_name, outFile);
              FREE(bands[0]);
              FREE(bands);
              FREE(banded_name);
            }
          }
        }

        meta_free(meta);
        free(basename);
    }

    // Process the clipped DEM if requested
    if (cfg->terrain_correct->save_terrcorr_dem) {
        // We know the name of the cut DEM in the temporary directory
        char *tmp = appendToBasename(cfg->terrain_correct->dem, "_cut");
        char *tmp2 = get_basename(tmp);
        sprintf(inFile, "%s/%s", cfg->general->tmp_dir, tmp2);
        free(tmp);
        free(tmp2);

        // output name will be the SAR image's name with a "_dem" added
        // to the basename
        tmp = appendToBasename(cfg->general->out_name, "_dem");
        strcpy(outFile, tmp);
        free(tmp);

        //Never re-geocode the DEM -- assume user has already put it into
        //their favored projection (since at this time we require that
        //DEMs be geocoded for terrain correction ingest).  So, proceed
        //directly to export.
        if (cfg->general->export) {
            update_status("Exporting clipped DEM...");

            check_return(
                asf_export(format, SIGMA, inFile, outFile),
                "exporting clipped dem (asf_export)\n");
        }
        else {
            // User requested that we save the clipped DEM, but chose not
            // to export.  So... just move the clipped DEM out of the tmp dir
            renameImgAndMeta(inFile, outFile);
        }
    }

    // Process the layover/shadow mask if requested
    if (cfg->terrain_correct->save_terrcorr_layover_mask) {
        if (cfg->general->geocoding) {
            update_status("Geocoding layover mask...");
            sprintf(inFile, "%s/terrain_correct_mask",cfg->general->tmp_dir);
            sprintf(outFile, "%s/layover_mask_geocoded",cfg->general->tmp_dir);
            check_return(
                asf_geocode_from_proj_file(
                    cfg->geocoding->projection, cfg->geocoding->force,
                    RESAMPLE_NEAREST_NEIGHBOR, cfg->geocoding->height,
                    datum, cfg->geocoding->pixel, NULL, inFile, outFile,
                    MASK_INVALID_DATA),
                "geocoding clipped DEM (asf_geocode)\n");
        }
        else {
            // no geocoding ... just prepare the 'outFile' param for export
            sprintf(outFile, "%s/terrain_correct_mask", cfg->general->tmp_dir);
        }

        sprintf(inFile, "%s", outFile);
        char *tmp = appendToBasename(cfg->general->out_name, "_layover_mask");
        strcpy(outFile, tmp);
        free(tmp);

        if (cfg->general->export) {
            update_status("Exporting layover mask...");
            check_return(
                asf_export_bands(format, TRUNCATE, 1, 0, 0, 0, 0,
                                 "layover_mask.lut", inFile, outFile, NULL),
                "exporting layover mask (asf_export)\n");
        }
        else {
            // no export... just move the geocoded file out of the
            // temporary directory
            renameImgAndMeta(inFile, outFile);
        }
    }

    if (!cfg->general->intermediates) {
        remove_dir(cfg->general->tmp_dir);
    }

    // Don't change this message unless you also change the code in
    // asf_convert_gui/execute.c to look for a different successful
    // completion string.  GUI currently detects successful processing
    // by looking for this message in the log file.... (yeah, I know.)
    asfPrintStatus("Successful completion!\n\n");
  }

  update_status("Done");
  free_convert_config(cfg);
  clear_status_file();

  return(EXIT_SUCCESS);
}

int asf_convert(int createflag, char *configFileName)
{
    return asf_convert_ext(createflag, configFileName, FALSE);
}
