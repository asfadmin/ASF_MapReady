#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "proj.h"
#include "asf_import.h"
#include "asf_contact.h"
#include "asf_import.h"
#include "asf_terrcorr.h"
#include "asf_geocode.h"
#include "asf_export.h"
#include "ardop_defs.h"
#include <unistd.h>
#include <ctype.h>

void check_return(int ret, char *msg)
{
  if (ret != 0)
    asfPrintError(msg);
}

char *uc(char *string)
{
  static char out[1024];
  int i;

  for (i=0; i<strlen(string); i++)
    out[i]=toupper(string[i]);
  out[i]='\0';

  return out;
}

void update_status(convert_config *cfg, const char *format, ...)
{
  if (cfg->general->status_file && strlen(cfg->general->status_file) > 0)
  {
    FILE *fStat = FOPEN(cfg->general->status_file, "wt");
    if (fStat) {
      va_list ap;
      va_start(ap, format);
      vfprintf(fStat, format, ap);
      va_end(ap);
      fclose(fStat);
    }
  }
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

int asf_convert(int createflag, char *configFileName)
{
  FILE *fBatch, *fConfig;
  convert_config *cfg;
  meta_parameters *meta;
  char cmd[1024], line[255], batchConfig[255];
  char inFile[255], outFile[255], fileName[255];
  char projection[255];
  char values[255], prefix[30], suffix[30];
  const int pid = getpid();
  int i;

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
  else {
    cfg = read_convert_config(configFileName);
  }

  // Batch mode processing
  if (strcmp(cfg->general->batchFile, "") != 0) {
    fBatch = FOPEN(cfg->general->batchFile, "r");
    while (fgets(line, 255, fBatch) != NULL)
    {
      sscanf(line, "%s", fileName);

      // Create temporary configuration file
      sprintf(batchConfig, "%s.config", fileName);
      fConfig = FOPEN(batchConfig, "w");
      fprintf(fConfig, "asf_convert temporary configuration file\n\n");
      fprintf(fConfig, "[General]\n");
      fprintf(fConfig, "default values = %s\n", cfg->general->defaults);
      fprintf(fConfig, "input file = %s\n", fileName);
      if (strcmp(cfg->general->prefix, "") == 0)
	strcpy(prefix, "");
      else
	sprintf(prefix, "%s_", cfg->general->prefix);
      if (strcmp(cfg->general->suffix, "") == 0)
	strcpy(suffix, "");
      else
	sprintf(suffix, "_%s", cfg->general->suffix);
      fprintf(fConfig, "output file = %s%s%s\n", prefix, fileName, suffix);
      FCLOSE(fConfig);

      // Run asf_convert for temporary configuration file
      asfPrintStatus("\nProcessing %s ...\n", fileName);
      check_return(call_asf_convert(batchConfig),
                   "Processing image in batch mode (asf_convert).\n");

      // Clean up
      unlink(batchConfig);
    }
  }
  // Regular processing
  else {

    update_status(cfg, "Processing...");

    // Check whether everything in the [Import] block is reasonable
    if (cfg->general->import) {
 
      // Import format: ASF, CEOS or STF
      if (strncmp(uc(cfg->import->format), "ASF", 3) != 0 &&
          strncmp(uc(cfg->import->format), "CEOS", 4) != 0 &&
          strncmp(uc(cfg->import->format), "STF", 3) != 0) {
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
      if (strcmp(cfg->import->lut, "") != 0) {
        if (!fileExists(cfg->import->lut)) {
          asfPrintError("Look up table file does not exist\n");
        }
      }

      // Precision state vector file check can only be done
      // from within asf_import

      // Assign appropriate file name
      if (strncmp(uc(cfg->import->format), "ASF", 3) == 0) {
        create_name(inFile, cfg->general->in_name, ".img");
        cfg->general->import = 0; // does not need import
      }
      else if (strcmp(uc(cfg->import->format), "CEOS") == 0) {
        create_name(inFile, cfg->general->in_name, ".D");
        if (!fileExists(inFile)) {
            create_name(inFile, cfg->general->in_name, ".RAW");
        }
      }
      else if (strcmp(uc(cfg->import->format), "STF") == 0)
	strcpy(inFile, cfg->general->in_name);

      // Data file existence check
      if (!fileExists(inFile))
          asfPrintError("Import data file '%s' does not exist!\n", inFile);
    }

    // Check whether everything in the [SAR processing] block is reasonable
    if (cfg->general->sar_processing) {
      
      // Radiometry
      if (strncmp(uc(cfg->sar_processing->radiometry), "AMPLITUDE_IMAGE", 15) != 0 &&
          strncmp(uc(cfg->sar_processing->radiometry), "POWER_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->sar_processing->radiometry), "SIGMA_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->sar_processing->radiometry), "GAMMA_IMAGE", 11) != 0 &&
          strncmp(uc(cfg->sar_processing->radiometry), "BETA_IMAGE", 10) != 0) {
        asfPrintError("Chosen radiometry not supported\n");
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
      if (!fileExists(cfg->terrain_correct->dem)) {
        asfPrintError("Reference DEM file '%s' does not exist\n",
		      cfg->terrain_correct->dem);
      }

      // Check for pixel size smaller than threshold ???

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
      if (strncmp(uc(cfg->geocoding->resampling), "NEAREST NEIGHBOR", 16) != 0 &&
          strncmp(uc(cfg->geocoding->resampling), "BILINEAR", 8) != 0 &&
          strncmp(uc(cfg->geocoding->resampling), "BICUBIC", 7) != 0) {
        asfPrintError("Chosen resampling method not supported\n");
      }
    }

    // Check whether everything in the [Export] block is reasonable
    if (cfg->general->export) {

      // Export format: ASF, TIFF, GEOTIFF, JPEG, PPM
      if (strncmp(uc(cfg->export->format), "ASF", 3) != 0 &&
          strncmp(uc(cfg->export->format), "TIFF", 4) != 0 &&
          strncmp(uc(cfg->export->format), "GEOTIFF", 7) != 0 &&
          strncmp(uc(cfg->export->format), "JPEG", 4) != 0 &&
          strncmp(uc(cfg->export->format), "PPM", 3) != 0) {
        asfPrintError("Chosen export format not supported\n");
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
          asfPrintError("Chosen scaling method not supported\n");
        }
      }
    }

    if (!cfg->general->import && !cfg->general->sar_processing &&
	!cfg->general->terrain_correct && !cfg->general->geocoding && 
	!cfg->general->export) {
      asfPrintError("Nothing to be done\n");
    }

    // Let's finally get to work
    sprintf(outFile, "%s", cfg->general->in_name);
    if (strcmp(cfg->general->out_name, "") == 0) {
      sprintf(cfg->general->out_name, "%s", cfg->general->in_name);
    }

    if (cfg->general->import) {

      update_status(cfg, "Importing...");

      int flags[NUM_IMPORT_FLAGS];
      for (i = 0; i < NUM_IMPORT_FLAGS; ++i)
	flags[i] = FLAG_NOT_SET;

      // Radiometry
      if (!cfg->general->sar_processing) {
          if (strncmp(uc(cfg->import->radiometry), "AMPLITUDE_IMAGE", 15) == 0) {
              flags[f_AMP] = FLAG_SET;
          } else if (strncmp(uc(cfg->import->radiometry), "POWER_IMAGE", 11) == 0) {
              flags[f_POWER] = FLAG_SET;
          } else if (strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) == 0) {
              flags[f_SIGMA] = FLAG_SET;
          } else if (strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) == 0) {
              flags[f_GAMMA] = FLAG_SET;
          } else if (strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) == 0) {
              flags[f_BETA] = FLAG_SET;
          }
      }

      // LUT
      if (strlen(cfg->import->lut) > 0)
	flags[f_LUT] = FLAG_SET;

      if (cfg->import->output_db)
        flags[f_DB] = FLAG_SET;

      // Generate a temporary output filename
      if (cfg->general->image_stats || cfg->general->detect_cr ||
	  cfg->general->sar_processing || cfg->general->terrain_correct ||
	  cfg->general->geocoding || cfg->general->export) {
        sprintf(outFile, "tmp%i_import", pid);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }

      // Call asf_import!
      check_return(asf_import(flags, uc(cfg->import->format),
			      cfg->import->lut, cfg->import->prc,
			      cfg->import->lat_begin, cfg->import->lat_end,
			      1.0, 1.0, 0.0, /* FIXME: Should be in cfg file */
			      cfg->general->in_name, outFile),
		   "ingesting data file (asf_import)\n");
    }

    if (cfg->general->sar_processing) {

      update_status(cfg, "Running ArDop...");

      // Check whether the input file is a raw image.
      // If not, skip the SAR processing step
      meta = meta_read(outFile);
      if (meta->general->image_data_type == RAW_IMAGE) {

          sprintf(inFile, "%s", outFile);
          if (cfg->general->image_stats || cfg->general->detect_cr ||
              cfg->general->sar_processing || cfg->general->terrain_correct ||
              cfg->general->geocoding || cfg->general->export) {
              sprintf(outFile, "tmp%i_sar_processing", pid);
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

	if (strcmp(cfg->sar_processing->radiometry, "AMPLITUDE_IMAGE") == 0)
	  sprintf(outFile, "tmp%i_sar_processing_amp", pid);
	else if (strcmp(cfg->sar_processing->radiometry, "POWER_IMAGE") == 0)
	  sprintf(outFile, "tmp%i_sar_processing_power", pid);
	else if (strcmp(cfg->sar_processing->radiometry, "SIGMA_IMAGE") == 0)
	  sprintf(outFile, "tmp%i_sar_processing_sigma", pid);
	else if (strcmp(cfg->sar_processing->radiometry, "GAMMA_IMAGE") == 0)
	  sprintf(outFile, "tmp%i_sar_processing_gamma", pid);
	else if (strcmp(cfg->sar_processing->radiometry, "BETA_IMAGE") == 0)
	  sprintf(outFile, "tmp%i_sar_processing_beta", pid);
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
    
    if (cfg->general->image_stats) {

      update_status(cfg, "Running Image Stats...");

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
      if (cfg->general->geocoding || cfg->general->export) {
        sprintf(outFile, "tmp%i_image_stats", pid);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }
      check_return(image_stats(inFile, outFile, values, cfg->image_stats->bins,
                               cfg->image_stats->interval),
                   "running statistics on data file (image_stats)\n");
    }

    if (cfg->general->detect_cr) {

      update_status(cfg, "Detecting Corner Reflectors...");

      // Intermediate results
      if (cfg->general->intermediates) {
	cfg->detect_cr->chips = 1;
	cfg->detect_cr->text = 1;
      }

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      if (cfg->general->geocoding || cfg->general->export) {
	sprintf(outFile, "tmp%i_detect_cr", pid);
      }
      else {
	sprintf(outFile, "%s", cfg->general->out_name);
      }
      check_return(detect_cr(inFile, cfg->detect_cr->cr_location, outFile,
			     cfg->detect_cr->chips, cfg->detect_cr->text),
		   "detecting corner reflectors (detect_cr)\n");
    }

    if (cfg->general->terrain_correct) {

      // Generate filenames
      sprintf(inFile, "%s", outFile);
      sprintf(outFile, "tmp%i_terrain_correct", pid);

      // Call asf_terrcorr!  Or refine_geolocation!
      if (cfg->terrain_correct->refine_geolocation_only) {
          update_status(cfg, "Refining Geolocation...");
          check_return(
			  refine_geolocation(inFile, cfg->terrain_correct->dem,"Mask.img",
                                 outFile, FALSE),
              "refining geolocation of the data file (refine_geolocation)\n");
      }
      else {
          update_status(cfg, "Terrain Correcting...");
          check_return(
			  asf_terrcorr_ext(inFile, cfg->terrain_correct->dem,"Mask.img", outFile, 
                               cfg->terrain_correct->pixel,
                               !cfg->general->intermediates,
                               TRUE, FALSE, cfg->terrain_correct->interp, 
                               TRUE, 20, TRUE,1),
              "terrain correcting data file (asf_terrcorr)\n");
      }
    }

    if (cfg->general->geocoding) {

      update_status(cfg, "Geocoding...");
      int force_flag = cfg->geocoding->force;
      resample_method_t resample_method = RESAMPLE_BILINEAR;
      double average_height = cfg->geocoding->height;
      datum_type_t datum = WGS84_DATUM;
      double pixel_size = cfg->geocoding->pixel;
      float background_val = cfg->geocoding->background;

      // Projection
      sprintf(projection, "-read-proj-file %s", cfg->geocoding->projection);

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
        sprintf(outFile, "tmp%i_geocoding", pid);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }

      check_return(asf_geocode_from_proj_file(cfg->geocoding->projection,
					      force_flag, resample_method,
					      average_height, datum,
					      pixel_size, inFile, outFile,
                                              background_val),
                   "geocoding data file (asf_geocode)\n");

      // Move the .meta file to be ready for export
      copy_meta(outFile, cfg->general->in_name);
    }

    if (cfg->general->export) {

      update_status(cfg, "Exporting...");

      output_format_t format = JPEG;
      long size = -1;
      scale_t scale = SIGMA;

      // Format
      if (strncmp(uc(cfg->export->format), "TIFF", 4) == 0) {
	format = TIF;
      } else if (strncmp(uc(cfg->export->format), "GEOTIFF", 7) == 0) {
	format = GEOTIFF;
      } else if (strncmp(uc(cfg->export->format), "JPEG", 4) == 0) {
	format = JPEG;
      } else if (strncmp(uc(cfg->export->format), "PPM", 3) == 0) {
	format = PPM;
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

      check_return(asf_export(format, size, scale, inFile, outFile),
                   "exporting data file (asf_export)\n");

      // Move the .meta file out of temporary status: <out basename>.meta
      copy_meta(inFile, outFile);
    }

    if (!cfg->general->intermediates) {
      sprintf(cmd, "rm -f tmp%i*", pid);
      asfSystem(cmd);
    }
  }
  return(EXIT_SUCCESS);
}
