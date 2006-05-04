#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "functions.h"
#include "proj.h"
#include "asf_reporting.h"
#include "asf_contact.h"
#include <unistd.h>

void check_return(int ret, char *msg)
{
  if (ret != 0)
    asfPrintError(msg);
}

char *uc(char *string)
{
  char *out=(char *)MALLOC(sizeof(char)*strlen(string));
  int i;

  for (i=0; i<strlen(string); i++) out[i]=toupper(string[i]);
  out[i]='\0';

  return out;
}

int asf_convert(int createflag, char *configFileName)
{
  FILE *fBatch, *fConfig;
  convert_config *cfg;
  meta_parameters *meta;
  char cmd[1024], options[255], line[255], batchConfig[255];
  char inFile[255], outFile[255], fileName[255];
  char format[255], radiometry[255], projection[255], datum[255], resampling[255];
  char scale[255], values[255], prefix[30], suffix[30];
  const int pid = getpid();
  extern int logflag, quietflag;
  int create_f, quiet_f;  /* log_f is a static global */

  // If requested, create a config file and exit (if the file does not exist),
  // otherwise read it
  if ( createflag==TRUE && !fileExists(configFileName) ) {
    init_convert_config(configFileName);
    exit_with_success();
  }
  // Extend the configuration file if the file already exist
  else if ( createflag==TRUE && fileExists(configFileName) ) {
    cfg = read_convert_config(configFileName);
    check_return(write_convert_config(configFileName, cfg), 
		 "Could not update configuration file");
    asfPrintStatus("   Initialized complete configuration file\n\n");
    FCLOSE(fLog);
    exit(EXIT_SUCCESS);
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
      sprintf(cmd, "rm -f %s", batchConfig);
      system(cmd);
    }
  }
  // Regular processing
  else {

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
      else if (strcmp(uc(cfg->import->format), "CEOS") == 0)
        create_name(inFile, cfg->general->in_name, ".D");
      else if (strcmp(uc(cfg->import->format), "STF") == 0)
	strcpy(inFile, cfg->general->in_name);

      // Data file existence check
      if (!fileExists(inFile))
        asfPrintError("Import data file does not exist\n");
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
      if (strncmp(cfg->geocoding->datum, "WGS84", 5) != 0 &&
          strncmp(cfg->geocoding->datum, "NAD27", 5) != 0 &&
          strncmp(cfg->geocoding->datum, "NAD83", 5) != 0) {
        asfPrintError("Chosen datum not supported\n");
      }

      // Resampling
      if (strncmp(cfg->geocoding->resampling, "NEAREST NEIGHBOR", 16) != 0 &&
          strncmp(cfg->geocoding->resampling, "BILINEAR", 8) != 0 &&
          strncmp(cfg->geocoding->resampling, "BICUBIC", 7) != 0) {
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
        if (strncmp(uc(cfg->export->byte), "SIGMA", 5) != 0 &&
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

      // Radiometry
      if (strncmp(uc(cfg->import->radiometry), "AMPLITUDE_IMAGE", 15) == 0) {
        sprintf(radiometry, "-amplitude ");
      } else if (strncmp(uc(cfg->import->radiometry), "POWER_IMAGE", 11) == 0) {
        sprintf(radiometry, "-power ");
      } else if (strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) == 0) {
        sprintf(radiometry, "-sigma ");
      } else if (strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) == 0) {
        sprintf(radiometry, "-gamma ");
      } else if (strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) == 0) {
        sprintf(radiometry, "-beta ");
      }

      // Pass in command line
      if (cfg->general->image_stats || cfg->general->detect_cr ||
	  cfg->general->sar_processing || cfg->general->terrain_correct ||
	  cfg->general->geocoding || cfg->general->export) {
        sprintf(outFile, "tmp%i_import", pid);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }
      check_return(asf_import(cfg->general->in_name, outFile, cfg->import->format,
                              radiometry, NULL, cfg->import->lat_begin,
                              cfg->import->lat_end),
                   "ingesting data file (asf_import)\n");
    }

    if (cfg->general->sar_processing) {

      // Check whether the input file is a raw image. If not, skip the SAR processing step
      meta = meta_read(outFile);
      if (meta->general->image_data_type == RAW_IMAGE) {

	// Radiometry
	if (strncmp(uc(cfg->sar_processing->radiometry), "POWER_IMAGE", 11) == 0) {
	  sprintf(radiometry, "-power");
	} else if (strncmp(uc(cfg->sar_processing->radiometry), "SIGMA_IMAGE", 11) == 0) {
	  sprintf(radiometry, "-sigma");
	} else if (strncmp(uc(cfg->sar_processing->radiometry), "GAMMA_IMAGE", 11) == 0) {
	  sprintf(radiometry, "-gamma");
	} else if (strncmp(uc(cfg->sar_processing->radiometry), "BETA_IMAGE", 10) == 0) {
	  sprintf(radiometry, "-beta");
	} else 
	  sprintf(radiometry, "");
	
	// Pass in command line
	sprintf(inFile, "%s", outFile);
	if (cfg->general->image_stats || cfg->general->detect_cr ||
	    cfg->general->sar_processing || cfg->general->terrain_correct ||
	    cfg->general->geocoding || cfg->general->export) {
	  sprintf(outFile, "tmp%i_sar_processing", pid);
	}
	else {
	  sprintf(outFile, "%s", cfg->general->out_name);
	}
	sprintf(options, "-quiet -log %s %s", logFile, radiometry);
	//	sprintf(options, "-quiet -log %s %s -e 1", logFile, radiometry);
	check_return(ardop(options, inFile, outFile),
		     "SAR processing data file (ardop)\n");
	if (strcmp(radiometry, "") == 0)
	  sprintf(outFile, "tmp%i_sar_processing_amp", pid);
	else if (strcmp(radiometry, "-power") == 0)
	  sprintf(outFile, "tmp%i_sar_processing_power", pid);
	else if (strcmp(radiometry, "-sigma") == 0)
	  sprintf(outFile, "tmp%i_sar_processing_sigma", pid);
	else if (strcmp(radiometry, "-gamma") == 0)
	  sprintf(outFile, "tmp%i_sar_processing_gamma", pid);
	else if (strcmp(radiometry, "-beta") == 0)
	  sprintf(outFile, "tmp%i_sar_processing_beta", pid);
      }
      else {
	meta_free(meta);
	asfPrintStatus("Image has already been processed - skipping SAR processing step");
      }
    }
    
    if (cfg->general->image_stats) {

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
      
      // Pixel size
      if (cfg->terrain_correct->pixel > 0)
	sprintf(options, "-log %s -quiet -pixel-size %.2lf", logFile,
		cfg->terrain_correct->pixel);
      else
	sprintf(options, "-log %s -quiet", logFile);

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      sprintf(outFile, "tmp%i_terrain_correct", pid);
      check_return(asf_terrcorr(options, inFile, cfg->terrain_correct->dem,
				outFile),
		   "terrain correcting data file (asf_terrcorr)\n");
    }

    if (cfg->general->geocoding) {

      // Projection
      sprintf(projection, "-read-proj-file %s", cfg->geocoding->projection);

      // Datum
      sprintf(datum, "-datum %s", uc(cfg->geocoding->datum));

      // Resampling method
      if (strncmp(uc(cfg->geocoding->resampling), "NEAREST_NEIGHBOR", 16) == 0) {
        sprintf(resampling, "-resample-method nearest_neighbor");
      }
      if (strncmp(uc(cfg->geocoding->resampling), "BILINEAR", 8) == 0) {
        sprintf(resampling, "-resample-method bilinear");
      }
      if (strncmp(uc(cfg->geocoding->resampling), "BICUBIC", 7) == 0) {
        sprintf(resampling, "-resample-method bicubic");
      }

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      if (cfg->general->export) {
        sprintf(outFile, "tmp%i_geocoding", pid);
      }
      else {
        sprintf(outFile, "%s", cfg->general->out_name);
      }
      if (cfg->geocoding->force) {
        sprintf(options, "-height %.1lf -pixel-size %.2lf %s %s %s --force",
                cfg->geocoding->height, cfg->geocoding->pixel,
                projection, datum, resampling);
      }
      else {
        sprintf(options, "-height %.1lf -pixel-size %.2lf %s %s %s",
                cfg->geocoding->height, cfg->geocoding->pixel,
                projection, datum, resampling);
      }
      check_return(asf_geocode(options, inFile, outFile),
                   "geocoding data file (asf_geocode)\n");
      sprintf(cmd, "cp %s.meta %s.meta", outFile, cfg->general->in_name);
      system(cmd);
    }

    if (cfg->general->export) {

      // Format
      if (strncmp(uc(cfg->export->format), "TIFF", 4) == 0) {
        sprintf(format, "-format tiff");
      } else if (strncmp(uc(cfg->export->format), "GEOTIFF", 7) == 0) {
        sprintf(format, "-format geotiff");
      } else if (strncmp(uc(cfg->export->format), "JPEG", 4) == 0) {
        sprintf(format, "-format jpeg");
      } else if (strncmp(uc(cfg->export->format), "PPM", 3) == 0) {
        sprintf(format, "-format ppm");
      }

      // Byte scaling
      if (strncmp(uc(cfg->export->byte), "TRUNCATE", 8) == 0) {
        sprintf(scale, "-byte truncate");
      } else if (strncmp(uc(cfg->export->byte), "MINMAX", 6) == 0) {
        sprintf(scale, "-byte minmax");
      } else if (strncmp(uc(cfg->export->byte), "SIGMA", 5) == 0) {
        sprintf(scale, "-byte sigma");
      } else if (strncmp(uc(cfg->export->byte), "HISTOGRAM_EQUALIZE", 18) == 0) {
        sprintf(scale, "-byte histogram_equalize");
      }

      // Pass in command line
      sprintf(inFile, "%s", outFile);
      sprintf(outFile, "%s", cfg->general->out_name);
      sprintf(options, "%s %s", format, scale);
      check_return(asf_export(options, inFile, outFile),
                   "exporting data file (asf_export)\n");
    }

    if (!cfg->general->intermediates) {
      sprintf(cmd, "rm -f tmp%i*", pid);
      system(cmd);
    }
  }

  exit_with_success();
  return 0; // just to quiet a compiler warning; will never get here
}
