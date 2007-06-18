#include <asf.h>
#include <lzFetch.h>
#include <unistd.h>
#include <sys/stat.h>
#include "ips.h"
#include "functions.h"
#include "asf_insar.h"

int ips(dem_config *cfg, char *configFile, int createFlag)
{
  meta_parameters *meta=NULL;
  char cmd[255], path[255], data[255], metadata[255], tmp[255], tmp2[255];
  char format[255], metaFile[25], *newBase=NULL;
  int datatype=0;
  float avg;
  resample_method_t resample_method;
  datum_type_t datum=WGS84_DATUM;
  int geocode_force_flag=1;
  double average_height=0.0;
  float background_value=0;
  output_format_t out_format;
  scale_t sample_mapping=SIGMA;

  // Determine datatype
  if (strncmp(uc(cfg->general->data_type), "STF", 3)==0) {
    datatype = 0;
    sprintf(format, "STF");
  }
  if (strncmp(uc(cfg->general->data_type), "RAW", 3)==0) {
    datatype = 1;
    sprintf(format, "CEOS");
  }
  if (strncmp(uc(cfg->general->data_type), "SLC", 3)==0) {
    datatype = 2;
    sprintf(format, "CEOS");
  }

  if (strncmp(cfg->general->status, "new", 3)==0 && !createFlag) {

    // Link master image
    sscanf(cfg->master->path, "%s", path);
    sscanf(cfg->master->data, "%s", data);
    sscanf(cfg->master->meta, "%s", metadata);

    sprintf(tmp, "%s/%s", path, data);
    if (!fileExists(tmp)) 
      check_return(1, "master image data file does not exist");
    if (datatype==0)
      link(tmp, "master.000");
    else 
      link(tmp, "master.D");
    sprintf(tmp, "%s/%s", path, metadata);
    if (!fileExists(tmp))
      check_return(1, "master image metadata file does not exist");
    if (datatype==0)
      link(tmp, "master.000.par");
    else
      link(tmp, "master.L");

    // Link slave image
    sscanf(cfg->slave->path, "%s", path);
    sscanf(cfg->slave->data, "%s", data);
    sscanf(cfg->slave->meta, "%s", metadata);

    sprintf(tmp, "%s/%s", path, data);
    if (!fileExists(tmp)) 
      check_return(1, "slave image data file does not exist");
    if (datatype==0)
      link(tmp, "slave.000");
    else
      link(tmp, "slave.D");
    sprintf(tmp, "%s/%s", path, metadata);
    if (!fileExists(tmp))
      check_return(1, "slave image metadata file does not exist");
    if (datatype==0)
      link(tmp, "slave.000.par");
    else
      link(tmp, "slave.L");
    create_clean_dir("reg");

    // Verify latitude constraints for STF data
    if (datatype == 0) {
      if (cfg->general->lat_begin < -90.0 || cfg->general->lat_begin > 90.0) {
	cfg->general->lat_begin = -99.0;
	cfg->general->lat_end = 99.0;
	check_return(write_config(configFile, cfg),
		     "Could not update configuration file");
      }
      if (cfg->general->lat_end < -90.0 || cfg->general->lat_end > 90.0) {
	cfg->general->lat_begin = -99.0;
	cfg->general->lat_end = 99.0;
	check_return(write_config(configFile, cfg),
		     "Could not update configuration file");
      }
    }
  }

  // Update configuration file
  if (!createFlag) sprintf(cfg->general->status, "progress");
  check_return(write_config(configFile, cfg), 
	       "Could not update configuration file");

  if (createFlag) {
    printf("   Initialized complete configuration file\n\n");
    if (logflag) {
      fLog = FOPEN(logFile, "a");
      printLog("   Initialized complete configuration file\n\n");
      FCLOSE(fLog);
    }
  }

  // Tell the user what data type and processing mode we found
  printf("   Data type: %s\n   Processing mode: %s\n",
	 cfg->general->data_type, cfg->general->mode);
  fLog = FOPEN(logFile, "a");
  logflag=TRUE;
  sprintf(logbuf, "   Data type: %s\n   Processing mode: %s\n",
	  cfg->general->data_type, cfg->general->mode);
  printLog(logbuf);

  // Ingest the various data types: STF, RAW, or SLC 
  if (check_status(cfg->ingest->status)) {
    
    check_return(asf_import(r_AMP, FALSE, format, NULL, NULL, NULL,
			    cfg->ingest->prc_master,
			    cfg->general->lat_begin, cfg->general->lat_end,
			    NULL, NULL, NULL, NULL, "master", "a"),
		 "ingesting master image (asf_import)");
    check_return(asf_import(r_AMP, FALSE, format, NULL, NULL, NULL,
			    cfg->ingest->prc_slave,
			    cfg->general->lat_begin, cfg->general->lat_end,
			    NULL, NULL, NULL, NULL, "slave", "b"),
		 "ingesting slave image (asf_import)");
    
    // Setting patches and offsets for processing level zero data
    if (datatype < 2) {
      strcat(strcpy(metaFile,"a"),".meta");
      cfg->coreg->master_offset =
	lzInt(metaFile, "sar.original_line_count:", NULL);
      cfg->coreg->master_patches =
	(int) ((cfg->coreg->master_offset-4096)/ARDOP_VALID_PATCH_LENGTH) + 2;
      strcat(strcpy(metaFile,"b"),".meta");
      cfg->coreg->slave_offset =
	lzInt(metaFile, "sar.original_line_count:", NULL);

      if (cfg->coreg->slave_offset > cfg->coreg->master_offset)
	cfg->coreg->slave_offset = cfg->coreg->master_offset;
      else
	cfg->coreg->master_offset = cfg->coreg->slave_offset;

      cfg->coreg->slave_patches = cfg->coreg->master_patches;
    }
    else { // Deal with complex single look complex
      
      check_return(c2p("a_cpx", "a"),
		   "converting complex master image into phase and amplitude (c2p)");
      meta = meta_init("a_cpx.meta");
      check_return(convert2byte("a_amp.img", "a_amp_byte.img", meta->ifm->nLooks, 1),
		   "creating byte amplitude master image (convert2byte)");
      check_return(c2p("b_cpx", "b"),
		   "converting complex slave image into phase and amplitude (c2p)");
      meta = meta_init("b_cpx.meta");
      cfg->igram_coh->looks = meta->sar->look_count;
    }

    sprintf(cfg->ingest->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Coregister master and slave image
  // This will require different strategies. Raw data need to be processed first.
  // Our standard procedure will be the PATCH approach. If that fails (even after
  // user interaction) we fall back to coregistering the entire FRAME.
  if (check_status(cfg->coreg->status)) {

    check_return(asf_coregister(datatype, cfg->general->coreg, 
				cfg->general->base, cfg->general->deskew,
				&cfg->coreg->p1_master_start,
				&cfg->coreg->p1_slave_start,
				cfg->coreg->p1_patches,
				&cfg->coreg->pL_master_start,
				&cfg->coreg->pL_slave_start,
				cfg->coreg->pL_patches,
				cfg->coreg->master_offset, 
				cfg->coreg->slave_offset,
				cfg->general->max_off,
				&cfg->coreg->master_patches,
				&cfg->coreg->slave_patches,
				&cfg->coreg->p1_range_offset,
				&cfg->coreg->p1_azimuth_offset,
				&cfg->coreg->pL_range_offset,
				&cfg->coreg->pL_azimuth_offset,
				&cfg->coreg->grid, &cfg->coreg->fft,
				cfg->coreg->power, "a", "b"),
		 "coregistering master and slave image (asf_coregister)");

    sprintf(cfg->coreg->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Calculate the interferogram and coherence
  if (check_status(cfg->igram_coh->status)) {

    // Check the validity of input parameters
    if (cfg->igram_coh->min < 0.0 || cfg->igram_coh->min > 1.0) {
      printf("\n   WARNING: minimum average coherence out of range - "
	     "set to value of 0.3\n\n");
      cfg->igram_coh->min = 0.3;
      check_return(write_config(configFile, cfg),
		   "Could not update configuration file");
    }

    meta = meta_read("a_amp");
    cfg->igram_coh->looks = meta->sar->look_count;
    meta_free(meta);

    check_return(asf_igram_coh(cfg->igram_coh->looks*3, 3, 
			       cfg->igram_coh->looks, 1, 
			       "a_cpx.img", "b_corr_cpx.img", 
			       cfg->general->base, &avg),
		 "interferogram/coherence image generation(asf_igram_coh)");
 
    if (avg < cfg->igram_coh->min) {
      sprintf(tmp, "average coherence level below minimum of %.1f",
	      cfg->igram_coh->min);
      check_return(1, tmp);
    }

    sprintf(cfg->igram_coh->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Refine the offset
  if (check_status(cfg->offset_match->status)) {

    asfPrintStatus("\nRefining the geolocation ...\n\n");
    sprintf(tmp, "%s_ml_amp.img", cfg->igram_coh->igram);
    check_return(asf_check_geolocation(tmp, cfg->general->dem, NULL,
				       "dem_sim.img", "dem_slant.img"),
		 "refining the geolocation of the SAR image");

    sprintf(cfg->offset_match->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Calculate differential interferogram
  if (strncmp(cfg->general->mode, "DINSAR", 6)==0) {
    check_return(dem2phase("dem_slant.img", base2str(0, cfg->general->base), 
			   "out_dem_phase.img"),
		 "creating simulated phase (dem2phase)");
    sprintf(tmp, "\'(a-b)%%6.2831853-3.14159265\' ml_phase.img "
	    "out_dem_phase.img");
    check_return(raster_calc("dinsar_phase.img", tmp),
		 "calculating differential interferogram (raster_calc)");
    sprintf(cfg->dinsar->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
    exit(0);
  }

  // Phase unwrapping
  if (check_status(cfg->unwrap->status)) {
    if (cfg->unwrap->flattening < 0 || cfg->unwrap->flattening > 1) {
      printf("\n   WARNING: flattening flag set to invalid value - "
	     "set to value of 0\n\n");
      cfg->unwrap->flattening = 0;
      check_return(write_config(configFile, cfg),
		   "Could not update configuration file");
    }

    check_return(asf_phase_unwrap(cfg->unwrap->algorithm, 
				  cfg->igram_coh->igram, "a_cpx.meta",
				  cfg->general->dem, "base.00",
				  cfg->unwrap->filter, cfg->unwrap->flattening,
				  "unwrap_mask", "unwrap"),
		 "unwrapping the phase (asf_phase_unwrap)");

    sprintf(cfg->unwrap->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Baseline refinement
  if (check_status(cfg->refine->status)) {
    if (cfg->refine->max < 1 || cfg->refine->max > 15) {
      printf("\n   WARNING: maximum number of iterations out of range - "
	     "set to value of 15\n\n");
      cfg->refine->max = 15;
      check_return(write_config(configFile, cfg),
		   "Could not update configuration file");
    }

    check_return(asf_baseline(cfg->general->base, cfg->igram_coh->igram, 
			      cfg->refine->seeds, cfg->refine->max,
			      &cfg->refine->iter),
		 "refining the baseline (asf_baseline)");

    sprintf(cfg->refine->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Elevation and elevation error
  if (check_status(cfg->elevation->status)) {

    fLog = FOPEN(logFile, "a");
    logflag = TRUE;

    newBase = base2str(cfg->refine->iter, cfg->general->base);
    check_return(deramp("unwrap_nod", newBase, "unwrap", 0),
		 "deramping unwrapped phase with refined baseline (deramp)");
    // FIXME: library call to raster_diff
    sprintf(cmd, "raster_diff -d %s unwrap_phase.img out_dem_phase.img",
	    cfg->unwrap->qc);
    asfSystem(cmd);
    sprintf(tmp, "a_amp.img");
    sprintf(tmp2, "coherence.img");

    check_return(asf_elevation("unwrap_phase.img", "unwrap_dem_mask.img",
			       newBase, cfg->refine->seeds,
			       cfg->elevation->dem, cfg->elevation->error,
			       tmp, tmp2, "elevation.img", 
			       "elevation_error.img",
			       "amplitude.img", "coherence_gr.img"),
		 "generating elevation and elevation error (asf_elevation)");;

    sprintf(cfg->elevation->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Geocoding 
  if (check_status(cfg->geocode->status)) {

    /* Need to create an options string to pass into asf_geocode.
       For UTM projection the center longitude can be passed in.
       For Albers Conic Equal Area, Lambert Conformal Conic and 
       Polar Stereographic we can use predefined projection files.*/
    if (cfg->geocode->pixel_spacing < 5) {
      printf("\n   WARNING: pixel spacing out of range - "
	     "set to default value of 20\n\n");
      cfg->geocode->pixel_spacing = 20;
      check_return(write_config(configFile, cfg),
		   "Could not update configuration file");
    }
    if (strcmp(cfg->geocode->name, "utm")==0 ||
	strcmp(cfg->geocode->name, "albers")==0 ||
	strcmp(cfg->geocode->name, "lamaz")==0 ||
	strcmp(cfg->geocode->name, "lamcc")==0 ||
	strcmp(cfg->geocode->name, "ps")==0) {
      if (!fileExists(cfg->geocode->proj))
	check_return(1, "projection file does not exist");
    }
    if (strcmp(cfg->geocode->resample, "nearest neighbor") == 0)
      resample_method = RESAMPLE_NEAREST_NEIGHBOR;
    else if (strcmp(cfg->geocode->resample, "bilinear") == 0)
      resample_method = RESAMPLE_BILINEAR;
    else if (strcmp(cfg->geocode->resample, "bicubic") == 0)
      resample_method = RESAMPLE_BICUBIC;
    else
      check_return(1, "unsupported resampling method (asf_geocode)");
    check_return(asf_geocode_from_proj_file(cfg->geocode->proj, geocode_force_flag,
					    resample_method, average_height,
					    datum, cfg->geocode->pixel_spacing,
					    NULL, "elevation", cfg->geocode->dem,
					    background_value),
		 "geocoding ground range DEM (asf_geocode)");
    check_return(asf_geocode_from_proj_file(cfg->geocode->proj, geocode_force_flag,
					    resample_method, average_height,
					    datum, cfg->geocode->pixel_spacing,
					    NULL, "amplitude", cfg->geocode->amp,
					    background_value),
		 "geocoding ground range amplitude (asf_geocode)");
    check_return(asf_geocode_from_proj_file(cfg->geocode->proj, geocode_force_flag,
					    resample_method, average_height,
					    datum, cfg->geocode->pixel_spacing,
					    NULL, "elevation_error", 
					    cfg->geocode->error,
					    background_value),
		 "geocoding ground range error map (asf_geocode)");
    check_return(asf_geocode_from_proj_file(cfg->geocode->proj, geocode_force_flag,
					    resample_method, average_height,
					    datum, cfg->geocode->pixel_spacing,
					    NULL, "coherence_gr", 
					    cfg->geocode->coh,
					    background_value),
		 "geocoding ground range coherence (asf_geocode)");
    
    sprintf(cfg->geocode->status, "success");
  }

  // Exporting
  if (check_status(cfg->export->status)) {

    if (strcmp(uc(cfg->export->format), "TIFF")==0)
      out_format = TIF;
    else if (strcmp(uc(cfg->export->format), "GEOTIFF")==0)
      out_format = GEOTIFF;
    else if (strcmp(uc(cfg->export->format), "JPEG")==0)
      out_format = JPEG;
    else if (strcmp(uc(cfg->export->format), "PGM")==0)
      out_format = PGM;
    else {
      sprintf(tmp, "export format '%s' not supported", cfg->export->format);
      check_return(1, tmp);
    }

    check_return(asf_export(out_format, sample_mapping, cfg->geocode->dem,
			    cfg->geocode->dem),
		 "exporting geocoded DEM (asf_export)");
    check_return(asf_export(out_format, sample_mapping, cfg->geocode->amp,
			    cfg->geocode->amp),
		 "exporting geocoded amplitude image (asf_export)");
    check_return(asf_export(out_format, sample_mapping, cfg->geocode->error,
			    cfg->geocode->error),
		 "exporting geocoded DEM error map (asf_export)");
    check_return(asf_export(out_format, sample_mapping, cfg->geocode->coh,
			    cfg->geocode->coh),
		 "exporting geocoded coherence image (asf_export)");

    sprintf(cfg->export->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  sprintf(cfg->general->status, "success");
  check_return(write_config(configFile, cfg), 
	       "Could not update configuration file");

  return(0);
}


