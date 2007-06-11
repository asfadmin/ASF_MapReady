#include <asf.h>
#include <lzFetch.h>
#include <unistd.h>
#include <sys/stat.h>
#include "ips.h"
#include "functions.h"
#include "asf_insar.h"

typedef struct
{
	double Bn,dBn;
	double Bp,dBp;
} baseLine;

static baseLine readBaseLine(char *fname)
{
	baseLine base;
	FILE *fp1;
	fp1 = FOPEN(fname,"r");
	if (4!=fscanf(fp1,"%lf%lf%lf%lf",&base.Bn,&base.dBn,&base.Bp,&base.dBp))
		{ printf("Unable to read baseline from file '%s'\n",fname); exit(1); }
	fclose(fp1);
	return base;
}

static double baseLineDiff(const baseLine *a,const baseLine *b)
{
	return fabs(a->Bn-b->Bn)+
		fabs(a->dBn-b->dBn)+
		fabs(a->Bp-b->Bp)+
		fabs(a->dBp-b->dBp);
}

static int check_refinement(char *base1, char *base2, char *base3)
{
    baseLine b1,b2,b3;
    b1=readBaseLine(base1);
    if (base2!=NULL)
    	b2=readBaseLine(base2);
    if (base3!=NULL)
    	b3=readBaseLine(base3);

    /*If the baseline's changed by less than 1 mm,*/
    if (base2!=NULL)
    	if (baseLineDiff(&b1,&b2) < 0.001)
    		return(1); /*we've sucessfully converged.*/

    /*If the baseline's changed by less than 1 mm,*/
    if (base3!=NULL)
    	if (baseLineDiff(&b1,&b3) < 0.001)
		return(1); /*we're cyclic, but (pretty close to) converged anyway.*/
    return(0);
}


static void check_return(int ret, char *msg)
{
  if (ret!=0) {
    asfPrintError("%s\n", msg);
  }
}


static int check_status(char *status)
{
  if (strncmp(status, "new", 3)==0) return 1;
  else if (strncmp(status, "stop", 4)==0) {
    printf("   Processing interrupted by user!\n\n");
    exit(0);
  }
  else return 0;
}


static char *base2str(int baseNo, char *base)
{
  char *baseStr=(char *)MALLOC(sizeof(char)*50);

  if (baseNo<10) sprintf(baseStr,"%s.base.0%i", base, baseNo);
  else sprintf(baseStr,"%s.base.%i", base, baseNo);

  return baseStr;
}



int ips(dem_config *cfg, char *configFile, int createFlag)
{
  meta_parameters *meta=NULL;
  FILE *fCoh;
  char cmd[255], path[255], data[255], metadata[255], tmp[255];
  char format[255], metaFile[25];
  char *veryoldBase=NULL, *oldBase=NULL, *newBase=NULL;
  int i, datatype=0;
  float avg;
  double lat1=0.0, lat2=0.0, lon1;
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
  sprintf(logbuf, "   Data type: %s\n   Processing mode: %s\n",
	  cfg->general->data_type, cfg->general->mode);
  printLog(logbuf);
  FCLOSE(fLog);

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
      lzInt(metaFile, "sar.look_count:", NULL);
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

    check_return(asf_igram_coh(cfg->igram_coh->looks*3, 3, 
			       cfg->igram_coh->looks, 1, 
			       "a_cpx.img", "b_corr_cpx.img", cfg->general->base),
		 "interferogram/coherence image generation(asf_igram_coh)");

    fCoh = FOPEN(logFile, "r");
    if (fCoh) {
      while (fgets(tmp, 255, fCoh) != NULL) {
	if (strncmp(tmp, "   Average Coherence:", 21)==0)
	  sscanf(tmp, "%s %s %f", tmp, tmp, &avg);
      }
      if (avg < cfg->igram_coh->min) {
	sprintf(tmp, "average coherence level below minimum of %.1f",
		cfg->igram_coh->min);
	check_return(1, tmp);
      }
      FCLOSE(fCoh);
    }

    sprintf(cfg->igram_coh->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Refine the offset
  if (check_status(cfg->offset_match->status)) {

    sprintf(tmp, "%s_ml_amp.img", cfg->igram_coh->igram);
    check_return(asf_check_geolocation(tmp, cfg->general->dem, NULL,
				       "dem_sim.img", "dem_slant.img"),
		 "refining the geolocation of the SAR image");

    sprintf(cfg->offset_match->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Simulated phase image and seed points
  if (check_status(cfg->sim_phase->status)) {
    check_return(dem2phase("dem_slant.img", "a_cpx.meta",
			   base2str(0, cfg->general->base), "out_dem_phase.img"),
		 "creating simulated phase (dem2phase)");

    sprintf(tmp, "%s_ml_amp.img", cfg->igram_coh->igram);
    check_return(dem2seeds("dem_slant.img", tmp, cfg->sim_phase->seeds, 0),
		 "creating seed points (dem2seeds)");

    sprintf(cfg->sim_phase->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Deramping and multilooking interferogram
  if (check_status(cfg->deramp_ml->status)) {
    check_return(deramp(cfg->igram_coh->igram,
			base2str(0, cfg->general->base), "igramd", 0),
		 "deramping interferogram (deramp)");
    check_return(multilook("igramd", "ml", "a_cpx.meta"),
		 "multilooking interferogram (multilook)");

    sprintf(cfg->deramp_ml->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Calculate differential interferogram
  if (strncmp(cfg->general->mode, "DINSAR", 6)==0) {
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
    if (cfg->unwrap->flattening==1) {
      check_return(raster_calc("ml_dem_phase.img", "\'(a-b)%6.2831853-3.14159265\' "
			       "ml_phase.img out_dem_phase.img"),
		   "subtracting terrain induced phase (raster_calc)");
    }

    if (strncmp(cfg->unwrap->algorithm, "escher", 6)==0) {
      if (cfg->unwrap->flattening==1) sprintf(tmp, "ml_dem_phase.img");
      else sprintf(tmp, "ml_phase.img");
      if (cfg->unwrap->filter < 0.0 || cfg->unwrap->filter > 3.0) {
	printf("\n   WARNING: phase filter value out of range - "
	       "set to value of 1.6\n\n");
	cfg->unwrap->filter = 1.6;
	check_return(write_config(configFile, cfg),
		     "Could not update configuration file");
      }
      if (cfg->unwrap->filter>0.0) {
	check_return(phase_filter(tmp, (double) cfg->unwrap->filter,
				  "filtered_phase"),
		     "phase filtering (phase_filter)");
	sprintf(tmp, "filtered_phase");
      }
      if (strcmp(tmp, "ml_phase.img")!=0) {
	check_return(zeroify(tmp, "ml_phase.img", "escher_in_phase.img"),
		     "phase value cosmetics (zeroify)");
	sprintf(tmp, "escher_in_phase.img");
      }
      if (cfg->unwrap->flattening==1) {
	check_return(escher(tmp,"unwrap_dem"), "phase unwrapping (escher)");
	check_return(raster_calc("unwrap_phase.img", "\'(a+b)*(a/a)*(b/b)\' "
				 "unwrap_dem.img out_dem_phase.img"),
		     "adding terrain induced phase back (raster_calc)");
      }
      else
	check_return(escher(tmp,"unwrap"), "phase unwrapping (escher)");

      check_return(deramp("unwrap", base2str(0, cfg->general->base),
			  "unwrap_nod", 1),
		   "reramping unwrapped phase (deramp)");

      link("unwrap_phase.meta", "unwrap_dem_mask.meta");
      /* FIXME
      check_return(asf_export_bands(JPEG, sample_mapping, 0, 0, 0, 0, 0, 
				    "interferogram.lut", "unwrap_dem_phase", 
				    "unwrap_mask", NULL),
		   "colorized phase unwrapping mask (asf_export)");
      */
    }

    if (strncmp(cfg->unwrap->algorithm, "snaphu", 6)==0) {

      if (cfg->general->test == 1) {
	if (cfg->unwrap->flattening==1) sprintf(tmp, "ml_dem.phase");
	else sprintf(tmp, "ml_phase.img");
	if (cfg->unwrap->filter < 0.0 || cfg->unwrap->filter > 3.0) {
	  printf("\n   WARNING: phase filter value out of range - "
		 "set to value of 1.6\n\n");
	  cfg->unwrap->filter = 1.6;
	  check_return(write_config(configFile, cfg),
		       "Could not update configuration file");
	}
	if (cfg->unwrap->filter>0.0) {
	  check_return(phase_filter(tmp, (double) cfg->unwrap->filter,
				    "filtered_phase"),
		       "phase filtering (phase_filter)");
	  sprintf(tmp, "filtered_phase");
	}
	if (strcmp(tmp, "ml_phase.img")!=0) {
	  check_return(zeroify(tmp, "ml_phase", "escher_in_phase.img"),
		       "phase value cosmetics (zeroify)");
	  sprintf(tmp, "escher_in_phase.img");
	}
	if (cfg->unwrap->flattening==1) {
	  check_return(escher(tmp,"unwrap_dem"), "phase unwrapping (escher)");
	  check_return(raster_calc("escher.phase", "\'(a+b)*(a/a)*(b/b)\' "
				   "unwrap_dem_phase.img out_dem_phase.img"),
		       "adding terrain induced phase back (raster_calc)");
	}
	else
	  check_return(escher(tmp,"escher"), "phase unwrapping (escher)");

	check_return(deramp("escher", base2str(0, cfg->general->base),
			    "escher_nod", 1),
		     "reramping unwrapped phase (deramp)");

	link("escher.meta", "unwrap_dem_mask.meta");
	/* FIXME
	check_return(asf_export_bands(JPEG, sample_mapping, 0, 0, 0, 0, 0, 
				      "interferogram.lut", "unwrap_dem_phase", 
				      "unwrap_mask", NULL),
		     "colorized phase unwrapping mask (asf_export)");
	*/
      }

      //sprintf(cmd, "make_snaphu_conf %s.phase unwrap.phase", 
      //      cfg->igram_coh->igram);
      //system(cmd);
      fLog = FOPEN(logFile, "a");
      sprintf(logbuf, "Command line: %s\n", cmd);
      printf("%s", logbuf);
      printLog(logbuf);
      FCLOSE(fLog);
      if (cfg->unwrap->tiles_azimuth == 0 || cfg->unwrap->tiles_range == 0) {
	meta = meta_init("a.meta");
	/* some more old trim_slc stuff to check!!!

	if (datatype==2) {
	meta_get_latLon(meta, cfg->trim_slc->line, 1, 0, &lat1, &lon);
	meta_get_latLon(meta, cfg->trim_slc->length, 1, 0, &lat2, &lon);
	}
	else {
	*/
	meta_get_latLon(meta, cfg->coreg->master_offset, 1, 0, &lat1, &lon1);
	meta_get_latLon(meta, cfg->coreg->master_offset, 1, 0, &lat2, &lon1);
/*	}*/
	cfg->unwrap->tiles_azimuth =
	  (int) (fabs(lat1-lat2)*cfg->unwrap->tiles_per_degree);
	cfg->unwrap->tiles_range = (int) (fabs(cfg->unwrap->tiles_per_degree*0.8));
      }
      if (cfg->unwrap->flattening==1)
	check_return(snaphu(cfg->unwrap->algorithm, "ml_phase.img", "ml_amp.img",
			    cfg->coreg->master_power, cfg->coreg->slave_power,
			    "snaphu.conf", "unwrap_phase.img",
			    cfg->unwrap->tiles_azimuth, cfg->unwrap->tiles_range,
			    cfg->unwrap->overlap_azimuth, cfg->unwrap->overlap_range,
			    cfg->unwrap->procs, 1), "phase unwrapping (snaphu)");
      else
	check_return(snaphu(cfg->unwrap->algorithm, "ml_phase.img", "ml_amp.img",
			    cfg->coreg->master_power, cfg->coreg->slave_power,
			    "snaphu.conf", "unwrap_phase.img",
			    cfg->unwrap->tiles_azimuth, cfg->unwrap->tiles_range,
			    cfg->unwrap->overlap_azimuth, cfg->unwrap->overlap_range,
			    cfg->unwrap->procs, 0), "phase unwrapping (snaphu)");
      link("ml_phase.meta", "unwrap_phase.meta");
    }

    check_return(deramp("unwrap", base2str(0, cfg->general->base),
			"unwrap_nod", 1),
		 "reramping unwrapped phase (deramp)");

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
    newBase = base2str(0, cfg->general->base);
    for (i=0; i<cfg->refine->max; i++)
      {
	veryoldBase = oldBase;
	oldBase = newBase;
	newBase = base2str(i+1, cfg->general->base);

	check_return(refine_base("unwrap_nod_phase.img",
				 cfg->sim_phase->seeds, oldBase, newBase),
		     "baseline refinement (refine_base)");
	if (i>0)
	  if (check_refinement(newBase,oldBase,veryoldBase)) break;
      }
    if (i==cfg->refine->max)
      check_return(1, "Baseline iterations failed to converge");
    cfg->refine->iter = i+1;

    check_return(deramp(cfg->igram_coh->igram,
			base2str(cfg->refine->iter, cfg->general->base),
			"igramd", 0),
		 "deramping interferogram with refined baseline (deramp)");
    sprintf(tmp, "%s_ml", cfg->igram_coh->igram);
    check_return(multilook("igramd", tmp, "a_cpx.meta"),
		 "multilooking refined interferogram (multilook)");

    sprintf(cfg->refine->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Elevation and elevation error
  if (check_status(cfg->elevation->status)) {
    newBase = base2str(cfg->refine->iter, cfg->general->base);
    check_return(deramp("unwrap_nod", newBase, "unwrap", 0),
		 "deramping unwrapped phase with refined baseline (deramp)");
    // FIXME: library call to raster_diff
    sprintf(cmd, "raster_diff -d %s unwrap_phase.img out_dem_phase.img",
	    cfg->unwrap->qc);
    asfSystem(cmd);

    check_return(asf_elevation("unwrap_phase.img", "unwrap_dem_mask.img",
			       newBase, cfg->sim_phase->seeds,
			       "a_amp.img", cfg->igram_coh->coh,
			       "elevation.img", "elevation_error.img",
			       "ampliutde.img", "coherence_gr.img"),
		 "generating elevation and elevation error (asf_elevation)");;

    /*
    check_return(elev("unwrap_phase.img", newBase, cfg->elevation->dem,
		      cfg->sim_phase->seeds),
		 "creating elevation map (elev)");
    if (strcmp(cfg->unwrap->algorithm, "escher")==0) {
      check_return(eleverr(cfg->igram_coh->coh, newBase,
			   "unwrap_dem_phase.mask" , cfg->elevation->error),
		   "elevation error estimate (eleverr)");
    }
    else
      check_return(eleverr(cfg->igram_coh->coh, newBase, NULL,
			   cfg->elevation->error),
		   "elevation error estimate (eleverr)");

    sprintf(cfg->elevation->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Remapping to ground range
  if (check_status(cfg->ground_range->status)) {
    check_return(deskew_dem(cfg->elevation->dem, "elevation.img", "", 1),
		 "remapping elevation to ground range DEM (deskew_dem)");
    check_return(deskew_dem(cfg->elevation->dem, "amplitude.img",
			    "a_amp.img", 1),
		 "remapping amplitude to ground range (deskew_dem)");
    check_return(deskew_dem(cfg->elevation->dem, "error.img",
			    cfg->elevation->error, 1),
		 "remapping error map to ground range (deskew_dem)");
    check_return(deskew_dem(cfg->elevation->dem, "coh_gr.img", "coh.img", 0),
		 "remapping coherence to ground range (deskew_dem)");
    */

    sprintf(cfg->elevation->status, "success");
    check_return(write_config(configFile, cfg),
		 "Could not update configuration file");
  }

  // Geocoding 
  if (check_status(cfg->geocode->status)) {
    /* Need to create an options string to pass into asf_geocode.
       For UTM projection the center longitude can be passed in.
       For Albers Conic Equal Area, Lambert Conformal Conic and Polar Stereographic
       we can use predefined projection files.*/
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
					    NULL, "error", cfg->geocode->error,
					    background_value),
		 "geocoding ground range error map (asf_geocode)");
    check_return(asf_geocode_from_proj_file(cfg->geocode->proj, geocode_force_flag,
					    resample_method, average_height,
					    datum, cfg->geocode->pixel_spacing,
					    NULL, "coh_gr", cfg->geocode->coh,
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


