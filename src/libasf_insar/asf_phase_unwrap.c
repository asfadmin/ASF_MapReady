#include "asf.h"
#include "asf_meta.h"
#include "asf_insar.h"
#include "asf_raster.h"
#include "asf_export.h"

int dem2phase(char *demFile, char *baseFile, char *phaseFile)
{
  int x, y, start_sample, start_line, line_count, sample_count;
  double k, *phase2elevBase, *sinFlat, *cosFlat, xScale, yScale;
  baseline base;
  meta_parameters *meta;
  FILE *fpDem, *fpPhase;
  float *phase,*dem;
  
  meta = meta_read(demFile);
  start_sample = meta->general->start_sample;
  start_line = meta->general->start_line;
  xScale = meta->sar->sample_increment;
  yScale = meta->sar->line_increment;
  line_count = meta->general->line_count;
  sample_count = meta->general->sample_count;
  
  meta_write(meta, phaseFile);
  
  // Allocate some memory
  phase = (float *)MALLOC(sizeof(float)*sample_count);
  dem =(float *)MALLOC(sizeof(float)*sample_count);
 
  // Get wavenumber
  k = meta_get_k(meta);
  
  // Read in baseline values
  base = read_baseline(baseFile);

  // Open files  
  fpDem = fopenImage(demFile, "rb");
  fpPhase = fopenImage(phaseFile,"wb");
  
  /* calculate the sine of the incidence angle across cols*/
  sinFlat = (double *)MALLOC(sizeof(double)*sample_count);
  cosFlat = (double *)MALLOC(sizeof(double)*sample_count);
  phase2elevBase = (double *)MALLOC(sizeof(double)*sample_count);
  for (x=0; x<sample_count; x++) {
    int img_x = x*xScale + start_sample;
    double incid = meta_incid(meta, 0.0, (float)img_x);
    double flat = meta_flat(meta, 0.0, (float)img_x);
    sinFlat[x] = sin(flat);
    cosFlat[x] = cos(flat);
    phase2elevBase[x] = 
      meta_get_slant(meta, 0.0, (float)img_x) * sin(incid)/(2.0*k);
  }
  
  
  // Loop through each row and calculate height
  for (y=0;y<line_count;y++) {
    double Bn_y, Bp_y;
    
    // Read in data 
    get_float_line(fpDem, meta, y, dem);
    
    // Calculate baseline for this row
    meta_interp_baseline(meta, base, y*(int)yScale+start_line, &Bn_y, &Bp_y);
    
    // Step through each pixel in row
    for (x=0; x<sample_count; x++)
      phase[x] = dem[x]/phase2elevBase[x]*(-Bp_y*sinFlat[x]-Bn_y*cosFlat[x]);
    put_float_line(fpPhase, meta, y, phase);
    asfLineMeter(y, line_count);
  }
  asfPrintStatus("Wrote %d lines of simulated phase data.\n\n", line_count);
  
  // Clean up
  FREE(phase);
  FREE(dem);
  FCLOSE(fpPhase);
  FCLOSE(fpDem);

  return(0);
}

int asf_phase_unwrap(char *algorithm, char *interferogram, char *metaFile, 
		     char *demFile, char *baseline, double filter_strength, 
		     int flattening, char *mask, char *unwrapped_phase)
{
  char tmp[255];

  // Simulated phase image from DEM
  check_return(dem2phase(demFile, baseline, "dem_phase.img"),
	       "creating simulated phase (dem2phase)");

  // Deramping and multilooking interferogram
  check_return(deramp(interferogram, baseline, "igramd", 0),
	       "deramping interferogram (deramp)");
  check_return(multilook("igramd", "ml", "a_cpx.meta", NULL),
	       "multilooking interferogram (multilook)");

  // Remove known topographic phase
  if (flattening == 1) {
    check_return(raster_calc("ml_dem_phase.img", 
			     "\'(a-b)%6.2831853-3.14159265\' "
			     "ml_phase.img dem_phase.img"),
		 "subtracting terrain induced phase (raster_calc)");
  }
  
  // Get rolling on the phase unwrapping
  if (strncmp(algorithm, "escher", 6)==0) {
    if (flattening == 1) 
      sprintf(tmp, "ml_dem_phase.img");
    else sprintf(tmp, "ml_phase.img");
    if (filter_strength < 0.0 || filter_strength > 3.0) {
      asfPrintWarning("phase filter value out of range - set to value "
		      "of 1.6\n");
      filter_strength = 1.6;
    }
    if (filter_strength > 0.0) {
      check_return(phase_filter(tmp, filter_strength, "filtered_phase"),
		   "phase filtering (phase_filter)");
      sprintf(tmp, "filtered_phase");
    }
    if (strcmp(tmp, "ml_phase.img") != 0) {
      check_return(zeroify(tmp, "ml_phase.img", "escher_in_phase.img"),
		   "phase value cosmetics (zeroify)");
      sprintf(tmp, "escher_in_phase.img");
    }
    if (flattening == 1) {
	check_return(escher(tmp,"unwrap_dem"), "phase unwrapping (escher)");
	check_return(raster_calc("unwrap_phase.img", "\'(a+b)*(a/a)*(b/b)\' "
				 "unwrap_dem.img dem_phase.img"),
		     "adding terrain induced phase back (raster_calc)");
    }
    else
      check_return(escher(tmp,"unwrap"), "phase unwrapping (escher)");

    check_return(deramp("unwrap", baseline, "unwrap_nod", 1),
		 "reramping unwrapped phase (deramp)");

    meta_parameters *m2 = meta_read("unwrap_phase.meta");
    m2->general->data_type = BYTE;
    meta_write(m2, "unwrap_dem_mask.meta");
    meta_free(m2);
    
    char inFile[256]; char outFile[256];
    strcpy(inFile, "unwrap_dem_mask");
    strcpy(outFile, "unwrap_mask");
    check_return(asf_export_with_lut(TIF, TRUNCATE, "unwrapping_mask.lut", 
				     inFile, outFile),
		 "colorized phase unwrapping mask (asf_export)");
  }
  else if (strncmp(algorithm, "snaphu", 6)==0) {

    asfPrintError("function still needs to be connected again\n");
    /*
    sprintf(cmd, "make_snaphu_conf %s.phase unwrap.phase", interferogram);
    asfSystem(cmd);
    if (cfg->unwrap->tiles_azimuth == 0 || cfg->unwrap->tiles_range == 0) {
      meta = meta_init("a.meta");
      meta_get_latLon(meta, cfg->coreg->master_offset, 1, 0, &lat1, &lon1);
      meta_get_latLon(meta, cfg->coreg->master_offset, 1, 0, &lat2, &lon1);
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
    */
  }

  return(0);
}
