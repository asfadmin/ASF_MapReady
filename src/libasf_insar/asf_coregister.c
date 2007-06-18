#include "asf.h"
#include "ardop_params.h"
#include "asf_insar.h"
#include "functions.h"

int average_in_doppler(char *inFileMaster, char *inFileSlave, char *outFile)
{  
  FILE *fp;
  struct ARDOP_PARAMS ardop_master, ardop_slave;
  float avg_t1, avg_t2, avg_t3;

  // Read .in file into ardop structures 
  read_params(inFileMaster, &ardop_master);
  read_params(inFileSlave, &ardop_slave);
  
  avg_t1 = (ardop_master.fd + ardop_slave.fd) / 2;
  avg_t2 = (ardop_master.fdd + ardop_slave.fdd) / 2;
  avg_t3 = (ardop_master.fddd + ardop_slave.fddd) / 2;
  
  asfPrintStatus(logbuf,"\nAverage Doppler: %e %e %e \n\n", 
		 avg_t1, avg_t2, avg_t3);
  
  // Store result, in the order fd fdd fddd
  fp = FOPEN(outFile, "w");
  fprintf(fp, "%e %e %e", avg_t1, avg_t2, avg_t3);
  FCLOSE(fp); 
  
  return(0);
}


int asf_coregister(int datatype, char *coregType, char *baseName, int deskew,
		   long *p1_master_start, long *p1_slave_start, int p1_patches,
		   long *pL_master_start, long *pL_slave_start, int pL_patches,
		   long master_offset, long slave_offset, int maximum_offset,
		   int *master_patches, int *slave_patches, int *p1_range_offset, 
		   int *p1_azimuth_offset, int *pL_range_offset, 
		   int *pL_azimuth_offset, int *grid, int *fft, int power,
		   char *masterFile, char *slaveFile)
{
  FILE *fp;
  char options[255], tmp[255], masterPatch[255], slavePatch[255];
  char coregSlave[255];
  int delta;

  // level zero data - requires processing first
  if (datatype<2) {
    
    // Calculate average Doppler
    check_return(average_in_doppler(masterFile, slaveFile, "reg/avedop"),
		 "calculating the average Doppler (avg_in_dop)");
    sprintf(tmp, "%s.dop", masterFile);
    fileCopy("reg/avedop", tmp);
    sprintf(tmp, "%s.dop", slaveFile);
    fileCopy("reg/avedop", tmp);
    
    // Coregister with patch method
    if (strncmp(coregType, "PATCH", 5)==0) {

      // Process first master and slave patch
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", masterFile);
      if (deskew == 1) sprintf(options, "-debug 1 -e 1");
      sprintf(masterPatch, "reg/%s_p1", masterFile);
      check_return(ardop(options, *p1_master_start, p1_patches, 
			 masterFile, masterPatch),
		   "processing first patch of master image (ardop)");
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", slaveFile);
      sprintf(slavePatch, "reg/%s_p1", slaveFile);
      check_return(ardop(options, *p1_slave_start, p1_patches, 
			 slaveFile, slavePatch),
		   "processing first patch of slave image (ardop)");
      
      // Determine offset first patch - pixel level
      check_return(coregister_coarse(masterPatch, slavePatch, 
				     "reg/ctrl1", NULL),
		   "offset estimation first patch (coregister_coarse)");
      
      // Determine default start of last patch
      if (*pL_master_start == 0 && *pL_slave_start == 0) {
	*pL_master_start = master_offset - pL_patches*4096;
	*pL_slave_start = slave_offset - pL_patches*4096;
      }
      
      // Process last master and slave patch
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", masterFile);
      if (deskew == 1) sprintf(options, "-debug 1 -e 1");
      sprintf(masterPatch, "reg/%s_pL", masterFile);
      check_return(ardop(options, *pL_master_start, pL_patches, 
			 masterFile, masterPatch),
		   "processing last patch of master image (ardop)");
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", slaveFile);
      sprintf(slavePatch, "reg/%s_pL", slaveFile);
      check_return(ardop(options, *pL_slave_start, pL_patches, 
			 slaveFile, slavePatch),
		   "processing last patch of slave image (ardop)");
      
      // Determine offset last patch - pixel level
      check_return(coregister_coarse(masterPatch, slavePatch, 
				     "reg/ctrlL", NULL),
		   "offset estimation last patch (coregister_coarse)");
      
      // Read offsets determined by coarse coregistration
      fp = FOPEN("reg/ctrl1", "r");
      fscanf(fp, "%d%d", p1_range_offset, p1_azimuth_offset);
      FCLOSE(fp);
      fp = FOPEN("reg/ctrlL", "r");
      fscanf(fp, "%d%d", pL_range_offset, pL_azimuth_offset);
      FCLOSE(fp);

      // Determine whether the measured offset are beyond the given limit
      if ((fabs(*p1_range_offset - *pL_range_offset) > maximum_offset) ||
	  (fabs(*p1_azimuth_offset - *pL_azimuth_offset) > maximum_offset)) {
	asfPrintError("Estimated offset for first and last patch differs more "
		      "than %d pixels\nProcessing terminated to allow manual "
		      "offset estimation\n", maximum_offset);
      }

      // Check whether grid size and FFT flag are set to reasonable values
      if (*grid < 20 || *grid > 200) {
	asfPrintWarning("grid size out of range - set to default value of 20\n");
	*grid = 20;
      }
      if (*fft < 0 || *fft > 1) {
	asfPrintWarning("FFT flag set to invalid value - set to value of 1\n");
	*fft = 1;
      }
      sprintf(tmp, "%s.base.00", baseName);
      fileCopy("base.00", tmp);
      
      // Perform fine coregistration - subpixel level
      sprintf(masterPatch, "reg/%s_p1_cpx", masterFile);
      sprintf(slavePatch, "reg/%s_p1_cpx", slaveFile);
      check_return(coregister_fine(masterPatch, slavePatch, "reg/ctrl1",
				   "reg/fico1", NULL, *grid, *fft),
		   "fine coregistration first patch (coregister_fine)");
      sprintf(masterPatch, "reg/%s_pL_cpx", masterFile);
      sprintf(slavePatch, "reg/%s_pL_cpx", slaveFile);
      check_return(coregister_fine(masterPatch, slavePatch, "reg/ctrlL",
				   "reg/ficoL", NULL, *grid, *fft),
		   "fine coregistration last patch (coregister_fine)");

      // Determine parameters for SAR processing
      check_return(fit_line("reg/fico1", "reg/line1"),
		   "fit regression line first patch (fit_line)");
      check_return(fit_line("reg/ficoL", "reg/lineL"),
		   "fit regression line last patch (fit_line)");
      check_return(calc_deltas("reg/line1", "reg/lineL",
			       *pL_master_start - *p1_master_start, 
			       "reg/deltas"),
		   "conversion of regression coefficients (calc_deltas)");

      // Process master image
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", masterFile);
      if (deskew == 1) sprintf(options, "-debug 1 -e 1");
      if (power == 1) strcat(options, " -power");
      check_return(ardop(options, *p1_master_start, *master_patches, 
			 masterFile, masterFile),
		   "processing master image (ardop)");
      if (power == 1) {
	sprintf(tmp, "%s_a_pwr.img", baseName);;
	fileRename("a_pwr.img", tmp);
	sprintf(tmp, "%s_a_pwr.meta", baseName);
	fileRename("mv a_pwr.meta", tmp);
      }
      sprintf(tmp, "%s.meta", baseName);
      fileCopy("a.meta", tmp);

      // Process slave image
      delta = *p1_master_start - master_offset;
      if(delta != 0) {
	double a, b, c, d;
	double e, f, g, h;
	
	fp = FOPEN("reg/deltas", "r");
	fscanf(fp, "%lf%lf%lf%lf", &a, &b, &c, &d);
	fscanf(fp, "%lf%lf%lf%lf", &e, &f, &g, &h);
	FCLOSE(fp);
	fp = FOPEN("reg/deltas", "w");
	fprintf(fp, "%e %e %e %e\n",
		(a-(delta*e)),(b-(delta*f)),(c-(delta*g)),(d-(delta*h)));
	fprintf(fp, "%e %e %e %e\n", e, f, g, h);
	FCLOSE(fp);
      }
      if (deskew == 0) sprintf(options, "-o reg/deltas -debug 1 -c %s.dop",
			       masterFile);
      if (deskew == 1) sprintf(options, "-o reg/deltas -debug 1 -e 1");
      if (power == 1) strcat(options, " -power");
      sprintf(coregSlave, "%s_corr", slaveFile);
      check_return(ardop(options, *p1_slave_start, *slave_patches, 
			 slaveFile, coregSlave),
		   "processing slave image (ardop)");
      if (power == 1) {
	sprintf(tmp, "%s_b_pwr.img", baseName);
	fileRename("b_corr_pwr.img", tmp);
	sprintf(tmp, "%s_b_pwr.meta", baseName);
	fileRename("b_corr_pwr.meta", tmp);
      }
    }

    // Coregister with FRAME method
    else if (strncmp(coregType, "FRAME", 5)==0) {

      // Process master image
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", masterFile);
      if (deskew == 1) sprintf(options, "-debug 1 -e 1");
      if (power == 1) strcat(options, " -power");
      check_return(ardop(options, *p1_master_start, *master_patches, 
			 masterFile, masterFile),
		   "processing slave image (ardop)");

      // Process slave image
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", masterFile);
      if (deskew == 1) sprintf(options, "-debug 1 -e 1");
      if (power == 1) strcat(options, " -power");
      check_return(ardop(options, *p1_slave_start, *slave_patches, 
			 slaveFile, slaveFile),
		   "processing slave image (ardop)");

      sprintf(masterPatch, "%s_cpx", masterFile);
      sprintf(slavePatch, "%s_cpx", slaveFile);
      check_return(coregister_coarse(masterPatch, slavePatch, "reg/ctrl", NULL),
		   "offset estimation slave image (coregister_coarse)");
      sprintf(tmp, "%s.base.00", baseName);
      fileCopy("base.00", tmp);
      
      // Adjusting to optimize the overlap 
      fp = FOPEN("reg/ctrl", "r");
      fscanf(fp, "%d", &delta);
      fscanf(fp, "%d", &delta);
      FCLOSE(fp);
      if (delta > 0) {
	*master_patches = 
	  (int) ((master_offset-4096-delta)/ARDOP_VALID_PATCH_LENGTH) + 1;
	*slave_patches = *master_patches;
	*p1_master_start = delta;
      }
      else {
	*slave_patches = 
	  (int) ((slave_offset-4096+delta)/ARDOP_VALID_PATCH_LENGTH) + 1;
	*master_patches = *slave_patches;
	*p1_slave_start = -delta;
      }
      check_return(ardop(options, *p1_master_start, *master_patches, 
			 masterFile, masterFile),
		   "processing master image (ardop)");
      check_return(ardop(options, *p1_slave_start, *slave_patches, 
			 slaveFile, slaveFile),
		   "processing slave image (ardop)");
      
      if (power == 1) {
	sprintf(tmp, "%s_a_pwr.img", baseName);
	fileRename("a_pwr.img", tmp);
	sprintf(tmp, "%s_a_pwr.meta", baseName);
	fileRename("a_pwr.meta", tmp);
	sprintf(tmp, "%s_b_pwr.img", baseName);
	fileRename("b_pwr.img", tmp);
	sprintf(tmp, "%s_b_pwr.meta", baseName);
	fileRename("b_pwr.meta", tmp);
      }
      
      sprintf(masterPatch, "%s_cpx", masterFile);
      sprintf(slavePatch, "%s_cpx", slaveFile);
      check_return(coregister_coarse(masterPatch, slavePatch, "reg/ctrl", NULL),
		   "offset estimation slave image (coregister_coarse)");
      check_return(coregister_fine(masterPatch, slavePatch, "reg/ctrl", 
				   "reg/fico", NULL, *grid, *fft),
		   "fine coregistration slave image (coregister_fine)");
      check_return(fit_plane("reg/fico", "reg/matrix", 0.8),
		   "calculate transformation parameters (fit_plane)");
      sprintf(tmp, "-matrix reg/matrix -sameSize");
      sprintf(slavePatch, "%s_cpx.img", slaveFile);
      sprintf(coregSlave, "%s_corr_cpx.img", slaveFile);
      check_return(remap(slavePatch, coregSlave, tmp),
		   "resampling of slave image (remap)");
    }
    else {
      asfPrintError("Coregistration type (%s) not supported!\n", coregType);
    }
  }

  // level one data - can be coregistered right away
  else if (datatype == 2) {

    // Coarse coregistration
    sprintf(masterPatch, "%s_cpx", masterFile);
    sprintf(slavePatch, "%s_cpx", slaveFile);
    check_return(coregister_coarse(masterPatch, slavePatch, "reg/ctrl", NULL),
		 "offset estimation (coregister_coarse)");
    sprintf(tmp, "%s.base.00", baseName);
    fileCopy("base.00", tmp);

    // Fine coregistration
    check_return(coregister_fine(masterPatch, slavePatch, "reg/ctrl", 
				 "reg/fico", NULL, *grid, *fft),
		 "fine coregistration slave image (coregister_fine)");
    
    // Remapping slave image on top of master image
    check_return(fit_plane("reg/fico", "reg/matrix", 0.8),
		 "calculating transformation parameters (fit_plane)");
    sprintf(tmp, "-matrix reg/matrix -sameSize");
    sprintf(slavePatch, "%s_cpx.img", slaveFile);
    sprintf(coregSlave, "%s_corr_cpx.img", slaveFile);
    check_return(remap(slavePatch, coregSlave, tmp),
		 "resampling of slave image (remap)");

    sprintf(tmp, "%s_a_amp.img", baseName);
    fileRename("a_amp.img", tmp);
    sprintf(tmp, "%s_a_amp.meta", baseName);
    fileRename("a_amp.meta", tmp);
    remove("b_amp.img");
    remove(" b_amp.meta");
    
    // Generate an amplitude image for the slave
    sprintf(coregSlave, "%s_corr_cpx", slaveFile);
    sprintf(tmp, "%s_corr", slaveFile);
    check_return(c2p(coregSlave, tmp),
		 "converting complex slave image into phase and amplitude (c2p)");
  }

  else 
    asfPrintError("Deta type (%d) not supported\n");

  return(0);
}
