/* Cal_params:
	Fetches noise and scaling factors from
input CEOS image, and returns them to
calibrate.*/


#include "asf.h"
#include "asf_meta.h"
#include "ceos.h"
#include <assert.h>

/**Harcodings to fix calibration of ASF data***
 --------------------------------------------
   Currently contains:

   double sspswb010_noise_vec[256];
   double sspswb011_noise_vec[256];
   double sspswb013_noise_vec[256];
   double sspswb014_noise_vec[256];
   double sspswb015_noise_vec[256];
   double sspswb016_noise_vec[256]; (identical to 15)

*********************************************/
#include "noise_vectors.h"
#include "matrix.h"

#ifndef PI
# define PI 3.14159265358979323846
#endif
#define GRID 16
#define TABLE 512

#define SQR(X) ((X)*(X))


/**************************************************************************
create_cal_params
Fills in the calibration block in metadata by reading the parameters
given inSAR CEOS file.
**************************************************************************/

void create_cal_params(const char *inSAR, meta_parameters *meta)
{
  int ii, kk;
  struct dataset_sum_rec dssr; // Data set summary record
  double *noise;
  char sarName[512], *facilityStr, *processorStr;
  /*
  cal_params *cal = (cal_params *) MALLOC(sizeof(cal_params));
  cal->asf = NULL;
  cal->asf_scansar = NULL;
  cal->esa = NULL;
  cal->rsat = NULL;
  cal->alos = NULL;
  */
  meta->calibration = meta_calibration_init();
  
  strcpy (sarName, inSAR);
  
  // Check for the varioius processors
  get_dssr(sarName, &dssr);
  facilityStr = trim_spaces(dssr.fac_id);
  processorStr = trim_spaces(dssr.sys_id);

  if (strncmp(facilityStr, "ASF", 3)== 0 &&
      strncmp(processorStr, "FOCUS", 5) != 0 &&
      meta->projection && meta->projection->type == SCANSAR_PROJECTION)
  {
    // ASF internal processor (PP or SSP), ScanSar data
    asf_scansar_cal_params *asf = MALLOC(sizeof(asf_scansar_cal_params));
    meta->calibration->type = asf_scansar_cal;
    meta->calibration->asf_scansar = asf;

    // Get values for calibration coefficients and LUT
    struct VRADDR rdr; // Radiometric data record
    get_raddr(sarName, &rdr);
    
    // hardcodings for not-yet-calibrated fields
    if (rdr.a[0] == -99.0 || rdr.a[1]==0.0 ) {
      asf->a0 = 1.1E4;
      asf->a1 = 2.2E-5;
      asf->a2 = 0.0;
    } 
    else {
      asf->a0 = rdr.a[0];
      asf->a1 = rdr.a[1];
      asf->a2 = rdr.a[2];
    }

    // Set the Noise Correction Vector to correct version
    if (strncmp(dssr.cal_params_file,"SSPSWB010.CALPARMS",18)==0) {
      asfPrintStatus("\n   Substituting hardcoded noise vector sspswb010\n");
      noise = sspswb010_noise_vec;
    } 
    else if (strncmp(dssr.cal_params_file,"SSPSWB011.CALPARMS",18)==0) {
      asfPrintStatus("\n   Substituting hardcoded noise vector sspswb011\n");
      noise = sspswb011_noise_vec;
    } 
    else if (strncmp(dssr.cal_params_file,"SSPSWB013.CALPARMS",18)==0) {
      asfPrintStatus("\n   Substituting hardcoded noise vector sspswb013\n");
      noise = sspswb013_noise_vec;
    } 
    else if (strncmp(dssr.cal_params_file,"SSPSWB014.CALPARMS",18)==0) {
      asfPrintStatus("\n   Substituting hardcoded noise vector sspswb014\n");
      noise = sspswb014_noise_vec;
    } 
    else if (strncmp(dssr.cal_params_file,"SSPSWB015.CALPARMS",18)==0) {
      asfPrintStatus("\n   Substituting hardcoded noise vector sspswb015\n");
      noise = sspswb015_noise_vec;
    } 
    else if (strncmp(dssr.cal_params_file,"SSPSWB016.CALPARMS",18)==0) {
      asfPrintStatus("\n   Substituting hardcoded noise vector sspswb016\n");
      noise = sspswb015_noise_vec;
      // 16 and 15 were identical antenna patterns, only metadata fields were 
      // changed, so the noise vector for 16 is the same and that for 15. JBN
    } 
    else 
      noise = rdr.noise;

    for (kk=0; kk<256; ++kk)
      asf->noise[kk] = noise[kk];

  }
  else if (strncmp(facilityStr, "ASF", 3)== 0 &&
           strncmp(processorStr, "FOCUS", 5) != 0)
  {
    // ASF internal processor (PP or SSP) (non-Scansar)
    asf_cal_params *asf = (asf_cal_params *) MALLOC(sizeof(asf_cal_params));
    meta->calibration->type = asf_cal;
    meta->calibration->asf = asf;

    // Get values for calibration coefficients and LUT
    struct VRADDR rdr; // Radiometric data record
    get_raddr(sarName, &rdr);
    
    // hardcodings for not-yet-calibrated fields
    if (rdr.a[0] == -99.0 || rdr.a[1]==0.0 ) {
      asf->a0 = 1.1E4;
      asf->a1 = 2.2E-5;
      asf->a2 = 0.0;
    } 
    else {
      asf->a0 = rdr.a[0];
      asf->a1 = rdr.a[1];
      asf->a2 = rdr.a[2];
    }

    // grab the noise vector
    for (kk=0; kk<256; ++kk)
      asf->noise[kk] = rdr.noise[kk];
    asf->sample_count = meta->general->sample_count;
  }
  else if ((strncmp(facilityStr, "ASF", 3) == 0 &&
	    strncmp(dssr.sys_id, "FOCUS", 5) == 0) ||
	   (strncmp(facilityStr, "CDPF", 4) == 0 ||
	    strncmp(facilityStr, "RSI", 3) == 0 ||
	    (strncmp(facilityStr, "CSTARS", 6) == 0 && 
	     strncmp(dssr.mission_id, "RSAT", 4) == 0))) {
    // Radarsat style calibration
    rsat_cal_params *rsat = 
      (rsat_cal_params *) MALLOC(sizeof(rsat_cal_params));
    meta->calibration->type = rsat_cal;
    meta->calibration->rsat = rsat;
    rsat->slc = FALSE;
    rsat->focus = FALSE;
    if (strncmp(dssr.product_type, "SLANT RANGE COMPLEX", 19) == 0 ||
	strncmp(dssr.product_type, 
		"SPECIAL PRODUCT(SINGL-LOOK COMP)", 32) == 0) {
      rsat->slc = TRUE;
    }
    if (strncmp(dssr.sys_id, "FOCUS", 5) == 0)
      rsat->focus = TRUE;

    // Read lookup up table from radiometric data record
    struct RSI_VRADDR radr;
    get_rsi_raddr(sarName, &radr);
    rsat->n = radr.n_samp;
    rsat->lut = (double *) MALLOC(sizeof(double) * rsat->n);
    for (ii=0; ii<rsat->n; ii++) {
      if (strncmp(dssr.sys_id, "FOCUS", 5) == 0)
	rsat->lut[ii] = radr.lookup_tab[0];
      else
	rsat->lut[ii] = radr.lookup_tab[ii];
    }
    rsat->samp_inc = radr.samp_inc;
    rsat->a3 = radr.offset;
    
  }
  else if (strncmp(facilityStr, "ES", 2) == 0 ||
	   strncmp(facilityStr, "D-PAF", 5) == 0 ||
	   strncmp(facilityStr, "I-PAF", 2) == 0 ||
	   strncmp(facilityStr, "Beijing", 7) == 0 ||
	   (strncmp(facilityStr, "CSTARS", 6) == 0 &&
	    (strncmp(dssr.mission_id, "E", 1) == 0 ||
	     strncmp(dssr.mission_id, "J", 1) == 0)))
    {
    // ESA style calibration
    esa_cal_params *esa = (esa_cal_params *) MALLOC(sizeof(esa_cal_params));
    meta->calibration->type = esa_cal;
    meta->calibration->esa = esa;

    // Read calibration coefficient and reference incidence angle
    struct ESA_FACDR facdr;
    get_esa_facdr(sarName, &facdr);
    esa->k = facdr.abs_cal_const;
    esa->ref_incid = dssr.incident_ang;
  }
  else if (strncmp(facilityStr, "EOC", 3) == 0) {
    // ALOS processor
    struct alos_rad_data_rec ardr; // ALOS Radiometric Data record
    alos_cal_params *alos = 
      (alos_cal_params *) MALLOC(sizeof(alos_cal_params));
    meta->calibration->type = alos_cal;
    meta->calibration->alos = alos;

    // Reading calibration coefficient
    get_ardr(sarName, &ardr);
    if (strncmp(dssr.lev_code, "1.1", 3) == 0) // SLC
      alos->cf = ardr.calibration_factor - 32;
    else if (strncmp(dssr.lev_code, "1.5", 3) == 0) // regular detected
      alos->cf = ardr.calibration_factor;
  }
  else
    // should never get here
    asfPrintError("Unknown calibration parameter scheme!\n");
    
}

/*----------------------------------------------------------------------
  Get_cal_dn:
        Convert amplitude image data number into calibrated image data
        number (in power scale), given the current noise value.
----------------------------------------------------------------------*/
float get_cal_dn(meta_parameters *meta, int line, int sample, float inDn, 
		 int dbFlag)
{
  double scaledPower, calValue, incidence_angle, invIncAngle;
  int x=line, y=sample;
  quadratic_2d q;
  radiometry_t radiometry = meta->general->radiometry;

  // Calculate according to the calibration data type
  if (meta->calibration->type == asf_cal) { // ASF style data (PP and SSP)

    if (radiometry == r_SIGMA || radiometry == r_SIGMA_DB)
      invIncAngle = 1.0;
    else if (radiometry == r_GAMMA || radiometry == r_GAMMA_DB) {
      incidence_angle = meta_incid(meta, line, sample);
      invIncAngle = 1/cos(incidence_angle);
    }
    else if (radiometry == r_BETA || radiometry == r_BETA_DB) {
      incidence_angle = meta_incid(meta, line, sample);
      invIncAngle = 1/sin(incidence_angle);
    }

    asf_cal_params *p = meta->calibration->asf;
    double index = (double)sample*256./(double)(p->sample_count);
    int base = (int) index;
    double frac = index - base;
    double *noise = p->noise;

    // Determine the noise value

    double noiseValue = noise[base] + frac*(noise[base+1] - noise[base]);

    // Convert (amplitude) data number to scaled, noise-removed power
    scaledPower = 
      (p->a1*(inDn*inDn-p->a0*noiseValue) + p->a2)*invIncAngle;
  }
  else if (meta->calibration->type == asf_scansar_cal) { // ASF style ScanSar

    asf_scansar_cal_params *p = meta->calibration->asf_scansar;

    if (radiometry == r_SIGMA || radiometry == r_SIGMA_DB)
      invIncAngle = 1.0;
    else if (radiometry == r_GAMMA || radiometry == r_GAMMA_DB) {
      incidence_angle = meta_incid(meta, line, sample);
      invIncAngle = 1/cos(incidence_angle);
    }
    else if (radiometry == r_BETA || radiometry == r_BETA_DB) {
      incidence_angle = meta_incid(meta, line, sample);
      invIncAngle = 1/sin(incidence_angle);
    }

    double look = 25.0; // FIXME: hack to get things compiled
    double index = (look-16.3)*10.0;
    double noiseValue;
    double *noise = p->noise;

    if (index <= 0)
      noiseValue = noise[0];
    else if (index >= 255)
      noiseValue = noise[255];
    else {
      // Use linear interpolation on noise array
      int base = (int)index;
      double frac = index - base;

      noiseValue = noise[base] + frac*(noise[base+1] - noise[base]);
    }

    // Convert (amplitude) data number to scaled, noise-removed power
    scaledPower = 
      (p->a1*(inDn*inDn-p->a0*noiseValue) + p->a2)*invIncAngle;
  }
  else if (meta->calibration->type == esa_cal) { // ESA style ERS and JERS data

    if (radiometry == r_GAMMA || radiometry == r_GAMMA_DB)
      invIncAngle = 1/cos(incidence_angle*D2R);

    esa_cal_params *p = meta->calibration->esa;

    if (radiometry == r_BETA || radiometry == r_BETA_DB)
      scaledPower = inDn*inDn/p->k;
    else if (radiometry == r_SIGMA || radiometry == r_SIGMA_DB) {
      incidence_angle = meta_incid(meta, line, sample);
      scaledPower = 
	inDn*inDn/p->k*sin(p->ref_incid*D2R)/sin(incidence_angle);
    }
    if (radiometry == r_GAMMA || radiometry == r_GAMMA_DB) {
      incidence_angle = meta_incid(meta, line, sample);
      scaledPower = 
	inDn*inDn/p->k*sin(p->ref_incid*D2R)/sin(incidence_angle) /
	invIncAngle;
    }

  }
  else if (meta->calibration->type == rsat_cal) { // CDPF style Radarsat data
    
    if (radiometry == r_BETA || radiometry == r_BETA_DB)
      invIncAngle = 1.0;
    if (radiometry == r_SIGMA || radiometry == r_SIGMA_DB) {
      incidence_angle = meta_incid(meta, line, sample);
      invIncAngle = 1/tan(incidence_angle);
    }
    else if (radiometry == r_GAMMA || radiometry == r_GAMMA_DB) {
      incidence_angle = meta_incid(meta, line, sample);
      invIncAngle = tan(incidence_angle);
    }

    rsat_cal_params *p = meta->calibration->rsat;
    double a2;
    if (meta->calibration->rsat->focus)
      a2 = p->lut[0];
    else if (sample < (p->samp_inc*(p->n-1))) {
      int i_low = sample/p->samp_inc;
      int i_up = i_low + 1;
      a2 = p->lut[i_low] +
	((p->lut[i_up] - p->lut[i_low])*((sample/p->samp_inc) - i_low));
    }
    else 
      a2 = p->lut[p->n-1] +
	((p->lut[p->n-1] - p->lut[p->n-2])*((sample/p->samp_inc) - p->n-1));
    if (p->slc)
      scaledPower = (inDn*inDn)/(a2*a2)*invIncAngle;
    else
      scaledPower = (inDn*inDn + p->a3)/a2*invIncAngle;
  }
  else if (meta->calibration->type == alos_cal) { // ALOS data
    
    if (radiometry == r_SIGMA || radiometry == r_SIGMA_DB)
      invIncAngle = 1.0;
    else if (radiometry == r_GAMMA || radiometry == r_GAMMA_DB) {
      incidence_angle = meta_incid(meta, line, sample);
      invIncAngle = 1/cos(incidence_angle);
    }
    else if (radiometry == r_BETA || radiometry == r_BETA_DB) {
      incidence_angle = meta_incid(meta, line, sample);
      invIncAngle = 1/sin(incidence_angle);
    }

    alos_cal_params *p = meta->calibration->alos;

    scaledPower = pow(10, p->cf/10.0)*inDn*inDn*invIncAngle;
  }
  else
    // should never get here
    asfPrintError("Unknown calibration data type!\n");
  
  // We don't want to convert the scaled power image into dB values
  // since it messes up the statistics
  // We set all values lower than the noise floor (0.001 is the equivalent
  // to -30 dB) to the mininum value of 0.001, removing outliers.
  if (scaledPower > 0.001 && inDn > 0.0) {
    if (dbFlag)
      calValue = 10.0 * log10(scaledPower);
    else
      calValue = scaledPower;
  }
  else {
    if (dbFlag)
      calValue = -30.0;
    else
      calValue = 0.001;
  }

  return calValue;
}
