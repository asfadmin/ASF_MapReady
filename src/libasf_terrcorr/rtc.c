#include <asf_raster.h>
#include <asf_terrcorr.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "vector.h"

static char *matrix[32] = 
  {"T11","T12_real","T12_imag","T13_real","T13_imag","T14_real","T14_imag",
   "T22","T23_real","T23_imag","T24_real","T24_imag","T33","T34_real",
   "T34_imag","T44","C11","C12_real","C12_imag","C13_real","C13_imag",
   "C14_real","C14_imag","C22","C23_real","C23_imag","C24_real","C24_imag",
   "C33","C34_real","C34_imag","C44"};

static char *decomposition[35] = 
  {"Freeman2_Ground","Freeman2_Vol","Freeman_Dbl","Freeman_Odd","Freeman_Vol",
   "VanZyl3_Dbl","VanZyl3_Odd","VanZyl3_Vol","Yamaguchi3_Dbl","Yamaguchi3_Odd",
   "Yamaguchi3_Vol","Yamaguchi4_Dbl","Yamaguchi4_Hlx","Yamaguchi4_Odd",
   "Yamaguchi4_Vol","Krogager_Kd","Krogager_Kh","Krogager_Ks","TSVM_alpha_s1",
   "TSVM_alpha_s2","TSVM_alpha_s3","TSVM_phi_s1","TSVM_phi_s2","TSVM_phi_s3",
   "TSVM_tau_m1","TSVM_tau_m2","TSVM_tau_m3","TSVM_psi1","TSVM_psi2",
   "TSVM_psi3","TSVM_alpha_s","TSVM_phi_s","TSVM_tau_m","TSVM_psi"};

static int isMatrixElement(char *bandExt)
{
  int ii, found=FALSE;

  for (ii=0; ii<32; ii++) {
    if (strcmp_case(matrix[ii], bandExt) == 0)
      found = TRUE;
  }
  
  return found;
}

static int isDecomposition(char *bandExt)
{
  int ii, found=FALSE;

  for (ii=0; ii<31; ii++) {
    if (strcmp_case(decomposition[ii], bandExt) == 0)
      found = TRUE;
  }
  
  return found;
}

static void geodetic_to_ecef(double lat, double lon, double h, Vector *v)
{
  //const double a = 6378144.0;    // GEM-06 Ellipsoid.
  //const double e = 8.1827385e-2; // GEM-06 Eccentricity
  const double a = 6378137.0000;
  const double e2 = 6.69437999014e-3;
  //const double e2 = e*e;

  lat *= D2R;
  lon *= D2R;

  double sin_lat = sin(lat);
  double cos_lat = cos(lat);

  double f = sqrt(1. - e2*sin_lat*sin_lat);
  double af = a/f;

  v->x = (af + h)*cos_lat*cos(lon);
  v->y = (af + h)*cos_lat*sin(lon);
  v->z = (af*(1.-e2) + h)*sin_lat;
}

static Vector get_satpos(meta_parameters *meta, int line)
{
  int ns = meta->general->sample_count;
  double t = meta_get_time(meta, line, ns/2);

  // find the state vector closest to the specified time
  int ii,closest_ii=0;
  double closest_diff=9999999;
  for (ii=0; ii<meta->state_vectors->vector_count; ++ii) {
    double diff = fabs(meta->state_vectors->vecs[ii].time - t);
    if (diff < closest_diff) {
      closest_ii = ii;
      closest_diff = diff;
    }
  }

  stateVector closest_vec = meta->state_vectors->vecs[closest_ii].vec;
  double closest_time = meta->state_vectors->vecs[closest_ii].time;

  stateVector stVec = propagate(closest_vec, closest_time, t);

  Vector satpos;
  satpos.x = stVec.pos.x;
  satpos.y = stVec.pos.y;
  satpos.z = stVec.pos.z;
  return satpos;
}

static void calculate_vectors_for_line(meta_parameters *meta_dem, meta_parameters *meta_img, int line, FILE *dem_fp, Vector **vectorLine, Vector *nextVectors)
{
  int jj;
  double lat, lon;
  int ns = meta_img->general->sample_count;
  float demLine[ns];

  get_float_line(dem_fp, meta_dem, line, demLine);

  for(jj = 0; jj < ns; ++jj) {
    Vector *v = MALLOC(sizeof(Vector));
    meta_get_latLon(meta_img, line, jj, 0, &lat, &lon);
    geodetic_to_ecef(lat, lon, demLine[jj], v);
    vectorLine[jj] = v;
  }
}

static void push_next_vector_line(Vector ***localVectors, Vector *nextVectors, meta_parameters *meta_dem, meta_parameters *meta_img, FILE *dem_fp, int line)
{
  int ns = meta_img->general->sample_count;
  Vector **vectorsLine = MALLOC(sizeof(Vector*)*ns);
  calculate_vectors_for_line(meta_dem, meta_img, line, dem_fp, vectorsLine, nextVectors);

  int i;
  for(i = 0; i < ns; i++)
    if(localVectors[0] != NULL)
      vector_free(localVectors[0][i]);
  FREE(localVectors[0]);

  localVectors[0] = localVectors[1];
  localVectors[1] = localVectors[2];
  localVectors[2] = vectorsLine;
}

static Vector * calculate_normal(Vector ***localVectors, int sample)
{
  Vector *v1, *v2, *normal;

  v1 = vector_copy(localVectors[0][sample]);
  vector_subtract(v1, localVectors[2][sample]);

  v2 = vector_copy(localVectors[1][sample-1]);
  vector_subtract(v2, localVectors[1][sample+1]);

  normal = vector_cross(v2, v1);
  vector_multiply(normal, 1./vector_magnitude(normal));

  vector_free(v1);
  vector_free(v2);
 /*
  v1 = vector_copy(localVectors[1][sample]);
  v2 = vector_copy(localVectors[1][sample]);
  vector_subtract(v1, localVectors[2][sample]);
  vector_subtract(v2, localVectors[1][sample+1]);
  normal = vector_cross(v1, v2);
  vector_multiply(normal, 1./vector_magnitude(normal));
  vector_free(v1);
  vector_free(v2);
 */
  return normal;
}

static float
calculate_local_incidence(Vector *n, Vector *satpos, Vector *p)
                          
{
  // R: vector from ground point (p) to satellite (satpos)
  Vector *R = vector_copy(satpos);
  vector_subtract(R, p);
  vector_multiply(R, -1./vector_magnitude(R));

  return acos(vector_dot(n,R)) * R2D;
}

static float
calculate_correction(meta_parameters *meta_in, int line, int samp,
                     Vector *satpos, Vector *n, Vector *p, Vector *p_next, float incid_angle)
{

  // R: vector from ground point (p) to satellite (satpos)
  Vector *R = vector_copy(satpos);
  vector_subtract(R, p);
  vector_multiply(R, 1./vector_magnitude(R));

  Vector *x = vector_cross(p,R);
  vector_multiply(x, 1./vector_magnitude(x));

  // Rx: R cross x -- image plane normal
  Vector *Rx = vector_cross(R,x);

  // cos(phi) is the correction factor we need
  double cosphi = vector_dot(Rx,n);
  if (cosphi < 0) cosphi = -cosphi;

  vector_free(x);
  vector_free(R);
  vector_free(Rx);

  // need to remove old correction factor (sin of the incidence angle)
  return cosphi / sin(incid_angle);
}

int rtc(char *input_file, char *dem_file, int maskFlag, char *mask_file,
        char *output_file, int save_incid_angles)
{
  // always save these for now
  save_incid_angles = TRUE;

  //asfPrintStatus("Input file: %s\n", input_file);
  //asfPrintStatus("DEM: %s\n", dem_file);
  //asfPrintStatus("Output file: %s\n", output_file);
  //asfPrintStatus("Layover/shadow mask: %s\n",
  //               maskFlag ? mask_file : "none");
  asfPrintStatus("Save incid angles: %s\n\n", save_incid_angles ? "Yes" : "No");

  char *inputImg = appendExt(input_file, ".img");
  char *inputMeta = appendExt(input_file, ".meta");
  char *demImg = appendExt(dem_file, ".img");
  char *demMeta = appendExt(dem_file, ".meta");
  char *outputImg = appendExt(output_file, ".img");
  char *outputMeta = appendExt(output_file, ".meta");
  char *maskImg = maskFlag ? appendExt(mask_file, ".img") : NULL;
  char *maskMeta = maskFlag ? appendExt(mask_file, ".meta") : NULL;
  char *sideProductsMetaName=NULL;
  meta_parameters *side_meta=NULL;
  FILE *fpSide = NULL;

  if (!fileExists(inputImg))
    asfPrintError("Not found: %s\n", inputImg);
  if (!fileExists(inputMeta))
    asfPrintError("Not found: %s\n", inputMeta);
  if (!fileExists(demImg))
    asfPrintError("Not found: %s\n", demImg);
  if (!fileExists(demMeta))
    asfPrintError("Not found: %s\n", demMeta);
  if (maskFlag) {
    if (!fileExists(maskImg))
      asfPrintError("Not found: %s\n", maskImg);
    if (!fileExists(maskMeta))
      asfPrintError("Not found: %s\n", maskMeta);
  }

  asfPrintStatus("Reading metadata...\n");
  meta_parameters *meta_in = meta_read(inputMeta);
  if (!meta_in) asfPrintError("Failed to read metadata: %s\n", inputMeta);
  meta_parameters *meta_out = meta_copy(meta_in);
  meta_parameters *meta_dem = meta_read(demMeta);
  if (!meta_dem) asfPrintError("Failed to read metadata: %s\n", demMeta);

  if(save_incid_angles) {
    const char *tmpdir = get_asf_tmp_dir();
    char *sideProductsImgName = MALLOC(sizeof(char)*(strlen(tmpdir)+strlen(output_file)+64));
    sprintf(sideProductsImgName, "%s%cterrcorr_side_products.img", 
	    tmpdir, DIR_SEPARATOR);
    sideProductsMetaName = appendExt(sideProductsImgName, ".meta");
    side_meta = meta_copy(meta_in);
    side_meta->general->band_count = 4;
    strcpy(side_meta->general->bands,
           "INCIDENCE_ANGLE_ELLIPSOID,INCIDENCE_ANGLE_LOCAL,RADIOMETRIC_CORRECTION,COS_PHI");
    fpSide = FOPEN(sideProductsImgName, "wb");
    FREE(sideProductsImgName);
  }

  // Check the input radiometry - only accept amplitude
  if (meta_in->general->radiometry != r_AMP)
    asfPrintError("Radiometric correction requires amplitude images!\n");
  char **bands = 
    extract_band_names(meta_in->general->bands, meta_in->general->band_count);

  int ns = meta_in->general->sample_count;
  int nl = meta_in->general->line_count;
  int nb = meta_in->general->band_count;
  int dns = meta_dem->general->sample_count;
  int dnl = meta_dem->general->line_count;
  if (nl != dnl || ns != dns) {
    asfPrintError("Image size (%dx%d LxS) does not match DEM (%dx%d)!\n",
                   nl, ns, dnl, dns);
  }

  Vector **localVectors[3] = { NULL, NULL, NULL };
  Vector nextVectors[ns];

  FILE *fpIn = FOPEN(inputImg, "rb");
  FILE *fpOut = FOPEN(outputImg, "wb");
  FILE *dem_fp = FOPEN(demImg, "rb");

  float corr[ns];
  float incid_angles[ns];
  float bufIn[ns];
  float bufOut[ns];

  asfPrintStatus("Applying radiometric correction...\n");

  int ii, jj, kk;
  for(ii = 1; ii < 3; ++ii) {
    localVectors[ii] = MALLOC(sizeof(Vector**)*ns);
    calculate_vectors_for_line(meta_in, meta_dem, ii - 1, dem_fp, localVectors[ii], nextVectors);
  }

  for (jj=0; jj<ns; ++jj) {
    incid_angles[jj] = 0;
    corr[jj] = 1;
  }

  if(save_incid_angles) {
    put_band_float_line(fpSide, side_meta, 0, 0, incid_angles);
    put_band_float_line(fpSide, side_meta, 0, nl - 1, incid_angles);
    put_band_float_line(fpSide, side_meta, 1, 0, incid_angles);
    put_band_float_line(fpSide, side_meta, 1, nl - 1, incid_angles);
    put_band_float_line(fpSide, side_meta, 2, 0, corr);
    put_band_float_line(fpSide, side_meta, 2, nl - 1, corr);
    // put zeros in for cos(phi) around the edges, not correct though
    put_band_float_line(fpSide, side_meta, 3, 0, incid_angles);
    put_band_float_line(fpSide, side_meta, 3, nl - 1, incid_angles);
  }

  // We aren't applying the correction to the edges of the image
  // (corr[jj] == 1 for the whole row)
  for(kk = 0; kk < nb; ++kk) {
    get_band_float_line(fpIn, meta_in, kk, 0, bufIn);
    if (strstr(bands[kk], "PHASE") != NULL) {
      for (jj=0; jj<ns; ++jj)
	bufOut[jj] = bufIn[jj];
    }
    else if (isMatrixElement(bands[kk]) || isDecomposition(bands[kk])) {
      for (jj=0; jj<ns; ++jj)
	bufOut[jj] = bufIn[jj]*corr[jj];
    }
    else {
      for (jj=0; jj<ns; ++jj)
	bufOut[jj] = 
	  get_rad_cal_dn(meta_in, 0, jj, bands[kk], bufIn[jj], corr[jj]);
    }
    put_band_float_line(fpOut, meta_out, kk, 0, bufOut);
  }

  for(ii = 1; ii < nl - 1; ++ii) {
    push_next_vector_line(localVectors, nextVectors, meta_dem, meta_in, dem_fp,
			  ii + 1);
    corr[0] = corr[ns-1] = 1;
    Vector satpos = get_satpos(meta_in, ii);
    incid_angles[0] = incid_angles[nl-1] = 0;

    // calculate the Ulander correction for this line
    for(jj = 1; jj < ns - 1; ++jj) {
      incid_angles[jj] = meta_incid(meta_in, ii, jj);
      Vector * normal = calculate_normal(localVectors, jj);
      corr[jj] = calculate_correction(meta_in, ii, jj, &satpos, normal, 
				      localVectors[1][jj], &nextVectors[jj], 
				      incid_angles[jj]);
      vector_free(normal);
    }

    // saving some intermediate products if requested
    if(save_incid_angles) {
      float *tmp_buf = MALLOC(sizeof(float)*ns);
      for (jj=0; jj<ns; ++jj)
        tmp_buf[jj] = incid_angles[jj] * R2D;
      put_band_float_line(fpSide, side_meta, 0, ii, tmp_buf);
      put_band_float_line(fpSide, side_meta, 2, ii, corr);
      for (jj=0; jj<ns; ++jj)
        tmp_buf[jj] = corr[jj] * sin(incid_angles[jj]);
      put_band_float_line(fpSide, side_meta, 3, ii, tmp_buf);
      for (jj=1; jj<ns-1; ++jj) {
        Vector * normal = calculate_normal(localVectors, jj);
        tmp_buf[jj] = calculate_local_incidence(normal, &satpos,
				      localVectors[1][jj]);
      }
      tmp_buf[jj] = tmp_buf[jj-1] = 0;
      put_band_float_line(fpSide, side_meta, 1, ii, tmp_buf);
      FREE(tmp_buf);
    }

    // correct all the bands with the calculated scale factor
    for (kk=0; kk<nb; ++kk) {
      get_band_float_line(fpIn, meta_in, kk, ii, bufIn);

      // we never apply the correction to phase
      if (strstr(bands[kk], "PHASE") != NULL) {
        for (jj=0; jj<ns; ++jj)
          bufOut[jj] = bufIn[jj];
      }
      // correct matrix element without applying calibration parameters
      else if (isMatrixElement(bands[kk]) || isDecomposition(bands[kk])) {
	for (jj=0; jj<ns; ++jj)
	  bufOut[jj] = bufIn[jj]*corr[jj];
      }
      // amplitude, or complex I or Q -- apply the radiometric correction
      else {
        for (jj=0; jj<ns; ++jj)
          bufOut[jj] = 
	    get_rad_cal_dn(meta_in, ii, jj, bands[kk], bufIn[jj], corr[jj]);
      }

      // write out the corrected line
      put_band_float_line(fpOut, meta_out, kk, ii, bufOut);
    }

    asfLineMeter(ii+1, nl);
  }

  // bottom line of the image, here we are cheating and reusing the previous
  // line's correction factors
  for(kk = 0; kk < nb; ++kk) {
    get_band_float_line(fpIn, meta_in, kk, nl-1, bufIn);
    if (strstr(bands[kk], "PHASE") != NULL) {
      for (jj=0; jj<ns; ++jj)
	bufOut[jj] = bufIn[jj];
    }
    else if (isMatrixElement(bands[kk]) || isDecomposition(bands[kk])) {
      for (jj=0; jj<ns; ++jj)
	bufOut[jj] = bufIn[jj]*corr[jj];
    }
    else {
      for (jj=0; jj<ns; ++jj)
	bufOut[jj] = 
	  get_rad_cal_dn(meta_in, nl-1, jj, bands[kk], bufIn[jj], corr[jj]);
    }
    put_band_float_line(fpOut, meta_out, kk, nl-1, bufIn);
  }

  for(ii = 0; ii < 3; ++ii) {
    for(jj = 0; jj < ns; ++jj) {
      vector_free(localVectors[ii][jj]);
    }
    FREE(localVectors[ii]);
  }

  FCLOSE(fpOut);
  FCLOSE(fpIn);
  if (fpSide) FCLOSE(fpSide);

  // update output metadata
  for (ii=0; ii<meta_out->general->band_count; ii++) {
    FREE(bands[ii]);
  }
  FREE(bands);
  meta_write(meta_out, outputMeta);

  if (save_incid_angles) {
    meta_write(side_meta, sideProductsMetaName);
    meta_free(side_meta);
    FREE(sideProductsMetaName);
  }

  meta_free(meta_out);
  meta_free(meta_in);
  meta_free(meta_dem);

  FREE(inputImg);
  FREE(inputMeta);
  FREE(demImg);
  FREE(demMeta);
  FREE(outputImg);
  FREE(outputMeta);
  FREE(maskImg);
  FREE(maskMeta);

  return FALSE;
}
