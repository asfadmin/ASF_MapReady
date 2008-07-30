#include <unistd.h>
#include "asf_sar.h"
#include "asf_raster.h"
#include "asf_complex.h"
#include <assert.h>

typedef struct {
   int line;
   quadPolFloat *buf;

   FILE *fp;
   meta_parameters *meta;
   complexMatrix *r;

   int hh_amp_band, hh_phase_band;
   int hv_amp_band, hv_phase_band;
   int vh_amp_band, vh_phase_band;
   int vv_amp_band, vv_phase_band;
} QuadPolData;

static int find_band(meta_parameters *meta, char *name, int *ok)
{
    char *rad_name = MALLOC(sizeof(char)*(strlen(name)+32));
    const char *rad;
    switch (meta->general->radiometry) {
      case r_AMP:
        rad = "";
        break;
      case r_SIGMA:
        rad = "SIGMA-";
        break;
      case r_BETA:
        rad = "BETA-";
        break;
      case r_GAMMA:
        rad = "GAMMA-";
        break;
      case r_SIGMA_DB:
        rad = "SIGMA_DB-";
        break;
      case r_BETA_DB:
        rad = "BETA_DB-";
        break;
      case r_GAMMA_DB:
        rad = "GAMMA_DB-";
        break;
      case r_POWER:
        rad = "POWER-";
        break;
      default:
        asfPrintWarning("Unexpected radiometry: %d\n",
                        meta->general->radiometry);
        rad = "";
        break;
    }

    sprintf(rad_name, "%s%s", rad, name);

    int band_num = get_band_number(meta->general->bands,
                                   meta->general->band_count, rad_name);

    if (band_num < 0) {
        asfPrintStatus("Band '%s' not found.\n", name);
        *ok = FALSE;
    }

    return band_num;
}

QuadPolData *qpd_new(FILE *fp, meta_parameters *meta)
{
  QuadPolData *qpd = MALLOC(sizeof(QuadPolData));
  qpd->fp = fp;
  qpd->meta = meta;

  int ok = TRUE;
  qpd->hh_amp_band = find_band(meta, "AMP-HH", &ok);
  qpd->hh_phase_band = find_band(meta, "PHASE-HH", &ok);
  qpd->hv_amp_band = find_band(meta, "AMP-HV", &ok);
  qpd->hv_phase_band = find_band(meta, "PHASE-HV", &ok);
  qpd->vh_amp_band = find_band(meta, "AMP-VH", &ok);
  qpd->vh_phase_band = find_band(meta, "PHASE-VH", &ok);
  qpd->vv_amp_band = find_band(meta, "AMP-VV", &ok);
  qpd->vv_phase_band = find_band(meta, "PHASE-VV", &ok);

  if (!ok)
      asfPrintError("Not all required bands found-- "
                    "is this SLC quad-pol data?\n");

  complexFloat re1 = complex_new(1,0);
  complexFloat im1 = complex_new(0,1);
  qpd->r = complex_matrix_new22(re1,im1,im1,re1);

  qpd->buf = CALLOC(meta->general->sample_count, sizeof(quadPolFloat));

  return qpd;
}

void qpd_free(QuadPolData *qpd)
{
  FREE(qpd->buf);
  complex_matrix_free(qpd->r);
  // do not free meta_parameters, or close the file
  FREE(qpd);
}

void qpd_get_line(QuadPolData *qpd, int line)
{
  // load quad pol data into the buffer
  meta_parameters *meta = qpd->meta;
  int ns = meta->general->sample_count;

  float *amp_buf = MALLOC(sizeof(float)*meta->general->sample_count);
  float *phase_buf = MALLOC(sizeof(float)*meta->general->sample_count);
  int k;

  get_band_float_line(qpd->fp, meta, qpd->hh_amp_band, line, amp_buf);
  get_band_float_line(qpd->fp, meta, qpd->hh_phase_band, line, phase_buf);
  for (k=0; k<ns; ++k)
    qpd->buf[k].hh = complex_new_polar(amp_buf[k], phase_buf[k]);

  get_band_float_line(qpd->fp, meta, qpd->hv_amp_band, line, amp_buf);
  get_band_float_line(qpd->fp, meta, qpd->hv_phase_band, line, phase_buf);
  for (k=0; k<ns; ++k)
    qpd->buf[k].hv = complex_new_polar(amp_buf[k], phase_buf[k]);
    
  get_band_float_line(qpd->fp, meta, qpd->vh_amp_band, line, amp_buf);
  get_band_float_line(qpd->fp, meta, qpd->vh_phase_band, line, phase_buf);
  for (k=0; k<ns; ++k)
    qpd->buf[k].vh = complex_new_polar(amp_buf[k], phase_buf[k]);
    
  get_band_float_line(qpd->fp, meta, qpd->vv_amp_band, line, amp_buf);
  get_band_float_line(qpd->fp, meta, qpd->vv_phase_band, line, phase_buf);
  for (k=0; k<ns; ++k)
    qpd->buf[k].vv = complex_new_polar(amp_buf[k], phase_buf[k]);

  qpd->line = line;
}

static double get_omega(QuadPolData *qpd, int line, int samp)
{
  assert(line < qpd->meta->general->line_count);
  assert(samp < qpd->meta->general->sample_count);
  assert(line == qpd->line);

  quadPolFloat *qpf = qpd->buf + samp;

  // This is the "M" matrix
  complexMatrix *m = complex_matrix_new22(qpf->hh, qpf->hv, qpf->vh, qpf->vv);

  // Calculating z = r*M*r, where r=[(1 j)(j 1)]
  complexMatrix *z = complex_matrix_mul3(qpd->r,m,qpd->r);

  // omega = 1/4 arg(z12 * conj(z21))
  float omega = 0.25 * (float)complex_arg(
                      complex_mul(complex_matrix_get(z,0,1),
                                 complex_conj(complex_matrix_get(z,1,0))));

  // omega = 1/4 arg(z21 * conj(z12))
  //float omega = 0.25 * (float)complex_arg(
  //                     complex_mul(complex_matrix_get(z,1,0),
  //                                complex_conj(complex_matrix_get(z,0,1))));

  complex_matrix_free(z);
  complex_matrix_free(m);

  return omega;
}

static complexMatrix *make_cpx_rotation_matrix(double ang)
{
  float c = cos(ang);
  float s = sin(ang);
  
  complexFloat cpx_cos = complex_new(c, 0);
  complexFloat cpx_sin = complex_new(s, 0);
  complexFloat cpx_minus_sin = complex_new(-s, 0);
  
  return complex_matrix_new22(cpx_cos, cpx_minus_sin, cpx_sin, cpx_cos);
}

void removeImgAndMeta(const char *f)
{
    char *meta_file = appendExt(f, ".meta");
    char *img_file = appendExt(f, ".img");

    unlink(meta_file);
    unlink(img_file);

    free(meta_file);
    free(img_file);
}

static void do_append(const char *file, const char *append_file,
                      const char *band_kludge)
{
  char *meta_name = appendExt(file, ".meta");
  meta_parameters *meta = meta_read(meta_name);

  // quick & dirty file append -- skip use of get_float_line(), etc
  // since we just want this to be fast
  FILE *rfp = fopenImage(append_file, "rb");
  FILE *afp = fopenImage(file, "ab");

  const int bufsiz = 262144; // 250k buffer
  char buffer[bufsiz]; 
  unsigned int amt;

  do {
    amt = fread(buffer, sizeof(char), bufsiz, rfp);
    if (amt)
      fwrite(buffer, sizeof(char), amt, afp);
  }
  while (amt == bufsiz);

  FCLOSE(rfp);
  FCLOSE(afp);

  // now fix up the metadata of the input
  meta_parameters *apmeta = meta_read(append_file);
  strcat(meta->general->bands, ",");
  strcat(meta->general->bands, apmeta->general->bands);
  strcat(meta->general->bands, band_kludge);
  meta->general->band_count += apmeta->general->band_count;
  meta_free(apmeta);

  meta_write(meta, meta_name);
  meta_free(meta);
}

void faraday_correct(const char *inFile, const char *outFile,
                     int save_intermediates, int use_single_rotation_value)
{
  char *meta_name = appendExt(inFile, ".meta");
  meta_parameters *inMeta = meta_read(meta_name);

  char *in_img_name = appendExt(inFile, ".img");
  char *rot_img_name = appendToBasename(in_img_name, "_farrot");
  char *smoothed_img_name = appendToBasename(in_img_name, "_smooth");
  char *residuals_img_name = appendToBasename(in_img_name, "_residuals");
  char *out_img_name = appendExt(outFile, ".img");

  int nl = inMeta->general->line_count;
  int ns = inMeta->general->sample_count;

  // STEP 1: Calculate the Faraday Rotation angle at each pixel
  //         and generate an output .img
  FILE *fin = fopenImage(in_img_name, "rb");
  QuadPolData *qpd = qpd_new(fin, inMeta);

  float *buf = MALLOC(sizeof(float)*ns);
  meta_parameters *rotMeta = NULL;
  FILE *fout = NULL;

  // "save_rot_img" -- we can skip the faraday rotation debug image in the
  // case where we don't want to save intermediates, and intend to use a
  // single average value
  int save_rot_img = save_intermediates || !use_single_rotation_value;
  if (save_rot_img) {
    // faraday rotation metadata -- only has one band
    char *rot_meta_name = appendExt(rot_img_name, ".meta");
    rotMeta = meta_read(meta_name);

    strcpy(rotMeta->general->bands, "OMEGA");
    rotMeta->general->image_data_type = AMPLITUDE_IMAGE;
    rotMeta->general->band_count = 1;
    meta_write(rotMeta, rot_meta_name);
    free(rot_meta_name);

    // open the rotation image
    fout = fopenImage(rot_img_name, "wb");
  }

  asfPrintStatus("Calculating per-pixel rotation angles...\n");

  // calculate the average rotation angle, even if we are using the
  // smoothed-image method, for informational purposes
  double avg_omega = 0;

  // now loop through the lines/samples of the image, calculating
  // the faraday rotation angle
  int i,j;
  for (i=0; i<nl; ++i) {
    asfLineMeter(i,nl);
    qpd_get_line(qpd,i);

    for (j=0; j<ns; ++j) {
      buf[j] = R2D * get_omega(qpd, i, j);
      avg_omega += buf[j];
    }

    if (save_rot_img)
      put_float_line(fout, rotMeta, i, buf);
  }

  FCLOSE(fin);
  fin = NULL;
  
  if (fout) {
    FCLOSE(fout);
    fout = NULL;
  }
  assert(fout == NULL);

  qpd_free(qpd);
  qpd = NULL;

  avg_omega /= (float)(nl*ns);
  asfPrintStatus("Average Faraday Rotation angle: %.2f degrees.\n", avg_omega);
  avg_omega *= D2R;

  // STEP 2: Smooth the Faraday rotation angle image, if necessary
  if (!use_single_rotation_value) {
    asfPrintStatus("Smoothing rotation angle image...\n");
    smooth(rot_img_name, smoothed_img_name, 599, EDGE_TRUNCATE);
    //smooth(rot_img_name, smoothed_img_name, 29, EDGE_TRUNCATE);
    //smooth(rot_img_name, smoothed_img_name, 5, EDGE_TRUNCATE);
  }

  // STEP 3: Calculate corrected values
  if (save_intermediates)
    asfPrintStatus("Calculating corrected values and residuals...\n");
  else
    asfPrintStatus("Calculating corrected values...\n");

  // final output metadata
  char *out_meta_name = appendExt(outFile, ".meta");
  meta_parameters *outMeta = meta_read(meta_name);

  // anything to update???

  // write out output metadata
  meta_write(outMeta, out_meta_name);

  // Now the data files...
  fin = fopenImage(in_img_name, "rb");
  fout = fopenImage(out_img_name, "wb");
  qpd = qpd_new(fin, inMeta);
  
  // We'll either (1) pull the rotation angle from the smoothed image file,
  // or (2) use the calculated average rotation angle.  If we're in case (1),
  // open the smoothed rotation angle file.
  FILE *fprot = NULL;
  float *rotation_vals = NULL;
  if (!use_single_rotation_value) {
    fprot = fopenImage(smoothed_img_name, "rb");
    rotation_vals = MALLOC(sizeof(float)*ns);
  }

  float *hh_amp = MALLOC(sizeof(float)*ns);
  float *hh_phase = MALLOC(sizeof(float)*ns);
  float *hv_amp = MALLOC(sizeof(float)*ns);
  float *hv_phase = MALLOC(sizeof(float)*ns);
  float *vh_amp = MALLOC(sizeof(float)*ns);
  float *vh_phase = MALLOC(sizeof(float)*ns);
  float *vv_amp = MALLOC(sizeof(float)*ns);
  float *vv_phase = MALLOC(sizeof(float)*ns);
  
  // residuals, if user has asked for them
  float *res = NULL;
  FILE *fpres = NULL;
  meta_parameters *resMeta = NULL;
  if (save_intermediates) {
    res = MALLOC(sizeof(float)*ns);
    fpres = fopenImage(residuals_img_name, "wb");

    // metadata for the residuals file
    char *res_meta_name = appendExt(residuals_img_name, ".meta");
    resMeta = meta_read(meta_name);

    strcpy(resMeta->general->bands, "RESIDUALS");
    resMeta->general->image_data_type = AMPLITUDE_IMAGE;
    resMeta->general->band_count = 1;
    meta_write(resMeta, res_meta_name);
  }
  
  // now iterate through the input image's pixels...
  for (i=0; i<nl; ++i) {
    asfLineMeter(i,nl);
    qpd_get_line(qpd,i);

    if (!use_single_rotation_value)
      get_float_line(fprot, rotMeta, i, rotation_vals);

    for (j=0; j<ns; ++j) {
      double omega;
      if (use_single_rotation_value)
        omega = avg_omega;
      else
        omega = D2R*rotation_vals[j];
      
      quadPolFloat *qpf = qpd->buf + j;

      // This is the "M" matrix
      complexMatrix *m =
          complex_matrix_new22(qpf->hh, qpf->hv, qpf->vh, qpf->vv);

      // rotate by the calculated faraday rotation angle
      // note that make_cpx_rotation_matrix actually makes a fully real matrix
      // but we want to use the cpx matrix multiplication fns
      complexMatrix *rot = make_cpx_rotation_matrix(omega);
      complexMatrix *corr = complex_matrix_mul3(rot,m,rot);
      complex_matrix_free(rot);

      // save corrected values in an array for use with put_float_line()
      complexFloat c = complex_matrix_get(corr, 0, 0);
      hh_amp[j] = complex_amp(c);
      hh_phase[j] = complex_arg(c);

      c = complex_matrix_get(corr, 0, 1);
      hv_amp[j] = complex_amp(c);
      hv_phase[j] = complex_arg(c);

      c = complex_matrix_get(corr, 1, 0);
      vh_amp[j] = complex_amp(c);
      vh_phase[j] = complex_arg(c);

      c = complex_matrix_get(corr, 1, 1);
      vv_amp[j] = complex_amp(c);
      vv_phase[j] = complex_arg(c);

      complex_matrix_free(m);
      complex_matrix_free(corr);
 
      // compute residual
      if (save_intermediates)
        res[j] = omega - get_omega(qpd, i, j);
    }
    
    // write out all 8 bands of the output...
    put_band_float_line(fout, outMeta, qpd->hh_amp_band, i, hh_amp);
    put_band_float_line(fout, outMeta, qpd->hh_phase_band, i, hh_phase);
    put_band_float_line(fout, outMeta, qpd->hv_amp_band, i, hv_amp);
    put_band_float_line(fout, outMeta, qpd->hv_phase_band, i, hv_phase);
    put_band_float_line(fout, outMeta, qpd->vh_amp_band, i, vh_amp);
    put_band_float_line(fout, outMeta, qpd->vh_phase_band, i, vh_phase);
    put_band_float_line(fout, outMeta, qpd->vv_amp_band, i, vv_amp);
    put_band_float_line(fout, outMeta, qpd->vv_phase_band, i, vv_phase);
    
    // write out residuals
    if (save_intermediates)
      put_float_line(fpres, resMeta, i, res);
  }

  FCLOSE(fin);
  FCLOSE(fout);
  if (!use_single_rotation_value)
    FCLOSE(fprot);
  if (save_intermediates)
    FCLOSE(fpres);

  // STEP 4: Clean up
  free(out_meta_name);
  FREE(buf);
  FREE(rotation_vals);

  FREE(hh_amp);
  FREE(hh_phase);
  FREE(hv_amp);
  FREE(hv_phase);
  FREE(vh_amp);
  FREE(vh_phase);
  FREE(vv_amp);
  FREE(vv_phase);
  FREE(res);

  if (save_intermediates) {
    // create a single image will all that the user might be interested in
    // faraday rotation temp file metadata -- can have 1, 2 or 3 bands
    // depending on what options have been selected:

    // use_single_rotation_value     bands in frrot file
    // -------------------------     -------------------
    // 1. No                          OMEGA,OMEGA_SMOOTH,RESIDUALS
    // 2. Yes                         OMEGA,RESIDUALS
    asfPrintStatus("Consolidating intermediates in a single %d-band file...\n",
                   use_single_rotation_value ? 2 : 3);

    if (use_single_rotation_value) {
      // concat the residuals file onto the omega file
      do_append(rot_img_name, residuals_img_name, "");
      removeImgAndMeta(residuals_img_name);
    }
    else {
      // concat the smoothed image & the residuals file onto the omega file
      do_append(rot_img_name, smoothed_img_name, "_SMOOTH");
      removeImgAndMeta(smoothed_img_name);

      do_append(rot_img_name, residuals_img_name, "");
      removeImgAndMeta(residuals_img_name);
    }
  }
  else { //if (!save_intermediates) {
    removeImgAndMeta(rot_img_name);
    if (!use_single_rotation_value)
      removeImgAndMeta(smoothed_img_name);
    // no need to remove residuals file, it is not generated if intermediates
    // are not being saved
  }

  meta_free(rotMeta);
  meta_free(inMeta);
  meta_free(outMeta);
  if (resMeta)
    meta_free(resMeta);

  free(in_img_name);
  free(rot_img_name);
  free(smoothed_img_name);
  free(residuals_img_name);
  free(out_img_name);

  qpd_free(qpd);
}

