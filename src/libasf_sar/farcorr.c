#include <unistd.h>
#include "asf_sar.h"
#include "asf_raster.h"
#include "asf_complex.h"
#include <assert.h>

#define MAX_OTHER 10

typedef struct {
   int line;
   quadPolS2Float *buf;

   FILE *fp;
   meta_parameters *meta;
   complexMatrix *r;

   int hh_amp_band, hh_phase_band;
   int hv_amp_band, hv_phase_band;
   int vh_amp_band, vh_phase_band;
   int vv_amp_band, vv_phase_band;

   int other_bands[MAX_OTHER];
} QuadPolData;

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

  qpd->buf = CALLOC(meta->general->sample_count, sizeof(quadPolS2Float));

  // find all the bands that we must "pass through" without changing
  int i;
  for (i=0; i<MAX_OTHER; ++i)
    qpd->other_bands[i] = -1;

  int j=0;
  for (i=0; i<meta->general->band_count; ++i) {

    if (i==qpd->hh_amp_band) continue;
    if (i==qpd->hh_phase_band) continue;
    if (i==qpd->hv_amp_band) continue;
    if (i==qpd->hv_phase_band) continue;
    if (i==qpd->vh_amp_band) continue;
    if (i==qpd->vh_phase_band) continue;
    if (i==qpd->vv_amp_band) continue;
    if (i==qpd->vv_phase_band) continue;

    // this band not one of the quad-pol bands: mark it for passing
    // through unchanged
    qpd->other_bands[j] = i;
    ++j;

    char *band_name = get_band_name(meta->general->bands,
                                    meta->general->band_count, i);
    if (!band_name)
      band_name = STRDUP(MAGIC_UNSET_STRING);

    //asfPrintStatus("Band %d (%s) not a quad-pol band, "
    //               "it will be left unchanged.\n", i, band_name);

    FREE(band_name);
  }

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

  quadPolS2Float *qpf = qpd->buf + samp;

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

  return -omega;
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
    char *img_file = appendExt(f, ".img");
    char *meta_file = appendExt(f, ".meta");
    char *hdr_file = appendExt(f, ".hdr");

    remove_file(meta_file);
    remove_file(img_file);
    remove_file(hdr_file);

    free(meta_file);
    free(img_file);
    free(hdr_file);
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

void faraday_correct(const char *inFile, const char *outFile, double threshold,
                     int save_intermediates, int use_single_rotation_value,
                     radiometry_t output_radiometry, int ksize)
{
  // to get debug images (where an intermediate is generated after
  // faraday rotation correction, but before calibration, so that the
  // raw rotation results can be assessed) -- uncomment the second
  // of these, comment out the first
  int debug = FALSE;
  //int debug = output_radiometry != r_AMP;

  char *meta_name = appendExt(inFile, ".meta");
  meta_parameters *inMeta = meta_read(meta_name);

  meta_parameters *dbgMeta = NULL;
  if (debug)
    dbgMeta = meta_read(meta_name);

  if (inMeta->general->radiometry != r_AMP) {
    asfPrintError("This image is already calibrated. Faraday Rotation can\n"
                  "only be applied to uncalibrated (amplitude) images. After\n"
                  "the Faraday Correction is applied, the image can be\n"
                  "calibrated.\n");
  }

  switch (output_radiometry) {
    case r_AMP:
      asfPrintStatus("Selected output calibration: (none)\n");
      break;
    case r_SIGMA:
      asfPrintStatus("Selected output calibration: Sigma\n");
      break;
    case r_SIGMA_DB:
      asfPrintStatus("Selected output calibration: Sigma (dB)\n");
      break;
    case r_BETA:
      asfPrintStatus("Selected output calibration: Beta\n");
      break;
    case r_BETA_DB:
      asfPrintStatus("Selected output calibration: Beta (dB)\n");
      break;
    case r_GAMMA:
      asfPrintStatus("Selected output calibration: Gamma\n");
      break;
    case r_GAMMA_DB:
      asfPrintStatus("Selected output calibration: Gamma (dB)\n");
      break;
    default:
      asfPrintError("Invalid radiometry: %d\n", output_radiometry);
      break;
  }

  char *in_img_name = appendExt(inFile, ".img");
  char *rot_img_name = appendToBasename(in_img_name, "_farrot");
  char *smoothed_img_name = appendToBasename(in_img_name, "_smooth");
  char *residuals_img_name = appendToBasename(in_img_name, "_residuals");
  char *out_img_name = appendExt(outFile, ".img");
  char *dbg_img_name = appendToBasename(out_img_name, "_no_cal");

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
  // smoothed-image method, for informational purposes, and to compare
  // against the threshold
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

  // See if we should even apply the correction.
  // Note that even if we are going to skip correction, we still do most
  // of the processing-- we just skip the actual correction step.  This is
  // so that we can still apply the calibration parameters, generate the
  // right intermediate file, and the right output file.
  int do_farcorr = TRUE;
  if (threshold > 0) {
    if (fabs(avg_omega) > threshold) {
      asfPrintStatus("Applying Faraday Rotation Correction "
                     "(exceeds threshold angle of %.1f)\n", threshold);
    }
    else {
      asfPrintStatus("Not applying Faraday Rotation Correction "
                     "(below threshold angle of %.1f)\n", threshold);

      if (output_radiometry != r_AMP)
        asfPrintStatus("Applying calibration only ...\n");
      else
        asfPrintStatus("Generating output ...\n");

      do_farcorr = FALSE;
      use_single_rotation_value = TRUE;
      avg_omega = 0;
    }
  }
  else {
    asfPrintStatus("Applying Faraday Rotation Correction "
                   "(no threshold specified)\n");
  }

  // convert to radians
  avg_omega *= D2R;

  // STEP 2: Smooth the Faraday rotation angle image, if necessary
  if (!use_single_rotation_value && do_farcorr) {
    asfPrintStatus("Smoothing rotation angle image (kernel size %d)...\n",
                   ksize);
    smooth(rot_img_name, smoothed_img_name, ksize, EDGE_TRUNCATE);
  }
    
  // STEP 3: Calculate corrected values
  if (do_farcorr) {
    if (save_intermediates)
      asfPrintStatus("Calculating corrected values and residuals...\n");
    else
      asfPrintStatus("Calculating corrected values...\n");
  }

  // Opening the data files...
  fin = fopenImage(in_img_name, "rb");
  fout = fopenImage(out_img_name, "wb");
  qpd = qpd_new(fin, inMeta);
    
  // generate the output metadata
  char *out_meta_name = appendExt(outFile, ".meta");
  meta_parameters *outMeta = meta_read(meta_name);
    
  // update radiometry, and the corresponding band names
  outMeta->general->radiometry = output_radiometry;
    
  int db_flag = FALSE;
  if (output_radiometry == r_SIGMA_DB ||
      output_radiometry == r_BETA_DB ||
      output_radiometry == r_GAMMA_DB)
    db_flag = TRUE;
  
  char bands[512];
  strcpy(bands, "");
  
  for (i=0; i<outMeta->general->band_count; ++i) {
    if (i==qpd->hh_amp_band)
      strcat(bands, get_cal_band_name(outMeta, "AMP-HH"));
    else if (i==qpd->hh_phase_band)
      strcat(bands, get_cal_band_name(outMeta, "PHASE-HH"));
    else if (i==qpd->hv_amp_band)
      strcat(bands, get_cal_band_name(outMeta, "AMP-HV"));
    else if (i==qpd->hv_phase_band)
      strcat(bands, get_cal_band_name(outMeta, "PHASE-HV"));
    else if (i==qpd->vh_amp_band)
      strcat(bands, get_cal_band_name(outMeta, "AMP-VH"));
    else if (i==qpd->vh_phase_band)
      strcat(bands, get_cal_band_name(outMeta, "PHASE-VH"));
    else if (i==qpd->vv_amp_band)
      strcat(bands, get_cal_band_name(outMeta, "AMP-VV"));
    else if (i==qpd->vv_phase_band)
      strcat(bands, get_cal_band_name(outMeta, "PHASE-VV"));
    else {
      // must be a pass-through band
      char *band_name = get_band_name(outMeta->general->bands,
                                      outMeta->general->band_count, i);
      strcat(bands, band_name);
    }
    strcat(bands, ",");
  }
    
  bands[strlen(bands)-1] = '\0'; //strip trailing comma
  strcpy(outMeta->general->bands, bands);
    
  // write out output metadata
  meta_write(outMeta, out_meta_name);
    
  FILE *fpdbg = NULL;
  if (debug && do_farcorr) {
    fpdbg = fopenImage(dbg_img_name, "wb");
    strcpy(dbgMeta->general->bands, "HH-AMP,HH-PHASE,HV-AMP,HV-PHASE,"
           "VH-AMP,VH-PHASE,VV-AMP,VV-PHASE");
    dbgMeta->general->band_count = 8;
    dbgMeta->general->radiometry = r_AMP;
    
    char *dbg_meta_name = appendExt(dbg_img_name, ".meta");
    meta_write(dbgMeta, dbg_meta_name);
    free(dbg_meta_name);
  }
    
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
  if (save_intermediates && do_farcorr) {
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
    
  // initialize the incidence angle array (for calibration)
  float *incid=NULL;
  if (output_radiometry != r_AMP)
    incid = incid_init(inMeta);
    
  // now iterate through the input image's pixels...
  for (i=0; i<nl; ++i) {
    asfLineMeter(i,nl);
    qpd_get_line(qpd,i);

    if (do_farcorr) {
      if (!use_single_rotation_value)
        get_float_line(fprot, rotMeta, i, rotation_vals);
      
      for (j=0; j<ns; ++j) {
        double omega;
        if (use_single_rotation_value)
          omega = avg_omega;
        else
          omega = D2R*rotation_vals[j];
        
        omega *= -1;
        quadPolS2Float *qpf = qpd->buf + j;
        
        // This is the "M" matrix
        complexMatrix *m =
          complex_matrix_new22(qpf->hh, qpf->hv, qpf->vh, qpf->vv);
        
        // rotate by the calculated faraday rotation angle
        // note that make_cpx_rotation_matrix actually makes a fully real
        // matrix, but we want to use the cpx matrix multiplication fns
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
          res[j] = fabs(omega - get_omega(qpd, i, j));
      }
      
      // dump a corrected image before calibration, for debugging
      if (fpdbg) {
        put_band_float_line(fpdbg, dbgMeta, 0, i, hh_amp);
        put_band_float_line(fpdbg, dbgMeta, 1, i, hh_phase);
        put_band_float_line(fpdbg, dbgMeta, 2, i, hv_amp);
        put_band_float_line(fpdbg, dbgMeta, 3, i, hv_phase);
        put_band_float_line(fpdbg, dbgMeta, 4, i, vh_amp);
        put_band_float_line(fpdbg, dbgMeta, 5, i, vh_phase);
        put_band_float_line(fpdbg, dbgMeta, 6, i, vv_amp);
        put_band_float_line(fpdbg, dbgMeta, 7, i, vv_phase);
      }
    }
    else {
      // do not rotate -- output same as input
      for (j=0; j<ns; ++j) {
        quadPolS2Float *qpf = qpd->buf + j;
        hh_amp[j] = complex_amp(qpf->hh);
        hh_phase[j] = complex_arg(qpf->hh);
        hv_amp[j] = complex_amp(qpf->hv);
        hv_phase[j] = complex_arg(qpf->hv);
        vh_amp[j] = complex_amp(qpf->vh);
        vh_phase[j] = complex_arg(qpf->vh);
        vv_amp[j] = complex_amp(qpf->vv);
        vv_phase[j] = complex_arg(qpf->vv);
      }
    }

    // apply calibration to amplitude bands, if necessary
    if (output_radiometry != r_AMP) {
      for (j=0; j<ns; ++j) {
        hh_amp[j] =  get_cal_dn(outMeta, incid[j], j, hh_amp[j], "HH", db_flag);
        hv_amp[j] =  get_cal_dn(outMeta, incid[j], j, hv_amp[j], "HV", db_flag);
        vh_amp[j] =  get_cal_dn(outMeta, incid[j], j, vh_amp[j], "VH", db_flag);
        vv_amp[j] =  get_cal_dn(outMeta, incid[j], j, vv_amp[j], "VV", db_flag);
      }
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
    if (do_farcorr && save_intermediates)
      put_float_line(fpres, resMeta, i, res);
  }
    
  // now the "pass through" bands (not part of the quad-pol data)
  // (for example, we sometimes add an amplitude band at the beginning)
  for (j=0; j<MAX_OTHER; ++j) {
    if (qpd->other_bands[j] >= 0) {
      char *name = get_band_name(outMeta->general->bands,
                                 outMeta->general->band_count, j);
      asfPrintStatus("Writing pass-through band: %s (band %d)\n", name, j);
      FREE(name);
      
      for (i=0; i<nl; ++i) {
        asfLineMeter(i,nl);
        get_band_float_line(fin, inMeta, j, i, buf);
        put_band_float_line(fout, outMeta, j, i, buf);
      }
    }
  }
    
  FCLOSE(fin);
  FCLOSE(fout);
  if (!use_single_rotation_value)
    FCLOSE(fprot);
  if (save_intermediates)
    FCLOSE(fpres);
  if (fpdbg)
    FCLOSE(fpdbg);
    
  // STEP 4: Clean up
  free(out_meta_name);
  FREE(buf);
  FREE(rotation_vals);
  if (incid)
    FREE(incid);
  
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
    // depending on what options have been selected.

    // If the rotation angle was below the threshold, then the output only
    // has one band, which is the individual rotation angles.  In this case,
    // we don't need to do anything, since the intermediate file already
    // has that one band and is correctly named.

    // Otherwise:

    // use_single_rotation_value     bands in frrot file
    // -------------------------     -------------------
    // 1. No                         OMEGA,OMEGA_SMOOTH,RESIDUALS
    // 2. Yes                        OMEGA,RESIDUALS

    if (do_farcorr) {
      asfPrintStatus("Consolidating intermediates in a single %d-band file.\n",
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
  if (dbgMeta)
    meta_free(dbgMeta);

  free(in_img_name);
  free(rot_img_name);
  free(smoothed_img_name);
  free(residuals_img_name);
  free(out_img_name);
  free(dbg_img_name);

  qpd_free(qpd);
}

