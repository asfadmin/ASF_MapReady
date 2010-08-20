#include "asf_import.h"
#include "asf_meta.h"
#include "dateUtil.h"
#include "asf_nan.h"
#include "asf_endian.h"
#include "asf_sar.h"
#include "asf_raster.h"
#include <ctype.h>

// in: hdr file
meta_parameters *meta_read_roipac(const char *in)
{
  char *ext = strrchr(in, '.');
  if (ext) ++ext;

  meta_parameters *meta = raw_init();
  strcpy(meta->general->basename, in);
  meta->general->data_type = COMPLEX_REAL32;
  meta->general->image_data_type = IMAGE;

  FILE *ifp = FOPEN(in, "r");

  char line[1024];
  while (fgets(line, 1024, ifp)) {
    char *field, *value;
    split2(line, ' ', &field, &value);
    if (strcmp_case(field, "WIDTH")==0)
      meta->general->sample_count = atoi(value);
    else if (strcmp_case(field, "FILE_LENGTH")==0)
      meta->general->line_count = atoi(value);
    else if (strcmp_case(field, "XMIN")==0)
      meta->general->start_sample = atoi(value);
    else if (strcmp_case(field, "YMIN")==0)
      meta->general->start_line = atoi(value);
    else if (strcmp_case(field, "RANGE_PIXEL_SIZE")==0)
      meta->general->x_pixel_size = atof(value);
    else if (strcmp_case(field, "AZIMUTH_PIXEL_SIZE")==0)
      meta->general->y_pixel_size = atof(value);
    else if (strcmp_case(field, "LATITUDE")==0)
      meta->general->center_latitude = atof(value);
    else if (strcmp_case(field, "LONGITUDE")==0)
      meta->general->center_longitude = atof(value);
    free(field);
    free(value);
  }

  return meta;
}

static void ingest_roipac_rmg(const char *inFile, FILE *fpOut,
                              meta_parameters *inMeta, meta_parameters *outMeta,
                              int amp_band, int phase_band)
{
  // ROI_PAC rmg files are alternating lines of amplitude and phase

  int inl = inMeta->general->line_count;
  int ins = inMeta->general->sample_count;

  int onl = outMeta->general->line_count;
  //int ons = outMeta->general->sample_count;

  if (inl<onl) {
    asfPrintError("Internal error: Bad line counts: %d<%d\n", inl,onl);
  }

  FILE *fpIn = FOPEN(inFile, "rb");

  float *buf = MALLOC(sizeof(float)*ins);

  int ii;
  for (ii=0; ii<onl; ++ii) {
    // amplitude line
    FREAD(buf, sizeof(float), ins, fpIn);
    //for (jj=0; jj<ins; ++jj) {
    //  big32(buf[jj]);
    //}

    put_band_float_line(fpOut, outMeta, amp_band, ii, buf);

    // phase line
    FREAD(buf, sizeof(float), ins, fpIn);
    //for (jj=0; jj<ins; ++jj) {
    //  big32(buf[jj]);
    //}

    put_band_float_line(fpOut, outMeta, phase_band, ii, buf);

    asfLineMeter(ii, onl);
  }

  FCLOSE(fpIn);
  FREE(buf);
}

static
void ingest_roipac_cpx(const char *inFile, FILE *fpOut,
                       meta_parameters *inMeta, meta_parameters *outMeta,
                       int amp_band, int phase_band)
{
  // ROI_PAC cpx files are alternating real and imaginery pixel values

  int onl = outMeta->general->line_count;
  int ons = outMeta->general->sample_count;

  int inl = inMeta->general->line_count;
  int ins = inMeta->general->sample_count;
  
  if (inl<onl) {
    asfPrintError("Internal error: Bad line counts: %d<%d\n", inl,onl);
  }

  FILE *fpIn = FOPEN(inFile, "rb");

  float *buf = MALLOC(sizeof(float)*ins*2);
  float *amp = MALLOC(sizeof(float)*ins);
  float *phase = MALLOC(sizeof(float)*ins);

  int ii, jj;
  for (ii=0; ii<onl; ++ii) {
    FREAD(buf, sizeof(float), ins*2, fpIn);
    for (jj=0; jj<ons; ++jj) {
      float re = buf[jj*2];
      float im = buf[jj*2+1];
    
      amp[jj] = hypot(re,im);
      phase[jj] = atan2_check(im,re);

      //big32(amp[jj]);
      //big32(phase[jj]);
    }

    asfLineMeter(ii,onl);
    put_band_float_line(fpOut, outMeta, amp_band, ii, amp);
    put_band_float_line(fpOut, outMeta, phase_band, ii, phase);
  }

  FCLOSE(fpIn);

  FREE(buf);
  FREE(amp);
  FREE(phase);
}

static void populate_baseline(meta_parameters *meta,
                              const char *rsc_baseline_file,
                              const char *rsc_master_file)
{
  FILE *ifp = FOPEN(rsc_baseline_file, "r");

  double h_baseline_top_hdr=0.0;
  double h_baseline_rate_hdr=0.0;
  double h_baseline_acc_hdr=0.0;
  double v_baseline_top_hdr=0.0;
  double v_baseline_rate_hdr=0.0;
  double v_baseline_acc_hdr=0.0;
  double azimuth_pixel_size=0.0;
  int file_length=0;
  double p_baseline_top=0.0;
  double p_baseline_bottom=0.0;
  double time_span_year=0.0;

  char line[1024];
  while (fgets(line, 1024, ifp)) {
    char *field, *value;
    split2(line, ' ', &field, &value);
    if (strcmp_case(field, "H_BASELINE_TOP_HDR")==0)
      h_baseline_top_hdr = atof(value);
    if (strcmp_case(field, "H_BASELINE_RATE_HDR")==0)
      h_baseline_rate_hdr = atof(value);
    if (strcmp_case(field, "H_BASELINE_ACC_HDR")==0)
      h_baseline_acc_hdr = atof(value);
    if (strcmp_case(field, "V_BASELINE_TOP_HDR")==0)
      v_baseline_top_hdr = atof(value);
    if (strcmp_case(field, "V_BASELINE_RATE_HDR")==0)
      v_baseline_rate_hdr = atof(value);
    if (strcmp_case(field, "P_BASELINE_TOP_HDR")==0)
      p_baseline_top = atof(value);
    if (strcmp_case(field, "P_BASELINE_BOTTOM_HDR")==0)
      p_baseline_bottom = atof(value);
    if (strcmp_case(field, "TIME_SPAN_YEAR")==0)
      time_span_year = atof(value);
    free(field);
    free(value);
  }
  fclose(ifp);

  ifp = FOPEN(rsc_master_file, "r");

  while (fgets(line, 1024, ifp)) {
    char *field, *value;
    split2(line, ' ', &field, &value);
    if (strcmp_case(field, "AZIMUTH_PIXEL_SIZE")==0)
      azimuth_pixel_size = atof(value);
    if (strcmp_case(field, "FILE_LENGTH")==0)
      file_length = atoi(value);
    free(field);
    free(value);
  }
  fclose(ifp);

  double x = (double)file_length * azimuth_pixel_size / 2.0;

  double h_baseline_center = h_baseline_top_hdr +
                                 h_baseline_rate_hdr * x +
                                 h_baseline_acc_hdr * x * x;
  double v_baseline_center = v_baseline_top_hdr +
                                 v_baseline_rate_hdr * x +
                                 v_baseline_acc_hdr * x * x;

  double len = hypot(h_baseline_center, v_baseline_center);

  double perp = .5 * (p_baseline_top + p_baseline_bottom);
  double par = sqrt(len*len - perp*perp);

  time_t rawtime;
  struct tm *timeinfo;
  time(&rawtime);
  timeinfo = localtime(&rawtime);
  int year = timeinfo->tm_year+1900;
  int days = (((year%4)==0)&&(((year%100)!=0)||((year%400)==0))) ? 366 : 365;
  int temporal = (int)(0.5 + time_span_year * (double)days);

  printf("\nAt line %d:\n"
         "Horizontal = %f\n"
         "Vertical = %f\n"
         "Length = %f\n"
         "Perpendicular = %f\n"
         "Parallel = %f\n"
         "Temporal = %d\n\n",
         file_length/2, h_baseline_center, v_baseline_center,
         len, perp, par, temporal);

  // when we have an insar block, we can populate it
}



static int min2(int a, int b) {
  return a<b ? a : b;
}
static int min3(int a, int b, int c) {
  return min2(min2(a,b),c);
}

void import_roipac(const char *basename, const char *outFile)
{
  char *amp = appendExt(basename, ".amp");
  char *amp_rsc = appendExt(basename, ".amp.rsc");
  char *cor = appendExt(basename, ".cor");
  char *cor_rsc = appendExt(basename, ".cor.rsc");
  char *inf = appendExt(basename, ".int");
  char *inf_rsc = appendExt(basename, ".int.rsc");

  int nbands=0;
  char bands[256];
  strcpy(bands, "");

  meta_parameters *meta; 

  meta_parameters *ampMeta = NULL;
  if (fileExists(amp) && fileExists(amp_rsc)) {
    ampMeta = meta_read_roipac(amp_rsc);
    meta = meta_copy(ampMeta);
    nbands += 2;
    strcat(bands, "AMP,PHASE,");
  } else {
    asfPrintStatus("Amplitude file '%s' not found.\n", amp);
  }

  meta_parameters *corMeta = NULL;
  if (fileExists(cor) && fileExists(cor_rsc)) {
    corMeta = meta_read_roipac(cor_rsc);
    if (!meta) meta = meta_copy(corMeta);
    nbands += 2;
    strcat(bands, "COR_AMP,COR_PHASE,");
  } else {
    asfPrintStatus("Correlation file '%s' not found.\n", cor);
  }

  meta_parameters *infMeta = NULL;
  if (fileExists(inf) && fileExists(inf_rsc)) {
    infMeta = meta_read_roipac(inf_rsc);
    if (!meta) meta = meta_copy(infMeta);
    nbands += 2;
    strcat(bands, "INT_AMP,INT_PHASE,");
  } else {
    asfPrintStatus("Interferogram file '%s' not found.\n", inf);
  }

  if (ampMeta==NULL && corMeta==NULL && infMeta==NULL) {
    asfPrintError("No files found.\n");
  }

  int ns_amp = ampMeta ? ampMeta->general->sample_count : 99999;
  int ns_cor = corMeta ? corMeta->general->sample_count : 99999;
  int ns_inf = infMeta ? infMeta->general->sample_count : 99999;
  int ns = min3(ns_amp, ns_cor, ns_inf);

  int nl_amp=999999, nl_cor=999999, nl_inf=999999;
  if (ampMeta) {
    nl_amp = ampMeta->general->line_count;
    int nl_calc = (int)(fileSize(amp) / ns_amp / 4 / 2);
    if (nl_calc < nl_amp) {
      printf("Amplitude file size shorter than expected, truncating.\n"
             "  (line count from metadata: %d, from file size: %d)\n",
             nl_amp, nl_calc);
      nl_amp = nl_calc;
    }
  }
  if (corMeta) {
    nl_cor = corMeta->general->line_count;
    int nl_calc = (int)(fileSize(cor) / ns_cor / 4 / 2);
    if (nl_calc < nl_cor) {
      printf("Correlation file size shorter than expected, truncating.\n"
             "  (line count from metadata: %d, from file size: %d)\n",
             nl_cor, nl_calc);
      nl_cor = nl_calc;
    }
  }
  if (infMeta) {
    nl_inf = infMeta->general->line_count;
    int nl_calc = (int)(fileSize(inf) / ns_inf / 4 / 2);
    if (nl_calc < nl_inf) {
      printf("Interferogram file size shorter than expected, truncating.\n"
             "  (line count from metadata: %d, from file size: %d)\n",
             nl_inf, nl_calc);
      nl_inf = nl_calc;
    }
  }

  int nl = min3(nl_amp, nl_cor, nl_inf);
  printf("Output line count: %d\n", nl);

  meta->general->data_type = REAL32;
  meta->general->line_count = nl;
  meta->general->sample_count = ns;

  int len = strlen(bands);
  if (nbands==0 || len==0) {
    // this error should have been caught earlier
    asfPrintError("Nothing to ingest.\n");
  }

  if (bands[len-1]==',') {
    bands[len-1]='\0';
  }
  meta->general->band_count = nbands;
  strcpy(meta->general->bands, bands);

  strcpy(meta->general->basename, basename);

  asfPrintStatus("Output file will have %d bands:\n %s\n", nbands, bands);

  char *imgFile = appendExt(outFile, ".img");
  FILE *fpOut = FOPEN(imgFile, "wb");

  int curr = 0;
  if (fileExists(amp) && fileExists(amp_rsc)) {
    asfPrintStatus("Processing amplitude file %s...\n", amp);
    ingest_roipac_cpx(amp, fpOut, ampMeta, meta, curr, curr+1);
    curr += 2;
  }
  if (fileExists(cor) && fileExists(cor_rsc)) {
    asfPrintStatus("Processing correlation file %s...\n", cor);
    ingest_roipac_rmg(cor, fpOut, corMeta, meta, curr, curr+1);
    curr += 2;
  }
  if (fileExists(inf) && fileExists(inf_rsc)) {
    asfPrintStatus("Processing interferogram file %s...\n", inf);
    ingest_roipac_cpx(inf, fpOut, infMeta, meta, curr, curr+1);
    curr += 2;
  }

  if (curr != nbands) {
    asfPrintError("Internal error: failed to calculate the band correctly.\n");
  }

  FCLOSE(fpOut);

  // read baseline info
  char *rsc_baseline_file = "master_slave_baseline.rsc";
  char *rsc_master_file = "master.slc.rsc";

  int ok = TRUE;
  if (!fileExists(rsc_baseline_file)) {
    asfPrintWarning("Cannot calculate baseline, input file not found: %s\n",
                    rsc_baseline_file);
    ok = FALSE;
  }
  if (!fileExists(rsc_master_file)) {
    asfPrintWarning("Cannot calculate baseline, input file not found: %s\n",
                    rsc_master_file);
    ok = FALSE;
  }
  
  if (ok) {
    asfPrintStatus("Calculating baseline values...\n");
    populate_baseline(meta, rsc_baseline_file, rsc_master_file);
  }

  meta_write(meta, outFile);
  FREE(imgFile);
}

