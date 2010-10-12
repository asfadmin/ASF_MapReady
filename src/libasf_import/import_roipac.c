#include "asf_import.h"
#include "asf_meta.h"
#include "dateUtil.h"
#include "asf_nan.h"
#include "asf_endian.h"
#include "asf_sar.h"
#include "asf_raster.h"
#include <ctype.h>
#include <time.h>

// prototypes for roipac code lifted to do the state vector interpolation
/*-----------------------------------------------------------------*/
struct StateVector_struct {
  double Date;
  double x[6];
  struct StateVector_struct *Next;
  };
typedef struct StateVector_struct StateVector_type;
static void Interpolate(StateVector_type *SV, double TargetDate,
                        StateVector_type *NewSV);
static void ReadStateVectorFile(const char *FileName, StateVector_type **Root);
static void ROI_PAC_HDR2polynom(const char *hdr_data_points_file, double Date,
                                double *px, double *py, double *pz,
                                double *vx, double *vy, double *vz);
// end of the roipac prototypes -- actual code is at the end of the file
/*-----------------------------------------------------------------*/

// in: hdr file
// sv_file: state vector file
meta_parameters *meta_read_roipac(const char *in, const char *sv_file)
{
  char *ext = strrchr(in, '.');
  if (ext) ++ext;

  meta_parameters *meta = raw_init();
  strcpy(meta->general->basename, in);
  meta->general->data_type = COMPLEX_REAL32;
  meta->general->image_data_type = IMAGE;
  strcpy(meta->general->sensor_name, "SAR");

  meta->sar = meta_sar_init();
  meta->sar->look_direction = 'R';
  meta->sar->image_type = 'S';
  meta->sar->slant_shift = 0.0;
  meta->sar->multilook = 1; // kind of
  meta->sar->deskewed = 1;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  strcpy(meta->general->processor, "");

  FILE *ifp = FOPEN(in, "r");
  int nl, ns, yr, mo, da, hr, mn, sc, ms=0, tm_val=0;
  double utc1=0, utc2=0, utc3=0;
  double prf=0, dop0=0, dop1=0, dop2=0;
  char line[1024];
  while (fgets(line, 1024, ifp)) {
    char *field, *value;
    split2(line, ' ', &field, &value);
    if (strcmp_case(field, "WIDTH")==0)
      meta->general->sample_count = ns = atoi(value);
    else if (strcmp_case(field, "FILE_LENGTH")==0)
      meta->general->line_count = nl = atoi(value);
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
    else if (strcmp_case(field, "EQUATORIAL_RADIUS")==0)
      meta->general->re_major = atof(value);
    else if (strcmp_case(field, "ECCENTRICITY_SQUARED")==0)
      meta->general->re_minor = meta->general->re_major*sqrt(1-atof(value));
    else if (strcmp_case(field, "STARTING_RANGE")==0)
      meta->sar->slant_range_first_pixel = atof(value);
    else if (strcmp_case(field, "CHIRP_SLOPE")==0)
      meta->sar->chirp_rate = atof(value);
    else if (strcmp_case(field, "RANGE_SAMPLING_FREQUENCY")==0)
      meta->sar->range_sampling_rate = atof(value);
    else if (strcmp_case(field, "PULSE_LENGTH")==0)
      meta->sar->pulse_duration = atof(value);
    else if (strcmp_case(field, "DELTA_LINE_UTC")==0)
      meta->sar->azimuth_time_per_pixel = atof(value);
    else if (strcmp_case(field, "PROCESSING_SYSTEM")==0) {
      strcat(meta->general->processor, value);
      strcat(meta->general->processor, " ");
    }
    else if (strcmp_case(field, "PROCESSING_VERSION")==0) {
      strcat(meta->general->processor, value);
      strcat(meta->general->processor, " ");
    }
    else if (strcmp_case(field, "WAVELENGTH")==0)
      meta->sar->wavelength = atof(value);
    else if (strcmp_case(field, "POLARIZATION")==0)
      strcpy(meta->sar->polarization, value);
    else if (strcmp_case(field, "PLATFORM")==0)
      strcpy(meta->general->sensor, value);
    else if (strcmp_case(field, "BEAM")==0)
      strcpy(meta->general->mode, value);
    // calculate this from the state vectors instead, if we can
    else if (sv_file == NULL && strcmp_case(field, "HEIGHT_TOP")==0)
      meta->sar->satellite_height = atof(value);
    else if (strcmp_case(field, "PRF")==0)
      meta->sar->prf = prf = atof(value);
    else if (strcmp_case(field, "DOPPLER_RANGE0")==0)
      dop0 = atof(value);
    else if (strcmp_case(field, "DOPPLER_RANGE1")==0)
      dop1 = atof(value);
    else if (strcmp_case(field, "DOPPLER_RANGE2")==0)
      dop2 = atof(value);
    else if (strcmp_case(field, "ORBIT_DIRECTION")==0)
      meta->general->orbit_direction= strcmp_case(value, "descending")==0 ? 'D' :
                                      strcmp_case(value, "ascending")==0 ? 'A' : '?';
    // we can get a better value for this using the state vectors, if
    // we have them
    else if (strcmp_case(field, "EARTH_RADIUS")==0)
      meta->sar->earth_radius = atof(value);
    else if (strcmp_case(field, "FIRST_FRAME")==0)
      meta->general->frame = atoi(value);
    else if (strcmp_case(field, "ORBIT_NUMBER")==0) {
      // this is of the form %d-%d, we just want the first one...
      char *o1, *o2;
      split2(value, '-', &o1, &o2);
      meta->general->orbit = atoi(o1);
    }
    else if (strcmp_case(field, "FIRST_LINE_YEAR")==0) {
      yr = atoi(value);
      tm_val++;
    }
    else if (strcmp_case(field, "FIRST_LINE_MONTH_OF_YEAR")==0) {
      mo = atoi(value);
      tm_val++;
    }
    else if (strcmp_case(field, "FIRST_LINE_DAY_OF_MONTH")==0) {
      da = atoi(value);
      tm_val++;
    }
    else if (strcmp_case(field, "FIRST_CENTER_HOUR_OF_DAY")==0) {
      hr = atoi(value);
      tm_val++;
    }
    else if (strcmp_case(field, "FIRST_CENTER_MN_OF_HOUR")==0) {
      mn = atoi(value);
      tm_val++;
    }
    else if (strcmp_case(field, "FIRST_CENTER_S_OF_MN")==0) {
      sc = atoi(value);
      tm_val++;
    }
    else if (strcmp_case(field, "FIRST_CENTER_MS_OF_S")==0)
      ms = atoi(value);
    else if (strcmp_case(field, "FIRST_LINE_UTC")==0)
      utc1 = atof(value);
    else if (strcmp_case(field, "CENTER_LINE_UTC")==0)
      utc2 = atof(value);
    else if (strcmp_case(field, "LAST_LINE_UTC")==0)
      utc3 = atof(value);
    free(field);
    free(value);
  }

  if (tm_val == 6) {
    sprintf(meta->general->acquisition_date, "%02d-%s-%04d, %02d:%02d:%02d",
            da,
            (mo == 1)  ? "Jan" :
            (mo == 2)  ? "Feb" :
            (mo == 3)  ? "Mar" :
            (mo == 4)  ? "Apr" :
            (mo == 5)  ? "May" :
            (mo == 6)  ? "Jun" :
            (mo == 7)  ? "Jul" :
            (mo == 8)  ? "Aug" :
            (mo == 9)  ? "Sep" :
            (mo == 10) ? "Oct" :
            (mo == 11) ? "Nov" :
            (mo == 12) ? "Dec" : "Unknown",
            yr, hr, mn, sc);

    if (sv_file != NULL && utc1>0 && utc2>0 && utc3>0) {
      meta_state_vectors *sv = meta_state_vectors_init(3);
      julian_date jdate;
      ymd_date ymd;

      ymd.year = yr;
      ymd.month = mo;
      ymd.day = da;
      date_ymd2jd(&ymd, &jdate);

      sv->vector_count = 3;

      sv->year = yr;
      sv->julDay = jdate.jd;
      sv->second = (60*hr + mn)*60 + sc + (double)ms/1000;
     
      double px, py, pz, vx, vy, vz;
      ROI_PAC_HDR2polynom(sv_file, utc1, &px, &py, &pz, &vx, &vy, &vz);
      sv->vecs[0].time = 0.0;
      sv->vecs[0].vec.pos.x = px;
      sv->vecs[0].vec.pos.y = py;
      sv->vecs[0].vec.pos.z = pz;
      sv->vecs[0].vec.vel.x = vx;
      sv->vecs[0].vec.vel.y = vy;
      sv->vecs[0].vec.vel.z = vz;
      ROI_PAC_HDR2polynom(sv_file, utc2, &px, &py, &pz, &vx, &vy, &vz);
      sv->vecs[1].time = utc2-utc1;
      sv->vecs[1].vec.pos.x = px;
      sv->vecs[1].vec.pos.y = py;
      sv->vecs[1].vec.pos.z = pz;
      sv->vecs[1].vec.vel.x = vx;
      sv->vecs[1].vec.vel.y = vy;
      sv->vecs[1].vec.vel.z = vz;
      ROI_PAC_HDR2polynom(sv_file, utc3, &px, &py, &pz, &vx, &vy, &vz);
      sv->vecs[2].time = utc3-utc1;
      sv->vecs[2].vec.pos.x = px;
      sv->vecs[2].vec.pos.y = py;
      sv->vecs[2].vec.pos.z = pz;
      sv->vecs[2].vec.vel.x = vx;
      sv->vecs[2].vec.vel.y = vy;
      sv->vecs[2].vec.vel.z = vz;

      meta->state_vectors = sv;

      meta->sar->earth_radius = meta_get_earth_radius(meta, nl/2, ns/2);
      meta->sar->satellite_height = sqrt(px*px + py*py + pz*pz);
    }
    else if (sv_file == NULL)
      asfPrintStatus("State vector file not available.\n");
    else
      asfPrintStatus("Not all time values found in %s\n", in);
  }
  else
    asfPrintStatus("Incomplete time information in %s", in);

  FCLOSE(ifp);

  meta->sar->original_line_count = meta->general->line_count;
  meta->sar->original_sample_count = meta->general->sample_count;
  meta->sar->slant_shift = 0;
  meta->sar->time_shift = 0;

  // roipac stores doppler in units of prf, here we conver to Hz
  meta->sar->range_doppler_coefficients[0] = prf*dop0;
  meta->sar->range_doppler_coefficients[1] = prf*dop1;
  meta->sar->range_doppler_coefficients[2] = prf*dop2;
  meta->sar->azimuth_doppler_coefficients[0] = prf*dop0;
  meta->sar->azimuth_doppler_coefficients[1] = 0;
  meta->sar->azimuth_doppler_coefficients[2] = 0;

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
    //for (jj=0; jj<ins; ++jj)
    //  big32(buf[jj]);

    if (amp_band >= 0)
      put_band_float_line(fpOut, outMeta, amp_band, ii, buf);

    // phase line
    FREAD(buf, sizeof(float), ins, fpIn);
    //for (jj=0; jj<ins; ++jj)
    //  big32(buf[jj]);

    if (phase_band)
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
    if (amp_band >= 0)
      put_band_float_line(fpOut, outMeta, amp_band, ii, amp);
    if (phase_band >= 0)
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

  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;

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
  double r2d = 180.0/(4*atan2(1,1));

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

  double base_c = h_baseline_top_hdr;
  double base_n = v_baseline_top_hdr;
  double base_dc = h_baseline_rate_hdr;
  double base_dn = v_baseline_rate_hdr;

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
  if (!meta->insar)
    meta->insar = meta_insar_init();

  strcpy(meta->insar->processor, "ROI_PAC");
  double look = meta_look(meta, nl/2, ns/2);
  meta->insar->center_look_angle = look*r2d;
  meta->insar->doppler = meta->sar->range_doppler_coefficients[0];
  meta->insar->doppler_rate = meta->sar->range_doppler_coefficients[1];
 
  double sin_look = sin(look);
  double cos_look = cos(look); 
  meta->insar->baseline_length = len;
  meta->insar->baseline_parallel = base_c*sin_look - base_n*cos_look;
  meta->insar->baseline_parallel_rate = base_dc*sin_look - base_dn*cos_look;
  meta->insar->baseline_perpendicular = base_c*cos_look + base_n*sin_look;
  meta->insar->baseline_perpendicular_rate = base_dc*cos_look + base_dn*sin_look;
  meta->insar->baseline_temporal = temporal;

  // Calculate critical baseline
  double range = meta_get_slant(meta, nl/2, ns/2);
  double wavelength = meta->sar->wavelength;
  //double bandwidth = meta->sar->chirp_rate;
  double bandwidth = meta->sar->range_sampling_rate;
  double tan_incid = tan(meta_incid(meta, nl/2, ns/2));
  meta->insar->baseline_critical =
      fabs(wavelength * range * bandwidth * tan_incid / SPD_LIGHT);
}

static int min2(int a, int b) {
  return a<b ? a : b;
}
static int min3(int a, int b, int c) {
  return min2(min2(a,b),c);
}

void import_roipac(const char *basename_in, const char *outFile)
{
  char *basename;
  if (strstr(basename_in, "flat_HDR"))
    basename = asf_strReplace(basename_in, "flat_HDR_", "");
  else
    basename = STRDUP(basename_in);

  char *tmp = stripExt(basename);
  free(basename);
  basename = stripExt(tmp);
  free(tmp);

  char *filename = get_filename(basename);

  char *path="";
  char *path1 = get_dirname(basename);
  if (strlen(path1) > 0) {
    if (path1[strlen(path1)-1] != '/')
      path = appendStr(path1, "/");
    else
      path = STRDUP(path1);
  }
  free(path1);

  char *amp = appendExt(basename, ".amp");
  char *amp_rsc = appendExt(basename, ".amp.rsc");
  char *cor = appendExt(basename, ".cor");
  char *cor_rsc = appendExt(basename, ".cor.rsc");

  char *inf, *inf_rsc;
  inf = MALLOC(sizeof(char)*(strlen(path)+strlen(basename)+64));
  sprintf(inf, "%sflat_HDR_%s.int", path, filename);
  inf_rsc = appendStr(inf, ".rsc");
  if (!fileExists(inf) || !fileExists(inf_rsc)) {
    asfPrintStatus("flat interferogram not found, trying unflattened.\n");
    inf = appendExt(basename, ".int");
    inf_rsc = appendExt(basename, ".int.rsc");
  }
  else
    asfPrintStatus("Using flattened interferogram file.\n");

  int nbands=0;
  char bands[256];
  strcpy(bands, "");

  char *sv_file, *master, *slave;
  split2(filename, '-', &master, &slave);

  if (strlen(master)>0 && strlen(slave)>0) {
    sv_file = MALLOC(sizeof(char)*(strlen(path)+strlen(master)+64));
    sprintf(sv_file, "%shdr_data_points_%s.rsc", path, master);
    if (!fileExists(sv_file)) {
      sprintf(sv_file, "%sodr_data_points_%s.rsc", path, master);
      if (!fileExists(sv_file)) {
        free(sv_file);
        sv_file = NULL;
      }
    }
  }
  else {
    sv_file = NULL;
  }

  if (sv_file)
    asfPrintStatus("Found state vectors file: %s\n", sv_file);

  meta_parameters *meta = NULL; 

  meta_parameters *ampMeta = NULL;
  if (fileExists(amp) && fileExists(amp_rsc)) {
    ampMeta = meta_read_roipac(amp_rsc, sv_file);
    meta = meta_copy(ampMeta);
    nbands += 1;
    strcat(bands, "AMP,");
  } else {
    asfPrintStatus("Amplitude file '%s' not found.\n", amp);
    asfPrintWarning("Since the amplitude file is not present, you will not "
	"be able\nto terrain correct this data!\n");
  }

  meta_parameters *infMeta = NULL;
  if (fileExists(inf) && fileExists(inf_rsc)) {
    infMeta = meta_read_roipac(inf_rsc, sv_file);
    if (!meta) meta = meta_copy(infMeta);
    nbands += 2;
    strcat(bands, "INTERFEROGRAM_AMP,INTERFEROGRAM_PHASE,");
  } else {
    asfPrintStatus("Interferogram file '%s' not found.\n", inf);
  }

  meta_parameters *corMeta = NULL;
  if (fileExists(cor) && fileExists(cor_rsc)) {
    corMeta = meta_read_roipac(cor_rsc, sv_file);
    if (!meta) meta = meta_copy(corMeta);
    nbands += 1;
    strcat(bands, "COHERENCE,");
  } else {
    asfPrintStatus("Correlation file '%s' not found.\n", cor);
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
      asfPrintStatus("Amplitude file size shorter than expected, truncating.\n"
                     "  (line count from metadata: %d, from file size: %d)\n",
                     nl_amp, nl_calc);
      nl_amp = nl_calc;
    }
  }

  if (infMeta) {
    nl_inf = infMeta->general->line_count;
    int nl_calc = (int)(fileSize(inf) / ns_inf / 4 / 2);
    if (nl_calc < nl_inf) {
      asfPrintStatus("Interferogram file shorter than expected, truncating.\n"
                     "  (line count from metadata: %d, from file size: %d)\n",
                     nl_inf, nl_calc);
      nl_inf = nl_calc;
    }
  }

  if (corMeta) {
    nl_cor = corMeta->general->line_count;
    int nl_calc = (int)(fileSize(cor) / ns_cor / 4 / 2);
    if (nl_calc < nl_cor) {
      asfPrintStatus("Correlation file shorter than expected, truncating.\n"
                     "  (line count from metadata: %d, from file size: %d)\n",
                     nl_cor, nl_calc);
      nl_cor = nl_calc;
    }
  }

  int nl = min3(nl_amp, nl_cor, nl_inf);
  asfPrintStatus("Output line count: %d\n", nl);

  meta->general->data_type = REAL32;
  meta->general->image_data_type = INSAR_STACK;
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
    ingest_roipac_cpx(amp, fpOut, ampMeta, meta, curr, -1);
    curr += 1;
  }
  if (fileExists(inf) && fileExists(inf_rsc)) {
    asfPrintStatus("Processing interferogram file %s...\n", inf);
    ingest_roipac_cpx(inf, fpOut, infMeta, meta, curr, curr+1);
    curr += 2;
  }
  if (fileExists(cor) && fileExists(cor_rsc)) {
    asfPrintStatus("Processing correlation file %s...\n", cor);
    ingest_roipac_rmg(cor, fpOut, corMeta, meta, -1, curr);
    curr += 1;
  }

  if (curr != nbands) {
    asfPrintError("Internal error: failed to calculate the band correctly.\n");
  }

  FCLOSE(fpOut);

  // read baseline info
  char *rsc_baseline_file = MALLOC(sizeof(char)*
                             (strlen(master)+strlen(slave)+strlen(path)+64));
  sprintf(rsc_baseline_file, "%s%s_%s_baseline.rsc", path, master, slave);
  char *rsc_master_file = MALLOC(sizeof(char)*
                             (strlen(master)+strlen(path)+64));
  sprintf(rsc_master_file, "%s%s.slc.rsc", path, master);

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
  FREE(basename);
  FREE(filename);
  FREE(amp); FREE(amp_rsc);
  FREE(cor); FREE(cor_rsc);
  FREE(inf); FREE(inf_rsc);
  FREE(rsc_baseline_file);
  FREE(rsc_master_file);
  FREE(sv_file);
}

/*----------------------------------------------------------------*/
/* The following code is from roipac's HDR2polynom code, we use it
   to interpolate the state vectors using their formatting        */
/* The only change is to rename the main function, and then return
   the state vector instead of just printing it                   */

/* Create a polynome to interpolate state vector values
   Creation: Frederic Crampe & Francois Rogez 1998 */

/*-----------------------------------------------------------------*/

#define STATE_VECTOR_FORMAT "%lf %lf %lf %lf %lf %lf %lf"
#define OUTPUT_FORMAT "%lf %lf %lf %lf %lf %lf"


/*-----------------------------------------------------------------*/
static void ROI_PAC_HDR2polynom(const char *hdr_data_points_file, double Date,
                                double *px, double *py, double *pz,
                                double *vx, double *vy, double *vz)
{
  StateVector_type *SV = NULL;
  StateVector_type TargetSV;

  ReadStateVectorFile(hdr_data_points_file,&SV);
  Interpolate(SV,Date,&TargetSV);

  *px = TargetSV.x[0];
  *py = TargetSV.x[1];
  *pz = TargetSV.x[2];

  *vx = TargetSV.x[3];
  *vy = TargetSV.x[4];
  *vz = TargetSV.x[5];
}

/*-----------------------------------------------------------------*/
static void ReadStateVectorFile(const char *FileName, StateVector_type **Root){

  char StrBuf[1024];
  FILE *FP;
  int Count;
  StateVector_type *NewSV;
  StateVector_type *SV_ptr;

  FP=fopen(FileName,"r");
  if(FP==NULL){
    fprintf(stderr,"Cannot read file '%s'\n",FileName);
    return;
    }

  for(SV_ptr=*Root; SV_ptr!=NULL && SV_ptr->Next!=NULL; SV_ptr=SV_ptr->Next);

  while(fgets(StrBuf, BUFSIZ, FP) != NULL){
    NewSV=(StateVector_type *)calloc(1,sizeof(StateVector_type));
    Count=sscanf(StrBuf,STATE_VECTOR_FORMAT,&(NewSV->Date),
                 &(NewSV->x[0]),&(NewSV->x[1]),&(NewSV->x[2]),
                 &(NewSV->x[3]),&(NewSV->x[4]),&(NewSV->x[5]));
    if(Count!=7){
      free(NewSV);
      continue;
      }
   if(*Root==NULL){
     *Root=NewSV;
     SV_ptr=NewSV;
     }else{
     SV_ptr->Next=NewSV;
     SV_ptr=SV_ptr->Next;
     }
   }
   return;
   }

 /*-----------------------------------------------------------------*/
static void Interpolate(StateVector_type *SV, double TargetDate,
                        StateVector_type *NewSV){

  StateVector_type *SV_ptr1;
  StateVector_type *SV_ptr2;
  int i;
  double tmp;

  NewSV->Date=TargetDate;
  for(i=0;i<6;i++){
    NewSV->x[i]=0;
    for(SV_ptr1 = SV; SV_ptr1!=NULL; SV_ptr1=SV_ptr1->Next){
      tmp=1;
      for(SV_ptr2 = SV; SV_ptr2!=NULL; SV_ptr2=SV_ptr2->Next){
        if(SV_ptr2==SV_ptr1)continue;
        tmp *= (SV_ptr2->Date-NewSV->Date)/(SV_ptr2->Date-SV_ptr1->Date);
        }
      NewSV->x[i] += (SV_ptr1->x[i]*tmp);
      }
    }
  return;
  }

