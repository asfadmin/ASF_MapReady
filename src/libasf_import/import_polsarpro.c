#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_license.h>
#include <asf_contact.h>
#include <envi.h>
#include <airsar.h>
#include <asf_import.h>
#include <asf_raster.h>

static int isCEOS(const char *dataFile)
{
  char **inBandName = NULL, **inMetaName = NULL;
  char baseName[512];
  int nBands, trailer;
  ceos_file_pairs_t ceos_pair = NO_CEOS_FILE_PAIR;

  ceos_pair = get_ceos_names(dataFile, baseName,
                             &inBandName, &inMetaName,
                             &nBands, &trailer);

  return (ceos_pair != NO_CEOS_FILE_PAIR);
}

static char *get_airsar(char *buf, char *str)
{
  char *p, *q;

  static char value[51];
  memset(value,0,51);

  q = (char *) CALLOC(51,sizeof(char));
  p = strstr(buf, str);
  if (p) {
    strncpy(q, p, 50);
    strcpy(value, q+strlen(str));
    //printf("%s: %s\n", str, value);
  }
  else
    strcpy(value, "");

  FREE(q);

  return value;
}

int isAIRSAR(char *dataFile)
{
  airsar_header *header = NULL;
  FILE *fp;
  int L_airsar = 0;
  int C_airsar = 0;
  int P_airsar = 0;
  char buf[4400], *value, *band_data, *s;
  double version;

  // Allocate memory and file handling
  value = (char *) MALLOC(sizeof(char)*25);
  header = (airsar_header *) CALLOC(1, sizeof(airsar_header));
  band_data = STRDUP(dataFile);

  // Try L-band
  s = strstr(band_data, "_");
  if (s) {
    *s = '\0';
    strcat(s, "_l.dat");
  }
  fp = fopen(band_data, "r");
  if (fp != NULL && fgets(buf, 4400, fp) == NULL)
    asfPrintError("Could not read general header\n");
  FCLOSE(fp);
  version = atof(get_airsar(buf, "JPL AIRCRAFT SAR PROCESSOR VERSION"));
  L_airsar = (version > 0.0) ? 1 : 0;

  // Try C-band
  s = strstr(band_data, "_");
  if (s) {
    *s = '\0';
    strcat(s, "_c.dat");
  }
  fp = fopen(band_data, "r");
  if (fp != NULL && fgets(buf, 4400, fp) == NULL)
    asfPrintError("Could not read general header\n");
  FCLOSE(fp);
  version = atof(get_airsar(buf, "JPL AIRCRAFT SAR PROCESSOR VERSION"));
  C_airsar = (version > 0.0) ? 1 : 0;

    // Try P-band
  s = strstr(band_data, "_");
  if (s) {
    *s = '\0';
    strcat(s, "_p.dat");
  }
  fp = fopen(band_data, "r");
  if (fp != NULL && fgets(buf, 4400, fp) == NULL)
    asfPrintError("Could not read general header\n");
  FCLOSE(fp);
  version = atof(get_airsar(buf, "JPL AIRCRAFT SAR PROCESSOR VERSION"));
  P_airsar = (version > 0.0) ? 1 : 0;

  return (L_airsar || C_airsar || P_airsar);
}

int strmatches(const char *key, ...)
{
    va_list ap;
    char *arg = NULL;
    int found = FALSE;

    va_start(ap, key);
    do {
        arg = va_arg(ap, char *);
        if (arg) {
            if (strcmp(key, arg) == 0) {
                found = TRUE;
                break;
            }
        }
    } while (arg);

    return found;
}

const char *input_format_to_str(int input_format)
{
  switch (input_format)
    {
    case STF: return "STF";
    case CEOS: return "CEOS";
    case GENERIC_GEOTIFF: return "GENERIC GEOTIFF";
    case BIL: return "BIL";
    case GRIDFLOAT: return "GRIDFLOAT";
    case AIRSAR: return "AIRSAR";
    case VP: return "VP";
    case JAXA_L0: return "JAXA_L0";
    case ALOS_MOSAIC: return "ALOS_MOSAIC";
    case POLSARPRO: return "POLSARPRO";
    default: return "UNKNOWN";
    }
}

static void ingest_airsar_polsar_amp(char *inFile, char *outFile,
				     double *p_range_scale,
				     double *p_azimuth_scale)
{
  FILE *fpIn, *fpOut;
  meta_parameters *meta = NULL;
  int ii, kk, do_resample = FALSE;
  float *power = NULL;
  double azimuth_scale, range_scale;
  char *byteBuf = NULL, *inBaseName = NULL, unscaleBaseName[1024];

  fpIn = FOPEN(inFile, "rb");
  if (p_azimuth_scale && p_range_scale) {
    range_scale = *p_range_scale;
    azimuth_scale = *p_azimuth_scale;
    do_resample = TRUE;
  }
  if (do_resample) {
    sprintf(unscaleBaseName, "%s_unscale", outFile);
    append_ext_if_needed(unscaleBaseName, ".img", NULL);
  }
  else
    append_ext_if_needed(outFile, ".img", NULL);
  meta = import_airsar_meta(inFile, inBaseName, TRUE);
  meta->general->data_type = REAL32;
  meta->general->band_count = 1;
  strcpy(meta->general->bands, "AMP");
  meta->general->image_data_type = IMAGE_LAYER_STACK;

  power = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  byteBuf = (char *) MALLOC(sizeof(char)*10);
  airsar_header *header = read_airsar_header(inFile);
  long offset = header->first_data_offset;
  if (do_resample)
    fpOut = FOPEN(unscaleBaseName, "wb");
  else
    fpOut = FOPEN(outFile, "wb");
  FSEEK(fpIn, offset, SEEK_SET);
  for (ii=0; ii<meta->general->line_count; ii++) {
    for (kk=0; kk<meta->general->sample_count; kk++) {
      FREAD(byteBuf, sizeof(char), 10, fpIn);
      power[kk] = sqrt(((float)byteBuf[1]/254.0 + 1.5) * pow(2, byteBuf[0]));
    }
    put_float_line(fpOut, meta, ii, power);
    asfLineMeter(ii, meta->general->line_count);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  if (do_resample)
    meta_write(meta, unscaleBaseName);
  else
    meta_write(meta, outFile);
  if (power)
    FREE(power);
  if (byteBuf)
    FREE(byteBuf);
  if (meta)
    meta_free(meta);

  if (do_resample) {
    asfPrintStatus("Resampling with scale factors: "
		   "%lf range, %lf azimuth.\n",
		   range_scale, azimuth_scale);

    resample(unscaleBaseName, outFile, range_scale, azimuth_scale);
  }
}

void import_polsarpro(char *s, char *ceosName, int byteFlag,
                      char *outBaseName)
{
  meta_parameters *metaIn = NULL, *metaOut = NULL;
  envi_header *envi;
  FILE *fpIn, *fpOut;
  float *floatBuf;
  double *p_azimuth_scale = NULL, *p_range_scale = NULL;
  double azimuth_scale, range_scale;
  char enviName[1024], outName[1024];
  int ii, multilook = FALSE;
  char *polsarName = s;

  // Read the ENVI header first. We need to know the dimensions of the
  // polarimetric data first in order to resample the amplitude data to the
  // correct size.
  char *ext = findExt(polsarName);
  if (!ext || (ext && (strcmp_case(ext, ".bin")!=0))) {
    // No .bin file extension ...or had some other file extension.
    // Make the guess that adding a .bin file extension may result in
    // finding the intended data and try again.
    polsarName = appendExt(s, ".bin");
  }

  sprintf(enviName, "%s.hdr", polsarName);
  envi = read_envi(enviName);
  int line_count = envi->lines;
  int sample_count = envi->samples;

  // Determine if the ancillary file is CEOS or AIRSAR
  if (ceosName == NULL || strlen(ceosName) <= 0)
    asfPrintError("Please add CEOS or AIRSAR ancillary files to the PolSARpro\n"
        "input files as appropriate\n");

  int is_airsar = isAIRSAR(ceosName);
  int is_ceos = isCEOS(ceosName);

  if (is_ceos)
    metaOut = meta_read(ceosName);
  else if (is_airsar)
    metaOut = import_airsar_meta(ceosName, ceosName, TRUE);
  else
    asfPrintError(
        "Ancillary file is not CEOS or AIRSAR format (required):\n%s\n",
        ceosName);

  if (line_count != metaOut->general->line_count ||
      sample_count != metaOut->general->sample_count) {
    azimuth_scale = 1.0 / (metaOut->general->line_count / line_count);
    range_scale = 1.0 / (metaOut->general->sample_count / sample_count);
    p_azimuth_scale = &azimuth_scale;
    p_range_scale = &range_scale;
    if (!FLOAT_EQUIVALENT(azimuth_scale, range_scale))
      multilook = TRUE;
  }
  meta_free(metaOut);

  // Ingest the CEOS/AirSAR data to generate an amplitude image (in case the
  // user wants to terrain correct. Will need to get the metadata anyway
  if (is_ceos) {
    asfPrintStatus("Ingesting CEOS data ...\n");
    import_ceos(ceosName, outBaseName, "none", NULL, p_range_scale,
		p_azimuth_scale, NULL, 0, 0, -99, -99, NULL, r_AMP, FALSE,
		FALSE, FALSE, TRUE, FALSE);
  }
  else if (is_airsar) {
    asfPrintStatus("Ingesting AirSAR data ...\n");
    ingest_airsar_polsar_amp(ceosName, outBaseName,
			     p_range_scale, p_azimuth_scale);
  }

  // Read the PolSAR Pro data into the layer stack
  sprintf(outName, "%s.img", outBaseName);
  metaIn = envi2meta(envi);
  metaOut = meta_read(outBaseName);
  metaOut->general->band_count = 2;
  strcat(metaOut->general->bands, ",POLSARPRO");
  floatBuf = (float *) MALLOC(sizeof(float)*metaOut->general->sample_count);

  fpIn = FOPEN(polsarName, "rb");
  fpOut = FOPEN(outName, "ab");

  // Let the reading and writing function take care of swapping bytes.
  for (ii=0; ii<metaOut->general->line_count; ii++) {
    get_float_line(fpIn, metaIn, ii, floatBuf);
    put_float_line(fpOut, metaOut, ii, floatBuf);
    asfLineMeter(ii, metaOut->general->line_count);
  }

  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(floatBuf);
  metaOut->sar->multilook = multilook;
  meta_write(metaOut, outBaseName);
  if (metaIn)
    meta_free(metaIn);
  if (metaOut)
    meta_free(metaOut);
}

