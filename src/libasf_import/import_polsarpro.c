#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_license.h>
#include <asf_contact.h>
#include <envi.h>
#include "radarsat2.h"
#include "terrasar.h"
#include <airsar.h>
#include <asf_import.h>
#include <asf_raster.h>
#include "xml_util.h"

#define EPS 1E-15

static char *t3_matrix[9] = {"T11.bin","T12_real.bin","T12_imag.bin",
                             "T13_real.bin","T13_imag.bin","T22.bin",
                             "T23_real.bin","T23_imag.bin","T33.bin"};
static char *t4_matrix[16] = {"T11.bin","T12_real.bin","T12_imag.bin",
                              "T13_real.bin","T13_imag.bin","T14_real.bin",
                              "T14_imag.bin","T22.bin","T23_real.bin",
                              "T23_imag.bin","T24_real.bin","T24_imag.bin",
                              "T33.bin","T34_real.bin","T34_imag.bin",
                              "T44.bin"};
static char *c2_matrix[4] = {"C11.bin","C12_real.bin","C12_imag.bin",
                             "C22.bin"};
static char *c3_matrix[9] = {"C11.bin","C12_real.bin","C12_imag.bin",
                             "C13_real.bin","C13_imag.bin","C22.bin",
                             "C23_real.bin","C23_imag.bin","C33.bin"};
static char *c4_matrix[16] = {"C11.bin","C12_real.bin","C12_imag.bin",
                              "C13_real.bin","C13_imag.bin","C14_real.bin",
                              "C14_imag.bin","C22.bin","C23_real.bin",
                              "C23_imag.bin","C24_real.bin","C24_imag.bin",
                              "C33.bin","C34_real.bin","C34_imag.bin",
                              "C44.bin"};

static char *freeman2_decomposition[2] =
  {"Freeman2_Ground.bin","Freeman2_Vol.bin"};
static char *freeman3_decomposition[3] =
  {"Freeman_Dbl.bin","Freeman_Odd.bin","Freeman_Vol.bin"};
static char *vanZyl3_decomposition[3] =
  {"VanZyl3_Dbl.bin","VanZyl3_Odd.bin","VanZyl3_Vol.bin"};
static char *yamaguchi3_decomposition[3] =
  {"Yamaguchi3_Dbl.bin","Yamaguchi3_Odd.bin","Yamaguchi3_Vol.bin"};
static char *yamaguchi4_decomposition[4] =
  {"Yamaguchi4_Dbl.bin","Yamaguchi4_Hlx.bin","Yamaguchi4_Odd.bin",
   "Yamaguchi4_Vol.bin"};
static char *krogager_decomposition[3] =
  {"Krogager_Kd.bin","Krogager_Kh.bin","Krogager_Ks.bin"};
static char *touzi1_decomposition[3] =
  {"TSVM_alpha_s1.bin","TSVM_alpha_s2.bin","TSVM_alpha_s3.bin"};
static char *touzi2_decomposition[3] =
  {"TSVM_phi_s1.bin","TSVM_phi_s2.bin","TSVM_phi_s3.bin"};
static char *touzi3_decomposition[3] =
  {"TSVM_tau_m1.bin","TSVM_tau_m2.bin","TSVM_tau_m3.bin"};
static char *touzi4_decomposition[3] =
  {"TSVM_psi1.bin","TSVM_psi2.bin","TSVM_psi3.bin"};
static char *touzi5_decomposition[4] =
  {"TSVM_alpha_s.bin","TSVM_phi_s.bin","TSVM_tau_m.bin","TSVM_psi.bin"};

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
    case GAMMA: return "GAMMA";
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
  //meta->general->image_data_type = IMAGE_LAYER_STACK;

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
      ASF_FREAD(byteBuf, sizeof(char), 10, fpIn);
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
		   "%f range, %f azimuth.\n",
		   range_scale, azimuth_scale);

    resample(unscaleBaseName, outFile, range_scale, azimuth_scale);
  }
}

static void ingest_radarsat2_polsar_amp(char *inFile, char *outFile,
					double *p_range_scale,
					double *p_azimuth_scale)
{
  int do_resample = FALSE;
  double azimuth_scale, range_scale;
  char unscaleBaseName[1024];

  if (p_azimuth_scale && p_range_scale) {
    range_scale = *p_range_scale;
    azimuth_scale = *p_azimuth_scale;
    do_resample = TRUE;
  }
  if (do_resample) {
    sprintf(unscaleBaseName, "%s_unscale", outFile);
    import_radarsat2(inFile, r_AMP, unscaleBaseName, TRUE);
  }
  else
    import_radarsat2(inFile, r_AMP, outFile, TRUE);
 
  if (do_resample) {
    asfPrintStatus("Resampling with scale factors: "
		   "%f range, %f azimuth.\n",
		   range_scale, azimuth_scale);

    resample(unscaleBaseName, outFile, range_scale, azimuth_scale);
  }
}

static void ingest_terrasar_polsar_amp(char *inFile, char *outFile,
				       double *p_range_scale,
				       double *p_azimuth_scale)
{
  int do_resample = FALSE;
  double azimuth_scale, range_scale;
  char unscaleBaseName[1024];

  if (p_azimuth_scale && p_range_scale) {
    range_scale = *p_range_scale;
    azimuth_scale = *p_azimuth_scale;
    do_resample = TRUE;
  }
  if (do_resample) {
    sprintf(unscaleBaseName, "%s_unscale", outFile);
    import_terrasar(inFile, r_AMP, unscaleBaseName, TRUE);
  }
  else
    import_terrasar(inFile, r_AMP, outFile, TRUE);
 
  if (do_resample) {
    asfPrintStatus("Resampling with scale factors: "
		   "%f range, %f azimuth.\n",
		   range_scale, azimuth_scale);

    resample(unscaleBaseName, outFile, range_scale, azimuth_scale);
  }
}

static void ingest_uavsar_polsar_amp(char *inFile, char *outFile,
				     char *data_type,
				     double *p_range_scale, 
				     double *p_azimuth_scale)
{
  int do_resample = FALSE;
  double azimuth_scale, range_scale;
  char unscaleBaseName[1024];

  if (p_azimuth_scale && p_range_scale) {
    range_scale = *p_range_scale;
    azimuth_scale = *p_azimuth_scale;
    do_resample = TRUE;
  }
  if (do_resample) {
    sprintf(unscaleBaseName, "%s_unscale", outFile);
    import_uavsar_ext(inFile, -99, -99, -99, -99, r_AMP, TRUE, data_type, 
		      unscaleBaseName);
  }
  else
    import_uavsar_ext(inFile, -99, -99, -99, -99, r_AMP, TRUE, data_type, 
		      outFile);
 
  if (do_resample) {
    asfPrintStatus("Resampling with scale factors: "
		   "%f range, %f azimuth.\n",
		   range_scale, azimuth_scale);

    resample(unscaleBaseName, outFile, range_scale, azimuth_scale);
  }
}

void import_polsarpro(char *s, char *ceosName, char *colormapName,
                      char *image_data_type, int db_flag, char *outBaseName)
{
  meta_parameters *metaIn = NULL, *metaOut = NULL;
  envi_header *envi;
  FILE *fpIn, *fpOut;
  float *floatBuf, *tmp = NULL, fValue;
  double *p_azimuth_scale = NULL, *p_range_scale = NULL;
  double azimuth_scale, range_scale, min, max, slope, offset;
  char enviName[1024], outName[1024], bandStr[50];
  char dirName[1024], fileName[1024];
  char *matrixType, *decompositionType, *error=NULL;
  int ii, multilook = FALSE, matrix = FALSE;
  int flip_horizontal = FALSE;
  int flip_vertical = FALSE;
  char *polsarName = (char *) MALLOC(sizeof(char)*(strlen(s) + 20));
  sprintf(polsarName, "%s", s);
  split_dir_and_file(polsarName, dirName, fileName);  

  if (strcmp_case(image_data_type, "POLARIMETRIC_MATRIX") == 0)
    matrix = TRUE;

  // Read the ENVI header first. We need to know the dimensions of the
  // polarimetric data first in order to resample the amplitude data to the
  // correct size.
  char *ext = findExt(polsarName);
  if ((!ext || (ext && (strcmp_case(ext, ".bin")!=0))) && !matrix) {
    // No .bin file extension ...or had some other file extension.
    // Make the guess that adding a .bin file extension may result in
    // finding the intended data and try again.
    polsarName = appendExt(s, ".bin");
  }

  isPolsarproSegmentation(polsarName, &error);
  if (error && 
      strcmp_case(image_data_type, "POLARIMETRIC_SEGMENTATION") == 0)
    asfPrintError("%s\n", error);
  if (error) {
    FREE(error);
    error = NULL;
  }

  int is_polsarpro_decomposition =
    isPolsarproDecomposition(polsarName, &decompositionType, &error);
  if (error && 
      strcmp_case(image_data_type, "POLARIMETRIC_DECOMPOSITION") == 0)
    asfPrintError("%s\n", error);
  if (error) {
    FREE(error);
    error = NULL;
  }

  isPolsarproParameter(polsarName, &error);
  if (error && 
      strcmp_case(image_data_type, "POLARIMETRIC_PARAMETER") == 0)
    asfPrintError("%s\n", error);
  if (error) {
    FREE(error);
    error = NULL;
  }

  int is_polsarpro_matrix = 
    matrix ? isPolsarproMatrix(polsarName, &matrixType, &error) : 0;
  if (error && strcmp_case(image_data_type, "POLARIMETRIC_MATRIX") == 0)
    asfPrintError("%s\n", error);
  if (error) {
    FREE(error);
    error = NULL;
  }
  if (is_polsarpro_matrix) {
    // Need to get the metadata information from the first element of the 
    // matrix
    if (strcmp(matrixType, "T3") == 0) {
      asfPrintStatus("   Found all elements of the coherency matrix T3.\n");
      strcat(polsarName, "/T11.bin");
    }
    else if (strcmp(matrixType, "T4") == 0) {
      asfPrintStatus("   Found all elements of the coherency matrix T4.\n");
      strcat(polsarName, "/T11.bin");
    }
    else if (strcmp(matrixType, "C2") == 0) {
      asfPrintStatus("   Found all elements of the covariance matrix C2.\n");
      strcat(polsarName, "/C11.bin");
    }
    else if (strcmp(matrixType, "C3") == 0) {
      asfPrintStatus("   Found all elements of the covariance matrix C3.\n");
      strcat(polsarName, "/C11.bin");
    }
    else if (strcmp(matrixType, "C4") == 0) {
      asfPrintStatus("   Found all elements of the covariance matrix C4.\n");
      strcat(polsarName, "/C11.bin");
    }
  }
  
  sprintf(outName, "%s.img", outBaseName);
  sprintf(enviName, "%s.hdr", polsarName);
  envi = read_envi(enviName);
  int line_count = envi->lines;
  int sample_count = envi->samples;

  int band, band_count=0;
  char **bands;
  if (strcmp_case(image_data_type, "POLARIMETRIC_MATRIX") == 0) {
    if (strcmp(matrixType, "T3") == 0) {
      band_count = 9;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(t3_matrix[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(matrixType, "T4") == 0) {
      band_count = 16;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(t4_matrix[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(matrixType, "C2") == 0) {
      band_count = 4;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(c2_matrix[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(matrixType, "C3") == 0) {
      band_count = 9;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(c3_matrix[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(matrixType, "C4") == 0) {
      band_count = 16;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(c4_matrix[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
  }
  else if (strcmp_case(image_data_type, "POLARIMETRIC_DECOMPOSITION") == 0) {
    if (strcmp(decompositionType, "Freeman 2") == 0) {
      band_count = 2;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(freeman2_decomposition[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(decompositionType, "Freeman 3") == 0) {
      band_count = 3;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(freeman3_decomposition[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(decompositionType, "Van Zyl 3") == 0) {
      band_count = 3;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(vanZyl3_decomposition[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(decompositionType, "Yamaguchi 3") == 0) {
      band_count = 3;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(yamaguchi3_decomposition[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(decompositionType, "Yamaguchi 4") == 0) {
      band_count = 4;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(yamaguchi4_decomposition[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(decompositionType, "Krogager") == 0) {
      band_count = 3;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(krogager_decomposition[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(decompositionType, "Touzi alpha_s") == 0) {
      band_count = 3;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(touzi1_decomposition[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(decompositionType, "Touzi phi_s") == 0) {
      band_count = 3;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(touzi2_decomposition[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(decompositionType, "Touzi tau_m") == 0) {
      band_count = 3;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(touzi3_decomposition[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(decompositionType, "Touzi psi") == 0) {
      band_count = 3;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(touzi4_decomposition[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(decompositionType, "Touzi 4 components") == 0) {
      band_count = 4;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(touzi5_decomposition[band]);
	char *p = strchr(band_name, '.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
  }
  else {
    band_count = 1;
    bands = (char **) MALLOC(sizeof(char *));
    bands[0] = (char *) MALLOC(sizeof(char)*1024);
    sprintf(bands[0], "%s", polsarName);
  }

  // Check for the map projection information in the ENVI header
  metaIn = envi2meta(envi);

  if (!metaIn->projection) {
    // Determine if the ancillary file is CEOS or AIRSAR
    if (ceosName == NULL || strlen(ceosName) <= 0)
      asfPrintError("Please add ancillary files to the PolSARpro input files "
		    "as appropriate\n");

    int is_airsar = FALSE, is_ceos = FALSE;
    int is_radarsat2 = FALSE, is_terrasar = FALSE;
    int is_uavsar = FALSE;
    char uavsar_type[5];

    is_airsar = isAIRSAR(ceosName);
    is_ceos = isCEOS(ceosName, &error);
    FREE(error); error = NULL;

    is_radarsat2 = isRadarsat2(ceosName, &error);
    FREE(error); error = NULL;

    is_terrasar = isTerrasar_ext(ceosName, TRUE, &error);
    FREE(error); error = NULL;

    is_uavsar = isUAVSAR(ceosName, &error);
    FREE(error); error = NULL;
    
    if (is_ceos)
      metaOut = meta_read(ceosName);
    else if (is_airsar)
      metaOut = import_airsar_meta(ceosName, ceosName, TRUE);
    else if (is_radarsat2) {
      radarsat2_meta *radarsat2 = read_radarsat2_meta(ceosName);
      if (strcmp_case(radarsat2->lineTimeOrdering, "DECREASING") == 0) 
	flip_vertical = TRUE;
      if (strcmp_case(radarsat2->pixelTimeOrdering, "DECREASING") == 0)
	flip_horizontal = TRUE;
      metaOut = radarsat2meta(radarsat2);
      FREE(radarsat2);
    }
    else if (is_terrasar) {
      terrasar_meta *terrasar = read_terrasar_meta(ceosName);
      metaOut = terrasar2meta(terrasar);
      if (metaOut->general->orbit_direction == 'A')
        flip_vertical = TRUE;
      FREE(terrasar);
    }
    else if (is_uavsar) {
      // Need to check out the dimensions
      // Our only chance to work out whether the data is MLC or GRD
      strcpy(uavsar_type, "???");
      uavsar_polsar *polsar_params = 
	read_uavsar_polsar_params(ceosName, POLSAR_MLC);
      metaOut = uavsar_polsar2meta(polsar_params);
      // Some slack for non-exact multiples
      if (line_count % metaOut->general->line_count < 5 &&
	  sample_count % metaOut->general->sample_count < 5)
	strcpy(uavsar_type, "MLC");
      else {
	FREE(polsar_params);
	meta_free(metaOut);
	polsar_params = read_uavsar_polsar_params(ceosName, POLSAR_GRD);
	metaOut = uavsar_polsar2meta(polsar_params);
	// Some slack for non-exact multiples
	if (line_count % metaOut->general->line_count < 5 &&
	    sample_count % metaOut->general->sample_count < 5)
	  strcpy(uavsar_type, "GRD");
      }
      FREE(polsar_params);
      
    }
    else
      asfPrintError("Ancillary file is in an unsupported format (required):"
		    "\n%s\n", ceosName);
    
    if (line_count != metaOut->general->line_count ||
	sample_count != metaOut->general->sample_count) {
      azimuth_scale = 1.0 / (metaOut->general->line_count / line_count);
      range_scale = 1.0 / (metaOut->general->sample_count / sample_count);
      p_azimuth_scale = &azimuth_scale;
      p_range_scale = &range_scale;
      if (!FLOAT_EQUIVALENT(azimuth_scale, range_scale))
	multilook = TRUE;
    }
    else {
      azimuth_scale = range_scale = 1.0;
      p_azimuth_scale = &azimuth_scale;
      p_range_scale = &range_scale;
      multilook = FALSE;
    }

    // Ingest the data to generate an amplitude image (in case the
    // user wants to terrain correct. Will need to get the metadata anyway
    if (is_ceos) {
      asfPrintStatus("   Ingesting CEOS data ...\n");
      *p_azimuth_scale *= metaOut->sar->azimuth_look_count;
      import_ceos(ceosName, outBaseName, "none", NULL, p_range_scale,
		  p_azimuth_scale, NULL, 0, 0, -99, -99, NULL, r_AMP, FALSE,
		  FALSE, FALSE, -1, -1, TRUE, FALSE);
    }
    else if (is_airsar) {
      asfPrintStatus("   Ingesting AirSAR data ...\n");
      ingest_airsar_polsar_amp(ceosName, outBaseName,
			       p_range_scale, p_azimuth_scale);
    }
    else if (is_radarsat2) {
      asfPrintStatus("   Ingesting Radarsat-2 data ...\n");
      ingest_radarsat2_polsar_amp(ceosName, outBaseName,
				  p_range_scale, p_azimuth_scale);
    }
    else if (is_terrasar) {
      asfPrintStatus("   Ingesting TerraSAR-X data ...\n");
      ingest_terrasar_polsar_amp(ceosName, outBaseName,
				 p_range_scale, p_azimuth_scale);
    }
    else if (is_uavsar) {
      if (strcmp_case(uavsar_type, "MLC") == 0) {
	asfPrintStatus("   Ingesting UAVSAR MLC data ...\n");
	ingest_uavsar_polsar_amp(ceosName, outBaseName, uavsar_type,
				 p_range_scale, p_azimuth_scale);
      }
      else if (strcmp_case(uavsar_type, "GRD") == 0) {
	asfPrintStatus("   Ingesting UAVSAR GRD data ...\n");
	ingest_uavsar_polsar_amp(ceosName, outBaseName, uavsar_type,
				 p_range_scale, p_azimuth_scale);
      }
      else
	asfPrintError("Unsupported UAVSAR data type!\n");
    }

    // Read the PolSAR Pro data into the layer stack
    if (metaOut)
      meta_free(metaOut);
    metaOut = meta_read(outBaseName);
    if (!is_polsarpro_matrix && !is_polsarpro_decomposition)
      strcat(metaOut->general->bands, ",POLSARPRO");
  }
  else {
    if (metaOut)
      meta_free(metaOut);
    metaOut = envi2meta(envi);
    metaOut->general->band_count = band_count;
    if (strcmp_case(image_data_type, "POLARIMETRIC_PARAMETER") == 0 ||
	strcmp_case(image_data_type, "POLARIMETRIC_SEGMENTATION") == 0)
      strcpy(metaOut->general->bands, "POLSARPRO");
    else
      strcpy(metaOut->general->bands, "");
    strcpy(metaOut->general->basename, polsarName);
    if (colormapName && strlen(colormapName) &&
	strcmp_case(image_data_type, "POLARIMETRIC_PARAMETER") == 0) {
      calc_minmax_polsarpro(polsarName, &min, &max);
      slope = 255 / (max-min);
      offset = -slope * min;
      metaOut->general->data_type = ASF_BYTE;
    }
  }

  // add some padding, since the sizes could be off by 1 due to roundoff
  int len = metaOut->general->sample_count > metaIn->general->sample_count ?
            metaOut->general->sample_count : metaIn->general->sample_count;
  floatBuf = (float *) MALLOC(sizeof(float)*len);
  if (flip_horizontal)
    tmp = (float *) MALLOC(sizeof(float)*len);

  for (band=0; band<band_count; band++) {
    if (band == 0 && metaIn->projection)
      fpOut = FOPEN(outName, "wb");
    else
      fpOut = FOPEN(outName, "ab");
    if (band_count == 1) {
      asfPrintStatus("\n   Ingesting PolSARPro data (%s) ...\n",
		     bands[band]);
      sprintf(polsarName, "%s", bands[band]);
    }
    else {
      if (is_polsarpro_matrix) {
	asfPrintStatus("\n   Ingesting PolSARPro matrix file (%s) ...\n", 
		       bands[band]);
	sprintf(polsarName, "%s%c%s.bin", s, DIR_SEPARATOR, bands[band]);
      }
      else if (is_polsarpro_decomposition) {
	asfPrintStatus("\n   Ingesting PolSARPro decomposition file (%s) ..."
		       "\n", bands[band]);
	if (strlen(dirName) > 0)
	  sprintf(polsarName, "%s%c%s.bin", 
		  dirName, DIR_SEPARATOR, bands[band]);
	else
	  sprintf(polsarName, "%s.bin", bands[band]);
      }
      if (band == 0 && strlen(metaOut->general->bands) <= 0)
	sprintf(bandStr, "%s", bands[band]);
      else
	sprintf(bandStr, ",%s", bands[band]);
      strcat(metaOut->general->bands, bandStr);
    }
    if (flip_horizontal)
      asfPrintStatus("   Data will be flipped horizontally while ingesting!"
		     "\n");
    if (flip_vertical)
      asfPrintStatus("   Data will be flipped vertically while ingesting!\n");

    fpIn = FOPEN(polsarName, "rb");

    // Do the ingest...
    for (ii=0; ii<metaIn->general->line_count; ii++) {
      int kk;
      for (kk=0; kk<len; kk++)
        floatBuf[kk] = 0.0;
      if (flip_vertical)
	get_float_line(fpIn, metaIn, metaIn->general->line_count-ii-1, 
		       floatBuf);
      else
	get_float_line(fpIn, metaIn, ii, floatBuf);
      for (kk=0; kk<metaIn->general->sample_count; kk++) {
	ieee_big32(floatBuf[kk]);
	if (colormapName && strlen(colormapName) &&
	    strcmp_case(image_data_type, "POLARIMETRIC_PARAMETER") == 0 &&
	    metaOut->general->data_type == ASF_BYTE) {
	  fValue = slope * floatBuf[kk] + offset;
	  floatBuf[kk] = fValue;
	}
	if (is_polsarpro_decomposition && db_flag) {
	  fValue = floatBuf[kk];
	  if (fValue <= EPS)
	    fValue = EPS;
	  floatBuf[kk] = 10*log10(fValue);
	} 
      }
      if (flip_horizontal) {
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  tmp[kk] = floatBuf[kk];
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  floatBuf[kk] = tmp[metaIn->general->sample_count-kk-1];
      }
      if (ii < metaOut->general->line_count) 
        put_float_line(fpOut, metaOut, ii, floatBuf);
      asfLineMeter(ii, metaIn->general->line_count);
    }

    // If there is a colormap associated with the file, then add it to the 
    // metadata as well
    if (colormapName && strlen(colormapName)) {
      apply_polsarpro_palette_to_metadata(colormapName, metaOut);
    }

    FCLOSE(fpIn);
    FCLOSE(fpOut);
  }
  //FCLOSE(fpOut);
  FREE(floatBuf);
  if (envi->band_name)
    FREE(envi->band_name);
  FREE(envi);
  if (metaOut->sar)
    metaOut->sar->multilook = multilook;
  for (ii=0; ii<band_count; ++ii)
    FREE(bands[ii]);
  FREE(bands);

  // Update image data type
  if (strcmp_case(image_data_type, "POLARIMETRIC_SEGMENTATION") == 0) {
    metaOut->general->image_data_type = POLARIMETRIC_SEGMENTATION;
    if (metaOut->projection)
      strcpy(metaOut->general->bands, "POLSARPRO");
  }
  else if (strcmp_case(image_data_type, "POLARIMETRIC_DECOMPOSITION") == 0)
    metaOut->general->image_data_type = POLARIMETRIC_DECOMPOSITION;
  else if (strcmp_case(image_data_type, "POLARIMETRIC_PARAMETER") == 0)
    metaOut->general->image_data_type = POLARIMETRIC_PARAMETER;
  else if (strcmp_case(image_data_type, "POLARIMETRIC_MATRIX") == 0) {
    if (strcmp_case(matrixType, "C2") == 0)
      metaOut->general->image_data_type = POLARIMETRIC_C2_MATRIX;
    else if (strcmp_case(matrixType, "C3") == 0)
      metaOut->general->image_data_type = POLARIMETRIC_C3_MATRIX;
    else if (strcmp_case(matrixType, "C4") == 0)
      metaOut->general->image_data_type = POLARIMETRIC_C4_MATRIX;
    else if (strcmp_case(matrixType, "T3") == 0)
      metaOut->general->image_data_type = POLARIMETRIC_T3_MATRIX;
    else if (strcmp_case(matrixType, "T4") == 0)
      metaOut->general->image_data_type = POLARIMETRIC_T4_MATRIX;
  }
  else
    metaOut->general->image_data_type = POLARIMETRIC_IMAGE;
  metaOut->general->band_count = band_count;
  if (!metaIn->projection)
    metaOut->general->band_count++;
  meta_write(metaOut, outBaseName);
  if (metaIn)
    meta_free(metaIn);
  if (metaOut)
    meta_free(metaOut);
}

// reads the appropriate look up table into a metadata colormap
// structure
#define MAX_JASC_LUT_DN 256
void apply_polsarpro_palette_to_metadata(const char *lut_basename, meta_parameters *imd)
{
  char *p = NULL;
  FILE *fp = NULL;
  int num_elements = 0;
  unsigned char * lut_buffer;
  if (!lut_basename) return;

  // Check LUT file validity and allocate appropriately sized buffer
  // to read into
  char magic_str[1024];
  char version_s[1024];
  char num_elements_s[1024];
  char lut_path[1024];
  sprintf(lut_path, "%s%clook_up_tables%c%s.pal", get_asf_share_dir(),
          DIR_SEPARATOR, DIR_SEPARATOR, lut_basename);
  fp = (FILE*)FOPEN(lut_path, "rt");
  p = fgets(magic_str, 1024, fp);
  if (!p){
    FCLOSE(fp);
    return; // eof
  }
  p = fgets(version_s, 1024, fp);
  if (!p){
    FCLOSE(fp);
    return; // eof
  }
  p = fgets(num_elements_s, 1024, fp);
  FCLOSE(fp);
  if (!p){
    return; // eof
  }
  int version = atoi(version_s);
  num_elements = atoi(num_elements_s);
  if (strncmp(magic_str, "JASC", 4) != 0) return;
  if (version != 100) return;
  if (num_elements <= 0 || num_elements > (2*MAX_JASC_LUT_DN)) return;
  if (num_elements > MAX_JASC_LUT_DN) {
    asfPrintWarning("PolSARpro look-up table contains more than 256 elements (%d).\n"
        "Only the first %d will be read and mapped to data.\n", MAX_JASC_LUT_DN);
  }
  lut_buffer = (unsigned char*)MALLOC(sizeof(unsigned char) * 3 * MAX_LUT_DN);

  // Read the LUT
  read_lut(lut_path, lut_buffer);

  // Populate the metadata colormap
  if (!imd->colormap) imd->colormap = meta_colormap_init();

  // Fill in the bands that this colormap should be applied to.
  // For the moment we are calling all PolSARpro band POLSARPRO but at some
  // stage we might to be more specific.
  strcpy(imd->colormap->band_id, "POLSARPRO");

  imd->colormap->num_elements = (num_elements <= MAX_JASC_LUT_DN) ? num_elements : MAX_JASC_LUT_DN;
  imd->colormap->rgb = (meta_rgb*)CALLOC(imd->colormap->num_elements, sizeof(meta_rgb));
  sprintf(imd->colormap->look_up_table, "%s.pal", lut_basename);
  int i;
  for (i = 0; i < imd->colormap->num_elements; i++) {
    imd->colormap->rgb[i].red   = lut_buffer[i*3];
    imd->colormap->rgb[i].green = lut_buffer[i*3+1];
    imd->colormap->rgb[i].blue  = lut_buffer[i*3+2];
  }
}

