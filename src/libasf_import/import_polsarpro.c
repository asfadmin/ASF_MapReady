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
  FREE(value);
  FREE(header);
  FREE(band_data);

  return (L_airsar || C_airsar || P_airsar);
}

int isTerrasar(char *dataFile)
{
  int found = FALSE;
  char *satellite = NULL;
  satellite = (char *) MALLOC(sizeof(char)*25);
  FILE *fp;
  fp = fopen(dataFile, "r");
  xmlDoc *doc = xmlReadFile(dataFile, NULL, 0);
  if (doc)
    strcpy(satellite, xml_get_string_value(doc, 
      "level1Product.productInfo.missionInfo.mission"));
  if (satellite &&
      strncmp_case(satellite, "TSX", 3) == 0)
    found = TRUE;
  fclose(fp);
  xmlFreeDoc(doc);
  xmlCleanupParser();

  return found;
}

int isRadarsat2(char *dataFile)
{
  int found = FALSE;
  char *satellite = (char *) MALLOC(sizeof(char)*25);
  FILE *fp;
  fp = fopen(dataFile, "r");
  xmlDoc *doc = xmlReadFile(dataFile, NULL, 0);
  if (doc)
    strcpy(satellite, 
	   xml_get_string_value(doc, "product.sourceAttributes.satellite"));
  if (satellite &&
      strcmp_case(satellite, "RADARSAT-2") == 0)
    found = TRUE;
  fclose(fp);
  xmlFreeDoc(doc);
  xmlCleanupParser();

  return found;
}

static char *fileName(char *path, char *file)
{
  char *file_name = (char *) MALLOC(sizeof(char)*1024);
  strcpy(file_name, "");
  if (strlen(path)>0) {
    if (path[strlen(path)-1] != DIR_SEPARATOR)
      sprintf(file_name, "%s%c", path, DIR_SEPARATOR);
  }
  else
    sprintf(file_name, "%s", path);
  strcat(file_name, file);
  
  return file_name;
}

static void checkFile(char *type, char *path, char *file, char *matrix)
{
  char file_name[1024];
  sprintf(file_name, "%s", fileName(path, file));
  if (!fileExists(file_name))
    asfPrintError("%s file (%s) of %s matrix element missing.\n",
		  type, file, matrix);
}

int isPolsarproMatrix(char *dataFile, char **matrixType)
{
  int matrix = FALSE;
  char *path;
  if (!dataFile || strlen(dataFile)<=0)
    return FALSE;
  if (is_dir(dataFile))
    path = STRDUP(dataFile);
  else
    path = get_dirname(dataFile);
  if (strlen(path)<=0 || strcmp(path, "./") == 0) 
    path = g_get_current_dir();
  char *p = path, *q = path;
  while (q) {
    if ((q = strchr(p, DIR_SEPARATOR)) != NULL)
      p = q+1;
  }
  int ii;
  char *directory = STRDUP(p);
  char headerFile[1024];
  // check files for coherency matrix T3
  if (strcmp_case(directory, "T3") == 0) {
    for (ii=0; ii<9; ii++) {
      checkFile("Data", path, t3_matrix[ii], directory);
      sprintf(headerFile, "%s.hdr", t3_matrix[ii]);
      checkFile("Header", path, headerFile, directory);
    }
    matrix = TRUE;
  }
  // check files for coherency matrix T4
  else if (strcmp_case(directory, "T4") == 0) {
    for (ii=0; ii<16; ii++) {
      checkFile("Data", path, t4_matrix[ii], directory);
      sprintf(headerFile, "%s.hdr", t4_matrix[ii]);
      checkFile("Header", path, headerFile, directory);
    }
    matrix = TRUE;
  }
  // check files for covariance matrix C2
  else if (strcmp_case(directory, "C2") == 0) {
    for (ii=0; ii<4; ii++) {
      checkFile("Data", path, c2_matrix[ii], directory);
      sprintf(headerFile, "%s.hdr", c2_matrix[ii]);
      checkFile("Header", path, headerFile, directory);
    }
    matrix = TRUE;
  }
  // check files for covariance matrix C3
  else if (strcmp_case(directory, "C3") == 0) {
    for (ii=0; ii<16; ii++) {
      checkFile("Data", path, c3_matrix[ii], directory);
      sprintf(headerFile, "%s.hdr", c3_matrix[ii]);
      checkFile("Header", path, headerFile, directory);
    }
    matrix = TRUE;
  }
  // check files for covariance matrix C4
  else if (strcmp_case(directory, "C4") == 0) {
    for (ii=0; ii<16; ii++) {
      checkFile("Data", path, c4_matrix[ii], directory);
      sprintf(headerFile, "%s.hdr", c4_matrix[ii]);
      checkFile("Header", path, headerFile, directory);
    }
    matrix = TRUE;
  }
  *matrixType = directory;

  return matrix;
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

void import_polsarpro(char *s, char *ceosName, char *colormapName,
                      char *image_data_type, char *outBaseName)
{
  meta_parameters *metaIn = NULL, *metaOut = NULL;
  envi_header *envi;
  FILE *fpIn, *fpOut;
  float *floatBuf, *tmp = NULL;
  double *p_azimuth_scale = NULL, *p_range_scale = NULL;
  double azimuth_scale, range_scale;
  char enviName[1024], outName[1024], *matrixType, bandStr[50];
  int ii, multilook = FALSE, need_ieee_big32, matrix = FALSE, flip_horizontal = FALSE;
  char *polsarName = (char *) MALLOC(sizeof(char)*(strlen(s) + 20));
  sprintf(polsarName, "%s", s);

  // Check whether the PolSARPro data is a matrix
  if (strcmp_case(image_data_type, "POLARIMETRIC_MATRIX") == 0)
    matrix = TRUE;
  int is_polsarpro_matrix = 
    matrix ? isPolsarproMatrix(polsarName, &matrixType) : 0;
  if (is_polsarpro_matrix) {
    // Need to get the metadata information from the first element of the matrix
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

  sprintf(outName, "%s.img", outBaseName);
  sprintf(enviName, "%s.hdr", polsarName);
  envi = read_envi(enviName);
  int line_count = envi->lines;
  int sample_count = envi->samples;

  int band, band_count;
  char **bands;
  if (strcmp_case(image_data_type, "POLARIMETRIC_MATRIX") == 0) {
    if (strcmp(matrixType, "T3") == 0) {
      band_count = 9;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	char *band_name = STRDUP(t3_matrix[band]);
	char *p = strchr(band_name, '\.');
	p[0] = '\0';
	sprintf(bands[band], "%s", band_name);
      }
    }
    else if (strcmp(matrixType, "T4") == 0) {
      band_count = 16;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	sprintf(bands[band], "%s", t4_matrix[band]);
      }
    }
    else if (strcmp(matrixType, "C2") == 0) {
      band_count = 4;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	sprintf(bands[band], "%s", c2_matrix[band]);
      }
    }
    else if (strcmp(matrixType, "C3") == 0) {
      band_count = 9;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	sprintf(bands[band], "%s", c3_matrix[band]);
      }
    }
    else if (strcmp(matrixType, "C4") == 0) {
      band_count = 16;
      bands = (char **) MALLOC(sizeof(char *)*band_count);
      for (band=0; band<band_count; band++) {
	bands[band] = (char *) MALLOC(sizeof(char)*1024);
	sprintf(bands[band], "%s", c4_matrix[band]);
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
      asfPrintError("Please add CEOS or AIRSAR ancillary files to the "
		    "PolSARpro\ninput files as appropriate\n");

    int is_airsar = FALSE, is_ceos = FALSE;
    int is_radarsat2 = FALSE, is_terrasar = FALSE;
   
    is_airsar = isAIRSAR(ceosName);
    is_ceos = isCEOS(ceosName);
    is_radarsat2 = isRadarsat2(ceosName);
    is_terrasar = isTerrasar(ceosName);
    
    if (is_ceos)
      metaOut = meta_read(ceosName);
    else if (is_airsar)
      metaOut = import_airsar_meta(ceosName, ceosName, TRUE);
    else if (is_radarsat2) {
      radarsat2_meta *radarsat2 = read_radarsat2_meta(ceosName);
      if (strcmp_case(radarsat2->dataType, "COMPLEX") != 0)
	asfPrintError("Wrong data type!\nPolarimetric processing requires SLC "
		      "data!\n");
      if (strcmp_case(radarsat2->pixelTimeOrdering, "DECREASING") == 0)
	flip_horizontal = TRUE;
      metaOut = radarsat2meta(radarsat2);
      FREE(radarsat2);
    }
    else if (is_terrasar) {
      terrasar_meta *terrasar = read_terrasar_meta(ceosName);
      if (strcmp_case(terrasar->imageDataType, "COMPLEX") != 0)
	asfPrintError("Wrong data type!\nPolarimetric processing requires SLC "
		      "data!\n");
      if (strcmp_case(terrasar->imageDataFormat, "COSAR") != 0)
	asfPrintError("Currently only COSAR data format supported!\n");
      metaOut = terrasar2meta(terrasar);
      FREE(terrasar);
    }
    else
      asfPrintError("Ancillary file is not CEOS or AIRSAR format (required):"
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

    // Ingest the data to generate an amplitude image (in case the
    // user wants to terrain correct. Will need to get the metadata anyway
    if (is_ceos) {
      asfPrintStatus("   Ingesting CEOS data ...\n");
      import_ceos(ceosName, outBaseName, "none", NULL, p_range_scale,
		  p_azimuth_scale, NULL, 0, 0, -99, -99, NULL, r_AMP, FALSE,
		  FALSE, FALSE, TRUE, FALSE);
    }
    else if (is_airsar) {
      asfPrintStatus("   Ingesting AirSAR data ...\n");
      ingest_airsar_polsar_amp(ceosName, outBaseName,
			       p_range_scale, p_azimuth_scale);
    }
    // FIXME: No scaling or multilooking yet
    else if (is_radarsat2) {
      asfPrintStatus("   Ingesting Radarsat-2 data ...\n");
      import_radarsat2(ceosName, r_AMP, outBaseName, TRUE);
    }
    // FIXME: No scaling or multilooking yet
    else if (is_terrasar) {
      asfPrintStatus("   Ingesting TerraSAR-X data ...\n");
      import_terrasar(ceosName, r_AMP, outBaseName, TRUE);
    }

    // Read the PolSAR Pro data into the layer stack
    metaOut = meta_read(outBaseName);
    if (strcmp_case(image_data_type, "POLARIMETRIC_MATRIX") != 0)
      strcat(metaOut->general->bands, ",POLSARPRO");
  }
  else {
    metaOut = envi2meta(envi);
    strcpy(metaOut->general->bands, "");
  }
  floatBuf = (float *) MALLOC(sizeof(float)*metaOut->general->sample_count);
  if (flip_horizontal)
    tmp = (float *) MALLOC(sizeof(float)*metaOut->general->sample_count);

  for (band=0; band<band_count; band++) {
    if (band == 0 && metaIn->projection)
      fpOut = FOPEN(outName, "wb");
    else
      fpOut = FOPEN(outName, "ab");
    if (band_count == 1) {
      asfPrintStatus("\n   Ingesting PolSARPro data ...\n");
      sprintf(polsarName, "%s", bands[band]);
    }
    else {
      asfPrintStatus("\n   Ingesting PolSARPro matrix file (%s) ...\n", bands[band]);
      if (flip_horizontal)
        asfPrintStatus("   Data will be flipped horizontally while ingesting!\n");
      if (band == 0 && strlen(metaOut->general->bands) <= 0)
	sprintf(bandStr, "%s", bands[band]);
      else
	sprintf(bandStr, ",%s", bands[band]);
      strcat(metaOut->general->bands, bandStr);
      sprintf(polsarName, "%s%c%s.bin", s, DIR_SEPARATOR, bands[band]);
    }
    fpIn = FOPEN(polsarName, "rb");

    // Check endianess from ENVI header file
    if (envi->byte_order)
      need_ieee_big32 = 0;
    else
      need_ieee_big32 = 1;
    
    // Do the ingest...
    for (ii=0; ii<metaOut->general->line_count; ii++) {
      get_float_line(fpIn, metaIn, ii, floatBuf);
      int kk;
      if (need_ieee_big32) {
	for (kk=0; kk<metaOut->general->sample_count; kk++) 
	  ieee_big32(floatBuf[kk]);
      }
      if (flip_horizontal) {
	for (kk=0; kk<metaOut->general->sample_count; kk++)
	  tmp[kk] = floatBuf[kk];
	for (kk=0; kk<metaOut->general->sample_count; kk++)
	  floatBuf[kk] = tmp[metaOut->general->sample_count-kk-1];
      }
      put_float_line(fpOut, metaOut, ii, floatBuf);
      asfLineMeter(ii, metaOut->general->line_count);
    }

    // If there is a colormap associated with the file, then add it to the metadata
    // as well
    if (colormapName && strlen(colormapName)) {
      apply_polsarpro_palette_to_metadata(colormapName, metaOut);
    }

    FCLOSE(fpIn);
  }
  FCLOSE(fpOut);
  FREE(floatBuf);
  FREE(envi);
  if (metaOut->sar)
    metaOut->sar->multilook = multilook;

  // Update image data type
  if (strcmp_case(image_data_type, "POLARIMETRIC_SEGMENTATION") == 0)
    metaOut->general->image_data_type = POLARIMETRIC_SEGMENTATION;
  else if (strcmp_case(image_data_type, "POLARIMETRIC_DECOMPOSITION") == 0)
    metaOut->general->image_data_type = POLARIMETRIC_DECOMPOSITION;
  else if (strcmp_case(image_data_type, "POLARIMETRIC_PARAMETERS") == 0)
    metaOut->general->image_data_type = POLARIMETRIC_PARAMETERS;
  else if (strcmp_case(image_data_type, "POLARIMETRIC_MATRIX") == 0)
    metaOut->general->image_data_type = POLARIMETRIC_MATRIX;
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

