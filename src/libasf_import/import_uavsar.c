#include "uavsar.h"
#include "airsar.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "asf_import.h"

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>

#define SQR(x) (x*x)
char **get_uavsar_products(const char *data_type, char *type, int *num_product)
{  
  char *rest, *token;
  int ii, product_count = 0;
  char *tmp = (char *) MALLOC(sizeof(char)*60);
  strcpy(tmp, data_type);

  if (strcmp_case(type, "POLSAR") == 0) {
    product_count = 5;
    if (strcmp_case(tmp, "ALL") == 0)
      sprintf(tmp, "SLC,MLC,DAT,GRD,HGT");
  }
  else if (strcmp_case(type, "INSAR") == 0) {
    product_count = 9;
    if (strcmp_case(tmp, "ALL") == 0)
      sprintf(tmp, "AMP,INT,UNW,COR,AMP_GRD,INT_GRD,UNW_GRD,COR_GRD,HGT_GRD");
  }
  else
  	asfPrintError("Could not identify data type!");

  char **product = (char **) MALLOC(sizeof(char*) * product_count);
  for (ii = 0; ii < product_count; ii++) {
    product[ii] = (char *) MALLOC(sizeof(char)*10);
    strcpy(product[ii], "");
  }

  ii = 0;
  while ((token = STRTOK_R(tmp, ",", &rest))) {
    strcpy(product[ii], token);
    tmp = rest;
    ii++;
  }

  *num_product = ii;

  return product;
}

int check_file(const char *path, char *line, char **fileName)
{
  char *p, *r;
  p = strchr(line, '=');
  char *file = (char *) MALLOC(sizeof(char)*255);
  strcpy(file, p+1);
  r = strchr(file, ';');
  if (r)
    r[0] = '\0';
  *fileName = MALLOC(sizeof(char)*(strlen(path) + strlen(file) + 32));
  strcpy(*fileName, path);
  char *trimmed_file = trim_spaces(file);
  strcat(*fileName, trimmed_file);
  FREE(file);
  FREE(trimmed_file);
  //  Removing size check-- will confuse user if they have file with
  //  the wrong size, better to let them process and have it fail with
  //  "read past end of file" error.
  //p = strchr(line, ';');
  //long long size;
  //sscanf(p+12, "%lld", &size);
  if (fileExists(*fileName)) // && fileSize(*fileName) == size)
    return TRUE;
  else
    return FALSE;
}

void
get_uavsar_file_names(const char *dataFile, uavsar_type_t type,
                      char ***pDataName, char ***pElement,
                      int **pDataType, int *nBands)
{
  int ii, slc = 0, mlc = 0, dat = 0, grd = 0, hgt = 0, kmz = 0;
  int igram = 0, unw = 0, cor = 0, amp = 0;
  int igram_grd = 0, unw_grd = 0, cor_grd = 0, amp_grd = 0, hgt_grd = 0;
  int igram_grd_kmz = 0, unw_grd_kmz = 0, cor_grd_kmz = 0, amp_grd_kmz = 0, hgt_grd_kmz = 0;

  char *file = (char *) MALLOC(sizeof(char) * 1024);
  char **dataName = (char **) MALLOC(6 * sizeof(char *));
  char **element = (char **) MALLOC(6 * sizeof(char *));
  int *dataType = (int *) MALLOC(6 * sizeof(int));
  for (ii = 0; ii < 6; ii++) {
    dataName[ii] = (char *) MALLOC(sizeof(char) * 512);
    element[ii] = (char *) MALLOC(sizeof(char) * 10);
  }
  *pDataName = dataName;
  *pElement = element;
  *pDataType = dataType;
  *nBands = 0;

  char *path = get_dirname(dataFile);

  char line[MAX_LINE], key[MAX_LINE], value[MAX_LINE];
  FILE *fp = FOPEN(dataFile, "r");
  while (fgets(line, MAX_LINE, fp)) {
    if(!parse_annotation_line(line, key, value)) {
      asfPrintWarning("Unable to parse line in annotation file: %s", line);
      continue;
    }
    if (!strcmp(key, ""))
      continue;
    if (type == POLSAR_SLC) {
      if (!strcmp(key, "slcHH")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[slc], file);
          strcpy(element[slc], "HH");
          dataType[slc] = 1;
          slc++;
        }
      }
      else if (!strcmp(key, "slcHV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[slc], file);
          strcpy(element[slc], "HV");
          dataType[slc] = 1;
          slc++;
        }
      }
      else if (!strcmp(key, "slcVH")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[slc], file);
          strcpy(element[slc], "VH");
          dataType[slc] = 1;
          slc++;
        }
      }
      else if (!strcmp(key, "slcVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[slc], file);
          strcpy(element[slc], "VV");
          dataType[slc] = 1;
          slc++;
        }
      }
      *nBands = slc;
    }
    else if (type == POLSAR_MLC) {
      if (!strcmp(key, "mlcHHHH")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C11");
          dataType[mlc] = 0;
          mlc++;
        }
      }
      else if (!strcmp(key, "mlcHVHV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C22");
          dataType[mlc] = 0;
          mlc++;
        }
      }
      else if (!strcmp(key, "mlcVVVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C33");
          dataType[mlc] = 0;
          mlc++;
        }
      }
      else if (!strcmp(key, "mlcHHHV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C12");
          dataType[mlc] = 1;
          mlc++;
        }
      }
      else if (!strcmp(key, "mlcHHVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C13");
          dataType[mlc] = 1;
          mlc++;
        }
      }
      else if (!strcmp(key, "mlcHVVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C23");
          dataType[mlc] = 1;
          mlc++;
        }
      }
      *nBands = mlc;
    }
    else if (type == POLSAR_DAT) {
      if (!strcmp(key, "dat")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[dat], file);
          dat++;
        }
      }
      *nBands = dat;
    }
    else if (type == POLSAR_GRD) {
      if (!strcmp(key, "grdHHHH")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C11");
          dataType[grd] = 0;
          grd++;
        }
      }
      else if (!strcmp(key, "grdHVHV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C22");
          dataType[grd] = 0;
          grd++;
        }
      }
      else if (!strcmp(key, "grdVVVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C33");
          dataType[grd] = 0;
          grd++;
        }
      }
      else if (!strcmp(key, "grdHHHV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C12");
          dataType[grd] = 1;
          grd++;
        }
      }
      else if (!strcmp(key, "grdHHVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C13");
          dataType[grd] = 1;
          grd++;
        }
      }
      else if (!strcmp(key, "grdHVVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C23");
          dataType[grd] = 1;
          grd++;
        }
      }
      *nBands = grd;
    }
    else if (type == POLSAR_HGT) {
      if (!strcmp(key, "hgt")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[hgt], file);
          strcpy(element[hgt], "HH");
          dataType[hgt] = 0;
          hgt++;
        }
      }
      *nBands = hgt;
    }
    else if (type == POLSAR_KMZ) {
      if (!strcmp(key, "kmz")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[kmz], file);
          strcpy(element[kmz], "HH");
          dataType[kmz] = 1;
          kmz++;
        }
      }
      *nBands = kmz;
    }
    else if (type == INSAR_AMP) {
      if (!strcmp(key, "Slant Range Amplitude of Pass 1")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[amp], file);
          strcpy(element[amp], "HH");
          dataType[amp] = 0;
          amp++;
        }
      }
      else if (!strcmp(key, "Slant Range Amplitude of Pass 2")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[amp], file);
          strcpy(element[amp], "HH");
          dataType[amp] = 0;
          amp++;
        }
      }
      *nBands = amp;
    }
    else if (type == INSAR_AMP_GRD) {
      if (!strcmp(key, "Ground Range Amplitude of Pass 1")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[amp_grd], file);
          strcpy(element[amp_grd], "HH");
          dataType[amp_grd] = 0;
          amp_grd++;
        }
      }
      else if (!strcmp(key, "Ground Range Amplitude of Pass 2")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[amp_grd], file);
          strcpy(element[amp_grd], "HH");
          dataType[amp_grd] = 0;
          amp_grd++;
        }
      }
      *nBands = amp_grd;
    }
    else if (type == INSAR_INT) {
      if (!strcmp(key, "Slant Range Interferogram")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[igram], file);
          strcpy(element[igram], "HH");
          dataType[igram] = 1;
          igram++;
        }
      }
      *nBands = igram;
    }
    else if (type == INSAR_INT_GRD) {
      if (!strcmp(key, "Ground Range Interferogram")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[igram_grd], file);
          strcpy(element[igram_grd], "HH");
          dataType[igram_grd] = 1;
          igram_grd++;
        }
      }
      *nBands = igram_grd;
    }
    else if (type == INSAR_UNW) {
      if (!strcmp(key, "Slant Range Unwrapped Phase")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[unw], file);
          strcpy(element[unw], "HH");
          dataType[unw] = 0;
          unw++;
        }
      }
      *nBands = unw;
    }
    else if (type == INSAR_UNW_GRD) {
      if (!strcmp(key, "Ground Range Unwrapped Phase")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[unw_grd], file);
          strcpy(element[unw_grd], "HH");
          dataType[unw_grd] = 0;
          unw_grd++;
        }
      }
      *nBands = unw_grd;
    }
    else if (type == INSAR_COR) {
      if (!strcmp(key, "Slant Range Correlation")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[cor], file);
          strcpy(element[cor], "HH");
          dataType[cor] = 0;
          cor++;
        }
      }
      *nBands = cor;
    }
    else if (type == INSAR_COR_GRD) {
      if (!strcmp(key, "Ground Range Correlation")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[cor_grd], file);
          strcpy(element[cor_grd], "HH");
          dataType[cor_grd] = 0;
          cor_grd++;
        }
      }
      *nBands = cor_grd;
    }
    else if (type == INSAR_HGT_GRD) {
      if (!strcmp(key, "DEM Used in Ground Projection")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[hgt_grd], file);
          strcpy(element[hgt_grd], "HH");
          dataType[hgt_grd] = 0;
          hgt_grd++;
        }
      }
      *nBands = hgt_grd;
    }
    else if (type == INSAR_AMP_GRD_KMZ) {
      if (!strcmp(key, "KMZ of Ground Range Amplitude of Pass 1")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[amp_grd_kmz], file);
          strcpy(element[amp_grd_kmz], "HH");
          dataType[amp_grd_kmz] = 0;
          amp_grd_kmz++;
        }
      }
      else if (!strcmp(key, "KMZ of Ground Range Amplitude of Pass 2")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[amp_grd_kmz], file);
          strcpy(element[amp_grd_kmz], "HH");
          dataType[amp_grd_kmz] = 0;
          amp_grd_kmz++;
        }
      }
      *nBands = amp_grd_kmz;
    }
    else if (type == INSAR_INT_GRD_KMZ) {
      if (!strcmp(key, "KMZ of Ground Range Interferogram")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[igram_grd_kmz], file);
          strcpy(element[igram_grd_kmz], "HH");
          dataType[igram_grd_kmz] = 1;
          igram_grd_kmz++;
        }
      }
      *nBands = igram_grd_kmz;
    }
    else if (type == INSAR_UNW_GRD_KMZ) {
      if (!strcmp(key, "KMZ of Ground Range Unwrapped Phase")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[unw_grd_kmz], file);
          strcpy(element[unw_grd_kmz], "HH");
          dataType[unw_grd_kmz] = 1;
          unw_grd_kmz++;
        }
      }
      *nBands = unw_grd_kmz;
    }
    else if (type == INSAR_COR_GRD_KMZ) {
      if (!strcmp(key, "KMZ of Ground Range Correlation")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[cor_grd_kmz], file);
          strcpy(element[cor_grd_kmz], "HH");
          dataType[cor_grd_kmz] = 0;
          cor_grd_kmz++;
        }
      }
      *nBands = cor_grd_kmz;
    }
    else if (type == INSAR_HGT_GRD_KMZ) {
      if (!strcmp(key, "KMZ of DEM Used in Ground Projection")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[hgt_grd_kmz], file);
          strcpy(element[hgt_grd_kmz], "HH");
          dataType[hgt_grd_kmz] = 0;
          hgt_grd_kmz++;
        }
      }
      *nBands = hgt_grd_kmz;
    }
  }

  FCLOSE(fp);
  FREE(file);
}

int sign(char byteBuf)
{
  if (byteBuf < 0)
    return -1;
  else
    return 1;
}

char *check_data_type(const char *inFileName)
{
  char line[MAX_LINE], key[MAX_LINE], value[MAX_LINE];
  char *type = (char *) MALLOC(sizeof(char)*25);
  strcpy(type, "");

  FILE *fp = FOPEN(inFileName, "r");
  while (fgets(line, MAX_LINE, fp)) {
    if(!parse_annotation_line(line, key, value)) {
      asfPrintWarning("Unable to parse line in annotation file: %s", line);
      continue;
    }
    if (!strcmp(key, ""))
      continue;
    if (!strcmp(key, "Acquisition Mode"))
      strcpy(type, value);
    else if (!strcmp(key, "Processing Mode"))
      strcpy(type, value);
    if (strcmp_case(type, "RPI") == 0)
      sprintf(type, "InSAR");
  }
  FCLOSE(fp);

  if (strlen(type) == 0)
    asfPrintError("Failed to detect type of UAVSAR data in %s\n", inFileName);

  return type;
}

void import_uavsar(const char *inFileName, int line, int sample, int width,
		   int height, radiometry_t radiometry,
		   const char *data_type, const char *outBaseName) {
  import_uavsar_ext(inFileName, line, sample, width, height, radiometry,
		    FALSE, data_type, outBaseName);
}

void import_uavsar_ext(const char *inFileName, int line, int sample, int width,
		       int height, radiometry_t radiometry, int firstBandOnly,
		       const char *data_type, const char *outBaseName) {

  // UAVSAR comes in two flavors: InSAR and PolSAR
  // Things look similar to AirSAR data, just organized a little different.
  // There does not seem to be a consistent identifier in the annotation file,
  // that would allow us to easily identify the data set as UAVSAR. No mention
  // of UAVSAR whatsoever.

  // The data can come in a large variety of flavors (single look versus multi-
  // look, derived magnitude and phase, etc.). I assume we can take anything or
  // nothing from this menu. The different files have different dimensions,
  // so we will need to generate several output images to accommodate that.

  // PolSAR data
  // slc - Single look complex slant range data 
  // mlc - Multilook cross product slant range data
  // dat - Compressed Stokes matrix of multilooked data
  // grd - Ground range projected (equi-rectangular) and multilooked data
  // hgt - Digital elevation model projected in projection
  // slc_mag and slc_phase - 8 bytes per pixel derived from slc
  // mlc_mag and mlc_phase - 8 bytes per pixel derived from mlc

  // InSAR data
  // int - Slant range interferogram
  // unw - Slant range unwrapped phase
  // cor - Slant range correlation
  // amp - Slant range amplitudes
  // int_grd - Ground range interferogram
  // unw_grd - Ground range unwrapped phase
  // cor_grd - Ground range correlation
  // amp_grd - Ground range amplitudes
  // hgt_grd - Digital elevation model in ground projection

  FILE *fpIn, *fpOut;
  int ii, kk, ll, nn, pp, nBands, ns, *dataType, product_count;
  int multi = FALSE;
  float *floatAmp, *floatPhase, *floatAmpBuf, re, im;
  float *floatComplexReal, *floatComplexImag;
  float *floatComplexBuf;
  char **dataName, **element, **product, tmp[50];
  char *type;
  char *outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
  uavsar_polsar *polsar_params;
  uavsar_insar *insar_params;
  meta_parameters *metaIn, *metaOut;

  type = check_data_type(inFileName);
  asfPrintStatus("   Data type: %s\n", type);
  product = get_uavsar_products(data_type, type, &product_count);
  if (product_count > 1)
    multi = TRUE;

  for (pp = 0; pp < product_count; pp++) {

    // InSAR data
    // Ground range interferogram
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "INT_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_INT_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range interferogram does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_INT_GRD);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaIn->general->sample_count;
      nn = 0;
      floatAmp = (float *) CALLOC(ns, sizeof(float));
      floatPhase = (float *) CALLOC(ns, sizeof(float));
      floatComplexBuf = (float *) CALLOC(2*ns, sizeof(float));
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      metaOut->general->band_count = 2;
      if (multi)
	outName = appendToBasename(outBaseName, "_int_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nGround range interferogram:\n");
      fpOut = FOPEN(outName, "wb");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      sprintf(metaOut->general->bands, "INTERFEROGRAM_AMP,INTERFEROGRAM_PHASE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	metaIn->general->sample_count = 2*ns;
	get_float_line(fpIn, metaIn, ii, floatComplexBuf);
	for (kk=0; kk<ns; kk++) {
	  re = floatComplexBuf[kk*2];
	  im = floatComplexBuf[kk*2+1];
	  ieee_big32(re);
	  ieee_big32(im);
	  floatAmp[kk] = hypot(re,im);
	  floatPhase[kk] = atan2_check(im,re);
	}
	put_band_float_line(fpOut, metaOut, 0, ii, floatAmp);
	put_band_float_line(fpOut, metaOut, 1, ii, floatPhase);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmp);
      FREE(floatPhase);
      FREE(floatComplexBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }
    
    // Ground range unwrapped phase
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "UNW_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_UNW_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range unwrapped phase does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_UNW_GRD);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_unw_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nGround range unwrapped phase:\n");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "UNWRAPPED_PHASE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  ieee_big32(floatAmpBuf[kk]);
	put_float_line(fpOut, metaOut, ii, floatAmpBuf);
      asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // Ground range correlation image
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "COR_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_COR_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range correlation image does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_COR_GRD);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      if (multi)
	outName = appendToBasename(outBaseName, "_cor_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nGround range correlation image:\n");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "COHERENCE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  ieee_big32(floatAmpBuf[kk]);
	put_float_line(fpOut, metaOut, ii, floatAmpBuf);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // Ground range amplitude images
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "AMP_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_AMP_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range amplitude images does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_AMP_GRD);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      metaOut->general->band_count = 2;
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_amp_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "AMP1,AMP2");
      asfPrintStatus("\nGround range amplitude images:\n");
      for (nn=0; nn<nBands; nn++) {
        char *filename = get_filename(dataName[nn]);
        asfPrintStatus("Ingesting %s ...\n", filename);
        FREE(filename);
	fpIn = FOPEN(dataName[nn], "rb");
	for (ii=0; ii<metaIn->general->line_count; ii++) {
	  get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	  for (kk=0; kk<metaIn->general->sample_count; kk++)
	    ieee_big32(floatAmpBuf[kk]);
	  put_band_float_line(fpOut, metaOut, nn, ii, floatAmpBuf);
	  asfLineMeter(ii, metaIn->general->line_count);
	}
	FCLOSE(fpIn);
      }
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // Ground range digital elevation model
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "HGT_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_HGT_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range digital elevation model does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_HGT_GRD);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_hgt_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "HEIGHT");
      asfPrintStatus("\nGround range digital elevation model:\n");
      for (nn=0; nn<nBands; nn++) {
        char *filename = get_filename(dataName[nn]);
        asfPrintStatus("Ingesting %s ...\n", filename);
        FREE(filename);
	fpIn = FOPEN(dataName[nn], "rb");
	for (ii=0; ii<metaIn->general->line_count; ii++) {
	  get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	  for (kk=0; kk<metaIn->general->sample_count; kk++)
	    ieee_big32(floatAmpBuf[kk]);
	  put_band_float_line(fpOut, metaOut, nn, ii, floatAmpBuf);
	  asfLineMeter(ii, metaIn->general->line_count);
	}
	FCLOSE(fpIn);
      }
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }
    
    // Slant range interferogram
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "INT") == 0) {
      get_uavsar_file_names(inFileName, INSAR_INT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range interferogram does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_INT);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaIn->general->sample_count;
      nn = 0;
      floatAmp = (float *) CALLOC(ns, sizeof(float));
      floatPhase = (float *) CALLOC(ns, sizeof(float));
      floatComplexBuf = (float *) CALLOC(2*ns, sizeof(float));
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_int.img");
      else
	outName = appendExt(outBaseName, ".img");
      metaOut->general->band_count = 2;
      if (multi)
	outName = appendToBasename(outBaseName, "_int.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nSlant range interferogram:\n");
      fpOut = FOPEN(outName, "wb");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      sprintf(metaOut->general->bands, "INTERFEROGRAM_AMP,INTERFEROGRAM_PHASE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	metaIn->general->sample_count = 2*ns;
	get_float_line(fpIn, metaIn, ii, floatComplexBuf);
	for (kk=0; kk<ns; kk++) {
	  re = floatComplexBuf[kk*2];
	  im = floatComplexBuf[kk*2+1];
	  ieee_big32(re);
	  ieee_big32(im);
	  floatAmp[kk] = hypot(re,im);
	  floatPhase[kk] = atan2_check(im,re);
	}
	put_band_float_line(fpOut, metaOut, 0, ii, floatAmp);
	put_band_float_line(fpOut, metaOut, 1, ii, floatPhase);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmp);
      FREE(floatPhase);
      FREE(floatComplexBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }
    
    // Slant range unwrapped phase
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "UNW") == 0) {
      get_uavsar_file_names(inFileName, INSAR_UNW, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range unwrapped phase does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_UNW);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_unw.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nSlant range unwrapped phase:\n");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "UNWRAPPED_PHASE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  ieee_big32(floatAmpBuf[kk]);
	put_float_line(fpOut, metaOut, ii, floatAmpBuf);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // Slant range correlation image
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "COR") == 0) {
      get_uavsar_file_names(inFileName, INSAR_COR, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range correlation image does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_COR);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_cor.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nSlant range correlation image:\n");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "COHERENCE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  ieee_big32(floatAmpBuf[kk]);
	put_float_line(fpOut, metaOut, ii, floatAmpBuf);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // Slant range amplitude images
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "AMP") == 0) {
      get_uavsar_file_names(inFileName, INSAR_AMP, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range amplitude images does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_AMP);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      metaOut->general->band_count = 2;
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_amp.img");
      else
	outName = appendExt(outBaseName, ".img");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "AMP1,AMP2");
      asfPrintStatus("\nSlant range amplitude images:\n");
      for (nn=0; nn<nBands; nn++) {
        char *filename = get_filename(dataName[nn]);
        asfPrintStatus("Ingesting %s ...\n", filename);
        FREE(filename);
	fpIn = FOPEN(dataName[nn], "rb");
	for (ii=0; ii<metaIn->general->line_count; ii++) {
	  get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	  for (kk=0; kk<metaIn->general->sample_count; kk++)
	    ieee_big32(floatAmpBuf[kk]);
	  put_band_float_line(fpOut, metaOut, nn, ii, floatAmpBuf);
	  asfLineMeter(ii, metaIn->general->line_count);
	}
	FCLOSE(fpIn);
      }
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // PolSAR data
    // Single look complex data
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "SLC") == 0) {
      asfPrintWarning("Ingest of SLC data is currently not supported!\n");
      /*
      get_uavsar_file_names(inFileName, POLSAR_SLC, &dataName, &element,
			    &dataType, &nBands);
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_SLC);
      metaIn = uavsar_polsar2meta(polsar_params);
      metaOut = uavsar_polsar2meta(polsar_params);
      if (multi)
	outName = appendToBasename(outBaseName, "_slc.img");
      else
	outName = appendExt(outBaseName, ".img");
      for (ii=0; ii<nBands; ii++)
	printf("file: %s\n", dataName[ii]);
      //meta_write(metaOut, outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(polsar_params);
      */
    }
    
    // Multilooked data
    if (strcmp_case(type, "PolSAR") == 0  &&
	strcmp_case(product[pp], "MLC") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_MLC, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range multilooked data do not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      if (firstBandOnly)
	nBands = 1;
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_MLC);
      metaIn = uavsar_polsar2meta(polsar_params);
      metaOut = uavsar_polsar2meta(polsar_params);
      int dbFlag = 
	(radiometry >= r_SIGMA_DB && radiometry <= r_GAMMA_DB) ? 1 : 0;
      metaOut->general->radiometry = radiometry;
      ns = metaIn->general->sample_count;
      floatAmp = (float *) MALLOC(sizeof(float)*ns);
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      floatComplexReal = (float *) MALLOC(sizeof(float)*ns);
      floatComplexImag = (float *) MALLOC(sizeof(float)*ns);
      floatComplexBuf = (float *) MALLOC(sizeof(float)*2*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      metaOut->general->band_count = ll = 0;
      create_cal_params(inFileName, metaOut, REPORT_LEVEL_NONE);
      if (multi)
	outName = appendToBasename(outBaseName, "_mlc.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nMultilooked data:\n");
      fpOut = FOPEN(outName, "wb");
      for (nn=0; nn<nBands; nn++) {
        char *filename = get_filename(dataName[nn]);
        asfPrintStatus("Ingesting %s ...\n", filename);
        FREE(filename);
	if (dataType[nn] == 0)
	  metaOut->general->band_count += 1;
	else
	  metaOut->general->band_count += 2;
	fpIn = FOPEN(dataName[nn], "rb");
	if (firstBandOnly)
	  strcpy(metaOut->general->bands, "AMP");
	else if (nn == 0)
	  sprintf(metaOut->general->bands, "%s", element[0]);
	else {
	  if (dataType[nn])
	    sprintf(tmp, ",%s_real,%s_imag", element[nn], element[nn]);
	  else
	    sprintf(tmp, ",%s", element[nn]);
	  strcat(metaOut->general->bands, tmp);
	}
	for (ii=0; ii<metaIn->general->line_count; ii++) {
	  if (dataType[nn]) {
	    metaIn->general->sample_count = 2*ns;
	    get_float_line(fpIn, metaIn, ii, floatComplexBuf);
	    for (kk=0; kk<ns; kk++) {
	      floatComplexReal[kk] = floatComplexBuf[kk*2];
	      floatComplexImag[kk] = floatComplexBuf[kk*2+1];
	      ieee_big32(floatComplexReal[kk]);
	      ieee_big32(floatComplexImag[kk]);
	    }
	    put_band_float_line(fpOut, metaOut, ll, ii, floatComplexReal);
	    put_band_float_line(fpOut, metaOut, ll+1, ii, floatComplexImag);
	  }
	  else {
	    metaIn->general->sample_count = ns;
	    get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	    if (nn == 0) {
	      for (kk=0; kk<ns; kk++) {
		ieee_big32(floatAmpBuf[kk]);
		if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB)
		  floatAmpBuf[kk] = get_cal_dn(metaOut, 0.0, kk, 
					       floatAmpBuf[kk], 
					       element[nn], dbFlag);
		else
		  floatAmpBuf[kk] = sqrt(floatAmpBuf[kk]);
	      }
	    }
	    else {
	      for (kk=0; kk<ns; kk++) {
		ieee_big32(floatAmpBuf[kk]);
		if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB)
		  floatAmpBuf[kk] = get_cal_dn(metaOut, 0.0, kk, 
					       floatAmpBuf[kk], element[nn], 
					       dbFlag);
	      }
	    }
	    put_band_float_line(fpOut, metaOut, ll, ii, floatAmpBuf);
	  }
	  asfLineMeter(ii, metaIn->general->line_count);      
	}
	if (dataType[nn])
	  ll += 2;
	else
	  ll++;
	FCLOSE(fpIn);
      }
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmp);
      FREE(floatAmpBuf);
      FREE(floatComplexReal);
      FREE(floatComplexImag);
      FREE(floatComplexBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(polsar_params);
    }    
    
    // Compressed Stokes matrix
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "DAT") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_DAT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range Stokes matrix does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_DAT);
      metaIn = uavsar_polsar2meta(polsar_params);
      metaOut = uavsar_polsar2meta(polsar_params);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_dat.img");
      else
	outName = appendExt(outBaseName, ".img");
      metaOut->general->band_count = 9;
      asfPrintStatus("\nCompressed Stokes matrix:\n");
      char *filename = get_filename(dataName[0]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      strcpy(metaOut->general->bands,
	     "AMP,GAMMA-AMP-HH,GAMMA-PHASE-HH,GAMMA-AMP-HV,GAMMA-PHASE-HV,"
	     "GAMMA-AMP-VH,GAMMA-PHASE-VH,GAMMA-AMP-VV,GAMMA-PHASE-VV");
      int ns = metaOut->general->sample_count;
      float total_power, ysca, amp, phase;
      complexFloat cpx;
      float *power = (float *) MALLOC(sizeof(float)*ns);
      float *shh_amp = (float *) MALLOC(sizeof(float)*ns);
      float *shh_phase = (float *) MALLOC(sizeof(float)*ns);
      float *shv_amp = (float *) MALLOC(sizeof(float)*ns);
      float *shv_phase = (float *) MALLOC(sizeof(float)*ns);
      float *svh_amp = (float *) MALLOC(sizeof(float)*ns);
      float *svh_phase = (float *) MALLOC(sizeof(float)*ns);
      float *svv_amp = (float *) MALLOC(sizeof(float)*ns);
      float *svv_phase = (float *) MALLOC(sizeof(float)*ns);
      char *byteBuf = (char *) MALLOC(sizeof(char)*10);

      /* DAT files start with an airsar header that mostly contains metadata we
       * already have from the annotation file. So find the start offset of the image
       * data from the header and skip the rest of the header
       */
      airsar_header *header = read_airsar_header(dataName[0]);
      fpIn = FOPEN(dataName[0], "rb");
      fpOut = FOPEN(outName, "wb");
      FSEEK(fpIn, header->first_data_offset, SEEK_SET);
      FREE(header);

      for (ii=0; ii<metaOut->general->line_count; ii++) {
	for (kk=0; kk<metaOut->general->sample_count; kk++) {
	  FREAD(byteBuf, sizeof(char), 10, fpIn);
          //float m11, m12, m13, m14, m22, m23, m24, m33, m34, m44;
	  // Scale is always 1.0 according to Bruce Chapman
	  //m11 = ((float)byteBuf[1]/254.0 + 1.5) * pow(2, byteBuf[0]);
	  //m12 = (float)byteBuf[2] * m11 / 127.0;
	  //m13 = sign(byteBuf[3]) * SQR((float)byteBuf[3] / 127.0) * m11;
	  //m14 = sign(byteBuf[4]) * SQR((float)byteBuf[4] / 127.0) * m11;
	  //m23 = sign(byteBuf[5]) * SQR((float)byteBuf[5] / 127.0) * m11;
	  //m24 = sign(byteBuf[6]) * SQR((float)byteBuf[6] / 127.0) * m11;
	  //m33 = (float)byteBuf[7] * m11 / 127.0;
	  //m34 = (float)byteBuf[8] * m11 / 127.0;
	  //m44 = (float)byteBuf[9] * m11 / 127.0;
	  //m22 = 1 - m33 -m44;
	  total_power =
	    ((float)byteBuf[1]/254.0 + 1.5) * pow(2, byteBuf[0]);
	  ysca = 2.0 * sqrt(total_power);
	  power[kk] = sqrt(total_power);
	  cpx.real = (float)byteBuf[2] * ysca / 127.0;
	  cpx.imag = (float)byteBuf[3] * ysca / 127.0;
	  amp = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
	  phase = atan2(cpx.imag, cpx.real);
	  shh_amp[kk] = amp*amp;
	  shh_phase[kk] = phase;
	  cpx.real = (float)byteBuf[4] * ysca / 127.0;
	  cpx.imag = (float)byteBuf[5] * ysca / 127.0;
	  amp = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
	  phase = atan2(cpx.imag, cpx.real);
	  shv_amp[kk] = amp*amp;
	  shv_phase[kk] = phase;
	  cpx.real = (float)byteBuf[6] * ysca / 127.0;
	  cpx.imag = (float)byteBuf[7] * ysca / 127.0;
	  amp = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
	  phase = atan2(cpx.imag, cpx.real);
	  svh_amp[kk] = amp*amp;
	  svh_phase[kk] = phase;
	  cpx.real = (float)byteBuf[8] * ysca / 127.0;
	  cpx.imag = (float)byteBuf[9] * ysca / 127.0;
	  amp = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
	  phase = atan2(cpx.imag, cpx.real);
	  svv_amp[kk] = amp*amp;
	  svv_phase[kk] = phase;
	}
	put_band_float_line(fpOut, metaOut, 0, ii, power);
	put_band_float_line(fpOut, metaOut, 1, ii, shh_amp);
	put_band_float_line(fpOut, metaOut, 2, ii, shh_phase);
	put_band_float_line(fpOut, metaOut, 3, ii, shv_amp);
	put_band_float_line(fpOut, metaOut, 4, ii, shv_phase);
	put_band_float_line(fpOut, metaOut, 5, ii, svh_amp);
	put_band_float_line(fpOut, metaOut, 6, ii, svh_phase);
	put_band_float_line(fpOut, metaOut, 7, ii, svv_amp);
	put_band_float_line(fpOut, metaOut, 8, ii, svv_phase);
	asfLineMeter(ii, metaOut->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      FREE(power);
      FREE(shh_amp);
      FREE(shh_phase);
      FREE(shv_amp);
      FREE(shv_phase);
      FREE(svh_amp);
      FREE(svh_phase);
      FREE(svv_amp);
      FREE(svv_phase);
      meta_write(metaOut, outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(outName);
      FREE(polsar_params);
    }
    
    // Ground range projected data
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "GRD") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range projected data do not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      if (firstBandOnly)
	nBands = 1;
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_GRD);
      metaIn = uavsar_polsar2meta(polsar_params);
      metaOut = uavsar_polsar2meta(polsar_params);
      ns = metaIn->general->sample_count;
      floatAmpBuf = (float *) CALLOC(ns, sizeof(float));
      floatComplexReal = (float *) CALLOC(ns, sizeof(float));
      floatComplexImag = (float *) CALLOC(ns, sizeof(float));
      floatComplexBuf = (float *) CALLOC(2*ns, sizeof(float));
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      metaOut->general->band_count = ll = 0;
      if (multi)
	outName = appendToBasename(outBaseName, "_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nGround range projected data:\n");
      fpOut = FOPEN(outName, "wb");
      for (nn=0; nn<nBands; nn++) {
        char *filename = get_filename(dataName[nn]);
        asfPrintStatus("Ingesting %s ...\n", filename);
        FREE(filename);
	if (dataType[nn])
	  metaOut->general->band_count += 2;
	else
	  metaOut->general->band_count += 1;
	fpIn = FOPEN(dataName[nn], "rb");
	if (firstBandOnly)
	  strcpy(metaOut->general->bands, "AMP");
	else if (nn == 0)
	  sprintf(metaOut->general->bands, "%s", element[0]);
	else {
	  if (dataType[nn])
	    sprintf(tmp, ",%s_real,%s_imag", element[nn], element[nn]);
	  else
	    sprintf(tmp, ",%s", element[nn]);
	  strcat(metaOut->general->bands, tmp);
	}
	if (dataType[nn]) {
	  for (ii=0; ii<metaIn->general->line_count; ii++) {
	    metaIn->general->sample_count = 2*ns;
	    get_float_line(fpIn, metaIn, ii, floatComplexBuf);
	    for (kk=0; kk<ns; kk++) {
	      floatComplexReal[kk] = floatComplexBuf[kk*2];
	      floatComplexImag[kk] = floatComplexBuf[kk*2+1];
	      ieee_big32(floatComplexReal[kk]);
	      ieee_big32(floatComplexImag[kk]);
	    }
	    put_band_float_line(fpOut, metaOut, ll, ii, floatComplexReal);
	    put_band_float_line(fpOut, metaOut, ll+1, ii, floatComplexImag);
	    asfLineMeter(ii, metaIn->general->line_count);
	  }
	}
	else {
	  for (ii=0; ii<metaIn->general->line_count; ii++) {
	    metaIn->general->sample_count = ns;
	    get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	    for (kk=0; kk<ns; kk++)
	      ieee_big32(floatAmpBuf[kk]);
	    put_band_float_line(fpOut, metaOut, ll, ii, floatAmpBuf);
	    asfLineMeter(ii, metaIn->general->line_count);      
	  }
	}
	FCLOSE(fpIn);
	if (dataType[nn])
	  ll += 2;
	else
	  ll++;
      }
      FCLOSE(fpOut);
      create_cal_params(inFileName, metaOut, REPORT_LEVEL_NONE);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(floatComplexReal);
      FREE(floatComplexImag);
      FREE(floatComplexBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(polsar_params);
    }
    
    // Digital elevation model
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "HGT") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_HGT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Digital elevation model does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_HGT);
      metaIn = uavsar_polsar2meta(polsar_params);
      metaOut = uavsar_polsar2meta(polsar_params);
      ns = metaOut->general->sample_count;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_hgt.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nDigital elevation model:\n");
      nn=0;
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "HEIGHT");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  ieee_big32(floatAmpBuf[kk]);
	put_float_line(fpOut, metaOut, ii, floatAmpBuf);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(polsar_params);
    }    
  }
  if (strcmp_case(type, "PolSAR") == 0)
    product_count = 5;
  else if (strcmp_case(type, "InSAR") == 0)
    product_count = 9;
  for (pp=0; pp<product_count; pp++)
    FREE(product[pp]);
  FREE(product);
  FREE(type);
}

void read_meta_uavsar(const char *inFileName, const char *outBaseName) 
{
  int pp, product_count, *dataType, nBands;
  char *type = check_data_type(inFileName);
  char **product = get_uavsar_products("ALL", type, &product_count);
  char **dataName, **element;
  char *outName = (char *) MALLOC(sizeof(char)*255);
  uavsar_polsar *polsar_params;
  uavsar_insar *insar_params;
  meta_parameters *meta;
  
  for (pp = 0; pp < product_count; pp++) {

    // InSAR
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "INT_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_INT_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_INT_GRD);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_int_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "UNW_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_UNW_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_UNW_GRD);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_unw_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "COR_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_COR_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_COR_GRD);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_cor_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "AMP_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_AMP_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_AMP_GRD);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_amp_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "HGT_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_HGT_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_HGT_GRD);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_hgt_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "INT") == 0) {
      get_uavsar_file_names(inFileName, INSAR_INT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_INT);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_int.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "UNW") == 0) {
      get_uavsar_file_names(inFileName, INSAR_UNW, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_UNW);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_unw.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "COR") == 0) {
      get_uavsar_file_names(inFileName, INSAR_COR, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_COR);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_cor.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "AMP") == 0) {
      get_uavsar_file_names(inFileName, INSAR_AMP, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_AMP);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_amp.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    // PolSAR
    if (strcmp_case(type, "PolSAR") == 0  &&
	strcmp_case(product[pp], "MLC") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_MLC, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_MLC);
      meta = uavsar_polsar2meta(polsar_params);
      sprintf(outName, "%s_mlc.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "DAT") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_DAT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_DAT);
      meta = uavsar_polsar2meta(polsar_params);
      sprintf(outName, "%s_dat.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "GRD") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_GRD);
      meta = uavsar_polsar2meta(polsar_params);
      sprintf(outName, "%s_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "HGT") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_HGT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_HGT);
      meta = uavsar_polsar2meta(polsar_params);
      sprintf(outName, "%s_hgt.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
  }
  for (pp=0; pp<product_count; pp++)
    FREE(product[pp]);
  FREE(product);
  FREE(type);
}

uavsar_type_t uavsar_type_name_to_enum(const char *type_name)
{
  if(!strcmp_case(type_name, "SLC")) {
    return POLSAR_SLC;
  }
  else if(!strcmp_case(type_name, "MLC")) {
    return POLSAR_MLC;
  }
  else if(!strcmp_case(type_name, "DAT")) {
    return POLSAR_DAT;
  }
  else if(!strcmp_case(type_name, "GRD")) {
    return POLSAR_GRD;
  }
  else if(!strcmp_case(type_name, "HGT")) {
    return POLSAR_HGT;
  }
  else if(!strcmp_case(type_name, "KMZ")) {
    return POLSAR_KMZ;
  }
  else if(!strcmp_case(type_name, "AMP")) {
    return INSAR_AMP;
  }
  else if(!strcmp_case(type_name, "INT")) {
    return INSAR_INT;
  }
  else if(!strcmp_case(type_name, "UNW")) {
    return INSAR_UNW;
  }
  else if(!strcmp_case(type_name, "COR")) {
    return INSAR_COR;
  }
  else if(!strcmp_case(type_name, "AMP_GRD")) {
    return INSAR_AMP_GRD;
  }
  else if(!strcmp_case(type_name, "INT_GRD")) {
    return INSAR_INT_GRD;
  }
  else if(!strcmp_case(type_name, "UNW_GRD")) {
    return INSAR_UNW_GRD;
  }
  else if(!strcmp_case(type_name, "COR_GRD")) {
    return INSAR_COR_GRD;
  }
  else if(!strcmp_case(type_name, "HGT_GRD")) {
    return INSAR_HGT_GRD;
  }
  else if(!strcmp_case(type_name, "AMP_GRD_KMZ")) {
    return INSAR_AMP_GRD_KMZ;
  }
  else if(!strcmp_case(type_name, "INT_GRD_KMZ")) {
    return INSAR_INT_GRD_KMZ;
  }
  else if(!strcmp_case(type_name, "UNW_GRD_KMZ")) {
    return INSAR_UNW_GRD_KMZ;
  }
  else if(!strcmp_case(type_name, "COR_GRD_KMZ")) {
    return INSAR_COR_GRD_KMZ;
  }
  else if(!strcmp_case(type_name, "HGT_GRD_KMZ")) {
    return INSAR_HGT_GRD_KMZ;
  }
  else {
    asfPrintError("uavsar_type_name_to_enum() Failure: No such uavsar type name: %s", type_name);
  }
  // Not Reached
  asfPrintError("Internal error");
  return 0;
}

