#include "asf.h"
#include "ceos.h"
#include "airsar.h"
#include "uavsar.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "proj.h"
#include "asf_export.h"
#include "asf_import.h"
#include "asf_contact.h"
#include "asf_sar.h"
#include "asf_terrcorr.h"
#include "asf_geocode.h"
#include "asf_nan.h"
#include "ardop_defs.h"
#include "get_ceos_names.h"
#include "get_stf_names.h"
#include "dateUtil.h"
#include <ctype.h>
#include <string.h>
#include <sys/types.h> /* 'DIR' structure (for opendir) */
#include <dirent.h>    /* for opendir itself            */

#define UNIT_TESTS_MICRON 0.000000001
#define FLOAT_COMPARE(a, b) (abs((a) - (b)) \
			     < UNIT_TESTS_MICRON ? 1 : 0)

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
  {"Freeman2_Ground","Freeman2_Vol"};
static char *freeman3_decomposition[3] = 
  {"Freeman_Dbl","Freeman_Odd","Freeman_Vol"};
static char *vanZyl3_decomposition[3] = 
  {"VanZyl3_Dbl","VanZyl3_Odd","VanZyl3_Vol"};
static char *yamaguchi3_decomposition[3] = 
  {"Yamaguchi3_Dbl","Yamaguchi3_Odd","Yamaguchi3_Vol"};
static char *yamaguchi4_decomposition[4] = 
  {"Yamaguchi4_Dbl","Yamaguchi4_Hlx","Yamaguchi4_Odd", "Yamaguchi4_Vol"};
static char *krogager_decomposition[3] = 
  {"Krogager_Kd","Krogager_Kh","Krogager_Ks"};
static char *touzi1_decomposition[3] = 
  {"TSVM_alpha_s1","TSVM_alpha_s2","TSVM_alpha_s3"};
static char *touzi2_decomposition[3] = 
  {"TSVM_phi_s1","TSVM_phi_s2","TSVM_phi_s3"};
static char *touzi3_decomposition[3] = 
  {"TSVM_tau_m1","TSVM_tau_m2","TSVM_tau_m3"};
static char *touzi4_decomposition[3] = 
  {"TSVM_psi1","TSVM_psi2","TSVM_psi3"};
static char *touzi5_decomposition[4] = 
  {"TSVM_alpha_s","TSVM_phi_s","TSVM_tau_m","TSVM_psi"};

static char *find_decomposition(meta_parameters *meta)
{
  int ii, kk;
  char *decomposition = MALLOC(sizeof(char)*25);
  strcpy(decomposition, "");

  char **bands = extract_band_names(meta->general->bands, 
				    meta->general->band_count);

  // Freeman2
  int found_bands = 0;
  for (ii=0; ii<meta->general->band_count; ii++)
    for (kk=0; kk<2; kk++)
      if (strcmp_case(freeman2_decomposition[kk], bands[ii]) == 0)
	found_bands++;
  if (found_bands == 2) {
    strcpy(decomposition, "Freeman2");
  }

  // Freeman3
  found_bands = 0;
  for (ii=0; ii<meta->general->band_count; ii++)
    for (kk=0; kk<3; kk++)
      if (strcmp_case(freeman3_decomposition[kk], bands[ii]) == 0)
	found_bands++;
  if (found_bands == 3) {
    strcpy(decomposition, "Freeman3");
  }

  // vanZyl3
  found_bands = 0;
  for (ii=0; ii<meta->general->band_count; ii++)
    for (kk=0; kk<3; kk++)
      if (strcmp_case(vanZyl3_decomposition[kk], bands[ii]) == 0)
	found_bands++;
  if (found_bands == 3) {
    strcpy(decomposition, "VanZyl3");
  }

  // Yamaguchi3
  found_bands = 0;
  for (ii=0; ii<meta->general->band_count; ii++)
    for (kk=0; kk<3; kk++)
      if (strcmp_case(yamaguchi3_decomposition[kk], bands[ii]) == 0)
	found_bands++;
  if (found_bands == 3) {
    strcpy(decomposition, "Yamaguchi3");
  }

  // Yamaguchi4
  found_bands = 0;
  for (ii=0; ii<meta->general->band_count; ii++)
    for (kk=0; kk<4; kk++)
      if (strcmp_case(yamaguchi4_decomposition[kk], bands[ii]) == 0)
	found_bands++;
  if (found_bands == 4) {
    strcpy(decomposition, "Yamaguchi4");
  }

  // Krogager
  found_bands = 0;
  for (ii=0; ii<meta->general->band_count; ii++)
    for (kk=0; kk<3; kk++)
      if (strcmp_case(krogager_decomposition[kk], bands[ii]) == 0)
	found_bands++;
  if (found_bands == 3) {
    strcpy(decomposition, "Krogager");
  }

  // Touzi1
  found_bands = 0;
  for (ii=0; ii<meta->general->band_count; ii++)
    for (kk=0; kk<3; kk++)
      if (strcmp_case(touzi1_decomposition[kk], bands[ii]) == 0)
	found_bands++;
  if (found_bands == 3) {
    strcpy(decomposition, "Touzi1");
  }

  // Touzi2
  found_bands = 0;
  for (ii=0; ii<meta->general->band_count; ii++)
    for (kk=0; kk<3; kk++)
      if (strcmp_case(touzi2_decomposition[kk], bands[ii]) == 0)
	found_bands++;
  if (found_bands == 3) {
    strcpy(decomposition, "Touzi2");
  }

  // Touzi3
  found_bands = 0;
  for (ii=0; ii<meta->general->band_count; ii++)
    for (kk=0; kk<3; kk++)
      if (strcmp_case(touzi3_decomposition[kk], bands[ii]) == 0)
	found_bands++;
  if (found_bands == 3) {
    strcpy(decomposition, "Touzi3");
  }

  // Touzi4
  found_bands = 0;
  for (ii=0; ii<meta->general->band_count; ii++)
    for (kk=0; kk<3; kk++)
      if (strcmp_case(touzi4_decomposition[kk], bands[ii]) == 0)
	found_bands++;
  if (found_bands == 3) {
    strcpy(decomposition, "Touzi4");
  }
  
  // Touzi5
  found_bands = 0;
  for (ii=0; ii<meta->general->band_count; ii++)
    for (kk=0; kk<4; kk++)
      if (strcmp_case(touzi5_decomposition[kk], bands[ii]) == 0)
	found_bands++;
  if (found_bands == 4) {
    strcpy(decomposition, "Touzi5");
  }

  for (kk=0; kk<meta->general->band_count; ++kk)
    FREE(bands[kk]);
  FREE(bands);

  return decomposition;
}

static int strindex(char s[], char t[])
{
  int i, j, k;

  for (i=0; s[i]!='\0'; i++) {
    for (j=i, k=0; t[k]!='\0' && s[j]==t[k]; j++, k++)
      ;
    if (k>0 && t[k]=='\0')
      return i;
  }
  return -1;
}

static char *read_param(char *line)
{
  int i, k;
  char *value=(char *)CALLOC(256, sizeof(char));

  strcpy(value, "");
  i=strindex(line, "]");
  k=strindex(line, "=");
  if (i>0) strncpy(value, line, i+1);
  if (k>0) strncpy(value, line, k);
  return value;
}

static char *read_str(char *line, char *param)
{
  static char value[255];
  char *p = strchr(line, '=');

  // skip past the '=' sign, and eat up any whitespace
  ++p;
  while (isspace(*p))
      ++p;

  strcpy(value, p);

  // eat up trailing whitespace, too
  p = value + strlen(value) - 1;
  while (isspace(*p))
      *p-- = '\0';

  return value;
}

int read_polarimetry_config(char *configFile, char *decomposition,
			    char **pRed, char **pGreen, char **pBlue)
{
  char line[128], params[25];
  char *test;
  char *red = (char *) MALLOC(sizeof(char)*25);
  char *green = (char *) MALLOC(sizeof(char)*25);
  char *blue = (char *) MALLOC(sizeof(char)*25);
  int found = FALSE;

  FILE *fConfig = fopen(configFile, "r");
  if (fConfig) {
    while (fgets(line, 128, fConfig) != NULL) {
      if (strncmp(line, "[Freeman3]", 10)==0) 
	strcpy(params, "freeman3");
      if (strcmp(params, "freeman3")==0 &&
	  strcmp_case(params, decomposition) == 0) {
        test = read_param(line);
        if (strncmp(test, "red", 3)==0)
	  strcpy(red, read_str(line, "red"));
        if (strncmp(test, "green", 5)==0)
	  strcpy(green, read_str(line, "green"));
        if (strncmp(test, "blue", 4)==0)
	  strcpy(blue, read_str(line, "blue"));
        FREE(test);
	found = TRUE;
      }
      if (strncmp(line, "[VanZyl3]", 9)==0) 
	strcpy(params, "vanzyl3");
      if (strcmp(params, "vanzyl3")==0 &&
	  strcmp_case(params, decomposition) == 0) {
        test = read_param(line);
        if (strncmp(test, "red", 3)==0)
	  strcpy(red, read_str(line, "red"));
        if (strncmp(test, "green", 5)==0)
	  strcpy(green, read_str(line, "green"));
        if (strncmp(test, "blue", 4)==0)
	  strcpy(blue, read_str(line, "blue"));
        FREE(test);
	found = TRUE;
      }
      if (strncmp(line, "[Yamaguchi3]", 12)==0) 
	strcpy(params, "yamaguchi3");
      if (strcmp(params, "yamaguchi3")==0 &&
	  strcmp_case(params, decomposition) == 0) {
        test = read_param(line);
        if (strncmp(test, "red", 3)==0)
	  strcpy(red, read_str(line, "red"));
        if (strncmp(test, "green", 5)==0)
	  strcpy(green, read_str(line, "green"));
        if (strncmp(test, "blue", 4)==0)
	  strcpy(blue, read_str(line, "blue"));
        FREE(test);
	found = TRUE;
      }
      if (strncmp(line, "[Yamaguchi4]", 12)==0) 
	strcpy(params, "yamaguchi4");
      if (strcmp(params, "yamaguchi4")==0 &&
	  strcmp_case(params, decomposition) == 0) {
        test = read_param(line);
        if (strncmp(test, "red", 3)==0)
	  strcpy(red, read_str(line, "red"));
        if (strncmp(test, "green", 5)==0)
	  strcpy(green, read_str(line, "green"));
        if (strncmp(test, "blue", 4)==0)
	  strcpy(blue, read_str(line, "blue"));
        FREE(test);
	found = TRUE;
      }
      if (strncmp(line, "[Krogager]", 10)==0) 
	strcpy(params, "krogager");
      if (strcmp(params, "krogager")==0 &&
	  strcmp_case(params, decomposition) == 0) {
        test = read_param(line);
        if (strncmp(test, "red", 3)==0)
	  strcpy(red, read_str(line, "red"));
        if (strncmp(test, "green", 5)==0)
	  strcpy(green, read_str(line, "green"));
        if (strncmp(test, "blue", 4)==0)
	  strcpy(blue, read_str(line, "blue"));
        FREE(test);
	found = TRUE;
      }
    }
    FCLOSE(fConfig);
  }
  *pRed = red;
  *pGreen = green;
  *pBlue = blue;
  
  return found;
}

static int get_scheme_count(char *line, char separator)
{
  char *p = line;
  int count = 0;
  do {
    char col[25];
    p = quoted_string_parse(p, col, 25, 1, separator);
    count++;
  } while (p);
  
  return count;
}

static char *get_scheme(char *line, int col, char separator)
{
  static char ret[255];
  int ii;
  char *p = line;
  for (ii=0; ii<col; ii++)
    p = quoted_string_parse(p, ret, 25, -1, separator);

  return ret;
}

static int imgFileExists(const char *basename)
{
   char *imgFile = appendExt(basename, ".img");
   int ret = fileExists(imgFile);
   FREE(imgFile);
   return ret;
}

static char *get_out_name(char *inFile, char *project, radiometry_t radiometry,
			  char *naming_scheme)
{
  // FIXME: Currently this only works with '_' as separator
  char tmp[25], scheme[25];
  int ii;
  char sep = '_';
  char *outFile = (char *) MALLOC(sizeof(char)*512);
  strcpy(outFile, "");
  meta_parameters *meta = meta_read(inFile);
  int count = get_scheme_count(naming_scheme, sep);
  for (ii=1; ii<=count; ii++) {
    strcpy(scheme, get_scheme(naming_scheme, ii, sep));
    if (strcmp_case(scheme, "<PROJECT>") == 0)
      sprintf(tmp, "%s%c", project, sep);
    else if (strcmp_case(scheme, "<DATE>") == 0) {
      meta->general->acquisition_date[11] = '\0';
      sprintf(tmp, "%s%c", meta->general->acquisition_date, sep);
    }
    else if (strcmp_case(scheme, "<SAT>") == 0) {
      if (strcmp_case(meta->general->sensor, "ERS1") == 0)
	sprintf(tmp, "E1%c", sep);
      else if (strcmp_case(meta->general->sensor, "ERS2") == 0)
	sprintf(tmp, "E2%c", sep);
      else if (strcmp_case(meta->general->sensor, "RSAT-1") == 0)
	sprintf(tmp, "R1%c", sep);
      else if (strcmp_case(meta->general->sensor, "ALOS") == 0)
	sprintf(tmp, "A3%c", sep);
    }
    else if (strcmp_case(scheme, "<MODE>") == 0)
      sprintf(tmp, "%s%c", meta->general->mode, sep);
    else if (strcmp_case(scheme, "<ORBIT>") == 0)
      sprintf(tmp, "%d%c", meta->general->orbit, sep);
    else if (strcmp_case(scheme, "<FRAME>") == 0)
      sprintf(tmp, "%d%c", meta->general->frame, sep);
    else if (strcmp_case(scheme, "<FILE>") == 0)
      strcat(outFile, meta->general->basename);
    else if (strcmp_case(scheme, "<RADIOMETRY>") == 0) {
      sprintf(tmp, "%s%c", radiometry2str(radiometry), sep);
      if (strncmp_case(tmp, "AMPLITUDE", 9) == 0)
	sprintf(tmp, "AMP%c", sep);
    }
    strcat(outFile, tmp);
  }
  meta_free(meta);
  outFile[strlen(outFile)-1] = '\0';

  return outFile;
}

int isInSAR(const char *infile)
{
  int found = FALSE;
  meta_parameters *meta=NULL;
  char *meta_name = appendExt(infile, ".meta");
  if (fileExists(meta_name)) {
    meta = meta_read(meta_name);
    if (meta->insar)
      found = TRUE;
    meta_free(meta);
  }
  free(meta_name);
  return found;
}

int is_uavsar(const char *infile)
{
  int found = FALSE;
  char line[1024], *ext = NULL;

  ext = findExt(infile);
  if (!ext) {
    char *inFile = (char *) MALLOC(sizeof(char) * strlen(infile) + 5);
    sprintf(inFile, "%s.ann", infile);
    int ret = is_uavsar(inFile);
    FREE(inFile);
    return ret;
  }
  else if (strcmp_case(ext, ".ann")==0) {    
    FILE *fp = fopen(infile, "r");
    if (fp) {
      while (fgets(line, 1024, fp)) {
	if (strstr(line, "http://uavsar.jpl.nasa.gov")) {
	  found = TRUE;
          break;
        }
      }
      fclose(fp);
    }
  }

  return found;
}

/* Returns true if a PolSARpro file set is detected based on the */
/* filename passed in.                                           */
int isPolSARpro(const char * infile)
{
  int found_bin = 0;
  int found_bin_hdr = 0;
  char *bin = NULL, *bin_hdr = NULL, *dupe = NULL, *ext = NULL;

  ext = findExt(infile);
  if (!ext) {
    // If no file extension exists, then maybe it has been stripped
    // off.  Guess .bin and check for existence...
    char *inFile = (char *)MALLOC(sizeof(char) * strlen(infile) + 5);
    sprintf(inFile, "%s.bin", infile);
    int ret = isPolSARpro(inFile);
    FREE(inFile);
    return ret;
  }
  if (strcmp_case(ext, ".bin")==0) {
    bin = (char *)infile;
    bin_hdr = MALLOC(sizeof(char)*(strlen(infile)+32));
    strcpy(bin_hdr, infile);
    strcat(bin_hdr, ".hdr");
    found_bin = fileExists(bin);
    found_bin_hdr = fileExists(bin_hdr);
    FREE(bin_hdr);
  }
  else if (strcmp_case(ext, ".hdr")==0) {
    dupe = STRDUP(infile);
    bin_hdr = (char *)infile;
    ext = findExt(dupe);
    *ext = '\0';
    ext = findExt(dupe);
    if (ext && (strcmp_case(ext, ".bin")==0)) {
      bin = dupe;
    }
    found_bin = fileExists(bin);
    found_bin_hdr = fileExists(bin_hdr);
    FREE(dupe);
  }

  return (int)(found_bin && found_bin_hdr);
}

meta_parameters *isAirSAR(const char *inFile, int *c, int *l, int *p)
{
  airsar_header *header;
  meta_parameters *meta = NULL;
  char dataFile[1024];
  int found_c_file = TRUE, found_l_file = TRUE, found_p_file = TRUE;

  //printf("isAirSAR: %s\n", inFile);

  // Look for C-band data
  sprintf(dataFile, "%s_c.datgr", inFile);
  if (!fileExists(dataFile))
    sprintf(dataFile, "%s_c.dat", inFile);
  if (!fileExists(dataFile))
    found_c_file = FALSE;
  else {
    header = read_airsar_header(dataFile);
    if (header) {
      meta = import_airsar_meta(dataFile, inFile, FALSE);
      *c = TRUE;
    }
    else {
      asfPrintWarning("Data file (%s) is not AirSAR data.\n", dataFile);
      *c = FALSE;
      return NULL;
    }
  }

  // Look for L-band data
  sprintf(dataFile, "%s_l.datgr", inFile);
  if (!fileExists(dataFile))
    sprintf(dataFile, "%s_l.dat", inFile);
  if (!fileExists(dataFile))
    found_l_file = FALSE;
  else {
    header = read_airsar_header(dataFile);
    if (header) {
      if (!meta)
	meta = import_airsar_meta(dataFile, inFile, FALSE);
      *l = TRUE;
    }
    else {
      asfPrintWarning("Data file (%s) is not AirSAR data.\n", dataFile);
      *l = FALSE;
      return NULL;
    }
  }

  // Look for P-band data
  sprintf(dataFile, "%s_p.datgr", inFile);
  if (!fileExists(dataFile))
    sprintf(dataFile, "%s_p.dat", inFile);
  if (!fileExists(dataFile))
    found_p_file = FALSE;
  else {
    header = read_airsar_header(dataFile);
    if (header) {
      if (!meta)
	meta = import_airsar_meta(dataFile, inFile, FALSE);
      *p = TRUE;
    }
    else {
      asfPrintWarning("Data file (%s) is not AirSAR data.\n", dataFile);
      *p = FALSE;
      return NULL;
    }
  }

  // Found any of files and we are good
  if (!found_c_file && !found_l_file && !found_p_file) {
    /*
    asfPrintWarning("Could not find any polarimetric data related to (%s).\n",
		    inFile);
    */
    *c = FALSE;
    *l = FALSE;
    *p = FALSE;
    return NULL;
  }
  else
    return meta;
}

int isASFInternal(const char *input_file)
{
  char *meta_file = appendExt(input_file, ".meta");
  if (fileExists(meta_file)) {
    char *img_file = appendExt(input_file, ".img");
    if (fileExists(img_file))
      return TRUE;
  }
  return FALSE;
}

int isSTF(const char *input_file)
{
  char **inBandName = NULL, **inMetaName = NULL;

  if (require_stf_pair(input_file, inBandName, inMetaName))
    return TRUE;
  else
    return FALSE;
}

int isPolarimetricSegmentation(const char *input_file)
{
  meta_parameters *meta;
  int ret = FALSE;

  if (isPolSARpro(input_file)) {
    char *error;
    if (isPolsarproSegmentation(input_file, &error))
      ret = TRUE; 
  }
  else if (isASFInternal(input_file)) {
    meta = meta_read(input_file);
    if (meta->general->image_data_type == POLARIMETRIC_SEGMENTATION)
      ret = TRUE;
    meta_free(meta);
  }
  return ret;
}

static void setup_tmp_dir(char *tmp_dir)
{
  // Create temporary test directory
  strcpy(tmp_dir, "tmp_meta-");
  strcat(tmp_dir, time_stamp_dir());
  create_clean_dir(tmp_dir);
}

meta_parameters *meta_read_cfg(const char *inName, convert_config *cfg)
{
  // Setup temporary directory for metadata file
  char *tmpDir = (char *) MALLOC(sizeof(char)*512);
  setup_tmp_dir(tmpDir);

  // Get the regular metadata structure
  char **inBandName = NULL, **inMetaName = NULL, *error;
  int nBands=1, trailer;
  if (isCEOS(inName, &error))
    require_ceos_pair(inName, &inBandName, &inMetaName,
              &nBands, &trailer);
  else if (isSTF(inName))
    require_stf_pair(inName, inBandName, inMetaName);
  meta_parameters *meta = meta_read(inMetaName[0]);

  // Assign temporary metadata file
  char *outMetaName = (char *) MALLOC(sizeof(char)*255);
  sprintf(outMetaName, "%s/tmp.meta", tmpDir);

  // Assign a couple values out of the configuration file
  int import_single_band = 0;
  int complex_flag = cfg->import->complex_slc;
  int nBandsOut = cfg->general->terrain_correct ? nBands+1 : nBands;
  char *bandExt = (char *) MALLOC(sizeof(char)*10);
  int ii;

  // Read radiometry
  radiometry_t radiometry;
  if (strncmp_case(cfg->import->radiometry, "AMPLITUDE_IMAGE", 15) == 0)
    radiometry = r_AMP;
  else if (strncmp_case(cfg->import->radiometry, "POWER_IMAGE", 11) == 0)
    radiometry = r_POWER;
  else if (strncmp_case(cfg->import->radiometry, "SIGMA_IMAGE", 11) == 0)
    radiometry = r_SIGMA;
  else if (strncmp_case(cfg->import->radiometry, "GAMMA_IMAGE", 11) == 0)
    radiometry = r_GAMMA;
  else if (strncmp_case(cfg->import->radiometry, "BETA_IMAGE", 10) == 0)
    radiometry = r_BETA;

  // Set image data type
  data_type_t data_type = meta->general->data_type;
  char *lutName = (char *) MALLOC(sizeof(char)*255);
  strcpy(lutName, cfg->import->lut);
  if (data_type >= COMPLEX_BYTE)
    meta->general->image_data_type = COMPLEX_IMAGE;
  else if (lutName)
    meta->general->image_data_type = LUT_IMAGE;
  else
    meta->general->image_data_type = AMPLITUDE_IMAGE;

  // Set data type and band count
  for (ii=0; ii<nBands; ii++) {
    int band = ii + 1;

    // Determine the band extension (band ID)
    if (strcmp_case(meta->general->sensor, "RSAT") == 0)
      strcpy(bandExt, "HH");
    else if (strcmp_case(meta->general->sensor, "ERS") == 0 ||
         strcmp_case(meta->general->sensor, "JERS") == 0)
      strcpy(bandExt, "VV");
    else if (strcmp_case(meta->general->sensor_name, "SAR") == 0 ||
         strcmp_case(meta->general->sensor_name, "PALSAR") == 0)
      bandExt = get_polarization(inBandName[ii]);
    else if (strcmp_case(meta->general->sensor_name, "AVNIR") == 0 ||
         strcmp_case(meta->general->sensor_name, "PRISM") == 0) {
      int band_number;
      band_number = get_alos_band_number(inBandName[ii]);
      if (band_number<10)
    sprintf(bandExt, "0%d", band_number);
      else
    sprintf(bandExt, "%d", band_number);
    }
    strcpy(meta->general->bands, "");

    if (meta->sar) {
      // Assign band names
      assign_band_names(meta, outMetaName, bandExt, band, nBands, nBandsOut,
            radiometry, complex_flag);
      if (complex_flag) {
        meta->general->data_type = COMPLEX_REAL32;
        meta->general->band_count = import_single_band ? 1 : band;
      }
      else if (data_type >= COMPLEX_BYTE) {
        meta->general->data_type = REAL32;
        meta->general->band_count = import_single_band ? 2 : band*2;
      }
      else {
        meta->general->data_type = REAL32;
        meta->general->band_count = import_single_band ? 1 : band;
      }
      meta_write(meta, outMetaName);
    }
    else if (meta->optical) {
      int band_number;
      band_number = get_alos_band_number(inBandName[ii]);
      if (band_number<9)
    sprintf(bandExt, "0%d", band_number);
      else
    sprintf(bandExt, "%d", band_number);
      //if (nBands > 1)
      //  asfPrintStatus("   Input band: %s\n", bandExt);
      if (band > 1) {
    if (strcmp_case(meta->general->sensor_name, "PRISM") != 0 &&
        (strcmp_case(meta->general->mode, "1A") != 0 ||
         strcmp_case(meta->general->mode, "1B1") != 0)) {
      meta_parameters *metaTmp=NULL;
      metaTmp = meta_read(outMetaName);
      strcat(meta->general->bands, metaTmp->general->bands);
      meta_free(metaTmp);
    }
      }
      if (strcmp(meta->general->bands, "") != 0)
    strcat(meta->general->bands, ",");
      strcat(meta->general->bands, bandExt);
    }
  }
  if (nBands == 2) {
    strcpy(meta->sar->polarization, "dual-pol");
    meta->general->image_data_type = POLARIMETRIC_IMAGE;
  }
  else if (nBands == 4) {
    strcpy(meta->sar->polarization, "quad-pol");
    meta->general->image_data_type = POLARIMETRIC_IMAGE;
  }

  // Clean up
  remove_dir(tmpDir);
  FREE(tmpDir);
  FREE(outMetaName);
  FREE(bandExt);

  return meta;
}

int findDemFile(char *fileName)
{
  int found = 0;

  // First check for the filename as-is
  if (is_dir(fileName))
    found = 1;
  else if (fileExists(fileName)) {
    found = 1;
  }
  else {
    // Check for .img, .tif, .tiff, .TIF, and .TIFF extensions
    char *img_File = appendExt(fileName, ".img");
    char *tif_File = appendExt(fileName, ".tif");
    char *tiff_File = appendExt(fileName, ".tiff");
    char *TIF_File = appendExt(fileName, ".TIF");
    char *TIFF_File = appendExt(fileName, ".TIFF");

    if (fileExists(img_File)) {
      found = 1;
    }
    else if (fileExists(tif_File)) {
      found = 1;
      strcpy(fileName, tif_File);
    }
    else if (fileExists(tiff_File)) {
      found = 1;
      strcpy(fileName, tiff_File);
    }
    else if (fileExists(TIF_File)) {
      found = 1;
      strcpy(fileName, TIF_File);
    }
    else if (fileExists(TIFF_File)) {
      found = 1;
      strcpy(fileName, TIFF_File);
    }
    else {
      found = 0;
    }

    // Clean up
    FREE(img_File);
    FREE(tif_File);
    FREE(tiff_File);
    FREE(TIF_File);
    FREE(TIFF_File);
  }

  return found;
}

void check_return(int ret, char *msg)
{
  if (ret != 0)
    asfPrintError(msg);
}

void check_input(convert_config *cfg, char *processing_step, char *input)
{
  meta_parameters *meta;
  char **inBandName = NULL, **inMetaName = NULL, *matrixType, *error;
  int nBands, trailer, airsar_c, airsar_l, airsar_p;

  if (strcmp_case(processing_step, "polarimetry") == 0) {
    // All of these require complex quad-pol data
    int requires_cpx = cfg->polarimetry->pauli ||
          cfg->polarimetry->cloude_pottier ||
          cfg->polarimetry->cloude_pottier_ext ||
          cfg->polarimetry->cloude_pottier_nc ||
          cfg->polarimetry->k_means_wishart ||
          cfg->polarimetry->k_means_wishart_ext ||
          cfg->polarimetry->lee_preserving ||
          cfg->polarimetry->freeman_durden;

    meta = isAirSAR(input, &airsar_c, &airsar_l, &airsar_p);
    if (meta) {
      // For any of the polarimetric calculation we need SIGMA power scale
      // images. Just in case the user did not select those we inforce here
      // for the AirSAR data.
      strcpy(cfg->import->radiometry, "SIGMA_IMAGE");
      cfg->airsar->c_pol = airsar_c;
      cfg->airsar->l_pol = airsar_l;
      cfg->airsar->p_pol = airsar_p;
    }
    else if(is_uavsar(input)) {
      if(cfg->polarimetry->sinclair ||
          cfg->polarimetry->cloude_pottier ||
          cfg->polarimetry->cloude_pottier_ext ||
          cfg->polarimetry->cloude_pottier_nc ||
          cfg->polarimetry->k_means_wishart ||
          cfg->polarimetry->k_means_wishart_ext ||
          cfg->polarimetry->lee_preserving ||
          cfg->polarimetry->freeman_durden)
        asfPrintError("Only the pauli polarimetric decomposition is supported for UAVSAR data at this time\n");
      if(cfg->polarimetry->farcorr)
        asfPrintError("Faraday rotation correction of UAVSAR data is not supported at this time\n");
      if(cfg->polarimetry->pauli &&
          (strstr(cfg->import->uavsar, "HGT") != NULL || strstr(cfg->import->uavsar, "hgt") != NULL))
        asfPrintError("Pauli decomposition cannot be performed on the UAVSAR .hgt file.\n");
      return;
    }
    else if (isPolsarproMatrix(input, &matrixType, &error)) { return; }
    else if (isASFInternal(input)) {
      meta = meta_read(input);
      if (meta && meta->sar) {
        if (requires_cpx &&
	    ((meta->general->image_data_type != POLARIMETRIC_S2_MATRIX &&
	      meta->general->image_data_type != POLARIMETRIC_C3_MATRIX &&
        meta->general->image_data_type != POLARIMETRIC_IMAGE &&
	      meta->general->image_data_type != POLARIMETRIC_T3_MATRIX) ||
	     strcmp_case(meta->sar->polarization, "QUAD-POL") != 0 ||
             meta->general->band_count < 8))
         asfPrintError("Pauli decomposition requires complex quad-pol data\n");
        if (cfg->polarimetry->sinclair &&
	    (meta->general->image_data_type != POLARIMETRIC_IMAGE ||
	     strcmp_case(meta->sar->polarization, "QUAD-POL") != 0))
         asfPrintError("Sinclair decomposition requires quad-pol data\n");
      }
      else {
        asfPrintError("Polarimetry requires SAR data.\n");
      }
      if (meta) meta_free(meta);
      return;
    }
    else if (isCEOS(input, &error)) {
      require_ceos_pair(input, &inBandName, &inMetaName,
            &nBands, &trailer);
      meta = meta_create(inMetaName[0]);
    }
    else if (isSTF(input)) {
      require_stf_pair(input, inBandName, inMetaName);
      meta = meta_create(inMetaName[0]);
    }
    if (meta->sar) {
      if (strcmp_case(meta->general->sensor, "AIRSAR") != 0) {
	meta_free(meta);
	// re-read meta with additional info
	meta = meta_read_cfg(inMetaName[0], cfg);
      }
      // Pauli decomposition only works for complex quad-pol data
      if (requires_cpx &&
	  ( 
           (meta->general->image_data_type != POLARIMETRIC_S2_MATRIX &&
	    meta->general->image_data_type != POLARIMETRIC_C3_MATRIX &&
            meta->general->image_data_type != POLARIMETRIC_IMAGE &&
	    meta->general->image_data_type != POLARIMETRIC_T3_MATRIX)
          ||
	   strcmp_case(meta->sar->polarization, "QUAD-POL") != 0
          ||
           meta->general->band_count < 8
          )
         )
           asfPrintError("The polarimetric decomposition requires complex quad-pol data\n");
      // Sinclair decomposition ought to work on complex and detected
      // quad-pol data
      if (cfg->polarimetry->sinclair &&
	  (meta->general->image_data_type != POLARIMETRIC_IMAGE ||
	   strcmp_case(meta->sar->polarization, "QUAD-POL") != 0))
        asfPrintError("Sinclair decomposition requires quad-pol data\n");
    }
    else {
      asfPrintError("Polarimetry requires SAR data.\n");
    }
  }
}

// If a temporary directory has not been specified, create one using the time
// stamp as the name of the temporary directory
// extract the file basename from the in_name, extract the directory to put
// the tmp_dir in from the out_name
static void create_and_set_tmp_dir(char *in_name, char *out_name, char *tmp_dir)
{
  int length = strlen(in_name)+1;
  if ((strlen(out_name)+1) > length) {
    length = strlen(out_name)+1;
  }
  char *junk     = MALLOC(sizeof(char)*length);
  char *basename = MALLOC(sizeof(char)*length);
  char *out_dir  = MALLOC(sizeof(char)*length);

  split_dir_and_file(in_name, junk, basename);
  split_dir_and_file(out_name, out_dir, junk);
  FREE(junk);

  int tmp_len = strlen(tmp_dir);
  int out_len = strlen(out_dir);

  if (0==tmp_len) {
    if (0==out_len) {
      strcpy(tmp_dir, "");
    }
    else {
      strcpy(tmp_dir, out_dir);
      strcat(tmp_dir, DIR_SEPARATOR_STR);
    }
    strcat(tmp_dir, basename);
    strcat(tmp_dir, "-");
    strcat(tmp_dir, time_stamp_dir());
    create_clean_dir(tmp_dir);
  }
  else {
    DIR *dirp = opendir(tmp_dir);
    if (!dirp)
      create_clean_dir(tmp_dir);
    else
      closedir(dirp);
  }

  set_asf_tmp_dir(tmp_dir);

  FREE(basename);
  FREE(out_dir);
}

/* Tracking useful intermediate files, for exposure in the GUI */
static char *intermediates_file = NULL;
extern char *g_status_file;
static void save_intermediate(convert_config *cfg, char *tag, char *filename)
{
  // the "tag" that is passed in needs to match what is looked for
  // in the GUI: asf_convert_gui/file_list.c:move_to_completed_files_list()

  

  if (cfg->general->intermediates && g_status_file && strlen(g_status_file)>0)
  {
    if (!intermediates_file) {
      // the name of the intermediates file needs to match what the GUI
      // will look for in asf_convert_gui/execute.c:do_convert()
      char *status_file = STRDUP(g_status_file);
      char *ext = strstr(status_file, ".status");
      if (ext) *ext = '\0';
      intermediates_file = appendExt(status_file, ".files");
      FILE *fp = fopen(intermediates_file, "w");
      if (fp) {
        fprintf(fp, "List of useful intermediate files.\n");
        fclose(fp);
      }
      free(status_file);
    }
    FILE *fp = fopen(intermediates_file, "a");
    if (fp) {
      fprintf(fp, "%s: %s\n", tag, filename);
      fclose(fp);
    }
  }
}

static void reset_intermediates()
{
    intermediates_file = NULL;
}

/* Make a copy of the metdata file. */
static void copy_meta(convert_config *cfg, char *src, char *dest)
{
  char *tmp1 = appendExt(src, ".meta");
  char *tmp2 = appendExt(dest, ".meta");
  fileCopy(tmp1, tmp2);
  free(tmp1);
  save_intermediate(cfg, "Meta", tmp2);
  free(tmp2);
}

/* Returns TRUE if the filename has a TIFF extension */
static int has_tiff_ext(const char *f)
{
    char *ext = findExt(f);
    if (ext)
        return
            strcmp_case(ext, ".tif") == 0 ||
            strcmp_case(ext, ".tiff") == 0;
    else
        return FALSE;
}

static output_format_t get_format(convert_config *cfg)
{
  output_format_t format = JPEG;
  if (strncmp(uc(cfg->export->format), "TIFF", 4) == 0) {
    format = TIF;
  } else if (strncmp(uc(cfg->export->format), "GEOTIFF", 7) == 0) {
    format = GEOTIFF;
  } else if (strncmp(uc(cfg->export->format), "JPEG", 4) == 0) {
    format = JPEG;
  } else if (strncmp(uc(cfg->export->format), "PGM", 3) == 0) {
    format = PGM;
  } else if (strncmp(uc(cfg->export->format), "PNG_ALPHA", 9) == 0) {
    format = PNG_ALPHA;
  } else if (strncmp(uc(cfg->export->format), "PNG_GE", 6) == 0) {
    format = PNG_GE;
  } else if (strncmp(uc(cfg->export->format), "PNG", 3) == 0) {
    format = PNG;
  } else if (strncmp_case(cfg->export->format, "POLSARPRO", 9) == 0) {
    format = POLSARPRO_HDR;
  }
  return format;
}

static char *
convert_tiff(const char *tiff_file, char *what, convert_config *cfg,
             int save_converted)
{
    char *tiff_basename, imported[255], geocoded[255], status[255];
    tiff_basename = stripExt(tiff_file);
    sprintf(imported, "%s/imported_%s", cfg->general->tmp_dir, what);
    char ancillary_file[256]="";

    // want see "dem" as "DEM" in the status messages, and asf_import
    // requires an uppercase string for the image_data_type
    char *uc_what = MALLOC(strlen(what)+2);
    strcpy(uc_what, uc(what)); // uc() returns ptr to static mem => make copy

    // if user wants to save the converted file, do so, otherwise we
    // put it into the temporary directory along with everything else
    if (save_converted) {
        char *outfileDir = get_dirname(cfg->general->out_name);
        char *basename = get_basename(tiff_basename);
        // the naming scheme of the geocoded dem/mask must match what the
        // gui expects in settings_update_dem/mask.
        sprintf(geocoded, "%sgeocoded_%s_%s", outfileDir, what, basename);
        free(outfileDir);
        free(basename);
    }
    else {
        sprintf(geocoded, "%s/geocoded_%s", cfg->general->tmp_dir, what);
    }

    sprintf(status, "Importing %s...", uc_what);
    update_status(status);

    sprintf(status, "ingesting GeoTIFF %s (asf_import)\n", uc_what);
    check_return(
        asf_import(r_AMP, FALSE, FALSE, FALSE, FALSE, GENERIC_GEOTIFF, NULL,
                   NULL, what, NULL, NULL, -999, -999, -999, -999, 0, 0, 
		   -99, -99, 0, NULL, NULL, NULL, FALSE, NULL, tiff_basename, 
		   ancillary_file, NULL, NULL, NULL, NULL, NULL, FALSE, NULL,
		   FALSE, imported),
        status);

    sprintf(status, "Geocoding %s...", uc_what);
    update_status(status);

    // Check to see if the TIFF is map-projected or not, and if not
    // then geocode it.  ALSO set the image_data_type field while
    // we're at it
    meta_parameters *mp = meta_read(imported);
    if (strncmp(uc_what, "DEM", 3) == 0) {
      mp->general->image_data_type = DEM;
    }
    else if (strncmp(uc_what, "MASK", 4) == 0) {
      mp->general->image_data_type = MASK;
    }
    meta_write(mp, imported);

    // Now we allow lat/lon pseudoprojected dems with terrain correction
    // So, we can usually get away with skipping the geocoding step for
    // the dem.  However, in the case where the user wants to save the
    // cut dem, we do geocode since in that case the cut dem will use the
    // the same projection as the final result (and the layover/shadow mask,
    // if they've also saved that)
    if ((!is_map_projected(mp) && !is_lat_lon_pseudo(mp)) ||
        (cfg->terrain_correct->save_terrcorr_dem && cfg->general->geocoding))
    {
      if (cfg->general->geocoding) {
          // use the user's projection, if we have it
          sprintf(status, "geocoding GeoTIFF %s (asf_geocode)\n", uc_what);
          check_return(
              asf_geocode_from_proj_file(
                  cfg->geocoding->projection, cfg->geocoding->force,
                  RESAMPLE_NEAREST_NEIGHBOR, 0.0, WGS84_DATUM, NAN,
                  NULL, imported, geocoded, cfg->geocoding->background),
              status);
      }
      else {
          // use UTM if no geocoding specified
          sprintf(status, "geocoding GeoTIFF %s (asf_geocode)\n", uc_what);
          check_return(
              asf_geocode_utm(RESAMPLE_NEAREST_NEIGHBOR, 0.0, WGS84_DATUM,
                              NAN, NULL, imported, geocoded, 0.0), status);
      }
    }
    else {
      // is map projected, so copy the imported file to the geocoded
      // filename for consistency
      copyImgAndMeta(imported, geocoded);
    }

    meta_free(mp);
    free(uc_what);
    free(tiff_basename);

    return STRDUP(geocoded);
}

static int check_airsar(char *outFile, char *suffix)
{
  char *base = appendToBasename(outFile, suffix);

  char *full_img = appendExt(base, ".img");
  char *full_meta = appendExt(base, ".meta");

  int ret = fileExists(full_img) && fileExists(full_meta);

  free(full_img);
  free(full_meta);
  free(base);

  return ret;
}

static datum_type_t str_to_datum(const char *datum_str)
{
  datum_type_t datum;
  if (strncmp_case(datum_str, "WGS84", 5) == 0) {
    datum = WGS84_DATUM;
  }
  else if (strncmp_case(datum_str, "NAD27", 5) == 0) {
    datum = NAD27_DATUM;
  }
  else if (strncmp_case(datum_str, "NAD83", 5) == 0) {
    datum = NAD83_DATUM;
  }
  else if (strncmp_case(datum_str, "HUGHES", 6) == 0 ) {
    datum = HUGHES_DATUM;
  }
  else if (strncmp_case(datum_str, "ITRF97", 6) == 0) {
    datum = ITRF97_DATUM;
  }
  else if (strncmp_case(datum_str, "ED50", 4) == 0) {
    datum = ED50_DATUM;
  }
  else if (strncmp_case(datum_str, "SAD69", 5) == 0) {
    datum = SAD69_DATUM;
  }
  else {
    datum = UNKNOWN_DATUM;
  }

  return datum;
}

static void calc_polarimetry(convert_config *cfg, char *inFile, char *outFile)
{
  char tmpFile[1024];

  // Calculate polarimetric parameters
  if (cfg->polarimetry->pauli)
    cpx2pauli(inFile, outFile, cfg->general->terrain_correct);
  else if (cfg->polarimetry->sinclair) {
    // for sinclair, there are two possibilities: SLC & non-SLC
    meta_parameters *meta = meta_read(inFile);
    if (meta->general->band_count >= 8) {
      cpx2sinclair(inFile, outFile, cfg->general->terrain_correct);
    }
    else {
      // here, we don't need to do any processing -- we just need to
      // update the RGB Bands to Red=HH, Green=HV, Blue=VV
      strcpy(cfg->export->rgb, "HH,HV,VV");
      strcpy(outFile, inFile);
    }
    meta_free(meta);
  }
  else if (cfg->polarimetry->cloude_pottier) {
    cpx2cloude_pottier8(inFile, outFile, cfg->general->terrain_correct);
  }
  else if (cfg->polarimetry->cloude_pottier_ext)
    cpx2cloude_pottier16(inFile, outFile, cfg->general->terrain_correct);
  else if (cfg->polarimetry->cloude_pottier_nc)
    cpx2entropy_anisotropy_alpha(inFile, outFile,
				 cfg->general->terrain_correct);
  else if (cfg->polarimetry->freeman_durden)
    cpx2freeman_durden(inFile, outFile, cfg->general->terrain_correct);
  else if (cfg->polarimetry->k_means_wishart)
    asfPrintError("K-means Wishart clustering not supported yet.\n");
  else if (cfg->polarimetry->k_means_wishart_ext)
    asfPrintError("Extended K-means Wishart clustering not supported yet.\n");
  else if (cfg->polarimetry->lee_preserving)
    asfPrintError("Lee category preserving not supported yet.\n");
  else
    asfPrintError("Unsupported polarimetric processing technique.\n");

  if (cfg->polarimetry->cloude_pottier ||
      cfg->polarimetry->cloude_pottier_ext ||
      cfg->polarimetry->cloude_pottier_nc)
    {
      sprintf(tmpFile, "%s/polarimetry_combined_hist.img",
	      cfg->general->tmp_dir);
      save_intermediate(cfg, "Cloude-Pottier Histogram", tmpFile);
      sprintf(tmpFile, "%s/polarimetry_class_map.img",
	      cfg->general->tmp_dir);
      save_intermediate(cfg, "Entropy-Alpha Class Map", tmpFile);
    }
}

static scale_t get_scale(convert_config *cfg);
static void do_export(convert_config *cfg, char *inFile, char *outFile);

static int check_config(const char *configFileName, convert_config *cfg)
{
  // Check that input name isn't the same as the output name
  // (This can happen with CEOS Level 0-- both use .raw)
  if (strcmp_case(cfg->import->format, "CEOS") == 0 &&
      strcmp_case(cfg->general->in_name, cfg->general->out_name) == 0) {
    // the names (actually, the basenames) can match for L1 data
    // it is only the ".RAW" ones we want to flag
    char *RAW_file = appendExt(cfg->general->in_name, ".RAW");
    char *raw_file = appendExt(cfg->general->in_name, ".raw");
    if (fileExists(raw_file) || fileExists(RAW_file))
      asfPrintError("Input and Output names are the same: %s\n",
		    cfg->general->in_name);
    free(raw_file); free(RAW_file);
    RAW_file = appendStr(cfg->general->in_name, ".RAW");
    raw_file = appendStr(cfg->general->in_name, ".raw");
    if (fileExists(raw_file) || fileExists(RAW_file))
      asfPrintError("Input and Output names are the same: %s\n",
		    cfg->general->in_name);
    free(raw_file); free(RAW_file);
  }
  
  // Check if the user said the file is L1, but we really have a L0 file
  if (strcmp_case(cfg->import->format, "CEOS (1)") == 0) {
    char *raw_file = appendExt(cfg->general->in_name, ".raw");
    char *ldr_file = appendExt(cfg->general->in_name, ".ldr");
    if (fileExists(raw_file) && fileExists(ldr_file))
      asfPrintError("You selected CEOS Level 1 import, however the input "
		    "file appears to be Level 0:\n  %s\n", raw_file);
    free(raw_file); free(ldr_file);
    
    char *RAW_file = appendExt(cfg->general->in_name, ".RAW");
    char *LDR_file = appendExt(cfg->general->in_name, ".LDR");
    if (fileExists(RAW_file) && fileExists(LDR_file))
      asfPrintError("You selected CEOS Level 1 import, however the input "
		    "file appears to be Level 0:\n  %s\n", RAW_file);
    free(RAW_file); free(LDR_file);
  }

  // Check for PolSARPro matrix processing
  if (strcmp_case(cfg->import->format, "POLSARPRO") == 0 &&
      cfg->general->calibration &&
      strncmp_case(cfg->calibrate->radiometry, "AMPLITUDE", 9) != 0)
    asfPrintError("No changes in radiometry allowed when processing PolSARPro "
		  "matrices\n");

  // Check whether everything in the [Import] block is reasonable
  if (cfg->general->import) {
    
    // Import format
    if (strncmp_case(cfg->import->format, "ASF", 3) != 0 &&
	strncmp_case(cfg->import->format, "CEOS", 4) != 0 &&
	strncmp_case(cfg->import->format, "STF", 3) != 0 &&
	strncmp_case(cfg->import->format, "AIRSAR", 6) != 0 &&
	strncmp_case(cfg->import->format, "BIL", 3) != 0 &&
	strncmp_case(cfg->import->format, "GRIDFLOAT", 9) != 0 &&
	strncmp_case(cfg->import->format, "GAMMA", 5) != 0 &&
	strncmp_case(cfg->import->format, "ROIPAC", 6) != 0 &&
	strncmp_case(cfg->import->format, "POLSARPRO", 9) != 0 &&
	strncmp_case(cfg->import->format, "TERRASAR", 8) != 0 &&
	strncmp_case(cfg->import->format, "RADARSAT2", 9) != 0 &&
	strncmp_case(cfg->import->format, "ALOS_MOSAIC", 11) != 0 &&
	strncmp_case(cfg->import->format, "GEOTIFF", 7) != 0 &&
	strncmp_case(cfg->import->format, "ROIPAC", 6) != 0 &&
	strncmp_case(cfg->import->format, "UAVSAR", 6) != 0) {
      asfPrintError("Selected import format not supported\n");
    }
        
    // Radiometry
    if (strncmp(uc(cfg->import->radiometry), "AMPLITUDE_IMAGE", 15) != 0 &&
	strncmp(uc(cfg->import->radiometry), "POWER_IMAGE", 11) != 0 &&
	strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) != 0 &&
	strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) != 0 &&
	strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) != 0) {
      asfPrintError("Selected radiometry not supported\n");
    }
   
    // UAVSAR data is already calibrated as GAMMA
    if (cfg->general->calibration &&
        strncmp_case(cfg->import->format, "UAVSAR", 6) == 0)
    {
        asfPrintStatus("UAVSAR data is already calibrated as GAMMA.\n");
        cfg->general->calibration = FALSE;
    }

    // Look up table file existence check
    if (strlen(cfg->import->lut) > 0) {
      if (!fileExists(cfg->import->lut)) {
	asfPrintError("Look up table file does not exist\n");
      }
    }
    
    // When importing AirSAR or UAVSAR data, don't allow terrain correction
    if (cfg->general->terrain_correct &&
        (strncmp_case(cfg->import->format, "AIRSAR", 6) == 0 ||
         strncmp_case(cfg->import->format, "UAVSAR", 6) == 0))
      asfPrintError("Terrain correction of %s data is not supported.\n",
                    cfg->import->format);
    
    // When importing a GeoTIFF, don't allow terrain correction.
    if (strncmp(uc(cfg->import->format), "GEOTIFF", 7) == 0 &&
	cfg->general->terrain_correct)
      asfPrintError("Terrain correction of GeoTIFFs is not supported.\n");
    
    // Precision state vector file check can only be done
    // from within asf_import    
    
    // Dealing with single look complex data
    // Options -multilook and -complex are mutually exclusive
    if (cfg->import->complex_slc && cfg->import->multilook_slc)
        asfPrintError("Only single look complex data stored as "
                      "amplitude and phase can be multilooked.\n");
    
    // Can skip import if the input is already asf internal.
    if (strncmp(uc(cfg->import->format), "ASF", 3) == 0) {
      cfg->general->import = 0;
    }
  }
  
  // Check whether everything in the [External] block is reasonable
  if (cfg->general->external) {
    
    if (strlen(cfg->external->cmd) == 0) {
      asfPrintWarning("No external command specified.  Ignored.\n");
      cfg->general->external = 0;
    }
    else {
      // the command must contain the strings "{Input}" and "{Output}"
      if (strstr(cfg->external->cmd, "{Input}") == NULL &&
	  strstr(cfg->external->cmd, "{Output}") == NULL)
	asfPrintError("External command does not contain the {Input}\n"
		      "and {Output} placeholders.");
      else if (strstr(cfg->external->cmd, "{Input}") == NULL)
	asfPrintError("External command does not contain the {Input} "
		      "placeholder.");
      else if (strstr(cfg->external->cmd, "{Output}") == NULL)
	asfPrintError("External command does not contain the {Output} "
		      "placeholder.");
    }
    
  }
  
  // Check whether everything in the [SAR processing] block is reasonable
  if (cfg->general->sar_processing) {
    
    if (strncmp_case(cfg->import->format, "AIRSAR", 6) == 0)
      asfPrintError("Cannot perform SAR Processing on AirSAR data.\n");
    else if (strncmp_case(cfg->import->format, "UAVSAR", 6) == 0)
      asfPrintError("Cannot perform SAR Processing on UAVSAR data.\n");

    // ADDED FOR 3.0 -- DO NOT SUPPORT L0 PROCESSING!
    // We actually think this is working in many cases, but it hasn't
    // been fully tested yet for all satellites/beams.
    asfPrintWarning("Processing from level 0 is not fully tested yet!\n");
    
    // Radiometry
    if (strncmp(uc(cfg->sar_processing->radiometry), "AMPLITUDE_IMAGE", 15) != 0 &&
	strncmp(uc(cfg->sar_processing->radiometry), "POWER_IMAGE", 11) != 0 &&
	strncmp(uc(cfg->sar_processing->radiometry), "SIGMA_IMAGE", 11) != 0 &&
	strncmp(uc(cfg->sar_processing->radiometry), "GAMMA_IMAGE", 11) != 0 &&
	strncmp(uc(cfg->sar_processing->radiometry), "BETA_IMAGE", 10) != 0) {
      asfPrintError("Selected radiometry not supported\n");
    }
    
  }
  
  // Check whether everything in the [C2P] block is reasonable
  if (cfg->general->c2p) {
    asfPrintWarning("SLC ingest with c2p is still underdevelopment.\n");
    // make sure input data has the extension .cpx
    char *ext = findExt(cfg->general->in_name);
    if (strcmp_case(ext, ".img") != 0) {
      asfPrintWarning("Input data is not complex.  c2p flag ignored.\n");
      cfg->general->c2p = 0;
    }
  }
  
  // Check whether everything in the [Image stats] block is reasonable
  if (cfg->general->image_stats) {
    
    // Values
    if (strncmp(cfg->image_stats->values, "LOOK", 4) != 0 &&
	strncmp(cfg->image_stats->values, "INCIDENCE", 9) != 0 &&
	strncmp(cfg->image_stats->values, "RANGE", 5) != 0) {
      asfPrintError("Selected values not supported\n");
    }
  }
  
  // Check whether everything in the [Polarimetry] block is reasonable
  if (cfg->general->polarimetry) {
    int pauli = cfg->polarimetry->pauli == 0 ? 0 : 1;
    int sinclair = cfg->polarimetry->sinclair == 0 ? 0 : 1;
    int cloude_pottier = cfg->polarimetry->cloude_pottier == 0 ? 0 : 1;
    int cloude_pottier_ext =
      cfg->polarimetry->cloude_pottier_ext == 0 ? 0 : 1;
    int cloude_pottier_nc = cfg->polarimetry->cloude_pottier_nc == 0 ? 0 : 1;
    int k_means_wishart = cfg->polarimetry->k_means_wishart == 0 ? 0 : 1;
    int freeman_durden = cfg->polarimetry->freeman_durden == 0 ? 0 : 1;
    int k_means_wishart_ext =
      cfg->polarimetry->k_means_wishart_ext == 0 ? 0 : 1;
    if (pauli + sinclair + cloude_pottier + cloude_pottier_ext +
	cloude_pottier_nc + k_means_wishart + k_means_wishart_ext +
	freeman_durden > 1)
      asfPrintError("More than one polarimetric processing scheme selected."
		    "\nOnly one of these options may be selected at a time."
		    "\n");
    if (pauli + sinclair + cloude_pottier + cloude_pottier_ext +
	cloude_pottier_nc + k_means_wishart + k_means_wishart_ext +
	freeman_durden + cfg->polarimetry->farcorr < 1)
      asfPrintError("No polarimetric processing selected.\n");
  }
  
  // Check whether everything in the [Terrain correction] block is
  // reasonable
  if (cfg->general->terrain_correct) {
    // Reference DEM file existence check
    if (strlen(cfg->terrain_correct->dem)==0) {
      asfPrintError("A DEM was not selected.\n");
    }
    if (!findDemFile(cfg->terrain_correct->dem)) {
      asfPrintError("Reference DEM file '%s' does not exist\n",
		    cfg->terrain_correct->dem);
    }

    // Look for polarimetric data.
    // Polarimetric segmentations and parameters can't be radiometrically
    // corrected.
    if (strcmp_case(cfg->import->image_data_type, 
		    "POLARIMETRIC_SEGMENTATION") == 0 &&
	cfg->terrain_correct->do_radiometric)
      asfPrintError("Polarimetric segmentations can't be radiometrically "
		    "terrain corrected!\n");
    if (strcmp_case(cfg->import->image_data_type, 
		    "POLARIMETRIC_PARAMETER") == 0 && 
	cfg->terrain_correct->do_radiometric)
      asfPrintError("Polarimetric parameters can't be radiometrically "
		    "terrain corrected!\n");
    
    // Check for pixel size smaller than threshold ???
    
    // specified a mask and asked for an auto-mask
    int have_mask = cfg->terrain_correct->mask &&
      strlen(cfg->terrain_correct->mask) > 0;
    if (have_mask) {
      if (!findDemFile(cfg->terrain_correct->mask)) {
	asfPrintError("MASK file '%s' does not exist\n",
		      cfg->terrain_correct->mask);
      }
      
      if (cfg->terrain_correct->auto_mask_water) {
	asfPrintStatus("Mask File: %s\n", cfg->terrain_correct->mask);
	asfPrintStatus("Auto Water Mask: %d\n",
		       cfg->terrain_correct->auto_mask_water);
	asfPrintWarning(
			"You cannot specify a mask for terrain correction "
			"and also request an automatically generated mask.\n"
			"Ignoring auto water mask -- will use the mask provided.\n");
      }
    }
  }

  /*
  // Check whether radiometric terrain correction is selected for PolSARPro
  // data. Refer to the manual for further explanation why this does not work.
  if (cfg->general->terrain_correct && cfg->terrain_correct->do_radiometric &&
      strncmp_case(cfg->import->format, "POLSARPRO", 9) == 0 &&
      !is_dir(cfg->general->in_name))
    asfPrintError("Radiometric terrain correction of PolSARPro data is not "
		  "supported.\nFor more information on alternative processing "
		  "flow based on\nradiometric terrain correction of CEOS "
		  "quad-pol data\nplease refer to the ASF MapReady manual.\n");
  */

  // Check whether everything in the [Geocoding] block is reasonable
  if (cfg->general->geocoding) {
    
    // Projection file existence check
    if (strcmp_case(cfg->geocoding->projection, "geographic") != 0 &&
        strcmp_case(cfg->geocoding->projection, "latlon") != 0 &&
        strcmp_case(cfg->geocoding->projection, "utm") != 0 &&
        !fileExists(cfg->geocoding->projection))
    {
      asfPrintError("Projection parameter file '%s' does not exist\n",
		    cfg->geocoding->projection);
    }
    
    // Check for pixel size smaller than threshold ???
    
    // Datum
    if (meta_is_valid_string(cfg->geocoding->datum)          &&
	strlen(cfg->geocoding->datum) > 0                    &&
	strncmp(uc(cfg->geocoding->datum), "NONE", 4)   != 0 &&
	strncmp(uc(cfg->geocoding->datum), "WGS84", 5)  != 0 &&
	strncmp(uc(cfg->geocoding->datum), "NAD27", 5)  != 0 &&
	strncmp(uc(cfg->geocoding->datum), "NAD83", 5)  != 0 &&
	strncmp(uc(cfg->geocoding->datum), "HUGHES", 6) != 0 &&
	strncmp_case(cfg->geocoding->datum, "ITRF97", 6) != 0 &&
	strncmp_case(cfg->geocoding->datum, "ED50", 4) != 0 &&
	strncmp_case(cfg->geocoding->datum, "SAD69", 5) != 0) {
      asfPrintError("Selected datum not supported\n");
    }
    
    // Resampling
    if (strncmp(uc(cfg->geocoding->resampling), "NEAREST_NEIGHBOR", 16) != 0 &&
	strncmp(uc(cfg->geocoding->resampling), "BILINEAR", 8) != 0 &&
	strncmp(uc(cfg->geocoding->resampling), "BICUBIC", 7) != 0) {
      asfPrintError("Selected resampling method not supported\n");
    }
    
    // Force resampling with Nearest Neighbor if we are doing
    // a Cloude-Pottier decomposition
    if (cfg->general->polarimetry &&
	(cfg->polarimetry->cloude_pottier ||
	 cfg->polarimetry->cloude_pottier_ext) &&
	strncmp(uc(cfg->geocoding->resampling), "NEAREST_NEIGHBOR", 16)!=0)
      {
        asfPrintWarning("When performing a Cloude-Pottier decomposition with "
                        "geocoding, the\nresampling method must be Nearest "
                        "Neighbor (since interpolating\nclassification numbers"
                        " does not make sense).\n\nChanging resampling method "
                        "from %s to NEAREST_NEIGHBOR.\n",
                        cfg->geocoding->resampling);
        strcpy(cfg->geocoding->resampling, "NEAREST_NEIGHBOR");
      }
    
    // Need to do the same when the input is a polarimetric segmentation
    // from PolSARPro
    if (strcmp_case(cfg->import->image_data_type, 
		    "POLARIMETRIC_SEGMENTATION") == 0 &&
	strcmp_case(cfg->geocoding->resampling, "NEAREST_NEIGHBOR") != 0) {
      asfPrintWarning("Input is a polarimetric segmentation that requires "
		      "nearest neighbor interpolation.\n");
      strcpy(cfg->geocoding->resampling, "NEAREST_NEIGHBOR");
    }
    
    // Check that the user didn't specify an average height, and
    // also is doing terrain correction
    if (cfg->general->terrain_correct &&
	!cfg->terrain_correct->refine_geolocation_only &&
	cfg->geocoding->height != 0)
      {
	asfPrintWarning("Since terrain correction is being applied, "
			"asf_geocode will ignore the\nspecified "
			"average height.\n");
      }
  }
  
  // Check whether everything in the [Terrain Correct] block is reasonable
  if (cfg->general->terrain_correct) {
    
    // specified a mask and asked for an auto-mask
    int have_mask = cfg->terrain_correct->mask &&
      strlen(cfg->terrain_correct->mask) > 0;
    
    if (have_mask && cfg->terrain_correct->auto_mask_water) {
      asfPrintStatus("Mask File: %s\n", cfg->terrain_correct->mask);
      asfPrintStatus("Auto Water Mask: %d\n",
		     cfg->terrain_correct->auto_mask_water);
      asfPrintWarning("You cannot specify a mask for terrain correction "
		      "and also request an automatically generated mask.\n"
		      "Ignoring auto water mask -- will use the mask "
		      "provided.\n");
    }
    
  }
  
  // Check whether everything in the [Export] block is reasonable
  if (cfg->general->export) {
    
    // Export format: ASF, TIFF, GEOTIFF, JPEG, PGM, POLSARPRO
    if (strncmp(uc(cfg->export->format), "ASF", 3) != 0 &&
	strncmp(uc(cfg->export->format), "TIFF", 4) != 0 &&
	strncmp(uc(cfg->export->format), "GEOTIFF", 7) != 0 &&
	strncmp(uc(cfg->export->format), "JPEG", 4) != 0 &&
	strncmp(uc(cfg->export->format), "PNG", 3) != 0 &&
	strncmp(uc(cfg->export->format), "PGM", 3) != 0 &&
	strncmp_case(cfg->export->format, "POLSARPRO", 9) != 0) {
      asfPrintError("Selected export format (%s) not supported\n",
		    cfg->export->format);
    }
    
    // Unset export flag when export format is ASF
    if (strncmp(uc(cfg->export->format), "ASF", 3) == 0) {
      cfg->general->export = 0;
    }
    
    // Scaling method for floating point GeoTIFFs
    if (strncmp(uc(cfg->export->format), "GEOTIFF", 7) == 0) {
      if (strlen(cfg->export->byte) < 1) {
	strcpy(cfg->export->byte, "NONE");
      }
      if (strncmp(uc(cfg->export->byte), "NONE", 4) != 0 &&
	  strncmp(uc(cfg->export->byte), "SIGMA", 5) != 0 &&
	  strncmp(uc(cfg->export->byte), "MINMAX", 6) != 0 &&
	  strncmp(uc(cfg->export->byte), "TRUNCATE", 8) != 0 &&
	  strncmp(uc(cfg->export->byte), "HISTOGRAM_EQUALIZE", 18) != 0) {
	asfPrintError("Selected scaling method (%s) not supported\n",
		      cfg->export->byte);
      }
      // Force polarimetric results with colormap to byte mapping
      if (strcmp_case(cfg->import->image_data_type, 
		      "POLARIMETRIC_PARAMETER") == 0 &&
	  strlen(cfg->import->polsarpro_colormap) > 0)
	strcpy(cfg->export->byte, "SIGMA"); 
   }
    
    // Only allow PolSARPro as export format if we are actually dealing
    // with PolSARPro data as input as well
    if (strncmp_case(cfg->export->format, "POLSARPRO", 9) == 0 &&
	strncmp_case(cfg->import->format, "POLSARPRO", 9) != 0)
      asfPrintError("Export format 'POLSARPRO' requires the same format "
		    "as input format!\n");

    // If RGB Banding option is "ignore,ignore,ignore" then the
    // user has probably been using the gui, and didn't pick
    // anything for any of the RGB channels.
    int rgbBanding = strlen(cfg->export->rgb) > 0 ? 1 : 0;
    if (rgbBanding &&
	strcmp(uc(cfg->export->rgb), "IGNORE,IGNORE,IGNORE") == 0)
      {
	asfPrintError(
		      "Exporting as RGB was selected, but no values for each RGB\n"
		      "channels were selected.  Please choose which bands you wish\n"
		      "to have placed into at least one of the RGB channels.\n");
      }
    
    // RGB Banding, True Color, and False Color are mutually-exclusive
    // options.  Check to make sure the config doesn't spec more than
    // one of these.
  }
  int rgbBanding = strlen(cfg->export->rgb) > 0 ? 1 : 0;
  int truecolor = cfg->export->truecolor == 0 ? 0 : 1;
  int falsecolor = cfg->export->falsecolor == 0 ? 0 : 1;
  if (rgbBanding + truecolor + falsecolor > 1)
    {
      asfPrintError("More than one color export mode was selected (rgb banding, truecolor\n"
		    "or falsecolor)  Only one of these options may be selected at a time.\n");
    }
  
  if (!cfg->general->import && !cfg->general->sar_processing &&
      !cfg->general->polarimetry && !cfg->general->terrain_correct &&
      !cfg->general->geocoding && !cfg->general->export) {
    asfPrintError("Invalid configuration file found (%s) or no processing\n"
		  "is enabled, e.g. import, terrain correction, geocoding,\n"
		  "and export processing flags are all set to zero in the\n"
		  "configuration file.\n", configFileName);
  }
  
  // Check input
  if (cfg->general->polarimetry)
    check_input(cfg, "polarimetry", cfg->general->in_name);

  // If we got here, all checks passed
  return TRUE;
}

char ***do_import(convert_config *cfg)
{
  char **imported_files;
  char **suffixes;

  char *outFile = MALLOC(sizeof(char)*(strlen(cfg->general->in_name) + 256));
  strcpy(outFile, "");

  // Call asf_import, if needed (=> input is not ASF Internal)
  if (cfg->general->import) {
    // Force radiometry to be amplitude for processing until the calibration
    // section.
    radiometry_t radiometry = r_AMP;
    int db_flag = FALSE;
    input_format_t format_type;
    char *meta_option = NULL;
    
    if (strncmp_case(cfg->import->radiometry, "SIGMA_IMAGE", 11) == 0 ||
	strncmp_case(cfg->import->radiometry, "GAMMA_IMAGE", 11) == 0 ||
	strncmp_case(cfg->import->radiometry, "BETA_IMAGE", 10) == 0) {
      cfg->general->calibration = TRUE;
      if (strncmp_case(cfg->import->radiometry, "SIGMA_IMAGE", 11) == 0) {
	if (cfg->import->output_db)
	  strcpy(cfg->calibrate->radiometry, "SIGMA_DB");
	else
	  strcpy(cfg->calibrate->radiometry, "SIGMA");
      }
      else if (strncmp_case(cfg->import->radiometry, "GAMMA_IMAGE", 11) == 0) {
	if (cfg->import->output_db)
	  strcpy(cfg->calibrate->radiometry, "GAMMA_DB");
	else
	  strcpy(cfg->calibrate->radiometry, "GAMMA");
      }
      else if (strncmp_case(cfg->import->radiometry, "BETA_IMAGE", 10) == 0) {
	if (cfg->import->output_db)
	  strcpy(cfg->calibrate->radiometry, "BETA_DB");
	else
	  strcpy(cfg->calibrate->radiometry, "BETA");
      }
    }
    
    // Generate a temporary output filename
    if (cfg->general->image_stats || cfg->general->detect_cr ||
	cfg->general->sar_processing || cfg->general->polarimetry ||
	cfg->general->terrain_correct || cfg->general->geocoding ||
	cfg->general->export) {
      sprintf(outFile, "%s/import", cfg->general->tmp_dir);
    }
    else {
      sprintf(outFile, "%s", cfg->general->out_name);
    }

    // Input Format Type
    char *polsarpro_colormap = NULL;
    //printf("\n\nINPUT FORMAT IS: %s\n\n", uc(cfg->import->format));
    if (strncmp_case(cfg->import->format, "CEOS", 4) == 0)
      format_type = CEOS;
    else if (strncmp_case(cfg->import->format, "STF", 3) == 0)
      format_type = STF;
    else if (strncmp_case(cfg->import->format, "GEOTIFF", 7) == 0)
      format_type = GENERIC_GEOTIFF;
    else if (strncmp_case(cfg->import->format, "BIL", 3) == 0)
      format_type = BIL;
    else if (strncmp_case(cfg->import->format, "GRIDFLOAT", 9) == 0)
      format_type = GRIDFLOAT;
    else if (strncmp_case(cfg->import->format, "AIRSAR", 6) == 0)
      format_type = AIRSAR;
    else if (strncmp_case(cfg->import->format, "VP", 2) == 0)
      format_type = VP;
    else if (strncmp_case(cfg->import->format, "POLSARPRO", 9) == 0) {
      char *cm = cfg->import->polsarpro_colormap; // convenience pointer
      if (cm && strlen(cm)) {
	polsarpro_colormap = (char*)MALLOC((strlen(cm) + 1) * sizeof(char));
	strcpy(polsarpro_colormap, cm);
      }
      format_type = POLSARPRO;
    }
    else if (strncmp_case(cfg->import->format, "TERRASAR", 8) == 0)
      format_type = TERRASAR;
    else if (strncmp_case(cfg->import->format, "RADARSAT2", 9) == 0)
      format_type = RADARSAT2;
    else if (strncmp_case(cfg->import->format, "ALOS_MOSAIC", 11) == 0)
      format_type = ALOS_MOSAIC;
    else if (strncmp_case(cfg->import->format, "GAMMA", 5) == 0)
      format_type = GAMMA;
    else if (strncmp_case(cfg->import->format, "ROIPAC", 6) == 0)
      format_type = ROIPAC;
    else if (strncmp_case(cfg->import->format, "UAVSAR", 6) == 0)
      format_type = UAVSAR;
    else {
      asfPrintError("Unknown Format: %s\n", cfg->import->format);
      format_type = CEOS; // actually this is not reached
    }
    
    // meta option (needed for GAMMA) -- leave NULL if not specified
    if (strlen(cfg->import->metadata_file) > 0)
      meta_option = cfg->import->metadata_file;
    
    // Call asf_import!
    check_return(asf_import(radiometry, db_flag,
			    cfg->import->complex_slc,
			    cfg->import->multilook_slc,
			    FALSE, // amp0_flag -- no longer used by asf_mapready
			    format_type,
			    NULL,
			    NULL,
			    cfg->import->image_data_type,
			    cfg->import->lut, cfg->import->prc,
			    cfg->import->lat_begin, cfg->import->lat_end,
			    -99, -99,
			    cfg->import->line, cfg->import->sample,
			    cfg->import->width, cfg->import->height,
			    cfg->general->intermediates, NULL, NULL, NULL,
			    cfg->import->ers2_gain_fix,
			    meta_option,
			    cfg->general->in_name,
			    cfg->general->ancillary_file,
			    polsarpro_colormap,
			    cfg->import->slave_metadata,
			    cfg->import->interferogram,
			    cfg->import->coherence,
			    cfg->import->baseline,
			    cfg->import->complex_gamma,
			    cfg->import->uavsar,
			    FALSE,
			    outFile),
		 "ingesting data file (asf_import)\n");
    FREE(polsarpro_colormap);

    int is_airsar = strncmp_case(cfg->import->format, "AIRSAR", 6) == 0;
    int is_uavsar = strncmp_case(cfg->import->format, "UAVSAR", 6) == 0;

    if (is_airsar) {

      int ii,max=10;
      imported_files = (char**)MALLOC(sizeof(char*)*max);
      suffixes = (char**)MALLOC(sizeof(char*)*max);
      for (ii=0; ii<max; ++ii) {
        imported_files[ii] = NULL;
        suffixes[ii] = NULL;
      }

      ii=0;

      // For AirSAR data, let's see what we actually got.
      // Not all products are always present - update settings to
      // disable processing of products not present

      if (cfg->airsar->l_pol && !check_airsar(outFile, "_l")) {
	asfPrintStatus("No L-band polarimetric AirSAR product.\n");
	cfg->airsar->l_pol = 0;
      }
      if (cfg->airsar->p_pol && !check_airsar(outFile, "_p")) {
	asfPrintStatus("No P-band polarimetric AirSAR product.\n");
	cfg->airsar->p_pol = 0;
      }
      if (cfg->airsar->c_pol && !check_airsar(outFile, "_c")) {
	asfPrintStatus("No C-band polarimetric AirSAR product.\n");
	cfg->airsar->c_pol = 0;
      }
      if (cfg->airsar->c_vv && !check_airsar(outFile, "_c_vv")) {
	asfPrintStatus("No C-band interferometric AirSAR product.\n");
	cfg->airsar->c_vv = 0;
      }
      if (cfg->airsar->l_vv && !check_airsar(outFile, "_l_vv")) {
	asfPrintStatus("No L-band interferometric AirSAR product.\n");
	cfg->airsar->l_vv = 0;
      }
      
      if (!cfg->airsar->l_pol && !cfg->airsar->p_pol &&
	  !cfg->airsar->c_pol && !cfg->airsar->c_vv &&
	  !cfg->airsar->l_vv)
	asfPrintError("No airsar products to process!\n");

      if (cfg->airsar->l_pol) {
        suffixes[ii] = STRDUP("_l");
        imported_files[ii] = appendToBasename(outFile, suffixes[ii]);
        ++ii;
      }
      if (cfg->airsar->p_pol) {
        suffixes[ii] = STRDUP("_p");
        imported_files[ii] = appendToBasename(outFile, suffixes[ii]);
        ++ii;
      }
      if (cfg->airsar->c_pol) {
        suffixes[ii] = STRDUP("_c");
        imported_files[ii] = appendToBasename(outFile, suffixes[ii]);
        ++ii;
      }
      if (cfg->airsar->c_vv) {
        suffixes[ii] = STRDUP("_c_vv");
        imported_files[ii] = appendToBasename(outFile, suffixes[ii]);
        ++ii;
        if (check_airsar(outFile, "_c_dem")) {
          suffixes[ii] = STRDUP("_c_dem");
          imported_files[ii] = appendToBasename(outFile, suffixes[ii]);
          ++ii;
        }
        if (check_airsar(outFile, "_c_coh")) {
          suffixes[ii] = STRDUP("_c_coh");
          imported_files[ii] = appendToBasename(outFile, suffixes[ii]);
          ++ii;
        }
      }
      if (cfg->airsar->l_vv) {
        suffixes[ii] = STRDUP("_l_vv");
        imported_files[ii] = appendToBasename(outFile, suffixes[ii]);
        ++ii;
        if (check_airsar(outFile, "_l_dem")) {
          suffixes[ii] = STRDUP("_l_dem");
          imported_files[ii] = appendToBasename(outFile, suffixes[ii]);
          ++ii;
        }
        if (check_airsar(outFile, "_l_coh")) {
          suffixes[ii] = STRDUP("_l_coh");
          imported_files[ii] = appendToBasename(outFile, suffixes[ii]);
          ++ii;
        }
      }

      asfPrintStatus("Found %d AIRSAR file%s to process.\n", ii,
                     ii==1 ? "" : "s");
    }
    else if (is_uavsar) {

      int ii, product_count;
      char *type = check_data_type(cfg->general->in_name);
      char **product = get_uavsar_products(cfg->import->uavsar, type, 
					   &product_count);

      // the +1 is necessary, since we have one extra slot for NULL
      imported_files = MALLOC(sizeof(char*)*(product_count+1));
      imported_files[product_count] = NULL;

      // uavsar files don't need the suffix, export does some special
      // translation for them on output, so we will set all of these to NULL
      suffixes = MALLOC(sizeof(char*)*(product_count+1));
      suffixes[product_count] = NULL;

      if (strcmp_case(type, "POLSAR") == 0) {
	for (ii = 0; ii < product_count; ii++) {
          suffixes[ii] = NULL;
          imported_files[ii] = MALLOC(sizeof(char)*(strlen(outFile) + 64));
          
          if (product_count > 1)
            sprintf(imported_files[ii], "%s_%s", outFile, lc(product[ii]));
          else
            strcpy(imported_files[ii], outFile);

	  if (strcmp_case(product[ii], "SLC") == 0)
	    cfg->uavsar->slc = 1;
	  if (strcmp_case(product[ii], "MLC") == 0)
	    cfg->uavsar->mlc = 1;
	  if (strcmp_case(product[ii], "DAT") == 0)
	    cfg->uavsar->dat = 1;
	  if (strcmp_case(product[ii], "GRD") == 0)
	    cfg->uavsar->grd = 1;
	  if (strcmp_case(product[ii], "HGT") == 0)
	    cfg->uavsar->hgt = 1;
	}
      }
      else if (strcmp_case(type, "INSAR") == 0) {
	for (ii = 0; ii < product_count; ii++) {
          suffixes[ii] = NULL;
          imported_files[ii] = MALLOC(sizeof(char)*(strlen(outFile) + 64));

          if (product_count > 1)
            sprintf(imported_files[ii], "%s_%s", outFile, lc(product[ii]));
          else
            strcpy(imported_files[ii], outFile);

	  if (strcmp_case(product[ii], "AMP") == 0)
	    cfg->uavsar->amp = 1;
	  if (strcmp_case(product[ii], "INT") == 0)
	    cfg->uavsar->igram = 1;
	  if (strcmp_case(product[ii], "UNW") == 0)
	    cfg->uavsar->unw = 1;
	  if (strcmp_case(product[ii], "COR") == 0)
	    cfg->uavsar->cor = 1;
	  if (strcmp_case(product[ii], "AMP_GRD") == 0)
	    cfg->uavsar->amp_grd = 1;
	  if (strcmp_case(product[ii], "INT_GRD") == 0)
	    cfg->uavsar->int_grd = 1;
	  if (strcmp_case(product[ii], "UNW_GRD") == 0)
	    cfg->uavsar->unw_grd = 1;
	  if (strcmp_case(product[ii], "COR_GRD") == 0)
	    cfg->uavsar->cor_grd = 1;
	  if (strcmp_case(product[ii], "HGT_GRD") == 0)
	    cfg->uavsar->hgt_grd = 1;
	}
      }
      else {
        asfPrintError("Impossible UAVSAR type: %s\n", type);
      }
    }
    else {
      // only one import file for all non-Airsar, non-uavsar data
      imported_files = (char**)MALLOC(sizeof(char*)*2);
      suffixes = (char**)MALLOC(sizeof(char*)*1);
      imported_files[0] = STRDUP(outFile);
      imported_files[1] = NULL;
      suffixes[0] = NULL;
    }

    if (!is_airsar && !is_uavsar) {
      // Make sure truecolor/falsecolor are only specified for optical data
      meta_parameters *meta = meta_read(outFile);
      if (meta->optical && cfg->general->terrain_correct) {
	asfPrintError("Terrain correction cannot be applied to optical images (...and\n"
		      "orthorectification is not yet supported.)\n");
      }
      if (!meta->optical && (cfg->export->truecolor || cfg->export->falsecolor)) {
	asfPrintError("Cannot select True Color or False Color output "
		      "with non-optical data\n");
      }
      if (cfg->export->band &&
	  strlen(cfg->export->band) > 0 &&
	  get_band_number(meta->general->bands,
			  meta->general->band_count, cfg->export->band) < 0)
        {
          asfPrintError("Selected export band (%s) does not exist: \n"
                        "   Imported file: %s.img\n"
                        " Available bands: %s\n",
                        cfg->export->band, cfg->general->in_name,
                        meta->general->bands);
        }
      
      meta_free(meta);
    }
  }
  else {
    // skipping import ==> "outFile" (what import should have produced)
    // is really just the original input file (already ASF Internal)
    imported_files = MALLOC(sizeof(char*)*2);
    suffixes = (char**)MALLOC(sizeof(char*)*1);
    imported_files[0] = STRDUP(cfg->general->in_name);
    imported_files[1] = NULL;
    suffixes[0] = NULL;
  }

  FREE(outFile);

  //returning a pointer to two string lists:
  // (1) list of imported files
  // (2) list of suffixes that need to be added to the final output file
  //these lists must be the same length, though the suffix entries can be NULL
  //to indicate that that particular file doesn't have a suffix
  char ***ret = MALLOC(sizeof(char**)*2);

  ret[0] = imported_files;
  ret[1] = suffixes;

  return ret;
}

static char *do_processing(convert_config *cfg, const char *inFile_in, int saveDEM)
{
  char *inFile = MALLOC(sizeof(char)*(strlen(inFile_in) + 256));
  strcpy(inFile, inFile_in);

  char *outFile = MALLOC(sizeof(char)*(strlen(inFile) + 256));
  char *tmpFile = MALLOC(sizeof(char)*(strlen(inFile) + 256));
  strcpy(outFile, inFile);

  // check for UAVSAR
  int uavsar = FALSE;
  meta_parameters *meta = meta_read(inFile_in);
  if (strcmp_case(meta->general->sensor, "UAVSAR") == 0)
    uavsar = TRUE;
  meta_free(meta);

  if (cfg->general->external) {
    
    update_status("Running external program...");
    
    sprintf(outFile, "%s/external", cfg->general->tmp_dir);
    
    char *quoted_inFile = MALLOC(sizeof(char)*(strlen(inFile)+8));
    sprintf(quoted_inFile, "\"%s\"", inFile);
    char *quoted_outFile = MALLOC(sizeof(char)*(strlen(outFile)+8));
    sprintf(quoted_outFile, "\"%s\"", outFile);
    
    int has_log = strstr(cfg->external->cmd, "{Log}") != NULL;
    char *tmpLogFile = NULL;
    
    char *cmd1 = asf_strReplace(cfg->external->cmd, "{Input}", quoted_inFile);
    if (has_log) {
      tmpLogFile = appendExt(outFile, ".log");
      char *quoted_logFile = MALLOC(sizeof(char)*(strlen(tmpLogFile)+8));
      sprintf(quoted_logFile, "\"%s\"", tmpLogFile);
      char *cmd2 = asf_strReplace(cmd1, "{Log}", quoted_logFile);
      FREE(cmd1);
      cmd1 = cmd2;
      FREE(quoted_logFile);
    }
    char *cmd = asf_strReplace(cmd1, "{Output}", quoted_outFile);
    FREE(cmd1);
    
    asfPrintStatus("Running external command:\n  %s\n", cmd);
    asfSystem(cmd);
    
    FREE(quoted_inFile);
    FREE(quoted_outFile);
    
    // external program must have created "<outFile>.meta" and "<outFile>.img"
    char *imgFile = appendExt(outFile, ".img");
    int img_exists = fileExists(imgFile);
    char *metaFile = appendExt(outFile, ".meta");
    int meta_exists = fileExists(metaFile);
    if (!img_exists && !meta_exists)
      asfPrintError("The external command:\n  %s\n"
		    "did not produce these expected files:\n"
		    "  %s\n  %s\n", cmd, imgFile, metaFile);
    else if (!img_exists)
      asfPrintError("The external command:\n  %s\n"
		    "did not produce the expected file:\n"
		    "  %s\n", cmd, imgFile);
    else if (!meta_exists)
      asfPrintError("The external command:\n  %s\n"
		    "did not produce the expected file:\n"
		    "  %s\n", cmd, metaFile);
    
    // if external program created a log file, eat it into our own
    if (has_log && tmpLogFile && fileExists(tmpLogFile)) {
      asfPrintToLogOnly("\nAttempting to import external log file.\n"
			"File: %s\n---- Begin external log\n", tmpLogFile);
      FILE *lf = fopen(tmpLogFile, "r");
      if (lf) {
        char line[1024];
        while (fgets(line, 1023, lf) != NULL) {
	  asfPrintToLogOnly("%s", line);
        }
        fclose(lf);
      }
      else {
        asfPrintToLogOnly("Could not open log file: %s\n", strerror(errno));
      }
      asfPrintToLogOnly("---- End external log\n\n");
    }
    
    FREE(cmd);
    FREE(imgFile);
    FREE(metaFile);
    FREE(tmpLogFile);
  }
  
  if (cfg->general->sar_processing) {
    update_status("Running ArDop...");
    
    // Check whether the input file is a raw image.
    // If not, skip the SAR processing step
    meta_parameters *meta = meta_read(outFile);
    if (meta->general->image_data_type == RAW_IMAGE) {
      
      sprintf(inFile, "%s", outFile);
      if (cfg->general->image_stats || cfg->general->detect_cr ||
	  cfg->general->polarimetry || cfg->general->terrain_correct ||
	  cfg->general->geocoding || cfg->general->export) {
	sprintf(outFile, "%s/sar_processing", cfg->general->tmp_dir);
      }
      else {
	sprintf(outFile, "%s", cfg->general->out_name);
      }
      
      struct INPUT_ARDOP_PARAMS *params_in;
      params_in = get_input_ardop_params_struct(inFile, outFile);
      int one = 1;
      
      // Radiometry
      if (strncmp(uc(cfg->sar_processing->radiometry), "POWER_IMAGE", 11) == 0) {
	params_in->pwrFlag = &one;
      } else if (strncmp(uc(cfg->sar_processing->radiometry), "SIGMA_IMAGE", 11) == 0) {
	params_in->sigmaFlag = &one;
      } else if (strncmp(uc(cfg->sar_processing->radiometry), "GAMMA_IMAGE", 11) == 0) {
	params_in->gammaFlag = &one;
      } else if (strncmp(uc(cfg->sar_processing->radiometry), "BETA_IMAGE", 10) == 0) {
	params_in->betaFlag = &one;
      }
      
      // When terrain correcting, add the deskew flag
      if (cfg->general->terrain_correct)
	params_in->deskew = &one;
      
      ardop(params_in);
      
      if (strcmp_case(cfg->sar_processing->radiometry, "AMPLITUDE_IMAGE") == 0)
	sprintf(outFile, "%s/sar_processing_amp", cfg->general->tmp_dir);
      else if (strcmp_case(cfg->sar_processing->radiometry, "POWER_IMAGE") == 0)
	sprintf(outFile, "%s/sar_processing_power", cfg->general->tmp_dir);
      else if (strcmp_case(cfg->sar_processing->radiometry, "SIGMA_IMAGE") == 0)
	sprintf(outFile, "%s/sar_processing_sigma", cfg->general->tmp_dir);
      else if (strcmp_case(cfg->sar_processing->radiometry, "GAMMA_IMAGE") == 0)
	sprintf(outFile, "%s/sar_processing_gamma", cfg->general->tmp_dir);
      else if (strcmp_case(cfg->sar_processing->radiometry, "BETA_IMAGE") == 0)
	sprintf(outFile, "%s/sar_processing_beta", cfg->general->tmp_dir);
      else
	asfPrintError("Unexpected radiometry: %s\n",
		      cfg->sar_processing->radiometry);
      
      // If we are not going to be terrain correcting, get the output
      // from ardop into ground range (terrain correction would do that
      // for us).
      if (!cfg->general->terrain_correct)
        {
	  asfPrintStatus("Converting to ground range.\n");
	  sprintf(inFile, "%s", outFile);
	  sprintf(outFile, "%s_gr", inFile);
	  sr2gr(inFile, outFile);
        }
    }
    else {
      asfPrintStatus("Image has already been processed - skipping SAR processing step");
    }
    meta_free(meta);
  }
  
  if (cfg->general->c2p) {
    char inDataName[512];
    char *baseName = (char *) MALLOC(sizeof(char)*512);
    baseName = get_basename(inFile);
    sprintf(inDataName, "%s.img", baseName);
    
    update_status("Converting Complex to Polar...");
    
    sprintf(inFile, "%s", outFile);
    if (cfg->general->polarimetry || cfg->general->terrain_correct ||
	cfg->general->geocoding || cfg->general->export)
      {
	sprintf(outFile, "%s/c2p", cfg->general->tmp_dir);
      }
    else
      {
	sprintf(outFile, "%s", cfg->general->out_name);
      }
    
    c2p(inDataName, outFile, cfg->c2p->multilook, TRUE);
  }
  
  if (cfg->general->image_stats) {
    char values[255];
    
    update_status("Running Image Stats...");
    
    // Values for statistics
    if (strncmp(uc(cfg->image_stats->values), "LOOK", 4) == 0) {
      sprintf(values, "-look");
    } else if (strncmp(uc(cfg->image_stats->values), "INCIDENCE", 9) == 0) {
      sprintf(values, "-incidence");
    } else if (strncmp(uc(cfg->image_stats->values), "RANGE", 5) == 0) {
      sprintf(values, "-range");
    }
    
    // Pass in command line
    sprintf(inFile, "%s", outFile);
    if (cfg->general->polarimetry || cfg->general->terrain_correct ||
	cfg->general->geocoding || cfg->general->export)
      {
        sprintf(outFile, "%s/image_stats", cfg->general->tmp_dir);
      }
    else {
      sprintf(outFile, "%s", cfg->general->out_name);
    }
    check_return(image_stats(inFile, outFile, values, cfg->image_stats->bins,
			     cfg->image_stats->interval),
		 "running statistics on data file (image_stats)\n");
  }
  
  if (cfg->general->detect_cr) {
    
    update_status("Detecting Corner Reflectors...");
    
    // Intermediate results
    if (cfg->general->intermediates) {
      cfg->detect_cr->chips = 1;
      cfg->detect_cr->text = 1;
    }
    
    // Pass in command line
    sprintf(inFile, "%s", outFile);
    if (cfg->general->polarimetry || cfg->general->polarimetry ||
	cfg->general->geocoding || cfg->general->export) {
      sprintf(outFile, "%s/detect_cr", cfg->general->tmp_dir);
    }
    else {
      sprintf(outFile, "%s", cfg->general->out_name);
    }
    check_return(detect_cr(inFile, cfg->detect_cr->cr_location, outFile,
			   cfg->detect_cr->chips, cfg->detect_cr->text),
		 "detecting corner reflectors (detect_cr)\n");
  }
  
  
  if (cfg->general->polarimetry) {
    
    // Here only the faraday rotation correction is applied. Any other
    // polarimetric processing is done after terrain correction and calibration.

    int doing_far = cfg->polarimetry->farcorr;
    
    if (doing_far) {
      update_status("Applying Faraday rotation correction ...");
      
      // Pass in command line for faraday correction
      sprintf(inFile, "%s", outFile);
      if (cfg->general->terrain_correct || cfg->general->geocoding ||
	  cfg->general->export)
        {
          sprintf(outFile, "%s/faraday_correction", cfg->general->tmp_dir);
        }
      else {
	sprintf(outFile, "%s", cfg->general->out_name);
      }
      
      int keep_flag = cfg->general->intermediates;
      int single_angle_flag = (FARCORR_MEAN == cfg->polarimetry->farcorr);
      asfPrintStatus("\nApplying Faraday Rotation correction.\n");
      faraday_correct(inFile, outFile, cfg->polarimetry->farcorr_threshold,
		      keep_flag, single_angle_flag, r_AMP, 599);
      asfPrintStatus("Done.\n\n");
      
      sprintf(tmpFile, "%s/import_farrot.img", cfg->general->tmp_dir);
      save_intermediate(cfg, "Faraday", tmpFile);
    }    
  }
  
  if (cfg->general->terrain_correct) {
    
    // If the DEM is a GeoTIFF, we need to import it, and geocode it.
    if (has_tiff_ext(cfg->terrain_correct->dem)) {
      char * converted_dem =
	convert_tiff(cfg->terrain_correct->dem, "dem", cfg, saveDEM);
      strcpy(cfg->terrain_correct->dem, converted_dem);
      free(converted_dem);
    }
    
    // If the Mask is a GeoTIFF, we need to import it, and geocode it.
    if (cfg->terrain_correct->mask &&
	strlen(cfg->terrain_correct->mask) > 0 &&
	has_tiff_ext(cfg->terrain_correct->mask))
      {
	char * converted_mask =
	  convert_tiff(cfg->terrain_correct->mask, "mask", cfg, saveDEM);
	strcpy(cfg->terrain_correct->mask, converted_mask);
	free(converted_mask);
      }
    
    // Generate filenames
    sprintf(inFile, "%s", outFile);
    sprintf(outFile, "%s/terrain_correct", cfg->general->tmp_dir);
    
    // Call asf_terrcorr!  Or refine_geolocation!
    if (cfg->terrain_correct->refine_geolocation_only) {
      update_status("Refining Geolocation...");
      check_return(
		   refine_geolocation(inFile, cfg->terrain_correct->dem,
				      cfg->terrain_correct->mask, outFile, FALSE,
				      cfg->terrain_correct->auto_mask_water,
				      cfg->terrain_correct->water_height_cutoff,
				      !cfg->general->intermediates, NULL),
		   "refining geolocation of the data file (refine_geolocation)\n");
    }
    else {
      update_status("Terrain Correcting...");
      check_return(
		   asf_terrcorr_ext(inFile, cfg->terrain_correct->dem,
				    cfg->terrain_correct->mask, outFile,
				    cfg->terrain_correct->pixel,
				    !cfg->general->intermediates,
				    !cfg->terrain_correct->no_resampling,
				    FALSE, cfg->terrain_correct->interp,
				    TRUE, 20, TRUE, cfg->terrain_correct->fill_value,
				    cfg->terrain_correct->auto_mask_water,
				    cfg->terrain_correct->save_terrcorr_dem, FALSE,
				    cfg->terrain_correct->water_height_cutoff,
				    cfg->terrain_correct->do_radiometric,
				    cfg->terrain_correct->smooth_dem_holes,
				    NULL, cfg->terrain_correct->no_matching,
				    cfg->terrain_correct->range_offset,
				    cfg->terrain_correct->azimuth_offset,
				    cfg->terrain_correct->use_gr_dem,
				    TRUE, // use_speckle
				    cfg->terrain_correct->if_coreg_fails_use_zero_offsets,
				    FALSE,
            cfg->terrain_correct->save_incid_angles), // no ground range DEM saving
		   "terrain correcting data file (asf_terrcorr)\n");
    }
    
    // save the simulated sar image intermediate
    char *dem_basename = get_basename(cfg->terrain_correct->dem);
    sprintf(tmpFile, "%s/%s_sim_sar.img", cfg->general->tmp_dir,
	    dem_basename);
    save_intermediate(cfg, "Simulated SAR", tmpFile);
    sprintf(tmpFile, "%s/import_slant.img", cfg->general->tmp_dir);
    save_intermediate(cfg, "Imported Slant Range", tmpFile);
    free(dem_basename);

    if (!cfg->general->export && !cfg->general->geocoding &&
	!cfg->general->calibration) {
      // if this was the last step, get the terrain corrected output
      // to the output directory -- as well as any other needed files
      renameImgAndMeta(outFile, cfg->general->out_name);
      
      // this is to get the thumbnail code all set -- it will use
      // "outFile" as the file to generate the thumbnail from
      strcpy(outFile, cfg->general->out_name);
    }
  }
  
  if ((cfg->general->calibration && !cfg->general->polarimetry) ||
      cfg->polarimetry->freeman_durden ||
      (cfg->polarimetry->pauli && !uavsar) ||
      cfg->polarimetry->cloude_pottier ||
      cfg->polarimetry->cloude_pottier_ext ||
      cfg->polarimetry->cloude_pottier_nc) {
    update_status("Applying calibration parameters...");
    
    // Generate filenames
    sprintf(inFile, "%s", outFile);
    sprintf(outFile, "%s/calibrate", cfg->general->tmp_dir);
    
    // Check radiometry
    radiometry_t radiometry;
    if (strcmp_case(cfg->calibrate->radiometry, "SIGMA") == 0)
      radiometry = r_SIGMA;
    else if (strcmp_case(cfg->calibrate->radiometry, "BETA") == 0)
      radiometry = r_BETA;
    else if (strcmp_case(cfg->calibrate->radiometry, "GAMMA") == 0)
      radiometry = r_GAMMA;
    else if (strcmp_case(cfg->calibrate->radiometry, "SIGMA_DB") == 0)
      radiometry = r_SIGMA_DB;
    else if (strcmp_case(cfg->calibrate->radiometry, "BETA_DB") == 0)
      radiometry = r_BETA_DB;
    else if (strcmp_case(cfg->calibrate->radiometry, "GAMMA_DB") == 0)
      radiometry = r_GAMMA_DB;
    else
      asfPrintError("No valid radiometry (%s) given!\n", 
		    cfg->calibrate->radiometry);
    check_return(asf_calibrate(inFile, outFile, radiometry,
			       cfg->calibrate->wh_scale),
		 "Applying calibration parameters (asf_calibrate)\n");

    // File name shuffling if needed
    if (!cfg->general->geocoding && !cfg->general->export) {
      renameImgAndMeta(outFile, cfg->general->out_name);
      strcpy(outFile, cfg->general->out_name);
    }
  }

  if (cfg->general->polarimetry) {

    // Since a potential radiometric terrain correction needs to be done on the
    // original amplitude data, any polarimetric processing that uses data
    // derived from amplitude and phase needs to happen afterwards.

    int doing_pol = cfg->polarimetry->pauli
      + cfg->polarimetry->sinclair
      + cfg->polarimetry->cloude_pottier
      + cfg->polarimetry->cloude_pottier_ext
      + cfg->polarimetry->cloude_pottier_nc
      + cfg->polarimetry->freeman_durden
      + cfg->polarimetry->k_means_wishart
      + cfg->polarimetry->k_means_wishart_ext;

    if (doing_pol) {
      update_status("Polarimetric processing ...");
      
      // Pass in command line for polarimetry
      sprintf(inFile, "%s", outFile);
      if (cfg->general->geocoding ||
	  cfg->general->export)
        {
          sprintf(outFile, "%s/polarimetry", cfg->general->tmp_dir);
        }
      else {
	sprintf(outFile, "%s", cfg->general->out_name);
      }

      calc_polarimetry(cfg, inFile, outFile);
    }
  }

  datum_type_t datum = str_to_datum(cfg->geocoding->datum);
  if (datum == UNKNOWN_DATUM) {
    asfPrintWarning("Unrecognized, missing, or unsupported datum found in configuration\n"
                    "file (%s).  Defaulting to WGS-84 unless reading parameters from a\n"
		    "projection definition (*.proj) file.  The proj file settings will\n"
		    "override all else.\n", uc(cfg->geocoding->datum));
    datum = WGS84_DATUM;
  }

  if (cfg->general->geocoding) {
    update_status("Geocoding...");
    int force_flag = cfg->geocoding->force;
    resample_method_t resample_method = RESAMPLE_BILINEAR;
    double average_height = cfg->geocoding->height;
    double pixel_size = cfg->geocoding->pixel;
    float background_val = cfg->geocoding->background;
    
    // When terrain correcting, ignore average height -- the height
    // has already been corrected for.
    if (cfg->general->terrain_correct &&
	!cfg->terrain_correct->refine_geolocation_only &&
	cfg->geocoding->height != 0)
      {
	asfPrintWarning("Since terrain correction was applied, ignoring "
			"average height specification\nfor geocoding.\n");
	average_height = 0.0;
      }
    
    // Datum
    
    // Resampling method
    if (strncmp(uc(cfg->geocoding->resampling), "NEAREST_NEIGHBOR", 16) == 0) {
      resample_method = RESAMPLE_NEAREST_NEIGHBOR;
    }
    if (strncmp(uc(cfg->geocoding->resampling), "BILINEAR", 8) == 0) {
      resample_method = RESAMPLE_BILINEAR;
    }
    if (strncmp(uc(cfg->geocoding->resampling), "BICUBIC", 7) == 0) {
      resample_method = RESAMPLE_BICUBIC;
    }
    
    sprintf(inFile, "%s", outFile);
    if (cfg->general->export || cfg->general->testdata) {
      sprintf(outFile, "%s/geocoding", cfg->general->tmp_dir);
    }
    else {
      sprintf(outFile, "%s%s%s",
	      cfg->general->prefix,
	      cfg->general->out_name,
	      cfg->general->suffix);
    }
    
    // Pass in command line
    check_return(asf_geocode_from_proj_file(cfg->geocoding->projection,
                                            force_flag, resample_method, average_height, datum,
					    pixel_size, NULL, inFile, outFile, background_val),
		   "geocoding data file (asf_geocode)\n");
  }
  
  if (cfg->general->testdata) {
    
    // Set up filenames
    sprintf(inFile, "%s", outFile);
    if (cfg->general->export) {
      sprintf(outFile, "%s/testdata", cfg->general->tmp_dir);
    }
    else {
      sprintf(outFile, "%s%s%s",
	      cfg->general->prefix,
	      cfg->general->out_name,
	      cfg->general->suffix);
    }
    check_return(trim(inFile, outFile,
		      cfg->testdata->sample, cfg->testdata->line,
		      cfg->testdata->width, cfg->testdata->height),
		 "generating test data set (trim)\n");
  }

  char *save_before_export = STRDUP(outFile);

  if (cfg->general->export) {
    // Set up filenames
    strcpy(inFile, outFile);
    strcpy(outFile, cfg->general->out_name);

    update_status("Exporting...");
    asfPrintStatus("Exporting... (%s) -> (%s)\n",inFile,outFile);
    do_export(cfg, inFile, outFile);
  }
  else {
    // result of geocoding is the final output file, since we are
    // not exporting
    char *imgFile = appendExt(outFile, ".img");
    char *metaFile = appendExt(outFile, ".meta");
    save_intermediate(cfg, "Output", imgFile);
    save_intermediate(cfg, "Meta", metaFile);
    free(imgFile);
    free(metaFile);
  }

  FREE(outFile);
  FREE(tmpFile);
  FREE(inFile);

  return save_before_export;
}

static int asf_convert_file(char *configFileName, int saveDEM)
{
  char inFile[512], outFile[512];

  convert_config *cfg = read_convert_config(configFileName);
  if (cfg->general->status_file && strlen(cfg->general->status_file) > 0)
    set_status_file(cfg->general->status_file);
  
  update_status("Processing...");
  
  // these are so we can tell how long processing took
  ymd_date start_date;
  hms_time start_time;
  get_current_date(&start_date, &start_time);
  
  char tmp[64];
  date_printTime(&start_time,0,':',tmp);
  asfPrintStatus("Starting at: %s\n", tmp);
  
  create_and_set_tmp_dir(cfg->general->in_name, cfg->general->out_name,
			 cfg->general->tmp_dir);
  save_intermediate(cfg, "Temp Dir", cfg->general->tmp_dir);

  if (!check_config(configFileName, cfg))
    return 0;

  //---------------------------------------------------------------
  // Let's get to work
  if (strlen(cfg->general->out_name) == 0) {
    sprintf(cfg->general->out_name, "%s", cfg->general->in_name);
  }
  sprintf(outFile, "%s", cfg->general->out_name);

  // global variable-- if set, tells meta_write to also dump .hdr (ENVI) files
  dump_envi_header = cfg->general->dump_envi;

  char *first_pre_export = NULL;

  // Let's import some files!
  update_status("Importing...");

  // import returns two lists of strings
  char ***lists = do_import(cfg);

  // This is the list of files that we get after doing the import.
  // Each of them will have to be processed
  char **imported_files = lists[0];

  // This is the list of suffixes that have to be added to the final
  // output file (i.e., user gave "blah.tif" we must create "blah_suffix.tif"
  char **suffixes = lists[1];

  // Report/count what we got
  int ii, num_imported_files = 0, num_processed_files = 0;
  char **current = imported_files;
  while (*current) {
    asfPrintStatus("Imported file: %s\n", *current);
    ++current;
    ++num_imported_files;
  }
  asfPrintStatus("%d imported file%s to process.\n", num_imported_files,
                 num_imported_files == 1 ? "" : "s");

  // Now process them all
  char *original_output_filename = STRDUP(cfg->general->out_name);
  for (ii=0; ii<num_imported_files; ++ii) {
    asfPrintStatus("Processing: %s\n", imported_files[ii]);

    // If the file does not exist, just skip it, do not error out.
    // sometimes certain types are not supported
    if (!imgFileExists(imported_files[ii])) {
      asfPrintStatus("Does not exist: %s\n", imported_files[ii]);
      asfPrintStatus("Skipping.\n");
      continue;
    }

    // set up the output filename with the right suffix so no files
    // are overwritten by the different outputs
    if (suffixes && suffixes[ii] && strlen(suffixes[ii]) > 0) {
      asfPrintStatus("Output file suffix: %s\n", suffixes[ii]); 
      
      char *tmp = appendToBasename(original_output_filename, suffixes[ii]);
      strcpy(cfg->general->out_name, tmp);
      FREE(tmp);
    }
    else {
      strcpy(cfg->general->out_name, original_output_filename);
    } 

    char *result = do_processing(cfg, imported_files[ii], saveDEM);
    asfPrintStatus("Result: %s\n", result);

    // save the output (pre-export, so it is ASF internal) if it is the
    // first one, we will use it for the thumbnail etc
    if (!first_pre_export)
      first_pre_export = result;
    else
      FREE(result);

    ++num_processed_files;
  }

  if (!first_pre_export)
    asfPrintError("No files successfully processed.\n");

  for (ii=0; ii<num_imported_files; ++ii) {
    FREE(suffixes[ii]);
    FREE(imported_files[ii]);
  }

  FREE(imported_files);
  FREE(suffixes);
  FREE(lists);
  FREE(original_output_filename);

  if (num_imported_files > 1) {
    asfPrintStatus("%d/%d imported files were processed.\n",
                   num_processed_files, num_imported_files);

    if (num_imported_files == num_processed_files)
      asfPrintStatus("All import files processed.\n");
  }

  strcpy(outFile, first_pre_export);

  //---------------------------------------------------------------------
  // At this point the processing of the SAR image is done.
  // We'll now do some of the extra stuff the user may have asked for.
  strcpy(inFile, outFile);
  
  // Generate a small thumbnail if requested.
  if (cfg->general->thumbnail) {
    asfPrintStatus("Generating Thumbnail image...\n");
    asfPrintStatus("Generating thumbnail from: %s\n", first_pre_export);
    
    output_format_t format = PNG;
    
    // scale using the same scaling as we did for the output image,
    // unless it was a floating-point output (not possible for PNG),
    // in which case we will use SIGMA.
    scale_t scale = get_scale(cfg);
    if (scale == NONE) scale = SIGMA;
    
    meta_parameters *meta;
    double scale_factor;
    int i,n;
    
    char tmpFile[1024];
    char overlayFile[1024];
    
    if (!cfg->general->export)
      sprintf(inFile, "%s", outFile);
    
    if (strlen(inFile) > 0) {
      // Calculate pixel size for generating right size thumbnail
      meta = meta_read(inFile);
      
      // Put the thumbnail in the intermediates directory, if it is
      // being kept, otherwise in the output directory.
      char *basename = get_basename(cfg->general->out_name);
 
      if (cfg->general->intermediates) {
	sprintf(outFile, "%s%c%s_thumb.png",
		cfg->general->tmp_dir, DIR_SEPARATOR, basename);
	sprintf(tmpFile, "%s%c%s_thumb",
		cfg->general->tmp_dir, DIR_SEPARATOR, basename);
	sprintf(overlayFile, "%s%c%s_overlay",
		cfg->general->tmp_dir, DIR_SEPARATOR, basename);
      } else {
	char *tmp = appendToBasename(cfg->general->out_name, "_thumb");
	strcpy(tmpFile, tmp);
	strcpy(outFile, tmp);
	strcat(outFile, ".png");
	tmp = appendToBasename(cfg->general->out_name, "_overlay");
	strcpy(overlayFile, tmp);
	free(tmp);
      }
      
      // outFile is the thumbnail file and the following needs to also look for
      // polsarpro input or not, whether there is a polsarpro colormap specified,
      // and whether or not there is an embedded colormap in the metadata (regardless
      // of type.  Check meta->general->bands, meta->colormap, and cfg->import->polsarpro_colormap
      // => export LUT overrides all, import LUT overrides metadata lut, and metadata LUT is
      //    used if nothing else exists, else default to non-LUT case below.
      char lut_file[2048] = "";
      /*
	int is_polsarpro =
	(strstr(meta->general->bands, "POLSARPRO") != NULL) ||
	((strstr(meta->general->bands, "T11") != NULL) &&
	(strstr(meta->general->bands, "T22") != NULL) &&
	(strstr(meta->general->bands, "T33") != NULL)) ||
	((strstr(meta->general->bands, "C11") != NULL) &&
	(strstr(meta->general->bands, "C22") != NULL));
      */
      int is_polsarpro = 0;
      if (meta->general->image_data_type >  POLARIMETRIC_IMAGE &&
	  meta->general->image_data_type <= POLARIMETRIC_T4_MATRIX)
	is_polsarpro = 1;
      
      if (cfg->export && cfg->export->lut && strlen(cfg->export->lut) > 0) {
	strcpy(lut_file, cfg->export->lut);
      }
      else if (is_polsarpro &&
	       cfg->import  &&
	       cfg->import->polsarpro_colormap &&
	       strlen(cfg->import->polsarpro_colormap) > 0)
	{
	  strcpy(lut_file, cfg->import->polsarpro_colormap);
	}
      else if (is_polsarpro && meta->colormap &&
	       meta->colormap->look_up_table && strlen(meta->colormap->look_up_table) > 0)
	{
	  strcpy(lut_file, meta->colormap->look_up_table);
	}
      if (strlen(cfg->export->lut) > 0 ||
	  (is_polsarpro && strlen(lut_file) > 0))
	{
	  // using a LUT -- we can't downsample in this case, because
	  // that will screw up the behavior of TRUNCATE.  The burden
	  // will be on the GUI to do the downsampling, but after we have
	  // generated an RGB image.
	  char **bands = extract_band_names(meta->general->bands,
					    meta->general->band_count);
	  //            char *bands[2];
	  //            bands[0] = meta->general->bands;
	  //            bands[1] = NULL;
	  
	  check_return(
		       asf_export_bands(format, is_polsarpro ? scale : TRUNCATE, TRUE, 0, 0,
					lut_file, inFile, outFile, bands,
					NULL, NULL),
		       "exporting thumbnail (asf_export), using rgb look up table.\n");
	  int i;
	  for (i=0; i<meta->general->band_count; i++) {
	    FREE(bands[i]);
	  }
	  FREE(bands);
	}
      else {
  scale_factor = meta->general->line_count > meta->general->sample_count ? 
    512./meta->general->line_count : 512./meta->general->sample_count;

	// make sure that only the first band of a multi-band image
	// is resized for generating a thumbnail, for polsarpro
	meta_parameters *metaTmp = NULL;
	int original_band_count = 0;
	if (is_polsarpro && format == (int)POLSARPRO) {
	  metaTmp = meta_read(inFile);
	  original_band_count = metaTmp->general->band_count;
	  if (original_band_count > 1) {
	    metaTmp->general->band_count = 1;
	    meta_write(metaTmp, inFile);
	  }
	  else {
	    meta_free(metaTmp);
	    metaTmp = NULL;
	  }
	}
	
	check_return(
		     resample(inFile, tmpFile, scale_factor,
					scale_factor),
		     "resampling data to thumbnail size (resample)\n");
	
	if (strlen(cfg->export->rgb) > 0) {
	  char *red, *green, *blue;
	  if (split3(cfg->export->rgb, &red, &green, &blue, ',')) {
	    char **bands = find_bands(inFile, 1, red, green, blue, &n);
	    if (n > 0) {
	      check_return(
			   asf_export_bands(format, scale, TRUE, 0, 0, NULL,
					    tmpFile, outFile, bands, NULL, NULL),
			   "exporting thumbnail data file (asf_export), banded\n");
	      // No zipping for the moment
        if(strcmp(meta->general->sensor, "UAVSAR") == 0)
          asfPrintStatus("Skipping KML Overlay Generation for UAVSAR\n");
        else
	        kml_overlay(tmpFile, overlayFile, FALSE);
	      for (i=0; i<n; ++i)
		FREE(bands[i]);
	      FREE(bands);
	    }
	  }
	}
	else { // no rgb bands selected
	  int true_color = cfg->export->truecolor;
	  int false_color = cfg->export->falsecolor;
    if (meta->optical && (true_color || false_color)) {
      if (meta->optical && (true_color || false_color)) {
        // Multi-band optical data, exporting as true or
        // false color single file
        char **bands = extract_band_names(meta->general->bands,
                                          meta->general->band_count);
        if (meta->general->band_count >= 4 && bands != NULL) {
          // The imported file IS a multiband file with enough bands,
          // but the extract bands need to be ordered correctly
          if (true_color) {
            strcpy(bands[0], "03");
            strcpy(bands[1], "02");
            strcpy(bands[2], "01");
            strcpy(bands[3], "");
          }
          else {
            strcpy(bands[0], "04");
            strcpy(bands[1], "03");
            strcpy(bands[2], "02");
            strcpy(bands[3], "");
          }
          check_return(asf_export_bands(format, NONE, TRUE,
                                        true_color, false_color, NULL,
                                        tmpFile, outFile, bands, NULL, NULL),
                       "exporting thumbnail (asf_export), color banded.\n");
          // No zipping for the moment
          kml_overlay(tmpFile, overlayFile, FALSE);
          for (i = 0; i < meta->general->band_count; ++i)
            FREE(bands[i]);
          FREE(bands);
        }
      }
    }
    else {                          // not a true or false color optical image

      if (is_polsarpro &&
          (meta->general->image_data_type > POLARIMETRIC_IMAGE &&
           meta->general->image_data_type <= POLARIMETRIC_T4_MATRIX)) {
	meta_free(meta);
	meta = meta_read(tmpFile);
	meta->general->image_data_type = BROWSE_IMAGE;
        meta->general->band_count = 1;
	meta_write(meta, tmpFile);
      }
      char **bands = extract_band_names(meta->general->bands,
                                        meta->general->band_count);
      int noutputs;
      char **outputs;
      check_return(asf_export_bands(format, scale, FALSE, 0, 0, NULL,
				    tmpFile, outFile, bands, &noutputs,
				    &outputs),
		   "exporting thumbnail data file (asf_export)\n");

      // No zipping for the moment
      if(strcmp(meta->general->sensor, "UAVSAR") == 0)
        asfPrintStatus("Skipping KML Overlay Generation for UAVSAR\n");
      else
        kml_overlay(tmpFile, overlayFile, FALSE);
      // strip off the band name at the end!
      char *banded_name = MALLOC(sizeof(char) * (strlen(outFile) + 32));
      if (cfg->general->intermediates) {
        if (is_polsarpro && meta->general->image_data_type == POLARIMETRIC_IMAGE)
          sprintf(banded_name, "%s/%s_thumb_%s.png",
                  cfg->general->tmp_dir, basename, bands[1]);
        else if (!strcmp(meta->general->sensor, "UAVSAR")) {
            strcpy(banded_name, outputs[0]);
        }
        else
          sprintf(banded_name, "%s/%s_thumb_%s.png",
                  cfg->general->tmp_dir, basename, bands[0]);
        sprintf(outFile, "%s/%s_thumb.png", cfg->general->tmp_dir, basename);
      }
      else {
        if (is_polsarpro && meta->general->image_data_type == POLARIMETRIC_IMAGE)
          sprintf(banded_name, "%s_thumb_%s.png", cfg->general->out_name,
                  bands[1]);
        else if (!strcmp(meta->general->sensor, "UAVSAR"))
          strcpy(banded_name, outputs[0]);
        else
          sprintf(banded_name, "%s_thumb_%s.png", cfg->general->out_name,
                  bands[0]);
        char *tmp = appendToBasename(cfg->general->out_name,
                                     "_thumb");
        strcpy(outFile, tmp);
        strcat(outFile, ".png");
        free(tmp);
      }
      fileRename(banded_name, outFile);
      FREE(banded_name);
      for (i = 0; i < meta->general->band_count; i++)
        FREE(bands[i]);
      FREE(bands);
      for (i = 0; i < noutputs; i++)
        FREE(outputs[i]);
      FREE(outputs);
    }
	}
	
	// in case we had changed the metadata, let's restore things
	if (metaTmp) {
	  metaTmp->general->band_count = original_band_count;
	  meta_write(metaTmp, inFile);
	  meta_free(metaTmp);
	}
      }
      
      meta_free(meta);
      free(basename);
    }
  }
  
  // Process the clipped DEM if requested
  if (cfg->general->terrain_correct &&
      cfg->terrain_correct &&
      cfg->terrain_correct->save_terrcorr_dem) {
    // We know the name of the cut DEM in the temporary directory
    char *tmp;
    int export_dem_ok = TRUE;
    
    if (cfg->terrain_correct->smooth_dem_holes)
      tmp = appendToBasename(cfg->terrain_correct->dem, "_tc_smooth_cut");
    else
      tmp = appendToBasename(cfg->terrain_correct->dem, "_cut");
    char *tmp2 = get_basename(tmp);
    char *tmp3 = appendExt(tmp2, ".img");
    sprintf(inFile, "%s/%s", cfg->general->tmp_dir, tmp3);
    free(tmp);
    free(tmp2);
    free(tmp3);
    if (!fileExists(inFile)) {
      // DEM may have been chunked...
      if (cfg->terrain_correct->smooth_dem_holes)
        tmp = appendToBasename(cfg->terrain_correct->dem, "_tc_smooth_chunk_cut");
      else
        tmp = appendToBasename(cfg->terrain_correct->dem, "_chunk_cut");
      tmp2 = get_basename(tmp);
      tmp3 = appendExt(tmp2, ".img");
      sprintf(inFile, "%s/%s", cfg->general->tmp_dir, tmp3);
      free(tmp);
      free(tmp2);
      free(tmp3);
      if (!fileExists(inFile)) {
	asfPrintWarning("Could not find DEM chunk to export!\n");
	export_dem_ok = FALSE;
      }
    }
    
    if (export_dem_ok) {
      // output name will be the SAR image's name with a "_dem" added
      // to the basename
      tmp = appendToBasename(cfg->general->out_name, "_dem");
      strcpy(outFile, tmp);
      free(tmp);
      
      //Never re-geocode the DEM -- assume user has already put it into
      //their favored projection (since at this time we require that
      //DEMs be geocoded for terrain correction ingest).  So, proceed
      //directly to export.
      if (cfg->general->export) {
        update_status("Exporting clipped DEM... ");
        char *tmp = stripExt(inFile);
        strcpy(inFile, tmp);
        free(tmp);
        asfPrintStatus("Exporting clipped DEM: %s -> %s\n", inFile, outFile);
        check_return(
          asf_export(get_format(cfg), get_scale(cfg), inFile, outFile),
          "exporting clipped dem (asf_export)\n");
      }
      else {
	// User requested that we save the clipped DEM, but chose not
	// to export.  So... just move the clipped DEM out of the tmp dir
	renameImgAndMeta(inFile, outFile);
      }
      
      save_intermediate(cfg, "Clipped DEM", outFile);
    }
  }

  datum_type_t datum = str_to_datum(cfg->geocoding->datum);

  // Process the incidence angles file if requested
  if (cfg->general->terrain_correct &&
      cfg->terrain_correct &&
      cfg->terrain_correct->do_radiometric &&
      cfg->terrain_correct->save_incid_angles)
  {
    if (cfg->general->geocoding) {
      asfPrintStatus("Geocoding incidence angles...");
      update_status("Geocoding incidence angles...");
      sprintf(inFile, "%s/incidence_angles",cfg->general->tmp_dir);
      sprintf(outFile, "%s/incidence_angles_geocoded",cfg->general->tmp_dir);
      check_return(
		   asf_geocode_from_proj_file(
					      cfg->geocoding->projection, cfg->geocoding->force,
					      RESAMPLE_BILINEAR, cfg->geocoding->height,
					      datum, cfg->geocoding->pixel, NULL, inFile, outFile,
					      0.0),
		   "geocoding incidence angles (asf_geocode)\n");
    }
    else {
      // no geocoding ... just prepare the 'outFile' param for export
      sprintf(outFile, "%s/incidence_angles", cfg->general->tmp_dir);
    }
    
    if (is_dir(cfg->general->out_name)) {
      char *tmp = 
	(char *) MALLOC(sizeof(char)*(strlen(cfg->general->tmp_dir)+20));
      sprintf(tmp, "%s%cimport.meta", cfg->general->tmp_dir, DIR_SEPARATOR);
      if (!fileExists(tmp))
	asfPrintError("Can't save incidence angle map!\n");
      meta_parameters *meta = meta_read(tmp);
      free(tmp);
      sprintf(inFile, "%s", outFile);
      if (meta->general->image_data_type == POLARIMETRIC_DECOMPOSITION)
	sprintf(outFile, "%s%c%s_incidence_angle", cfg->general->out_name,
		DIR_SEPARATOR, find_decomposition(meta));
      else
	sprintf(outFile, "%s%cincidence_angle", cfg->general->out_name,
		DIR_SEPARATOR);      
      meta_free(meta);
    }
    else {
      sprintf(inFile, "%s", outFile);
      char *tmp = appendToBasename(cfg->general->out_name, "_incidence_angle");
      strcpy(outFile, tmp);
      free(tmp);
    }
    
    if (cfg->general->export) {
      update_status("Exporting incidence angles...");

      char **band = (char **) MALLOC(sizeof(char *)*2);
      band[0] = (char *) MALLOC(sizeof(char)*25);
      band[1] = NULL;
      strcpy(band[0], "INCIDENCE_ANGLES");
      check_return(asf_export_bands(GEOTIFF, NONE, 0, 0, 0, NULL,
				    inFile, outFile, band, NULL, NULL),
		   "exporting indcidence angles (asf_export)\n");
      FREE(band[0]);
      FREE(band);
    }
    else {
      // no export... just move the geocoded file out of the
      // temporary directory
      renameImgAndMeta(inFile, outFile);
    }
    
    save_intermediate(cfg, "Incidence Angles", outFile);
  }
  
  // Process the layover/shadow mask if requested
  if (cfg->terrain_correct->save_terrcorr_layover_mask) {
    if (cfg->general->geocoding) {
      update_status("Geocoding layover mask...");
      sprintf(inFile, "%s/terrain_correct_mask",cfg->general->tmp_dir);
      sprintf(outFile, "%s/layover_mask_geocoded",cfg->general->tmp_dir);
      check_return(
		   asf_geocode_from_proj_file(
					      cfg->geocoding->projection, cfg->geocoding->force,
					      RESAMPLE_NEAREST_NEIGHBOR, cfg->geocoding->height,
					      datum, cfg->geocoding->pixel, NULL, inFile, outFile,
					      MASK_INVALID_DATA),
		   "geocoding clipped DEM (asf_geocode)\n");
    }
    else {
      // no geocoding ... just prepare the 'outFile' param for export
      sprintf(outFile, "%s/terrain_correct_mask", cfg->general->tmp_dir);
    }
    
    if (is_dir(cfg->general->out_name)) {
      char *tmp = 
	(char *) MALLOC(sizeof(char)*(strlen(cfg->general->tmp_dir)+20));
      sprintf(tmp, "%s%cimport.meta", cfg->general->tmp_dir, DIR_SEPARATOR);
      meta_parameters *meta = meta_read(tmp);
      free(tmp);
      sprintf(inFile, "%s", outFile);
      if (meta->general->image_data_type == POLARIMETRIC_DECOMPOSITION)
	sprintf(outFile, "%s%c%s_layover_mask", cfg->general->out_name,
		DIR_SEPARATOR, find_decomposition(meta));
      else
	sprintf(outFile, "%s%clayover_mask", cfg->general->out_name,
		DIR_SEPARATOR);
      meta_free(meta);
    }
    else {
      sprintf(inFile, "%s", outFile);
      char *tmp = appendToBasename(cfg->general->out_name, "_layover_mask");
      strcpy(outFile, tmp);
      free(tmp);
    }
    
    if (cfg->general->export) {
      update_status("Exporting layover mask...");
      
      meta_parameters *meta = meta_read(inFile);
      
      char *bands[2];
      bands[0] = meta->general->bands;
      bands[1] = NULL;
      
      check_return(
		   asf_export_bands(GEOTIFF, TRUNCATE, 1, 0, 0,
				    "layover_mask.lut", inFile, outFile, bands,
				    NULL, NULL),
		   "exporting layover mask (asf_export)\n");
      meta_free(meta);
    }
    else {
      // no export... just move the geocoded file out of the
      // temporary directory
      renameImgAndMeta(inFile, outFile);
    }
    
    save_intermediate(cfg, "Layover/Shadow Mask", outFile);
  }
  
  if (!cfg->general->intermediates) {
    remove_dir(cfg->general->tmp_dir);
  }
  
  // figure out how long the processing took, tell the user all about it
  ymd_date end_date;
  hms_time end_time;
  get_current_date(&end_date, &end_time);
  double elapsed = date_difference(&end_date, &end_time,
				   &start_date, &start_time);
  
  date_printTime(&end_time,0,':',tmp);
  asfPrintStatus("Done at: %s\n", tmp);
  
  if (elapsed < 60) {
    int sec=(int)elapsed;
    asfPrintStatus("Elapsed time: %d second%s.\n", sec, sec==1?"":"s");
  }
  else if (elapsed < 60*60) {
    int min = elapsed/60;
    int sec = elapsed-min*60;
    asfPrintStatus("Elapsed time: %d minute%s, %d second%s.\n",
		   min, min==1?"":"s", sec, sec==1?"":"s");
  }
  else if (elapsed < 60*60*24) {
    int hour = elapsed/(60*60);
    double rem = elapsed-hour*60*60;
    int min = rem/60;
    int sec = rem-min*60;
    asfPrintStatus("Elapsed time: %d hour%s, %d minute%s, %d second%s.\n",
		   hour, hour==1?"":"s", min, min==1?"":"s",
		   sec, sec==1?"":"s");
  }
  else {
    int day = elapsed/(60*60*24);
    double rem = elapsed-day*60*60*24;
    int hour = rem/(60*60);
    rem -= hour*60*60;
    int min = rem/60;
    int sec = rem-min*60;
    asfPrintStatus("Elapsed time: %d day%s, %d hour%s, %d minute%s, "
		   "%d second%s.\n",
		   day, day==1?"":"s", hour, hour==1?"":"s",
		   min, min==1?"":"s", sec, sec==1?"":"s");
  }
  
  // Don't change this message unless you also change the code in
  // asf_convert_gui/execute.c to look for a different successful
  // completion string.  GUI currently detects successful processing
  // by looking for this message in the log file.... (yeah, I know.)
  asfPrintStatus("\nSuccessful completion!\n\n");

  free_convert_config(cfg);

  FREE(first_pre_export);

  return TRUE;
}

int asf_convert_ext(int createflag, char *configFileName, int saveDEM)
{
  convert_config *cfg;
  char inFile[255], outFile[255];

  // If requested, create a config file and exit (if the file does not exist),
  // otherwise read it
  if ( createflag==TRUE && !fileExists(configFileName) ) {
    init_convert_config(configFileName);
    return(EXIT_SUCCESS);
  }
  // Extend the configuration file if the file already exist
  else if ( createflag==TRUE && fileExists(configFileName) ) {
    cfg = read_convert_config(configFileName);
    check_return(write_convert_config(configFileName, cfg),
                 "Could not update configuration file");
    asfPrintStatus("   Initialized complete configuration file\n\n");
    FCLOSE(fLog);
    fLog = NULL;
    return(EXIT_SUCCESS);
  }
  else if (!fileExists(configFileName)) {
    asfPrintStatus("Couldn't find config file: %s\n", configFileName);
    FCLOSE(fLog);
    fLog = NULL;
    return EXIT_FAILURE;
  }
  else {
    cfg = read_convert_config(configFileName);
  }

  // Using PGM with polarimetry is not allowed -- all are color output
  // (except when using entropy/anisotropy/alpha -- that could be PGM,
  // if each is exported as a separate file)
  if (strncmp(uc(cfg->export->format), "PGM", 3) == 0 &&
      (cfg->general->polarimetry && !cfg->polarimetry->cloude_pottier_nc))
  {
    asfPrintError("Greyscale PGM output is not compatible with the color "
                  "polarimetric options.\n");
  }

  // Check for greyscale PGM output versus selection of a color option
  if (strncmp(uc(cfg->export->format), "PGM", 3) == 0 &&
      (strlen(cfg->export->rgb) > 0 ||
      cfg->export->truecolor        ||
      cfg->export->falsecolor)
     )
  {
    asfPrintWarning("Greyscale PGM output is not compatible with color options:\n"
        "(RGB, True Color or False Color) ...\n"
        "Defaulting to producing separate greyscale PGM files for each\n"
        "available band.\n");
    strcpy(cfg->export->rgb, "");
    cfg->export->truecolor = 0;
    cfg->export->falsecolor = 0;
  }

  // Check for greyscale single-band output versus selection of a color option
  if (strlen(cfg->export->band) > 0 &&
      (strlen(cfg->export->rgb) > 0 ||
          cfg->export->truecolor        ||
          cfg->export->falsecolor)
     )
  {
    asfPrintWarning("Single-band output is not compatible with color options:\n"
        "(RGB, True Color or False Color) ...\n"
        "Defaulting to producing a single greyscale file for the\n"
        "selected band (%s).\n", cfg->export->band);
    strcpy(cfg->export->rgb, "");
    cfg->export->truecolor = 0;
    cfg->export->falsecolor = 0;
  }

  // Check for sampling option in PolsarPro data
  if (strncmp_case(cfg->export->format, "POLSARPRO", 9) == 0 && 
      strlen(cfg->export->byte)>0) {
    asfPrintWarning("No sampling to byte values permitted when exporting data"
		    "to PolsarPro format.\nDefaulting to no sampling.\n");
    strcpy(cfg->export->byte, "");
  }

  // Check for and resolve input/output name clashes
  // - Only applies to importing ASF-internal format files combined
  //   with non-export processing that will not change the file extension,
  //   otherwise name clashes are already resolved by the change in file
  //   extension that occurs.
  // - When checking for same-name, do it in all caps since Windows is
  //   not case-sensitive in the file naming.
  //
  if ((!cfg->general->import ||
      (cfg->general->import  && strncmp(uc(cfg->import->format), "ASF", 3) == 0))  &&
       !cfg->general->export && (strlen(cfg->general->out_name) <= 0               ||
       (strlen(cfg->general->in_name) == strlen(cfg->general->out_name)            &&
       strcmp(uc(cfg->general->in_name), uc(cfg->general->out_name)) == 0))        &&
       strlen(cfg->general->prefix) <= 0 && strlen(cfg->general->suffix) <= 0)
  {
    if (cfg->general->terrain_correct || cfg->general->geocoding) {
      sprintf(cfg->general->suffix, "%s%s",
              cfg->general->suffix,
              "_out");
    }
  }

  // Check for unsupported product level (Just Palsar L0 for now...)
  if (cfg->general->import) {
    char *basename = MALLOC(sizeof(char)*(strlen(cfg->general->in_name)+10));
    char **dataName = NULL, **metaName = NULL;
    int nBands, trailer;
    ceos_file_pairs_t s = get_ceos_names(cfg->general->in_name, basename,
                                         &dataName, &metaName, &nBands, &trailer);
    // Check for raw Palsar
    ceos_description *ceos = NULL;
    if (s != NO_CEOS_FILE_PAIR) {
        ceos = get_ceos_description(cfg->general->in_name, REPORT_LEVEL_NONE);
    }

    if (s != NO_CEOS_FILE_PAIR &&
        (ceos && ceos->sensor == PALSAR && ceos->product == RAW))
    {
        free_ceos_names(dataName, metaName);
        FREE(basename);
        asfPrintError("Palsar Level 0 unsupported.\n");
    }
    free_ceos_names(dataName, metaName);
    FREE(basename);
    FREE(ceos);
  }

  // Mosaicking
  if (cfg->general->mosaic) {
    // Requires configuration file with batch processing switched on.
    // GUI needs to generate temporary defaults values file and data list.
    // Input is batch configuration file that contains those two parameters.
    FILE *fList;
    char *mosaic_dir=NULL, line[255], tmpList[255];
    int batch = TRUE;

    // Check whether batch processing is required
    if (strncmp(uc(cfg->import->format), "ASF", 3) == 0)
      cfg->general->import = 0;
    if (!cfg->general->import && !cfg->general->terrain_correct)
      batch = FALSE;
    else
      mosaic_dir = MALLOC(sizeof(char)*255);

    if (batch) {
      // Create a temporary directory to collect intermediate processing
      // results
      char baseName[255], baseDir[255];
      char *p = findExt(configFileName);
      if (p)
        *p = '\0';
      split_dir_and_file(configFileName, baseDir, baseName);
      strcpy(mosaic_dir,"");
      create_and_set_tmp_dir(configFileName, configFileName, mosaic_dir);

      split_dir_and_file(configFileName, baseDir, baseName);

      // Generate temporary configuration files
      char tmp_dir[255];;
      convert_config *tmp_cfg=NULL;
      char tmpCfgName[255];
      int n_ok = 0, n_bad = 0;
      FILE *fBatch = FOPEN(cfg->general->batchFile, "r");
      sprintf(tmpList, "%s/data.lst", mosaic_dir);
      fList = FOPEN(tmpList, "w");

      strcpy(tmp_dir, cfg->general->tmp_dir);
      while (fgets(line, 255, fBatch) != NULL) {
        char batchItem[255], batchItemFile[255], batchItemDir[255];
        sscanf(line, "%s", batchItem);

        // Create temporary directory for processing
        char *p = findExt(batchItem);
        if (p) *p = '\0';
        split_dir_and_file(batchItem, batchItemDir, batchItemFile);
        create_and_set_tmp_dir(batchItem, batchItem, tmp_dir);

        // Generate temporary defaults values file
        char tmpDefaults[255];
        sprintf(tmpDefaults, "%s/tmp.defaults", tmp_dir);
        FILE *fDef = FOPEN(tmpDefaults, "w");
        fprintf(fDef, "import = %d\n", cfg->general->import);
        fprintf(fDef, "polarimetry = %d\n", cfg->general->polarimetry);
        fprintf(fDef, "terrain correction = %d\n",
                cfg->general->terrain_correct);
        fprintf(fDef, "geocoding = 0\n");
        fprintf(fDef, "export = 0\n");
        fprintf(fDef, "intermediates = %d\n", cfg->general->intermediates);
        fprintf(fDef, "quiet = 1\n");
        fprintf(fDef, "short configuration file = 1\n");
        if (cfg->general->import) {
          fprintf(fDef, "input format = %s\n", cfg->import->format);
          fprintf(fDef, "radiometry = %s\n", cfg->import->radiometry);
          fprintf(fDef, "output db = %d\n", cfg->import->output_db);
          fprintf(fDef, "multilook SLC = %d\n", cfg->import->multilook_slc);
        }
        if (cfg->general->polarimetry) {
          fprintf(fDef, "pauli = %d\n", cfg->polarimetry->pauli);
          fprintf(fDef, "sinclair = %d\n", cfg->polarimetry->sinclair);
          fprintf(fDef, "freeman durden = %d\n",
                  cfg->polarimetry->freeman_durden);
          fprintf(fDef, "cloude pottier = %d\n",
                  cfg->polarimetry->cloude_pottier);
          fprintf(fDef, "faraday correction = %d\n", cfg->polarimetry->farcorr);
        }
        if (cfg->general->terrain_correct) {
          fprintf(fDef, "pixel spacing = %f\n", cfg->terrain_correct->pixel);
          fprintf(fDef, "digital elevation model = %s\n",
                  cfg->terrain_correct->dem);
          fprintf(fDef, "mask = %s\n", cfg->terrain_correct->mask);
          fprintf(fDef, "smooth dem holes =1\n");
          fprintf(fDef, "do radiometric = %d\n",
                  cfg->terrain_correct->do_radiometric);
          fprintf(fDef, "interpolate = 1\n");
        }
        FCLOSE(fDef);

        // Create temporary configuration file
        sprintf(tmpCfgName, "%s/%s.cfg", tmp_dir, batchItemFile);
        FILE *fConfig = FOPEN(tmpCfgName, "w");
        fprintf(fConfig, "asf_convert temporary configuration file\n\n");
        fprintf(fConfig, "[General]\n");
        fprintf(fConfig, "default values = %s\n", tmpDefaults);
        fprintf(fConfig, "input file = %s\n", batchItem);
        fprintf(fConfig, "output file = %s%s/%s\n", batchItemDir, mosaic_dir,
                batchItemFile);
        fprintf(fConfig, "tmp dir = %s\n", tmp_dir);
        FCLOSE(fConfig);

        // Extend the temporary configuration file
        tmp_cfg = read_convert_config(tmpCfgName);
        check_return(write_convert_config(tmpCfgName, tmp_cfg),
                     "Could not update configuration file");
        free_convert_config(tmp_cfg);

        // This is really quite a kludge-- we used to call the library
        // function here, now we shell out and run the tool directly, sort
        // of a step backwards, it seems.  Unfortunately, in order to keep
        // processing the batch even if an error occurs, we're stuck with
        // this method.  (Otherwise, we'd have to teach asfPrintError to
        // get us back here, to continue the loop.)
        asfPrintStatus("\nProcessing %s ...\n", batchItem);
        char cmd[1024];
        if (logflag) {
          sprintf(cmd, "%sasf_convert%s -log %s %s",
              get_argv0(), bin_postfix(), logFile, tmpCfgName);
        }
        else {
          sprintf(cmd, "%sasf_convert%s %s", get_argv0(), bin_postfix(),
                  tmpCfgName);
        }
        int ret = asfSystem(cmd);

        if (ret != 0) {
          asfPrintStatus("%s: failed\n", batchItem);
          ++n_bad;
        }
        else {
          asfPrintStatus("%s: ok\n", batchItem);
          fprintf(fList, "%s/%s.img\n", mosaic_dir, batchItem);
          ++n_ok;
        }

        strcpy(tmp_dir, cfg->general->tmp_dir);
      }
      FCLOSE(fBatch);
      FCLOSE(fList);
    }

    // Read file names to pass to mosaicking
    int ii, nFiles = 0;
    if (mosaic_dir)
      fList = FOPEN(tmpList, "r");
    else
      fList = FOPEN(cfg->general->batchFile, "r");
    while (fgets(line, 255, fList) != NULL)
      nFiles++;
    FCLOSE(fList);
    char *in_base_names[nFiles+1];
    for (ii=0; ii<nFiles; ii++)
      in_base_names[ii] = MALLOC(sizeof(char)*255);
    in_base_names[nFiles] = NULL;
    ii = 0;
    if (mosaic_dir)
      fList = FOPEN(tmpList, "r");
    else
      fList = FOPEN(cfg->general->batchFile, "r");
    while (fgets(line, 255, fList) != NULL) {
      line[strlen(line)-1] = '\0';
      strcpy(in_base_names[ii], line);
      ++ii;
    }
    FCLOSE(fList);

    // Mosaic imported (and terrain corrected) images
    double lat_min = -999, lon_min = -999;
    double lat_max = 999, lon_max = 999;
    project_parameters_t pp;
    projection_type_t proj_type;
    datum_type_t datum;
    spheroid_type_t spheroid;
    resample_method_t resample_method;
    int multiband = 1;
    int band_num = 0;
    char *err=NULL;

    if (strcmp(cfg->geocoding->resampling, "NEAREST NEIGHBOR") == 0)
      resample_method = RESAMPLE_NEAREST_NEIGHBOR;
    else if (strcmp(cfg->geocoding->resampling, "BILINEAR") == 0)
      resample_method = RESAMPLE_BILINEAR;
    else if(strcmp(cfg->geocoding->resampling, "BICUBIC") == 0)
      resample_method = RESAMPLE_BICUBIC;

    if (mosaic_dir)
      sprintf(outFile, "%s/%s", mosaic_dir, cfg->general->out_name);
    else
      sprintf(outFile, "%s", cfg->general->out_name);
    if (!parse_proj_args_file(cfg->geocoding->projection, &pp, &proj_type, &datum, &spheroid, 
			      &err)) {
      asfPrintError("%s",err);
    }
    update_status("Mosaicking...");
    asf_mosaic(&pp, proj_type, cfg->geocoding->force, resample_method,
	   cfg->geocoding->height, datum, spheroid, cfg->geocoding->pixel,
           multiband, band_num, in_base_names, outFile,
           cfg->geocoding->background,
           lat_min, lat_max, lon_min, lon_max, cfg->mosaic->overlap, FALSE);

    // Export mosaic
    if (cfg->general->export) {
      sprintf(inFile, "%s", outFile);
      sprintf(outFile, "%s", cfg->general->out_name);
      update_status("Exporting...");
      do_export(cfg, inFile, outFile);
    }
  }

  // Batch mode processing
  else if (strlen(cfg->general->batchFile) > 0) {
    convert_config *tmp_cfg=NULL;
    char tmp_dir[255];
    char tmpCfgName[255];
    char line[255];
    int n_ok = 0, n_bad = 0;
    FILE *fBatch = FOPEN(cfg->general->batchFile, "r");

    strcpy(tmp_dir, cfg->general->tmp_dir);
    while (fgets(line, 255, fBatch) != NULL) {
      char batchItem[255], batchItemFile[255], batchItemDir[255];
      sscanf(line, "%s", batchItem);

      // strip off known extensions
      char *p = findExt(batchItem);
      if (p) *p = '\0';

      split_dir_and_file(batchItem, batchItemDir, batchItemFile);

      char *tmpDir = MALLOC(sizeof(char)*(strlen(cfg->general->defaults)+1));
      char *tmpFile = MALLOC(sizeof(char)*(strlen(cfg->general->defaults)+1));
      split_dir_and_file(cfg->general->defaults, tmpDir, tmpFile);
      char cwd[10000];
      char *buf = getcwd(cwd,10000);
      if (!buf) asfPrintError("Error determining cwd: %s\n", strerror(errno));
      int defaultsLen;
      if (strlen(cfg->general->defaults) > strlen(cwd)+strlen(tmpFile)) {
        defaultsLen = strlen(cfg->general->defaults);
      } else {
        defaultsLen = strlen(cwd)+strlen(tmpFile);
      }
      char *defaults = MALLOC(sizeof(char)*defaultsLen+2);
      if (0==strlen(tmpDir)) {
        sprintf(defaults,"%s/%s",cwd,tmpFile);
      } else {
        strcpy(defaults,cfg->general->defaults);
      }
      FREE(tmpDir);
      FREE(tmpFile);

      // Create temporary configuration file
      create_and_set_tmp_dir(batchItem, cfg->general->default_out_dir, tmp_dir);
      sprintf(tmpCfgName, "%s/%s.cfg", tmp_dir, batchItemFile);


      FILE *fConfig = FOPEN(tmpCfgName, "w");
      fprintf(fConfig, "asf_mapready temporary configuration file\n\n");
      fprintf(fConfig, "[General]\n");
      fprintf(fConfig, "default values = %s\n", defaults);
      fprintf(fConfig, "input file = %s\n", batchItem);
      if (strlen(cfg->general->default_out_dir) == 0)
        fprintf(fConfig, "output file = %s%s%s\n",
                cfg->general->prefix, batchItemFile, cfg->general->suffix);
      else
        fprintf(fConfig, "output file = %s%c%s%s%s\n",
                cfg->general->default_out_dir, DIR_SEPARATOR,
                cfg->general->prefix, batchItemFile, cfg->general->suffix);
      fprintf(fConfig, "tmp dir = %s\n", tmp_dir);
      FCLOSE(fConfig);
      FREE(defaults);

      // Extend the temporary configuration file
      tmp_cfg = read_convert_config(tmpCfgName);
      check_return(write_convert_config(tmpCfgName, tmp_cfg),
                   "Could not update configuration file");
      free_convert_config(tmp_cfg);

      // Run asf_mapready for temporary configuration file

      // This is really quite a kludge-- we used to call the library
      // function here, now we shell out and run the tool directly, sort
      // of a step backwards, it seems.  Unfortunately, in order to keep
      // processing the batch even if an error occurs, we're stuck with
      // this method.  (Otherwise, we'd have to teach asfPrintError to
      // get us back here, to continue the loop.)
      asfPrintStatus("\nProcessing %s ...\n", batchItem);
      char cmd[1024];
      if (logflag) {
          sprintf(cmd, "%sasf_mapready%s -log %s %s",
                get_argv0(), bin_postfix(), logFile, tmpCfgName);
      }
      else {
          sprintf(cmd, "%sasf_mapready%s %s",
                get_argv0(), bin_postfix(), tmpCfgName);
      }
      int ret = asfSystem(cmd);

      if (ret != 0) {
          asfPrintStatus("%s: failed\n", batchItem);
          ++n_bad;
      } else {
          asfPrintStatus("%s: ok\n", batchItem);
          ++n_ok;
      }

      strcpy(tmp_dir, cfg->general->tmp_dir);
    }
    FCLOSE(fBatch);

    asfPrintStatus("\n\nBatch Complete.\n");
    asfPrintStatus("Successfully processed %d/%d file%s.\n", n_ok,
        n_ok + n_bad, n_ok + n_bad == 1 ? "" : "s");

    if (n_bad > 0)
        asfPrintStatus("  *** %d file%s failed. ***\n", n_bad,
            n_bad==1 ? "" : "s");
  }
  // Regular processing
  else {

    // Check for project settings
    if (cfg->general->project && cfg->project) {
      asfPrintStatus("Project: %s\n\n", cfg->project->long_name);
      cfg->general->import = 1;

      int count;
      char tmpConfigFile[512];
      radiometry_t radiometry = r_AMP;
      for (count=0; count<cfg->files->file_count; count++) {
	asfPrintStatus("Processing %s [%d/%d] ...\n\n", 
		       cfg->files->name[count], count+1, 
		       cfg->files->file_count);
	if (cfg->general->calibration) {
	  if (strncmp_case(cfg->calibrate->radiometry, "SIGMA_DB", 8) == 0) {
	    strcpy(cfg->import->radiometry, "SIGMA_IMAGE");
	    cfg->import->output_db = TRUE;
	    radiometry = r_SIGMA_DB;
	  }
	  else if (strncmp_case(cfg->calibrate->radiometry, "GAMMA_DB", 8) == 0) {
	    strcpy(cfg->import->radiometry, "GAMMA_IMAGE");
	    cfg->import->output_db = TRUE;
	    radiometry = r_GAMMA_DB;
	  }
	  else if (strncmp_case(cfg->calibrate->radiometry, "BETA_DB", 7) == 0) {
	    strcpy(cfg->import->radiometry, "BETA_IMAGE");
	    cfg->import->output_db = TRUE;
	    radiometry = r_BETA_DB;
	  }
	  else if (strncmp_case(cfg->calibrate->radiometry, "SIGMA", 5) == 0) {
	    strcpy(cfg->import->radiometry, "SIGMA_IMAGE");
	    cfg->import->output_db = FALSE;
	    radiometry = r_SIGMA;
	  }
	  else if (strncmp_case(cfg->calibrate->radiometry, "GAMMA", 5) == 0) {
	    strcpy(cfg->import->radiometry, "GAMMA_IMAGE");
	    cfg->import->output_db = FALSE;
	    radiometry = r_GAMMA;
	  }
	  else if (strncmp_case(cfg->calibrate->radiometry, "BETA", 4) == 0) {
	    strcpy(cfg->import->radiometry, "BETA_IMAGE");
	    cfg->import->output_db = FALSE;
	    radiometry = r_BETA;
	  }
	  else if (strncmp_case(cfg->calibrate->radiometry, "AMPLITUDE", 9) ==0) {
	    strcpy(cfg->import->radiometry, "AMPLITUDE_IMAGE");
	    radiometry = r_AMP;
	  }
	}
	sprintf(cfg->general->in_name, "%s", cfg->files->name[count]);
	sprintf(cfg->general->out_name, "%s",
		get_out_name(cfg->general->in_name, cfg->project->short_name,
			     radiometry, cfg->project->naming_scheme));
	sprintf(tmpConfigFile, "%s.cfg", cfg->general->out_name);
	write_convert_config(tmpConfigFile, cfg);
	asf_convert_file(tmpConfigFile, saveDEM);
	remove_file(tmpConfigFile);
      }
    }
    else
      asf_convert_file(configFileName, saveDEM);
  }

  update_status("Done");

  // If this function (asf_convert_ext) is called recursively, we SHOULD NOT
  // clean up the config files, clear the status file, or reset intermediates
  // just yet. We'll do this all later. 
  if ( createflag == TRUE ) { 
	clear_status_file();
  	reset_intermediates();
  }
  free_convert_config(cfg);
  return(EXIT_SUCCESS);
}

int asf_convert(int createflag, char *configFileName)
{
    return asf_convert_ext(createflag, configFileName, FALSE);
}

static scale_t get_scale(convert_config *cfg)
{
  scale_t scale = SIGMA;

  // Byte scaling
  if (strncmp_case(cfg->export->byte, "TRUNCATE", 8) == 0) {
    scale = TRUNCATE;
  } else if (strncmp_case(cfg->export->byte, "MINMAX_MEDIAN", 13) == 0) {
    scale = MINMAX_MEDIAN;
  } else if (strncmp_case(cfg->export->byte, "MINMAX", 6) == 0) {
    scale = MINMAX;
  } else if (strncmp_case(cfg->export->byte, "SIGMA", 5) == 0) {
    scale = SIGMA;
  } else if (strncmp_case(cfg->export->byte, "HISTOGRAM_EQUALIZE", 18) == 0) {
    scale = HISTOGRAM_EQUALIZE;
  } else if (strncmp_case(cfg->export->byte, "NONE", 4) == 0) {
    scale = NONE;
  }

  return scale;
}

static void do_export(convert_config *cfg, char *inFile, char *outFile)
{
  int true_color = cfg->export->truecolor;
  int false_color = cfg->export->falsecolor;
  output_format_t format = get_format(cfg);
  scale_t scale = get_scale(cfg);
  int i, num_outputs, is_polsarpro = 0;
  char **output_names;

  // Move the .meta file out of temporary status
  // Don't need to do this if we skipped import, we'd already have .meta
  int is_airsar = strncmp_case(cfg->import->format, "AIRSAR", 6) == 0;
  int is_uavsar = strncmp_case(cfg->import->format, "UAVSAR", 6) == 0;
  if (cfg->general->import && !is_airsar && !is_uavsar)
    copy_meta(cfg, inFile, outFile);
  int is_insar = isInSAR(inFile);

  meta_parameters *meta = meta_read(inFile);
  if (meta->general->image_data_type == RGB_STACK)
    strcpy(cfg->export->rgb, meta->general->bands);
  char lut_file[2048] = "";
  if (meta->general->image_data_type >= POLARIMETRIC_SEGMENTATION &&
      meta->general->image_data_type <= POLARIMETRIC_T4_MATRIX)
    is_polsarpro = TRUE;
  if (cfg->export && cfg->export->lut && strlen(cfg->export->lut) > 0) {
    strcpy(lut_file, cfg->export->lut);
  }
  else if (is_polsarpro &&
           cfg->import  &&
           cfg->import->polsarpro_colormap &&
           strlen(cfg->import->polsarpro_colormap) > 0)
  {
    strcpy(lut_file, cfg->import->polsarpro_colormap);
  }
  else if (is_polsarpro && meta->colormap &&
           meta->colormap->look_up_table && strlen(meta->colormap->look_up_table) > 0)
  {
    strcpy(lut_file, meta->colormap->look_up_table);
  }
  if (strlen(cfg->export->lut) > 0 ||
      ((is_polsarpro || is_insar) && strlen(lut_file) > 0))
  {
    if (!is_polsarpro && !is_insar && meta->general->band_count > 1) {
      asfPrintWarning("RGB Look-up-table not allowed for multi-band images."
                      " Ignored.\n");
      strcpy(lut_file,"");
    }
    else {
      if (strlen(cfg->export->rgb) > 0) {
        asfPrintWarning("RGB Banding option not allowed with RGB look up table."
                        " Ignored.\n");
      }
      if (!is_polsarpro && !is_insar && scale != TRUNCATE) {
        asfPrintWarning("Scale option %s not allowed with RGB look-up-table."
                        " Using TRUNCATE.\n",
                        cfg->export->byte);
      }
      asfPrintStatus("Exporting using look-up-table: %s\n", lut_file);

      char **bands = extract_band_names(meta->general->bands, meta->general->band_count);
//      bands[0] = meta->general->bands;
//      bands[1] = NULL;

      check_return(
	asf_export_bands(format, (is_polsarpro || is_insar) ? scale : TRUNCATE,
                         is_polsarpro ? FALSE : TRUE, 0, 0,
                         lut_file, inFile, outFile, bands, &num_outputs,
                         &output_names),
        "exporting data file (asf_export), using rgb look up table.\n");
      int i;
      for (i=0; i<meta->general->band_count; i++) {
          FREE(bands[i]);
      }
      FREE(bands);
    }
  }

  if (strlen(lut_file)==0) {
    // non look-up-table case
    if (strlen(cfg->export->rgb) > 0) {
      // user has requested banding
      char *red,  *green, *blue;
      if (split3(cfg->export->rgb, &red, &green, &blue, ',')) {
        int num_found;
        char **bands = find_bands(inFile, TRUE, red, green, blue,
                                  &num_found);
        if (num_found > 0) {
          asfPrintStatus("\nExporting RGB bands into single color file:\n"
                         "Red band  : %s\n"
                         "Green band: %s\n"
                         "Blue band : %s\n\n",
                         red, green, blue);
          check_return(asf_export_bands(format, scale, TRUE, 0, 0, NULL,
                                        inFile, outFile, bands, &num_outputs,
                                        &output_names),
                       "export data file (asf_export), banded.\n");
          for (i=0; i<num_found; i++)
            FREE(bands[i]);
          FREE(bands);
        } else {
          asfPrintError("The requested bands (%s) "
                        "were not available in the file: %s\n",
                        cfg->export->rgb, inFile);
          strcpy(cfg->export->rgb, "");
        }
        FREE(red); FREE(green); FREE(blue);
      } else {
        asfPrintError("Invalid listing of RGB bands: %s\n",
                      cfg->export->rgb);
        strcpy(cfg->export->rgb, "");
      }
    }

    if (strlen(cfg->export->rgb) == 0)
    {
      if (meta->optical && (true_color || false_color)) {
        // Multi-band optical data, exporting as true or false color single file
        asfPrintStatus("\nExporting %s file...\n\n\n",
                       true_color ? "True Color" : false_color ? "False Color" : "Unknown");
        if (true_color &&
            (strstr(meta->general->bands, "01") == NULL ||
             strstr(meta->general->bands, "02") == NULL ||
             strstr(meta->general->bands, "03") == NULL)
          )
        {
          asfPrintError("Imported file does not contain required color bands\n"
                        "necessary for true color output (03, 02, 01)\n");
        }
        if (false_color &&
            (strstr(meta->general->bands, "02") == NULL ||
             strstr(meta->general->bands, "03") == NULL ||
             strstr(meta->general->bands, "04") == NULL)
          )
        {
          asfPrintError("Imported file does not contain required color bands\n"
                        "necessary for false color output (04, 03, 02)\n");
        }
        if (scale != SIGMA) {
          asfPrintWarning("A byte conversion other than SIGMA was specified.  Since\n"
                          "True Color or False Color output was selected, the byte conversion\n"
                          "will be overridden with SIGMA, a 2-sigma contrast expansion.\n");
        }
        char **bands = extract_band_names(meta->general->bands, meta->general->band_count);
        if (meta->general->band_count >= 4 && bands != NULL) {
          // The imported file IS a multiband file with enough bands,
          // but the extract bands need to be ordered correctly
          if (true_color) {
            strcpy(bands[0], "03");
            strcpy(bands[1], "02");
            strcpy(bands[2], "01");
            strcpy(bands[3], "");
          }
          else {
            strcpy(bands[0], "04");
            strcpy(bands[1], "03");
            strcpy(bands[2], "02");
            strcpy(bands[3], "");
          }
          check_return(asf_export_bands(format, SIGMA, TRUE,
                                        true_color, false_color, NULL,
                                        inFile, outFile, bands,
                                        &num_outputs, &output_names),
                       "exporting data file (asf_export), color banded.\n");
          for (i=0; i<meta->general->band_count; ++i)
            FREE (bands[i]);
          FREE(bands);
        }
        else {
          asfPrintError("Cannot determine band names from imported metadata file:\n  %s\n",
                        inFile);
        }
      }
      else {
        if (meta->optical && scale != TRUNCATE && scale != NONE) {
          asfPrintWarning("Optical data should not use the specified sample "
            "mapping %s.\nChanging to TRUNCATE.\n", uc(cfg->export->byte));
          scale = TRUNCATE;
        }
        if (meta->general->band_count != 1 && strlen(cfg->export->band) == 0) {
          // multi-band, exporting as separate greyscale files
	  if ((meta->general->image_data_type >= POLARIMETRIC_C2_MATRIX &&
	       meta->general->image_data_type <= POLARIMETRIC_STOKES_MATRIX) ||
	      meta->general->image_data_type == POLARIMETRIC_DECOMPOSITION)
	    asfPrintStatus("\nExporting %d bands as separate greyscale files "
			   "...\n", meta->general->band_count-1);
	  else
	    asfPrintStatus("\nExporting %d bands as separate greyscale files "
			   "...\n", meta->general->band_count);
	  char **bands = extract_band_names(meta->general->bands, 
					    meta->general->band_count);
	  if (strcmp_case(cfg->import->format, "POLSARPRO") == 0 &&
	      format == GEOTIFF)
	    check_return(asf_export_bands(format, scale, FALSE,
					  0, 0, NULL,
					  inFile, outFile, bands,
					  &num_outputs, &output_names),
			 "exporting data file (asf_export), greyscale bands.\n");
	  else
	    check_return(asf_export_bands(format, scale, FALSE,
					  0, 0, NULL,
					  inFile, outFile, NULL,
					  &num_outputs, &output_names),
			 "exporting data file (asf_export), greyscale bands.\n");
          FREE(*bands);
          FREE(bands);

	  // Exporting polarimetric decompositions into an RGB file as well
	  if (meta->general->image_data_type == POLARIMETRIC_DECOMPOSITION &&
	      format != POLSARPRO_HDR) {
            int i, found = FALSE;
	    char decomposition[25], configFile[512], decompFile[512];
	    char *red, *green, *blue;
	    sprintf(decomposition, "%s", find_decomposition(meta));
	    sprintf(configFile, "%s%cpolarimetry.cfg", 
		    get_asf_share_dir(), DIR_SEPARATOR);
	    found = read_polarimetry_config(configFile, decomposition, 
					    &red, &green, &blue);
	    // Only write RGB version if you find information about it in the
	    // configuration file.
	    if (found) {
	      char **bands = MALLOC(sizeof(char*)*meta->general->band_count);
	      for (i=0; i<meta->general->band_count; i++)
		bands[i] = MALLOC(sizeof(char)*32);
	      strcpy(bands[0], red);
	      strcpy(bands[1], green);
	      strcpy(bands[2], blue);
	      if (meta->general->band_count > 3)
		strcpy(bands[3], "");
	      if (meta->general->band_count > 4)
		strcpy(bands[4], "");
	      asfPrintStatus("\nExporting decomposition into single color file:\n"
			     "Red band  : %s\n"
			     "Green band: %s\n"
			     "Blue band : %s\n\n",
			     red, green, blue);
	      sprintf(decompFile, "%s%c%s_RGB", 
		      outFile, DIR_SEPARATOR, decomposition);
	      meta_parameters *md = meta_read(inFile);
	      md->general->image_data_type = RGB_STACK;
	      meta_write(md, inFile);
	      meta_free(md);
	      check_return(asf_export_bands(format, scale, TRUE, 0, 0, NULL,
					    inFile, decompFile, bands, 
					    &num_outputs, &output_names),
			   "export data file (asf_export), polarimetric decomposition.\n");
	      md = meta_read(inFile);
	      md->general->image_data_type = POLARIMETRIC_DECOMPOSITION;
	      meta_write(md, inFile);
	      meta_free(md);
	      
	      for (i=0; i<meta->general->band_count; i++)
		FREE(bands[i]);
	      FREE(bands);
	    }
	  }
        }
        else if (meta->general->band_count != 1 && strlen(cfg->export->band) > 0) {
          // multi-band, exporting single band in one greyscale file
          asfPrintStatus("\nExporting band \"%s\" in a single greyscale file...\n",
                         cfg->export->band);
          int num_bands_found = 0;
          char **band_names = find_single_band(inFile, cfg->export->band,
                                               &num_bands_found);
          if (num_bands_found != 1) {
            asfPrintError("Selected band for export not found.\n");
          }
          check_return(asf_export_bands(format, scale, FALSE,
                                        0, 0, NULL,
                                        inFile, outFile, band_names,
                                        &num_outputs, &output_names),
                       "exporting data file (asf_export), single selected greyscale band.\n");
          if (*band_names) FREE(*band_names);
          if (band_names) FREE(band_names);
        }
        else {
          // single band
          check_return(asf_export_bands(format, scale, 0, 0, 0, NULL,
                                        inFile, outFile, NULL,
                                        &num_outputs, &output_names),
                       "exporting data file (asf_export), single band.\n");
        }
      }
    }
  }
  meta_free(meta);

  for (i=0; i<num_outputs; ++i) {
    save_intermediate(cfg, "Output", output_names[i]);
    FREE(output_names[i]);
  }
  FREE(output_names);
}

