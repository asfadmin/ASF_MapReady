#include "asf.h"
#include "asf_meta.h"
#include <math.h>

static char *get_spec_string_param(char *spec_file, char *parameter)
{
  char *filename = NULL, line[1024], param[100], type[25], *p;
  char *strValue;
  int found = FALSE;
  
  // First try to find the spec file locally
  FILE *fp = fopen(spec_file, "r");
  if (!fp) {
    filename = appendExt(spec_file, ".specs");
    fp = fopen(filename, "r");
  }

  // Second try the spec file from share directory
  if (!fp) {
    FREE(filename);
    filename = (char *) MALLOC(sizeof(char)*1024);
    sprintf(filename, "%s/lib_test/%s", get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp) {
    sprintf(filename, "%s/lib_test/%s.specs", 
	    get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp)
    asfPrintError("Could not find spec file (%s)\n", spec_file);
  if (filename)
    FREE(filename);

  while(fgets(line, 1024, fp)) {
    // skip comments
    if (strstr(line, "#"))
      continue;
    else
      line[strlen(line)-1] = '\0';

    // parameter out of the metadata structure
    p = strchr(line, ',');
    *p = '\0';
    sprintf(param, "%s", line);

    if (strcmp_case(param, parameter) == 0) {
      // data type
      sprintf(line, "%s", p+1);
      p = strchr(line, ',');
      if (p)
	*p = '\0';
      sprintf(type, "%s", line);
      
      if (strncmp(type, "char", 4) == 0) { 
	found = TRUE;
	// value
	if (p) {
	  sprintf(line, "%s", p+1);
	  if (line[0] == '\'' && line[strlen(line)-1] == '\'') {
	    strValue = (char *) MALLOC(sizeof(char)*255);
	    sprintf(strValue, "%s", line+1);
	    strValue[strlen(strValue)-1] = '\0';
	  }
	}
      }
    }
  }
  if (found)
    return strValue;
  else
    return NULL;
}

static int get_spec_int_param(char *spec_file, char *parameter)
{
  char *filename = NULL, line[1024], param[100], type[25], *p;
  int nValue = MAGIC_UNSET_INT;
  
  // First try to find the spec file locally
  FILE *fp = fopen(spec_file, "r");
  if (!fp) {
    filename = appendExt(spec_file, ".specs");
    fp = fopen(filename, "r");
  }

  // Second try the spec file from share directory
  if (!fp) {
    FREE(filename);
    filename = (char *) MALLOC(sizeof(char)*1024);
    sprintf(filename, "%s/lib_test/%s", get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp) {
    sprintf(filename, "%s/lib_test/%s.specs", 
	    get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp)
    asfPrintError("Could not find spec file (%s)\n", spec_file);
  if (filename)
    FREE(filename);

  while(fgets(line, 1024, fp)) {
    // skip comments
    if (strstr(line, "#"))
      continue;
    else
      line[strlen(line)-1] = '\0';

    // parameter out of the metadata structure
    p = strchr(line, ',');
    *p = '\0';
    sprintf(param, "%s", line);

    if (strcmp_case(param, parameter) == 0) {
      // data type
      sprintf(line, "%s", p+1);
      p = strchr(line, ',');
      if (p)
	*p = '\0';
      sprintf(type, "%s", line);
      
      if (strncmp(type, "int", 4) == 0) { 
	// value
	if (p) {
	  sprintf(line, "%s", p+1);
	  sscanf(line, "%d", &nValue);
	}
      }
    }
  }
  return nValue;
}

static float get_spec_float_param(char *spec_file, char *parameter)
{
  char *filename = NULL, line[1024], param[100], type[25], *p;
  float fValue = MAGIC_UNSET_DOUBLE;
  
  // First try to find the spec file locally
  FILE *fp = fopen(spec_file, "r");
  if (!fp) {
    filename = appendExt(spec_file, ".specs");
    fp = fopen(filename, "r");
  }

  // Second try the spec file from share directory
  if (!fp) {
    FREE(filename);
    filename = (char *) MALLOC(sizeof(char)*1024);
    sprintf(filename, "%s/lib_test/%s", get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp) {
    sprintf(filename, "%s/lib_test/%s.specs", 
	    get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp)
    asfPrintError("Could not find spec file (%s)\n", spec_file);
  if (filename)
    FREE(filename);

  while(fgets(line, 1024, fp)) {
    // skip comments
    if (strstr(line, "#"))
      continue;
    else
      line[strlen(line)-1] = '\0';

    // parameter out of the metadata structure
    p = strchr(line, ',');
    *p = '\0';
    sprintf(param, "%s", line);

    if (strcmp_case(param, parameter) == 0) {
      // data type
      sprintf(line, "%s", p+1);
      p = strchr(line, ',');
      if (p)
	*p = '\0';
      sprintf(type, "%s", line);
      
      if (strncmp(type, "float", 4) == 0) { 
	// value
	if (p) {
	  sprintf(line, "%s", p+1);
	  sscanf(line, "%f", &fValue);
	}
      }
    }
  }
  return fValue;
}

static double get_spec_double_param(char *spec_file, char *parameter)
{
  char *filename = NULL, line[1024], param[100], type[25], *p;
  double lfValue = MAGIC_UNSET_DOUBLE;
  
  // First try to find the spec file locally
  FILE *fp = fopen(spec_file, "r");
  if (!fp) {
    filename = appendExt(spec_file, ".specs");
    fp = fopen(filename, "r");
  }

  // Second try the spec file from share directory
  if (!fp) {
    FREE(filename);
    filename = (char *) MALLOC(sizeof(char)*1024);
    sprintf(filename, "%s/lib_test/%s", get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp) {
    sprintf(filename, "%s/lib_test/%s.specs", 
	    get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp)
    asfPrintError("Could not find spec file (%s)\n", spec_file);
  if (filename)
    FREE(filename);

  while(fgets(line, 1024, fp)) {
    // skip comments
    if (strstr(line, "#"))
      continue;
    else
      line[strlen(line)-1] = '\0';

    // parameter out of the metadata structure
    p = strchr(line, ',');
    *p = '\0';
    sprintf(param, "%s", line);

    if (strcmp_case(param, parameter) == 0) {
      // data type
      sprintf(line, "%s", p+1);
      p = strchr(line, ',');
      if (p)
	*p = '\0';
      sprintf(type, "%s", line);
      
      if (strncmp(type, "double", 4) == 0) { 
	// value
	if (p) {
	  sprintf(line, "%s", p+1);
	  sscanf(line, "%lf", &lfValue);
	}
      }
    }
  }
  return lfValue;
}

int lib_test_ext(char *lib_func, char *path, char *spec_file, 
		 report_level_t level)
{
  int passed = FALSE;
  char *specs = 
    (char *) MALLOC(sizeof(char)*(strlen(path)+strlen(spec_file)+5));
  sprintf(specs, "%s%c%s", path, DIR_SEPARATOR, spec_file);

  if (strcmp_case(lib_func, "get_cal_dn") == 0) {
    char *meta_file = get_spec_string_param(specs, "meta_file");
    float incid = get_spec_float_param(specs, "incidence_angle");
    int sample = get_spec_int_param(specs, "sample");
    float inDn = get_spec_float_param(specs, "inDn");
    char *bandExt = get_spec_string_param(specs, "bandExt");
    int dbFlag = get_spec_int_param(specs, "dbFlag");
    float expected_value = get_spec_float_param(specs, "expected_value");
    float tolerance = get_spec_float_param(specs, "tolerance");

    char *metadata = 
      (char *) MALLOC(sizeof(char)*(strlen(path)+strlen(meta_file)+5));
    sprintf(metadata, "%s%c%s", path, DIR_SEPARATOR, meta_file);
    meta_parameters *meta = meta_read(metadata);
    float value = get_cal_dn(meta, incid*D2R, sample, inDn, bandExt, dbFlag);
    meta_free(meta);
    if (FLOAT_COMPARE_TOLERANCE(value, expected_value, tolerance))
      passed = TRUE;
    else
      printf("\nValue: %.6f, expected value: %.6f\n", value, expected_value);
  }

  return passed;
}

int lib_test(char *lib_func, char *path, char *spec_file)
{
  return lib_test_ext(lib_func, path, spec_file, REPORT_LEVEL_NONE);
}
