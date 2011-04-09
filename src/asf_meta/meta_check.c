#include "asf.h"
#include <math.h>

// Literally a copy of the lzFetch function. However, had to be done since
// the original functionality, written for Vexcel par files, indexed location
// blocks. This does not work with the location block in the metadata file.
int isspace(int c);

static char *getString(char *file,char *param, int *err)
{
  int structDepth, indexVec = 0;
  char structName[16][255];
  char line[255];
  FILE *in = fopen(file, "r");

  if (in == NULL) {
    if (err == NULL)
      asfPrintError("Couldn't open spec file '%s'!\n", file);
    else {
      *err = 3;
      return NULL;
    }
  }
    
  structDepth=0;
  strcpy(structName[structDepth],"");
  
  while (NULL != fgets(line, 255, in)) {
    if (strchr(line, '{') != NULL) {
      char newStruct[255];
      char index[255];
      sscanf(line, "%s", newStruct);
      if (0 == strcmp(newStruct, "state_vector")) {
	sprintf(index, "[%d]", indexVec);
	indexVec++;
      }
      else strcpy(index, "");
      strcpy(structName[structDepth+1], structName[structDepth]);
      if (structDepth != 0)
	strcat(structName[structDepth+1], ".");
      strcat(structName[structDepth+1], newStruct);
      strcat(structName[structDepth+1], index);
      structDepth++;
    } 
    else if (strchr(line,'}') != NULL)
      structDepth--;
    else {
      char thisParamName[255];
      char thisParam[255];
      sscanf(line, "%s", thisParamName);
      strcpy(thisParam, structName[structDepth]);
      if (structDepth != 0)
	strcat(thisParam, ".");
      strcat(thisParam, thisParamName);
      if (0 == strcmp(thisParam, param)) {
	char *ret = (char *) MALLOC(sizeof(char)*255);
	char *end = strchr(line, '#');
	end--;
	while (isspace(*end)) {
	  *end = '\0';
	  end--;
	}
	char *start = strchr(line, ':');
	start++;
	while (isspace(*start)) 
	  start++;
	strcpy(ret, start);
	if (err!=NULL)
	  *err=0; 
	FCLOSE(in);
	return ret;
      }
    }
  }
  if (err == NULL) {
    asfPrintWarning("Couldn't find field '%s' in spec file '%s'.\n",
		    param, file);
    FCLOSE(in);
    return NULL;
  }
  *err = 1;
  FCLOSE(in);
  return NULL;
}

static double getDouble(char *file, char *param, int *err)
{
  double ret;
  char *str;
  
  str = getString(file, param, err);
  if ((err != NULL) && (*err != 0)) 
    return 0.0;
  if (str == NULL && err != NULL) 
    *err = 2;
  if (str && 1 != sscanf(str, "%lf", &ret)) {
    if (err == NULL)
      asfPrintError("Couldn't convert '%s' to a double.\n",str);
    else
      *err=2;
  }
  FREE(str);
  return ret;
}

static int getInt(char *file, char *param, int *err)
{
    int ret;
    char *str = getString(file, param, err);
    if ((err != NULL) && (*err != 0)) 
      return 0;
    if (!str) 
      return 0;
    if (1 != sscanf(str, "%d", &ret)) {
      if (err==NULL) 
	asfPrintError("Couldn't convert '%s' to an integer.\n", str);
      else
	*err=3;
    }
    FREE(str);
    return ret;
}

int meta_test(char *in_file, char *spec_file)
{
  meta_test_ext(in_file, spec_file, REPORT_LEVEL_NONE);
}

int meta_test_ext(char *in_file, char *spec_file, report_level_t level)
{
  char *filename = NULL, line[1024], param[100], type[25], valid[500], *p;
  char *strValue;
  int nMin, nMax, nValue, err, passed = TRUE;
  double lfMin, lfMax, lfValue;
  
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
    sprintf(filename, "%s/meta_test/%s", get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp) {
    sprintf(filename, "%s/meta_test/%s.specs", 
	    get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp)
    asfPrintError("Could not find spec file (%s)\n", spec_file);
  if (filename)
    FREE(filename);

  // FIXME: Need to implement some pre-checks.
  // For example, map projections will need special treatment. For a given
  // map projection, a set of parameters need are required to define that.
  // We need to be able to ignore some parameters in this context as well.
  // Only for the UAVSAR case, we are good to go.
  
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
    strcat(param, ":");

    // data type
    sprintf(line, "%s", p+1);
    p = strchr(line, ',');
    if (p)
      *p = '\0';
    sprintf(type, "%s", line);

    // valid
    // If no value is given, we just want to check the parameter is given in
    // the metadata file. If we find quotation marks then assume we have a list
    // of values to check. Otherwise, we assume to find a minimum and a maximum
    // value to check.
    if (p) {
      sprintf(line, "%s", p+1);
      if (line[0] == '\'' && line[strlen(line)-1] == '\'') {
	sprintf(valid, "%s", line+1);
	valid[strlen(valid)-1] = '\0';
	strValue = getString(in_file, param, &err);
	if (err || (strlen(valid) > 0  && strstr(valid, strValue) == NULL)) {
	  asfReport(level, "   %s failed\n", param);
	  passed = FALSE;
	}
      }
      else {
	if (strncmp_case(type, "INT", 3) == 0) {
	  sscanf(line, "%d,%d", &nMin, &nMax);
	  nValue = getInt(in_file, param, &err);
	  if (err || (nValue < nMin || nValue > nMax)) {
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
	else if (strncmp_case(type, "DOUBLE", 6) == 0) {
	  sscanf(line, "%lf,%lf", &lfMin, &lfMax);
	  lfValue = getDouble(in_file, param, &err);
	  if (err || (lfValue < lfMin && lfValue > lfMax)) {
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
      }
    }
  }
  return passed;
}
