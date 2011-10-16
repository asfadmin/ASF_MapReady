#include "asf.h"
#include "asf_test.h"
#include <ctype.h>

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

static int read_int(char *line, char *param)
{
  char *tmp;
  int value;

  tmp = read_str(line, param);
  sscanf(tmp, "%i", &value);

  return value;
}

static float read_float(char *line, char *param)
{
  char *tmp;
  float value;

  tmp = read_str(line, param);
  sscanf(tmp, "%f", &value);

  return value;
}

static double read_double(char *line, char *param)
{
  char *tmp;
  double value;

  tmp = read_str(line, param);
  sscanf(tmp, "%lf", &value);

  return value;
}

int init_test_config(char *configFile)
{
  FILE *fConfig;

  if ( fileExists(configFile) ) {
    asfPrintError("Cannot create file, %s, because it already exists.\n",
                  configFile);
  }
  fConfig = FOPEN(configFile, "w");

  fprintf(fConfig, "asf_test configuration file\n\n");

  fprintf(fConfig, "[General]\n\n");
  // suite
  fprintf(fConfig, "# This parameter defines the name of the test suite\n\n");
  fprintf(fConfig, "suite = \n\n");
  // type
  fprintf(fConfig, "# This parameter defines the test type: metadata, library, "
	  "binary\n# metadata runs checks on .meta file.\n"
	  "# library run on library functions.\n"
	  "# binary run checks on binary data files.\n\n");
  fprintf(fConfig, "type = metadata\n\n");
  // short configuration
  fprintf(fConfig, "# The short configuration file flag allows the experienced "
	  "user to\n# generate configuration files without the verbose "
	  "comments that explain all\n# entries for the parameters in the "
	  "configuration file (1 for a configuration\n# without comments, 0 "
	  "for a configuration file with verbose comments)\n\n");
  fprintf(fConfig, "short configuration file = 0\n\n");

  // status
  fprintf(fConfig, "# The general status field indicates the progress of the "
	  "testing.\n# Unless the interface is chosen to be 'manual' the tool "
	  "will run all tests\n# regardless of their individual status.\n"
	  "# Values for status: new, passed, failed\n\n");
  fprintf(fConfig, "status = new\n\n");

  fprintf(fConfig, "# Each test of the suite has its own section. The name of "
	  "the test is\n# user defined in name of the section [<test name>]"
	  "\n\n");
  fprintf(fConfig, "[Test]\n\n");
  // file
  fprintf(fConfig, "# This parameters defines the name of the file to be "
	  "tested.\n\n");
  fprintf(fConfig, "file = \n\n");\
  // specs
  fprintf(fConfig, "# This parameters defines the name of the specification "
	  "file that contains\n# the metadata parameters that are being "
	  "checked. They can be checked for\n# existence in the file, a value "
	  "within a range (defined by minimum and maximum)\n# and values in a "
	  "predefined list.\n"
	  "# The specification files are located in %s/meta_test.\n\n", 
	  get_asf_share_dir());
  fprintf(fConfig, "specs = \n\n");
  // status
  fprintf(fConfig, "# The parameter indicates the status of this test.\n"
	  "# Unless the interface is chosen to be 'manual' the tool "
	  "will run all tests\n# regardless of their individual status.\n"
	  "# Once a test is run, the status is either 'passed' or 'failed'. "
	  "To avoid\n# that a test is executed altogether in manual mode set "
	  "the status to 'skip'\n"
	  "# Values for status: new, passed, failed, skip\n\n");
  fprintf(fConfig, "status = new\n\n");

  FCLOSE(fConfig);

  asfPrintStatus("   Initialized test configuration file\n\n");

  return(0);
}

void free_library_function(t_library *lib)
{
  if (strcmp(lib->name, "get_cal_dn") == 0) {
    FREE(lib->get_cal_dn->meta_file);
    FREE(lib->get_cal_dn->bandExt);
    FREE(lib->get_cal_dn);
  }
  FREE(lib);
}

void free_test_config(test_config *cfg)
{
  if (cfg) {
    if (cfg->general) {
      int ii;
      for (ii=0; ii<cfg->general->test_count; ii++) {
	FREE(cfg->test[ii]->test);
	FREE(cfg->test[ii]->file);
	FREE(cfg->test[ii]->specs);
	if (strcmp_case(cfg->general->type, "library") == 0)
	  free_library_function(cfg->test[ii]->lib);
	FREE(cfg->test[ii]->status);
	FREE(cfg->test[ii]);
      }
      FREE(cfg->test);
      FREE(cfg->general->suite);
      FREE(cfg->general->type);
      FREE(cfg->general->status);
      FREE(cfg->general);
    }
    FREE(cfg);
    cfg = NULL;
  }
}

test_config *init_fill_test_config(char *configFile)
{
#define newStruct(type) (type *)MALLOC(sizeof(type))

  char params[25];

  // Create structure
  strcpy(params, "");
  test_config *cfg = newStruct(test_config);
  cfg->general = newStruct(t_general);

  // Initialize structure
  strcpy(cfg->comment, "asf_test configuration file");

  cfg->general->suite = (char *) MALLOC(sizeof(char)*100);
  strcpy(cfg->general->suite, "");
  cfg->general->type = (char *) MALLOC(sizeof(char)*25);
  strcpy(cfg->general->type, "");
  cfg->general->test_count = 0;
  cfg->general->status = (char *) MALLOC(sizeof(char)*25);
  strcpy(cfg->general->status, "");

  return cfg;
}

void init_library_function(t_library *lib, char *name)
{
  lib->get_cal_dn = NULL;
  if (strcmp_case(name, "get_cal_dn") == 0) {
    lib->get_cal_dn = (t_get_cal_dn *) MALLOC(sizeof(t_get_cal_dn));
    lib->get_cal_dn->meta_file = (char *) MALLOC(sizeof(char)*255);
    lib->get_cal_dn->bandExt = (char *) MALLOC(sizeof(char)*25);
  }
}

test_config *read_test_config(char *configFile)
{
  FILE *fConfig;
  test_config *cfg = NULL;
  char line[255], params[50], lib[100];
  char *test=NULL, *p;

  strcpy(params, "");
  cfg = init_fill_test_config(configFile);
  if (cfg == NULL) 
    check_return(1, "Creating configuration structure.\n");
  fConfig = fopen(configFile, "r");
  if (!fConfig) 
    return NULL;

  // Determine how many tests we actually have
  while (fgets(line, 255, fConfig) != NULL) {
    if (strchr(line, '[') && strchr(line, ']') && line[0] != '#' &&
	!strncmp(line, "[General]", 9) == 0)
      cfg->general->test_count++;
  }
  cfg->test = (t_test **) MALLOC(sizeof(t_test *)*cfg->general->test_count);
  int num = -1;
  FCLOSE(fConfig);

  // Read all test cases along with the general information
  fConfig = FOPEN(configFile, "r");
  strcpy(lib, "");
  while (fgets(line, 255, fConfig) != NULL) {

    if (strncmp(line, "[General]", 9) == 0) 
      strcpy(params, "general");
    if (strcmp(params, "general") == 0) {
      test = read_param(line);
      if (strncmp(test, "suite", 5) == 0)
	strcpy(cfg->general->suite, read_str(line, "suite"));
      if (strncmp(test, "type", 4) == 0)
	strcpy(cfg->general->type, read_str(line, "type"));
      if (strncmp(test, "short configuration file", 24) == 0)
	cfg->general->short_config = 
	  read_int(line, "short configuration file");
      if (strncmp(test, "status", 6) == 0)
	strcpy(cfg->general->status, read_str(line, "status"));
    }

    if (strchr(line, '[') && strchr(line, ']') && line[0] != '#' &&
	!strncmp(line, "[General]", 9) == 0) {
      strcpy(params, "testcase");
      num++;
      cfg->test[num] = newStruct(t_test);
      cfg->test[num]->test = (char *) MALLOC(sizeof(char)*100);
      strcpy(cfg->test[num]->test, "");
      cfg->test[num]->file = (char *) MALLOC(sizeof(char)*1024);
      strcpy(cfg->test[num]->file, "");
      cfg->test[num]->specs = (char *) MALLOC(sizeof(char)*100);
      strcpy(cfg->test[num]->specs, "");
      cfg->test[num]->status = (char *) MALLOC(sizeof(char)*25);
      strcpy(cfg->test[num]->status, "");
      if (strncmp(cfg->general->type, "library", 7) == 0) {
	cfg->test[num]->lib = (t_library *) MALLOC(sizeof(t_library));
	cfg->test[num]->lib->name = (char *) MALLOC(sizeof(char)*255);
      }
      p = strchr(line, ']');
      *p = '\0';
      strcpy(cfg->test[num]->test, line+1);
    }
    if (strcmp(params, "testcase") == 0) {
      test = read_param(line);
      if (strncmp(test, "file", 4) == 0)
	strcpy(cfg->test[num]->file, read_str(line, "file"));
      if (strncmp(test, "specs", 5) == 0)
	strcpy(cfg->test[num]->specs, read_str(line, "specs"));
      if (strncmp(test, "status", 6) == 0)
	strcpy(cfg->test[num]->status, read_str(line, "status"));
      if (strncmp(test, "library function", 16) == 0) {
	strcpy(cfg->test[num]->lib->name, 
	       read_str(line, "library function"));
	if (strncmp(cfg->test[num]->lib->name, "get_cal_dn", 10) == 0) {
	  init_library_function(cfg->test[num]->lib, "get_cal_dn");
	  strcpy(lib, "get_cal_dn");
	}
      }
      if (strlen(lib) > 0 && strcmp_case(lib, "get_cal_dn") == 0) {
	if (strncmp_case(test, "meta file", 9) == 0)
	  strcpy(cfg->test[num]->lib->get_cal_dn->meta_file, 
		 read_str(line, "meta file"));
	if (strncmp_case(test, "incidence angle", 15) == 0)
	  cfg->test[num]->lib->get_cal_dn->incid = 
	    read_int(line, "incidence angle");
	if (strncmp_case(test, "sample", 6) == 0)
	  cfg->test[num]->lib->get_cal_dn->sample = read_int(line, "sample");
	if (strncmp_case(test, "inDn", 4) == 0)
	  cfg->test[num]->lib->get_cal_dn->inDn = read_float(line, "inDn");
	if (strncmp_case(test, "bandExt", 7) == 0)
	  strcpy(cfg->test[num]->lib->get_cal_dn->bandExt,
		 read_str(line, "bandExt"));
	if (strncmp_case(test, "dbFlag", 6) == 0)
	  cfg->test[num]->lib->get_cal_dn->dbFlag = read_int(line, "dbFlag");
	if (strncmp(test, "expected value", 14) == 0)
	  cfg->test[num]->lib->get_cal_dn->value = 
	    read_double(line, "expected value");
      }
    }
  }

  if (test)
    FREE(test);
  else
    asfPrintError("Could not find manual tests in configuration file (%s).\n"
		  "Does the configuration contains unit tests?\n", configFile);
  FCLOSE(fConfig);

  return cfg;
}

int write_test_config(char *configFile, test_config *cfg)
{
  FILE *fConfig;
  int shortFlag = FALSE, ii;

  if (cfg == NULL)
    check_return(1, "No configuration structure to write.\n");
  if (cfg->general->short_config)
    shortFlag = TRUE;

  fConfig = FOPEN(configFile, "w");

  fprintf(fConfig, "%s\n\n", cfg->comment);

  fprintf(fConfig, "[General]\n");
  // suite
  if (!shortFlag)
    fprintf(fConfig, "\n# This parameter defines the name of the test suite\n\n");
  fprintf(fConfig, "suite = %s\n", cfg->general->suite);
  // type
  if (!shortFlag)
    fprintf(fConfig, "\n# This parameter defines the test type: metadata, binary\n"
	  "# metadata runs checks on .meta file.\n"
	  "# binary run checks on binary data files.\n\n");
  fprintf(fConfig, "type = %s\n", cfg->general->type);
  // short configuration
  if (!shortFlag)
    fprintf(fConfig, "\n# The short configuration file flag allows the experienced "
	  "user to\n# generate configuration files without the verbose "
	  "comments that explain all\n# entries for the parameters in the "
	  "configuration file (1 for a configuration\n# without comments, 0 "
	  "for a configuration file with verbose comments)\n\n");
  fprintf(fConfig, "short configuration file = %d\n", 
	  cfg->general->short_config);
  // status
  if (!shortFlag)
    fprintf(fConfig, "\n# The general status field indicates the progress of the "
	  "testing.\n# Unless the interface is chosen to be 'manual' the tool "
	  "will run all tests\n# regardless of their individual status.\n"
	  "# Values for status: new, passed, failed\n\n");
  fprintf(fConfig, "status = %s\n", cfg->general->status);

  if (!shortFlag)
    fprintf(fConfig, "\n# Each test of the suite has its own section. The name of "
	  "the test is\n# user defined in name of the section [<test name>]"
	  "\n\n");
  for (ii=0; ii<cfg->general->test_count; ii++) {
    fprintf(fConfig, "\n[%s]\n", cfg->test[ii]->test);
    if (strcmp_case(cfg->general->type, "library") == 0) {
      // library
      if (!shortFlag)
	fprintf(fConfig, "\n# This parameter defines the name of the library function to be tested.\n\n");
      fprintf(fConfig, "library function = %s\n", cfg->test[ii]->lib->name);
    }
    else {
      // file
      if (!shortFlag)
	fprintf(fConfig, "\n# This parameters defines the name of the file to be "
		"tested.\n\n");
      fprintf(fConfig, "file = %s\n", cfg->test[ii]->file);
    }
    // specs
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameters defines the name of the specification "
	    "file that contains\n# the metadata parameters that are being "
	    "checked. They can be checked for\n# existence in the file, a value "
	    "within a range (defined by minimum and maximum)\n# and values in a "
	    "predefined list.\n"
	    "# The specification files are located in %s/meta_test.\n\n", 
	    get_asf_share_dir());
    fprintf(fConfig, "specs = %s\n", cfg->test[ii]->specs);
    // status
    if (!shortFlag)
      fprintf(fConfig, "\n# The parameter indicates the status of this test.\n"
	    "# Unless the interface is chosen to be 'manual' the tool "
	    "will run all tests\n# regardless of their individual status.\n"
	    "# Once a test is run, the status is either 'passed' or 'failed'. "
	    "To avoid\n# that a test is executed altogether in manual mode set "
	    "the status to 'skip'\n"
	    "# Values for status: new, passed, failed, skip\n\n");
    fprintf(fConfig, "status = %s\n", cfg->test[ii]->status);
  }

  FCLOSE(fConfig);

  return(0);
}
