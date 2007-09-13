/*******************************************************************************
NAME: diffmeta

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0            9/07   B. Dixon    As released

 PURPOSE:
  1. diffmeta has 2 goals while it checks the 'new version'
     metadata:
    a. The first is to validate that required values
       were written to the metadata (PRECHECK phase)

    b. The second goal is to verify that required values
       are populated, and if (any) value is populated that
       that it falls within the required range or set (enums,
       list of string values, etc) (PRECHECK phase)

    c. The third goal is to find if the metadata
       differs in a significant way from a 'baseline'
       set of metadata (BASELINE COMPARISON phase)

CAVEATS:
  1. While validation checking is performed during the PRECHECK
     phase, no effort is made to make sure that the values make
     any sense in _combination_, e.g. a SAR image with a populated
     optical block (etc)

*******************************************************************************/
#include "asf_meta.h"
#include "diffmeta.h"
#include "diffmeta_tolerances.h"

#define VERSION 1.0

/**** MACRO DEFINITIONS ****/
#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)

/**** PROTOTYPES ****/
void usage(char *name);
char *data_type2str(int data_type);
char *image_data_type2str(int image_data_type);
void projection_type_2_str(projection_type_t proj, char *proj_str);
void report_validation_errors(char *outputFile, char *file,
                              char *err_msg, char *block_id);
void report_difference_errors(char *outputFile,
                              char *file1, char *file2,
                              char *err_msg, char *block_id);
void validate_string(char *err_msgs, char *str,
                     char *block_id, char *var_name,
                     int *failed);
void verify_string(char *err_msgs, char *str,
                   char **valid_strings, int num_strings,
                   char *block_id, char *var_name,
                   int required, int *failed);
void verify_int(char *err_msgs, int num,
                int lower_lim, int upper_lim,
                char *block_id, char *var_name,
                int required, int *failed);
void verify_char(char *err_msgs, char ch,
                 char *valid_chars, int num_chars,
                 char *block_id, char *var_name,
                 int required, int *failed);
void validate_double(char *err_msgs, double num,
                     char *block_id, char *var_name,
                     int *failed);
void verify_double(char *err_msgs, double num,
                   double lower_lim, double upper_lim,
                   char *block_id, char *var_name,
                   int required, int *failed);
void compare_meta_string(char *err_msgs, char *block_id, char *var_name,
                      char *var1, char *var2, int *failed);
void compare_meta_enum(char *err_msgs, char *block_id, char *var_name,
                       int var1, int var2,
                       char* (*enum2str)(int), int *failed);
void compare_meta_int(char *err_msgs, char *block_id, char *var_name,
                      int var1, int var2, int *failed);
void compare_meta_char(char *err_msgs, char *block_id, char *var_name,
                       char var1, char var2, int *failed);
void compare_meta_double_with_tolerance(char *err_msgs, char *block_id, char *var_name,
                      double var1, double var2,
                      double tolerance, int *failed);

int main(int argc, char **argv)
{
  char *inFile1,*inFile2;
  char *metafile1, *metafile2;
  char *outputFile;
  extern int optind;            /* argv index of the next argument */
  extern char *optarg;          /* current argv[] */
  int c;                        /* option letter from getopt() */
  extern FILE *fLog;            /* output file descriptor, stdout or log file */
  FILE *fError;                 /* Error log, stderr or log file */
  extern int logflag, quietflag;
  int outputflag=0;
  char msg[1024];

  fLog=NULL;
  logflag=quietflag=0;
  outputFile=(char*)CALLOC(1024, sizeof(char));

  /* process command line */
  while ((c=getopt(argc,argv,"o:l:")) != EOF)
  {
    switch (c) {
      case 'l':/* -log <filename>, get logfile; this is sorta hacked */
        if (0==strncmp(optarg,"og",2)) {
          sscanf(argv[optind++], "%s", logFile);
          logflag=1;
          fLog = FOPEN(logFile, "w");
        }
        else {
          FREE(outputFile);
          usage(argv[0]);
        }
        break;
      case 'o':/* -output <filename>, get output filename ...empty if no differences */
        if (0==strncmp(optarg,"utput",5)) {
          sscanf(argv[optind++], "%s", outputFile);
          outputflag=1;
        }
        else {
          FREE(outputFile);
          usage(argv[0]);
        }
        break;
      default:
        FREE(outputFile);
        usage(argv[0]);
        break;
    }
  }
  asfSplashScreen(argc, argv);

  // After parsing out the command line arguments, there should be 2 arguments left...
  // the two files to compare
  if ((argc-optind) != 2) {
    if ((argc-optind) > 2) {
      printf("\n => Too many inputs.\n");
    }
    if ((argc-optind) < 2) {
      printf("\n => Too few inputs.\n");
    }
    FREE(outputFile);
    usage(argv[0]);
  }
  else {
    // Grab the file names
    inFile1=argv[optind];
    inFile2=argv[optind+1];
  }
  // Set up output redirection for error and log messages
  if (logflag == 0) {
    fLog = NULL;
    fError = NULL;
  }
  else {
    fError = fLog;
  }
  if (!outputflag) {
    sprintf(msg, "Missing output file name ...no place to store file differences!\n");
    fprintf(stderr, "** Error: ********\n%s** End of error **\n", msg);
    usage(argv[0]);
  }
  if (strcmp(logFile, outputFile) == 0) {
    sprintf(msg, "Log file cannot be the same as the output file:\n     Log file: %s\n  Output file: %s\n",
            logFile, outputFile);
    if (outputFile) FREE(outputFile);
    fprintf(stderr, "** Error: ********\n%s** End of error **\n", msg);
    usage(argv[0]);
  }

  // Create blank output file
  FILE *outFP=(FILE*)FOPEN(outputFile, "w");
  if(outFP == NULL) {
    asfPrintError("Cannot open output file for write:\n%s\n", outputFile);
  }
  else {
    FCLOSE(outFP);
  }

  if (strcmp(inFile1, inFile2) == 0) {
    asfPrintStatus("\nInput files are the same - PASS.\n\n");
    exit(0);
  }
  metafile1 = STRDUP_PLUS(inFile1, 4);
  metafile2 = STRDUP_PLUS(inFile2, 4);
  appendExt(metafile1, ".meta");
  appendExt(metafile2, ".meta");
  if (strcmp(metafile1, metafile2) == 0) {
    if (outputFile) FREE(outputFile);
    FREE(outputFile);
    return (0); // PASS - a file compared to itself is always the same
  }
  if (!fileExists(metafile1)) {
    sprintf(msg, "File not found: %s\n", metafile1);
    FREE(outputFile);
    asfPrintError(msg);
  }
  if (!fileExists(metafile2)) {
    sprintf(msg, "File not found: %s\n", metafile2);
    FREE(outputFile);
    asfPrintError(msg);
  }

  /***** And away we go.... *****/
  diff_check_metadata(outputFile, metafile1, metafile2);

  // Cleanup
  if (outputFile) FREE (outputFile);
  if (metafile1) FREE (metafile1);
  if (metafile2) FREE (metafile2);
  return (0);
}

void usage(char *name)
{
  printf("\nUSAGE:\n"
         "   %s <-output <diff_output_file>> [-log <file>] <metafile1> <metafile2>\n"
         "\nOPTIONS:\n"
      "   -output <diff_output_file>:  output to write metadata differencing\n"
      "                 results to (required.)\n"
      "   -log <file>:  allows the output to be written to a log file\n"
      "                 in addition to stdout (not required but strongly suggested.)\n"
      "\nINPUTS:\n"
      "   <metafile1>:  ASF metadata file to compare to.  File extension not required.\n"
      "   <metafile2>:  ASF metadata file to look for differences in.  File extenstion\n"
      "                 not required.\n"
      "\nDESCRIPTION:\n"
      "   1. diffmeta first checks all required fields in metadata2 for validity,\n"
      "      e.g. doubles should be valid doubles, etcetera.\n"
      "   2. diffmeta then verifies all fields in metafile2 for valid ranges and types\n"
      "      of data.\n"
      "   3. finally, metadiff compares metadata fields between metafile1 and metafile2:\n"
      "      a. If metafile2 is missing a field that exists in metafile1 => FAIL\n"
      "      b. If metafile2 has a field not found in metafile1 => OK\n"
      "      c. If similar fields found in both files are different by more the allowed\n"
      "         tolerance => FAIL\n"
      "      d. If the 3 steps above complete with no failures => PASS\n"
      "      e. If no failures of any kind occurred, the output file will be created but\n"
      "         will have zero length (empty file.)\n"
      "\nVersion %.2f, Alaska Satellite Facility Tools\n\n",name,VERSION);
  exit(1);
}

// User must free the returned string
char *image_data_type2str(int image_data_type)
{
  image_data_type_t type = (image_data_type_t)image_data_type;
  char *retstr = (char*)CALLOC(64, sizeof(char));

  switch (type) {
    case RAW_IMAGE:
      strcpy(retstr, "RAW_IMAGE");
      break;
    case COMPLEX_IMAGE:
      strcpy(retstr, "COMPLEX_IMAGE");
      break;
    case AMPLITUDE_IMAGE:
      strcpy(retstr, "AMPLITUTDE_IMAGE");
      break;
    case PHASE_IMAGE:
      strcpy(retstr, "PHASE_IMAGE");
      break;
    case POWER_IMAGE:
      strcpy(retstr, "POWER_IMAGE");
      break;
    case SIGMA_IMAGE:
      strcpy(retstr, "SIGMA_IMAGE");
      break;
    case GAMMA_IMAGE:
      strcpy(retstr, "GAMMA_IMAGE");
      break;
    case BETA_IMAGE:
      strcpy(retstr, "BETA_IMAGE");
      break;
    case COHERENCE_IMAGE:
      strcpy(retstr, "COHERENCE_IMAGE");
      break;
    case GEOREFERENCED_IMAGE:
      strcpy(retstr, "GEOREFERENCED_IMAGE");
      break;
    case GEOCODED_IMAGE:
      strcpy(retstr, "GEOCODED_IMAGE");
      break;
    case LUT_IMAGE:
      strcpy(retstr, "LUT_IMAGE");
      break;
    case ELEVATION:
      strcpy(retstr, "ELEVATION");
      break;
    case DEM:
      strcpy(retstr, "DEM");
      break;
    case IMAGE:
      strcpy(retstr, "IMAGE");
      break;
    case MASK:
      strcpy(retstr, "MASK");
      break;
    default:
      strcpy(retstr, "UNKNOWN");
      break;
  }

  return retstr;
}

// User must free the returned string
char *data_type2str(int data_type)
{
  data_type_t type = (data_type_t)data_type;
  char *retstr = (char*)CALLOC(64, sizeof(char));

  switch (type) {
    case BYTE:
      strcpy(retstr, "BYTE");
      break;
    case INTEGER16:
      strcpy(retstr, "INTEGER16");
      break;
    case INTEGER32:
      strcpy(retstr, "INTEGER32");
      break;
    case REAL32:
      strcpy(retstr, "REAL32");
      break;
    case REAL64:
      strcpy(retstr, "REAL64");
      break;
    case COMPLEX_BYTE:
      strcpy(retstr, "COMPLEX_BYTE");
      break;
    case COMPLEX_INTEGER16:
      strcpy(retstr, "COMPLEX_INTEGER16");
      break;
    case COMPLEX_INTEGER32:
      strcpy(retstr, "COMPLEX_INTEGER32");
      break;
    case COMPLEX_REAL32:
      strcpy(retstr, "COMPLEX_REAL32");
      break;
    case COMPLEX_REAL64:
      strcpy(retstr, "COMPLEX_REAL64");
      break;
    default:
      strcpy(retstr, "UNKNOWN");
      break;
  }

  return retstr;
}

void projection_type_2_str(projection_type_t proj, char *proj_str)
{
  switch (proj) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      strcpy(proj_str, "UTM");
      break;
    case ALBERS_EQUAL_AREA:
      strcpy(proj_str, "Albers Equal Area");
      break;
    case LAMBERT_CONFORMAL_CONIC:
      strcpy(proj_str, "Lambert Conformal Conic");
      break;
    case POLAR_STEREOGRAPHIC:
      strcpy(proj_str, "Polar Stereographic");
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      strcpy(proj_str, "Lambert Azimuthal Equal Area");
      break;
    default:
      strcpy(proj_str, "Unknown");
      break;
  }
}

void report_validation_errors(char *outputFile, char *file,
                              char *err_msg, char *block_id)
{
  char msg[1024];
  FILE *outFP = (FILE*)FOPEN(outputFile, "a");
  fprintf(outFP, "\n-----------------------------------------------\n");
  asfPrintStatus("\n-----------------------------------------------\n");

  sprintf(msg, "FAIL: Validation Checking of \n  %s\n\n", file);
  fprintf(outFP, msg);
  asfPrintStatus(msg);
  sprintf(msg, "%s Block Errors:\n\n", block_id);
  fprintf(outFP, msg);
  asfPrintStatus(msg);

  fprintf(outFP, err_msg);
  asfPrintStatus(err_msg);

  fprintf(outFP, "-----------------------------------------------\n\n");
  asfPrintStatus("-----------------------------------------------\n\n");
  FCLOSE(outFP);
}

void report_difference_errors(char *outputFile,
                              char *file1, char *file2,
                              char *err_msg, char *block_id)
{
  char msg[1024];
  FILE *outFP = (FILE*)FOPEN(outputFile, "a");

  fprintf(outFP, "\n-----------------------------------------------\n");
  asfPrintStatus("\n-----------------------------------------------\n");

  sprintf(msg, "FAIL: Differences found when comparing:\n  %s\nto\n  %s\n\n",
          file1, file2);
  fprintf(outFP, msg);
  asfPrintStatus(msg);
  sprintf(msg, "%s Block Errors:\n\n", block_id);
  fprintf(outFP, msg);
  asfPrintStatus(msg);

  fprintf(outFP, err_msg);
  asfPrintStatus(err_msg);

  fprintf(outFP, "-----------------------------------------------\n\n");
  asfPrintStatus("-----------------------------------------------\n\n");
  FCLOSE(outFP);
}

void validate_string(char *err_msgs, char *str,
                     char *block_id, char *var_name,
                     int *failed)
{
  if (!meta_is_valid_string(str)) {
    sprintf(err_msgs, "%s  [%s] Missing %s field in new version file.\n\n",
            err_msgs, block_id, var_name);
    *failed = 1;
  }
}

void verify_string(char *err_msgs, char *str,
                   char **valid_strings, int num_strings,
                   char *block_id, char *var_name,
                   int required, int *failed)
{
  if (meta_is_valid_string(str) && strlen(str) > 0)
  {
    int i, found;
    for (i=0, found=0; i<num_strings; i++) {
      if (strcmp(valid_strings[i], str) == 0) {
        found = 1;
      }
    }
    if (!found) {
      sprintf(err_msgs, "%s  [%s] Invalid %s field in new version file:\n    %s\n\n"
              "    Expected one of:\n",
              err_msgs, block_id, var_name, str);
      char cat_str[1024];
      strcpy(cat_str, "");
      for (i=0; i<num_strings; i++) {
        if (strlen(valid_strings[i]) > 0) {
          sprintf(cat_str, "%s      %s\n", cat_str, valid_strings[i]);
        }
      }
      strcat(err_msgs, cat_str);
      strcat(err_msgs, "\n\n");
      *failed = 1;
    }
  }
  else if (required) {
    sprintf(err_msgs, "%s  [%s] Missing %s field in new version file.\n\n"
            "    Expected one of:\n",
            err_msgs, block_id, var_name);
    int i;
    char cat_str[1024];
    strcpy(cat_str, "");
    for (i=0; i<num_strings; i++) {
      if (strlen(valid_strings[i]) > 0) {
        sprintf(cat_str, "%s      %s\n", cat_str, valid_strings[i]);
      }
    }
    strcat(err_msgs, cat_str);
    strcat(err_msgs, "\n\n");
    *failed = 1;
  }
}

void validate_int(char *err_msgs, int num,
                  char *block_id, char *var_name,
                  int *failed)
{
  if (!meta_is_valid_int(num)) {
    sprintf(err_msgs, "%s  [%s] Missing %s field in new version file.\n\n",
            err_msgs, block_id, var_name);
    *failed = 1;
  }
}

void verify_int(char *err_msgs, int num,
                int lower_lim, int upper_lim,
                char *block_id, char *var_name,
                int required, int *failed)
{
  if (meta_is_valid_int(num))
  {
    if (num < lower_lim || num > upper_lim)
    {
      sprintf(err_msgs,
              "%s  [%s] Die %s Nummern sind nicht richtig!!:\n    %d\n\n"
              "    Es sollte sein:\n      %d zu %d\n\n  RUDIGER muss es richtig machen!!\n\n",
              err_msgs, block_id, var_name, num, lower_lim, upper_lim);
      *failed = 1;
    }
  }
  else if (required) {
    sprintf(err_msgs,
            "%s  [%s] The %s field is missing\n\n"
            "    Expected:\n      %d to %d\n\n",
            err_msgs, block_id, var_name, lower_lim, upper_lim);
    *failed = 1;
  }
}

void verify_char(char *err_msgs, char ch,
                 char *valid_chars, int num_chars,
                 char *block_id, char *var_name,
                 int required, int *failed)
{
  if (meta_is_valid_char(ch))
  {
    int i, found;
    for (i=0, found=0; i<num_chars; i++) {
      if (ch == valid_chars[i]) {
        found = 1;
      }
    }
    if (!found) {
      sprintf(err_msgs, "%s  [%s] Invalid %s field in new version file:\n    '%c'\n\n"
              "    Expected one of:\n",
              err_msgs, block_id, var_name, ch);
      char cat_str[1024];
      strcpy(cat_str, "");
      for (i=0; i<num_chars; i++) {
        if (meta_is_valid_char(valid_chars[i])) {
          sprintf(cat_str, "%s      '%c'\n", cat_str, valid_chars[i]);
        }
      }
      strcat(err_msgs, cat_str);
      strcat(err_msgs, "\n\n");
      *failed = 1;
    }
  }
  else if (required) {
    sprintf(err_msgs, "%s  [%s] Missing %s field in new version file.\n\n"
            "    Expected one of:\n",
            err_msgs, block_id, var_name);
    int i;
    char cat_str[1024];
    strcpy(cat_str, "");
    for (i=0; i<num_chars; i++) {
      if (meta_is_valid_char(valid_chars[i])) {
        sprintf(cat_str, "%s      '%c'\n", cat_str, valid_chars[i]);
      }
    }
    strcat(err_msgs, cat_str);
    strcat(err_msgs, "\n\n");
    *failed = 1;
  }
}

void validate_double(char *err_msgs, double num,
                     char *block_id, char *var_name,
                     int *failed)
{
  if (!meta_is_valid_double(num)) {
    sprintf(err_msgs, "%s  [%s] Missing %s field in new version file.\n\n",
            err_msgs, block_id, var_name);
    *failed = 1;
  }
}

void verify_double(char *err_msgs, double num,
                   double lower_lim, double upper_lim,
                   char *block_id, char *var_name,
                   int required, int *failed)
{
  if (meta_is_valid_double(num))
  {
    if (num < lower_lim || num > upper_lim)
    {
      sprintf(err_msgs,
              "%s  [%s] The %s field is out of range:\n    %f\n\n"
              "    Expected:\n      %f to %f\n\n",
              err_msgs, block_id, var_name, num, lower_lim, upper_lim);
      *failed = 1;
    }
  }
  else if (required) {
    sprintf(err_msgs,
            "%s  [%s] The %s field is missing.\n\n"
            "    Expected:\n      %f to %f\n\n",
            err_msgs, block_id, var_name, lower_lim, upper_lim);
    *failed = 1;
  }
}

void compare_meta_string(char *err_msgs, char *block_id, char *var_name,
                         char *var1, char *var2, int *failed)
{
  if (meta_is_valid_string(var1)  &&
      meta_is_valid_string(var2)  &&
      strcmp(var1, var2) != 0)
  {
    sprintf(err_msgs, "%s  [%s] Baseline and new version %s different:\n    %s\n    %s\n\n",
            err_msgs, block_id,
            var_name, var1, var2);
    *failed = 1;
  }
}

void compare_meta_enum(char *err_msgs, char *block_id, char *var_name,
                       int var1, int var2,
                       char* (*enum2str)(int), int *failed)
{
  if (var1 >= 0  &&
      var2 >= 0  &&
      var1 != var2)
  {
    sprintf(err_msgs, "%s  [%s] Baseline and new version %s different:\n    %s\n    %s\n\n",
            err_msgs, block_id,
            var_name, (*enum2str)(var1), (*enum2str)(var1));
    *failed = 1;
  }
}

void compare_meta_int(char *err_msgs, char *block_id, char *var_name,
                      int var1, int var2, int *failed)
{
  if (meta_is_valid_int(var1)  &&
      meta_is_valid_int(var2)  &&
      var1 != var2)
  {
    sprintf(err_msgs, "%s  [%s] Baseline and new version %s different:\n    %d\n    %d\n\n",
            err_msgs, block_id,
            var_name, var1, var2);
    *failed = 1;
  }
}

void compare_meta_char(char *err_msgs, char *block_id, char *var_name,
                       char var1, char var2, int *failed)
{
  if (meta_is_valid_char(var1)  &&
      meta_is_valid_char(var2)  &&
      var1 != var2)
  {
    sprintf(err_msgs, "%s  [%s] Baseline and new version %s different:\n    '%c'\n    '%c'\n\n",
            err_msgs, block_id,
            var_name, var1, var2);
    *failed = 1;
  }
}

void compare_meta_double_with_tolerance(char *err_msgs, char *block_id, char *var_name,
                                        double var1, double var2,
                                        double tolerance, int *failed)
{
  if (meta_is_valid_double(var1)  &&
      meta_is_valid_double(var2)  &&
      !FLOAT_COMPARE_TOLERANCE(var1, var2, tolerance))
  {
    sprintf(err_msgs, "%s  [%s] Baseline and new version %s too different (%f):\n    %f\n    %f\n\n",
            err_msgs, block_id,
            var_name, fabs(var2-var1),
            var1, var2);
    *failed = 1;
  }
}

void diff_check_metadata(char *outputFile, char *metafile1, char *metafile2)
{
  int failed = 0;
  char precheck_err_msgs[8192];
  char compare_err_msgs[8192];

  // Block level convenience pointers
  meta_general *mg1, *mg2;
  meta_sar *msar1, *msar2;
  meta_optical *mo1, *mo2;
  meta_thermal *mtherm1, *mtherm2;
  meta_projection *mp1, *mp2;
  meta_transform *mtrans1, *mtrans2;
  meta_stats *mstats1, *mstats2;
  meta_band_stats *mbstats1, *mbstats2;
  meta_state_vectors *mstatev1, *mstatev2;
  meta_location *mloc1, *mloc2;

  // Element level convenience pointers
  proj_albers *albers1, *albers2; // Albers conical equal area
  proj_atct   *atct1, *atct2;     // Along track - cross track
  proj_lamaz  *lamaz1, *lamaz2;   // Lambert Azimuthal Equal Area
  proj_lamcc  *lamcc1, *lamcc2;   // Lambert Conformal Conic
  proj_ps     *ps1, *ps2;         // Polar Stereographic
  proj_utm    *utm1, *utm2;       // Universal Transverse Mercator
  proj_state  *state1, *state2;   // State Plane

  // Read metadata and set up convenience pointers
  meta_parameters *meta1 = meta_read(metafile1);
  meta_parameters *meta2 = meta_read(metafile2);
  mg1 = meta1->general;
  mg2 = meta2->general;
  msar1 = meta1->sar;                 // Can be NULL
  msar2 = meta2->sar;
  mo1 = meta1->optical;               // Can be NULL
  mo2 = meta2->optical;
  mtherm1 = meta1->thermal;           // Can be NULL
  mtherm2 = meta2->thermal;
  mp1 = meta1->projection;            // Can be NULL
  mp2 = meta2->projection;
  mtrans1 = meta1->transform;         // Can be NULL
  mtrans2 = meta2->transform;
  mstats1 = meta1->stats;
  mstats2 = meta2->stats;
  mbstats1 = meta1->band;
  mbstats2 = meta2->band;
  mstatev1 = meta1->state_vectors;    // Can be NULL
  mstatev2 = meta2->state_vectors;
  mloc1 = meta1->location;
  mloc2 = meta1->location;
  albers1 = &mp1->param.albers;
  albers2 = &mp2->param.albers;
  atct1 = &mp1->param.atct;
  atct2 = &mp2->param.atct;
  lamaz1 = &mp1->param.lamaz;
  lamaz2 = &mp2->param.lamaz;
  lamcc1 = &mp1->param.lamcc;
  lamcc2 = &mp2->param.lamcc;
  ps1 = &mp1->param.ps;
  ps2 = &mp2->param.ps;
  utm1 = &mp1->param.utm;
  utm2 = &mp2->param.utm;
  state1 = &mp1->param.state;
  state2 = &mp2->param.state;

  ////////////////////////////////////////////////////////////
  // PRECHECK                                               //
  //                                                        //
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Check General Block
  //
  failed = 0; // Start out with no failures
  strcpy(precheck_err_msgs, "");
  // These strings are required
  validate_string(precheck_err_msgs, mg2->basename,
                  "General", "basename",
                  &failed);
  validate_string(precheck_err_msgs, mg2->sensor,
                  "General", "sensor",
                  &failed);
  validate_string(precheck_err_msgs, mg2->sensor_name,
                  "General", "sensor_name",
                  &failed);
  validate_string(precheck_err_msgs, mg2->mode,
                  "General", "mode",
                  &failed);
  validate_string(precheck_err_msgs, mg2->processor,
                  "General", "processor",
                  &failed);
  validate_string(precheck_err_msgs, mg2->system,
                  "General", "system",
                  &failed);
  validate_string(precheck_err_msgs, mg2->acquisition_date,
                  "General", "acquisition_date",
                  &failed);

# define NUM_SENSOR_STRINGS 6
  char *sensor_strings[NUM_SENSOR_STRINGS] =
    {"SIR-C", "ERS1",
     "ERS2",  "JERS1",
     "ALOS",  "RSAT-1"};
  verify_string(precheck_err_msgs, mg2->sensor,
                sensor_strings, NUM_SENSOR_STRINGS,
                "General", "sensor",
                1, &failed);

# define NUM_SENSOR_NAME_STRINGS 3
  char *sensor_name_strings[NUM_SENSOR_NAME_STRINGS] =
    {"SAR", "AVNIR", "PRISM"};
  verify_string(precheck_err_msgs, mg2->sensor_name,
                sensor_name_strings, NUM_SENSOR_NAME_STRINGS,
                "General", "sensor_name",
                1, &failed);

  // FIXME: All of these strings are probably in a .h file.  If not, then they
  // need to be put into a .h file.  The code here should develop the array of
  // valid mode strings at run time.  The same thing may apply to the sensor_name,
  // sensor, and other strings or lists of characters...
# define NUM_MODE_STRINGS 164
  char *mode_strings[NUM_MODE_STRINGS] =
    {
      "ALOS","STD",
      "1A","1B1","1B2R","1B2G",
      "SWA","SWB","SNA", "SNB",
      "ST1","ST2","ST3","ST4","ST5","ST6","ST7",
      "WD1","WD2","WD3",
      "EL1",
      "EH1","EH2","EH3","EH4","EH5","EH6",
      "FN1","FN2","FN3","FN4","FN5",
      "FBS1","FBS2","FBS3","FBS4","FBS5","FBS6","FBS7","FBS8","FBS9","FBS10",
      "FBS11","FBS12","FBS13","FBS14","FBS15","FBS16","FBS17","FBS18",
      "FBS1","FBS2","FBS3","FBS4","FBS5","FBS6","FBS7","FBS8","FBS9","FBS10",
      "FBS11","FBS12","FBS13","FBS14","FBS15","FBS16","FBS17","FBS18",
      "FBD1","FBD2","FBD3","FBD4","FBD5","FBD6","FBD7","FBD8","FBD9","FBD10",
      "FBD11","FBD12","FBD13","FBD14","FBD15","FBD16","FBD17","FBD18",
      "FBD1","FBD2","FBD3","FBD4","FBD5","FBD6","FBD7","FBD8","FBD9","FBD10",
      "FBD11","FBD12","FBD13","FBD14","FBD15","FBD16","FBD17","FBD18",
      "WD1","WD2","WD1","WD2","WD1","WD2","WD1","WD2","WD1","WD2","WD1","WD2",
      "DSN1","DSN2","DSN3","DSN4","DSN5","DSN6","DSN7","DSN8","DSN9","DSN10",
      "DSN11","DSN12","DSN13","DSN14","DSN15","DSN16","DSN17","DSN18",
      "DSN1","DSN2","DSN3","DSN4","DSN5","DSN6","DSN7","DSN8","DSN9","DSN10",
      "DSN11","DSN12","DSN13","DSN14","DSN15","DSN16","DSN17","DSN18",
      "PLR1","PLR2","PLR3","PLR4","PLR5","PLR6","PLR7","PLR8","PLR9","PLR10",
      "PLR11","PLR12"};
  verify_string(precheck_err_msgs, mg2->mode,
                mode_strings, NUM_MODE_STRINGS,
                "General", "mode",
                1, &failed);

  if (mg2->data_type != BYTE              &&
      mg2->data_type != INTEGER16         &&
      mg2->data_type != INTEGER32         &&
      mg2->data_type != REAL32            &&
      mg2->data_type != REAL64            &&
      mg2->data_type != COMPLEX_BYTE      &&
      mg2->data_type != COMPLEX_INTEGER16 &&
      mg2->data_type != COMPLEX_INTEGER32 &&
      mg2->data_type != COMPLEX_REAL32    &&
      mg2->data_type != COMPLEX_REAL64)
  {
    char *d = data_type2str(mg2->data_type);
    sprintf(precheck_err_msgs,
            "%s[General] Unrecognized data_type field in new version file:\n"
            "  %s\n\n",
            precheck_err_msgs,
            d);
    if(d)FREE(d);
    failed = 1;
  }
  if (mg2->image_data_type != RAW_IMAGE             &&
      mg2->image_data_type != COMPLEX_IMAGE         &&
      mg2->image_data_type != AMPLITUDE_IMAGE       &&
      mg2->image_data_type != PHASE_IMAGE           &&
      mg2->image_data_type != POWER_IMAGE           &&
      mg2->image_data_type != SIGMA_IMAGE           &&
      mg2->image_data_type != GAMMA_IMAGE           &&
      mg2->image_data_type != BETA_IMAGE            &&
      mg2->image_data_type != COHERENCE_IMAGE       &&
      mg2->image_data_type != GEOREFERENCED_IMAGE   &&
      mg2->image_data_type != GEOCODED_IMAGE        &&
      mg2->image_data_type != LUT_IMAGE             &&
      mg2->image_data_type != ELEVATION             &&
      mg2->image_data_type != DEM                   &&
      mg2->image_data_type != IMAGE                 &&
      mg2->image_data_type != MASK)
  {
    char *d = image_data_type2str(mg2->image_data_type);
    sprintf(precheck_err_msgs,
            "%s[General] Unrecognized image_data_type field in new version file:\n"
            "  %s\n\n",
            precheck_err_msgs,
            d);
    if(d)FREE(d);
    failed = 1;
  }

  verify_int(precheck_err_msgs, mg2->orbit,
             0, DM_MAX_ORBIT,
             "General", "orbit",
             0, &failed);

# define NUM_ORBIT_DIRECTION_CHARS 2
  char orbit_direction_chars[NUM_ORBIT_DIRECTION_CHARS] =
    {'A', 'D'};
  verify_char(precheck_err_msgs, mg2->orbit_direction,
              orbit_direction_chars, NUM_ORBIT_DIRECTION_CHARS,
              "General", "orbit_direction",
              1, &failed);

  verify_int(precheck_err_msgs, mg2->frame,
             DM_MIN_FRAME, DM_MAX_FRAME,
             "General", "frame",
             0, &failed);

  verify_int(precheck_err_msgs, mg2->band_count,
             DM_MIN_BAND_COUNT, DM_MAX_BAND_COUNT,
             "General", "band_count",
             1, &failed);

  verify_int(precheck_err_msgs, mg2->line_count,
             DM_MIN_LINE_COUNT, DM_MAX_LINE_COUNT,
             "General", "line_count",
             1, &failed);

  verify_int(precheck_err_msgs, mg2->sample_count,
             DM_MIN_SAMPLE_COUNT, DM_MAX_SAMPLE_COUNT,
             "General", "sample_count",
             1, &failed);

  verify_int(precheck_err_msgs, mg2->start_line,
             DM_MIN_START_LINE, DM_MAX_START_LINE,
             "General", "start_line",
             1, &failed);
  if (meta_is_valid_int(mg2->start_line) &&
      meta_is_valid_int(mg2->line_count) &&
      (mg2->start_line >= mg2->line_count)) {
    sprintf(precheck_err_msgs,
            "%s[General] New version start_line (%d) greater than line_count (%d)\n\n",
            precheck_err_msgs, mg2->start_line, mg2->line_count);
    failed = 1;
  }

  verify_int(precheck_err_msgs, mg2->start_sample,
             DM_MIN_START_SAMPLE, DM_MAX_START_SAMPLE,
             "General", "start_sample",
             1, &failed);
  if (meta_is_valid_int(mg2->start_sample) &&
      meta_is_valid_int(mg2->sample_count) &&
      (mg2->start_sample >= mg2->sample_count)) {
    sprintf(precheck_err_msgs,
            "%s[General] New version start_sample (%d) greater than sample_count (%d)\n\n",
            precheck_err_msgs, mg2->start_sample, mg2->sample_count);
    failed = 1;
  }

  verify_double(precheck_err_msgs, mg2->x_pixel_size,
                DM_MIN_X_PIXEL_SIZE, DM_MAX_X_PIXEL_SIZE,
                "General", "x_pixel_size",
                1, &failed);

  verify_double(precheck_err_msgs, mg2->y_pixel_size,
                DM_MIN_Y_PIXEL_SIZE, DM_MAX_Y_PIXEL_SIZE,
                "General", "y_pixel_size",
                1, &failed);

  verify_double(precheck_err_msgs, mg2->center_latitude,
                DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                "General", "center_latitude",
                1, &failed);

  verify_double(precheck_err_msgs, mg2->center_longitude,
                DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                "General", "center_longitude",
                1, &failed);

  verify_double(precheck_err_msgs, mg2->re_major,
                DM_MIN_RE_MAJOR, DM_MAX_RE_MAJOR,
                "General", "re_major",
                1, &failed);

  verify_double(precheck_err_msgs, mg2->re_minor,
                DM_MIN_RE_MINOR, DM_MAX_RE_MINOR,
                "General", "re_minor",
                1, &failed);

  verify_int(precheck_err_msgs, mg2->bit_error_rate,
             DM_MIN_BIT_ERROR_RATE, DM_MAX_BIT_ERROR_RATE,
             "General", "bit_error_rate",
             0, &failed);

  int max_missing_lines;
  if (meta_is_valid_int(mg2->line_count) &&
      mg2->line_count > 0)
  {
    max_missing_lines = mg2->line_count - 1;
  }
  else {
    max_missing_lines = DM_MAX_MISSING_LINES;
  }
  verify_int(precheck_err_msgs, mg2->missing_lines,
             DM_MIN_MISSING_LINES, max_missing_lines,
             "General", "missing_lines",
             0, &failed);

  // GENERAL BLOCK REPORTING
  // If any failures occurred, produce a report in the output file
  if (failed) {
    report_validation_errors(outputFile, metafile2,
                             precheck_err_msgs, "GENERAL");
  }
  //
  // End of General Block Validity Check
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Check SAR Block
  //
  if (msar2) {
    failed = 0;
    strcpy(precheck_err_msgs, "");

#   define NUM_IMAGE_TYPE_CHARS 4
    char image_type_chars[NUM_IMAGE_TYPE_CHARS] =
      {'S', 'G', 'R', 'P'};
    verify_char(precheck_err_msgs, msar2->image_type,
                image_type_chars, NUM_IMAGE_TYPE_CHARS,
                "SAR", "image_type",
                1, &failed);

#   define NUM_LOOK_DIRECTION_CHARS 2
    char look_direction_chars[NUM_LOOK_DIRECTION_CHARS] =
      {'L', 'R'};
    verify_char(precheck_err_msgs, msar2->look_direction,
                look_direction_chars, NUM_LOOK_DIRECTION_CHARS,
                "SAR", "look_direction",
                1, &failed);

    verify_int(precheck_err_msgs, msar2->look_count,
               DM_MIN_LOOK_COUNT, DM_MAX_LOOK_COUNT,
               "SAR", "look_count",
               0, &failed);

    verify_int(precheck_err_msgs, msar2->deskewed,
               DM_MIN_DESKEWED, DM_MAX_DESKEWED,
               "SAR", "deskewed",
               1, &failed);

    verify_int(precheck_err_msgs, msar2->original_line_count,
               DM_MIN_ORIGINAL_LINE_COUNT, DM_MAX_ORIGINAL_LINE_COUNT,
               "SAR", "original_line_count",
               1, &failed);

    verify_int(precheck_err_msgs, msar2->original_sample_count,
               DM_MIN_ORIGINAL_SAMPLE_COUNT, DM_MAX_ORIGINAL_SAMPLE_COUNT,
               "SAR", "original_sample_count",
               1, &failed);

    verify_int(precheck_err_msgs, msar2->line_increment,
               DM_MIN_LINE_INCREMENT, DM_MAX_LINE_INCREMENT,
               "SAR", "line_increment",
               1, &failed);

    verify_int(precheck_err_msgs, msar2->sample_increment,
               DM_MIN_SAMPLE_INCREMENT, DM_MAX_SAMPLE_INCREMENT,
               "SAR", "sample_increment",
               1, &failed);

    verify_double(precheck_err_msgs, msar2->range_time_per_pixel,
                  DM_MIN_RANGE_TIME_PER_PIXEL, DM_MAX_RANGE_TIME_PER_PIXEL,
                  "SAR", "range_time_per_pixel",
                  1, &failed);

    verify_double(precheck_err_msgs, msar2->azimuth_time_per_pixel,
                  DM_MIN_AZIMUTH_TIME_PER_PIXEL, DM_MAX_AZIMUTH_TIME_PER_PIXEL,
                  "SAR", "azimuth_time_per_pixel",
                  1, &failed);

    verify_double(precheck_err_msgs, msar2->slant_shift,
                  DM_MIN_SLANT_SHIFT, DM_MAX_SLANT_SHIFT,
                  "SAR", "slant_shift",
                  1, &failed);

    verify_double(precheck_err_msgs, msar2->time_shift,
                  DM_MIN_TIME_SHIFT, DM_MAX_TIME_SHIFT,
                  "SAR", "time_shift",
                  1, &failed);

    verify_double(precheck_err_msgs, msar2->slant_range_first_pixel,
                  DM_MIN_SLANT_RANGE_FIRST_PIXEL, DM_MAX_SLANT_RANGE_FIRST_PIXEL,
                  "SAR", "slant_range_first_pixel",
                  1, &failed);

    verify_double(precheck_err_msgs, msar2->wavelength,
                  DM_MIN_WAVELENGTH, DM_MAX_WAVELENGTH,
                  "SAR", "wavelength",
                  1, &failed);

    verify_double(precheck_err_msgs, msar2->prf,
                  DM_MIN_PRF, DM_MAX_PRF,
                  "SAR", "prf",
                  1, &failed);

    verify_double(precheck_err_msgs, msar2->earth_radius,
                  DM_MIN_EARTH_RADIUS, DM_MAX_EARTH_RADIUS,
                  "SAR", "earth_radius",
                  1, &failed);

    verify_double(precheck_err_msgs, msar2->earth_radius_pp,
                  DM_MIN_EARTH_RADIUS, DM_MAX_EARTH_RADIUS,
                  "SAR", "earth_radius",
                  1, &failed);

    verify_double(precheck_err_msgs, msar2->satellite_height,
                  DM_MIN_SATELLITE_HEIGHT, DM_MAX_SATELLITE_HEIGHT,
                  "SAR", "satellite_height",
                  1, &failed);

    // Ignore satellite_binary_time
    // Ignore satellite_clock_time

    verify_double(precheck_err_msgs, msar2->range_doppler_coefficients[0],
                  DM_MIN_DOP_RANGE_CENTROID, DM_MAX_DOP_RANGE_CENTROID,
                  "SAR", "range_doppler_coefficients[0]",
                  0, &failed);

    verify_double(precheck_err_msgs, msar2->range_doppler_coefficients[1],
                  DM_MIN_DOP_RANGE_PER_PIXEL, DM_MAX_DOP_RANGE_PER_PIXEL,
                  "SAR", "range_doppler_coefficients[1]",
                  0, &failed);

    verify_double(precheck_err_msgs, msar2->range_doppler_coefficients[2],
                  DM_MIN_DOP_RANGE_QUAD, DM_MAX_DOP_RANGE_QUAD,
                  "SAR", "range_doppler_coefficients[2]",
                  0, &failed);

    verify_double(precheck_err_msgs, msar2->azimuth_doppler_coefficients[0],
                  DM_MIN_DOP_AZIMUTH_CENTROID, DM_MAX_DOP_AZIMUTH_CENTROID,
                  "SAR", "azimuth_doppler_coefficients[0]",
                  0, &failed);

    verify_double(precheck_err_msgs, msar2->azimuth_doppler_coefficients[1],
                  DM_MIN_DOP_AZIMUTH_PER_PIXEL, DM_MAX_DOP_AZIMUTH_PER_PIXEL,
                  "SAR", "azimuth_doppler_coefficients[1]",
                  0, &failed);

    verify_double(precheck_err_msgs, msar2->azimuth_doppler_coefficients[2],
                  DM_MIN_DOP_AZIMUTH_QUAD, DM_MAX_DOP_AZIMUTH_QUAD,
                  "SAR", "azimuth_doppler_coefficients[2]",
                  0, &failed);

    // SAR BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      report_validation_errors(outputFile, metafile2,
                               precheck_err_msgs, "SAR");
    }
  }
  //
  // End of SAR Block Validity Check
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Check Optical Block
  //
  if (mo2) {
    failed = 0;
    strcpy(precheck_err_msgs, "");

# define NUM_POINTING_DIRECTION_STRINGS 4
    char *pointing_direction_strings[NUM_POINTING_DIRECTION_STRINGS] =
      {"FORWARD", "BACKWARD",
       "NADIR",  "OFF-NADIR"};
    verify_string(precheck_err_msgs, mo2->pointing_direction,
                  pointing_direction_strings, NUM_POINTING_DIRECTION_STRINGS,
                  "Optical", "pointing_direction",
                  1, &failed);

    verify_double(precheck_err_msgs, mo2->off_nadir_angle,
                  DM_MIN_OFF_NADIR_ANGLE, DM_MAX_OFF_NADIR_ANGLE,
                  "Optical", "off_nadir_angle",
                  1, &failed);

#   define NUM_CORRECTION_LEVEL_CHARS 4
    char *correction_level_chars[NUM_CORRECTION_LEVEL_CHARS] =
      {"N", "R", "G", "D"};
    verify_string(precheck_err_msgs, mo2->correction_level,
                  correction_level_chars, NUM_CORRECTION_LEVEL_CHARS,
                  "Optical", "correction_level",
                  1, &failed);

    verify_double(precheck_err_msgs, mo2->cloud_percentage,
                  DM_MIN_CLOUD_PERCENTAGE, DM_MAX_CLOUD_PERCENTAGE,
                  "Optical", "cloud_percentage",
                  0, &failed);

    verify_double(precheck_err_msgs, mo2->sun_azimuth_angle,
                  DM_MIN_SUN_AZIMUTH_ANGLE, DM_MAX_SUN_AZIMUTH_ANGLE,
                  "Optical", "sun_azimuth_angle",
                  0, &failed);

    verify_double(precheck_err_msgs, mo2->sun_elevation_angle,
                  DM_MIN_SUN_ELEVATION_ANGLE, DM_MAX_SUN_ELEVATION_ANGLE,
                  "Optical", "sun_elevation_angle",
                  0, &failed);

    // OPTICAL BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      report_validation_errors(outputFile, metafile2,
                               precheck_err_msgs, "OPTICAL");
    }
  }
  //
  // End of Optical Block Validity Check
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Check Thermal Block
  //
  if (mtherm2) {
    failed = 0;
    strcpy(precheck_err_msgs, "");

    verify_double(precheck_err_msgs, mtherm2->band_gain,
                  DM_MIN_BAND_GAIN, DM_MAX_BAND_GAIN,
                  "Thermal", "band_gain",
                  0, &failed);

    verify_double(precheck_err_msgs, mtherm2->band_gain_change,
                  DM_MIN_BAND_GAIN_CHANGE, DM_MAX_BAND_GAIN_CHANGE,
                  "Thermal", "band_gain_change",
                  0, &failed);

    verify_int(precheck_err_msgs, mtherm2->day,
               DM_MIN_DAY, DM_MAX_DAY,
               "Thermal", "day",
               0, &failed);

    // THERMAL BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      report_validation_errors(outputFile, metafile2,
                               precheck_err_msgs, "THERMAL");
    }
  }
  //
  // End of Thermal Block Validity Check
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Check Transform Block
  //
  if (mtrans2) {
    failed = 0;
    strcpy(precheck_err_msgs, "");
    int i;
    for (i=0; i<mtrans2->parameter_count; i++) {
      if (!meta_is_valid_double(mtrans2->x[i]) ||
          !meta_is_valid_double(mtrans2->y[i]) ||
          !meta_is_valid_double(mtrans2->l[i]) ||
          !meta_is_valid_double(mtrans2->s[i]))
      {
        failed = 1;
      }
      if (failed) {
        sprintf(precheck_err_msgs, "%s  %s\n",
                precheck_err_msgs,
                "[Transform] One or more of the transform parameters\n"
                "    in the metadata transform block is not a valid double\n"
                "    or is NaN.");
      }
    }
    // TRANSFORM BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      report_validation_errors(outputFile, metafile2,
                               precheck_err_msgs, "TRANSFORM");
    }
  }
  //
  // End of Transform Block Validity Check
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Check Projection Block
  //
  if (mp2) {
    failed = 0;
    strcpy(precheck_err_msgs, "");
    if (mp2->type != UNIVERSAL_TRANSVERSE_MERCATOR  &&
        mp2->type != POLAR_STEREOGRAPHIC            &&
        mp2->type != ALBERS_EQUAL_AREA              &&
        mp2->type != LAMBERT_CONFORMAL_CONIC        &&
        mp2->type != LAMBERT_AZIMUTHAL_EQUAL_AREA   &&
        mp2->type != STATE_PLANE                    &&
        mp2->type != SCANSAR_PROJECTION             &&
        mp2->type != LAT_LONG_PSEUDO_PROJECTION     &&
        mp2->type != UNKNOWN_PROJECTION)
    {
      sprintf(precheck_err_msgs,
              "%s%s%s%s    %s, or\n    %s, or\n    %s, or\n    %s, or\n   %s, or\n    %s, or\n"
              "    %s, or\n    %s, or\n    %s\n",
              precheck_err_msgs,
              "  [Projection] New version projection type (",
              (mp2->type == UNIVERSAL_TRANSVERSE_MERCATOR) ? "UNIVERSAL_TRANSVERSE_MERCATOR"  :
              (mp2->type == POLAR_STEREOGRAPHIC)           ? "POLAR_STEREOGRAPHIC"            :
              (mp2->type == ALBERS_EQUAL_AREA)             ? "ALBERS_EQUAL_AREA"              :
              (mp2->type == LAMBERT_CONFORMAL_CONIC)       ? "LAMBERT_CONFORMAL_CONIC"        :
              (mp2->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)  ? "LAMBERT_AZIMUTHAL_EQUAL_AREA"   :
              (mp2->type == STATE_PLANE)                   ? "STATE_PLANE"                    :
              (mp2->type == SCANSAR_PROJECTION)            ? "SCANSAR_PROJECTION"             :
              (mp2->type == LAT_LONG_PSEUDO_PROJECTION)    ? "LAT_LONG_PSEUDO_PROJECTION"     :
              (mp2->type == UNKNOWN_PROJECTION)            ? "UNKNOWN_PROJECTION"             :
              "Unknown type found",
              ") invalid.\n  Expected one of:\n",
              "UNIVERSAL_TRANSVERSE_MERCATOR",
              "POLAR_STEREOGRAPHIC",
              "ALBERS_EQUAL_AREA",
              "LAMBERT_CONFORMAL_CONIC",
              "LAMBERT_AZIMUTHAL_EQUAL_AREA",
              "STATE_PLANE",
              "SCANSAR_PROJECTION",
              "LAT_LONG_PSEUDO_PROJECTION",
              "UNKNOWN_PROJECTION");
      failed = 1;
    }

    verify_double(precheck_err_msgs, mp2->startX,
                  DM_MIN_STARTX, DM_MAX_STARTX,
                  "Projection", "startX",
                  1, &failed);

    verify_double(precheck_err_msgs, mp2->startY,
                  DM_MIN_STARTY, DM_MAX_STARTY,
                  "Projection", "startY",
                  1, &failed);

    verify_double(precheck_err_msgs, mp2->perX,
                  DM_MIN_PERX, DM_MAX_PERX,
                  "Projection", "perX",
                  1, &failed);

    verify_double(precheck_err_msgs, mp2->perY,
                  DM_MIN_PERY, DM_MAX_PERY,
                  "Projection", "perY",
                  1, &failed);

#   define NUM_UNITS_STRINGS 3
    char *units_strings[NUM_UNITS_STRINGS] =
      {"meters", "arcsec", "degrees"};
    verify_string(precheck_err_msgs, mp2->units,
                  units_strings, NUM_UNITS_STRINGS,
                  "Projection", "units",
                  1, &failed);

#   define NUM_HEM_CHARS 2
    char hem_chars[NUM_HEM_CHARS] =
      {'N', 'S'};
    verify_char(precheck_err_msgs, mp2->hem,
                hem_chars, NUM_HEM_CHARS,
                "Projection", "hem",
                1, &failed);

    if (mp2->spheroid != BESSEL_SPHEROID            &&
        mp2->spheroid != CLARKE1866_SPHEROID        &&
        mp2->spheroid != CLARKE1880_SPHEROID        &&
        mp2->spheroid != GEM6_SPHEROID              &&
        mp2->spheroid != GEM10C_SPHEROID            &&
        mp2->spheroid != GRS1980_SPHEROID           &&
        mp2->spheroid != INTERNATIONAL1924_SPHEROID &&
        mp2->spheroid != INTERNATIONAL1967_SPHEROID &&
        mp2->spheroid != WGS72_SPHEROID             &&
        mp2->spheroid != WGS84_SPHEROID             &&
        mp2->spheroid != HUGHES_SPHEROID            &&
        mp2->spheroid != UNKNOWN_SPHEROID)
    {
      sprintf(precheck_err_msgs, "%s%s\n",
              precheck_err_msgs,
              "  [Projection] New version spheroid invalid or unrecognized.\n    Expected one of:\n"
                  "    BESSEL, or \n"
                  "    CLARKE1866, or \n"
                  "    CLARKE1880, or \n"
                  "    GEM6, or \n"
                  "    GEM10C, or \n"
                  "    GRS1980, or \n"
                  "    INTERNATIONAL1924, or \n"
                  "    INTERNATIONAL1967, or \n"
                  "    WGS72, or \n"
                  "    WGS84, or \n"
                  "    HUGHES, or \n"
                  "    UNKNOWN (enum spheroid_type_t UNKNOWN_SPHEROID)\n");
      failed = 1;
    }

    verify_double(precheck_err_msgs, mp2->re_major,
                  DM_MIN_RE_MAJOR, DM_MAX_RE_MAJOR,
                  "Projection", "re_major",
                  1, &failed);

    verify_double(precheck_err_msgs, mp2->re_minor,
                  DM_MIN_RE_MINOR, DM_MAX_RE_MINOR,
                  "Projection", "re_minor",
                  1, &failed);

    if (mp2->datum != EGM96_DATUM &&
        mp2->datum != ED50_DATUM &&
        mp2->datum != ETRF89_DATUM &&
        mp2->datum != ETRS89_DATUM &&
        mp2->datum != ITRF97_DATUM &&
        mp2->datum != NAD27_DATUM &&
        mp2->datum != NAD83_DATUM &&
        mp2->datum != WGS72_DATUM &&
        mp2->datum != WGS84_DATUM &&
        mp2->datum != HUGHES_DATUM &&
        mp2->datum != UNKNOWN_DATUM)
    {
      sprintf(precheck_err_msgs, "%s%s\n",
              precheck_err_msgs,
              "  [Projection] New version datum invalid or unrecognized.\n    Expected one of:\n"
                  "    EGM96\n"
                  "    ED50\n"
                  "    ETRF89\n"
                  "    ETRS89\n"
                  "    ITRF97\n"
                  "    NAD27\n"
                  "    NAD83\n"
                  "    WGS72\n"
                  "    WGS84\n"
                  "    HUGHES\n"
                  "    UNKNOWN (enum datum_type_t UNKNOWN_DATUM)\n");
      failed = 1;
    }

    verify_double(precheck_err_msgs, mp2->height,
                  DM_MIN_HEIGHT, DM_MAX_HEIGHT,
                  "Projection", "height",
                  1, &failed);

    switch (mp2->type) {
      case UNIVERSAL_TRANSVERSE_MERCATOR:
        verify_int(precheck_err_msgs, mp2->param.utm.zone,
                   DM_MIN_UTM_ZONE, DM_MAX_UTM_ZONE,
                   "Projection - UTM", "zone",
                   1, &failed);

        verify_double(precheck_err_msgs, mp2->param.utm.false_easting,
                      DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                      "Projection - UTM", "false_easting",
                      0, &failed);

        if (meta_is_valid_double(mp2->param.utm.false_northing)      &&
            mp2->param.utm.false_northing != DM_N_UTM_FALSE_NORTHING &&
            mp2->param.utm.false_northing != DM_S_UTM_FALSE_NORTHING)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "  [Projection - UTM] New version false_northing out of range (",
                  mp2->param.utm.false_northing,
                  ").\n    Expected:\n      ",
                  DM_MIN_LATITUDE,
                  " through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }

        verify_double(precheck_err_msgs, mp2->param.utm.lat0,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - UTM", "lat0",
                      0, &failed);

        verify_double(precheck_err_msgs, mp2->param.utm.lon0,
                      DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                      "Projection - UTM", "lon0",
                      0, &failed);

        if (meta_is_valid_double(mp2->param.utm.scale_factor)   &&
            (mp2->param.utm.scale_factor != DM_UTM_SCALE_FACTOR &&
             mp2->param.utm.scale_factor != DM_DEFAULT_SCALE_FACTOR))
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "  [Projection - UTM] New version scale_factor out of range (",
                  mp2->param.utm.scale_factor,
                  ").\n    Expected:\n      ",
                  DM_UTM_SCALE_FACTOR,
                  " or ",
                  DM_DEFAULT_SCALE_FACTOR);
          failed = 1;
        }
        break;
      case POLAR_STEREOGRAPHIC:
        verify_double(precheck_err_msgs, mp2->param.ps.slat,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - PS", "slat",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.ps.slon,
                      DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                      "Projection - PS", "slon",
                      1, &failed);

        verify_int(precheck_err_msgs, mp2->param.ps.is_north_pole,
                   0, 1,
                   "Projection - PS", "is_north_pole",
                   1, &failed);

        verify_double(precheck_err_msgs, mp2->param.ps.false_easting,
                      DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                      "Projection - PS", "false_easting",
                      0, &failed);

        verify_double(precheck_err_msgs, mp2->param.ps.false_northing,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - PS", "false_northing",
                      0, &failed);
        break;
      case ALBERS_EQUAL_AREA:
        verify_double(precheck_err_msgs, mp2->param.albers.std_parallel1,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - ALBERS", "std_parallel1",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.albers.std_parallel2,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - ALBERS", "std_parallel2",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.albers.center_meridian,
                      DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                      "Projection - ALBERS", "center_meridian",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.albers.orig_latitude,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - ALBERS", "orig_latitude",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.albers.false_easting,
                      DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                      "Projection - ALBERS", "false_easting",
                      0, &failed);

        verify_double(precheck_err_msgs, mp2->param.albers.false_northing,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - ALBERS", "false_northing",
                      0, &failed);
        break;
      case LAMBERT_CONFORMAL_CONIC:
        verify_double(precheck_err_msgs, mp2->param.lamcc.plat1,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - LAMCC", "plat1",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.lamcc.plat2,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - LAMCC", "plat2",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.lamcc.lat0,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - LAMCC", "lat0",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.lamcc.lon0,
                      DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                      "Projection - LAMCC", "lon0",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.lamcc.false_easting,
                      DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                      "Projection - LAMCC", "false_easting",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.lamcc.false_northing,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - LAMCC", "false_northing",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.lamcc.scale_factor,
                      DM_MIN_LAMCC_SCALE_FACTOR, DM_MAX_LAMCC_SCALE_FACTOR,
                      "Projection - LAMCC", "scale_factor",
                      0, &failed);
        break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA:
        verify_double(precheck_err_msgs, mp2->param.lamaz.center_lon,
                      DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                      "Projection - LAMAZ", "center_lon",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.lamaz.center_lat,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - LAMAZ", "center_lat",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.lamaz.false_easting,
                      DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                      "Projection - LAMAZ", "false_easting",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.lamaz.false_northing,
                      DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                      "Projection - LAMAZ", "false_northing",
                      1, &failed);
        break;
      case STATE_PLANE:
        verify_int(precheck_err_msgs, mp2->param.state.zone,
                   DM_MIN_STATE_PLANE_ZONE, DM_MAX_STATE_PLANE_ZONE,
                   "Projection - STATE_PLANE", "zone",
                   1, &failed);
        break;
      case SCANSAR_PROJECTION:
        verify_double(precheck_err_msgs, mp2->param.atct.rlocal,
                      DM_MIN_EARTH_RADIUS, DM_MAX_EARTH_RADIUS,
                      "Projection - ATCT", "rlocal",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.atct.alpha1,
                      DM_MIN_ROTATION_ANGLE, DM_MAX_ROTATION_ANGLE,
                      "Projection - ATCT", "alpha1",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.atct.alpha2,
                      DM_MIN_ROTATION_ANGLE, DM_MAX_ROTATION_ANGLE,
                      "Projection - ATCT", "alpha2",
                      1, &failed);

        verify_double(precheck_err_msgs, mp2->param.atct.alpha3,
                      DM_MIN_ROTATION_ANGLE, DM_MAX_ROTATION_ANGLE,
                      "Projection - ATCT", "alpha3",
                      1, &failed);
        break;
      case LAT_LONG_PSEUDO_PROJECTION:
      case UNKNOWN_PROJECTION:
        break;
      default:
        sprintf(precheck_err_msgs, "%s%s\n",
                precheck_err_msgs,
                "  [Projection] Unexpected projection type found.");
        failed = 1;
    }
    // PROJECTION BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      report_validation_errors(outputFile, metafile2,
                               precheck_err_msgs, "PROJECTION");
    }
  }
  //
  // End of Projection Block Validity Check
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Check Stats Block(s)
  //
  if (mstats2) {
    failed = 0;
    strcpy(precheck_err_msgs, "");

    validate_double(precheck_err_msgs, mstats2->min,
                    "Stats", "min", &failed);

    validate_double(precheck_err_msgs, mstats2->max,
                    "Stats", "max", &failed);

    validate_double(precheck_err_msgs, mstats2->mean,
                    "Stats", "mean", &failed);

    // rmse ignored

    validate_double(precheck_err_msgs, mstats2->std_deviation,
                    "Stats", "std_deviation", &failed);

    // mask ignored
    // STATS BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      report_validation_errors(outputFile, metafile2,
                               precheck_err_msgs, "STATS");
    }
  }
  //
  // End of Stats Block Validity Check
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Check State Vector Block(s)
  //
  if (mstatev2) {
    failed = 0;
    strcpy(precheck_err_msgs, "");

    verify_int(precheck_err_msgs, mstatev2->year,
               DM_MIN_YEAR, DM_MAX_YEAR,
               "State Vector", "year",
               1, &failed);

    verify_int(precheck_err_msgs, mstatev2->julDay,
               DM_MIN_JULIAN_DAY, DM_MAX_JULIAN_DAY,
               "State Vector", "julDay",
               1, &failed);

    verify_double(precheck_err_msgs, mstatev2->second,
                  DM_MIN_SECOND, DM_MAX_SECOND,
                  "State Vector", "second",
                  1, &failed);

    int vector_count_valid=0;
    verify_int(precheck_err_msgs, mstatev2->vector_count,
               DM_MIN_VECTOR_COUNT, DM_MAX_VECTOR_COUNT,
               "State Vector", "vector_count",
               1, &failed);
    if (meta_is_valid_int(mstatev2->vector_count)     &&
        mstatev2->vector_count >= DM_MIN_VECTOR_COUNT &&
        mstatev2->vector_count <= DM_MAX_VECTOR_COUNT)
    {
      vector_count_valid = 1;
    }
    // num ignored
    if (vector_count_valid) {
      int i;
      state_loc *sv;
      vector *vp, *vv;
      char vector_id[64] = "";
      for (i=0; i<mstatev2->vector_count; i++) {
        sv = &mstatev2->vecs[i];
        vp = &mstatev2->vecs[i].vec.pos;
        vv = &mstatev2->vecs[i].vec.vel;
        sprintf(vector_id, "State Vector - Vector #%d", i);
        verify_double(precheck_err_msgs, sv->time,
                      DM_MIN_VEC_TIME, DM_MAX_VEC_TIME,
                      vector_id, "time",
                      1, &failed);

        verify_double(precheck_err_msgs, vp->x,
                      DM_MIN_ECR_COORD, DM_MAX_ECR_COORD,
                      vector_id, "x",
                      1, &failed);

        verify_double(precheck_err_msgs, vp->y,
                      DM_MIN_ECR_COORD, DM_MAX_ECR_COORD,
                      vector_id, "y",
                      1, &failed);

        verify_double(precheck_err_msgs, vp->z,
                      DM_MIN_ECR_COORD, DM_MAX_ECR_COORD,
                      vector_id, "z",
                      1, &failed);

        verify_double(precheck_err_msgs, vv->x,
                      DM_MIN_ECR_COORD, DM_MAX_ECR_COORD,
                      vector_id, "vx",
                      1, &failed);

        verify_double(precheck_err_msgs, vv->y,
                      DM_MIN_ECR_COORD, DM_MAX_ECR_COORD,
                      vector_id, "vy",
                      1, &failed);

        verify_double(precheck_err_msgs, vv->z,
                      DM_MIN_ECR_COORD, DM_MAX_ECR_COORD,
                      vector_id, "vz",
                      1, &failed);

      }
    }
    // STATE VECTOR BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      report_validation_errors(outputFile, metafile2,
                               precheck_err_msgs, "STATE VECTOR");
    }
  }
  //
  // End of State Vectors Block Validity Check
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Check Location Block
  //
  if (mloc2) {
    failed = 0;
    strcpy(precheck_err_msgs, "");

    verify_double(precheck_err_msgs, mloc2->lat_start_near_range,
                  DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                  "Location", "lat_start_near_range",
                  1, &failed);

    verify_double(precheck_err_msgs, mloc2->lon_start_near_range,
                  DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                  "Location", "lon_start_near_range",
                  1, &failed);

    verify_double(precheck_err_msgs, mloc2->lat_start_far_range,
                  DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                  "Location", "lat_start_far_range",
                  1, &failed);

    verify_double(precheck_err_msgs, mloc2->lon_start_far_range,
                  DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                  "Location", "lon_start_far_range",
                  1, &failed);

    verify_double(precheck_err_msgs, mloc2->lat_end_near_range,
                  DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                  "Location", "lat_end_near_range",
                  1, &failed);

    verify_double(precheck_err_msgs, mloc2->lon_end_near_range,
                  DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                  "Location", "lon_end_near_range",
                  1, &failed);

    verify_double(precheck_err_msgs, mloc2->lat_end_far_range,
                  DM_MIN_LATITUDE, DM_MAX_LATITUDE,
                  "Location", "lat_end_far_range",
                  1, &failed);

    verify_double(precheck_err_msgs, mloc2->lon_end_far_range,
                  DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                  "Location", "lon_end_far_range",
                  1, &failed);
    // LOCATION BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      report_validation_errors(outputFile, metafile2,
                               precheck_err_msgs, "LOCATION");
    }
  }
  //
  // End of Location Block Validity Check
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  //  END PRECHECK                                          //
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  //  BASELINE COMPARISON                                   //
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Compare General Blocks
  failed = 0;
  strcpy(compare_err_msgs, "");
  compare_meta_string(compare_err_msgs, "General", "basename",
                      mg1->basename, mg2->basename, &failed);
  compare_meta_string(compare_err_msgs, "General", "sensor",
                      mg1->sensor, mg2->sensor, &failed);
  compare_meta_string(compare_err_msgs, "General", "sensor_name",
                      mg1->sensor_name, mg2->sensor_name, &failed);
  compare_meta_string(compare_err_msgs, "General", "mode",
                      mg1->mode, mg2->mode, &failed);
  compare_meta_string(compare_err_msgs, "General", "processor",
                      mg1->processor, mg2->processor, &failed);
  compare_meta_string(compare_err_msgs, "General", "system",
                      mg1->system, mg2->system, &failed);
  compare_meta_string(compare_err_msgs, "General", "acquisition_date",
                      mg1->acquisition_date, mg2->acquisition_date, &failed);
  compare_meta_enum(compare_err_msgs, "General", "data_type",
                    mg1->data_type, mg2->data_type,
                    data_type2str, &failed);
  compare_meta_enum(compare_err_msgs, "General", "image_data_type",
                    mg1->image_data_type, mg2->image_data_type,
                    image_data_type2str, &failed);
  compare_meta_int(compare_err_msgs, "General", "orbit",
                   mg1->orbit, mg2->orbit, &failed);
  compare_meta_char(compare_err_msgs, "General", "orbit_direction",
                   mg1->orbit_direction, mg2->orbit_direction, &failed);
  compare_meta_int(compare_err_msgs, "General", "frame",
                   mg1->frame, mg2->frame, &failed);
  compare_meta_int(compare_err_msgs, "General", "band_count",
                   mg1->band_count, mg2->band_count, &failed);
  compare_meta_int(compare_err_msgs, "General", "line_count",
                   mg1->line_count, mg2->line_count, &failed);
  compare_meta_int(compare_err_msgs, "General", "sample_count",
                   mg1->sample_count, mg2->sample_count, &failed);
  compare_meta_int(compare_err_msgs, "General", "start_line",
                   mg1->start_line, mg2->start_line, &failed);
  compare_meta_int(compare_err_msgs, "General", "start_sample",
                   mg1->start_sample, mg2->start_sample, &failed);
  compare_meta_double_with_tolerance(compare_err_msgs, "General", "x_pixel_size",
                   mg1->x_pixel_size, mg2->x_pixel_size,
                   DM_PIXEL_SIZE_M_TOL, &failed);
  compare_meta_double_with_tolerance(compare_err_msgs, "General", "y_pixel_size",
                   mg1->y_pixel_size, mg2->y_pixel_size,
                   DM_PIXEL_SIZE_M_TOL, &failed);
  ////////////////////////////////////////////////////////////
  // General Block Reporting
  if (failed) {
    report_difference_errors(outputFile,
                             metafile1, metafile2,
                             compare_err_msgs, "GENERAL");
  }
  // End Compare General Blocks
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Compare SAR Blocks
  failed = 0;
  strcpy(compare_err_msgs, "");

  if (msar1 && msar2) {
    compare_meta_int(compare_err_msgs, "SAR", "image_type",
                    msar1->image_type, msar2->image_type, &failed);
    compare_meta_char(compare_err_msgs, "SAR", "look_direction",
                    msar1->look_direction, msar2->look_direction, &failed);
    compare_meta_int(compare_err_msgs, "SAR", "look_count",
                    msar1->look_count, msar2->look_count, &failed);
    compare_meta_int(compare_err_msgs, "SAR", "deskewed",
                    msar1->deskewed, msar2->deskewed, &failed);
    compare_meta_int(compare_err_msgs, "SAR", "original_line_count",
                    msar1->original_line_count, msar2->original_line_count, &failed);
    compare_meta_int(compare_err_msgs, "SAR", "original_sample_count",
                    msar1->original_sample_count, msar2->original_sample_count, &failed);
    compare_meta_int(compare_err_msgs, "SAR", "line_increment",
                    msar1->line_increment, msar2->line_increment, &failed);
    compare_meta_int(compare_err_msgs, "SAR", "sample_increment",
                    msar1->sample_increment, msar2->sample_increment, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "range_time_per_pixel",
                        msar1->range_time_per_pixel, msar2->range_time_per_pixel,
                        DM_RANGE_TIME_PER_PIXEL_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "azimuth_time_per_pixel",
                        msar1->azimuth_time_per_pixel, msar2->azimuth_time_per_pixel,
                        DM_RANGE_TIME_PER_PIXEL_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "slant_shift",
                        msar1->slant_shift, msar2->slant_shift,
                        DM_SLANT_SHIFT_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "time_shift",
                        msar1->time_shift, msar2->time_shift,
                        DM_TIME_SHIFT_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "slant_range_first_pixel",
                        msar1->slant_range_first_pixel, msar2->slant_range_first_pixel,
                        DM_SLANT_RANGE_FIRST_PIXEL_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "wavelength",
                        msar1->wavelength, msar2->wavelength,
                        DM_WAVELENGTH_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "prf",
                        msar1->prf, msar2->prf,
                        DM_PRF_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "earth_radius",
                        msar1->earth_radius, msar2->earth_radius,
                        DM_EARTH_RADIUS_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "earth_radius_pp",
                        msar1->earth_radius_pp, msar2->earth_radius_pp,
                        DM_EARTH_RADIUS_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "satellite_height",
                        msar1->satellite_height, msar2->satellite_height,
                        DM_SATELLITE_HEIGHT_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "range_doppler_coefficients[0]",
                        msar1->range_doppler_coefficients[0],
                        msar2->range_doppler_coefficients[0],
                        DM_DOP_RANGE_CENTROID_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "range_doppler_coefficients[1]",
                        msar1->range_doppler_coefficients[1],
                        msar2->range_doppler_coefficients[1],
                        DM_DOP_RANGE_PER_PIXEL_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "range_doppler_coefficients[2]",
                        msar1->range_doppler_coefficients[2],
                        msar2->range_doppler_coefficients[2],
                        DM_DOP_RANGE_QUAD_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "azimuth_doppler_coefficients[0]",
                        msar1->azimuth_doppler_coefficients[0],
                        msar2->azimuth_doppler_coefficients[0],
                        DM_DOP_AZIMUTH_CENTROID_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "azimuth_doppler_coefficients[1]",
                        msar1->azimuth_doppler_coefficients[1],
                        msar2->azimuth_doppler_coefficients[1],
                        DM_DOP_AZIMUTH_PER_PIXEL_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "SAR", "azimuth_doppler_coefficients[2]",
                        msar1->azimuth_doppler_coefficients[2],
                        msar2->azimuth_doppler_coefficients[2],
                        DM_DOP_AZIMUTH_QUAD_TOL, &failed);
  }
  ////////////////////////////////////////////////////////////
  // SAR Block Reporting
  if (failed) {
    report_difference_errors(outputFile,
                             metafile1, metafile2,
                             compare_err_msgs, "SAR");
  }
  // End Compare SAR Blocks
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Compare Optical Blocks
  failed = 0;
  strcpy(compare_err_msgs, "");
  if (mo1 && mo2) {
    compare_meta_string(compare_err_msgs, "Optical", "pointing_direction",
                        mo1->pointing_direction, mo2->pointing_direction, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Optical", "off_nadir_angle",
                        mo1->off_nadir_angle, mo2->off_nadir_angle,
                        DM_OFF_NADIR_ANGLE_TOL, &failed);
    compare_meta_string(compare_err_msgs, "Optical", "correction_level",
                        mo1->correction_level, mo2->correction_level, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Optical", "cloud_percentage",
                        mo1->cloud_percentage, mo2->cloud_percentage,
                        DM_CLOUD_PERCENTAGE_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Optical", "sun_azimuth_angle",
                        mo1->sun_azimuth_angle, mo2->sun_azimuth_angle,
                        DM_SUN_AZIMUTH_ANGLE_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Optical", "sun_elevation_angle",
                        mo1->sun_elevation_angle, mo2->sun_elevation_angle,
                        DM_SUN_ELEVATION_ANGLE_TOL, &failed);
  }
  ////////////////////////////////////////////////////////////
  // Optical Block Reporting
  if (failed) {
    report_difference_errors(outputFile,
                             metafile1, metafile2,
                             compare_err_msgs, "OPTICAL");
  }
  // End Compare Optical Blocks
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Compare Thermal Blocks
  failed = 0;
  strcpy(compare_err_msgs, "");
  if (mtherm1 && mtherm2) {
    compare_meta_double_with_tolerance(compare_err_msgs, "Thermal", "band_gain",
                                       mtherm1->band_gain, mtherm2->band_gain,
                                       DM_BAND_GAIN_TOL, &failed);

    compare_meta_double_with_tolerance(compare_err_msgs, "Thermal", "band_gain_change",
                                       mtherm1->band_gain_change, mtherm2->band_gain_change,
                                       DM_BAND_GAIN_CHANGE_TOL, &failed);

    compare_meta_int(compare_err_msgs, "Thermal", "day",
                     mtherm1->day, mtherm2->day, &failed);
  }
  ////////////////////////////////////////////////////////////
  // Thermal Block Reporting
  if (failed) {
    report_difference_errors(outputFile,
                             metafile1, metafile2,
                             compare_err_msgs, "THERMAL");
  }
  // End Compare Thermal Blocks
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Compare Transform Blocks
  failed = 0;
  strcpy(compare_err_msgs, "");
  if (mtrans1 && mtrans2) {
    compare_meta_int(compare_err_msgs, "Transform", "parameter_count",
                     mtrans1->parameter_count, mtrans2->parameter_count, &failed);
    //FIXME: parameter_count is always 4 ...but fix the following to use a loop anyway
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - Phi(0)", "y[0]",
                                       mtrans1->y[0], mtrans2->y[0],
                                       DM_PHI0_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - Phi(1)", "y[1]",
                                       mtrans1->y[1], mtrans2->y[1],
                                       DM_PHI1_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - Phi(2)", "y[2]",
                                       mtrans1->y[2], mtrans2->y[2],
                                       DM_PHI2_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - Phi(3)", "y[3]",
                                       mtrans1->y[3], mtrans2->y[3],
                                       DM_PHI3_TOL, &failed);

    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - Lambda(0)", "x[0]",
                                       mtrans1->x[0], mtrans2->x[0],
                                       DM_LAMBDA0_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - Lambda(1)", "x[1]",
                                       mtrans1->x[1], mtrans2->x[1],
                                       DM_LAMBDA1_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - Lambda(2)", "x[2]",
                                       mtrans1->x[2], mtrans2->x[2],
                                       DM_LAMBDA2_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - Lambda(3)", "x[3]",
                                       mtrans1->x[3], mtrans2->x[3],
                                       DM_LAMBDA3_TOL, &failed);

    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - i(0)", "s[0]",
                                       mtrans1->s[0], mtrans2->s[0],
                                       DM_I0_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - i(1)", "s[1]",
                                       mtrans1->s[1], mtrans2->s[1],
                                       DM_I1_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - i(2)", "s[2]",
                                       mtrans1->s[2], mtrans2->s[2],
                                       DM_I2_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - i(3)", "s[3]",
                                       mtrans1->s[3], mtrans2->s[3],
                                       DM_I3_TOL, &failed);

    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - j(0)", "l[0]",
                                       mtrans1->l[0], mtrans2->l[0],
                                       DM_J0_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - j(1)", "l[1]",
                                       mtrans1->l[1], mtrans2->l[1],
                                       DM_J1_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - j(2)", "l[2]",
                                       mtrans1->l[2], mtrans2->l[2],
                                       DM_J2_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Transform - j(3)", "l[3]",
                                       mtrans1->l[3], mtrans2->l[3],
                                       DM_J3_TOL, &failed);
  }
  ////////////////////////////////////////////////////////////
  // Transform Block Reporting
  if (failed) {
    report_difference_errors(outputFile,
                             metafile1, metafile2,
                             compare_err_msgs, "TRANSFORM");
  }
  // End Compare Transform Blocks
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Compare Projection Blocks
  failed = 0;
  strcpy(compare_err_msgs, "");
  if (mp1 && mp2) {
    compare_meta_int(compare_err_msgs, "Projection", "type",
                     mp1->type, mp2->type, &failed);

    compare_meta_double_with_tolerance(compare_err_msgs, "Projection", "startX",
                                       mp1->startX, mp2->startX,
                                       DM_STARTX_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Projection", "startY",
                                       mp1->startY, mp2->startY,
                                       DM_STARTY_TOL, &failed);

    compare_meta_double_with_tolerance(compare_err_msgs, "Projection", "startX",
                                       mp1->startX, mp2->startX,
                                       DM_PERX_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Projection", "startY",
                                       mp1->startY, mp2->startY,
                                       DM_PERY_TOL, &failed);

    compare_meta_string(compare_err_msgs, "Projection", "units",
                        mp1->units, mp2->units, &failed);

    compare_meta_char(compare_err_msgs, "Projection", "hem",
                      mp1->hem, mp2->hem, &failed);

    compare_meta_int(compare_err_msgs, "Projection", "spheroid",
                     mp1->spheroid, mp2->spheroid, &failed);

    compare_meta_double_with_tolerance(compare_err_msgs, "Projection", "re_major",
                                       mp1->re_major, mp2->re_major,
                                       DM_EARTH_RADIUS_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Projection", "re_minor",
                                       mp1->re_minor, mp2->re_minor,
                                       DM_EARTH_RADIUS_TOL, &failed);

    compare_meta_int(compare_err_msgs, "Projection", "datum",
                     mp1->datum, mp2->datum, &failed);

    compare_meta_double_with_tolerance(compare_err_msgs, "Projection", "height",
                                       mp1->height, mp2->height,
                                       DM_HEIGHT_TOL, &failed);

    if (mp1->type == mp2->type &&
        (mp2->type == UNIVERSAL_TRANSVERSE_MERCATOR  ||
         mp2->type == POLAR_STEREOGRAPHIC            ||
         mp2->type == ALBERS_EQUAL_AREA              ||
         mp2->type == LAMBERT_CONFORMAL_CONIC        ||
         mp2->type == LAMBERT_AZIMUTHAL_EQUAL_AREA   ||
         mp2->type == STATE_PLANE                    ||
         mp2->type == SCANSAR_PROJECTION             ||
         mp2->type == LAT_LONG_PSEUDO_PROJECTION     ||
         mp2->type == UNKNOWN_PROJECTION))
    {
      // Can't be here if both types aren't the same, so OK to just switch on mp2->type
      switch(mp2->type) {
        case UNIVERSAL_TRANSVERSE_MERCATOR:
          compare_meta_int(compare_err_msgs, "Projection - UTM", "zone",
                           mp1->param.utm.zone, mp2->param.utm.zone, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - UTM", "false_easting",
                                             mp1->param.utm.false_easting, mp2->param.utm.false_easting,
                                             DM_LONGITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - UTM", "false_northing",
                                             mp1->param.utm.false_northing, mp2->param.utm.false_northing,
                                             DM_LATITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - UTM", "lat0",
                                             mp1->param.utm.lat0, mp2->param.utm.lat0,
                                             DM_LATITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - UTM", "lon0",
                                             mp1->param.utm.lon0, mp2->param.utm.lon0,
                                             DM_LONGITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - UTM", "scale_factor",
                                             mp1->param.utm.scale_factor, mp2->param.utm.scale_factor,
                                             DM_SCALE_FACTOR_TOL, &failed);
          break;
        case POLAR_STEREOGRAPHIC:
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - PS", "slat",
                                             mp1->param.ps.slat, mp2->param.ps.slat,
                                             DM_LATITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - PS", "slon",
                                             mp1->param.ps.slon, mp2->param.ps.slon,
                                             DM_LONGITUDE_TOL, &failed);
          compare_meta_int(compare_err_msgs, "Projection - PS", "is_north_pole",
                           mp1->param.ps.is_north_pole, mp2->param.ps.is_north_pole, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - PS", "false_easting",
                                             mp1->param.ps.false_easting, mp2->param.ps.false_easting,
                                             DM_LONGITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - PS", "false_northing",
                                             mp1->param.ps.false_northing, mp2->param.ps.false_northing,
                                             DM_LATITUDE_TOL, &failed);
          break;
        case ALBERS_EQUAL_AREA:
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - ALBERS", "std_parallel1",
                                             mp1->param.albers.std_parallel1, mp2->param.albers.std_parallel1,
                                             DM_LATITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - ALBERS", "std_parallel2",
                                             mp1->param.albers.std_parallel2, mp2->param.albers.std_parallel2,
                                             DM_LATITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - ALBERS", "center_meridian",
                                             mp1->param.albers.center_meridian, mp2->param.albers.center_meridian,
                                             DM_LONGITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - ALBERS", "orig_latitude",
                                             mp1->param.albers.orig_latitude, mp2->param.albers.orig_latitude,
                                             DM_LATITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - ALBERS", "false_easting",
                                             mp1->param.albers.false_easting, mp2->param.albers.false_easting,
                                             DM_LONGITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - ALBERS", "false_northing",
                                             mp1->param.albers.false_northing, mp2->param.albers.false_northing,
                                             DM_LATITUDE_TOL, &failed);
          break;
        case LAMBERT_CONFORMAL_CONIC:
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMCC", "plat1",
                                             mp1->param.lamcc.plat1, mp2->param.lamcc.plat1,
                                             DM_LATITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMCC", "plat2",
                                             mp1->param.lamcc.plat2, mp2->param.lamcc.plat2,
                                             DM_LATITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMCC", "lat0",
                                             mp1->param.lamcc.lat0, mp2->param.lamcc.lat0,
                                             DM_LATITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMCC", "lon0",
                                             mp1->param.lamcc.lon0, mp2->param.lamcc.lon0,
                                             DM_LONGITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMCC", "false_easting",
                                             mp1->param.lamcc.false_easting, mp2->param.lamcc.false_easting,
                                             DM_LONGITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMCC", "false_northing",
                                             mp1->param.lamcc.false_northing, mp2->param.lamcc.false_northing,
                                             DM_LATITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMCC", "scale_factor",
                                             mp1->param.lamcc.scale_factor, mp2->param.lamcc.scale_factor,
                                             DM_SCALE_FACTOR_TOL, &failed);
          break;
        case LAMBERT_AZIMUTHAL_EQUAL_AREA:
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMAZ", "center_lon",
                                             mp1->param.lamaz.center_lon, mp2->param.lamaz.center_lon,
                                             DM_LONGITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMAZ", "center_lat",
                                             mp1->param.lamaz.center_lat, mp2->param.lamaz.center_lat,
                                             DM_LATITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMAZ", "false_easting",
                                             mp1->param.lamaz.false_easting, mp2->param.lamaz.false_easting,
                                             DM_LONGITUDE_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMAZ", "false_northing",
                                             mp1->param.lamaz.false_northing, mp2->param.lamaz.false_northing,
                                             DM_LATITUDE_TOL, &failed);
          break;
        case STATE_PLANE:
          compare_meta_int(compare_err_msgs, "Projection - STATE_PLANE", "zone",
                           mp1->param.state.zone, mp2->param.state.zone, &failed);
          break;
        case SCANSAR_PROJECTION:
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - ATCT (Scansar)", "rlocal",
                                             mp1->param.atct.rlocal, mp2->param.atct.rlocal,
                                             DM_EARTH_RADIUS_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - ATCT (Scansar)", "alpha1",
                                             mp1->param.atct.alpha1, mp2->param.atct.alpha1,
                                             DM_ALPHA_ROTATION_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - ATCT (Scansar)", "alpha2",
                                             mp1->param.atct.alpha2, mp2->param.atct.alpha2,
                                             DM_ALPHA_ROTATION_TOL, &failed);
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - ATCT (Scansar)", "alpha3",
                                             mp1->param.atct.alpha3, mp2->param.atct.alpha3,
                                             DM_ALPHA_ROTATION_TOL, &failed);
          break;
        case LAT_LONG_PSEUDO_PROJECTION:
        case UNKNOWN_PROJECTION:
        default:
          break;
      }
    }
  }

  ////////////////////////////////////////////////////////////
  // Projection Block Reporting
  if (failed) {
    report_difference_errors(outputFile,
                             metafile1, metafile2,
                             compare_err_msgs, "PROJECTION");
  }
  // End Compare Projection Blocks
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Compare Stats Blocks
  failed = 0;
  strcpy(compare_err_msgs, "");
  if (mstats1 && mstats2) {
    compare_meta_double_with_tolerance(compare_err_msgs, "Stats", "min",
                                       mstats1->min, mstats2->min,
                                       DM_STATS_MIN_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Stats", "max",
                                       mstats1->max, mstats2->max,
                                       DM_STATS_MAX_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Stats", "mean",
                                       mstats1->mean, mstats2->mean,
                                       DM_STATS_MEAN_TOL, &failed);
    // rmse is ignored
    compare_meta_double_with_tolerance(compare_err_msgs, "Stats", "std_deviation",
                                       mstats1->std_deviation, mstats2->std_deviation,
                                       DM_STATS_STD_DEVIATION_TOL, &failed);
    // mask is ignored
  }
  ////////////////////////////////////////////////////////////
  // Stats Block Reporting
  if (failed) {
    report_difference_errors(outputFile,
                             metafile1, metafile2,
                             compare_err_msgs, "STATS");
  }
  // End Compare Stats Blocks
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Compare State Vector Blocks
  failed = 0;
  strcpy(compare_err_msgs, "");
  if (mstatev1 && mstatev2) {
    compare_meta_int(compare_err_msgs, "State Vector", "year",
                     mstatev1->year, mstatev2->year,
                     &failed);
    compare_meta_int(compare_err_msgs, "State Vector", "julDay",
                     mstatev1->julDay, mstatev2->julDay,
                     &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "State Vector", "second",
                                       mstatev1->second, mstatev2->second,
                                       DM_SECONDS_TOL, &failed);
    compare_meta_int(compare_err_msgs, "State Vector", "vector_count",
                     mstatev1->vector_count, mstatev2->vector_count,
                     &failed);
    if (meta_is_valid_int(mstatev1->vector_count)        &&
        meta_is_valid_int(mstatev2->vector_count)        &&
        mstatev1->vector_count == mstatev2->vector_count &&
        mstatev2->vector_count > 0)
    {
      int i;
      state_loc *sv1, *sv2;
      vector *vp1, *vp2, *vv1, *vv2;
      char vector_id[64] = "";
      for (i=0; i<mstatev2->vector_count; i++) {
        sv1 = &mstatev1->vecs[i];
        vp1 = &mstatev1->vecs[i].vec.pos;
        vv1 = &mstatev1->vecs[i].vec.vel;
        sv2 = &mstatev2->vecs[i];
        vp2 = &mstatev2->vecs[i].vec.pos;
        vv2 = &mstatev2->vecs[i].vec.vel;
        sprintf(vector_id, "State Vector - Vector #%d", i);
        compare_meta_double_with_tolerance(compare_err_msgs, vector_id, "time",
                                           sv1->time, sv2->time,
                                           DM_SECONDS_TOL, &failed);
        compare_meta_double_with_tolerance(compare_err_msgs, vector_id, "x",
                                           vp1->x, vp2->x,
                                           DM_XYZ_TOL, &failed);
        compare_meta_double_with_tolerance(compare_err_msgs, vector_id, "y",
                                           vp1->y, vp2->y,
                                           DM_XYZ_TOL, &failed);
        compare_meta_double_with_tolerance(compare_err_msgs, vector_id, "z",
                                           vp1->z, vp2->z,
                                           DM_XYZ_TOL, &failed);
        compare_meta_double_with_tolerance(compare_err_msgs, vector_id, "vx",
                                           vv1->x, vv2->x,
                                           DM_XYZ_VEL_TOL, &failed);
        compare_meta_double_with_tolerance(compare_err_msgs, vector_id, "vy",
                                           vv1->y, vv2->y,
                                           DM_XYZ_VEL_TOL, &failed);
        compare_meta_double_with_tolerance(compare_err_msgs, vector_id, "vz",
                                           vv1->z, vv2->z,
                                           DM_XYZ_VEL_TOL, &failed);
      }
    }
  }
  ////////////////////////////////////////////////////////////
  // State Vector Block Reporting
  if (failed) {
    report_difference_errors(outputFile,
                             metafile1, metafile2,
                             compare_err_msgs, "STATE_VECTOR");
  }
  // End Compare State Vector Blocks
  ////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////
  // Compare Location Blocks
  failed = 0;
  strcpy(compare_err_msgs, "");
  if (mloc1 && mloc2) {
    compare_meta_double_with_tolerance(compare_err_msgs, "Location", "lat_start_near_range",
                                       mloc1->lat_start_near_range, mloc2->lat_start_near_range,
                                       DM_LATITUDE_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Location", "lon_start_near_range",
                                       mloc1->lon_start_near_range, mloc2->lon_start_near_range,
                                       DM_LONGITUDE_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Location", "lat_start_far_range",
                                       mloc1->lat_start_far_range, mloc2->lat_start_far_range,
                                       DM_LATITUDE_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Location", "lon_start_far_range",
                                       mloc1->lon_start_far_range, mloc2->lon_start_far_range,
                                       DM_LONGITUDE_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Location", "lat_end_near_range",
                                       mloc1->lat_end_near_range, mloc2->lat_end_near_range,
                                       DM_LATITUDE_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Location", "lon_end_near_range",
                                       mloc1->lon_end_near_range, mloc2->lon_end_near_range,
                                       DM_LONGITUDE_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Location", "lat_end_far_range",
                                       mloc1->lat_end_far_range, mloc2->lat_end_far_range,
                                       DM_LATITUDE_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Location", "lon_end_far_range",
                                       mloc1->lon_end_far_range, mloc2->lon_end_far_range,
                                       DM_LONGITUDE_TOL, &failed);
  }
  ////////////////////////////////////////////////////////////
  // Location Block Reporting
  if (failed) {
    report_difference_errors(outputFile,
                             metafile1, metafile2,
                             compare_err_msgs, "LOCATION");
  }
  // End Compare Location Blocks
  ////////////////////////////////////////////////////////////

} // End diff_check_metadata

