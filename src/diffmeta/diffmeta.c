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

/* Turning this on, since we read a lot of blocks that aren't actually tested
   yet.  Want to keep the read code until we get the test code going.  This
   enables us to use -Werror.  This warning appears new as in gcc 4.6
*/
#if __GNUC__ > 4 || \
              (__GNUC__ == 4 && (__GNUC_MINOR__ >= 6 ) )
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif

/**** MACRO DEFINITIONS ****/
#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)

/**** PROTOTYPES ****/
void usage(char *name);
int  is_geocentric(meta_parameters *meta);
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
void verify_double_with_mask(char *err_msgs, double num, double mask,
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
static char* data_type2str_w(int data_type);
static char* image_data_type2str_w(int image_data_type);

int main(int argc, char **argv)
{
  char *inFile1=NULL, *inFile2=NULL;
  char *metafile1, *metafile2;
  char *outputFile;
  extern int optind;            /* argv index of the next argument */
  extern char *optarg;          /* current argv[] */
  int c;                        /* option letter from getopt() */
  extern FILE *fLog;            /* output file descriptor, stdout or log file */
  extern int logflag, quietflag;
  int formatflag;
  int is_not_a_geotiff=1; // GeoTIFFs don't require the same fields
  char format[64];
  int outputflag=0;
  char msg[1024];

  fLog=NULL;
  logflag=quietflag=0;
  outputFile=(char*)CALLOC(1024, sizeof(char));

  /* process command line */
  while ((c=getopt(argc,argv,"o:l:f:")) != EOF)
  {
    switch (c) {
      case 'f':/* -format <format type> */
        if (0==strncmp(optarg,"ormat",5)) {
          sscanf(argv[optind++], "%s", format);
          formatflag=1;
        }
        else {
          FREE(outputFile);
          usage(argv[0]);
        }
        break;
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
          strcpy(outputFile, "");
          outputflag=0;
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
  }
  if (!outputflag) {
    sprintf(msg, "Missing output file name ...file differences will be directed to stderr (only)\n");
    printf("** Warning: ********\n%s** End of warning **\n\n", msg);
  }
  if (outputflag && strcmp(logFile, outputFile) == 0) {
    sprintf(msg, "Log file cannot be the same as the output file:\n     Log file: %s\n  Output file: %s\n",
            logFile, outputFile);
    if (outputFile) FREE(outputFile);
    fprintf(stderr, "** Error: ********\n%s** End of error **\n", msg);
    usage(argv[0]);
  }

  // Check for diff against self
  char *basename1, *basename2;
  basename1 = get_basename(inFile1);
  basename2 = get_basename(inFile2);
  if (strcmp(inFile1, inFile2)     == 0 ||
      strcmp(basename1, basename2) == 0)
  {
    asfPrintStatus("\nInput metadata files are the same - PASS.\n\n");
    exit(0);
  }

  // Look for metadata files ..be forgiving of name goofs
  metafile1 = appendExt(inFile1, ".meta");
  metafile2 = appendExt(inFile2, ".meta");
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

  // Create blank output file if necessary
  if (outputflag) {
    FILE *outFP=(FILE*)FOPEN(outputFile, "w");
    if(outFP == NULL) {
        asfPrintError("Cannot open output file for write:\n%s\n", outputFile);
    }
    else {
        FCLOSE(outFP);
    }
  }

  if (formatflag && strncmp(uc(format), "GEOTIFF", 7) == 0) {
    is_not_a_geotiff=0; // Input file is a geotiff
  }
  else {
    is_not_a_geotiff=1; // It's not a geotiff format input file
  }
  meta_parameters *meta2 = meta_read(metafile2);
  if (meta2 &&
      strlen(meta2->general->basename) &&
      strlen(meta2->general->sensor) &&
      (strstr(uc(meta2->general->basename), ".TIF") ||
       strncmp(meta2->general->sensor, "USGS", 4) == 0)
     )
  {
    is_not_a_geotiff = 0; // Input file is a geotiff
  }
  if (meta2 &&
      strlen(meta2->general->sensor_name) &&
      strlen(meta2->general->basename) &&
      !strstr(uc(meta2->general->basename), ".TIF") &&
      strncmp_case(meta2->general->sensor_name, "SAR", 3) == 0
     )
  {
    is_not_a_geotiff = 1; // Input file is not a geotiff
  }
  meta_free(meta2);

  /***** And away we go.... *****/
  diff_check_metadata(outputFile, is_not_a_geotiff, metafile1, metafile2);

  // Cleanup
  if (outputFile) FREE (outputFile);
  if (metafile1) FREE (metafile1);
  if (metafile2) FREE (metafile2);
  if (basename1) FREE (basename1);
  if (basename2) FREE (basename2);
  return (0);
}

void usage(char *name)
{
  printf("\nUSAGE:\n"
         "   %s [-output <diff_output_file>] [-format <type>] [-log <file>] <metafile1> <metafile2>\n"
         "\nOPTIONS:\n"
      "   -output <diff_output_file>:  output to write metadata differencing\n"
      "                 results to (required.)\n"
      "   -log <file>:  allows the output to be written to a log file\n"
      "                 in addition to stdout (not required but strongly suggested.)\n"
      "   -format <format>: allows modified checking of the metadata based on what\n"
      "                 type the imported data was.  Example: Importing a GeoTIFF\n"
      "                 results in unknown sensor and mode parameters (etc) and should\n"
      "                 not be required.  Currently, only geotiff is supported for the\n"
      "                 format flag:\n\n"
      "                   diffmeta -format geotiff -output <report file> <metafile1> <metafile2>\n"
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
      "Version:\n   " SVN_REV " (part of " TOOL_SUITE_NAME " "MAPREADY_VERSION_STRING ")\n\n",
      name);
  exit(1);
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
  int out_file_exists = 0;
  char msg[1024];
  FILE *outFP=NULL;

  if (outputFile && strlen(outputFile)) {
      outFP = (FILE*)FOPEN(outputFile, "a");
      if (!outFP) {
          outFP = stderr;
      }
      else {
          out_file_exists = 1;
      }
  }
  else {
      outFP = stderr;
  }
  if (out_file_exists) {
      fprintf(outFP, "\n-----------------------------------------------\n");

      sprintf(msg, "FAIL: Validation Checking of \n  %s\n\n", file);
      fprintf(outFP, "%s", msg);
      sprintf(msg, "%s Block Errors:\n\n", block_id);
      fprintf(outFP, "%s", msg);

      fprintf(outFP, "%s", err_msg);

      fprintf(outFP, "-----------------------------------------------\n\n");
  }
  fprintf(stderr,"\n-----------------------------------------------\n");

  sprintf(msg, "FAIL: Validation Checking of \n  %s\n\n", file);
  fprintf(stderr,"%s",msg);
  sprintf(msg, "%s Block Errors:\n\n", block_id);
  fprintf(stderr,"%s",msg);

  fprintf(stderr,"%s",err_msg);

  fprintf(stderr,"-----------------------------------------------\n\n");
  if(outputFile && strlen(outputFile) && outFP) FCLOSE(outFP);
}

void report_difference_errors(char *outputFile,
                              char *file1, char *file2,
                              char *err_msg, char *block_id)
{
    int out_file_exists = 0;
    char msg[1024];
  FILE *outFP=NULL;

  if (outputFile && strlen(outputFile)) {
      outFP = (FILE*)FOPEN(outputFile, "a");
      if (!outFP) {
          outFP = stderr;
      }
      else {
          out_file_exists = 1;
      }
  }
  else {
      outFP = stderr;
  }
  if (out_file_exists) {
      fprintf(outFP, "\n-----------------------------------------------\n");

      sprintf(msg, "FAIL: Differences found when comparing:\n  %s\nto\n  %s\n\n",
              file1, file2);
      fprintf(outFP, "%s", msg);
      sprintf(msg, "%s Block Errors:\n\n", block_id);
      fprintf(outFP, "%s", msg);

      fprintf(outFP, "%s", err_msg);

      fprintf(outFP, "-----------------------------------------------\n\n");
  }
  fprintf(stderr,"\n-----------------------------------------------\n");

  sprintf(msg, "FAIL: Differences found when comparing:\n  %s\nto\n  %s\n\n",
          file1, file2);
  fprintf(stderr,"%s",msg);
  sprintf(msg, "%s Block Errors:\n\n", block_id);
  fprintf(stderr,"%s",msg);

  fprintf(stderr,"%s",err_msg);

  fprintf(stderr,"-----------------------------------------------\n\n");
  if(outputFile && strlen(outputFile) && outFP) FCLOSE(outFP);
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
    int found = 0;
    if (meta_is_valid_string(str) && strlen(str) > 0)
    {
        int i;
        for (i=0; i<num_strings; i++) {
            if (strcmp(valid_strings[i], str) == 0) {
                found = 1;
            }
        }
        if (!found) {
            char *s = STRDUP(err_msgs);
            sprintf(err_msgs, "%s  [%s] Invalid %s field in new version file:\n    %s\n\n"
                    "    Expected one of:\n",
                    s, block_id, var_name, str);
            FREE(s);
            char cat_str[2048];
            strcpy(cat_str, "");
            for (i=0; i<num_strings; i++) {
                if (strlen(valid_strings[i]) > 0) {
                    char *s = STRDUP(cat_str);
                    sprintf(cat_str, "%s      %s\n", s, valid_strings[i]);
                    FREE(s);
                }
            }
            strcat(err_msgs, cat_str);
            strcat(err_msgs, "\n\n");
            *failed = 1;
        }
    }
    if (required && !found) {
        char *s = STRDUP(err_msgs);
        sprintf(err_msgs, "%s  [%s] Missing %s field in new version file.\n\n"
                "    Expected one of:\n",
                s, block_id, var_name);
        FREE(s);
        int i;
        char cat_str[2048];
        strcpy(cat_str, "");
        for (i=0; i<num_strings; i++) {
            if (strlen(valid_strings[i]) > 0) {
                char *s = STRDUP(cat_str);
                sprintf(cat_str, "%s      %s\n", s, valid_strings[i]);
                FREE(s);
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
              "%s  [%s] The %s field is out of range:\n    %0.9f\n\n"
              "    Expected:\n      %0.9f to %0.9f\n\n",
              err_msgs, block_id, var_name, num, lower_lim, upper_lim);
      *failed = 1;
    }
  }
  else if (required) {
    sprintf(err_msgs,
            "%s  [%s] The %s field is missing.\n\n"
            "    Expected:\n      %0.9f to %0.9f\n\n",
            err_msgs, block_id, var_name, lower_lim, upper_lim);
    *failed = 1;
  }
}

void verify_double_with_mask(char *err_msgs, double num, double mask,
                   double lower_lim, double upper_lim,
                   char *block_id, char *var_name,
                   int required, int *failed)
{
  if (meta_is_valid_double(num))
  {
    if (!FLOAT_COMPARE_TOLERANCE(num, mask, 0.0000000001) &&
        (num < lower_lim || num > upper_lim))
    {
      sprintf(err_msgs,
              "%s  [%s] The %s field is out of range:\n    %0.9f\n\n"
                  "    Expected:\n      %0.9f to %0.9f\n\n",
              err_msgs, block_id, var_name, num, lower_lim, upper_lim);
      *failed = 1;
    }
  }
  else if (required) {
    sprintf(err_msgs,
            "%s  [%s] The %s field is missing.\n\n"
                "    Expected:\n      %0.9f to %0.9f\n\n",
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
            var_name, (*enum2str)(var1), (*enum2str)(var2));
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

void diff_check_metadata(char *outputFile, int is_not_a_geotiff, char *metafile1, char *metafile2)
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
  meta_statistics *mstats1, *mstats2;
  meta_state_vectors *mstatev1, *mstatev2;
  meta_location *mloc1, *mloc2;
  meta_airsar *mair1, *mair2;
  meta_colormap *mc1, *mc2;

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
  //float mver1 = meta1->meta_version;
  //float mver2 = meta2->meta_version;
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
  mstatev1 = meta1->state_vectors;    // Can be NULL
  mstatev2 = meta2->state_vectors;
  mloc1 = meta1->location;
  mloc2 = meta1->location;
  mair1 = meta1->airsar;              // Can be NULL
  mair2 = meta2->airsar;
  mc1 = meta1->colormap;              // Can be NULL
  mc2 = meta2->colormap;

  if (mp1) {
      albers1 = &mp1->param.albers;
      atct1   = &mp1->param.atct;
      lamaz1  = &mp1->param.lamaz;
      lamcc1  = &mp1->param.lamcc;
      ps1     = &mp1->param.ps;
      utm1    = &mp1->param.utm;
      state1  = &mp1->param.state;
  }
  else {
      albers1 = NULL;
      atct1   = NULL;
      lamaz1  = NULL;
      lamcc1  = NULL;
      ps1     = NULL;
      utm1    = NULL;
      state1  = NULL;
  }
  if (mp2) {
      albers2 = &mp2->param.albers;
      atct2   = &mp2->param.atct;
      lamaz2  = &mp2->param.lamaz;
      lamcc2  = &mp2->param.lamcc;
      ps2     = &mp2->param.ps;
      utm2    = &mp2->param.utm;
      state2  = &mp2->param.state;
  }
  else {
      albers2 = NULL;
      atct2   = NULL;
      lamaz2  = NULL;
      lamcc2  = NULL;
      ps2     = NULL;
      utm2    = NULL;
      state2  = NULL;
  }

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
  if (is_not_a_geotiff) {
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
    validate_string(precheck_err_msgs, mg2->acquisition_date,
                    "General", "acquisition_date",
                    &failed);
# define NUM_SENSOR_STRINGS 9
    char *sensor_strings[NUM_SENSOR_STRINGS] =
      {"SIR-C", "ERS1",
       "ERS2",  "JERS1",
       "ALOS",  "RSAT-1",
       "AIRSAR", "UAVSAR",
       "SEASAT"};
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

  // I am commenting this out for now -- looks like the mode now contains
  // some extra information
# define NUM_MODE_STRINGS 164
/*
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
                  0, &failed);
*/
  }

  // FIXME: Add a verify_enum() function and let it use a hook to a
  // function that tells if the value is a valid member or not.
  if (mg2->data_type != ASF_BYTE              &&
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
      mg2->image_data_type != INTERFEROGRAM         &&
      mg2->image_data_type != COHERENCE_IMAGE       &&
      mg2->image_data_type != GEOREFERENCED_IMAGE   &&
      mg2->image_data_type != GEOCODED_IMAGE        &&
      mg2->image_data_type != POLARIMETRIC_IMAGE    &&
      mg2->image_data_type != POLARIMETRIC_SEGMENTATION &&
      mg2->image_data_type != POLARIMETRIC_DECOMPOSITION &&
      mg2->image_data_type != POLARIMETRIC_PARAMETER &&
      mg2->image_data_type != POLARIMETRIC_C2_MATRIX &&
      mg2->image_data_type != POLARIMETRIC_C3_MATRIX &&
      mg2->image_data_type != POLARIMETRIC_C4_MATRIX &&
      mg2->image_data_type != POLARIMETRIC_T3_MATRIX &&
      mg2->image_data_type != POLARIMETRIC_T4_MATRIX &&
      mg2->image_data_type != POLARIMETRIC_S2_MATRIX &&
      mg2->image_data_type != POLARIMETRIC_STOKES_MATRIX &&
      mg2->image_data_type != LUT_IMAGE             &&
      mg2->image_data_type != ELEVATION             &&
      mg2->image_data_type != DEM                   &&
      mg2->image_data_type != IMAGE                 &&
      mg2->image_data_type != SIMULATED_IMAGE       &&
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

  // FIXME: Write a verify_char_with_mask() so we can ignore single
  // values, e.g. MAGIC_UNSET_CHAR.  NOTE: AirSAR ignores the unset char
  // for orbit direction
# define NUM_ORBIT_DIRECTION_CHARS 2
  if (is_not_a_geotiff && !mair1 && !mair2) {
    char orbit_direction_chars[NUM_ORBIT_DIRECTION_CHARS] =
      {'A', 'D'};
    verify_char(precheck_err_msgs, mg2->orbit_direction,
                orbit_direction_chars, NUM_ORBIT_DIRECTION_CHARS,
                "General", "orbit_direction",
                1, &failed);
  }

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
                0, &failed);

  verify_double(precheck_err_msgs, mg2->center_longitude,
                DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
                "General", "center_longitude",
                0, &failed);

  verify_double(precheck_err_msgs, mg2->re_major,
                DM_MIN_RE_MAJOR, DM_MAX_RE_MAJOR,
                "General", "re_major",
                1, &failed);

  verify_double(precheck_err_msgs, mg2->re_minor,
                DM_MIN_RE_MINOR, DM_MAX_RE_MINOR,
                "General", "re_minor",
                1, &failed);

  verify_double(precheck_err_msgs, mg2->bit_error_rate,
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

    verify_int(precheck_err_msgs, msar2->azimuth_look_count,
               DM_MIN_LOOK_COUNT, DM_MAX_LOOK_COUNT,
               "SAR", "azimuth_look_count",
               1, &failed);

    verify_int(precheck_err_msgs, msar2->range_look_count,
               DM_MIN_LOOK_COUNT, DM_MAX_LOOK_COUNT,
               "SAR", "range_look_count",
               1, &failed);

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

    verify_double_with_mask(precheck_err_msgs, msar2->prf, 0.0,
                  DM_MIN_PRF, DM_MAX_PRF,
                  "SAR", "prf",
                  0, &failed);

    verify_double(precheck_err_msgs, msar2->earth_radius,
                  DM_MIN_EARTH_RADIUS, DM_MAX_EARTH_RADIUS,
                  "SAR", "earth_radius",
                  1, &failed);

    verify_double(precheck_err_msgs, msar2->earth_radius_pp,
                  DM_MIN_EARTH_RADIUS, DM_MAX_EARTH_RADIUS,
                  "SAR", "earth_radius_pp",
                  0, &failed);

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
    verify_string(precheck_err_msgs, uc(mo2->pointing_direction),
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
                  1, &failed);

    verify_double(precheck_err_msgs, mo2->sun_elevation_angle,
                  DM_MIN_SUN_ELEVATION_ANGLE, DM_MAX_SUN_ELEVATION_ANGLE,
                  "Optical", "sun_elevation_angle",
                  1, &failed);

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
                  1, &failed);

    verify_double(precheck_err_msgs, mtherm2->band_gain_change,
                  DM_MIN_BAND_GAIN_CHANGE, DM_MAX_BAND_GAIN_CHANGE,
                  "Thermal", "band_gain_change",
                  1, &failed);

    verify_int(precheck_err_msgs, mtherm2->day,
               DM_MIN_DAY, DM_MAX_DAY,
               "Thermal", "day",
               1, &failed);

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
                "[Transform] One or more of the phi(), lambda(), i(), or j() transform\n"
                "    parameters in the metadata transform block is not a valid double\n"
                "    or is NaN.");
      }
    }
    // NOTE: The incid_a() and map_ transform parameters can be NaN and that's OK, so don't
    // check them.  If this changes, then uncomment-out the code below.
    // FIXME: incid_a[] is in the meta_transform block in metadata versions 2.7 and lower, but
    // in the meta_sar block in versions 2.8 and higher ...the commented out code below needs
    // to reflect this change
    //for (i=0; i<6; i++) {
        //if (!meta_is_valid_double(mtrans2->incid_a[i]))
        //{
            //failed = 1;
        //}
        //if (failed) {
            //sprintf(precheck_err_msgs, "%s  %s\n",
                    //precheck_err_msgs,
                    //"[Transform] One or more of the incid_a() transform parameters in the\n"
                    //"    metadata transform block is not a valid double or is NaN.");
        //}
    //}
    //for (i=0; i<10; i++) {
        //if (!meta_is_valid_double(mtrans2->map2ls_a[i]) ||
            //!meta_is_valid_double(mtrans2->map2ls_b[i]))
        //{
            //failed = 1;
        //}
        //if (failed) {
            //sprintf(precheck_err_msgs, "%s  %s\n",
                    //precheck_err_msgs,
                    //"[Transform] One or more of the map_a() or map_b() transform parameters\n"
                    //"    in the metadata transform block is not a valid double or is NaN.");
        //}
    //}
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
  // Check AirSAR Block
  //
  if (mair2) {
      failed = 0;
      strcpy(precheck_err_msgs, "");

      verify_double(precheck_err_msgs, mair2->scale_factor,
                    DM_MIN_AIRSAR_SCALE_FACTOR, DM_MAX_AIRSAR_SCALE_FACTOR,
                    "AirSAR", "scale_factor",
                    1, &failed);

      verify_double(precheck_err_msgs, mair2->gps_altitude,
                    DM_MIN_GPS_ALTITUDE, DM_MAX_GPS_ALTITUDE,
                    "AirSAR", "gps_altitude",
                    1, &failed);

      verify_double(precheck_err_msgs, mair2->lat_peg_point,
                    DM_MIN_LAT_PEG_POINT, DM_MAX_LAT_PEG_POINT,
                    "AirSAR", "lat_peg_point",
                    1, &failed);

      verify_double(precheck_err_msgs, mair2->lon_peg_point,
                    DM_MIN_LON_PEG_POINT, DM_MAX_LON_PEG_POINT,
                    "AirSAR", "lon_peg_point",
                    1, &failed);

      verify_double(precheck_err_msgs, mair2->head_peg_point,
                    DM_MIN_HEADING_PEG_POINT, DM_MAX_HEADING_PEG_POINT,
                    "AirSAR", "head_peg_point",
                    1, &failed);

      verify_double(precheck_err_msgs, mair2->along_track_offset,
                    DM_MIN_ALONG_TRACK_OFFSET, DM_MAX_ALONG_TRACK_OFFSET,
                    "AirSAR", "along_track_offset",
                    1, &failed);

      verify_double(precheck_err_msgs, mair2->cross_track_offset,
                    DM_MIN_CROSS_TRACK_OFFSET, DM_MAX_CROSS_TRACK_OFFSET,
                    "AirSAR", "cross_track_offset",
                    1, &failed);

    // AIRSAR BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
      if (failed) {
          report_validation_errors(outputFile, metafile2,
                                   precheck_err_msgs, "AIRSAR");
      }
  }
  //
  // End of AirSAR Block Validity Check
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
        mp2->type != MERCATOR                       &&
        mp2->type != EQUI_RECTANGULAR               &&
        mp2->type != SINUSOIDAL                     &&
        mp2->type != STATE_PLANE                    &&
        mp2->type != SCANSAR_PROJECTION             &&
        mp2->type != LAT_LONG_PSEUDO_PROJECTION     &&
        mp2->type != UNKNOWN_PROJECTION)
    {
      sprintf(precheck_err_msgs,
              "%s%s%s%s    %s, or\n    %s, or\n    %s, or\n    %s, or\n   %s, or\n    %s, or\n"
              "    %s, or\n    %s, or\n    %s, or\n    %s, or\n    %s, or\n    %s\n",
              precheck_err_msgs,
              "  [Projection] New version projection type (",
              (mp2->type == UNIVERSAL_TRANSVERSE_MERCATOR) ? "UNIVERSAL_TRANSVERSE_MERCATOR"  :
              (mp2->type == POLAR_STEREOGRAPHIC)           ? "POLAR_STEREOGRAPHIC"            :
              (mp2->type == ALBERS_EQUAL_AREA)             ? "ALBERS_EQUAL_AREA"              :
              (mp2->type == LAMBERT_CONFORMAL_CONIC)       ? "LAMBERT_CONFORMAL_CONIC"        :
              (mp2->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)  ? "LAMBERT_AZIMUTHAL_EQUAL_AREA"   :
              (mp2->type == MERCATOR)                      ? "MERCATOR"                       :
              (mp2->type == EQUI_RECTANGULAR)              ? "EQUI_RECTANGULAR"               :
              (mp2->type == SINUSOIDAL)                    ? "SINUSOIDAL"                     :
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
              "MERCATOR",
              "EQUI_RECTANGULAR",
              "SINUSOIDAL",
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

    if (!is_geocentric(meta2)) {
        // Found geographic or other
        verify_double(precheck_err_msgs, mp2->perX,
                      DM_MIN_PERX, DM_MAX_PERX,
                      "Projection", "perX",
                      1, &failed);
        verify_double(precheck_err_msgs, mp2->perY,
                      DM_MIN_PERY, DM_MAX_PERY,
                      "Projection", "perY",
                      1, &failed);
    }
    else {
        // Found geocentric
        verify_double(precheck_err_msgs, mp2->perX,
                      DM_GEOCENTRIC_MIN_PERX, DM_GEOCENTRIC_MAX_PERX,
                      "Projection", "perX",
                      1, &failed);
        verify_double(precheck_err_msgs, mp2->perY,
                      DM_GEOCENTRIC_MIN_PERY, DM_GEOCENTRIC_MAX_PERY,
                      "Projection", "perY",
                      1, &failed);
    }

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
        mp2->spheroid != INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_SPHEROID &&
        mp2->spheroid != WGS66_SPHEROID             &&
        mp2->spheroid != WGS72_SPHEROID             &&
        mp2->spheroid != WGS84_SPHEROID             &&
        mp2->spheroid != HUGHES_SPHEROID            &&
        mp2->spheroid != JGD2000_SPHEROID           &&
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
                  "    INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997, or \n"
                  "    WGS66, or \n"
                  "    WGS72, or \n"
                  "    WGS84, or \n"
                  "    HUGHES, or \n"
                  "    JGD2000, or \n"
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
                  "    JGD2000\n"
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

        if (meta_is_valid_double(mp2->param.utm.false_easting) &&
            mp2->param.utm.false_easting != DM_UTM_FALSE_EASTING)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f\n\n",
                  precheck_err_msgs,
                  "  [Projection - UTM] New version false_easting invalid (",
                  mp2->param.utm.false_easting,
                  ").\n    Expected:\n      ",
                  DM_UTM_FALSE_EASTING);
          failed = 1;
        }

        if (meta_is_valid_double(mp2->param.utm.false_northing)      &&
            mp2->param.utm.false_northing != DM_N_UTM_FALSE_NORTHING &&
            mp2->param.utm.false_northing != DM_S_UTM_FALSE_NORTHING)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n\n",
                  precheck_err_msgs,
                  "  [Projection - UTM] New version false_northing invalid (",
                  mp2->param.utm.false_northing,
                  ").\n    Expected:\n      ",
                  DM_N_UTM_FALSE_NORTHING,
                  " or ",
                  DM_S_UTM_FALSE_NORTHING);
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
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n\n",
                  precheck_err_msgs,
                  "  [Projection - UTM] New version scale_factor invalid (",
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
        // FIXME: Double check what should be required, and not.
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
        // FIXME: Double check what should be required, and not.
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
      case MERCATOR:
        verify_double(precheck_err_msgs, mp2->param.mer.orig_latitude,
          DM_MIN_LATITUDE, DM_MAX_LATITUDE,
          "Projection - MERCATOR", "orig_latitude", 1, &failed);
        verify_double(precheck_err_msgs, mp2->param.mer.central_meridian,
          DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
          "Projection - MERCATOR", "central_meridian", 1, &failed);
        verify_double(precheck_err_msgs, mp2->param.mer.standard_parallel,
          DM_MIN_LATITUDE, DM_MAX_LATITUDE,
          "Projection - MERCATOR", "standard_parallel", 1, &failed);
        verify_double(precheck_err_msgs, mp2->param.mer.false_easting,
          DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
          "Projection - MERCATOR", "false_easting", 1, &failed);
        verify_double(precheck_err_msgs, mp2->param.mer.false_northing,
          DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
          "Projection - MERCATOR", "false_northing", 1, &failed);
        break;
      case EQUI_RECTANGULAR:
        verify_double(precheck_err_msgs, mp2->param.eqr.orig_latitude,
          DM_MIN_LATITUDE, DM_MAX_LATITUDE,
          "Projection - EQR", "orig_latitude", 1, &failed);
        verify_double(precheck_err_msgs, mp2->param.eqr.central_meridian,
          DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
          "Projection - EQR", "center_meridian", 1, &failed);
        verify_double(precheck_err_msgs, mp2->param.eqr.false_easting,
          DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
          "Projection - EQR", "false_easting", 1, &failed);
        verify_double(precheck_err_msgs, mp2->param.eqr.false_northing,
          DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
          "Projection - EQR", "false_northing", 1, &failed);
        break;
      case SINUSOIDAL:
        verify_double(precheck_err_msgs, mp2->param.sin.longitude_center,
          DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
          "Projection - SINUSOIDAL", "central_meridian", 1, &failed);
        verify_double(precheck_err_msgs, mp2->param.sin.false_easting,
          DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
          "Projection - SINUSOIDAL", "false_easting", 1, &failed);
        verify_double(precheck_err_msgs, mp2->param.sin.false_northing,
          DM_MIN_LONGITUDE, DM_MAX_LONGITUDE,
          "Projection - SINUSOIDAL", "false_northing", 1, &failed);
        verify_double(precheck_err_msgs, mp2->param.sin.sphere,
          DM_MIN_SPHERE, DM_MAX_SPHERE,
          "Projection - SINUSOIDAL", "sphere", 1, &failed);
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
        // Should never be here
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
    int band_no;
    failed = 0;
    strcpy(precheck_err_msgs, "");

    verify_int(precheck_err_msgs, mstats2->band_count,
               DM_MIN_BAND_COUNT, DM_MAX_BAND_COUNT,
               "Stats", "band_count",
               1, &failed);
    for (band_no=0; mstats2->band_count > 0 && band_no<mstats2->band_count; band_no++) {
      char block_id[96];
      if (meta_is_valid_string(mstats2->band_stats[band_no].band_id) &&
          strlen(mstats2->band_stats[band_no].band_id)) {
        sprintf(block_id, "Stats - Band %s", mstats2->band_stats[band_no].band_id);
      }
      else {
        sprintf(block_id, "Stats - Band %02d", band_no);
      }
      validate_string(precheck_err_msgs, mstats2->band_stats[band_no].band_id,
                      block_id, "band_id", &failed);
      validate_double(precheck_err_msgs, mstats2->band_stats[band_no].min,
                      block_id, "min", &failed);

      validate_double(precheck_err_msgs, mstats2->band_stats[band_no].max,
                      block_id, "max", &failed);

      validate_double(precheck_err_msgs, mstats2->band_stats[band_no].mean,
                      block_id, "mean", &failed);

      // rmse ignored

      validate_double(precheck_err_msgs, mstats2->band_stats[band_no].std_deviation,
                      block_id, "std_deviation", &failed);
    }

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
  compare_meta_string(compare_err_msgs, "General", "acquisition_date",
                      mg1->acquisition_date, mg2->acquisition_date, &failed);
  compare_meta_enum(compare_err_msgs, "General", "data_type",
                    mg1->data_type, mg2->data_type,
                    data_type2str_w, &failed);
  compare_meta_enum(compare_err_msgs, "General", "image_data_type",
                    mg1->image_data_type, mg2->image_data_type,
                    image_data_type2str_w, &failed);
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
    compare_meta_char(compare_err_msgs, "SAR", "image_type",
                    msar1->image_type, msar2->image_type, &failed);
    compare_meta_char(compare_err_msgs, "SAR", "look_direction",
                    msar1->look_direction, msar2->look_direction, &failed);
    compare_meta_int(compare_err_msgs, "SAR", "azimuth_look_count",
                    msar1->azimuth_look_count, msar2->azimuth_look_count, &failed);
    compare_meta_int(compare_err_msgs, "SAR", "range_look_count",
                    msar1->range_look_count, msar2->range_look_count, &failed);
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
    float incid_a1 = MAGIC_UNSET_DOUBLE;
    float incid_a2 = MAGIC_UNSET_DOUBLE;
    if (msar1->incid_a != NULL && msar2->incid_a != NULL) {
        char block_id[256] = "";
        char var_name[256] = "";
        int i;
        for (i=0; i<6; i++) {
            //        incid_a1 = mver1 >= 2.8 ? msar1->incid_a[i] : mtrans1->incid_a[i];
            //        incid_a2 = mver2 >= 2.8 ? msar2->incid_a[i] : mtrans2->incid_a[i];;
            //        sprintf(block_id, "File 1 %s block, File 2 %s block - incid_a(%d)",
            //                mver1 >= 2.8 ? "Sar" : "Transform",
            //                mver2 >= 2.8 ? "Sar" : "Transform",
            //                i);
            incid_a1 = msar1->incid_a[i];
            incid_a2 = msar2->incid_a[i];
            strcpy(block_id, "Sar");
            sprintf(var_name, "incid_a[%d]", i);
            if (meta_is_valid_double(incid_a1) &&
                meta_is_valid_double(incid_a2))
            {
                compare_meta_double_with_tolerance(compare_err_msgs, block_id, var_name,
                                                incid_a1, incid_a2,
                                                (i == 0) ? DM_INCID_A0_TOL :
                                                (i == 1) ? DM_INCID_A1_TOL :
                                                (i == 2) ? DM_INCID_A2_TOL :
                                                (i == 3) ? DM_INCID_A3_TOL :
                                                (i == 4) ? DM_INCID_A4_TOL :
                                                (i == 5) ? DM_INCID_A5_TOL :
                                                        0.0001,
                                                        &failed);
            }
            else if (( meta_is_valid_double(incid_a1) && !meta_is_valid_double(incid_a2)) ||
                    (!meta_is_valid_double(incid_a1) &&  meta_is_valid_double(incid_a2)))
            {
                sprintf(&compare_err_msgs[strlen(compare_err_msgs)],
                        "  [%s] Baseline and new version %s are different:\n    %f\n    %f\n\n",
                        block_id, var_name, incid_a1, incid_a2);
                failed = 1;
            }
        }
    }
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
                        uc(mo1->pointing_direction), uc(mo2->pointing_direction), &failed);
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
  //
  // NOTE: The incid_a[] polynomial coefficients are located in
  //       the transform block if the metadata version is v2.7
  //       or lower, but in the sar block if the version is v2.8
  //       or higher (see svn rev 7798)
  failed = 0;
  strcpy(compare_err_msgs, "");
  if (mtrans1 && mtrans2) {
    char block_id[256] = "";
    char var_name[256] = "";
    int i;
    compare_meta_int(compare_err_msgs, "Transform", "parameter_count",
                     mtrans1->parameter_count, mtrans2->parameter_count, &failed);
    if (meta_is_valid_int(mtrans1->parameter_count) &&
        meta_is_valid_int(mtrans2->parameter_count) &&
        mtrans1->parameter_count == mtrans2->parameter_count)
    {
      for (i=0; i<mtrans2->parameter_count; i++) {
        sprintf(block_id, "Transform - Phi(%d)", i);
        sprintf(var_name, "y[%d]", i);
        compare_meta_double_with_tolerance(compare_err_msgs, block_id, var_name,
                                           mtrans1->y[i], mtrans2->y[i],
                                           (i == 0)  ? DM_PHI0_TOL :
                                           (i == 1)  ? DM_PHI1_TOL :
                                           (i == 2)  ? DM_PHI2_TOL :
                                           (i == 3)  ? DM_PHI3_TOL :
                                           (i == 4)  ? DM_PHI4_TOL :
                                           (i == 5)  ? DM_PHI5_TOL :
                                           (i == 6)  ? DM_PHI6_TOL :
                                           (i == 7)  ? DM_PHI7_TOL :
                                           (i == 8)  ? DM_PHI8_TOL :
                                           (i == 9)  ? DM_PHI9_TOL :
                                           (i == 10) ? DM_PHI10_TOL :
                                           0.0001,
                                           &failed);
      }
      for (i=0; i<mtrans2->parameter_count; i++) {
        sprintf(block_id, "Transform - Lambda(%d)", i);
        sprintf(var_name, "x[%d]", i);
        compare_meta_double_with_tolerance(compare_err_msgs, block_id, var_name,
                                           mtrans1->x[i], mtrans2->x[i],
                                           (i == 0)  ? DM_LAMBDA0_TOL :
                                           (i == 1)  ? DM_LAMBDA1_TOL :
                                           (i == 2)  ? DM_LAMBDA2_TOL :
                                           (i == 3)  ? DM_LAMBDA3_TOL :
                                           (i == 4)  ? DM_LAMBDA4_TOL :
                                           (i == 5)  ? DM_LAMBDA5_TOL :
                                           (i == 6)  ? DM_LAMBDA6_TOL :
                                           (i == 7)  ? DM_LAMBDA7_TOL :
                                           (i == 8)  ? DM_LAMBDA8_TOL :
                                           (i == 9)  ? DM_LAMBDA9_TOL :
                                           (i == 10) ? DM_LAMBDA10_TOL :
                                           0.0001,
                                           &failed);
      }
      for (i=0; i<mtrans2->parameter_count; i++) {
        sprintf(block_id, "Transform - i(%d)", i);
        sprintf(var_name, "s[%d]", i);
        compare_meta_double_with_tolerance(compare_err_msgs, block_id, var_name,
                                           mtrans1->s[i], mtrans2->s[i],
                                           (i == 0)  ? DM_I0_TOL :
                                           (i == 1)  ? DM_I1_TOL :
                                           (i == 2)  ? DM_I2_TOL :
                                           (i == 3)  ? DM_I3_TOL :
                                           (i == 4)  ? DM_I4_TOL :
                                           (i == 5)  ? DM_I5_TOL :
                                           (i == 6)  ? DM_I6_TOL :
                                           (i == 7)  ? DM_I7_TOL :
                                           (i == 8)  ? DM_I8_TOL :
                                           (i == 9)  ? DM_I9_TOL :
                                           (i == 10) ? DM_I10_TOL :
                                           0.0001,
                                           &failed);
      }
      for (i=0; i<mtrans2->parameter_count; i++) {
        sprintf(block_id, "Transform - j(%d)", i);
        sprintf(var_name, "l[%d]", i);
        compare_meta_double_with_tolerance(compare_err_msgs, block_id, var_name,
                                           mtrans1->l[i], mtrans2->l[i],
                                           (i == 0)  ? DM_J0_TOL  :
                                           (i == 1)  ? DM_J1_TOL  :
                                           (i == 2)  ? DM_J2_TOL  :
                                           (i == 3)  ? DM_J3_TOL  :
                                           (i == 4)  ? DM_J4_TOL  :
                                           (i == 5)  ? DM_J5_TOL  :
                                           (i == 6)  ? DM_J6_TOL  :
                                           (i == 7)  ? DM_J7_TOL  :
                                           (i == 8)  ? DM_J8_TOL  :
                                           (i == 9)  ? DM_J9_TOL  :
                                           (i == 10) ? DM_J10_TOL :
                                           0.0001,
                                           &failed);
      }
    }
    // Ignore comparing origin_lat (appears to not be used)
    // Ignore comparing origin_lon (appears to not be used)
    for (i=0; i<10; i++) {
        sprintf(block_id, "Transform - map_a(%d)", i);
        sprintf(var_name, "map2ls_a[%d]", i);
        if (meta_is_valid_double(mtrans1->map2ls_a[i]) &&
            meta_is_valid_double(mtrans2->map2ls_a[i]))
        {
            compare_meta_double_with_tolerance(compare_err_msgs, block_id, var_name,
                                               mtrans1->map2ls_a[i], mtrans2->map2ls_a[i],
                                               (i == 0) ? DM_MAP2LS_A0_TOL :
                                               (i == 1) ? DM_MAP2LS_A1_TOL :
                                               (i == 2) ? DM_MAP2LS_A2_TOL :
                                               (i == 3) ? DM_MAP2LS_A3_TOL :
                                               (i == 4) ? DM_MAP2LS_A4_TOL :
                                               (i == 5) ? DM_MAP2LS_A5_TOL :
                                               (i == 6) ? DM_MAP2LS_A6_TOL :
                                               (i == 7) ? DM_MAP2LS_A7_TOL :
                                               (i == 8) ? DM_MAP2LS_A8_TOL :
                                               (i == 9) ? DM_MAP2LS_A9_TOL :
                                               0.0001,
                                               &failed);
        }
        else if (( meta_is_valid_double(mtrans1->map2ls_a[i]) && !meta_is_valid_double(mtrans2->map2ls_a[i])) ||
                 (!meta_is_valid_double(mtrans1->map2ls_a[i]) &&  meta_is_valid_double(mtrans2->map2ls_a[i])))
        {
            sprintf(&compare_err_msgs[strlen(compare_err_msgs)],
                     "  [%s] Baseline and new version %s are different:\n    %f\n    %f\n\n",
                     block_id, var_name, mtrans1->map2ls_a[i], mtrans2->map2ls_a[i]);
            failed = 1;
        }
    }
    for (i=0; i<10; i++) {
        sprintf(block_id, "Transform - map_a(%d)", i);
        sprintf(var_name, "map2ls_b[%d]", i);
        if (meta_is_valid_double(mtrans1->map2ls_b[i]) &&
            meta_is_valid_double(mtrans2->map2ls_b[i]))
        {
            compare_meta_double_with_tolerance(compare_err_msgs, block_id, var_name,
                                               mtrans1->map2ls_b[i], mtrans2->map2ls_b[i],
                                               (i == 0) ? DM_MAP2LS_B0_TOL :
                                               (i == 1) ? DM_MAP2LS_B1_TOL :
                                               (i == 2) ? DM_MAP2LS_B2_TOL :
                                               (i == 3) ? DM_MAP2LS_B3_TOL :
                                               (i == 4) ? DM_MAP2LS_B4_TOL :
                                               (i == 5) ? DM_MAP2LS_B5_TOL :
                                               (i == 6) ? DM_MAP2LS_B6_TOL :
                                               (i == 7) ? DM_MAP2LS_B7_TOL :
                                               (i == 8) ? DM_MAP2LS_B8_TOL :
                                               (i == 9) ? DM_MAP2LS_B9_TOL :
                                               0.0001,
                                               &failed);
        }
        else if (( meta_is_valid_double(mtrans1->map2ls_b[i]) && !meta_is_valid_double(mtrans2->map2ls_b[i])) ||
                 (!meta_is_valid_double(mtrans1->map2ls_b[i]) &&  meta_is_valid_double(mtrans2->map2ls_b[i])))
        {
            sprintf(&compare_err_msgs[strlen(compare_err_msgs)],
                     "  [%s] Baseline and new version %s are different:\n    %f\n    %f\n\n",
                     block_id, var_name, mtrans1->map2ls_b[i], mtrans2->map2ls_b[i]);
            failed = 1;
        }
    }
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
  // Compare AirSAR Blocks
  failed = 0;
  strcpy(compare_err_msgs, "");
  if (mair1 && mair2) {
      compare_meta_double_with_tolerance(compare_err_msgs, "AirSAR", "scale_factor",
                                         mair1->scale_factor, mair2->scale_factor,
                                         DM_SCALE_FACTOR_TOL, &failed);

      compare_meta_double_with_tolerance(compare_err_msgs, "AirSAR", "gps_altitude",
                                         mair1->gps_altitude, mair2->gps_altitude,
                                         DM_GPS_ALTITUDE_TOL, &failed);

      compare_meta_double_with_tolerance(compare_err_msgs, "AirSAR", "lat_peg_point",
                                         mair1->lat_peg_point, mair2->lat_peg_point,
                                         DM_LAT_PEG_POINT_TOL, &failed);

      compare_meta_double_with_tolerance(compare_err_msgs, "AirSAR", "lon_peg_point",
                                         mair1->lon_peg_point, mair2->lon_peg_point,
                                         DM_LON_PEG_POINT_TOL, &failed);

      compare_meta_double_with_tolerance(compare_err_msgs, "AirSAR", "head_peg_point",
                                         mair1->head_peg_point, mair2->head_peg_point,
                                         DM_HEAD_PEG_POINT_TOL, &failed);

      compare_meta_double_with_tolerance(compare_err_msgs, "AirSAR", "along_track_offset",
                                         mair1->along_track_offset, mair2->along_track_offset,
                                         DM_ALONG_TRACK_OFFSET_TOL, &failed);

      compare_meta_double_with_tolerance(compare_err_msgs, "AirSAR", "cross_track_offset",
                                         mair1->cross_track_offset, mair2->cross_track_offset,
                                         DM_CROSS_TRACK_OFFSET_TOL, &failed);
  }
  ////////////////////////////////////////////////////////////
  // AirSAR Block Reporting
  if (failed) {
      report_difference_errors(outputFile,
                               metafile1, metafile2,
                               compare_err_msgs, "AIRSAR");
  }
  // End Compare AirSAR Blocks
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

    compare_meta_double_with_tolerance(compare_err_msgs, "Projection", "perX",
                                       mp1->perX, mp2->perX,
                                       DM_PERX_TOL, &failed);
    compare_meta_double_with_tolerance(compare_err_msgs, "Projection", "perY",
                                       mp1->perY, mp2->perY,
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
          /* A tolerance test on an optional parameter does not make sense to me
          compare_meta_double_with_tolerance(compare_err_msgs, "Projection - LAMCC", "scale_factor",
                                             mp1->param.lamcc.scale_factor, mp2->param.lamcc.scale_factor,
                                             DM_SCALE_FACTOR_TOL, &failed);
          */
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
//  if (mstats1 && mstats2) {
//    compare_meta_double_with_tolerance(compare_err_msgs, "Stats", "min",
//                                       mstats1->min, mstats2->min,
//                                       DM_STATS_MIN_TOL, &failed);
//    compare_meta_double_with_tolerance(compare_err_msgs, "Stats", "max",
//                                       mstats1->max, mstats2->max,
//                                       DM_STATS_MAX_TOL, &failed);
//    compare_meta_double_with_tolerance(compare_err_msgs, "Stats", "mean",
//                                       mstats1->mean, mstats2->mean,
//                                       DM_STATS_MEAN_TOL, &failed);
    // rmse is ignored
//    compare_meta_double_with_tolerance(compare_err_msgs, "Stats", "std_deviation",
//                                       mstats1->std_deviation, mstats2->std_deviation,
//                                       DM_STATS_STD_DEVIATION_TOL, &failed);
    // mask is ignored
//  }
  ////////////////////////////////////////////////////////////
  // Stats Block Reporting
//  if (failed) {
//    report_difference_errors(outputFile,
//                             metafile1, metafile2,
//                             compare_err_msgs, "STATS");
//  }
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
    /* temporarily disabled until the next release build is official
    compare_meta_double_with_tolerance(compare_err_msgs, "State Vector", "second",
                                       mstatev1->second, mstatev2->second,
                                       DM_SECONDS_TOL, &failed);
    */
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

int is_geocentric(meta_parameters *meta)
{
    int ret = 0;

    if (meta &&
        meta->projection &&
        (strncmp_case(meta->projection->units, "DEGREES", 7) == 0 ||
         strncmp_case(meta->projection->units, "ARCSEC" , 6) == 0)
       )
    {
        ret = 1;
    }

    return ret;
}

static char* data_type2str_w(int data_type)
{
 return data_type2str((data_type_t)data_type);
}

static char* image_data_type2str_w(int image_data_type)
{
 return image_data_type2str((image_data_type_t)image_data_type);
}


