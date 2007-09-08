/*******************************************************************************
NAME: diffmeta

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0            7/07   B. Dixon    As released

ALGORITHM DESCRIPTION:
  1. Reads file1 and file2 metadata files and validates all fields
     (valid strings, ints, doubles, etc)
  2. Checks all of file2 fields for valid ranges according to range
     values in diffmeta_tolerances.h
  3. Compares fields between file1 and file2:
     a. If file2 is missing a field found in file1 => fail
     b. If file2 has a field not found in file1 => ok
     c. If like fields in file1 and file2 are different by
        more than the allowed comparison tolerance => fail
  4. Output file name is required.  Output file will always be created
     but if no differences or failure states were detected, then the
     file will be zero-length (empty)

BUGS:

*******************************************************************************/
#include "asf.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "libasf_proj.h"
#include <math.h>
#include <ctype.h>
#include "typlim.h"
#include <gsl/gsl_math.h>
#include "diffmeta.h"
#include "diffmeta_tolerances.h"

#define VERSION 1.0

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define FLOAT_TOLERANCE 0.000001

/**** TYPES ****/

/**** PROTOTYPES ****/
void usage(char *name);
char *data_type2str(data_type_t data_type);
void projection_type_2_str(projection_type_t proj, char *proj_str);


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
    asfPrintError(msg);
  }
  if (strcmp(logFile, outputFile) == 0) {
    sprintf(msg, "Log file cannot be the same as the output file:\n     Log file: %s\n  Output file: %s\n",
            logFile, outputFile);
    if (outputFile) FREE(outputFile);
    asfPrintError(msg);
  }

  // Create blank output file
  FILE *outFP=(FILE*)FOPEN(outputFile, "w");
  if(outFP == NULL) {
    asfPrintError("Cannot open output file %s\n", outputFile);
  }
  else {
    FCLOSE(outFP);
  }

  metafile1 = STRDUP(inFile1);
  metafile2 = STRDUP(inFile2);
  append_ext_if_needed(metafile1, ".meta", ".meta");
  append_ext_if_needed(metafile2, ".meta", ".meta");
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
  FCLOSE(outFP);
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
      "   1. diffmeta first checks all fields in both metadata files for validity, e.g.\n"
      "      doubles should be valid doubles, etcetera.\n"
      "   2. diffmeta then checks all fields in metafile2 for valid ranges and types\n"
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
char *data_type2str(data_type_t data_type)
{
  char *retstr = (char*)CALLOC(64, sizeof(char));

  switch (data_type) {
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
  // PRECHECK
  ////////////////////////////////////////////////////////////
  // Check General Block
  //
  failed = 0; // Start out with no failures
  strcpy(precheck_err_msgs, "");
  if (strlen(mg2->basename) <= 0        ||
      strlen(mg2->sensor) <= 0          ||
      strlen(mg2->sensor_name) <= 0     ||
      strlen(mg2->mode) <= 0            ||
      strlen(mg2->processor) <= 0       ||
      strlen(mg2->system) <= 0          ||
      strlen(mg2->acquisition_date) <= 0)
  {
    // Missing fields in metafile2
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nString field(s) missing in new version file, e.g.\n"
            "basename, sensor, sensor_name, mode, processor, system, or acquisition_date\n");
    failed = 1;
  }
  if (strlen(mg2->sensor) > 0               &&
      strncmp(mg2->sensor, "SIR-C", 5) != 0 &&
      strncmp(mg2->sensor, "ERS1", 4) != 0  &&
      strncmp(mg2->sensor, "ERS2", 4) != 0  &&
      strncmp(mg2->sensor, "JERS1",5 ) != 0 &&
      strncmp(mg2->sensor, "ALOS", 4) != 0  &&
      strncmp(mg2->sensor, "RSAT-1", 6) != 0)
  {
    // Unrecognized sensor (platform name)
    sprintf(precheck_err_msgs, "%s%s%s\n", precheck_err_msgs,
            "[General]\nInvalid sensor field in new version file: \n", mg2->sensor);
    failed = 1;
  }
  if (strlen(mg2->sensor_name) > 0                &&
      strncmp(mg2->sensor_name, "SAR", 3) != 0    &&
      strncmp(mg2->sensor_name, "AVNIR", 5) != 0  &&
      strncmp(mg2->sensor_name, "PRISM", 5) != 0)
  {
    // Invalid sensor name
    sprintf(precheck_err_msgs, "%s%s%s\n", precheck_err_msgs,
            "[General]\nInvalid sensor_name field in new version file: \n", mg2->sensor_name);
    failed = 1;
  }
  if (strlen(mg2->mode) > 0                 &&
      strlen(mg2->sensor) > 0               &&
      strncmp(mg2->sensor, "ALOS", 4) != 0  &&
      strcmp(mg2->mode, "STD") != 0         &&
      strcmp(mg2->mode, "1A") != 0          &&
      strcmp(mg2->mode, "1B1") != 0         &&
      strcmp(mg2->mode, "1B2R") != 0        &&
      strncmp(mg2->mode, "1B2G", 4) != 0)
  {
    // Unrecognized non-ALOS mode
    sprintf(precheck_err_msgs, "%s%s%s%s\n", precheck_err_msgs,
            "[General]\nInvalid (non-ALOS) mode field in new version file: \n", mg2->mode,
            " Should be STD, 1A, 1B1, 1B2R, or 1B2R for non-ALOS data\n");
    failed = 1;
  }
  if (strlen(mg2->mode) > 0 &&
      strlen(mg2->sensor) > 0 &&
      strncmp(mg2->sensor, "ALOS", 4) == 0)
  {
    int beam_num=0;
    char beam[3], *s;
    if (strncmp(mg2->mode, "WB", 2) == 0) {
      strncpy(beam, mg2->mode, 2);
      s = mg2->mode;
      s += 2;
      beam_num = atoi(s);
    }
    else {
      s = mg2->mode;
      s++;
      strncpy(beam, s, 2);
      s += 2;
      beam_num = atoi(s);
    }
    if (strncmp(beam, "BS", 2) != 0 &&
        strncmp(beam, "BD", 2) != 0 &&
        strncmp(beam, "WB", 2) != 0 &&
        strncmp(beam, "SN", 2) != 0 &&
        strncmp(beam, "LR", 2) != 0)
    {
      // Unrecognized or missing ALOS beam mode
      sprintf(precheck_err_msgs, "%s%s%s%s\n", precheck_err_msgs,
              "[General]\nInvalid (ALOS) mode field in new version file: %s\n", mg2->mode,
                  " Should be FBSx, FBDx, WBx, DSNx, or PLRx, where 'x' is beam number\n");
      failed = 1;
    }
    if (strncmp(beam, "BS", 2) == 0 &&
        (beam_num < 1 || beam_num > 18))
    {
      sprintf(precheck_err_msgs, "%s%s%s%s%d%s%d to %d.\n",
              precheck_err_msgs,
              "[General]\nInvalid (ALOS) beam number in mode field in new version file.\nMode field is ",
              mg2->mode,
              "\nBeam number is ",
              beam_num,
              "\nAllowed range is ",
              1, 18);
      failed = 1;
    }
    if (strncmp(beam, "BD", 2) == 0 &&
        (beam_num < 1 || beam_num > 18))
    {
      sprintf(precheck_err_msgs, "%s%s%s%s%d%s%d to %d.\n",
              precheck_err_msgs,
              "[General]\nInvalid (ALOS) beam number in mode field in new version file.\nMode field is ",
              mg2->mode,
              "\nBeam number is ",
              beam_num,
              "\nAllowed range is ",
              1, 18);
      failed = 1;
    }
    if (strncmp(beam, "WB", 2) == 0 &&
        (beam_num < 1 || beam_num > 2))
    {
      sprintf(precheck_err_msgs, "%s%s%s%s%d%s%d to %d.\n",
              precheck_err_msgs,
              "[General]\nInvalid (ALOS) beam number in mode field in new version file.\nMode field is ",
              mg2->mode,
              "\nBeam number is ",
              beam_num,
              "\nAllowed range is ",
              1, 2);
      failed = 1;
    }
    if (strncmp(beam, "SN", 2) == 0 &&
        (beam_num < 1 || beam_num > 18))
    {
      sprintf(precheck_err_msgs, "%s%s%s%s%d%s%d to %d.\n",
              precheck_err_msgs,
              "[General]\nInvalid (ALOS) beam number in mode field in new version file.\nMode field is ",
              mg2->mode,
              "\nBeam number is ",
              beam_num,
              "\nAllowed range is ",
              1, 18);
      failed = 1;
    }
    if (strncmp(beam, "LR", 2) == 0 &&
        (beam_num < 1 || beam_num > 12))
    {
      sprintf(precheck_err_msgs, "%s%s%s%s%d%s%d to %d.\n",
              precheck_err_msgs,
              "[General]\nInvalid (ALOS) beam number in mode field in new version file.\nMode field is ",
              mg2->mode,
              "\nBeam number is ",
              beam_num,
              "\nAllowed range is ",
              1, 12);
      failed = 1;
    }
  }
  if (strlen(mg2->processor) <= 0)
  {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nprocessor field in new version file is blank\n");
    failed = 1;
  }
  if (strlen(mg2->acquisition_date) <= 0) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nacquisition_date field in new version file is blank\n");
    failed = 1;
  }
  if (strncmp(mg2->system, "lil_ieee", 8) != 0) {
    sprintf(precheck_err_msgs, "%s%s%s%s\n",
            precheck_err_msgs,
            "[General]\nUnexpected system field in new version file: ",
            mg2->system,
            "\nExpected lil_ieee\n");
    failed = 1;
  }
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
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nUnrecognized data_type field in new version file\n");
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
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nUnrecognized image_data_type field in new version file\n");
    failed = 1;
  }
  if (mg2->orbit < 0 || mg2->orbit > DM_MAX_ORBIT) {
    sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
            precheck_err_msgs,
            "[General]\nNew version orbit number out of range (",
            mg2->orbit,
            ").  Expected\n",
            0,
            "through ",
            DM_MAX_ORBIT);
    failed = 1;
  }
  if (mg2->orbit_direction != 'A' && mg2->orbit_direction != 'D') {
    sprintf(precheck_err_msgs, "%s%s%c%s\n",
            precheck_err_msgs,
            "[General]\nInvalid orbit_direction in new version file ('",
            mg2->orbit_direction,
            "').  Expected 'A' or 'D'\n");
    failed = 1;
  }
  if (mg2->frame < DM_MIN_FRAME || mg2->frame > DM_MAX_FRAME) {
    sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
            precheck_err_msgs,
            "[General]\nNew version frame number out of range (",
            mg2->frame,
            ").  Expected\n",
            DM_MIN_FRAME,
            "through ",
            DM_MAX_FRAME);
    failed = 1;
  }
  if (mg2->band_count < DM_MIN_BANDCOUNT || mg2->band_count > DM_MAX_BANDCOUNT) {
    sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
            precheck_err_msgs,
            "[General]\nNew version band count out of range (",
            mg2->band_count,
            ").  Expected\n",
            DM_MIN_BANDCOUNT,
            "through ",
            DM_MAX_BANDCOUNT);
    failed = 1;
  }
  if (mg2->line_count < DM_MIN_LINECOUNT || mg2->line_count > DM_MAX_LINECOUNT) {
    sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
            precheck_err_msgs,
            "[General]\nNew version line count out of range (",
            mg2->line_count,
            ").  Expected\n",
            DM_MIN_LINECOUNT,
            "through ",
            DM_MAX_LINECOUNT);
    failed = 1;
  }
  if (mg2->sample_count < DM_MIN_SAMPLECOUNT || mg2->sample_count > DM_MAX_SAMPLECOUNT) {
    sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
            precheck_err_msgs,
            "[General]\nNew version sample count out of range (",
            mg2->sample_count,
            ").  Expected\n",
            DM_MIN_SAMPLECOUNT,
            "through ",
            DM_MAX_SAMPLECOUNT);
    failed = 1;
  }
  if (mg2->start_line < DM_MIN_STARTLINE || mg2->start_line >= DM_MAX_STARTLINE) {
    sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
            precheck_err_msgs,
            "[General]\nNew version start_line out of range (",
            mg2->start_line,
            ").  Expected\n",
            DM_MIN_STARTLINE,
            "through ",
            DM_MAX_STARTLINE);
    failed = 1;
  }
  if (mg2->start_line >= mg2->line_count) {
    sprintf(precheck_err_msgs, "%s%s%d%s%d%s\n", precheck_err_msgs,
            "[General]\nNew version start_line (",
            mg2->start_line,
            ") greater than line_count (",
            mg2->line_count,
            ")\n");
    failed = 1;
  }
  if (mg2->start_sample < DM_MIN_STARTSAMPLE || mg2->start_sample >= DM_MAX_STARTSAMPLE) {
    sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
            precheck_err_msgs,
            "[General]\nNew version start_sample out of range (",
            mg2->start_sample,
            ").  Expected\n",
            DM_MIN_STARTSAMPLE,
            "through ",
            DM_MAX_STARTSAMPLE);
    failed = 1;
  }
  if (mg2->start_sample >= mg2->sample_count) {
    sprintf(precheck_err_msgs, "%s%s%d%s%d%s\n", precheck_err_msgs,
            "[General]\nNew version start_sample (",
            mg2->start_sample,
            ") greater than sample_count (",
            mg2->sample_count,
            ")\n");
    failed = 1;
  }
  if (mg2->x_pixel_size < DM_MIN_PIXELSIZE || mg2->x_pixel_size > DM_MAX_PIXELSIZE) {
    sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
            precheck_err_msgs,
            "[General]\nNew version x_pixel_size out of range (",
            mg2->x_pixel_size,
            ").  Expected\n",
            DM_MIN_PIXELSIZE,
            "through ",
            DM_MAX_PIXELSIZE);
    failed = 1;
  }
  if (mg2->y_pixel_size < DM_MIN_PIXELSIZE || mg2->y_pixel_size > DM_MAX_PIXELSIZE) {
    sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
            precheck_err_msgs,
            "[General]\nNew version y_pixel_size out of range (",
            mg2->y_pixel_size,
            ").  Expected\n",
            DM_MIN_PIXELSIZE,
            "through ",
            DM_MAX_PIXELSIZE);
    failed = 1;
  }
  if (mg2->center_latitude < DM_MIN_LATITUDE || mg2->center_latitude > DM_MAX_LATITUDE) {
    sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
            precheck_err_msgs,
            "[General]\nNew version center_latitude out of range (",
            mg2->center_latitude,
            ").  Expected\n",
            DM_MIN_LATITUDE,
            "through ",
            DM_MAX_LATITUDE);
    failed = 1;
  }
  if (mg2->center_longitude < DM_MIN_LONGITUDE || mg2->center_longitude > DM_MAX_LONGITUDE) {
    sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
            precheck_err_msgs,
            "[General]\nNew version center_longitude out of range (",
            mg2->center_longitude,
            ").  Expected\n",
            DM_MIN_LONGITUDE,
            "through ",
            DM_MAX_LONGITUDE);
    failed = 1;
  }
  if (mg2->re_major < DM_MIN_MAJOR_AXIS || mg2->re_major > DM_MAX_MAJOR_AXIS) {
    sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
            precheck_err_msgs,
            "[General]\nNew version re_major out of range (",
            mg2->re_major,
            ").  Expected\n",
            DM_MIN_MAJOR_AXIS,
            "through ",
            DM_MAX_MAJOR_AXIS);
    failed = 1;
  }
  if (mg2->re_minor < DM_MIN_MINOR_AXIS || mg2->re_minor > DM_MAX_MINOR_AXIS) {
    sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
            precheck_err_msgs,
            "[General]\nNew version re_minor out of range (",
            mg2->re_minor,
            ").  Expected\n",
            DM_MIN_MINOR_AXIS,
            "through ",
            DM_MAX_MINOR_AXIS);
    failed = 1;
  }
  if (mg2->bit_error_rate < DM_MIN_BIT_ERROR_RATE || mg2->bit_error_rate > DM_MAX_BIT_ERROR_RATE) {
    sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
            precheck_err_msgs,
            "[General]\nNew version bit_error_rate out of range (",
            mg2->bit_error_rate,
            ").  Expected\n",
            DM_MIN_BIT_ERROR_RATE,
            "through ",
            DM_MAX_BIT_ERROR_RATE);
    failed = 1;
  }
  if (mg2->missing_lines < DM_MIN_MISSING_LINES || mg2->missing_lines > DM_MAX_MISSING_LINES) {
    sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
            precheck_err_msgs,
            "[General]\nNew version missing_lines out of range (",
            mg2->missing_lines,
            ").  Expected\n",
            DM_MIN_MISSING_LINES,
            "through ",
            DM_MAX_MISSING_LINES);
    failed = 1;
  }
  // GENERAL BLOCK REPORTING
  // If any failures occurred, produce a report in the output file
  if (failed) {
    char msg[1024];
    FILE *outFP = (FILE*)FOPEN(outputFile, "wa");
  // Strict comparison utilizes all values
    fprintf(outFP, "\n-----------------------------------------------\n");
    asfPrintStatus("\n-----------------------------------------------\n");

    sprintf(msg, "FAIL: Comparing\n  %s\nto\n  %s\n\n",
            metafile1, metafile2);
    fprintf(outFP, msg);
    asfPrintStatus(msg);
    sprintf(msg, "  GENERAL Block Errors:\n\n");
    fprintf(outFP, msg);
    asfPrintStatus(msg);

    fprintf(outFP, precheck_err_msgs);
    asfPrintStatus(precheck_err_msgs);

    fprintf(outFP, "-----------------------------------------------\n\n");
    asfPrintStatus("-----------------------------------------------\n\n");
    FCLOSE(outFP);
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
    if (msar2->image_type != 'S' &&
        msar2->image_type != 'G' &&
        msar2->image_type != 'R' &&
        msar2->image_type != 'P')
    {
      sprintf(precheck_err_msgs, "%s%s%c%s%c, %c, %c, or %c\n",
              precheck_err_msgs,
              "[SAR]\nNew version image_type (",
              msar2->image_type,
              ") invalid.  Expected\n ",
              'S', 'G', 'R', 'P');
      failed = 1;
    }
    if (msar2->look_direction != 'L' && msar2->look_direction != 'R') {
      sprintf(precheck_err_msgs, "%s%s'%c'%s\n",
              precheck_err_msgs,
              "[SAR]\nNew version look_direction (",
              msar2->look_direction,
              ") invalid.  Expected\n 'L' or 'R'\n");
      failed = 1;
    }
    if (msar2->look_count < DM_MIN_LOOK_COUNT || msar2->look_count > DM_MAX_LOOK_COUNT) {
      sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
              precheck_err_msgs,
              "[SAR]\nNew version look_count out of range (",
              msar2->look_count,
              ").  Expected\n",
              DM_MIN_LOOK_COUNT,
              "through ",
              DM_MAX_LOOK_COUNT);
      failed = 1;
    }
    if (msar2->deskewed < DM_MIN_DESKEWED || msar2->deskewed > DM_MAX_DESKEWED) {
      sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
              precheck_err_msgs,
              "[SAR]\nNew version deskewed out of range (",
              msar2->deskewed,
              ").  Expected\n",
              DM_MIN_DESKEWED,
              "through ",
              DM_MAX_DESKEWED);
      failed = 1;
    }
    if (msar2->original_line_count < DM_MIN_ORIGINAL_LINE_COUNT ||
        msar2->original_line_count > DM_MAX_ORIGINAL_LINE_COUNT) {
      sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
              precheck_err_msgs,
              "[SAR]\nNew version original_line_count out of range (",
              msar2->original_line_count,
              ").  Expected\n",
              DM_MIN_ORIGINAL_LINE_COUNT,
              "through ",
              DM_MAX_ORIGINAL_LINE_COUNT);
      failed = 1;
    }
    if (msar2->original_sample_count < DM_MIN_ORIGINAL_SAMPLE_COUNT ||
        msar2->original_sample_count > DM_MAX_ORIGINAL_SAMPLE_COUNT) {
      sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
              precheck_err_msgs,
              "[SAR]\nNew version original_sample_count out of range (",
              msar2->original_sample_count,
              ").  Expected\n",
              DM_MIN_ORIGINAL_SAMPLE_COUNT,
              "through ",
              DM_MAX_ORIGINAL_SAMPLE_COUNT);
      failed = 1;
    }
    if (msar2->line_increment < DM_MIN_LINE_INCREMENT ||
        msar2->line_increment > DM_MAX_LINE_INCREMENT) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version line_increment out of range (",
              msar2->line_increment,
              ").  Expected\n",
              DM_MIN_LINE_INCREMENT,
              "through ",
              DM_MAX_LINE_INCREMENT);
      failed = 1;
    }
    if (msar2->sample_increment < DM_MIN_SAMPLE_INCREMENT ||
        msar2->sample_increment > DM_MAX_SAMPLE_INCREMENT) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version sample_increment out of range (",
              msar2->sample_increment,
              ").  Expected\n",
              DM_MIN_SAMPLE_INCREMENT,
              "through ",
              DM_MAX_SAMPLE_INCREMENT);
      failed = 1;
    }
    if (msar2->range_time_per_pixel < DM_MIN_RANGE_TIME_PER_PIXEL ||
        msar2->range_time_per_pixel > DM_MAX_RANGE_TIME_PER_PIXEL) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version range_time_per_pixel out of range (",
              msar2->range_time_per_pixel,
              ").  Expected\n",
              DM_MIN_RANGE_TIME_PER_PIXEL,
              "through ",
              DM_MAX_RANGE_TIME_PER_PIXEL);
      failed = 1;
    }
    if (msar2->azimuth_time_per_pixel < DM_MIN_AZIMUTH_TIME_PER_PIXEL ||
        msar2->azimuth_time_per_pixel > DM_MAX_AZIMUTH_TIME_PER_PIXEL) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version azimuth_time_per_pixel out of range (",
              msar2->azimuth_time_per_pixel,
              ").  Expected\n",
              DM_MIN_AZIMUTH_TIME_PER_PIXEL,
              "through ",
              DM_MAX_AZIMUTH_TIME_PER_PIXEL);
      failed = 1;
    }
    if (msar2->slant_shift < DM_MIN_SLANT_SHIFT ||
        msar2->slant_shift > DM_MAX_SLANT_SHIFT) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version slant_shift out of range (",
              msar2->slant_shift,
              ").  Expected\n",
              DM_MIN_SLANT_SHIFT,
              "through ",
              DM_MAX_SLANT_SHIFT);
      failed = 1;
    }
    if (msar2->time_shift < DM_MIN_TIME_SHIFT ||
        msar2->time_shift > DM_MAX_TIME_SHIFT) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version time_shift out of range (",
              msar2->time_shift,
              ").  Expected\n",
              DM_MIN_TIME_SHIFT,
              "through ",
              DM_MAX_TIME_SHIFT);
      failed = 1;
    }
    if (msar2->slant_range_first_pixel < DM_MIN_SLANT_RANGE_FIRST_PIXEL ||
        msar2->slant_range_first_pixel > DM_MAX_SLANT_RANGE_FIRST_PIXEL) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version slant_range_first_pixel out of range (",
              msar2->slant_range_first_pixel,
              ").  Expected\n",
              DM_MIN_SLANT_RANGE_FIRST_PIXEL,
              "through ",
              DM_MAX_SLANT_RANGE_FIRST_PIXEL);
      failed = 1;
    }
    if (msar2->wavelength < DM_MIN_WAVELENGTH ||
        msar2->wavelength > DM_MAX_WAVELENGTH) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version wavelength out of range (",
              msar2->wavelength,
              ").  Expected\n",
              DM_MIN_WAVELENGTH,
              "through ",
              DM_MAX_WAVELENGTH);
      failed = 1;
    }
    if (msar2->prf < DM_MIN_PRF ||
        msar2->prf > DM_MAX_PRF) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version prf out of range (",
              msar2->prf,
              ").  Expected\n",
              DM_MIN_PRF,
              "through ",
              DM_MAX_PRF);
      failed = 1;
    }
    if (msar2->earth_radius < DM_MIN_EARTH_RADIUS ||
        msar2->earth_radius > DM_MAX_EARTH_RADIUS) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version earth_radius out of range (",
              msar2->earth_radius,
              ").  Expected\n",
              DM_MIN_EARTH_RADIUS,
              "through ",
              DM_MAX_EARTH_RADIUS);
      failed = 1;
    }
    if (msar2->earth_radius_pp < DM_MIN_EARTH_RADIUS ||
        msar2->earth_radius_pp > DM_MAX_EARTH_RADIUS) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version earth_radius out of range (",
              msar2->earth_radius_pp,
              ").  Expected\n",
              DM_MIN_EARTH_RADIUS,
              "through ",
              DM_MAX_EARTH_RADIUS);
      failed = 1;
    }
    if (msar2->satellite_height < DM_MIN_SATELLITE_HEIGHT ||
        msar2->satellite_height > DM_MAX_SATELLITE_HEIGHT) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version satellite_height out of range (",
              msar2->satellite_height,
              ").  Expected\n",
              DM_MIN_SATELLITE_HEIGHT,
              "through ",
              DM_MAX_SATELLITE_HEIGHT);
      failed = 1;
    }
    // Ignore satellite_binary_time
    // Ignore satellite_clock_time
    if (msar2->range_doppler_coefficients[0] < DM_MIN_DOP_RANGE_CENTROID ||
        msar2->range_doppler_coefficients[0] > DM_MAX_DOP_RANGE_CENTROID) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version doppler_coefficients[0] out of range (",
              msar2->range_doppler_coefficients[0],
              ").  Expected\n",
              DM_MIN_DOP_RANGE_CENTROID,
              "through ",
              DM_MAX_DOP_RANGE_CENTROID);
      failed = 1;
    }
    if (msar2->range_doppler_coefficients[1] < DM_MIN_DOP_RANGE_PER_PIXEL ||
        msar2->range_doppler_coefficients[1] > DM_MAX_DOP_RANGE_PER_PIXEL) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version doppler_coefficients[1] out of range (",
              msar2->range_doppler_coefficients[1],
              ").  Expected\n",
              DM_MIN_DOP_RANGE_PER_PIXEL,
              "through ",
              DM_MAX_DOP_RANGE_PER_PIXEL);
      failed = 1;
    }
    if (msar2->range_doppler_coefficients[2] < DM_MIN_DOP_RANGE_QUAD ||
        msar2->range_doppler_coefficients[2] > DM_MAX_DOP_RANGE_QUAD) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version doppler_coefficients[2] out of range (",
              msar2->range_doppler_coefficients[2],
              ").  Expected\n",
              DM_MIN_DOP_RANGE_QUAD,
              "through ",
              DM_MAX_DOP_RANGE_QUAD);
      failed = 1;
    }
    if (msar2->azimuth_doppler_coefficients[0] < DM_MIN_DOP_AZIMUTH_CENTROID ||
        msar2->azimuth_doppler_coefficients[0] > DM_MAX_DOP_AZIMUTH_CENTROID) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version doppler_coefficients[0] out of azimuth (",
              msar2->azimuth_doppler_coefficients[0],
              ").  Expected\n",
              DM_MIN_DOP_AZIMUTH_CENTROID,
              "through ",
              DM_MAX_DOP_AZIMUTH_CENTROID);
      failed = 1;
    }
    if (msar2->azimuth_doppler_coefficients[1] < DM_MIN_DOP_AZIMUTH_PER_PIXEL ||
        msar2->azimuth_doppler_coefficients[1] > DM_MAX_DOP_AZIMUTH_PER_PIXEL) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version azimuth_doppler_coefficients[1] out of azimuth (",
              msar2->azimuth_doppler_coefficients[1],
              ").  Expected\n",
              DM_MIN_DOP_AZIMUTH_PER_PIXEL,
              "through ",
              DM_MAX_DOP_AZIMUTH_PER_PIXEL);
      failed = 1;
    }
    if (msar2->azimuth_doppler_coefficients[2] < DM_MIN_DOP_AZIMUTH_QUAD ||
        msar2->azimuth_doppler_coefficients[2] > DM_MAX_DOP_AZIMUTH_QUAD) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[SAR]\nNew version azimuth_doppler_coefficients[2] out of azimuth (",
              msar2->azimuth_doppler_coefficients[2],
              ").  Expected\n",
              DM_MIN_DOP_AZIMUTH_QUAD,
              "through ",
              DM_MAX_DOP_AZIMUTH_QUAD);
      failed = 1;
    }
    // SAR BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      char msg[1024];
      FILE *outFP = (FILE*)FOPEN(outputFile, "wa");
      // Strict comparison utilizes all values
      fprintf(outFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      sprintf(msg, "FAIL: Comparing\n  %s\nto\n  %s\n\n",
              metafile1, metafile2);
      fprintf(outFP, msg);
      asfPrintStatus(msg);
      sprintf(msg, "  SAR Block Errors:\n\n");
      fprintf(outFP, msg);
      asfPrintStatus(msg);

      fprintf(outFP, precheck_err_msgs);
      asfPrintStatus(precheck_err_msgs);

      fprintf(outFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
      FCLOSE(outFP);
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
    // FIXME: Might need to add MAGIC_UNSET_STRING to lists of string comparisons
    // in the optical block.
    if (strncmp(uc(mo2->pointing_direction), "FORWARD", 6)   != 0 &&
        strncmp(uc(mo2->pointing_direction), "BACKWARD", 7)  != 0 &&
        strncmp(uc(mo2->pointing_direction), "NADIR", 5)     != 0 &&
        strncmp(uc(mo2->pointing_direction), "OFF-NADIR", 9) != 0)
    {
      sprintf(precheck_err_msgs, "%s%s%s%s %s, or\n %s, or\n %s, or\n %s\n",
              precheck_err_msgs,
              "[Optical]\nNew version pointing_direction (",
              mo2->pointing_direction,
              ") invalid.  Expected one of:\n",
              "Forward", "Backward", "Nadir", "Off-nadir");
      failed = 1;
    }
    if (mo2->off_nadir_angle < DM_MIN_OFF_NADIR_ANGLE ||
        mo2->off_nadir_angle > DM_MAX_OFF_NADIR_ANGLE) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Optical]\nNew version off_nadir_angle (",
              mo2->off_nadir_angle,
              ") invalid.  Expected\n ",
              DM_MIN_OFF_NADIR_ANGLE,
              " to \n",
              DM_MAX_OFF_NADIR_ANGLE);
      failed = 1;
    }
    if (strncmp(uc(mo2->correction_level), "N", 1) != 0 &&
        strncmp(uc(mo2->correction_level), "R", 1) != 0 &&
        strncmp(uc(mo2->correction_level), "G", 1) != 0 &&
        strncmp(uc(mo2->correction_level), "D", 1) != 0)
    {
      sprintf(precheck_err_msgs, "%s%s%s%s %s, %s, %s, or %s\n",
              precheck_err_msgs,
              "[Optical]\nNew version correction_level (",
              mo2->correction_level,
              ") invalid.  Expected one of:\n",
              "N", "R", "G", "D");
      failed = 1;
    }
    if (!ISNAN(mo2->cloud_percentage) &&
        (mo2->cloud_percentage < DM_MIN_CLOUD_PERCENTAGE ||
         mo2->cloud_percentage > DM_MAX_CLOUD_PERCENTAGE))
    {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Optical]\nNew version cloud_percentage (",
              mo2->cloud_percentage,
              ") invalid.  Expected\n ",
              DM_MIN_CLOUD_PERCENTAGE,
              " to \n",
              DM_MAX_CLOUD_PERCENTAGE);
      failed = 1;
    }
    if (!ISNAN(mo2->sun_azimuth_angle) &&
         (mo2->sun_azimuth_angle < DM_MIN_SUN_AZIMUTH_ANGLE ||
         mo2->sun_azimuth_angle > DM_MAX_SUN_AZIMUTH_ANGLE))
    {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Optical]\nNew version sun_azimuth_angle (",
              mo2->sun_azimuth_angle,
              ") invalid.  Expected\n ",
              DM_MIN_SUN_AZIMUTH_ANGLE,
              " to \n",
              DM_MAX_SUN_AZIMUTH_ANGLE);
      failed = 1;
    }
    if (!ISNAN(mo2->sun_elevation_angle) &&
         (mo2->sun_elevation_angle < DM_MIN_SUN_ELEVATION_ANGLE ||
         mo2->sun_elevation_angle > DM_MAX_SUN_ELEVATION_ANGLE))
    {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Optical]\nNew version sun_elevation_angle (",
              mo2->sun_elevation_angle,
              ") invalid.  Expected\n ",
              DM_MIN_SUN_ELEVATION_ANGLE,
              " to \n",
              DM_MAX_SUN_ELEVATION_ANGLE);
      failed = 1;
    }
    // OPTICAL BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      char msg[1024];
      FILE *outFP = (FILE*)FOPEN(outputFile, "wa");
      // Strict comparison utilizes all values
      fprintf(outFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      sprintf(msg, "FAIL: Comparing\n  %s\nto\n  %s\n\n",
              metafile1, metafile2);
      fprintf(outFP, msg);
      asfPrintStatus(msg);
      sprintf(msg, "  Optical Block Errors:\n\n");
      fprintf(outFP, msg);
      asfPrintStatus(msg);

      fprintf(outFP, precheck_err_msgs);
      asfPrintStatus(precheck_err_msgs);

      fprintf(outFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
      FCLOSE(outFP);
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
    if (mtherm2->band_gain < DM_MIN_BAND_GAIN ||
        mtherm2->band_gain > DM_MAX_BAND_GAIN)
    {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Thermal]\nNew version band_gain (",
              mtherm2->band_gain,
              ") invalid.  Expected ",
              DM_MIN_BAND_GAIN,
              " to ",
              DM_MAX_BAND_GAIN);
      failed = 1;
    }
    if (mtherm2->band_gain_change < DM_MIN_BAND_GAIN ||
        mtherm2->band_gain_change > DM_MAX_BAND_GAIN)
    {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Thermal]\nNew version band_gain_change (",
              mtherm2->band_gain_change,
              ") invalid.  Expected ",
              DM_MIN_BAND_GAIN,
              " to ",
              DM_MAX_BAND_GAIN);
      failed = 1;
    }
    if (mtherm2->day != DM_DAY && mtherm2->day != DM_NIGHT)
    {
      sprintf(precheck_err_msgs, "%s%s%s%s%d%s%d\n",
              precheck_err_msgs,
              "[Thermal]\nNew version day (",
              (mtherm2->day == DM_DAY) ? "Day (1)" :
                  (mtherm2->day == DM_NIGHT) ? "Night (0)" : "Unknown",
              ") invalid.  Expected ",
              DM_DAY,
              " or ",
              DM_NIGHT);
      failed = 1;
    }
    // THERMAL BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      char msg[1024];
      FILE *outFP = (FILE*)FOPEN(outputFile, "wa");
      // Strict comparison utilizes all values
      fprintf(outFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      sprintf(msg, "FAIL: Comparing\n  %s\nto\n  %s\n\n",
              metafile1, metafile2);
      fprintf(outFP, msg);
      asfPrintStatus(msg);
      sprintf(msg, "  Thermal Block Errors:\n\n");
      fprintf(outFP, msg);
      asfPrintStatus(msg);

      fprintf(outFP, precheck_err_msgs);
      asfPrintStatus(precheck_err_msgs);

      fprintf(outFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
      FCLOSE(outFP);
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
    if (mtrans2->band_gain < DM_MIN_BAND_GAIN ||
START HERE        mtrans2->band_gain > DM_MAX_BAND_GAIN)
    {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Transform]\nNew version band_gain (",
              mtrans2->band_gain,
              ") invalid.  Expected ",
              DM_MIN_BAND_GAIN,
              " to ",
              DM_MAX_BAND_GAIN);
      failed = 1;
    }
    // THERMAL BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      char msg[1024];
      FILE *outFP = (FILE*)FOPEN(outputFile, "wa");
      // Strict comparison utilizes all values
      fprintf(outFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      sprintf(msg, "FAIL: Comparing\n  %s\nto\n  %s\n\n",
              metafile1, metafile2);
      fprintf(outFP, msg);
      asfPrintStatus(msg);
      sprintf(msg, "  Transform Block Errors:\n\n");
      fprintf(outFP, msg);
      asfPrintStatus(msg);

      fprintf(outFP, precheck_err_msgs);
      asfPrintStatus(precheck_err_msgs);

      fprintf(outFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
      FCLOSE(outFP);
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
              "%s%s%s%s  %s, or\n  %s, or\n  %s, or\n  %s, or\n  %s, or\n  %s, or\n"
              "  %s, or\n  %s, or\n  %s\n",
              precheck_err_msgs,
              "[Projection]\nNew version projection type (",
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
              ") invalid.\nExpected one of:\n",
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
    if (mp2->startX < DM_MIN_STARTX ||
        mp2->startX > DM_MAX_STARTX)
    {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Projection]\nNew version startX (",
              mp2->startX,
              ") invalid.  Expected ",
              DM_MIN_STARTX,
              " to ",
              DM_MAX_STARTX);
      failed = 1;
    }
    if (mp2->startY < DM_MIN_STARTY ||
        mp2->startY > DM_MAX_STARTY)
    {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Projection]\nNew version startY (",
              mp2->startY,
              ") invalid.  Expected ",
              DM_MIN_STARTY,
              " to ",
              DM_MAX_STARTY);
      failed = 1;
    }
    if (mp2->perX < DM_MIN_PERX ||
        mp2->perX > DM_MAX_PERX)
    {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Projection]\nNew version perX (",
              mp2->perX,
              ") invalid.  Expected ",
              DM_MIN_PERX,
              " to ",
              DM_MAX_PERX);
      failed = 1;
    }
    if (mp2->perY < DM_MIN_PERY ||
        mp2->perY > DM_MAX_PERY)
    {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Projection]\nNew version perY (",
              mp2->perY,
              ") invalid.  Expected ",
              DM_MIN_PERY,
              " to ",
              DM_MAX_PERY);
      failed = 1;
    }
    if (strncmp(uc(mp2->units), "METERS",  6) != 0 &&
        strncmp(uc(mp2->units), "ARCSEC",  6) != 0 &&
        strncmp(uc(mp2->units), "DEGREES", 7) != 0)
    {
      sprintf(precheck_err_msgs, "%s%s%s%s\n",
              precheck_err_msgs,
              "[Projection]\nNew version units are invalid (",
              mp2->units,
              ").  Expected \"meters\", \"arcsec\", or \"desgrees\"");
      failed = 1;
    }
    if (mp2->hem != 'N' && mp2->hem != 'S') {
      sprintf(precheck_err_msgs, "%s%s%c%s\n",
              precheck_err_msgs,
              "[Projection]\nNew version hem (hemisphere) invalid (",
              mp2->hem,
              ").  Expected 'N' or 'S'");
      failed = 1;
    }
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
              "[Projection]\nNew version spheroid invalid or unrecognized.  Expected one of:\n"
                  "  BESSEL, or \n"
                  "  CLARKE1866, or \n"
                  "  CLARKE1880, or \n"
                  "  GEM6, or \n"
                  "  GEM10C, or \n"
                  "  GRS1980, or \n"
                  "  INTERNATIONAL1924, or \n"
                  "  INTERNATIONAL1967, or \n"
                  "  WGS72, or \n"
                  "  WGS84, or \n"
                  "  HUGHES, or \n"
                  "  UNKNOWN (enum spheroid_type_t UNKNOWN_SPHEROID)\n");
      failed = 1;
    }
    if (mp2->re_major < DM_MIN_MAJOR_AXIS || mp2->re_major > DM_MAX_MAJOR_AXIS) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Projection]\nNew version re_major out of range (",
              mp2->re_major,
              ").  Expected\n",
              DM_MIN_MAJOR_AXIS,
              "through ",
              DM_MAX_MAJOR_AXIS);
      failed = 1;
    }
    if (mp2->re_minor < DM_MIN_MINOR_AXIS || mp2->re_minor > DM_MAX_MINOR_AXIS) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Projection]\nNew version re_minor out of range (",
              mp2->re_minor,
              ").  Expected\n",
              DM_MIN_MINOR_AXIS,
              "through ",
              DM_MAX_MINOR_AXIS);
      failed = 1;
    }
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
              "[Projection]\nNew version datum invalid or unrecognized.  Expected one of:\n"
                  "  EGM96\n"
                  "  ED50\n"
                  "  ETRF89\n"
                  "  ETRS89\n"
                  "  ITRF97\n"
                  "  NAD27\n"
                  "  NAD83\n"
                  "  WGS72\n"
                  "  WGS84\n"
                  "  HUGHES\n"
                  "  UNKNOWN (enum datum_type_t UNKNOWN_DATUM)\n");
      failed = 1;
    }
    if (mp2->height < DM_MIN_TERRAIN_HEIGHT || mp2->height > DM_MAX_TERRAIN_HEIGHT) {
      sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
              precheck_err_msgs,
              "[Projection]\nNew version height out of range (",
              mp2->height,
              ").  Expected\n",
              DM_MIN_TERRAIN_HEIGHT,
              "through ",
              DM_MAX_TERRAIN_HEIGHT);
      failed = 1;
    }
    switch (mp2->type) {
      case UNIVERSAL_TRANSVERSE_MERCATOR:
        if (mp2->param.utm.zone < DM_MIN_UTM_ZONE ||
            mp2->param.utm.zone > DM_MAX_UTM_ZONE)
        {
          failed = 1;
        }
        if (mp2->param.utm.false_easting != DM_UTM_FALSE_EASTING)
        {
          failed = 1;
        }
        if (mp2->param.utm.false_northing != DM_N_UTM_FALSE_NORTHING &&
            mp2->param.utm.false_northing != DM_S_UTM_FALSE_NORTHING)
        {
          failed = 1;
        }
        if (mp2->param.utm.lat0 < DM_MIN_LATITUDE ||
            mp2->param.utm.lat0 > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][UTM]\nNew version lat0 out of range (",
                  mp2->param.utm.lat0,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        if (mp2->param.utm.lon0 < DM_MIN_LONGITUDE ||
            mp2->param.utm.lon0 > DM_MAX_LONGITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][UTM]\nNew version lon0 out of range (",
                  mp2->param.utm.lon0,
                  ").  Expected\n",
                  DM_MIN_LONGITUDE,
                  "through ",
                  DM_MAX_LONGITUDE);
          failed = 1;
        }
        if (mp2->param.utm.scale_factor != DM_UTM_SCALE_FACTOR &&
            mp2->param.utm.scale_factor != DM_DEFAULT_SCALE_FACTOR)
        {
          failed = 1;
        }
        break;
      case POLAR_STEREOGRAPHIC:
        if (mp2->param.ps.slat < DM_MIN_LATITUDE ||
            mp2->param.ps.slat > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][PS]\nNew version slat out of range (",
                  mp2->param.ps.slat,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        if (mp2->param.ps.slon < DM_MIN_LONGITUDE ||
            mp2->param.ps.slon > DM_MAX_LONGITUDE)
        {
          failed = 1;
        }
        if (mp2->param.ps.is_north_pole != 0 &&
            mp2->param.ps.is_north_pole != 1)
        {
          failed = 1;
        }
        if (mp2->param.ps.false_easting < DM_MIN_LONGITUDE ||
            mp2->param.ps.false_easting > DM_MAX_LONGITUDE)
        {
          failed = 1;
        }
        if (mp2->param.ps.false_northing < DM_MIN_LATITUDE ||
            mp2->param.ps.false_northing > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][PS]\nNew version false_northing out of range (",
                  mp2->param.ps.false_northing,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        break;
      case ALBERS_EQUAL_AREA:
        if (mp2->param.albers.std_parallel1 < DM_MIN_LATITUDE ||
            mp2->param.albers.std_parallel1 > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][ALBERS]\nNew version std_parallel1 out of range (",
                  mp2->param.albers.std_parallel1,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        if (mp2->param.albers.std_parallel2 < DM_MIN_LATITUDE ||
            mp2->param.albers.std_parallel2 > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][ALBERS]\nNew version std_parallel2 out of range (",
                  mp2->param.albers.std_parallel2,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        if (mp2->param.albers.center_meridian < DM_MIN_LONGITUDE ||
            mp2->param.albers.center_meridian > DM_MAX_LONGITUDE)
        {
          failed = 1;
        }
        if (mp2->param.albers.orig_latitude < DM_MIN_LATITUDE ||
            mp2->param.albers.orig_latitude > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][ALBERS]\nNew version orig_latitude out of range (",
                  mp2->param.albers.orig_latitude,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        if (mp2->param.albers.false_easting < DM_MIN_LATITUDE ||
            mp2->param.albers.false_easting > DM_MAX_LATITUDE)
        {
          failed = 1;
        }
        if (mp2->param.albers.false_northing < DM_MIN_LATITUDE ||
            mp2->param.albers.false_northing > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][ALBERS]\nNew version false_northing out of range (",
                  mp2->param.albers.false_northing,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        break;
      case LAMBERT_CONFORMAL_CONIC:
        if (mp2->param.lamcc.plat1 < DM_MIN_LATITUDE ||
            mp2->param.lamcc.plat1 > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][LAMCC]\nNew version plat1 out of range (",
                  mp2->param.lamcc.plat1,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        if (mp2->param.lamcc.plat2 < DM_MIN_LATITUDE ||
            mp2->param.lamcc.plat2 > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][LAMCC]\nNew version plat2 out of range (",
                  mp2->param.lamcc.plat2,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        if (mp2->param.lamcc.lat0 < DM_MIN_LATITUDE ||
            mp2->param.lamcc.lat0 > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][LAMCC]\nNew version lat0 out of range (",
                  mp2->param.lamcc.lat0,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        if (mp2->param.lamcc.lon0 < DM_MIN_LONGITUDE ||
            mp2->param.lamcc.lon0 > DM_MAX_LONGITUDE)
        {
          failed = 1;
        }
        if (mp2->param.lamcc.false_easting < DM_MIN_LONGITUDE ||
            mp2->param.lamcc.false_easting > DM_MAX_LONGITUDE)
        {
          failed = 1;
        }
        if (mp2->param.lamcc.false_northing < DM_MIN_LATITUDE ||
            mp2->param.lamcc.false_northing > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][LAMCC]\nNew version false_northing out of range (",
                  mp2->param.lamcc.false_northing,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        if (mp2->param.lamcc.scale_factor < DM_MIN_LAMCC_SCALE_FACTOR ||
            mp2->param.lamcc.scale_factor > DM_MAX_LAMCC_SCALE_FACTOR)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][LAMCC]\nNew version scale_factor out of range (",
                  mp2->param.lamcc.scale_factor,
                  ").  Expected\n",
                  DM_MIN_LAMCC_SCALE_FACTOR,
                  "through ",
                  DM_MAX_LAMCC_SCALE_FACTOR);
          failed = 1;
        }
        break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA:
        if (mp2->param.lamaz.center_lon < DM_MIN_LONGITUDE ||
            mp2->param.lamaz.center_lon > DM_MAX_LONGITUDE)
        {
          failed = 1;
        }
        if (mp2->param.lamaz.center_lat < DM_MIN_LATITUDE ||
            mp2->param.lamaz.center_lat > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][LAMAZ]\nNew version center_lat out of range (",
                  mp2->param.lamaz.center_lat,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        if (mp2->param.lamaz.false_easting < DM_MIN_LONGITUDE ||
            mp2->param.lamaz.false_easting > DM_MAX_LONGITUDE)
        {
          failed = 1;
        }
        if (mp2->param.lamaz.false_northing < DM_MIN_LATITUDE ||
            mp2->param.lamaz.false_northing > DM_MAX_LATITUDE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][LAMAZ]\nNew version false_northing out of range (",
                  mp2->param.lamaz.false_northing,
                  ").  Expected\n",
                  DM_MIN_LATITUDE,
                  "through ",
                  DM_MAX_LATITUDE);
          failed = 1;
        }
        break;
      case STATE_PLANE:
        if (mp2->param.state.zone < DM_MIN_STATE_PLANE_ZONE ||
            mp2->param.state.zone > DM_MAX_STATE_PLANE_ZONE)
        {
          sprintf(precheck_err_msgs, "%s%s%d%s%d%s%d\n",
                  precheck_err_msgs,
                  "[Projection][STATE_PLANE]\nNew version state plane zone out of range (",
                  mp2->param.state.zone,
                  ").  Expected\n",
                  DM_MIN_STATE_PLANE_ZONE,
                  "through ",
                  DM_MAX_STATE_PLANE_ZONE);
          failed = 1;
        }
        break;
      case SCANSAR_PROJECTION:
        if (mp2->param.atct.rlocal < DM_MIN_EARTH_RADIUS ||
            mp2->param.atct.rlocal > DM_MAX_EARTH_RADIUS)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][ATCT - SCANSAR PROJECTION]\nNew version rlocal out of range (",
                  mp2->param.atct.rlocal,
                  ").  Expected\n",
                  DM_MIN_EARTH_RADIUS,
                  "through ",
                  DM_MAX_EARTH_RADIUS);
          failed = 1;
        }
        if (mp2->param.atct.alpha1 < DM_MIN_ROTATION_ANGLE ||
            mp2->param.atct.alpha1 > DM_MAX_ROTATION_ANGLE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][ATCT - SCANSAR PROJECTION]\nNew version alpha1 out of range (",
                  mp2->param.atct.alpha1,
                  ").  Expected\n",
                  DM_MIN_ROTATION_ANGLE,
                  "through ",
                  DM_MAX_ROTATION_ANGLE);
          failed = 1;
        }
        if (mp2->param.atct.alpha2 < DM_MIN_ROTATION_ANGLE ||
            mp2->param.atct.alpha2 > DM_MAX_ROTATION_ANGLE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][ATCT - SCANSAR PROJECTION]\nNew version alpha2 out of range (",
                  mp2->param.atct.alpha2,
                  ").  Expected\n",
                  DM_MIN_ROTATION_ANGLE,
                  "through ",
                  DM_MAX_ROTATION_ANGLE);
          failed = 1;
        }
        if (mp2->param.atct.alpha3 < DM_MIN_ROTATION_ANGLE ||
            mp2->param.atct.alpha3 > DM_MAX_ROTATION_ANGLE)
        {
          sprintf(precheck_err_msgs, "%s%s%f%s%f%s%f\n",
                  precheck_err_msgs,
                  "[Projection][ATCT - SCANSAR PROJECTION]\nNew version alpha3 out of range (",
                  mp2->param.atct.alpha3,
                  ").  Expected\n",
                  DM_MIN_ROTATION_ANGLE,
                  "through ",
                  DM_MAX_ROTATION_ANGLE);
          failed = 1;
        }
        break;
      case LAT_LONG_PSEUDO_PROJECTION:
      case UNKNOWN_PROJECTION:
        break;
      default:
        sprintf(precheck_err_msgs, "%s%s\n",
                precheck_err_msgs,
                "[Projection]\nUnexpected projection type found");
        failed = 1;
    }
    // PROJECTION BLOCK REPORTING
    // If any failures occurred, produce a report in the output file
    if (failed) {
      char msg[1024];
      FILE *outFP = (FILE*)FOPEN(outputFile, "wa");
      // Strict comparison utilizes all values
      fprintf(outFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      sprintf(msg, "FAIL: Comparing\n  %s\nto\n  %s\n\n",
              metafile1, metafile2);
      fprintf(outFP, msg);
      asfPrintStatus(msg);
      sprintf(msg, "  Projection Block Errors:\n\n");
      fprintf(outFP, msg);
      asfPrintStatus(msg);

      fprintf(outFP, precheck_err_msgs);
      asfPrintStatus(precheck_err_msgs);

      fprintf(outFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
      FCLOSE(outFP);
    }
  }
  //
  // End of Projection Block Validity Check
  ////////////////////////////////////////////////////////////
}

