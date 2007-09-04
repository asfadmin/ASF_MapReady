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
  projection_type_t ptype1, ptype2;
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
    int mode_num, beam_num=0;
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
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version orbit number out of range (%d).  Expected\n%d through %d\n",
           mg2->orbit, 0, DM_MAX_ORBIT);
    failed = 1;
  }
  if (mg2->orbit_direction != 'A' && mg2->orbit_direction != 'D') {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nInvalid orbit_dirction in new version file ('%c').  Expected 'A' or 'D'\n",
           mg2->orbit_direction);
    failed = 1;
  }
  if (mg2->frame < DM_MIN_FRAME || mg2->frame > DM_MAX_FRAME) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version frame number out of range (%d).  Expected\n%d through %d\n",
            mg2->frame, DM_MIN_FRAME, DM_MAX_FRAME);
    failed = 1;
  }
  if (mg2->band_count < 1 || mg2->band_count > DM_MAX_BANDCOUNT) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version band_count out of range (%d).  Expected\n%d through %d\n",
            mg2->band_count, 1, DM_MAX_BANDCOUNT);
    failed = 1;
  }
  if (mg2->line_count < DM_MIN_LINECOUNT || mg2->line_count > DM_MAX_LINECOUNT) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version line_count out of range (%d).  Expected\n%d through %d\n",
            mg2->line_count, DM_MIN_LINECOUNT, DM_MAX_LINECOUNT);
    failed = 1;
  }
  if (mg2->sample_count < DM_MIN_SAMPLECOUNT || mg2->sample_count > DM_MAX_SAMPLECOUNT) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version sample_count out of range (%d).  Expected\n%d through %d\n",
            mg2->sample_count, DM_MIN_SAMPLECOUNT, DM_MAX_SAMPLECOUNT);
    failed = 1;
  }
  if (mg2->start_line < DM_MIN_STARTLINE || mg2->start_line >= DM_MAX_LINECOUNT) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version start_line out of range (%d).  Expected\n%d through %d\n",
            mg2->start_line, DM_MIN_STARTLINE, DM_MAX_STARTLINE);
    failed = 1;
  }
  if (mg2->start_line >= mg2->line_count) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version start_line (%d) greater than line_count (%d)\n",
            mg2->start_line, mg2->line_count);
    failed = 1;
  }
  if (mg2->start_sample < DM_MIN_STARTSAMPLE || mg2->start_sample >= DM_MAX_SAMPLECOUNT) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version start_sample out of range (%d).  Expected\n%d through %d\n",
            mg2->start_sample, DM_MIN_STARTSAMPLE, DM_MAX_STARTSAMPLE);
    failed = 1;
  }
  if (mg2->start_sample >= mg2->sample_count) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version start_sample (%d) greater than sample_count (%d)\n",
            mg2->start_sample, mg2->sample_count);
    failed = 1;
  }
  if (mg2->x_pixel_size < DM_MIN_PIXELSIZE || mg2->x_pixel_size > DM_MAX_PIXELSIZE) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version x_pixel_size (%f) out of range.  Expected\n %f through %f\n",
            mg2->x_pixel_size, DM_MIN_PIXELSIZE, DM_MAX_PIXELSIZE);
    failed = 1;
  }
  if (mg2->y_pixel_size < DM_MIN_PIXELSIZE || mg2->y_pixel_size > DM_MAX_PIXELSIZE) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version y_pixel_size (%f) out of range.  Expected\n %f through %f\n",
            mg2->y_pixel_size, DM_MIN_PIXELSIZE, DM_MAX_PIXELSIZE);
    failed = 1;
  }
  if (mg2->center_latitude < -90.0 || mg2->center_latitude > 90.0) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version center_latitude (%f) out of range.  Expected\n %f through %f\n",
            mg2->center_latitude, -90.0, 90.0);
    failed = 1;
  }
  if (mg2->center_longitude < -180.0 || mg2->center_longitude > 180.0) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version center_longitude (%f) out of range.  Expected\n %f through %f\n",
            mg2->center_longitude, -180.0, 180.0);
    failed = 1;
  }
  if (mg2->re_major < DM_MIN_MAJOR_AXIS || mg2->re_major > DM_MAX_MAJOR_AXIS) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version re_major (%f) out of range.  Expected\n %f through %f\n",
            mg2->re_major, DM_MIN_MAJOR_AXIS, DM_MAX_MAJOR_AXIS);
    failed = 1;
  }
  if (mg2->re_minor < DM_MIN_MINOR_AXIS || mg2->re_minor > DM_MAX_MINOR_AXIS) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version re_minor (%f) out of range.  Expected\n %f through %f\n",
            mg2->re_minor, DM_MIN_MINOR_AXIS, DM_MAX_MINOR_AXIS);
    failed = 1;
  }
  if (mg2->bit_error_rate < DM_MIN_BIT_ERROR_RATE || mg2->bit_error_rate > DM_MAX_BIT_ERROR_RATE) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version bit_error_rate (%d) out of range.  Expected\n %d through %d\n",
            mg2->bit_error_rate, DM_MIN_BIT_ERROR_RATE, DM_MAX_BIT_ERROR_RATE);
    failed = 1;
  }
  if (mg2->missing_lines < DM_MIN_MISSING_LINES || mg2->missing_lines > DM_MAX_MISSING_LINES) {
    sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
            "[General]\nNew version missing_lines (%d) out of range.  Expected\n %d through %d\n",
            mg2->missing_lines, DM_MIN_MISSING_LINES, DM_MAX_MISSING_LINES);
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
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version image_type (%c) invalid.  Expected\n %c, %c, %c, or %c\n",
              msar2->image_type, 'S', 'G', 'R', 'P');
      failed = 1;
    }
    if (msar2->look_direction != 'L' && msar2->look_direction != 'R') {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version look_direction (%c) invalid.  Expected\n %c or %c\n",
              msar2->look_direction, 'L', 'R');
      failed = 1;
    }
    if (msar2->look_count < DM_MIN_LOOK_COUNT || msar2->look_count > DM_MAX_LOOK_COUNT) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version look_count (%d) invalid.  Expected\n %d to %d\n",
              msar2->look_count, DM_MIN_LOOK_COUNT, DM_MAX_LOOK_COUNT);
      failed = 1;
    }
    if (msar2->deskewed < DM_MIN_DESKEWED || msar2->deskewed > DM_MAX_DESKEWED) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version deskewed flag (%d) invalid.  Expected\n %d to %d\n",
              msar2->deskewed, DM_MIN_DESKEWED, DM_MAX_DESKEWED);
      failed = 1;
    }
    if (msar2->original_line_count < DM_MIN_ORIGINAL_LINE_COUNT ||
        msar2->original_line_count > DM_MAX_ORIGINAL_LINE_COUNT) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version original_line_count (%d) invalid.  Expected\n %d to %d\n",
              msar2->original_line_count, DM_MIN_ORIGINAL_LINE_COUNT, DM_MAX_ORIGINAL_LINE_COUNT);
      failed = 1;
    }
    if (msar2->original_sample_count < DM_MIN_ORIGINAL_SAMPLE_COUNT ||
        msar2->original_sample_count > DM_MAX_ORIGINAL_SAMPLE_COUNT) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version original_sample_count (%d) invalid.  Expected\n %d to %d\n",
              msar2->original_sample_count, DM_MIN_ORIGINAL_SAMPLE_COUNT, DM_MAX_ORIGINAL_SAMPLE_COUNT);
      failed = 1;
    }
    if (msar2->line_increment < DM_MIN_LINE_INCREMENT ||
        msar2->line_increment > DM_MAX_LINE_INCREMENT) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version line_increment (%f) invalid.  Expected\n %f to %f\n",
              msar2->line_increment, DM_MIN_LINE_INCREMENT, DM_MAX_LINE_INCREMENT);
      failed = 1;
    }
    if (msar2->sample_increment < DM_MIN_SAMPLE_INCREMENT ||
        msar2->sample_increment > DM_MAX_SAMPLE_INCREMENT) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version sample_increment (%f) invalid.  Expected\n %f to %f\n",
              msar2->sample_increment, DM_MIN_SAMPLE_INCREMENT, DM_MAX_SAMPLE_INCREMENT);
      failed = 1;
    }
    if (msar2->range_time_per_pixel < DM_MIN_RANGE_TIME_PER_PIXEL ||
        msar2->range_time_per_pixel > DM_MAX_RANGE_TIME_PER_PIXEL) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version range_time_per_pixel (%f) invalid.  Expected\n %f to %f\n",
              msar2->range_time_per_pixel, DM_MIN_RANGE_TIME_PER_PIXEL, DM_MAX_RANGE_TIME_PER_PIXEL);
      failed = 1;
    }
    if (msar2->azimuth_time_per_pixel < DM_MIN_AZIMUTH_TIME_PER_PIXEL ||
        msar2->azimuth_time_per_pixel > DM_MAX_AZIMUTH_TIME_PER_PIXEL) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version azimuth_time_per_pixel (%f) invalid.  Expected\n %f to %f\n",
              msar2->azimuth_time_per_pixel, DM_MIN_AZIMUTH_TIME_PER_PIXEL, DM_MAX_AZIMUTH_TIME_PER_PIXEL);
      failed = 1;
    }
    if (msar2->slant_shift < DM_MIN_SLANT_SHIFT ||
        msar2->slant_shift > DM_MAX_SLANT_SHIFT) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version slant_shift (%f) invalid.  Expected\n %f to %f\n",
              msar2->slant_shift, DM_MIN_SLANT_SHIFT, DM_MAX_SLANT_SHIFT);
      failed = 1;
    }
    if (msar2->time_shift < DM_MIN_TIME_SHIFT ||
        msar2->time_shift > DM_MAX_TIME_SHIFT) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version time_shift (%f) invalid.  Expected\n %f to %f\n",
              msar2->time_shift, DM_MIN_TIME_SHIFT, DM_MAX_TIME_SHIFT);
      failed = 1;
    }
    if (msar2->slant_range_first_pixel < DM_MIN_SLANT_RANGE_FIRST_PIXEL ||
        msar2->slant_range_first_pixel > DM_MAX_SLANT_RANGE_FIRST_PIXEL) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version slant_range_first_pixel (%f) invalid.  Expected\n %f to %f\n",
              msar2->slant_range_first_pixel, DM_MIN_SLANT_RANGE_FIRST_PIXEL, DM_MAX_SLANT_RANGE_FIRST_PIXEL);
      failed = 1;
    }
    if (msar2->wavelength < DM_MIN_WAVELENGTH ||
        msar2->wavelength > DM_MAX_WAVELENGTH) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version wavelength (%f) invalid.  Expected\n %f to %f\n",
              msar2->wavelength, DM_MIN_WAVELENGTH, DM_MAX_WAVELENGTH);
      failed = 1;
    }
    if (msar2->prf < DM_MIN_PRF ||
        msar2->prf > DM_MAX_PRF) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version prf (%f) invalid.  Expected\n %f to %f\n",
              msar2->prf, DM_MIN_PRF, DM_MAX_PRF);
      failed = 1;
    }
    if (msar2->earth_radius < DM_MIN_EARTH_RADIUS ||
        msar2->earth_radius > DM_MAX_EARTH_RADIUS) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version earth_radius (%f) invalid.  Expected\n %f to %f\n",
              msar2->earth_radius, DM_MIN_EARTH_RADIUS, DM_MAX_EARTH_RADIUS);
      failed = 1;
    }
    if (msar2->earth_radius_pp < DM_MIN_EARTH_RADIUS ||
        msar2->earth_radius_pp > DM_MAX_EARTH_RADIUS) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version earth_radius_pp (%f) invalid.  Expected\n %f to %f\n",
              msar2->earth_radius_pp, DM_MIN_EARTH_RADIUS, DM_MAX_EARTH_RADIUS);
      failed = 1;
    }
    if (msar2->satellite_height < DM_MIN_SATELLITE_HEIGHT ||
        msar2->satellite_height > DM_MAX_SATELLITE_HEIGHT) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version satellite_height (%f) invalid.  Expected\n %f to %f\n",
              msar2->satellite_height, DM_MIN_SATELLITE_HEIGHT, DM_MAX_SATELLITE_HEIGHT);
      failed = 1;
    }
    // Ignore satellite_binary_time
    // Ignore satellite_clock_time
    if (msar2->range_doppler_coefficients[0] < DM_MIN_DOP_RANGE_CENTROID ||
        msar2->range_doppler_coefficients[0] > DM_MAX_DOP_RANGE_CENTROID) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version dopRangeCen (%f) invalid.  Expected\n %f to %f\n",
              msar2->range_doppler_coefficients[0], DM_MIN_DOP_RANGE_CENTROID, DM_MAX_DOP_RANGE_CENTROID);
      failed = 1;
    }
    if (msar2->range_doppler_coefficients[1] < DM_MIN_DOP_RANGE_PER_PIXEL ||
        msar2->range_doppler_coefficients[1] > DM_MAX_DOP_RANGE_PER_PIXEL) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version dopRangeLin (%f) invalid.  Expected\n %f to %f\n",
              msar2->range_doppler_coefficients[1], DM_MIN_DOP_RANGE_PER_PIXEL, DM_MAX_DOP_RANGE_PER_PIXEL);
      failed = 1;
    }
    if (msar2->range_doppler_coefficients[2] < DM_MIN_DOP_RANGE_QUAD ||
        msar2->range_doppler_coefficients[2] > DM_MAX_DOP_RANGE_QUAD) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version dopRangeQuad (%f) invalid.  Expected\n %f to %f\n",
              msar2->range_doppler_coefficients[2], DM_MIN_DOP_RANGE_QUAD, DM_MAX_DOP_RANGE_QUAD);
      failed = 1;
    }
    if (msar2->range_doppler_coefficients[0] < DM_MIN_DOP_AZIMUTH_CENTROID ||
        msar2->range_doppler_coefficients[0] > DM_MAX_DOP_AZIMUTH_CENTROID) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version dopRangeCen (%f) invalid.  Expected\n %f to %f\n",
              msar2->range_doppler_coefficients[0], DM_MIN_DOP_AZIMUTH_CENTROID, DM_MAX_DOP_AZIMUTH_CENTROID);
      failed = 1;
    }
    if (msar2->range_doppler_coefficients[1] < DM_MIN_DOP_AZIMUTH_PER_PIXEL ||
        msar2->range_doppler_coefficients[1] > DM_MAX_DOP_AZIMUTH_PER_PIXEL) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version dopRangeLin (%f) invalid.  Expected\n %f to %f\n",
              msar2->range_doppler_coefficients[1], DM_MIN_DOP_AZIMUTH_PER_PIXEL, DM_MAX_DOP_AZIMUTH_PER_PIXEL);
      failed = 1;
    }
    if (msar2->range_doppler_coefficients[2] < DM_MIN_DOP_AZIMUTH_QUAD ||
        msar2->range_doppler_coefficients[2] > DM_MAX_DOP_AZIMUTH_QUAD) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[SAR]\nNew version dopRangeQuad (%f) invalid.  Expected\n %f to %f\n",
              msar2->range_doppler_coefficients[2], DM_MIN_DOP_AZIMUTH_QUAD, DM_MAX_DOP_AZIMUTH_QUAD);
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
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Optical]\nNew version pointing_direction (%s) invalid.  Expected one of:\n"
              "  %s, or\n"
              "  %s, or\n"
              "  %s, or\n"
              "  %s\n",
              mo2->pointing_direction,
              "Forward", "Backward", "Nadir", "Off-nadir");
      failed = 1;
    }
    if (mo2->off_nadir_angle < DM_MIN_OFF_NADIR_ANGLE ||
        mo2->off_nadir_angle > DM_MAX_OFF_NADIR_ANGLE) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Optical]\nNew version off_nadir_angle (%f) invalid.  Expected\n %f to %f\n",
              mo2->off_nadir_angle, DM_MIN_OFF_NADIR_ANGLE, DM_MAX_OFF_NADIR_ANGLE);
      failed = 1;
    }
    if (strncmp(uc(mo2->correction_level), "N", 1) != 0 &&
        strncmp(uc(mo2->correction_level), "R", 1) != 0 &&
        strncmp(uc(mo2->correction_level), "G", 1) != 0 &&
        strncmp(uc(mo2->correction_level), "D", 1) != 0)
    {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Optical]\nNew version correction_level (%s) invalid.  Expected one of:\n"
                  "  %s, or\n"
                  "  %s, or\n"
                  "  %s, or\n"
                  "  %s\n",
              mo2->correction_level,
              "N", "R", "G", "D");
      failed = 1;
    }
    if (mo2->off_nadir_angle < DM_MIN_OFF_NADIR_ANGLE ||
        mo2->off_nadir_angle > DM_MAX_OFF_NADIR_ANGLE) {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Optical]\nNew version off_nadir_angle (%f) invalid.  Expected\n %f to %f\n",
              mo2->off_nadir_angle, DM_MIN_OFF_NADIR_ANGLE, DM_MAX_OFF_NADIR_ANGLE);
      failed = 1;
    }
    if (!ISNAN(mo2->cloud_percentage) &&
        (mo2->cloud_percentage < DM_MIN_CLOUD_PERCENTAGE ||
         mo2->cloud_percentage > DM_MAX_CLOUD_PERCENTAGE))
    {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Optical]\nNew version cloud_percentage (%f) invalid.  Expected\n %f to %f or NaN\n",
              mo2->cloud_percentage, DM_MIN_CLOUD_PERCENTAGE, DM_MAX_CLOUD_PERCENTAGE);
      failed = 1;
    }
    if (!ISNAN(mo2->sun_azimuth_angle) &&
         (mo2->sun_azimuth_angle < DM_MIN_SUN_AZIMUTH_ANGLE ||
         mo2->sun_azimuth_angle > DM_MAX_SUN_AZIMUTH_ANGLE))
    {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Optical]\nNew version sun_azimuth_angle (%f) invalid.  Expected\n %f to %f or NaN\n",
              mo2->sun_azimuth_angle, DM_MIN_SUN_AZIMUTH_ANGLE, DM_MAX_SUN_AZIMUTH_ANGLE);
      failed = 1;
    }
    if (!ISNAN(mo2->sun_elevation_angle) &&
         (mo2->sun_elevation_angle < DM_MIN_SUN_ELEVATION_ANGLE ||
         mo2->sun_elevation_angle > DM_MAX_SUN_ELEVATION_ANGLE))
    {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Optical]\nNew version sun_elevation_angle (%f) invalid.  Expected\n %f to %f or NaN\n",
              mo2->sun_elevation_angle, DM_MIN_SUN_ELEVATION_ANGLE, DM_MAX_SUN_ELEVATION_ANGLE);
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
  /* THERMAL BLOCK IS IGNORED - UNUSED
  if (mtherm2) {
    failed = 0;
    strcpy(precheck_err_msgs, "");
    if (mtherm2->band_gain < DM_MIN_BAND_GAIN ||
        mtherm2->band_gain > DM_MAX_BAND_GAIN)
    {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Thermal]\nNew version band_gain (%f) invalid.  Expected %f to %f\n",
              mtherm2->band_gain,
              DM_MIN_BAND_GAIN, DM_MAX_BAND_GAIN);
      failed = 1;
    }
    if (mtherm2->band_gain_change < DM_MIN_BAND_GAIN ||
        mtherm2->band_gain_change > DM_MAX_BAND_GAIN)
    {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Thermal]\nNew version band_gain_change (%f) invalid.  Expected %f to %f\n",
              mtherm2->band_gain_change,
              DM_MIN_BAND_GAIN, DM_MAX_BAND_GAIN);
      failed = 1;
    }
    if (mtherm2->day != DM_DAY && mtherm2->day > DM_NIGHT)
    {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Thermal]\nNew version day (%s) invalid.  Expected %s or %s\n",
              (mtherm2->day == DM_DAY) ? "Day (1)" :
                  (mtherm2->day == DM_NIGHT) ? "Night (0)" : "Unknown",
              DM_DAY, DM_NIGHT);
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
  */

  // FIXME: Insert meta_transform check here

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
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Projection]\nNew version projection type (%s) invalid.  Expected one of:\n"
              "  %s\n, or"
              "  %s\n, or"
              "  %s\n, or"
              "  %s\n, or"
              "  %s\n, or"
              "  %s\n, or"
              "  %s\n, or"
              "  %s\n, or"
              "  %s\n",
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
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Projection]\nNew version startX (%f) invalid.  Expected %f to %f\n",
              mp2->startX,
              DM_MIN_STARTX, DM_MAX_STARTX);
      failed = 1;
    }
    if (mp2->startY < DM_MIN_STARTY ||
        mp2->startY > DM_MAX_STARTY)
    {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Projection]\nNew version startY (%f) invalid.  Expected %f to %f\n",
              mp2->startY,
              DM_MIN_STARTY, DM_MAX_STARTY);
      failed = 1;
    }
    if (mp2->perX < DM_MIN_PERX ||
        mp2->perX > DM_MAX_PERX)
    {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Projection]\nNew version perX (%f) invalid.  Expected %f to %f\n",
              mp2->perX,
              DM_MIN_PERX, DM_MAX_PERX);
      failed = 1;
    }
    if (mp2->perY < DM_MIN_PERY ||
        mp2->perY > DM_MAX_PERY)
    {
      sprintf(precheck_err_msgs, "%s%s\n", precheck_err_msgs,
              "[Projection]\nNew version perY (%f) invalid.  Expected %f to %f\n",
              mp2->perY,
              DM_MIN_PERY, DM_MAX_PERY);
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

