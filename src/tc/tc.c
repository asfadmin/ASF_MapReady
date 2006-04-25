#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdarg.h>
#include <limits.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_reporting.h>
#include <asf_contact.h>
#include <asf_copyright.h>
#include <asf_license.h>


int
int_rnd(double x)
{
  return (int)floor(x+0.5);
}

int
asfSystem(const char *format, ...)
{
  va_list ap;
  char cmd[4096];

  va_start(ap, format);
  vsprintf(cmd, format, ap);

  asfPrintStatus("Running system commamd: %s\n", cmd);

  int ret = system(cmd);
  
  if (ret != 0) {
    printf("Error running command %d: %s\n", errno, strerror(errno));
    exit(ret);
  }
  return ret;
}

char * appendSuffix(const char *inFile, const char *suffix)
{
  char *suffix_pid = MALLOC(sizeof(char)*(strlen(suffix)+25));
  sprintf(suffix_pid, "%s_tctmp%d", suffix, (int)getpid());

  char *ret = MALLOC(sizeof(char)*(strlen(inFile)+strlen(suffix_pid)+5));
  strcpy(ret, inFile);
  char *p = strrchr(ret, '.');
  if (p) {
    *p = '\0';
    ++p;
    strcat(ret, suffix_pid);
    strcat(ret, ".");
    strcat(ret, p);
  } else {
    strcat(ret, suffix_pid);
  }

  free(suffix_pid);
  return ret;
}

void ensure_ext(char **filename, const char *ext)
{
  char *ret = MALLOC(sizeof(char)*(strlen(*filename)+strlen(ext)+5));
  strcpy(ret, *filename);

  // blow away current extension if necessary
  char *p = strrchr(ret, '.');
  if (p) *p = '\0';

  if (ext[0] != '.') strcat(ret, ".");
  strcat(ret, ext);

  free(*filename);
  *filename = ret;
}

void read_corr(const char *corrFile, double *dx, double *dy)
{
  FILE *cf = FOPEN(corrFile, "rt");
  if (cf) {
    fscanf(cf, "%lf %lf", dx, dy);
  } else {
    asfPrintError("Couldn't open fftMatch correlation file: %s!\n", corrFile);
    *dx = *dy = 0;
  }
  fclose(cf);
}

char * change_extension(const char * file, const char * ext)
{
  char * replaced = (char *)
    MALLOC(sizeof(char) * (strlen(file) + strlen(ext) + 10));
  
  strcpy(replaced, file);
  char * p = strrchr(replaced, '.');
  
  if (p)
    *p = '\0';
  
  strcat(replaced, ".");
  strcat(replaced, ext);
  
  return replaced;
}

int file_exists(const char * file)
{
  int fd = open(file, 0);
  int stat = fd >= 3;
  close(fd);
  return stat;
}

void remove_file(const char * file)
{
  if (file_exists(file)) {
    asfPrintStatus("Removing intermediate file: %s\n", file);
    unlink(file);
  }
}

// attempt to remove "<file>.img" and "<file>.meta", etc files
void clean(const char *file)
{
  char * img_file = change_extension(file, "img");
  char * meta_file = change_extension(file, "meta");
  char * ddr_file = change_extension(file, "ddr");

  remove_file(img_file);
  remove_file(meta_file);
  remove_file(ddr_file);
  remove_file(file);

  free(img_file);
  free(meta_file);
  free(ddr_file);
}

#define NUM_ARGS 3
void usage(const char *name)
{
  printf("Usage: %s <inFile> <demFile> <outFile>\n", name);
  exit(EXIT_FAILURE);
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

// Main program body.
int
main (int argc, char *argv[])
{
  char *inFile, *demFile, *resampleFile, *srFile, *outFile;
  char *demGridFile, *demPolyFile, *demClipped, *demSlant, *demSimAmp;
  char *demTrimSimAmp, *corrFile, *corrFile2, *demTrimSlant;
  double demRes, sarRes;
  int demWidth, demHeight;
  meta_parameters *metaSAR, *metaDEM;
  double dx, dy, azScale;
  int currArg, idx, idy;
  int polyOrder = 5, clean_files = 1;

  currArg = 1;

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-log","--log",NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
      quietflag = TRUE;
    }
    else if (strmatches(key,"-keep","--keep","-k",NULL)) {
      clean_files = 0;
    }
    else {
      printf( "\n**Invalid option:  %s\n", argv[currArg-1]);
      usage(argv[0]);
    }
  }
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }

  inFile = argv[currArg];
  demFile = argv[currArg+1];
  outFile = argv[currArg+2];

  asfPrintStatus("Input File: %s\n", inFile);
  asfPrintStatus("DEM File: %s\n", demFile);
  asfPrintStatus("Output File: %s\n", outFile);

  metaSAR = meta_read(inFile);
  metaDEM = meta_read(demFile);

  demRes = metaDEM->general->x_pixel_size;
  sarRes = metaSAR->general->x_pixel_size;

  //resample
  asfPrintStatus("DEM Resolution: %g, SAR Resolution: %g\n", demRes, sarRes);
  if (demRes > 1.5 * sarRes) {
    resampleFile = appendSuffix(inFile, "_rsmpl");
    resample_to_square_pixsiz(inFile, resampleFile, demRes);
    meta_free(metaSAR);
    metaSAR = meta_read(resampleFile);
  } else {
    resampleFile = strdup(inFile);
  }

  double sr_x_ps = SPD_LIGHT / ((2.0 * metaSAR->sar->range_sampling_rate) *
      metaSAR->general->sample_count / metaSAR->sar->original_sample_count);
  asfPrintStatus("Calculated Slant Range x pixel size: %g\n", sr_x_ps);

  //gr2sr
  if (metaSAR->sar->image_type != 'S') {
    srFile = appendSuffix(inFile, "_slant");
    double sr_pixel_size = 
      (meta_get_slant(metaSAR,0,metaSAR->general->sample_count) -
       meta_get_slant(metaSAR,0,0)) / metaSAR->general->sample_count;
    asfSystem("gr2sr -p %.8f %s %s\n", sr_pixel_size, resampleFile, srFile);

    meta_free(metaSAR);
    metaSAR = meta_read(srFile);
  } else {
    srFile = strdup(resampleFile);
  }

  //create_dem_grid
  demGridFile = appendSuffix(inFile, "_demgrid");
  asfSystem("create_dem_grid -w %d -h %d %s %s %s",
	  metaSAR->general->sample_count, metaSAR->general->line_count,
	  demFile, srFile, demGridFile);

  //fit_poly
  demPolyFile = appendSuffix(inFile, "_dempoly");
  asfSystem("fit_poly %s %d %s", demGridFile, polyOrder, demPolyFile);

  //remap
  demClipped = appendSuffix(demFile, "_clip");
  demWidth = metaSAR->general->sample_count + 400;
  demHeight = metaSAR->general->line_count;
  asfSystem("remap -translate 0 0 -poly %s -width %d -height %d "
	  "-bilinear -float %s %s", demPolyFile, demWidth, demHeight,
	  demFile, demClipped);

  //reskew_dem
  demSlant = appendSuffix(demFile, "_slant");
  demSimAmp = appendSuffix(demFile, "_sim_amp");
  asfSystem("reskew_dem %s %s %s %s", srFile, demClipped, demSlant, demSimAmp);

  //trim
  demTrimSimAmp = appendSuffix(demFile, "_sim_amp_trim");
  trim(demSimAmp, demTrimSimAmp, 0, 0, metaSAR->general->sample_count,
       demHeight);

  //fftMatch
  corrFile = appendSuffix(inFile, "_corr");
  fftMatch(srFile, demTrimSimAmp, NULL, corrFile);

  read_corr(corrFile, &dx, &dy);
  asfPrintStatus("Correlation: dx=%g dy=%g\n", dx, dy);
  idx = - int_rnd(dx);
  idy = - int_rnd(dy);

  //trim
  trim(demSimAmp, demTrimSimAmp, idx, idy, metaSAR->general->sample_count,
       demHeight);

  //fftMatch
  corrFile2 = appendSuffix(inFile, "_corr2");
  fftMatch(srFile, demTrimSimAmp, NULL, corrFile2);

  //trim
  demTrimSlant = appendSuffix(demFile, "_slant_trim");
  trim(demSlant, demTrimSlant, idx, idy, metaSAR->general->sample_count,
       demHeight);

  //deskew_dem
  ensure_ext(&demTrimSlant, "img");
  ensure_ext(&srFile, "img");
  asfSystem("deskew_dem -i %s 0 %s %s", srFile, demTrimSlant, outFile);

  if (clean_files) {
    clean(resampleFile);
    clean(srFile);
    clean(demClipped);
    clean(demPolyFile);
    clean(demGridFile);
    clean(demTrimSlant);
    clean(demTrimSimAmp);
    clean(corrFile2);
    clean(corrFile);
    clean(demSimAmp);
    clean(demSlant);
  }

  asfPrintStatus("\n\nTerrain Correction Complete!\n");

  free(resampleFile);
  free(srFile);
  free(demClipped);
  free(demPolyFile);
  free(demGridFile);
  free(demTrimSlant);
  free(demTrimSimAmp);
  free(corrFile2);
  free(corrFile);
  free(demSimAmp);
  free(demSlant);

  meta_free(metaSAR);
  meta_free(metaDEM);

  return EXIT_SUCCESS;
}
