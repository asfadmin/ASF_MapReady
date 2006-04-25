#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_reporting.h>
#include <asf_contact.h>
#include <asf_copyright.h>
#include <asf_license.h>

#define CLIGHT   2.997924562e8

int
int_rnd(double x)
{
  return (int)floor(x+0.5);
}

int
do_system(char *cmd)
{
  asfPrintStatus("Running system: %s\n", cmd);
  int ret = system(cmd);
  if (ret != 0) {
    printf("Error running command %d: %s\n", errno, strerror(errno));
    exit(ret);
  }
  return ret;
}

char * appendSuffix(const char *inFile, const char *suffix)
{
  char *ret = MALLOC(sizeof(char)*(strlen(inFile)+strlen(suffix)+5));
  strcpy(ret, inFile);
  char *p = strrchr(ret, '.');
  if (p) {
    *p = '\0';
    ++p;
    strcat(ret, suffix);
    strcat(ret, ".");
    strcat(ret, p);
  } else {
    strcat(ret, suffix);
  }
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
    *dx = *dy = 0;
  }
  fclose(cf);
}

// Main program body.
int
main (int argc, char *argv[])
{
  char *inFile, *demFile, *resampleFile, *srFile, *srFileUnscaled, *outFile;
  char *demGridFile, *demPolyFile, *demClipped, *demSlant, *demSimAmp;
  char *demTrimSimAmp, *corrFile, *corrFile2, *demTrimSlant;
  char cmd[4096];
  int demRes, sarRes, demWidth, demHeight;
  meta_parameters *metaSAR, *metaDEM;
  double dx, dy, azScale;
  int idx, idy;

  int polyOrder = 5;

  if (argc != 4) {
    printf("Usage: %s <inFile> <demFile> <outFile>\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  inFile = argv[1];
  demFile = argv[2];
  outFile = argv[3];

  metaSAR = meta_read(inFile);
  metaDEM = meta_read(demFile);

  demRes = metaDEM->general->x_pixel_size;
  sarRes = metaSAR->general->x_pixel_size;

  //resample test test_60m 60
  if (demRes > sarRes) {
    resampleFile = appendSuffix(inFile, "_r");
    sprintf(cmd, "resample %s %s %d\n", inFile, resampleFile, demRes);
    do_system(cmd);

    meta_free(metaSAR);
    metaSAR = meta_read(resampleFile);
  } else {
    resampleFile = strdup(inFile);
  }

  float xp, yp;
  xpyp_getPixSizes(metaSAR, &xp, &yp);
  
  double sr_x_ps = CLIGHT / ((2.0 * metaSAR->sar->range_sampling_rate) *
      metaSAR->general->sample_count / metaSAR->sar->original_sample_count);
  printf("Calculated Slant Range x pixel size: %g\n", sr_x_ps);

  //gr2sr test_60m test_sr2_60m
  if (metaSAR->sar->image_type != 'S') {
    srFileUnscaled = appendSuffix(resampleFile, "_usr");
    double sr_pixel_size = metaSAR->general->x_pixel_size *
          sin(meta_look(metaSAR, metaSAR->general->line_count/2,
		    metaSAR->general->sample_count/2 ));
    sprintf(cmd, "gr2sr -p %g %s %s\n", sr_pixel_size, resampleFile,
	    srFileUnscaled);
    do_system(cmd);

    meta_free(metaSAR);
    metaSAR = meta_read(srFileUnscaled);
  } else {
    srFileUnscaled = strdup(resampleFile);
  }

  //remap -scale 0.99667767023656 0.63338076567152 test_sr2_60m
  //       test_sr2_60m_scaled

  srFile = appendSuffix(resampleFile, "_sr");
  azScale = 1;// sr_x_ps / metaSAR->general->y_pixel_size;
  sprintf(cmd, "remap -scale 1 %.15lf %s %s", azScale, srFileUnscaled, srFile);
  do_system(cmd);

  //metaSAR->general->x_pixel_size = metaSAR->general->y_pixel_size;
  metaSAR->general->y_pixel_size = metaSAR->general->x_pixel_size;
  metaSAR->sar->azimuth_time_per_pixel *= azScale;
  meta_write(metaSAR, srFile);

  //create_dem_grid -w 1024 -h 1706 delta_fixed.img test_sr2_60m_scaled.img 
  //                dem_grid
  demGridFile = appendSuffix(inFile, "_demgrid");
  sprintf(cmd, "create_dem_grid -w %d -h %d %s %s %s",
	  metaSAR->general->sample_count, metaSAR->general->line_count,
	  demFile, srFile, demGridFile);
  do_system(cmd);

  //fit_poly dem_grid 5 dem_poly
  demPolyFile = appendSuffix(inFile, "_dempoly");
  sprintf(cmd, "fit_poly %s %d %s", demGridFile, polyOrder, demPolyFile);
  do_system(cmd);

  //remap -translate 0 0 -poly dem_poly -width 1421 -height 1081 
  //      -bilinear -float delta_fixed.img dem_big.img
  demClipped = appendSuffix(demFile, "_clip");
  demWidth = metaSAR->general->sample_count + 400;
  demHeight = metaSAR->general->line_count;
  sprintf(cmd, "remap -translate 0 0 -poly %s -width %d -height %d "
	  "-bilinear -float %s %s", demPolyFile, demWidth, demHeight,
	  demFile, demClipped);
  do_system(cmd);

  //reskew_dem test_sr2_60m_scaled.meta dem_big.img dem_slant.img 
  //           dem_sim_amp.img
  demSlant = appendSuffix(demFile, "_slant");
  demSimAmp = appendSuffix(demFile, "_sim_amp");
  sprintf(cmd, "reskew_dem %s %s %s %s", srFile, demClipped, demSlant,
	  demSimAmp);
  do_system(cmd);

  //trim -h 1081 -w 1021 dem_sim_amp.img dem_trimsim_amp.img 0 0
  demTrimSimAmp = appendSuffix(demSimAmp, "_tr");
  sprintf(cmd, "trim -h %d -w %d %s %s 0 0", demHeight,
	  metaSAR->general->sample_count, demSimAmp, demTrimSimAmp);
  do_system(cmd);

  //fftMatch -m dem.corr test_sr2_60m_scaled.img dem_trimsim_amp.img
  corrFile = appendSuffix(inFile, "_corr");
  sprintf(cmd, "fftMatch -m %s %s %s", corrFile, srFile, demTrimSimAmp);
  do_system(cmd);

  read_corr(corrFile, &dx, &dy);
  asfPrintStatus("Correlation: dx=%g dy=%g\n", dx, dy);
  idx = - int_rnd(dx);
  idy = - int_rnd(dy);

  //trim -h 1081 -w 1021 dem_sim_amp.img dem_trimsim_amp.img
  //     `neg ${dy}` `neg ${dx}`
  sprintf(cmd, "trim -h %d -w %d %s %s %d %d", demHeight,
	  metaSAR->general->sample_count, demSimAmp, demTrimSimAmp,
	  idy, idx);
  do_system(cmd);

  //fftMatch -m dem.corr2 test_sr2_60m_scaled.img dem_trimsim_amp.img
  corrFile2 = appendSuffix(inFile, "_corr2");
  sprintf(cmd, "fftMatch -m %s %s %s", corrFile2, srFile, demTrimSimAmp);
  do_system(cmd);

  //trim -h 1081 -w 1021 dem_slant.img dem_trimmed_slant.img 
  //     `neg ${dy}` `neg ${dx}`
  demTrimSlant = appendSuffix(demSlant, "_tr");
  sprintf(cmd, "trim -h %d -w %d %s %s %d %d", demHeight,
	  metaSAR->general->sample_count, demSlant, demTrimSlant,
	  idy, idx);
  do_system(cmd);
	  
  //deskew_dem -i test_sr2_60m_scaled.img 0 dem_trimmed_slant.img
  //           test_sr2_tc.img
  ensure_ext(&demTrimSlant, "img");
  ensure_ext(&srFile, "img");
  sprintf(cmd, "deskew_dem -i %s 0 %s %s", srFile, demTrimSlant, outFile);
  do_system(cmd);

  //asf_geocode -p utm test_sr2_tc test_sr2_tc_utm

  asfPrintStatus("Terrain Correction Complete!\n");

  free(resampleFile);
  free(srFile);
  free(demClipped);
  free(demPolyFile);
  free(demGridFile);
  free(demTrimSlant);
  free(corrFile2);
  free(corrFile);
  free(demSimAmp);
  free(demSlant);

  meta_free(metaSAR);
  meta_free(metaDEM);

  return EXIT_SUCCESS;
}
