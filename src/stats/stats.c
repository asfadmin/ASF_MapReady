/****************************************************************
NAME: stats

SYNOPSIS: stats [-mask <value>] [-log <logFile>] [-quiet]
                [-overmeta] [-nometa] [-overstat] [-nostat]
                [-startline <line>] [-startsample <sample>]
                [-width <width>] [-height <height>]
                [-trim <fraction>] <sar_name>

DESCRIPTION:
  Takes statistics on an image file and prints them out to
  a .stat file as well as inserting a stats structure in the
  .meta file.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:

    VERS:   DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0     3/2003   P. Denny     Put sar image file statistics into
                                   meta file and stats file
    1.1     4/2003   B. Kerin     Added windowing functionality.
    1.2     5/2003   P. Denny     Fixed windowing bug,
                                    wrote stat_write and stat_read
    1.3     5/2004   R. Gens      Added trimming option

HARDWARE/SOFTWARE LIMITATIONS:
  None known

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS: None known

*******************************************************************************
*                                                                             *
*   Figures statistics for a SAR image file and dumps them to file            *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "stats.h"
#include "asf_license.h"
#include "asf_nan.h"

#define VERSION 1.4

/* For floating point comparisons.  */
#define MICRON 0.00000001
#ifdef FLOAT_EQUIVALENT
#  undef  FLOAT_EQUIVALENT
#  define FLOAT_EQUIVALENT(a, b) (fabs(a - b) < MICRON ? 1 : 0)
#endif

#define SQR(X) ((X)*(X))

stat_parameters calc_hist(stat_parameters stats, char *sar_name, int band, meta_parameters *meta,
                          double sum_of_samples, long samples_counted, int mask_flag);

static void
usage(char *name)
{
  printf("\n"
   "USAGE:\n"
   "   %s [-mask <value>] [-log <logFile>] [-quiet]\n"
   "         [-overmeta] [-nometa] [-overstat] [-nostat]\n"
   "         <sar_name>\n", name);
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   sar_name   Name of input image file (including extension)\n");
  printf("\n"
   "OPTIONAL ARGUMENTS:\n"
   "   -mask         Value to ignore while taking statistics.\n"
   "   -log          Copy terminal output to <logFile>.\n"
   "   -quiet        Supress terminal output.\n"
   "   -overmeta     Force overwrite of existing .meta file.\n"
   "   -nometa       Do not write a .meta file.\n"
   "   -overstat     Force overwrite of existing .stat file.\n"
   "   -nostat       Do not write a .stat file.\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program takes statistics on a SAR data file and writes them\n"
   "   with a histogram out to a .stat file. It also inserts a stats block\n"
   "   in the .meta file.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  int overmeta_flag = FALSE, overstat_flag = FALSE, nometa_flag = FALSE;
  int nostat_flag=FALSE, mask_flag=FALSE;
  double mask = MAGIC_UNSET_DOUBLE;
  meta_parameters *meta = NULL;
  //stat_parameters *stats = NULL;
  char file_name[261], stat_name[261], sar_name[256];
  extern int currArg;

  handle_license_and_version_args(argc, argv, "stats");
  logflag=quietflag=FALSE;
  while (currArg < (argc-1)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-quiet")) {
      quietflag=TRUE;
    }
    else if (strmatch(key,"-log")) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag=TRUE;
    }
    else if (strmatch(key,"-mask")) {
      CHECK_ARG(1);
      mask = atof(GET_ARG(1));
      mask_flag=TRUE;
    }
    else if (strmatch(key,"-overmeta")) {
      overmeta_flag=TRUE;
    }
    else if (strmatch(key,"-overstat")) {
      overstat_flag=TRUE;
    }
    else if (strmatch(key,"-nometa")) {
      nometa_flag=TRUE;
    }
    else if (strmatch(key,"-nostat")) {
      nostat_flag=TRUE;
    }
    else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }

  if ((argc-currArg)<1) {printf("Insufficient arguments.\n"); usage(argv[0]);}
  strcpy (sar_name, argv[currArg]);
  char *ext = findExt(sar_name);
  if (ext == NULL || strcmp("IMG", uc(ext)) != 0) {
    strcpy(sar_name, appendExt(sar_name, ".img"));
  }
  create_name(file_name, sar_name, ".img");
  create_name(stat_name, sar_name, ".stat");

  asfSplashScreen(argc, argv);

  // Make sure we don't over write any files that we don't want to
  meta = meta_read(file_name);
  if (meta->stats && !overmeta_flag && !nometa_flag)
    asfPrintError(
      "The meta file already has a populated statistics structure.\n"
      "If you want to run this program and replace that structure,\n"
      "then use the -overmeta option to do so. If you want to run\n"
      "this program, but don't want to replace the structure, use\n"
      "the -nometa option.\n");
  if (fileExists(stat_name) && !overstat_flag && !nostat_flag)
    asfPrintError(
      "The file, %s, already exists. If you want to\n"
      "overwrite it, then use the -overstat option to do so.\n"
      "If you want to run the progam but don't want to write\n"
      "over the current file, then use the -nostat option.\n", stat_name);

  // Calculate statistics
  char **band_names = NULL;
  if (meta_is_valid_string(meta->general->bands) &&
      strlen(meta->general->bands)               &&
      meta->general->band_count > 0)
  {
    band_names = 
      extract_band_names(meta->general->bands, meta->general->band_count);
  }
  else {
    if (meta->general->band_count <= 0) meta->general->band_count = 1;
    band_names = (char **) MALLOC (meta->general->band_count * sizeof(char *));
    int i;
    for (i=0; i<meta->general->band_count; i++) {
      band_names[i] = (char *) MALLOC (64 * sizeof(char));
      sprintf(band_names[i], "%02d", i);
    }
  }
  meta->stats = meta_statistics_init(meta->general->band_count);
  stat_parameters *stats = 
    (stat_parameters *) MALLOC(sizeof(stat_parameters)*meta->stats->band_count);
  int ii, band;
  double min, max, mean, rmse, stdDev, percentValid;
  gsl_histogram *hist;
  for (band = 0; band < meta->stats->band_count; band++) {
    strcpy(meta->stats->band_stats[band].band_id, band_names[band]);
    calc_stats_rmse_from_file_ext(file_name, band_names[band], mask, &min, &max,
                                  &mean, &stdDev, &rmse, &percentValid, &hist);
    stats->min = min;
    stats->max = max;
    stats->mean = mean;
    stats->rmse = rmse;
    stats->std_deviation = stdDev;
    stats->percent_valid = percentValid;
    stats->mask = mask;
    for (ii=0; ii<256; ii++)
      stats[band].histogram[ii] = hist->bin[ii];
    meta->stats->band_stats[band].min = min;
    meta->stats->band_stats[band].max = max;
    meta->stats->band_stats[band].mean = mean;
    meta->stats->band_stats[band].rmse = rmse;
    meta->stats->band_stats[band].std_deviation = stdDev;
    meta->stats->band_stats[band].percent_valid = percentValid;
    meta->stats->band_stats[band].mask = mask;
  }
  if (band_names) {
    for (ii=0; ii<meta->general->band_count; ii++) {
      if (band_names[ii]) 
        FREE (band_names[ii]);
    }
    FREE(band_names);
  }

  asfPrintStatus("\nStatistics found:\n");
  if (mask_flag)
    asfPrintStatus("Used mask %-16.11g\n",mask);
  asfPrintStatus("Number of bands: %d\n", meta->stats->band_count);
  for (band=0; band<meta->stats->band_count; band++) {
    asfPrintStatus("\n\nBand name = \"%s\"\n", 
      meta->stats->band_stats[band].band_id);
    asfPrintStatus("Minimum = %-16.11g\n", meta->stats->band_stats[band].min);
    asfPrintStatus("Maximum = %-16.11g\n", meta->stats->band_stats[band].max);
    asfPrintStatus("Mean = %-16.11g\n", meta->stats->band_stats[band].mean);
    asfPrintStatus("Root mean squared error = %-16.11g\n", 
      meta->stats->band_stats[band].rmse);
    asfPrintStatus("Standard deviation = %-16.11g\n", 
      meta->stats->band_stats[band].std_deviation);
    asfPrintStatus("Percent of valid values = %-16.11g\n",
      meta->stats->band_stats[band].percent_valid);
    asfPrintStatus("Histogram:\n");
    for (ii=0; ii<256; ii++) {
      if (ii%8 == 0)
        asfPrintStatus("%s%3i-%3i:", (ii==0) ? "" : "\n", ii, ii+7);
      asfPrintStatus(" %8i", stats[band].histogram[ii]);
    }
    asfPrintStatus("\n");
  }

  if (!nometa_flag)
    meta_write(meta, file_name);
  if (!nostat_flag)
    stat_write(stats, stat_name, meta->stats->band_count);
  meta_free(meta);

  asfPrintStatus("\nStatistics taken on image file %s.\n",sar_name);
  if (!nometa_flag)
    asfPrintStatus("Statistics written to the stats block in %s.\n", file_name);
  if (!nostat_flag)
    asfPrintStatus("Statistics plus histogram written to %s.\n\n", stat_name);

  return 0;
}
