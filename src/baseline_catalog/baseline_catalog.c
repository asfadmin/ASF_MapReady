#include "asf_baseline.h"

#define VERSION 2.0

void baseline_catalog(char *beam_mode)
{
  char *files, *sensor, *pairs;
  int track, nFiles, nPairs;
  report_level_t report=STATUS;
  struct base_info *srf;
  struct base_pair *base_pairs;

  files = (char *) MALLOC(sizeof(char)*255);
  sensor = (char *) MALLOC(sizeof(char)*10);
  pairs = (char *) MALLOC(sizeof(char)*255);

  asfPrintStatus("Calculating baseline catalog for beam mode %s\n\n", beam_mode);

  sprintf(files, "rsat_baselines_%s", beam_mode);

  // Get a list of recent SRFs
  asfReport(report, "Searching for SRFs ...\n");
  filter_srf(files, sensor, beam_mode, &nFiles);
  asfReport(report, "Found %d SRFs for beam mode %s (stored in %s)\n", 
	    nFiles, beam_mode, files);

  // Step through the archive track by track - save plenty of memory
  for (track=84; track<=343; track++) {

    // Find image pairs for track
    asfReport(report, "Working on track %d ...\n", track);
    sprintf(pairs, "%s_%d_pairs", beam_mode, track);
    find_pairs(track, files, pairs, &nPairs);
    asfReport(report, "Found %d pairs for track %d (stored in %s)\n", 
	      nPairs, track, pairs);

    if (nPairs > 0) {
      // Extract information
      asfReport(report, "Reading scan results files ...\n");
      read_srf(files, nFiles, pairs, nPairs, sensor, beam_mode, &srf, &base_pairs);
      
      // Determine baselines
      asfReport(report, "Calculating baselines ...\n");
      determine_baseline(sensor, track, srf, base_pairs, nPairs);
      
      // Generate products
      asfReport(report, "Generating products ...\n");
      generate_products(base_pairs, nPairs);
    }
  }

  // Clean up
  FREE(srf);
  FREE(base_pairs);
  FREE(files);
  FREE(sensor);
  FREE(beam_mode);
  FREE(pairs);
}
