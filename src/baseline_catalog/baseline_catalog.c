#include "asf_baseline.h"

#define VERSION 2.0

void baseline_catalog(char *beam_mode)
{
  char *files, *sensor;
  int track, nOrbits, nPairs;
  report_level_t report=STATUS;
  struct base_pair *base_pairs;
  struct srf_orbit *srf_orbit;

  files = (char *) MALLOC(sizeof(char)*255);
  sensor = (char *) MALLOC(sizeof(char)*10);

  asfPrintStatus("Calculating baseline catalog for beam mode %s\n\n", beam_mode);

  sprintf(files, "rsat_baselines_%s", beam_mode);

  // Step through the archive track by track - save plenty of memory
  for (track=7; track<=7; track++) {

    // Get a list of recent SRFs
    asfReport(report, "Searching for SRFs ...\n");
    read_srf(beam_mode, track, srf_orbit, &nOrbits);

    // Determine baselines
    asfReport(report, "Calculating baselines ...\n");
    determine_baseline(sensor, track, srf_orbit, nOrbits, base_pairs, &nPairs);
    
    // Generate products
    asfReport(report, "Generating products ...\n");
    generate_products(base_pairs, nPairs);
  }

  // Clean up
  FREE(base_pairs);
  FREE(files);
  FREE(sensor);
  FREE(beam_mode);
}
