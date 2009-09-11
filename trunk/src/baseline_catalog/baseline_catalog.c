#include "asf_baseline.h"

#define VERSION 2.0

void baseline_catalog(char *sensor, char *beam_mode, char *input_dir, 
		      char *output_dir)
{
  struct dirent *dp;
  DIR *dir;
  FILE *fpTxt, *fpKml, *fpShape, *fpDB;
  int i, track, nOrbits, nFrames, nPairs, nTracks, orbit;
  report_level_t report = REPORT_LEVEL_STATUS;
  struct base_pair *base_pairs=NULL;
  struct srf_orbit *srf_orbit=NULL;
  char cmd[255], tmp_dir[1024], track_list[1024];
  char m_sensor[10], s_sensor[10];
  char phases[] = { 'A', 'B', 'C', 'D', 'F', 'G' };
  int tracks[] = { 43, 43, 501, 43, 1784, 501 };

  // Some more error checking on beam modes
  if (strcmp_case(sensor, "R1") == 0) {
    if (strcmp_case(beam_mode, "FN1") != 0 &&
	strcmp_case(beam_mode, "FN2") != 0 &&
	strcmp_case(beam_mode, "FN3") != 0 &&
	strcmp_case(beam_mode, "FN4") != 0 &&
	strcmp_case(beam_mode, "FN5") != 0 &&
	strcmp_case(beam_mode, "ST1") != 0 &&
	strcmp_case(beam_mode, "ST2") != 0 &&
	strcmp_case(beam_mode, "ST3") != 0 &&
	strcmp_case(beam_mode, "ST4") != 0 &&
	strcmp_case(beam_mode, "ST5") != 0 &&
	strcmp_case(beam_mode, "ST6") != 0 &&
	strcmp_case(beam_mode, "ST7") != 0)
      asfPrintError("Unknown beam mode '%s' for sensor '%s'.\n",
		    beam_mode, sensor);
  }
  else if ((strcmp_case(sensor, "E1") == 0 || 
	    strcmp_case(sensor, "E2") == 0) &&
	   strcmp_case(beam_mode, "STD") != 0)
    asfPrintError("Unknown beam mode '%s' for sensor '%s'.\n",
		  beam_mode, sensor);
  else if (strcmp_case(sensor, "J1") == 0 && 
	   strcmp_case(beam_mode, "STD") != 0)
    asfPrintError("Unknown beam mode '%s' for sensor '%s'.\n",
		  beam_mode, sensor);
  else if (strcmp_case(sensor, "PALSAR") == 0 &&
	   strcmp_case(beam_mode, "PLR") != 0)
    asfPrintError("Unknown beam mode '%s' for sensor '%s'.\n",
		  beam_mode, sensor);

  // Report what is going on
  asfReport(report, "Calculating baseline catalog for satellite '%s' in "
	    "beam mode '%s'\n\n", sensor, beam_mode);

  // Create temporary directory for list files
  sprintf(tmp_dir, "%s/%s", output_dir, time_stamp_dir());
  create_clean_dir(tmp_dir);

  // Setup the baseline calculation
  setup_files(sensor, beam_mode, input_dir, tmp_dir, &nTracks, &nFrames);
     
  // Step through the archive track by track - save plenty of memory
  for (track=0; track<nTracks; track++) {
    asfPrintStatus("\nTrack: %d (out of %d)\n", track+1, nTracks);
    nOrbits = 0;
    nPairs = 0;
    
    // Get a list of recent SRFs
    if (strcmp(sensor, "PSR") == 0)
      asfPrintStatus("Reading metadata file ...\n");
    else
      asfPrintStatus("Reading SRFs ...\n");
    sprintf(track_list, "%s/%s_track%d.lst", tmp_dir, sensor, track);
    if (fileExists(track_list)) {
      if (strcmp(sensor, "PSR") == 0)
	read_palsar(track_list, &srf_orbit, &nOrbits);
      else
	read_srf(input_dir, track_list, &srf_orbit, &nOrbits);
    }
    else
      continue;
    
    // Determine baselines
    if (nOrbits) {
      asfPrintStatus("Determining baselines ...\n");
      determine_baseline(sensor, sensor, beam_mode, track, orbit, 
			 srf_orbit, nOrbits, &base_pairs, &nPairs);
    }
    
    // Generate products
    if (nPairs) {
      asfPrintStatus("Generate products ...\n");
      generate_products(output_dir, base_pairs, nPairs);
    }
    
    // Clean up before last track
    if (srf_orbit)
      FREE(srf_orbit);
    if (base_pairs)
      FREE(base_pairs);
    srf_orbit = NULL;
    base_pairs = NULL;
    //exit(0);
  }

  // Pack text, KML and shape files for entire mode
  chdir(output_dir);
  fpTxt = FOPEN("txt.lst", "w");
  fpKml = FOPEN("kml.lst", "w");
  fpShape = FOPEN("shape.lst", "w");
  fpDB = FOPEN("db.lst", "w");
  dir = opendir(output_dir);
  while ((dp = readdir(dir)) != NULL) {
    if (strstr(dp->d_name, ".txt"))
      fprintf(fpTxt, "%s\n", dp->d_name);
    if (strstr(dp->d_name, ".kml"))
      fprintf(fpKml, "%s\n", dp->d_name);
    if (strstr(dp->d_name, ".tgz"))
      fprintf(fpShape, "%s\n", dp->d_name);
    if (strstr(dp->d_name, ".db") && !strstr(dp->d_name, ".dbf"))
      fprintf(fpDB, "%s\n", dp->d_name);
  }
  closedir(dir);
  FCLOSE(fpTxt);
  FCLOSE(fpKml);
  FCLOSE(fpShape);
  FCLOSE(fpDB);
  printf("\nZipping text files ...\n");
  sprintf(cmd, "tar czf %s_%s_txt.tgz -T txt.lst", sensor, beam_mode);
  asfSystem(cmd);
  printf("\nZipping KML files ...\n");
  sprintf(cmd, "tar czf %s_%s_kml.tgz -T kml.lst", sensor, beam_mode);
  asfSystem(cmd);
  printf("\nZipping shape files ...\n");
  sprintf(cmd, "tar czf %s_%s_shape.tgz -T shape.lst", sensor, beam_mode);
  asfSystem(cmd);
  printf("\nZipping database files ...\n");
  sprintf(cmd, "tar czf %s_%s_db.tgz -T db.lst", sensor, beam_mode);
  asfSystem(cmd);
  sprintf(cmd, "rm txt.lst kml.lst shape.lst db.lst");
  asfSystem(cmd);

  printf("\n\n");
}
