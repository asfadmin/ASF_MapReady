#include "asf_baseline.h"

#define VERSION 2.0

void baseline_catalog(char *sensor, char *beam_mode, int orbit,
		      char *input_dir, char *output_dir)
{
  struct dirent *dp;
  DIR *dir;
  FILE *fpTxt, *fpKml, *fpShape;
  int i, k, l, n, track, nOrbits, nPairs, years, *months, *orbits, found, index;
  report_level_t report=REPORT_LEVEL_STATUS;
  struct base_pair *base_pairs=NULL;
  struct srf_orbit *srf_orbit=NULL;
  char cmd[255], today[15], year[5];
  int first = 1996;
  ymd_date date;
  hms_time time;

  asfReport(report, "Calculating baseline catalog for beam mode %s\n\n",
	    beam_mode);

  // Prepare the timeline matrix for the mode index page
  sprintf(today, "%s", date_stamp());
  strncpy(year, &today[7], 4);
  year[4] = 0;
  years = atoi(year) - first + 1;
  months = (int *) MALLOC(sizeof(int)*years*12);
  orbits = (int *) MALLOC(sizeof(int)*years*12*MAX_ORBITS);
  for (i=0; i<years*12*MAX_ORBITS; i++)
    orbits[i] = 0;
  for (i=0; i<years*12; i++)
    months[i] = FALSE;

  // Step through the archive track by track - save plenty of memory
  for (track=1; track<=343; track++) {

    printf("\rTrack: %d", track);
    nOrbits = 0;
    nPairs = 0;

    // Get a list of recent SRFs
    read_srf(input_dir, beam_mode, track, &srf_orbit, &nOrbits);

    // Determine baselines
    if (nOrbits)
      determine_baseline(sensor, beam_mode, track, orbit, srf_orbit, nOrbits,
			 &base_pairs, &nPairs);

    // Generate products
    if (nPairs) {
      generate_products(output_dir, base_pairs, nPairs);
      /*
      for (i=0; i<years; i++)
	for (k=0; k<12; k++) {
	  nOrbits = 0;
	  for (l=0; l<nPairs; l++) {
	    parse_odlTime(base_pairs[l].m_time, &date, &time);
	    if (date.year == i+first && date.month == k+1) {
	      months[i*12+k] = TRUE;
	      found = FALSE;
	      index = (i*12+k)*MAX_ORBITS;
	      for (n=0; n<nOrbits; n++) {
		if (orbits[index+n] == base_pairs[l].master) {
		  found = TRUE;
		  break;
		}
	      }
	      if (!found) {
		orbits[index+nOrbits] = base_pairs[l].master;
		nOrbits++;
	      }
	      assert(nOrbits < MAX_ORBITS);
	    }
	  }
	}
      */
    }

    // Clean up before last track
    if (srf_orbit)
      FREE(srf_orbit);
    if (base_pairs)
      FREE(base_pairs);
    srf_orbit = NULL;
    base_pairs = NULL;
  }

  /*
  // Pack text, KML and shape files for entire mode
  chdir(output_dir);
  fpTxt = FOPEN("txt.lst", "w");
  fpKml = FOPEN("kml.lst", "w");
  fpShape = FOPEN("shape.lst", "w");
  dir = opendir(output_dir);
  while ((dp = readdir(dir)) != NULL) {
    if (strstr(dp->d_name, ".txt"))
      fprintf(fpTxt, "%s\n", dp->d_name);
    if (strstr(dp->d_name, ".kml"))
      fprintf(fpKml, "%s\n", dp->d_name);
    if (strstr(dp->d_name, ".tgz"))
      fprintf(fpShape, "%s\n", dp->d_name);
  }
  closedir(dir);
  FCLOSE(fpTxt);
  FCLOSE(fpKml);
  FCLOSE(fpShape);
  sprintf(cmd, "tar czf %s_%s_txt.tgz -T txt.lst", sensor, beam_mode);
  asfSystem(cmd);
  sprintf(cmd, "tar czf %s_%s_kml.tgz -T kml.lst", sensor, beam_mode);
  asfSystem(cmd);
  sprintf(cmd, "tar czf %s_%s_shape.tgz -T shape.lst", sensor, beam_mode);
  asfSystem(cmd);
  sprintf(cmd, "rm txt.lst kml.lst shape.lst");
  asfSystem(cmd);

  // Generate HTML index page
  month2html(output_dir, sensor, beam_mode, orbits);
  mode2html(output_dir, sensor, beam_mode, months);
  update_index(output_dir);
  */
  printf("\n\n");
}
