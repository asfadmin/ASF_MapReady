#include "asf_baseline.h"

void read_srf(char *mode, int track, struct srf_orbit *srf_orbit, int *nOrbits)
{
  struct dirent *dp;
  struct stat statbuf;
  DIR *dir;
  report_level_t report=STATUS;
  FILE *fp;
  int f, i, k, m, n, file_size, nSegments, nFrames, orbit;
  char file_name[25], **files, line[1024], *s, *sensor;
  char current_file[15], next_file[15];

  // Check out the files in current directory
  // First need to get all files of appropriate size before we can check for
  // most recent scan results files
  files = (char **) MALLOC(sizeof(char *)*SIZE);
  for (i=0; i<SIZE; i++)
    files[i] = (char *) MALLOC(sizeof(char)*20);
  s = (char *) MALLOC(sizeof(char)*25);

  asfReport(report, "Searching current directory for %s scan results on track %d"
	    " ...\n", mode, track);
  m = 0;
  dir = opendir(".");
  while ((dp = readdir(dir)) != NULL) {

    if (stat(dp->d_name, &statbuf) == -1)
      continue;
    sprintf(file_name, "%s", dp->d_name);
    file_size = (int)statbuf.st_size;
    sprintf(s, "%s", &file_name[2]);
    s[5] = '\0';
    orbit = atoi(s);
    if (file_size > 2000 && strlen(file_name) == 19 && strstr(file_name, mode) && 
	track == orbit-(orbit/343)*343) {
      sprintf(files[m], "%s", file_name);
      printf("file name: %s, file size: %d\n", file_name, file_size);
      m++; 
    }
  }
  closedir(dir);

  sensor = (char *) MALLOC(sizeof(char)*10);
  sprintf(sensor, "%s", files[0]);
  sensor[2] = '\0';
  
  // Check out scan results files for given track
  asfReport(report, "Searching through %d scan results files "
	    "for most recent ones ...\n", m);
  n = 0;
  for (f=0; f<m; f++) {
    if (f==14)
      printf("Boo\n");
    strncpy(current_file, files[f], 12);
    current_file[12] = '\0';
    if (f < m-1) {
      strncpy(next_file, files[f+1], 12);
      next_file[12] = '\0';
    }
    else
      sprintf(next_file, " ");
    if (strcmp(current_file, next_file) != 0) {
      
      printf("Reading %s ...\n", files[f]);
      
      fp = FOPEN(files[f], "r");
      sprintf(srf_orbit[n].sensor, "%s", sensor);
      while (fgets(line, 1024, fp)) {
	
	// Look for sequence number
	if (strstr(line, "SEQUENCE"))
	  sscanf(line, "  SEQUENCE = %i", &srf_orbit[n].seq);
	
	// Look for segments
	if (strstr(line, "SEGMENT_COUNT")) 
	  sscanf(line, "  SEGMENT_COUNT = %i", &nSegments);
	for (i=0; i<nSegments; i++) {
	  if (strstr(line, "OBJECT = SEGMENT") &&
	      strncmp(line, "  OBJECT = SEGMENT", 18)==0) {
	    
	    // Look for frames
	    nFrames = 0;
	    while (fgets(line, 1024, fp)) {
	      if (strstr(line, "FRAME_COUNT")) 
		sscanf(line, "   FRAME_COUNT = %i", &nFrames);
	      for (k=0; k<nFrames; k++) {
		while (fgets(line, 1024, fp)) {
		  if (strstr(line, "OBJECT = FRAME") &&
		      strncmp(line, "   OBJECT = FRAME", 17)==0) {
		    while (fgets(line, 1024, fp)) {
		      if (strstr(line, "END_OBJECT = FRAME") &&
			  strncmp(line, "   END_OBJECT = FRAME", 21)==0) break;
		      else if (strstr(line, "FRAME_ID")) 
			sscanf(line, "    FRAME_ID = %i", &srf_orbit[n].frame);
		      else if (strstr(line, "CENTER_TIME"))
			sscanf(line, "    CENTER_TIME = %s", srf_orbit[n].time);
		      else if (strstr(line, "CENTER_LAT"))
			sscanf(line, "    CENTER_LAT = %f", &srf_orbit[n].c_lat);
		      else if (strstr(line, "CENTER_LON"))
			sscanf(line, "    CENTER_LON = %f", &srf_orbit[n].c_lon);
		      else if (strstr(line, "NEAR_START_LAT"))
			sscanf(line, "    NEAR_START_LAT = %f", 
			       &srf_orbit[n].ns_lat);
		      else if (strstr(line, "NEAR_START_LON"))
			sscanf(line, "    NEAR_START_LON = %f", 
			       &srf_orbit[n].ns_lon);
		      else if (strstr(line, "FAR_START_LAT"))
			sscanf(line, "    FAR_START_LAT = %f", 
			       &srf_orbit[n].fs_lat);
		      else if (strstr(line, "FAR_START_LON"))
			sscanf(line, "    FAR_START_LON = %f", 
			       &srf_orbit[n].fs_lon);
		      else if (strstr(line, "NEAR_END_LAT"))
			sscanf(line, "    NEAR_END_LAT = %f", 
			       &srf_orbit[n].ne_lat);
		      else if (strstr(line, "NEAR_END_LON"))
			sscanf(line, "    NEAR_END_LON = %f", 
			       &srf_orbit[n].ne_lon);
		      else if (strstr(line, "FAR_END_LAT"))
			sscanf(line, "    FAR_END_LAT = %f", 
			       &srf_orbit[n].fe_lat);
		      else if (strstr(line, "FAR_END_LON"))
			sscanf(line, "    FAR_END_LON = %f", 
			       &srf_orbit[n].fe_lon);
		      else if (strstr(line, "SL_RNG_MID_PIX")) {
			sscanf(line, "    SL_RNG_MID_PIX = %lf", 
			       &srf_orbit[n].range);
			srf_orbit[n].range *= 1000;
		      }
		      else if (strstr(line, "ASC_DESC"))
			sscanf(line, "    ASC_DESC = \"%c\"", 
			       &srf_orbit[n].orbit_dir);
		      else if (strstr(line, "REVOLUTION")) 
			sscanf(line, "      REVOLUTION = %i", &srf_orbit[n].orbit);
		      else if (strstr(line, "X_POSITION"))
			sscanf(line, "      X_POSITION = %lf", &srf_orbit[n].x);
		      else if (strstr(line, "Y_POSITION"))
			sscanf(line, "      Y_POSITION = %lf", &srf_orbit[n].y);
		      else if (strstr(line, "Z_POSITION"))
			sscanf(line, "      Z_POSITION = %lf", &srf_orbit[n].z);
		      else if (strstr(line, "X_VELOCITY"))
			sscanf(line, "      X_VELOCITY = %lf", &srf_orbit[n].vx);
		      else if (strstr(line, "Y_VELOCITY"))
			sscanf(line, "      Y_VELOCITY = %lf", &srf_orbit[n].vy);
		      else if (strstr(line, "Z_VELOCITY"))
			sscanf(line, "      Z_VELOCITY = %lf", &srf_orbit[n].vz);
		      else if (strstr(line, "DOPPLER_FREQ"))
			sscanf(line, "    DOPPLER_FREQ = (%lf", 
			       &srf_orbit[n].doppler);	      

		    } // while inside OBJECT = FRAME
		    n++;
		    
		    if (strstr(line, "END_OBJECT = FRAME") &&
			strncmp(line, "   END_OBJECT = FRAME", 21)==0) break;
		  } // if within a FRAME object
		  
		  if (strstr(line, "END_OBJECT = FRAME") &&
		      strncmp(line, "   END_OBJECT = FRAME", 21)==0) break;

		} // while looking for FRAME objects
		
	      } // looping through all the frames (for)
	      
	    } // while inside OBJECT = SEGMENT
	    
	    if (strstr(line, "END_OBJECT = SEGMENT") &&
		strncmp(line, "  END_OBJECT = SEGMENT", 22)==0) break;
	  } // if within a SEGMENT object
	  
	} // looping through a SEGMENT object (for)
	
      } // while reading an individual scan results file
    
      FCLOSE(fp);
    
    } // if scan results file is recent
  
  } // while reading through list of scan results files (for loop)

  FREE(s);
  for (i=0; i<SIZE; i++)
    FREE(files[i]);
  FREE(files);

  *nOrbits = n;

  /*
  // Write out structure for debugging
  for (i=0; i<n; i++)
    printf("Orbit: %d, frame: %d\n", srf_orbit[i].orbit, srf_orbit[i].frame);
  exit(0);  
  */
}
