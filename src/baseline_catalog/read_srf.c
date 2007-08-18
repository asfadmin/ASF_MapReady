#include "asf_baseline.h"

void read_srf(char *input_dir, char *mode, int track, 
	      struct srf_orbit **srf_orbits, int *nOrbits)
{
  size_t *p;
  struct dirent *dp;
  struct stat statbuf;
  DIR *dir;
  FILE *fp;
  struct srf_orbit *srf_orbit;
  int f, i, k, m, n, file_size, nSegments, nFrames, nTotalFrames;
  int orbit, sequence;
  char file_name[25], **files, line[1024], *s, *sensor;
  char current_file[15], next_file[15];
  double *orbits;

  // Check out the files in current directory
  // First need to get all files of appropriate size before we can check for
  // most recent scan results files
  files = (char **) MALLOC(sizeof(char *)*SIZE);
  for (i=0; i<SIZE; i++)
    files[i] = (char *) MALLOC(sizeof(char)*20);
  orbits = (double *) MALLOC(sizeof(double)*SIZE);
  s = (char *) MALLOC(sizeof(char)*25);
  sensor = (char *) MALLOC(sizeof(char)*10);

  m = 0;
  chdir(input_dir);
  dir = opendir(input_dir);
  while ((dp = readdir(dir)) != NULL) {

    if (stat(dp->d_name, &statbuf) == -1)
      continue;
    sprintf(file_name, "%s", dp->d_name);
    file_size = (int)statbuf.st_size;
    strncpy(s, &file_name[2], 8);
    s[7] = 0;
    orbits[m] = atoi(s);
    s[5] = 0;
    orbit = atoi(s);
    strncpy(s, &file_name[14], 4);
    s[3] = 0;
    orbits[m] += atof(s)/1000;
    if (file_size > 2000 && strlen(file_name) == 19 && strstr(file_name, mode)
	&& track == orbit-((orbit-84)/343)*343-84) {
      sprintf(files[m], "%s", file_name);
      m++; 
    }
  }
  closedir(dir);

  // Sort the file list
  if (m > 0) {
    p = (size_t *) MALLOC(sizeof(size_t)*m);
    gsl_sort_index(p, orbits, 1, m);
  }
  else {
    FREE(s);
    FREE(orbits);
    for (i=0; i<SIZE; i++)
      FREE(files[i]);
    FREE(files);
    *srf_orbits = NULL;
    *nOrbits = 0;
    return;
  }

  strncpy(sensor, files[p[0]], 2);
  sensor[2] = 0;

  // Check out how many frames we need to allocate memory for
  nTotalFrames = 0;
  for (f=0; f<m; f++) {
    strncpy(current_file, files[p[f]], 12);
    current_file[12] = 0;
    if (f < m-1) {
      strncpy(next_file, files[p[f+1]], 12);
      next_file[12] = 0;
    }
    else
      sprintf(next_file, " ");
    if (strcmp(current_file, next_file) != 0) {
      
      fp = FOPEN(files[p[f]], "r");
      while (fgets(line, 1024, fp)) {
	
	// Look for segments
	if (strstr(line, "SEGMENT_COUNT")) {
	  sscanf(line, "  SEGMENT_COUNT = %i", &nSegments);
	  for (i=0; i<nSegments; i++) {
	    
	    while (fgets(line, 1024, fp)) {
	      if (strstr(line, "OBJECT = SEGMENT") &&
		  strncmp(line, "  OBJECT = SEGMENT", 18)==0) {
		
		// Look for frames
		nFrames = 0;
		while (fgets(line, 1024, fp)) {
		  if (strstr(line, "FRAME_COUNT")) {
		    sscanf(line, "   FRAME_COUNT = %i", &nFrames);
		    nTotalFrames += nFrames;
		    break;
		  }
		  /*
		  for (k=0; k<nFrames; k++) {
		    while (fgets(line, 1024, fp)) {
		      if (strstr(line, "OBJECT = FRAME") &&
			  strncmp(line, "   OBJECT = FRAME", 17)==0) {
			while (fgets(line, 1024, fp)) {
			  if (strstr(line, "END_OBJECT = FRAME") &&
			      strncmp(line, "   END_OBJECT = FRAME", 21)==0) 
			    break;
			} // while inside OBJECT = FRAME
			
			if (strstr(line, "END_OBJECT = FRAME") &&
			    strncmp(line, "   END_OBJECT = FRAME", 21)==0) 
			  break;
			
		      } // if within a FRAME object
		      
		      if (strstr(line, "END_OBJECT = FRAME") &&
			  strncmp(line, "   END_OBJECT = FRAME", 21)==0) 
			break;
		      
		    } // while looking for FRAME objects
		  
		  } // looping through all the frames (for)
		  */
		} // while inside OBJECT = SEGMENT
	      
		if (strstr(line, "END_OBJECT = SEGMENT") &&
		    strncmp(line, "  END_OBJECT = SEGMENT", 22)==0) 
		  break;
		
	      } // if within a SEGMENT object

	    } // while looking for SEGMENTS objects
	    
	  } // looping through a SEGMENT object (for)

	} // if SEGMENT_COUNT was found
	
      } // while reading an individual scan results file
      
      FCLOSE(fp);
      
    } // if scan results file is recent
  
  } // while reading through list of scan results files (for loop)
  
  // Allocate memory for SRF structure array
  srf_orbit = 
    (struct srf_orbit *) MALLOC(sizeof(struct srf_orbit)*nTotalFrames);

  // Check out scan results files for given track
  n = 0;
  for (f=0; f<m; f++) {
    strncpy(current_file, files[p[f]], 12);
    current_file[12] = 0;
    if (f < m-1) {
      strncpy(next_file, files[p[f+1]], 12);
      next_file[12] = 0;
    }
    else
      sprintf(next_file, " ");
    if (strcmp(current_file, next_file) != 0) {
      
      //printf("Reading %s ...\n", files[p[f]]);
      
      fp = FOPEN(files[p[f]], "r");
      while (fgets(line, 1024, fp)) {
	
	// Look for sequence number
	if (strstr(line, "SEQUENCE"))
	  sscanf(line, "  SEQUENCE = %i", &sequence);
	
	// Look for segments
	if (strstr(line, "SEGMENT_COUNT")) {
	  sscanf(line, "  SEGMENT_COUNT = %i", &nSegments);
	  for (i=0; i<nSegments; i++) {
	    
	    while (fgets(line, 1024, fp)) {
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
			strcpy(srf_orbit[n].sensor, sensor);
			srf_orbit[n].seq = sequence;
			while (fgets(line, 1024, fp)) {
			  if (strstr(line, "END_OBJECT = FRAME") &&
			      strncmp(line, "   END_OBJECT = FRAME", 21)==0) 
			    break;
			  else if (strstr(line, "FRAME_ID")) 
			    sscanf(line, "    FRAME_ID = %i", 
				   &srf_orbit[n].frame);
			  else if (strstr(line, "CENTER_TIME"))
			    sscanf(line, "    CENTER_TIME = %s", 
				   srf_orbit[n].time);
			  else if (strstr(line, "CENTER_LAT"))
			    sscanf(line, "    CENTER_LAT = %f", 
				   &srf_orbit[n].c_lat);
			  else if (strstr(line, "CENTER_LON"))
			    sscanf(line, "    CENTER_LON = %f", 
				   &srf_orbit[n].c_lon);
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
			    sscanf(line, "      REVOLUTION = %i", 
				   &srf_orbit[n].orbit);
			  else if (strstr(line, "X_POSITION"))
			    sscanf(line, "      X_POSITION = %lf", 
				   &srf_orbit[n].x);
			  else if (strstr(line, "Y_POSITION"))
			    sscanf(line, "      Y_POSITION = %lf", 
				   &srf_orbit[n].y);
			  else if (strstr(line, "Z_POSITION"))
			    sscanf(line, "      Z_POSITION = %lf", 
				   &srf_orbit[n].z);
			  else if (strstr(line, "X_VELOCITY"))
			    sscanf(line, "      X_VELOCITY = %lf", 
				   &srf_orbit[n].vx);
			  else if (strstr(line, "Y_VELOCITY"))
			    sscanf(line, "      Y_VELOCITY = %lf", 
				   &srf_orbit[n].vy);
			  else if (strstr(line, "Z_VELOCITY"))
			    sscanf(line, "      Z_VELOCITY = %lf", 
				   &srf_orbit[n].vz);
			  else if (strstr(line, "DOPPLER_FREQ"))
			    sscanf(line, "    DOPPLER_FREQ = (%lf", 
				   &srf_orbit[n].doppler);	      
			  
			} // while inside OBJECT = FRAME
			n++;
			
			if (strstr(line, "END_OBJECT = FRAME") &&
			    strncmp(line, "   END_OBJECT = FRAME", 21)==0) 
			  break;

		      } // if within a FRAME object
		      
		      if (strstr(line, "END_OBJECT = FRAME") &&
			  strncmp(line, "   END_OBJECT = FRAME", 21)==0) 
			break;
		    
		    } // while looking for FRAME objects
		    
		  } // looping through all the frames (for)
		  
		} // while inside OBJECT = SEGMENT
	      
		if (strstr(line, "END_OBJECT = SEGMENT") &&
		    strncmp(line, "  END_OBJECT = SEGMENT", 22)==0) 
		  break;

	      } // if within a SEGMENT object

	    } // while looking for SEGMENT objects
	    
	  } // looping through a SEGMENT object (for)

	} // if SEGMENT_COUNT was found
	  
      } // while reading an individual scan results file
    
      FCLOSE(fp);
    
    } // if scan results file is recent
  
  } // while reading through list of scan results files (for loop)

  FREE(p);
  FREE(s);
  FREE(orbits);
  for (i=0; i<SIZE; i++)
    FREE(files[i]);
  FREE(files);

  *nOrbits = nTotalFrames;
  *srf_orbits = srf_orbit;
}
