#include "asf_baseline.h"

void read_srf(char *list, int nFiles, char *pairs_list, int nPairs, 
	      char *sensor, char *beam_mode, 
	      struct base_info **srf, struct base_pair **base_pairs)
{
  struct base_info *root_srf, *old_srf, *current_srf;
  struct srf_orbit *root_orbit, *old_orbit, *current_orbit;
  struct base_pair *pairs;
  FILE *fpList=NULL, *fpIn=NULL;
  int nSegments=0, nFrames=0;
  char file[100], line[1024], mode[10];
  int i, k;
  char **files, center_time[30], orbit_dir;
  int seq, frame_number, new;
  int revolution;
  float center_latitude, center_longitude;
  float ns_lat, ns_lon, fs_lat, fs_lon, ne_lat, ne_lon, fe_lat, fe_lon;
  double slant_range, doppler;
  float x_position, y_position, z_position, x_velocity, y_velocity, z_velocity;

  // Read in scan results file list
  files = (char **) MALLOC(20*nFiles*sizeof(char));
  for (i=0; i<nFiles; i++)
    files[i] = (char *) MALLOC (20*sizeof(char));
  fpList = FOPEN(list, "r");
  for (i=0; i<nFiles; i++) {
    fgets(line, 1024, fpList);
    strncpy(files[i], line, 19);
  }
  FCLOSE(fpList);

  // Extract the orbits and frames that need to require baseline information
  // and generate list of orbits for which we need information
  pairs = (struct base_pair *) MALLOC(sizeof(struct base_pair)*nPairs);
  root_srf = (struct base_info *) MALLOC(sizeof(struct base_info));
  root_orbit = (struct srf_orbit *) MALLOC(sizeof(struct srf_orbit));
  root_orbit->orbit = -99;
  old_orbit = root_orbit;

  fpList = FOPEN(pairs_list, "r");
  for (i=0; i<nPairs; i++) {
    fgets(line, 1024, fpList);
    sscanf(line, "%d,%d,%d,%d,%d", &pairs[i].master, &pairs[i].m_seq, 
	   &pairs[i].slave, &pairs[i].s_seq, &pairs[i].frame);
    sprintf(pairs[i].mode, "%s", beam_mode);
    if (pairs[i].m_seq >= 10) 
      sprintf(file, "%s%d%d", sensor, pairs[i].master, pairs[i].m_seq);
    else
      sprintf(file, "%s%d0%d", sensor, pairs[i].master, pairs[i].m_seq);
    for (k=0; k<nFiles; k++) {
      if (strstr(files[k], file) != NULL && 
	  (old_orbit->orbit != pairs[i].master ||
	   old_orbit->seq != pairs[i].m_seq)) {
	if (i==0) {
	  root_orbit->orbit = pairs[0].master;
	  root_orbit->seq = pairs[0].m_seq;
	  root_orbit->index = k;
	}
	else {
	  current_orbit = (struct srf_orbit *) MALLOC(sizeof(struct srf_orbit));
	  old_orbit->next = current_orbit;
	  current_orbit->orbit = pairs[i].master;
	  current_orbit->seq = pairs[i].m_seq;
	  current_orbit->index = k;
	  old_orbit = current_orbit;
	}
      }
    }
  }
  old_orbit->next = NULL;
  FCLOSE(fpList);

  /* List the orbits (for debugging)  
  current_orbit = root_orbit;
  new = TRUE;
  while (current_orbit->next) {
    if (current_orbit->next && !new)
      current_orbit = current_orbit->next;
    if (new)
      new = FALSE;
    printf("Orbit: %d, File: %s\n", 
	   current_orbit->orbit, files[current_orbit->index]);
  }*/

  // Go through the list of orbits for which we need information
  new = TRUE;
  current_orbit = root_orbit;
  do {

    fpIn = FOPEN(files[current_orbit->index], "r");
    while (fgets(line, 1024, fpIn)) {

      // Look for sequence number
      if (strstr(line, "SEQUENCE"))
        sscanf(line, "  SEQUENCE = %i", &seq);
      
      // Look for segments
      if (strstr(line, "SEGMENT_COUNT")) 
	sscanf(line, "  SEGMENT_COUNT = %i", &nSegments);
      for (i=0; i<nSegments; i++) {
	if (strstr(line, "OBJECT = SEGMENT") &&
            strncmp(line, "  OBJECT = SEGMENT", 18)==0) {
	  
	  // Check beam mode
	  fgets(line, 1024, fpIn);
	  fgets(line, 1024, fpIn);
	  if (strstr(line, "MODE")) {
	    sscanf(line, "   MODE = \"%s\"", mode);
	    mode[3] = '\0';
	  }
	  // Look for frames
	  while (fgets(line, 1024, fpIn)) {
	    if (strstr(line, "FRAME_COUNT")) 
	      sscanf(line, "   FRAME_COUNT = %i", &nFrames);
	    for (k=0; k<nFrames; k++) {
	      while (fgets(line, 1024, fpIn)) {
		if (strstr(line, "OBJECT = FRAME") &&
		  strncmp(line, "   OBJECT = FRAME", 17)==0) {
		  while (fgets(line, 1024, fpIn)) {
		    if (strstr(line, "END_OBJECT = FRAME") &&
			strncmp(line, "   END_OBJECT = FRAME", 21)==0) break;
		    else if (strstr(line, "FRAME_ID"))
		      sscanf(line, "    FRAME_ID = %i", &frame_number);
		    else if (strstr(line, "CENTER_TIME"))
		      sscanf(line, "    CENTER_TIME = %s", center_time);
		    else if (strstr(line, "CENTER_LAT"))
		      sscanf(line, "    CENTER_LAT = %f", &center_latitude);
		    else if (strstr(line, "CENTER_LON"))
		      sscanf(line, "    CENTER_LON = %f", &center_longitude);
		    else if (strstr(line, "NEAR_START_LAT"))
		      sscanf(line, "    NEAR_START_LAT = %f", &ns_lat);
		    else if (strstr(line, "NEAR_START_LON"))
		      sscanf(line, "    NEAR_START_LON = %f", &ns_lon);
		    else if (strstr(line, "FAR_START_LAT"))
		      sscanf(line, "    FAR_START_LAT = %f", &fs_lat);
		    else if (strstr(line, "FAR_START_LON"))
		      sscanf(line, "    FAR_START_LON = %f", &fs_lon);
		    else if (strstr(line, "NEAR_END_LAT"))
		      sscanf(line, "    NEAR_END_LAT = %f", &ne_lat);
		    else if (strstr(line, "NEAR_END_LON"))
		      sscanf(line, "    NEAR_END_LON = %f", &ne_lon);
		    else if (strstr(line, "FAR_END_LAT"))
		      sscanf(line, "    FAR_END_LAT = %f", &fe_lat);
	 	    else if (strstr(line, "FAR_END_LON"))
		      sscanf(line, "    FAR_END_LON = %f", &fe_lon);
		    else if (strstr(line, "SL_RNG_MID_PIX"))
		      sscanf(line, "    SL_RNG_MID_PIX = %lf", &slant_range);
		    else if (strstr(line, "ASC_DESC"))
		      sscanf(line, "    ASC_DESC = \"%c\"", &orbit_dir);
		    else if (strstr(line, "REVOLUTION")) 
	 	      sscanf(line, "      REVOLUTION = %i", &revolution);
		    else if (strstr(line, "X_POSITION"))
		      sscanf(line, "      X_POSITION = %f", &x_position);
		    else if (strstr(line, "Y_POSITION"))
		      sscanf(line, "      Y_POSITION = %f", &y_position);
		    else if (strstr(line, "Z_POSITION"))
		      sscanf(line, "      Z_POSITION = %f", &z_position);
		    else if (strstr(line, "X_VELOCITY"))
		      sscanf(line, "      X_VELOCITY = %f", &x_velocity);
		    else if (strstr(line, "Y_VELOCITY"))
		      sscanf(line, "      Y_VELOCITY = %f", &y_velocity);
		    else if (strstr(line, "Z_VELOCITY"))
		      sscanf(line, "      Z_VELOCITY = %f", &z_velocity);
		    else if (strstr(line, "DOPPLER_FREQ"))
		      sscanf(line, "    DOPPLER_FREQ = (%lf", &doppler);

		  } // while inside OBJECT = FRAME
  
		  if (new) {
		    current_srf = root_srf;
		    new = FALSE;
		  }
		  else {
		    current_srf =
		      (struct base_info *) MALLOC(sizeof(struct base_info));
		    old_srf->next = current_srf;
		  }
		  current_srf->orbit = current_orbit->orbit;
		  current_srf->frame = frame_number;
		  current_srf->seq = seq;
		  current_srf->orbit_dir = orbit_dir;
		  sprintf(current_srf->time, "%s", center_time);
		  current_srf->c_lat = center_latitude;
		  current_srf->c_lon = center_longitude;
		  current_srf->ns_lat = ns_lat;
		  current_srf->ns_lon = ns_lon;
		  current_srf->fs_lat = fs_lat;
		  current_srf->fs_lon = fs_lon;
		  current_srf->ne_lat = ne_lat;
		  current_srf->ne_lon = ne_lon;
		  current_srf->fe_lat = fe_lat;
		  current_srf->fe_lon = fe_lon;
		  current_srf->range = slant_range*1000;
		  current_srf->x = x_position;
		  current_srf->y = y_position;
		  current_srf->z = z_position;
		  current_srf->vx = x_velocity;
		  current_srf->vy = y_velocity;
		  current_srf->vz = z_velocity;
		  current_srf->doppler = doppler;
		  old_srf = current_srf;
	      
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

    }  // while reading an individual scan results file
    FCLOSE(fpIn);
    if (current_orbit->next)
      current_orbit = current_orbit->next;
    else
      break;
    
  } while (current_orbit); // while going through list of orbits 
  old_srf->next = NULL;

  // clean up
  for (i=0; i<nFiles; i++)
    FREE(files[i]);
  FREE(files);

  *srf = root_srf;
  *base_pairs = pairs;
}
