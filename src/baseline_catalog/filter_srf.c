#include "asf_baseline.h"

void read_srf(char *mode, int track, struct srf_orbit **srf_orbit)
{
  struct dirent *dp;
  struct stat statbuf;
  struct srf_orbit *root_srf, *current_srf, *old_srf;
  struct srf_frame *root_frame, *current_frame, *old_frame;
  DIR *dir;
  FILE *fp;
  int i,k, new = TRUE, file_size, nSegments, nFrames, orbit;
  char file_name[25], line[1024], *s, *sensor; 

  // Check out the files in current directory
  root_srf = (struct srf_orbit *) MALLOC(sizeof(struct srf_orbit));
  current_srf = root_srf;

  s = (char *) MALLOC(sizeof(char)*25);
  
  dir = opendir(".");
  while ((dp = readdir(dir)) != NULL) {

    if (stat(dp->d_name, &statbuf) == -1)
      continue;
    sprintf(file_name, "%s", dp->d_name);
    file_size = (int)statbuf.st_size;
    if (file_size > 2000 && strlen(file_name) == 19 && strstr(file_name, mode)) {

      sprintf(sensor, "%s", file_name);
      sensor[2] = '\0';
      sprintf(s, "%s", &file_name[2]);
      s[5] = '\0';
      orbit = atoi(s);
      if (track == orbit-(orbit/343)*343) {
	if (new)
	  new = FALSE;
	else {
	  current_srf = (struct srf_orbit *) MALLOC(sizeof(struct srf_orbit));
	  old_srf->next = current_srf;
	}
	root_frame = (struct srf_frame *) MALLOC(sizeof(struct srf_frame));
	current_srf->frame = root_frame;
	old_srf = current_srf;
	sprintf(current_srf->sensor, "%s", sensor);
	current_srf->orbit = orbit;
	
	fp = FOPEN(file_name, "r");
	while (fgets(line, 1024, fp)) {
	  
	  // Look for sequence number
	  if (strstr(line, "SEQUENCE"))
	    sscanf(line, "  SEQUENCE = %i", &current_srf->seq);
	  
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
			else if (strstr(line, "FRAME_ID")) {
			  if (k>0) {
			    current_frame = 
			      (struct srf_frame *) MALLOC(sizeof(struct srf_frame));
			    old_frame->next = current_frame;
			  }
			  sscanf(line, "    FRAME_ID = %i", &current_frame->frame);
			  old_frame = current_frame;
			}
			else if (strstr(line, "CENTER_TIME"))
			  sscanf(line, "    CENTER_TIME = %s", current_frame->time);
			else if (strstr(line, "CENTER_LAT"))
			  sscanf(line, "    CENTER_LAT = %f", 
				 &current_frame->c_lat);
			else if (strstr(line, "CENTER_LON"))
			  sscanf(line, "    CENTER_LON = %f", 
				 &current_frame->c_lon);
			else if (strstr(line, "NEAR_START_LAT"))
			  sscanf(line, "    NEAR_START_LAT = %f", 
				 &current_frame->ns_lat);
			else if (strstr(line, "NEAR_START_LON"))
			  sscanf(line, "    NEAR_START_LON = %f", 
				 &current_frame->ns_lon);
			else if (strstr(line, "FAR_START_LAT"))
			  sscanf(line, "    FAR_START_LAT = %f", 
				 &current_frame->fs_lat);
			else if (strstr(line, "FAR_START_LON"))
			  sscanf(line, "    FAR_START_LON = %f", 
				 &current_frame->fs_lon);
			else if (strstr(line, "NEAR_END_LAT"))
			  sscanf(line, "    NEAR_END_LAT = %f", 
				 &current_frame->ne_lat);
			else if (strstr(line, "NEAR_END_LON"))
			  sscanf(line, "    NEAR_END_LON = %f", 
				 &current_frame->ne_lon);
			else if (strstr(line, "FAR_END_LAT"))
			  sscanf(line, "    FAR_END_LAT = %f", 
				 &current_frame->fe_lat);
			else if (strstr(line, "FAR_END_LON"))
			  sscanf(line, "    FAR_END_LON = %f", 
				 &current_frame->fe_lon);
			else if (strstr(line, "SL_RNG_MID_PIX")) {
			  sscanf(line, "    SL_RNG_MID_PIX = %lf", 
				 &current_frame->range);
			  current_frame->range *= 1000;
			}
			else if (strstr(line, "ASC_DESC"))
			  sscanf(line, "    ASC_DESC = \"%c\"", 
				 &current_frame->orbit_dir);
			else if (strstr(line, "X_POSITION"))
			  sscanf(line, "      X_POSITION = %lf", 
				 &current_frame->x);
			else if (strstr(line, "Y_POSITION"))
			  sscanf(line, "      Y_POSITION = %lf", 
				 &current_frame->y);
			else if (strstr(line, "Z_POSITION"))
			  sscanf(line, "      Z_POSITION = %lf", 
				 &current_frame->z);
			else if (strstr(line, "X_VELOCITY"))
			  sscanf(line, "      X_VELOCITY = %lf", 
				 &current_frame->vx);
			else if (strstr(line, "Y_VELOCITY"))
			  sscanf(line, "      Y_VELOCITY = %lf", 
				 &current_frame->vy);
			else if (strstr(line, "Z_VELOCITY"))
			  sscanf(line, "      Z_VELOCITY = %lf", 
				 &current_frame->vz);
			else if (strstr(line, "DOPPLER_FREQ"))
			  sscanf(line, "    DOPPLER_FREQ = (%lf", 
				 &current_frame->doppler);
			
		      } // while inside OBJECT = FRAME
		      
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
	current_frame->next = NULL;
	
      } // if orbit is on track

    } // if file size is adequate
  
  } // while reading through list of scan results files
  current_srf->next = NULL;

  closedir(dir);
  FCLOSE(fp);
  FREE(s);

  *srf_orbit = root_srf;
}
