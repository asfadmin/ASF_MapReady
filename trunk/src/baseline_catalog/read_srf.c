#include "asf_baseline.h"
#include "parse_funcs.h"

void read_srf(char *input_dir, char *inFile, 
	      struct srf_orbit **srf_orbits, int *nOrbits)
{
  FILE *fp, *fpList;
  struct srf_orbit *srf_orbit;
  int i, k, n, nSegments, nFrames, nTotalFrames, sequence;
  char sensor[5], file_name[50], line[1024], list[1024], srf[1024];

  // Check out how many frames we need to allocate memory for
  nTotalFrames = 0;
  fpList = FOPEN(inFile, "r");
  while (fgets(list, 1024, fpList)) {
    sscanf(list, "%d %s", &nFrames, file_name);
    nTotalFrames += nFrames;
  }
  FCLOSE(fpList);
  
  // Allocate memory
  srf_orbit = 
    (struct srf_orbit *) MALLOC(sizeof(struct srf_orbit)*nTotalFrames);

  // Check out scan results files for given track
  n = 0;
  fpList = FOPEN(inFile, "r");
  while (fgets(list, 1024, fpList)) {
    
    sscanf(list, "%d %s", &nFrames, file_name);
    sprintf(srf, "%s/%s", input_dir, file_name);
    //printf("File: '%s', frames: %d\n", file_name, nFrames);
    fp = FOPEN(srf, "r");
    while (fgets(line, 1024, fp)) {
      
      // Look for sequence number
      if (strstr(line, "SEQUENCE"))
	sscanf(line, "  SEQUENCE = %i", &sequence);
      if (strstr(line, "PLATFORM") &&
	  strncmp(line, "  PLATFORM", 10) == 0) {
	sscanf(line, "  PLATFORM = \"%s\"", sensor);
	sensor[2] = 0;
      }
      
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

  FCLOSE(fpList);

  *nOrbits = nTotalFrames;
  *srf_orbits = srf_orbit;
}

void read_palsar(char *inFile, struct srf_orbit **srf_orbits, int *nOrbits)
{
  FILE *fp;
  struct srf_orbit *srf_orbit;
  int n, version;
  char timeStr[25], line[4096], orbit_dir[25], versionStr[32];
  
  // Check out how many frames we need to allocate memory for
  int nTotalFrames = 0;

  fp = FOPEN(inFile, "r");
  while (fgets(line, 4096, fp))
    nTotalFrames++;
  FCLOSE(fp);

  // Allocate memory
  srf_orbit = 
    (struct srf_orbit *) MALLOC(sizeof(struct srf_orbit)*nTotalFrames);

  // Reading the csv file
  n = 0;
  fp = FOPEN(inFile, "r");
  while (fgets(line, 4096, fp)) {
    //printf("%s", line);
    sprintf(srf_orbit[n].sensor, "%s", my_get_str(line, 0));
    srf_orbit[n].off_nadir = (float) my_get_double(line, 1);
    srf_orbit[n].orbit = my_get_int(line, 2);
    sprintf(orbit_dir, "%s", my_get_str(line, 3));
    if (strchr(orbit_dir, 'A'))
      srf_orbit[n].orbit_dir = 'A';
    else if (strchr(orbit_dir, 'D'))
      srf_orbit[n].orbit_dir = 'D';
    srf_orbit[n].frame = my_get_int(line, 4);
    sprintf(srf_orbit[n].time, "%s", my_get_str(line, 5));
    srf_orbit[n].c_lat = my_get_double(line, 6);
    srf_orbit[n].c_lon = my_get_double(line, 7);
    srf_orbit[n].ns_lat = my_get_double(line, 8);
    srf_orbit[n].ns_lon = my_get_double(line, 9);
    srf_orbit[n].fs_lat = my_get_double(line, 10);
    srf_orbit[n].fs_lon = my_get_double(line, 11);
    srf_orbit[n].ne_lat = my_get_double(line, 12);
    srf_orbit[n].ne_lon = my_get_double(line, 13);
    srf_orbit[n].fe_lat = my_get_double(line, 14);
    srf_orbit[n].fe_lon = my_get_double(line, 15);
    srf_orbit[n].x = my_get_double(line, 16);
    srf_orbit[n].y = my_get_double(line, 17);
    srf_orbit[n].z = my_get_double(line, 18);
    srf_orbit[n].vx = my_get_double(line, 19);
    srf_orbit[n].vy = my_get_double(line, 20);
    srf_orbit[n].vz = my_get_double(line, 21);
    sprintf(versionStr, "%s", my_get_str(line, 22));
    sscanf(&versionStr[6], "%d", &version);
    
    // No range and Doppler information for ALOS PALSAR.
    srf_orbit[n].range = 0.0;
    srf_orbit[n].doppler = 0.0;
    srf_orbit[n].seq = 0;
    n++;
  }
  FCLOSE(fp);

  *nOrbits = nTotalFrames;
  *srf_orbits = srf_orbit;
}
