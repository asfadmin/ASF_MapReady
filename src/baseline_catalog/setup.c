#include "asf_baseline.h"
#include "parse_funcs.h"

// ALOS beam modes
static char *alos_beam_mode[]={
  "FBS1","FBS2","FBS3","FBS4","FBS5","FBS6","FBS7","FBS8","FBS9","FBS10", 
  "FBS11","FBS12","FBS13","FBS14","FBS15","FBS16","FBS17","FBS18",
  "FBS1","FBS2","FBS3","FBS4","FBS5","FBS6","FBS7","FBS8","FBS9","FBS10",
  "FBS11","FBS12","FBS13","FBS14","FBS15","FBS16","FBS17","FBS18",
  "FBD1","FBD2","FBD3","FBD4","FBD5","FBD6","FBD7","FBD8","FBD9","FBD10",
  "FBD11","FBD12","FBD13","FBD14","FBD15","FBD16","FBD17","FBD18",
  "FBD1","FBD2","FBD3","FBD4","FBD5","FBD6","FBD7","FBD8","FBD9","FBD10",
  "FBD11","FBD12","FBD13","FBD14","FBD15","FBD16","FBD17","FBD18",
  "WD1","WD2","WD1","WD2","WD1","WD2","WD1","WD2","WD1","WD2","WD1","WD2",
  "DSN1","DSN2","DSN3","DSN4","DSN5","DSN6","DSN7","DSN8","DSN9","DSN10",
  "DSN11","DSN12","DSN13","DSN14","DSN15","DSN16","DSN17","DSN18",
  "DSN1","DSN2","DSN3","DSN4","DSN5","DSN6","DSN7","DSN8","DSN9","DSN10",
  "DSN11","DSN12","DSN13","DSN14","DSN15","DSN16","DSN17","DSN18",
  "PLR1","PLR2","PLR3","PLR4","PLR5","PLR6","PLR7","PLR8","PLR9","PLR10",
  "PLR11","PLR12"};

void get_palsar_info(char *inFile, char *outputDir, char *sensor, char *mode, 
		     int *nFrames)
{
  FILE *fp, *fpList;
  ymd_date ymd;
  julian_date jd;
  hms_time hms;
  char line[4096], list[1024], sensorMeta[10], modeMeta[10], versionStr[500];
  char orbit_dir[5], timeStr[30], time[30];
  int track, orbit, frame, version;
  double off_nadir, c_lat, c_lon, ns_lat, ns_lon, fs_lat, fs_lon, ne_lat;
  double ne_lon, fe_lat, fe_lon, x, y, z, vx, vy, vz;

  int minOrbit = PSR_MIN_ORBIT;
  int maxOrbit = PSR_MAX_ORBIT;
  int nTracks = PSR_ORBITS_PER_CYCLE;

  // Check how many frames we have
  int nTotalFrames = 0;
  fp = FOPEN(inFile, "r");
  while (fgets(line, 4096, fp)) {
    if (strstr(line, "SQL>") || strchr(line, ',') == NULL)
      continue;
    else {
      sprintf(sensorMeta, "%s", my_get_str(line, 8));
      sprintf(modeMeta, "%s", alos_beam_mode[my_get_int(line, 17)]);
      off_nadir = (float) my_get_double(line, 19);
      orbit = my_get_int(line, 31);
      sprintf(orbit_dir, "%s", my_get_str(line, 32));
      frame = my_get_int(line, 25);
      sprintf(timeStr, "%s", my_get_str(line, 36));
      sscanf(timeStr, "%4d%2d%2d %d:%d:%lf", 
	     &ymd.year, &ymd.month, &ymd.day, &hms.hour, &hms.min, &hms.sec);
      date_ymd2jd(&ymd, &jd);
      sprintf(time, "%d-%dT%02d:%02d:%02.3f", 
	      jd.year, jd.jd, hms.hour, hms.min, hms.sec);
      c_lat = my_get_double(line, 37);
      c_lon = my_get_double(line, 38);
      ns_lat = my_get_double(line, 39);
      ns_lon = my_get_double(line, 40);
      fs_lat = my_get_double(line, 41);
      fs_lon = my_get_double(line, 42);
      ne_lat = my_get_double(line, 43);
      ne_lon = my_get_double(line, 44);
      fe_lat = my_get_double(line, 45);
      fe_lon = my_get_double(line, 46);
      x = my_get_double(line, 47);
      y = my_get_double(line, 48);
      z = my_get_double(line, 49);
      vx = my_get_double(line, 50);
      vy = my_get_double(line, 51);
      vz = my_get_double(line, 52);
      sprintf(versionStr, "%s", my_get_str(line, 54));
      sscanf(&versionStr[6], "%d", &version);
      if (strcmp_case(sensor, sensorMeta) == 0 && strstr(modeMeta, mode)) {

	// Calculate which track we are on
	track = orbit-(orbit/nTracks)*nTracks;
	sprintf(list, "%s/%s_track%d.lst", outputDir, sensor, track);
	
	// Write file name to the appropriate list
	if (orbit >= minOrbit && orbit <= maxOrbit) {
	  fpList = FOPEN(list, "a");
	  fprintf(fpList, "%s, %.4f, %d, %s, %d, %s, %.4f, %.4f, %.4f, %.4f, "
		  "%.4f, %.4f, %.4f, %.4f, %.4f, %.4f, %.3f, %.3f, "
		  "%.3f, %.3f, %.3f, %.3f, %d\n", 
		  sensorMeta, off_nadir, orbit, orbit_dir, frame, time,
		  c_lat, c_lon, ns_lat, ns_lon, fs_lat, fs_lon, ne_lat, ne_lon,
		  fe_lat, fe_lon, x, y, z, vx, vy, vz, version);
	  FCLOSE(fpList);
	}
	nTotalFrames++;
	printf("\rNumber of frames in this beam mode: %6d", nTotalFrames);
      }
    }
  }
  FCLOSE(fp);
  printf("\n");

  *nFrames = nTotalFrames;
}

int get_basic_info(const char *inFile, char *sensor, char *mode, 
		   int *orbit, int *sequence, int *frames, double *scan)
{
  FILE *fp;
  julian_date jd;
  hms_time hms;
  char line[1024], sensorSRF[5], modeSRF[10], modeStr[10];
  int i, nSegments = 0, nFrames = 0, nTotalFrames, orbitSRF, sequenceSRF;
  int foundSRF = FALSE, foundSensor = FALSE, foundMode = FALSE;
  int foundOrbit = FALSE, foundSequence = FALSE, foundScan = FALSE;
  double scanSRF;

  // Reset the beam mode in case we are looking for tandem mission data
  if (strcmp_case(mode, "TANDEM") == 0)
    sprintf(modeStr, "STD");
  else
    sprintf(modeStr, "%s", mode);

  // Check whether the file is in fact a scan results file
  nTotalFrames = 0;
  fp = FOPEN(inFile, "r");
  while (fgets(line, 1024, fp)) {
    
    // Convert time stamp to seconds since 1900 in order to have something to
    // figure out most recent SRF
    if (strstr(line, "OBJECT = SCAN_RESULTS_FILE")) {
      foundSRF = TRUE;
      while (fgets(line, 1024, fp)) {
	if (strstr(line, "OBJECT = COMMON_HEADER") &&
	    strncmp(line, " OBJECT = COMMON_HEADER", 23)==0) {
	  
	  while (fgets(line, 1024, fp)) {
	    if (strstr(line, "TIME") &&
		strncmp(line, "  TIME", 6) == 0) {
	      sscanf(line, " TIME = %d-%dT%d:%d:%lf", 
		     &jd.year, &jd.jd, &hms.hour, &hms.min, &hms.sec);
	      scanSRF = date2sec(&jd, &hms);
	      foundScan = TRUE;
	      //printf("scan: %.1lf\n", scanSRF);
	      break;
	    }
	  } // while inside OBJECT = COMMON_HEADER
	}

	if (strstr(line, "OBJECT = BODY") &&
	    strncmp(line, " OBJECT = BODY", 14) == 0) {
	  while (fgets(line, 1024, fp)) {
	    
	    // Look for platform (sensor)
	    if (strstr(line, "PLATFORM") &&
		strncmp(line, "  PLATFORM", 10) == 0) {
	      sscanf(line, "  PLATFORM = \"%s\"", sensorSRF);
	      sensorSRF[2] = 0;
	      if (strcmp_case(sensor, sensorSRF) == 0)
		foundSensor = TRUE;
	      //printf("sensor: %s\n", sensorSRF);
	    }
	    
	    // Look for revolution (orbit)
	    if (strstr(line, "REVOLUTION") &&
		strncmp(line, "  REVOLUTION", 12) == 0) {
	      sscanf(line, "  REVOLUTION = %i", &orbitSRF);
	      foundOrbit = TRUE;
	      //printf("orbit: %d\n", orbitSRF);
	    }
	    
	    // Look for sequence number
	    if (strstr(line, "SEQUENCE") &&
		strncmp(line, "  SEQUENCE", 10) == 0) {
	      sscanf(line, "  SEQUENCE = %i", &sequenceSRF);
	      foundSequence = TRUE;
	      //printf("sequence: %d\n", sequenceSRF);
	      break;
	    }
	  }
	}

	// Look for segments
	if (strstr(line, "SEGMENT_COUNT")) {
	  sscanf(line, "  SEGMENT_COUNT = %i", &nSegments);
	  for (i=0; i<nSegments; i++) {
	    
	    while (fgets(line, 1024, fp)) {
		
	      if (strstr(line, "OBJECT = SEGMENT") &&
		  strncmp(line, "  OBJECT = SEGMENT", 18) == 0) {
		
		// Look for beam mode
		while (fgets(line, 1024, fp)) {
		  if (strstr(line, "MODE")) {
		    sscanf(line, "   MODE = \"%s\"", modeSRF);
		    modeSRF[3] = 0;
		    if (strcmp_case(modeStr, modeSRF) == 0)
		      foundMode = TRUE;
		    //printf("mode: %s\n", modeSRF);
		    break;
		  }
		}

		// Look for orbit number
		while (fgets(line, 1024, fp)) {
		  if (strstr(line, "FRAME_COUNT")) {
		    sscanf(line, "   FRAME_COUNT = %i", &nFrames);
		    nTotalFrames += nFrames;
		    break;
		  }
		}

	      } // while inside OBJECT = SEGMENT
	      
	      if (strstr(line, "END_OBJECT = SEGMENT") &&
		  strncmp(line, "  END_OBJECT = SEGMENT", 22)==0) 
		break;
	      
	    } // if within a SEGMENT object
	    
	  } // while looking for SEGMENTS objects
	  
	} // looping through a SEGMENT object (for)
	
      } // if SEGMENT_COUNT was found
      
    }
  }
  FCLOSE(fp);
  if (foundSensor && foundMode && foundOrbit && foundSequence && foundScan) {  
    *orbit = orbitSRF;
    *sequence = sequenceSRF;
    *frames = nTotalFrames;
    *scan = scanSRF;
    return TRUE;
  }
  else
    return FALSE;
}

static void write_file_list(struct srf_file srf, char *outputDir, int *tracks)
{
  FILE *fp;
  int minOrbit, maxOrbit, nTracks, track, orbit;
  char list[255], phase[5];
  
  // Set the number of tracks according to the sensor
  if (strcmp_case(srf.sensor, "R1") == 0) {
    minOrbit = R1_MIN_ORBIT;
    maxOrbit = R1_MAX_ORBIT;
    nTracks = R1_ORBITS_PER_CYCLE;
    strcpy(phase, "");
  }
  else if (strcmp_case(srf.sensor, "E1") == 0) {
    // Commissioning Phase A
    if (srf.orbit >= E1_A_MIN_ORBIT && srf.orbit <= E1_A_MAX_ORBIT) {
      minOrbit = E1_A_MIN_ORBIT;
      maxOrbit = E1_A_MAX_ORBIT;
      nTracks = E1_A_ORBITS_PER_CYCLE;
      strcpy(phase, "A");
    }
    // First Ice Phase B
    else if (srf.orbit >= E1_B_MIN_ORBIT && srf.orbit <= E1_B_MAX_ORBIT) {
      minOrbit = E1_B_MIN_ORBIT;
      maxOrbit = E1_B_MAX_ORBIT;
      nTracks = E1_B_ORBITS_PER_CYCLE;
      strcpy(phase, "B");
    }
    // Multidisciplinary Phase C
    else if (srf.orbit >= E1_C_MIN_ORBIT && srf.orbit <= E1_C_MAX_ORBIT) {
      minOrbit = E1_C_MIN_ORBIT;
      maxOrbit = E1_C_MAX_ORBIT;
      nTracks = E1_C_ORBITS_PER_CYCLE;
      strcpy(phase, "C");
    }
    // Second Ice Phase D
    else if (srf.orbit >= E1_D_MIN_ORBIT && srf.orbit <= E1_D_MAX_ORBIT) {
      minOrbit = E1_D_MIN_ORBIT;
      maxOrbit = E1_D_MAX_ORBIT;
      nTracks = E1_D_ORBITS_PER_CYCLE;
      strcpy(phase, "D");
    }
    // Geodetic Phase F
    else if (srf.orbit >= E1_F_MIN_ORBIT && srf.orbit <= E1_F_MAX_ORBIT) {
      minOrbit = E1_F_MIN_ORBIT;
      maxOrbit = E1_F_MAX_ORBIT;
      nTracks = E1_F_ORBITS_PER_CYCLE;
      strcpy(phase, "F");
    }
    // Second Multidisciplinary Phase G
    else if (srf.orbit >= E1_G_MIN_ORBIT && srf.orbit <= E1_G_MAX_ORBIT) {
      minOrbit = E1_G_MIN_ORBIT;
      maxOrbit = E1_G_MAX_ORBIT;
      nTracks = E1_G_ORBITS_PER_CYCLE;
      strcpy(phase, "G");
    }
  }
  else if (strcmp_case(srf.sensor, "E2") == 0) {
    minOrbit = E2_MIN_ORBIT;
    maxOrbit = E2_MAX_ORBIT;
    nTracks = E2_ORBITS_PER_CYCLE;
    strcpy(phase, "");
  }
  else if (strcmp_case(srf.sensor, "J1") == 0) {
    minOrbit = J1_MIN_ORBIT;
    maxOrbit = J1_MAX_ORBIT;
    nTracks = J1_ORBITS_PER_CYCLE;
  }
  else
    asfPrintError("Unknown sensor '%s'.\n", srf.sensor);

  // Calculate which track we are on
  track = srf.orbit-(srf.orbit/nTracks*nTracks);
  sprintf(list, "%s/%s_track%d%s.lst", outputDir, srf.sensor, track, phase);

  // Write file name to the appropriate list
  if (srf.orbit >= minOrbit && srf.orbit <= maxOrbit) {
    fp = FOPEN(list, "a");
    fprintf(fp, "%d %s\n", srf.frames, srf.file_name);
    FCLOSE(fp);
  }

  *tracks = nTracks;
}


void setup_files(char *sensor, char *beam_mode, char *input_dir, 
		 char *output_dir, int *nTracks, int *nFrames)
{
  struct dirent *dp;
  struct stat statbuf;
  DIR *dir;
  size_t *p;
  struct srf_file *files;
  double *orbits, scan;
  int i, orbit, multiple, sequence, recent;
  char file_name[50];
  
  if (strcmp_case(sensor, "PSR") != 0) {
    // Read directory for scan results files of sensor + beam mode
    asfPrintStatus("Analyzing scan results file directory ...\n\n");
    int nFiles = 0;
    files = (struct srf_file *) MALLOC(sizeof(struct srf_file)*SIZE);
    orbits = (double *) MALLOC(sizeof(double)*SIZE);
    chdir(input_dir);
    dir = opendir(input_dir);
    while ((dp = readdir(dir)) != NULL) {
      
      if (stat(dp->d_name, &statbuf) == -1)
	continue;
      sprintf(file_name, "%s", dp->d_name);
      if (strlen(file_name) > 3 &&
	  get_basic_info(file_name, sensor, beam_mode, 
			 &orbit, &sequence, nFrames, &scan)) {
	strcpy(files[nFiles].file_name, file_name);
	strcpy(files[nFiles].sensor, sensor);
	strcpy(files[nFiles].beam_mode, beam_mode);
	files[nFiles].orbit = orbit;
	files[nFiles].sequence = sequence;
	files[nFiles].frames = *nFrames;
	files[nFiles].scan = scan;
	orbits[nFiles] = orbit + scan/1000000000.0;
	//printf("File: %s, Frames: %d\n", file_name, nFrames);
	nFiles++;
	asfPrintStatus("\rNumber of scan results files read: %6d", nFiles);
      }
    }
    closedir(dir);
    printf("\n");
    
    // Sort the file list
    if (nFiles) {
      p = (size_t *) MALLOC(sizeof(size_t)*nFiles);
      gsl_sort_index(p, orbits, 1, nFiles);
      for (i=0; i<nFiles; i++) {
	if (i < nFiles-1) {
	  /*
	  printf("%d: %s, %s, %s, %d, %d, %d, %lf\n", 
		 i, files[p[i]].file_name, files[p[i]].sensor, 
		 files[p[i]].beam_mode, files[p[i]].orbit, 
		 files[p[i]].sequence, files[p[i]].frames, files[p[i]].scan);
	  */
	  if (files[p[i]].orbit == files[p[i+1]].orbit &&
	      files[p[i]].sequence == files[p[i+1]].sequence) {
	    multiple = TRUE;
	    if (files[p[i+1]].scan > files[p[i]].scan)
	      recent = p[i+1];
	    else
	      recent = p[i];
	  }
	  else if (multiple) {
	    write_file_list(files[p[i]], output_dir, nTracks);
	    multiple = FALSE;
	  }
	  else
	    write_file_list(files[p[i]], output_dir, nTracks);
	}
	else
	  write_file_list(files[p[i]], output_dir, nTracks);
	
      }
      FREE(files);
    }
    else {
      asfPrintStatus("No scan results files for this sensor/beam mode "
		     "combination.\n");
      if (orbits)
	FREE(orbits);
      if (files)
	FREE(files);
      return;
    }
  }
  // PALSAR metadata need to be treated differently
  else {
    if (is_dir(input_dir) || !fileExists(input_dir))
      asfPrintError("PALSAR metadata file '%s' does not exist!\n", input_dir);
    get_palsar_info(input_dir, output_dir, sensor, beam_mode, nFrames);
    *nTracks = PSR_ORBITS_PER_CYCLE;
  }
}
