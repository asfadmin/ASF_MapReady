#include "foot_print.h"

#define VERSION 1.0

void usage()
{
  printf("\n"
   "USAGE:\n"
   "   foot_print <granule table> <satellite> <beam mode> <shape file type>\n");
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   granule table             database dump of the table\n"
   "   satellite                 satellite name (E1, E2, J1, R1, A3)\n"
   "   beam mode                 R1 - FN1 to FN5, ST1 to ST2, SNA, SNB,\n"
   "                                  SWA, SWB, WD1 to WD3,EL1, EH3, EH4, EH6\n"
   "                             E1 - A, B, C, D, E, F, G (phases)\n"
   "                             E2 - STD\n"
   "                             J1 - STD\n"
   "                             A3 - FBS, FBD, PLR, WB1, WB2\n"
   "                             SS - STD\"
   "   shape file type           types: count, list, details\n"
   "      count                  number of available frames per base frame\n"
   "      list                   list of granule names per base frame\n"
   "      details                granule names plus extra metadata including\n"
   "                             geolocation\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program converts information for image stacks out of database into\n"
   "   ArcGIS shape files.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

static int is_granule(char *line, char *satellite, char *beam_mode,
		      double off_nadir)
{
  int orbit, ret = FALSE;
  char sat[5], mode[10];
  double angle;
  
  sprintf(sat, "%s", get_str(line, 1));
  sprintf(mode, "%s", get_str(line, 2));
  angle = get_double(line, 3);
  if (strcmp_case(satellite, "A3") == 0) {
    if (strcmp_case(satellite, sat) == 0 && strcmp_case(beam_mode, mode) == 0 &&
	FLOAT_EQUIVALENT(off_nadir, angle))
      ret = TRUE;
  }
  else if (strcmp_case(satellite, "E1") == 0) {
    orbit = get_int(line, 3);
    if (orbit >= E1_A_MIN_ORBIT && orbit <= E1_A_MAX_ORBIT)
      strcpy(mode, "A");
    else if (orbit >= E1_B_MIN_ORBIT && orbit <= E1_B_MAX_ORBIT)
      strcpy(mode, "B");
    else if (orbit >= E1_C_MIN_ORBIT && orbit <= E1_C_MAX_ORBIT)
      strcpy(mode, "C");
    else if (orbit >= E1_D_MIN_ORBIT && orbit <= E1_D_MAX_ORBIT)
      strcpy(mode, "D");
    else if (orbit >= E1_E_MIN_ORBIT && orbit <= E1_E_MAX_ORBIT)
      strcpy(mode, "E");
    else if (orbit >= E1_F_MIN_ORBIT && orbit <= E1_F_MAX_ORBIT)
      strcpy(mode, "F");
    else if (orbit >= E1_G_MIN_ORBIT && orbit <= E1_G_MAX_ORBIT)
      strcpy(mode, "G");
    if (strcmp_case(satellite, sat) == 0 && strcmp_case(beam_mode, mode) == 0)
      ret = TRUE;    
  }
  else {
    if (strcmp_case(satellite, sat) == 0 && strcmp_case(beam_mode, mode) == 0)
      ret = TRUE;
  }
    
  return ret;
}

static int line2granule(char *line, char *satellite, char *beam_mode,
			double off_nadir, granule_t *granule)
{
  int ret = FALSE;
  granule_t g;

  strcpy(g.name, get_str(line, 0));
  strcpy(g.satellite, get_str(line, 1));
  strcpy(g.beam_mode, get_str(line, 2));
  if (strcmp_case(g.satellite, "A3") == 0) {
    // Table column names for ALOS PALSAR
    // GRANULE - name
    // SATELLITE - satellite
    // BEAM_MODE - beam_mode
    // OFF_NADIR - off_nadir
    // ORBIT - orbit
    // FRAME - frame
    // ACQ_START - acq_start
    // ACQ_END - acq_end
    // ORBIT_DIR - orbit_dir
    // PATH - path
    // LAT1 - near_start_lat
    // LON1 - near_start_lon
    // LAT2 - far_start_lat
    // LON2 - far_start_lon
    // LAT3 - near_end_lat
    // LON3 - near_end_lon
    // LAT3 - far_end_lat
    // LON4 - far_end_lon

    g.off_nadir = get_double(line, 3);
    g.orbit = get_int(line, 4);
    g.frame = get_int(line, 5);
    strcpy(g.acq_start, get_str(line, 6));
    strcpy(g.acq_start, get_str(line, 7));
    strcpy(g.orbit_dir, get_str(line, 8));
    g.path = get_int(line, 9);
    g.near_start_lat = get_double(line, 10);
    g.near_start_lon = get_double(line, 11);
    g.far_start_lat = get_double(line, 12);
    g.far_start_lon = get_double(line, 13);
    g.far_end_lat = get_double(line, 14);
    g.far_end_lon = get_double(line, 15);
    g.near_end_lat = get_double(line, 16);
    g.near_end_lon = get_double(line, 17);
    if (strcmp_case(g.satellite, satellite) == 0 &&
    	  strcmp_case(g.beam_mode, beam_mode) == 0 &&
    	  FLOAT_EQUIVALENT(g.off_nadir, off_nadir)) {
      *granule = g;
      ret = TRUE;
    }
  }
  else {
    // Table column names for legacy
    // GRANULE - name
    // SATELLITE - satellite
    // BEAM_MODE - beam_mode
    // ORBIT - orbit
    // FRAME - frame
    // ACQ_START - acq_start
    // ACQ_END - acq_end
    // ORBIT_DIR - orbit_dir
    // LAT1 - near_start_lat
    // LON1 - near_start_lon
    // LAT2 - far_start_lat
    // LON2 - far_start_lon
    // LAT3 - near_end_lat
    // LON3 - near_end_lon
    // LAT3 - far_end_lat
    // LON4 - far_end_lon

    g.orbit = get_int(line, 3);
    g.off_nadir = -1.0;
    if (strcmp_case(g.satellite, "E1") == 0) {
      if (g.orbit >= E1_A_MIN_ORBIT && g.orbit <= E1_A_MAX_ORBIT)
        strcpy(g.beam_mode, "A");
      else if (g.orbit >= E1_B_MIN_ORBIT && g.orbit <= E1_B_MAX_ORBIT)
        strcpy(g.beam_mode, "B");
      else if (g.orbit >= E1_C_MIN_ORBIT && g.orbit <= E1_C_MAX_ORBIT)
        strcpy(g.beam_mode, "C");
      else if (g.orbit >= E1_D_MIN_ORBIT && g.orbit <= E1_D_MAX_ORBIT)
        strcpy(g.beam_mode, "D");
      else if (g.orbit >= E1_E_MIN_ORBIT && g.orbit <= E1_E_MAX_ORBIT)
        strcpy(g.beam_mode, "E");
      else if (g.orbit >= E1_F_MIN_ORBIT && g.orbit <= E1_F_MAX_ORBIT)
        strcpy(g.beam_mode, "F");
      else if (g.orbit >= E1_G_MIN_ORBIT && g.orbit <= E1_G_MAX_ORBIT)
        strcpy(g.beam_mode, "G");
    }
    g.frame = get_int(line, 4);
    strcpy(g.acq_start, get_str(line, 5));
    strcpy(g.acq_end, get_str(line, 6));
    strcpy(g.orbit_dir, get_str(line, 7));
    g.path = -1;
    g.near_start_lat = get_double(line, 8);
    g.near_start_lon = get_double(line, 9);
    g.far_start_lat = get_double(line, 10);
    g.far_start_lon = get_double(line, 11);
    g.far_end_lat = get_double(line, 12);
    g.far_end_lon = get_double(line, 13);
    g.near_end_lat = get_double(line, 14);
    g.near_end_lon = get_double(line, 15);
    if (strcmp_case(g.satellite, satellite) == 0 &&
      	strcmp_case(g.beam_mode, beam_mode) == 0) {
      char tmp[30];
      sprintf(tmp, "%s", g.name+14);
      g.frame = atoi(tmp);
      *granule = g;
      ret = TRUE;
    }
  }
  
  return ret;
}

static void my_swap(granule_t *a, granule_t *b)
{
  granule_t c=*a;
  *a = *b;
  *b = c;
}

static void my_sort(granule_t *list, int left, int right)
{
  register int i, j;
  granule_t x;
  
  i = left;
  j = right;
  x = list[(left+right)/2];
  
  do {
    while ((strcmp_case(list[i].name, x.name) < 0) && (i < right))
      i++;
    while ((strcmp_case(list[j].name, x.name) > 0) && (j > left))
      j--;
    if (i <= j) {
      my_swap(&list[i], &list[j]);
      i++;
      j--;
    }
  } while (i <= j);
  
  if (left < j) 
    my_sort(list, left, j);
  if (i < right)
    my_sort(list, i, right);
}

int main(int argc, char **argv)
{
  long granule_count;
  char granule_table[512], line[1024];
  char satellite[15], beam[10], beam_mode[5], shape_file_type[10];

  // Parse command line
  if ((argc-currArg)<4) {
    printf("Insufficient arguments.\n"); 
    usage();
  }
  strcpy(granule_table, argv[currArg]);
  strcpy(satellite, argv[currArg+1]);
  strcpy(beam_mode, argv[currArg+2]);
  strcpy(shape_file_type, argv[currArg+3]);
  
  asfSplashScreen(argc, argv);

  FILE *fp, *fpCSV, *fpGran;
  DBFHandle dbase;
  SHPHandle shape;
  char outFile[255], csvFile[255], granuleFile[255];
  int ii, kk, ll, mm, orbit, track, track_count, frame_count, max_frame_count;
  int inc, stack_id, beam_mode_count=0;
  long nn;
  double lat[5], lon[5], off_nadir;

  if (strcmp(satellite, "R1") == 0) {
    track_count = R1_ORBITS_PER_CYCLE;
    max_frame_count = 900;
    beam_mode_count = 1;
    inc = 1;
  }
  else if (strcmp(satellite, "E1") == 0) {
    if (strcmp_case(beam_mode, "A") == 0)
      track_count = E1_A_ORBITS_PER_CYCLE;
    else if (strcmp_case(beam_mode, "B") == 0)
      track_count = E1_B_ORBITS_PER_CYCLE;
    else if (strcmp_case(beam_mode, "C") == 0)
      track_count = E1_C_ORBITS_PER_CYCLE;
    else if (strcmp_case(beam_mode, "D") == 0)
      track_count = E1_D_ORBITS_PER_CYCLE;
    else if (strcmp_case(beam_mode, "E") == 0)
      track_count = E1_E_ORBITS_PER_CYCLE;
    else if (strcmp_case(beam_mode, "F") == 0)
      track_count = E1_F_ORBITS_PER_CYCLE;
    else if (strcmp_case(beam_mode, "G") == 0)
      track_count = E1_G_ORBITS_PER_CYCLE;
    max_frame_count = 900;
    beam_mode_count = 1;
    inc = 1;
  }
  else if (strcmp(satellite, "E2") == 0) {
    track_count = E2_ORBITS_PER_CYCLE;
    max_frame_count = 900;
    beam_mode_count = 1;
    inc = 1;
  }
  else if (strcmp(satellite, "J1") == 0) {
    track_count = J1_ORBITS_PER_CYCLE;
    max_frame_count = 900;
    beam_mode_count = 1;
    inc = 1;
  }
  else if (strcmp(satellite, "A3") == 0) {
    track_count = PSR_ORBITS_PER_CYCLE;
    max_frame_count = 7200;
    if (strcmp_case(beam_mode, "FBS") == 0)
      beam_mode_count = FBS_COUNT;
    else if (strcmp_case(beam_mode, "FBD") == 0)
      beam_mode_count = FBD_COUNT;
    else if (strcmp_case(beam_mode, "PLR") == 0)
      beam_mode_count = PLR_COUNT;
    else if (strcmp_case(beam_mode, "WB1") == 0)
      beam_mode_count = WB1_COUNT;
    else if (strcmp_case(beam_mode, "WB2") == 0)
      beam_mode_count = WB2_COUNT;
    if (strncmp_case(beam_mode, "WB", 2) == 0)
      inc = 50;
    else
      inc = 10;
  }

  // Assign CSV file names
  sprintf(csvFile, "%s_%s_foot_print.csv", satellite, beam_mode);
  fpCSV = FOPEN(csvFile, "w");
  fprintf(fpCSV, "STACK_ID,FRAME_CNT,GRANULE,SATELLITE,BEAM_MODE,OFF_NADIR,"
    "ORBIT,FRAME,ACQ_START,ACQ_END,ORBIT_DIR,PATH,LAT1,LON1,LAT2,LON2,LAT3,"
    "LON3,LAT4,LON4\n");

  sprintf(granuleFile, "%s_%s_granules.csv", satellite, beam_mode);
  fpGran = FOPEN(granuleFile, "w");
  fprintf(fpGran, "STACK_ID,GRANULE,SATELLITE,BEAM_MODE,OFF_NADIR,ORBIT,FRAME,"
    "ACQ_START,ACQ_END,ORBIT_DIR,PATH,LAT1,LON1,LAT2,LON2,LAT3,LON3,LAT4,LON4"
    "\n");

  // Initialize foot print shape file
  sprintf(outFile, "%s_%s_foot_print", satellite, beam_mode);
  shape_init(outFile, FOOT_PRINT);
  open_shape(outFile, &dbase, &shape);
  
  stack_id = 0;

  for (ii=0; ii<beam_mode_count; ii++) {

    if (strcmp_case(satellite, "A3") == 0) {
      if (strcmp_case(beam_mode, "FBS") == 0) {
	sprintf(beam, "%s %.1lf", beam_mode, fbs_modes[ii]);
	off_nadir = fbs_modes[ii];
      }
      else if (strcmp_case(beam_mode, "FBD") == 0) {
	sprintf(beam, "%s %.1lf", beam_mode, fbd_modes[ii]);
	off_nadir = fbd_modes[ii];
      }
      else if (strcmp_case(beam_mode, "PLR") == 0) {
	sprintf(beam, "%s %.1lf", beam_mode, plr_modes[ii]);
	off_nadir = plr_modes[ii];
      }
      else if (strcmp_case(beam_mode, "WB1") == 0) {
	sprintf(beam, "%s %.1lf", beam_mode, wb1_modes[ii]);
	off_nadir = wb1_modes[ii];
      }
      else if (strcmp_case(beam_mode, "WB2") == 0) {
	sprintf(beam, "%s %.1lf", beam_mode, wb2_modes[ii]);
	off_nadir = wb2_modes[ii];
      }
      asfPrintStatus("\nProcessing ALOS %s granules ...\n", beam);
    }
    else if (strcmp_case(satellite, "R1") == 0)
      asfPrintStatus("\nProcessing R1 %s granules ...\n", beam_mode);
    else if (strcmp_case(satellite, "E1") == 0)
      asfPrintStatus("\nProcessing E1 granules (phase %s) ...\n", beam_mode);
    else if (strcmp_case(satellite, "E2") == 0)
      asfPrintStatus("\nProcessing E2 granules ...\n");

    // Determine how many granules we deal with
    granule_count = 0;
    asfPrintStatus("Determining granule count ...\n");
    fp = FOPEN(granule_table, "r");
    while (fgets(line, 1024, fp)) {
      if (is_granule(line, satellite, beam_mode, off_nadir))
	      granule_count++;
    }
    FCLOSE(fp);
    asfPrintStatus("Found %ld %s %s granules in file (%s)\n", 
		   granule_count, satellite, beam_mode, granule_table);

    // Read granule information from file
    granule_t granule;
    granule_t *g = (granule_t *) MALLOC(sizeof(granule_t)*granule_count);
    nn = 0;
    asfPrintStatus("Reading granule table ...\n");
    fp = FOPEN(granule_table, "r");
    while (fgets(line, 1024, fp)) {
      if (line2granule(line, satellite, beam_mode, off_nadir, &granule)) {
	      g[nn] = granule;
	      nn++;
      }
    }
    FCLOSE(fp);

    frame_stack_t *stack = 
      (frame_stack_t *) MALLOC(sizeof(frame_stack_t)*track_count);
    for (kk=0; kk<track_count; kk++) // track numbers
      for (ll=0; ll<=max_frame_count; ll+=inc) // frames
	      stack[kk].frame[ll] = 0;
    
    // Count the frames in a stack
    long foot_print_count = 0;
    for (nn=0; nn<granule_count; nn++) {
      orbit = g[nn].orbit;
      track = orbit-(orbit/track_count)*track_count;
      g[nn].track = track;
      if (stack[track].frame[g[nn].frame] == 0 &&
	        strcmp_case(beam_mode, g[nn].beam_mode) == 0)
    	  foot_print_count++;
      stack[track].frame[g[nn].frame]++;
    }
    asfPrintStatus("Found %ld foot prints\n", foot_print_count);
    
    for (kk=0; kk<track_count; kk++) {  
      printf("\rTrack: %03d/%d", kk+1, track_count);
      fflush(NULL);
      for (ll=0; ll<=max_frame_count; ll+=inc) {
	      frame_count = stack[kk].frame[ll];
	    if (frame_count > 0) {
	      granule_t *list = (granule_t *) MALLOC(sizeof(granule_t)*frame_count);
          for (nn=0; nn<granule_count; nn++) {
            mm = 0;
            if (g[nn].track == kk && g[nn].frame == ll) {
              list[mm].stack_id = stack_id + 1;
              list[mm].frame_count = frame_count;
              strcpy(list[mm].name, g[nn].name);
              strcpy(list[mm].satellite, g[nn].satellite);
              strcpy(list[mm].beam_mode, g[nn].beam_mode);
              list[mm].off_nadir = g[nn].off_nadir;
              list[mm].orbit = g[nn].orbit;
              list[mm].frame = g[nn].frame;
              strcpy(list[mm].acq_start, g[nn].acq_start);
              strcpy(list[mm].acq_end, g[nn].acq_end);
              strcpy(list[mm].orbit_dir, g[nn].orbit_dir);
              list[mm].path = g[nn].path;
              list[mm].near_start_lat = g[nn].near_start_lat;
              list[mm].near_start_lon = g[nn].near_start_lon;
              list[mm].far_start_lat = g[nn].far_start_lat;
              list[mm].far_start_lon = g[nn].far_start_lon;
              list[mm].near_end_lat = g[nn].near_end_lat;
              list[mm].near_end_lon = g[nn].near_end_lon;
              list[mm].far_end_lat = g[nn].far_end_lat;
              list[mm].far_end_lon = g[nn].far_end_lon;
              fprintf(fpGran, "%d,\"%s\",\"%s\",\"%s\",%.1lf,%d,%d,\"%s\",\"%s\","
                "\"%s\",%d,%.4lf,%.4lf,%.4lf,%.4lf,%.4lf,%.4lf,%.4lf,%.4lf\n",
                stack_id+1, g[nn].name, g[nn].satellite, 
                g[nn].beam_mode, g[nn].off_nadir, g[nn].orbit, 
                g[nn].frame, g[nn].acq_start, g[nn].acq_end, g[nn].orbit_dir, 
                g[nn].path, g[nn].near_start_lat, g[nn].near_start_lon, 
                g[nn].far_start_lat, g[nn].far_start_lon, g[nn].far_end_lat, 
                g[nn].far_end_lon, g[nn].near_end_lat, g[nn].near_end_lon);
              mm++;
            }
          }
          if (mm > 0)
            my_sort(list, 0, mm-1);
    
          // Write into CSV file
          fprintf(fpCSV, "%d,%d,\"%s\",\"%s\",\"%s\",%.1lf,%d,%d,\"%s\",\"%s\","
            "\"%s\",%d,%.4lf,%.4lf,%.4lf,%.4lf,%.4lf,%.4lf,%.4lf,%.4lf\n",
            list[0].stack_id, list[0].frame_count, list[0].name, 
            list[0].satellite, list[0].beam_mode, list[0].off_nadir, 
            list[0].orbit, list[0].frame, list[0].acq_start, list[0].acq_end,
            list[0].orbit_dir, list[0].path, list[0].near_start_lat, 
            list[0].near_start_lon, list[0].far_start_lat, 
            list[0].far_start_lon, list[0].far_end_lat, list[0].far_end_lon, 
            list[0].near_end_lat, list[0].near_end_lon);
    
          // Write into shape file
          lat[0] = lat[4] = list[0].near_start_lat;
          lon[0] = lon[4] = list[0].near_start_lon;
          lat[1] = list[0].far_start_lat;
          lon[1] = list[0].far_start_lon;
          lat[2] = list[0].far_end_lat;
          lon[2] = list[0].far_end_lon;
          lat[3] = list[0].near_end_lat;
          lon[3] = list[0].near_end_lon;
          DBFWriteIntegerAttribute(dbase, stack_id, 0, list[0].stack_id);
          DBFWriteIntegerAttribute(dbase, stack_id, 1, list[0].frame_count);
          DBFWriteStringAttribute(dbase, stack_id, 2, list[0].name);
          DBFWriteStringAttribute(dbase, stack_id, 3, list[0].satellite);
          DBFWriteStringAttribute(dbase, stack_id, 4, list[0].beam_mode);
          DBFWriteDoubleAttribute(dbase, stack_id, 5, list[0].off_nadir);
          DBFWriteIntegerAttribute(dbase, stack_id, 6, list[0].orbit);
          DBFWriteIntegerAttribute(dbase, stack_id, 7, list[0].frame);
          DBFWriteStringAttribute(dbase, stack_id, 8, list[0].acq_start);
          DBFWriteStringAttribute(dbase, stack_id, 9, list[0].acq_end);
          DBFWriteStringAttribute(dbase, stack_id, 10, list[0].orbit_dir);
          DBFWriteIntegerAttribute(dbase, stack_id, 11, list[0].path);	
          DBFWriteDoubleAttribute(dbase, stack_id, 12, list[0].near_start_lat);
          DBFWriteDoubleAttribute(dbase, stack_id, 13, list[0].near_start_lon);
          DBFWriteDoubleAttribute(dbase, stack_id, 14, list[0].far_start_lat);
          DBFWriteDoubleAttribute(dbase, stack_id, 15, list[0].far_start_lon);
          DBFWriteDoubleAttribute(dbase, stack_id, 16, list[0].near_end_lat);
          DBFWriteDoubleAttribute(dbase, stack_id, 17, list[0].near_end_lon);
          DBFWriteDoubleAttribute(dbase, stack_id, 18, list[0].far_end_lat);
          DBFWriteDoubleAttribute(dbase, stack_id, 19, list[0].far_end_lon);
    
          SHPObject *shapeObject=NULL;
          shapeObject = 
            SHPCreateSimpleObject(SHPT_POLYGON, 5, lon, lat, NULL);
          if (shapeObject == NULL)
            asfPrintError("Could not create shape object (%d)\n", stack_id);
          SHPWriteObject(shape, -1, shapeObject);
          SHPDestroyObject(shapeObject);
    
          FREE(list);
          stack_id++;
        }
      }
    }
    FREE(g);
  }
  FCLOSE(fpCSV);
  FCLOSE(fpGran);
  printf("\n\n");
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  return(0);
}
