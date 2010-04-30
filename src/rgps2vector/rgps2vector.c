#include "dateUtil.h"
#include "asf_vector.h"
#include "asf_raster.h"
#include "asf.h"
#include "asf_meta.h"
#include "gsl/gsl_sort.h"
#include "stdio.h"

#define VERSION 1.0

void usage()
{
  printf("\n"
   "USAGE:\n"
   "   rgps2vector [ -cells <grid points> <attributes> <connectivity> \n"
   "                 <cell definition> <grid definition> <input type> | \n"
   "                 -weather <table> <date> ] <snapshot>\n");
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   snapshot      Basename of the output file.\n"
   "                 cells: cells_<snapshot>, grid points: grid_<snapshot>\n");
  printf("\n"
   "OPTIONAL ARGUMENTS:\n"
   "   -cells        Generates a data layer with the cell information.\n"
   "   grid points   File name with grid point table.\n"
   "   attributes    File name with cell attribute table.\n"
   "   connectvity   File name with cell connectivity table.\n"
   "   cell definition    File name with cell definition table.\n"
   "   grid definition    File name with grid definition table.\n"
   "   input type    input files can database files or backup text files.\n"
   "                 database: 'db' or text file: 'txt'.\n"
   "   -weather      Generates a data layer with weather information.\n"
   "   table         File name with weather table.\n"
   "   date          Date for which to extract the weather data\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program converts information out of the RGPS database into\n"
   "   ArcGIS shape files and KML files.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  DBFHandle dbase;
  SHPHandle shape;
  project_parameters_t pps;
  projection_type_t proj_type;
  datum_type_t datum;
  spheroid_type_t spheroid;
  meta_projection *meta_proj;
  ymd_date date, rgpsDate;
  julian_date jd;
  grid_t *grid;
  grid_attr_t *gridAttr;
  cell_t *cell;
  size_t *p;
  FILE *fp, *fpKml;
  extern int currArg; // Pre-initialized to 1
  char outFile[255], line[1024], *date_str, sourceImage[25], targetImage[25];
  char stream, kmlFile[255], snapshot[25], cmd[255];
  char *grid_points, *cell_attributes, *cell_connectivity, *cell_definition;
  char *grid_definition, *weather_table, *input_type, str[50], separator;
  int cells, nBla, grid_id, old_cell=0, nVertices, cell_id, bla, status;
  int db, quality;
  double latitude, longitude, u, v, direction, speed, temperature, pressure;
  double tmp, *grid_order, fBla, grid_x, grid_y, lat[20], lon[20], height;
  double cellDay, area, multi_year_ice, open_water, incidence_angle, cell_x;
  double cell_y, dudx, dudy, dvdx, dvdy, dtp, u_wind, v_wind, gridDay;
  long i, k, l, n=0, nGrid=0, nCell=0, nGridAttr=0, index;;

  // Parse command line
  while (currArg < (argc-6)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-cells")) {
      CHECK_ARG(6);
      grid_points = GET_ARG(6);
      cell_attributes = GET_ARG(5);
      cell_connectivity = GET_ARG(4);
      cell_definition = GET_ARG(3);
      grid_definition = GET_ARG(2);
      input_type = GET_ARG(1);
      if (strncmp(uc(input_type), "DB", 2) == 0)
	db = 1;
      else
	db = 0;
      cells = TRUE;
    }
    else if (strmatch(key,"-weather")) {
      CHECK_ARG(2);
      weather_table = GET_ARG(2);
      date_str = GET_ARG(1);
      cells = FALSE;
    }
    else {
      printf( "\n**Invalid option:  %s\n", argv[currArg-1]); 
      usage(argv[0]);
    }
  }

  if ((argc-currArg)<1) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }
  strcpy(snapshot, argv[currArg]);

  asfSplashScreen(argc, argv);

  if (cells) {
    asfPrintStatus("Generating shape file for RGPS cell information\n");
    if (db)
      asfPrintStatus("Reading database style files ...\n\n");
    else
      asfPrintStatus("Reading backup style files ...\n\n");

    // Determine how many grid points we need to deal with and allocate the
    // appropriate memory
    if (db) {
      fp = FOPEN(cell_connectivity, "r");
      while (fgets(line, 1024, fp))
	nGrid++;
      FCLOSE(fp);
    }
    else {
      fp = FOPEN(cell_connectivity, "r");
      while (fgets(line, 1024, fp)) {
	if (strstr(line, "LOCK"))
	  break;
      }
      fscanf(fp, "INSERT INTO %s VALUES ", str);
      while (!feof(fp)) {
	fscanf(fp, "(%d,%d,%d,'%c')", &cell_id, &grid_id, &grid_id, &stream);
	nGrid++;
	fscanf(fp, "%c", &separator);
        if (separator == ';') {
          fscanf(fp, "%6s", str);
          if (strncmp(str, "UNLOCK", 6) == 0)
            break;
          else
            fscanf(fp, " INTO %s VALUES ", str);
        }
      }
      FCLOSE(fp);
    }
    grid = (grid_t *) MALLOC(sizeof(grid_t)*nGrid);
    grid_order = (double *) MALLOC(sizeof(double)*nGrid);
    p = (size_t *) MALLOC(sizeof(size_t)*nGrid);

    // Read in the information from the cell connectivity table
    printf("Reading cell connectivity table ...\n");
    nGrid = 0;
    if (db) {
      fp = FOPEN(cell_connectivity, "r");
      while (fgets(line, 1024, fp)) {
	sscanf(line, "%ld %ld %d", &grid[nGrid].cell_id, &grid[nGrid].grid_id, 
	       &grid[nGrid].ordering);
	grid_order[nGrid] = grid[nGrid].cell_id + 
	  (double) grid[nGrid].ordering / 100;
	grid[nGrid].alive = TRUE;
	nGrid++;
      }
      FCLOSE(fp);
    }
    else {
      fp = FOPEN(cell_connectivity, "r");
      while (fgets(line, 1024, fp)) {
	if (strstr(line, "LOCK"))
	  break;
      }
      fscanf(fp, "INSERT INTO %s VALUES ", str);
      while (!feof(fp)) {
	fscanf(fp, "(%ld,%ld,%d,'%c')", &grid[nGrid].cell_id, 
	       &grid[nGrid].grid_id, &grid[nGrid].ordering, &stream);
	grid_order[nGrid] = grid[nGrid].cell_id + 
	  (double) grid[nGrid].ordering / 100;
	grid[nGrid].alive = TRUE;
	nGrid++;
	fscanf(fp, "%c", &separator);
        if (separator == ';') {
          fscanf(fp, "%6s", str);
          if (strncmp(str, "UNLOCK", 6) == 0)
            break;
          else
            fscanf(fp, " INTO %s VALUES ", str);
        }
      }
      FCLOSE(fp);
    }

    // Sort the connectivity table
    gsl_sort_index(p, grid_order, 1, nGrid);
    FREE(grid_order);

    // Determine how may cells we need to deal with and allocate the
    // appropriate memory
    for (i=0; i<nGrid; i++) {
      if (grid[p[i]].cell_id > old_cell) {
	nCell++;
	old_cell = grid[p[i]].cell_id;
      }
    }
    cell = (cell_t *) MALLOC(sizeof(cell_t)*nCell);
    printf("Number of cells: %ld\n", nCell);
    printf("Number of grid points: %ld\n\n", nGrid);

    // Initialize cell structure with IDs
    nCell = 0;
    old_cell = 0;
    for (i=0; i<nGrid; i++) {
      if (grid[p[i]].cell_id > old_cell) {
	cell[nCell].cell_id = grid[p[i]].cell_id;
	nCell++;
	old_cell = grid[p[i]].cell_id;
      }
    }

    // Read cell definition table
    printf("Read cell definition table ...\n\n");
    if (db) {
      fp = FOPEN(cell_definition, "r");
      while (fgets(line, 1024, fp)) {
	sscanf(line, "%d %d %d %d %lf %d", &cell_id, &bla, &nVertices, &bla, 
	       &fBla, &status);
	for (i=0; i<nCell; i++) {
	  if (cell[i].cell_id == cell_id) {
	    cell[i].cell_id = cell_id;
	    cell[i].nVertices = nVertices;
	    if (status == -1)
	      cell[i].alive = TRUE;
	    else
	      cell[i].alive = FALSE;
	  }
	}
      }
      FCLOSE(fp);
    }
    else {
      fp = FOPEN(cell_definition, "r");
      while (fgets(line, 1024, fp)) {
	if (strstr(line, "LOCK"))
	  break;
      }
      fscanf(fp, "INSERT INTO %s VALUES ", str);
      while (!feof(fp)) {
	fscanf(fp, "(%d,%d,%d,%d,%lf,%d,%d,'%c',%lf,%lf,%lf)", 
	       &cell_id, &bla, &nVertices, &bla, &fBla, &status, &bla, 
	       &stream, &fBla, &fBla, &fBla);
	for (i=0; i<nCell; i++) {
	  if (cell[i].cell_id == cell_id) {
	    cell[i].cell_id = cell_id;
	    cell[i].nVertices = nVertices;
	    if (status == -1)
	      cell[i].alive = TRUE;
	    else
	      cell[i].alive = FALSE;
	  }
	}
	fscanf(fp, "%c", &separator);
        if (separator == ';') {
          fscanf(fp, "%6s", str);
          if (strncmp(str, "UNLOCK", 6) == 0)
            break;
          else
            fscanf(fp, " INTO %s VALUES ", str);
        }
      }
      FCLOSE(fp);
    }

    // Read cell attribute table
    printf("Read cell attribute table ...\n\n");
    if (db) {
      fp = FOPEN(cell_attributes, "r");
      while (fgets(line, 1024, fp)) {
	sscanf(line, "%d %d %lf %lf %c %d %d %d %d %d %d %d %d %d %d %d %d "
	       "%d %d %d %d %d %d %d %d %d %d %d %d %d %lf %lf %lf %lf %lf "
	       "%lf %lf %lf %lf %lf %lf %lf %lf",
	       &cell_id, &jd.year, &cellDay, &area, &stream, &bla, &bla, 
	       &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, 
	       &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, 
	       &bla, &bla, &bla, &multi_year_ice, &open_water, 
	       &incidence_angle, &cell_x, &cell_y, &dudx, &dudy, &dvdx, 
	       &dvdy, &dtp, &temperature, &u_wind, &v_wind);
	for (i=0; i<nCell; i++) {
	  if (cell[i].cell_id == cell_id) {
	    jd.jd = (int) cellDay;
	    date_jd2ymd(&jd, &date);
	    sprintf(cell[i].date, "%d/%d/%d", date.month, date.day, date.year);
	    cell[i].day = cellDay;
	    sprintf(cell[i].stream, "%c", stream);
	    cell[i].area = area;
	    cell[i].multi_year_ice = multi_year_ice;
	    cell[i].open_water = open_water;
	    cell[i].incidence_angle = incidence_angle;
	    cell[i].cell_x = cell_x;
	    cell[i].cell_y = cell_y;
	    cell[i].dudx = dudx;
	    cell[i].dudy = dudy;
	    cell[i].dvdx = dvdx;
	    cell[i].dvdy = dvdy;
	    cell[i].dtp = dtp;
	    cell[i].temperature = temperature;
	    cell[i].u_wind = u_wind;
	    cell[i].v_wind = v_wind;
	  }
	}
      }
      FCLOSE(fp);
    }
    else {
      fp = FOPEN(cell_attributes, "r");
      while (fgets(line, 1024, fp)) {
	if (strstr(line, "LOCK"))
	  break;
      }
      fscanf(fp, "INSERT INTO %s VALUES ", str);
      while (!feof(fp)) {
	fscanf(fp, "(%d,%d,%lf,%lf,'%c',%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,"
	       "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%lf,%lf,%lf,%lf,%lf,"
	       "%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf)",
	       &cell_id, &jd.year, &cellDay, &area, &stream, &bla, &bla, 
	       &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, 
	       &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, &bla, 
	       &bla, &bla, &bla, &multi_year_ice, &open_water, 
	       &incidence_angle, &cell_x, &cell_y, &dudx, &dudy, &dvdx, 
	       &dvdy, &dtp, &temperature, &u_wind, &v_wind);
	for (i=0; i<nCell; i++) {
	  if (cell[i].cell_id == cell_id) {
	    jd.jd = (int) cellDay;
	    date_jd2ymd(&jd, &date);
	    sprintf(cell[i].date, "%d/%d/%d", date.month, date.day, date.year);
	    cell[i].day = cellDay;
	    sprintf(cell[i].stream, "%c", stream);
	    cell[i].area = area;
	    cell[i].multi_year_ice = multi_year_ice;
	    cell[i].open_water = open_water;
	    cell[i].incidence_angle = incidence_angle;
	    cell[i].cell_x = cell_x;
	    cell[i].cell_y = cell_y;
	    cell[i].dudx = dudx;
	    cell[i].dudy = dudy;
	    cell[i].dvdx = dvdx;
	    cell[i].dvdy = dvdy;
	    cell[i].dtp = dtp;
	    cell[i].temperature = temperature;
	    cell[i].u_wind = u_wind;
	    cell[i].v_wind = v_wind;
	  }
	}
	fscanf(fp, "%c", &separator);
        if (separator == ';') {
          fscanf(fp, "%6s", str);
          if (strncmp(str, "UNLOCK", 6) == 0)
            break;
          else
            fscanf(fp, " INTO %s VALUES ", str);
        }
      }
      FCLOSE(fp);
    }

    // Read projection file
    read_proj_file("polar_stereographic_north.proj", 
		   &pps, &proj_type, &datum, &spheroid);

    // Initialize meta_projection block
    meta_proj = meta_projection_init();
    meta_proj->type = proj_type;
    meta_proj->datum = datum;
    meta_proj->spheroid = spheroid;
    meta_proj->param = pps;

    // Determine how many observations are available in the grid point file
    if (db) {
      fp = FOPEN(grid_points, "r");
      while (fgets(line, 1024, fp))
	nGridAttr++;
      FCLOSE(fp);
    }
    else {
      fp = FOPEN(grid_points, "r");
      while (fgets(line, 1024, fp)) {
        if (strstr(line, "LOCK"))
          break;
      }
      fscanf(fp, "INSERT INTO %s VALUES ", str);
      while (!feof(fp)) {
        fscanf(fp, "(%d,%d,%d,%lf,%lf,%lf,%lf,'%16s','%16s',%d,'%c',%d)",
               &grid_id, &nBla, &jd.year, &gridDay, &grid_x, &grid_y, &fBla,
               sourceImage, targetImage, &nBla, &stream, &quality);
	nGridAttr++;
        fscanf(fp, "%c", &separator);
        if (separator == ';') {
          fscanf(fp, "%6s", str);
          if (strncmp(str, "UNLOCK", 6) == 0)
            break;
          else
            fscanf(fp, " INTO %s VALUES ", str);
        }
      }
      FCLOSE(fp);
    }

    // Allocate the necessary memory for grid attribute information
    gridAttr = (grid_attr_t *) MALLOC(sizeof(grid_attr_t)*nGridAttr);

    // Read grid point table and determine lat/lon for each grid point
    printf("\nReading grid point table ...\n\n");
    k = 0;
    if (db) {
      fp = FOPEN(grid_points, "r");
      while (fgets(line, 1024, fp)) {
        sscanf(line, "%d %d %d %lf %lf %lf %lf %16s %16s %d %c %d",
               &grid_id, &nBla, &jd.year, &gridDay, &grid_x, &grid_y, &fBla,
               sourceImage, targetImage, &nBla, &stream, &quality);
        sourceImage[16] = 0;
        targetImage[16] = 0;
	jd.jd = (int) gridDay;
	date_jd2ymd(&jd, &date);
	gridAttr[k].grid_id = grid_id;
	sprintf(gridAttr[k].date, "%d/%d/%d", date.month, date.day, date.year);
	gridAttr[k].day = gridDay;
	gridAttr[k].grid_x = grid_x;
	gridAttr[k].grid_y = grid_y;
	proj_to_latlon(meta_proj, grid_x, grid_y, 0.0,
                           &gridAttr[k].lat, &gridAttr[k].lon, &height);
        gridAttr[k].lat *= R2D;
        gridAttr[k].lon *= R2D;
	sprintf(gridAttr[k].sourceImage, "%s", sourceImage);
	sprintf(gridAttr[k].targetImage, "%s", targetImage);
	sprintf(gridAttr[k].stream, "%c", stream);
	gridAttr[k].quality = quality;
	gridAttr[k].alive = TRUE;
	gridAttr[k].done = FALSE;
	k++;
      }
      FCLOSE(fp);
    }
    else {
      fp = FOPEN(grid_points, "r");
      while (fgets(line, 1024, fp)) {
        if (strstr(line, "LOCK"))
          break;
      }
      fscanf(fp, "INSERT INTO %s VALUES ", str);
      while (!feof(fp)) {
        fscanf(fp, "(%d,%d,%d,%lf,%lf,%lf,%lf,'%16s','%16s',%d,'%c',%d)",
               &grid_id, &nBla, &jd.year, &gridDay, &grid_x, &grid_y, &fBla,
               sourceImage, targetImage, &nBla, &stream, &quality);
        sourceImage[16] = 0;
        targetImage[16] = 0;
	jd.jd = (int) gridDay;
	date_jd2ymd(&jd, &date);
	gridAttr[k].grid_id = grid_id;
	sprintf(gridAttr[k].date, "%d/%d/%d", date.month, date.day, date.year);
	gridAttr[k].day = gridDay;
	gridAttr[k].grid_x = grid_x;
	gridAttr[k].grid_y = grid_y;
	proj_to_latlon(meta_proj, grid_x, grid_y, 0.0,
		       &gridAttr[k].lat, &gridAttr[k].lon, &height);
	gridAttr[k].lat *= R2D;
	gridAttr[k].lon *= R2D;
	sprintf(gridAttr[k].sourceImage, "%s", sourceImage);
	sprintf(gridAttr[k].targetImage, "%s", targetImage);
	sprintf(gridAttr[k].stream, "%c", stream);
	gridAttr[k].quality = quality;
	gridAttr[k].alive = TRUE;
	gridAttr[k].done = FALSE;
	k++;
        fscanf(fp, "%c", &separator);
        if (separator == ';') {
          fscanf(fp, "%6s", str);
          if (strncmp(str, "UNLOCK", 6) == 0)
            break;
          else
            fscanf(fp, " INTO %s VALUES ", str);
        }
      }
      FCLOSE(fp);
    }

    // Read grid definition table
    printf("Read grid definition table ...\n\n");
    if (db) {
      fp = FOPEN(grid_definition, "r");
      while (fgets(line, 1024, fp)) {
        sscanf(line, "%d %d %lf %d %lf", 
	       &grid_id, &bla, &fBla, &status, &gridDay);
        for (i=0; i<nGrid; i++) {
          if (grid[i].grid_id == grid_id && status != -1)
            grid[i].alive = FALSE;
        }
        for (i=0; i<nGridAttr; i++) {
          if (gridAttr[i].grid_id == grid_id && status != -1 &&
	      gridAttr[i].day > gridDay)
            gridAttr[i].alive = FALSE;
        }
      }
      FCLOSE(fp);
    }
    else {
      fp = FOPEN(grid_definition, "r");
      while (fgets(line, 1024, fp)) {
        if (strstr(line, "LOCK"))
          break;
      }
      fscanf(fp, "INSERT INTO %s VALUES ", str);
      while (!feof(fp)) {
        fscanf(fp, "(%d,%d,%lf,%d,%lf,'%c')",
               &grid_id, &bla, &fBla, &status, &fBla, &stream);
        for (i=0; i<nGrid; i++) {
          if (grid[i].grid_id == grid_id && status != -1)
            grid[i].alive = FALSE;
        }
        for (i=0; i<nGridAttr; i++) {
          if (gridAttr[i].grid_id == grid_id && status != -1 &&
              gridAttr[i].day > gridDay)
            gridAttr[i].alive = FALSE;
        }
        fscanf(fp, "%c", &separator);
        if (separator == ';') {
          fscanf(fp, "%6s", str);
          if (strncmp(str, "UNLOCK", 6) == 0)
            break;
          else
            fscanf(fp, " INTO %s VALUES ", str);
        }
      }
      FCLOSE(fp);
    }

    // Initialize cell vector files
    sprintf(outFile, "cells_%s", snapshot);
    shape_init(outFile, RGPS);
    open_shape(outFile, &dbase, &shape);
    sprintf(kmlFile, "%s.kml", outFile);
    fpKml = FOPEN(kmlFile, "w");
    kml_header(fpKml);

    // Write cell info into vector files
    printf("Writing cell vector files ...\n\n");
    n = 0;
    for (i=0; i<nCell; i++) {
      if (cell[i].alive) {
	nVertices = 0;
	for (k=0; k<nGrid; k++) {
	  if (grid[p[k]].alive) {
	    gridDay = 0.0;
	    if (grid[p[k]].cell_id == cell[i].cell_id) {
	      for (l=0; l<nGridAttr; l++) {
		if (gridAttr[l].grid_id == grid[p[k]].grid_id &&
		    gridAttr[l].day == cell[i].day) {
		  lat[nVertices] = gridAttr[l].lat;
		  lon[nVertices] = gridAttr[l].lon;
		  sprintf(cell[i].sourceImage, "%s", gridAttr[l].sourceImage);
		  sprintf(cell[i].targetImage, "%s", gridAttr[l].targetImage);
		  nVertices++;
		  break;
		}
	      }
	    }
	  }
	}
	lat[nVertices] = lat[0];
	lon[nVertices] = lon[0];
	cell[i].nVertices = nVertices;
	if (nVertices > 3) {
	  rgps2shape(cell[i], lat, lon, nVertices, dbase, shape, n);
	  rgps2kml(cell[i], lat, lon, fpKml);
	  n++;
	}
      }
    }

    // Wrap up cell vector files
    close_shape(dbase, shape);
    write_esri_proj_file(outFile);
    kml_footer(fpKml);
    FCLOSE(fpKml);
    sprintf(cmd, "zip %s.kmz %s", outFile, kmlFile);
    asfSystem(cmd);

    // Initialize grid vector files
    sprintf(outFile, "grid_%s", snapshot);
    shape_init(outFile, RGPS_GRID);
    open_shape(outFile, &dbase, &shape);
    sprintf(kmlFile, "%s.kml", outFile);
    fpKml = FOPEN(kmlFile, "w");
    kml_header(fpKml);

    // Write grid info into vector files
    printf("Writing grid vector files ...\n\n");
    n = 0;
    for (i=0; i<nGrid; i++) {
      if (grid[i].alive) {
	index = 0;
	gridDay = 0.0;
	// Look up latest observation for each grid point
	for (k=0; k<nGridAttr; k++) {
	  if (gridAttr[k].grid_id == grid[i].grid_id &&
	      gridAttr[k].day > gridDay) {
	    index = k;
	    gridDay = gridAttr[k].day;
	  }
	}
	if (!gridAttr[index].done) {
	  // Write the latest information to vector files
	  rgps_grid2shape(gridAttr[index], dbase, shape, n);
	  rgps_grid2kml(gridAttr[index], fpKml);
	  n++;
	}
	gridAttr[index].done = TRUE;
      }
    }

    // Wrap up grid vector files
    close_shape(dbase, shape);
    write_esri_proj_file(outFile);
    kml_footer(fpKml);
    FCLOSE(fpKml);
    sprintf(cmd, "zip %s.kmz %s %s/grid_point.png", 
	    outFile, kmlFile, get_asf_share_dir());
    asfSystem(cmd);

    // Clean up
    FREE(p);
    FREE(grid);
    FREE(gridAttr);
    FREE(cell);
  }
  else {
    asfPrintStatus("Generating shape file for RGPS weather information\n\n");

    sscanf(date_str, "%d/%d/%d", &date.month, &date.day, &date.year);
    fp = FOPEN(weather_table, "r");
    shape_init(outFile, RGPS_WEATHER);
    open_shape(outFile, &dbase, &shape);
    while (fgets(line, 1024, fp)) {
      sscanf(line, "%d %lf %lf %lf %lf %lf %lf %lf", &jd.year, &tmp, 
	     &latitude, &longitude, &u, &v, &temperature, &pressure);
      jd.jd = (int) tmp;
      date_jd2ymd(&jd, &rgpsDate);
      if (date.year >= rgpsDate.year &&
	  date.month >= rgpsDate.month &&
	  date.day >= rgpsDate.day) {
	speed = sqrt(u*u + v*v);
	direction = atan2(u, v)*R2D;
	if (direction < 0)
	  direction += 360.0;
	sprintf(line, "%s,%.4lf,%.4lf,%.4lf,%.1lf,%.1lf,%.1lf\n",
		date_str, latitude, longitude, direction, speed, temperature, 
		pressure);
 	rgps_weather2shape(line, dbase, shape, n);
	n++;
      }
    }
    FCLOSE(fp);
    close_shape(dbase, shape);
    write_esri_proj_file(outFile);
  }

  return(0);
}
