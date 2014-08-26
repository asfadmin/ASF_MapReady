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
   "   rgps_grid2cell <motion> <deformation> <connectivity> <cell>\n");
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   motion         File name of the motion file (input).\n"
   "   deformation    File name of the deformation file (input).\n"
   "   connectivity   File name of the cell connectivity CSV file (input).\n"
   "   definition     File name of the cell definition CSV file (input).\n"
   "   cell           File name of the cell CSV file (output).\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program adds cell vertex coordinates to RGPS MEaSUREs products\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

static void rgps_grid2cell(char *line, rgps_grid_t *grid, int nGrids, 
  rgps_cell_t *cell, int nCells, dbf_header_t *dbf, FILE *fpOut)
{
  char **col;
  int ii, nCols, nCoords=0, cell_id=0, obs_year;
  double obs_time, x_disp, y_disp;

  // Extract information from line
  split_into_array(line, ',', &nCols, &col);
  for (ii=0; ii<nCols; ii++) {
    if (dbf[ii].format == CSV_STRING)
      dbf[ii].sValue = STRDUP(col[ii]);
    else if (dbf[ii].format == CSV_DOUBLE)
      dbf[ii].fValue = atof(col[ii]);
    else if (dbf[ii].format == CSV_INTEGER)
      dbf[ii].nValue = atoi(col[ii]);
    if (strcmp_case(dbf[ii].shape, "CELL_ID") == 0)
      cell_id = dbf[ii].nValue;
    else if (strcmp_case(dbf[ii].shape, "OBS_YEAR") == 0)
      obs_year = dbf[ii].nValue;
    else if (strcmp_case(dbf[ii].shape, "OBS_TIME") == 0) {
      obs_time = dbf[ii].fValue;
      if (obs_year > grid[0].obs_year)
        obs_time += date_getDaysInYear(grid[0].obs_year);
    }
    else if (strcmp_case(dbf[ii].shape, "X_DISP") == 0)
      x_disp = dbf[ii].fValue;
    else if (strcmp_case(dbf[ii].shape, "Y_DISP") == 0)
      y_disp = dbf[ii].fValue;
  }
  free_char_array(&col, nCols);

  // Extract information from connectivity table
  int *grid_id = (int *) MALLOC(sizeof(int)*50);
  double *grid_order = (double *) MALLOC(sizeof(double)*50);
  size_t *p = (size_t *) MALLOC(sizeof(size_t)*50);
  for (ii=0; ii<nCells; ii++) {
    if (cell_id == cell[ii].cell_id) {
      grid_id[nCoords] = cell[ii].grid_id;
      grid_order[nCoords] = cell[ii].cell_id + (double) cell[ii].order / 100;
      nCoords++;
    }
  }
  gsl_sort_index(p, grid_order, 1, nCoords);
  double disp_mag = sqrt(x_disp*x_disp + y_disp*y_disp);

  // Extract grid information from motion product
  int kk, index;
  double diff, grid_time, birth_time, death_time;
  char image_id[30];
  char *tmp = (char *) MALLOC(sizeof(char)*25);
  char *coordStr = (char *) MALLOC(sizeof(char)*50*nCoords);
  strcpy(coordStr, "");
  for (kk=0; kk<nCoords; kk++) {
    index = -1;
    for (ii=0; ii<nGrids; ii++) {
      grid_time = grid[ii].obs_time;
      if (grid[ii].obs_year > grid[0].obs_year)
        grid_time += date_getDaysInYear(grid[0].obs_year);
      diff = fabs(grid_time - obs_time);
      if ((grid_id[p[kk]] == grid[ii].gpid) && (diff < 0.01)) {
        birth_time = grid[ii].birth_time;
        if (grid[ii].birth_year > grid[0].obs_year)
          birth_time += date_getDaysInYear(grid[0].obs_year);
        index = ii;
      }
    }
    if (index > 0 && birth_time <= grid_time) {
      sprintf(tmp, ",%.4f,%.4f", grid[index].x, grid[index].y);
      strcat(coordStr, tmp);
      strcpy(image_id, grid[index].image_id);
    }
  }

  // Check cell death time    
  int n, cell_death_year;
  double cell_death_time;
  split_into_array(line, ',', &n, &col);
  for (ii=0; ii<nCells; ii++) {
    if (cell[ii].cell_id == atoi(col[0])) {
      cell_death_time = death_time = cell[ii].death_time;
      cell_death_year = cell[ii].death_year;
      if (cell[ii].death_year > cell[0].birth_year)
        death_time += date_getDaysInYear(cell[0].birth_year);
      if (cell[ii].death_year == -1)
        death_time = -1;
    }
  }
  if (death_time < 0 || obs_time < (death_time + 0.01)) {
    fprintf(fpOut, "%.6f,%s,%s,%s", obs_time, col[0], col[1], col[2]);
    fprintf(fpOut, ",%s,%d,%.6f", col[3], cell_death_year, cell_death_time);
    for (kk=4; kk<n; kk++)
      fprintf(fpOut, ",%s", col[kk]);
    fprintf(fpOut, ",%s,%.6f%s\n", image_id, disp_mag, coordStr);
  }
  free_char_array(&col, n);

  // Clean up
  FREE(grid_id);
  FREE(grid_order);
  FREE(p);
  FREE(tmp);
  FREE(coordStr);

  return;
}

int main(int argc, char **argv)
{
  dbf_header_t *header;
  extern int currArg; // Pre-initialized to 1
  int n=0;
  char motion[512], deformation[512], connectivity[512], definition[512];
  char cell[512], shape_type[25], line[1024], **col;

  // Parse command line
  if ((argc-currArg)<1) {
    printf("Insufficient arguments.\n"); 
    usage();
  }
  strcpy(motion, argv[currArg]);
  strcpy(deformation, argv[currArg+1]);
  strcpy(connectivity, argv[currArg+2]);
  strcpy(definition, argv[currArg+3]);
  strcpy(cell, argv[currArg+4]);

  asfSplashScreen(argc, argv);

  // Reading cell connectivity file
  asfPrintStatus("Reading cell connectivity file ...\n");
  FILE *fp = FOPEN(connectivity, "r");
  int ii, kk, nCells = 0;
  fgets(line, 1024, fp);
  while (fgets(line, 1024, fp))
    nCells++;
  FCLOSE(fp);
  rgps_cell_t *cells = (rgps_cell_t *) MALLOC(sizeof(rgps_cell_t)*nCells);
  fp = FOPEN(connectivity, "r");
  fgets(line, 1024, fp);
  for (ii=0; ii<nCells; ii++) {
    fgets(line, 1024, fp);
    chomp(line);
    split_into_array(line, ',', &n, &col);
    cells[ii].cell_id = atoi(col[0]);
    cells[ii].grid_id = atoi(col[1]);
    cells[ii].order = atoi(col[2]);
    free_char_array(&col, n);
  }
  FCLOSE(fp);

  // Reading cell definition file
  asfPrintStatus("Reading cell definition file ...\n");
  fp = FOPEN(definition, "r");
  fgets(line, 1024, fp);
  while (fgets(line, 1024, fp)) {
    chomp(line);
    split_into_array(line, ',', &n, &col);
    for (ii=0; ii<nCells; ii++)
      if (cells[ii].cell_id == atoi(col[0])) {
        cells[ii].birth_year = atoi(col[3]);
        cells[ii].birth_time = atof(col[4]);
        cells[ii].death_year = atoi(col[5]);
        cells[ii].death_time = atof(col[6]);
      }
    free_char_array(&col, n);
  }
  FCLOSE(fp);
  
  // Reading ice motion file
  asfPrintStatus("Reading ice motion file ...\n");
  read_header_config("RGPS_LP_GRID", &header, &n, shape_type);
  fp = FOPEN(motion, "r");
  int nGrids = 0;
  fgets(line, 1024, fp);
  chomp(line);
  while (fgets(line, 1024, fp))
    nGrids++;
  FCLOSE(fp);
  rgps_grid_t *grid = (rgps_grid_t *) MALLOC(sizeof(rgps_grid_t)*nGrids);
  fp = FOPEN(motion, "r");
  fgets(line, 1024, fp);
  for (ii=0; ii<nGrids; ii++) {
    fgets(line, 1024, fp);
    chomp(line);
    split_into_array(line, ',', &n, &col);
    for (kk=0; kk<n; kk++) {
      if (strcmp_case(header[kk].shape, "IMAGE_ID") == 0)
        strcpy(grid[ii].image_id, col[kk]);
      else if (strcmp_case(header[kk].shape, "GPID") == 0)
        grid[ii].gpid = atoi(col[kk]);
      else if (strcmp_case(header[kk].shape, "OBS_YEAR") == 0)
        grid[ii].obs_year = atoi(col[kk]);
      else if (strcmp_case(header[kk].shape, "OBS_TIME") == 0)
        grid[ii].obs_time = atof(col[kk]);
      else if (strcmp_case(header[kk].shape, "BIRTH_YEAR") == 0)
        grid[ii].birth_year = atoi(col[kk]);
      else if (strcmp_case(header[kk].shape, "BIRTH_TIME") == 0)
        grid[ii].birth_time = atof(col[kk]);
      else if (strcmp_case(header[kk].shape, "DEATH_YEAR") == 0)
        grid[ii].death_year = atoi(col[kk]);
      else if (strcmp_case(header[kk].shape, "DEATH_TIME") == 0)
        grid[ii].death_time = atof(col[kk]);
      else if (strcmp_case(header[kk].shape, "X_MAP") == 0)
        grid[ii].x = atof(col[kk]);
      else if (strcmp_case(header[kk].shape, "Y_MAP") == 0)
        grid[ii].y = atof(col[kk]);
    }
    free_char_array(&col, n);
  }
  FCLOSE(fp);

  // Reading ice deformation file
  asfPrintStatus("Converting ice deformation file ...\n");
  read_header_config("RGPS_DP_GRID", &header, &n, shape_type);
  fp = FOPEN(deformation, "r");
  FILE *fpOut = FOPEN(cell, "w");
  fprintf(fpOut, "CELL_TIME,CELL_ID,STREAM,BIRTH_YEAR,BIRTH_TIME,DEATH_YEAR,"
    "DEATH_TIME,N_OBS,OBS_YEAR,OBS_TIME,X_MAP,Y_MAP,LAT,LON,X_DISP,Y_DISP,"
    "C_AREA,D_AREA,DTP,DUDX,DUDY,DVDX,DVDY,IMAGE_ID,DISP_MAG\n");
  fgets(line, 1024, fp); // header line
  while (fgets(line, 1024, fp)) {
    chomp(line);
    rgps_grid2cell(line, grid, nGrids, cells, nCells, header, fpOut);
  }
  FCLOSE(fp);
  FCLOSE(fpOut);
  FREE(grid);
  FREE(cells);
  
  return(0);
}
