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
   "   motion         File name of the grid CSV file (input).\n"
   "   deformation    File name of the deformation file (input).\n"
   "   connectivity   File name of the connectivity CSV file (input).\n"
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
  rgps_con_t *con, int nCon, dbf_header_t *dbf, FILE *fpOut)
{
  char **col;
  int ii, nCols, nCoords=0, cell_id=0, obs_year;
  double obs_time;

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
    else if (strcmp_case(dbf[ii].shape, "OBS_TIME") == 0)
      obs_time = dbf[ii].fValue;
  }
  FREE(col);

  // Extract information from connectivity table
  int *grid_id = (int *) MALLOC(sizeof(int)*50);
  double *grid_order = (double *) MALLOC(sizeof(double)*50);
  size_t *p = (size_t *) MALLOC(sizeof(size_t)*50);
  for (ii=0; ii<nCon; ii++) {
    if (cell_id == con[ii].cell_id) {
      grid_id[nCoords] = con[ii].grid_id;
      grid_order[nCoords] = con[ii].cell_id + (double) con[ii].order / 100;
      nCoords++;
    }
  }
  gsl_sort_index(p, grid_order, 1, nCoords);
  fprintf(fpOut, "%s", line);

  // Extract grid information from motion product
  int kk;
  double diff;
  for (kk=0; kk<nCoords; kk++) {
    for (ii=0; ii<nGrids; ii++) {
      diff = fabs(obs_time - grid[ii].obs_time);
      if (grid_id[p[kk]] == grid[ii].gpid && obs_year == grid[ii].obs_year && 
        diff < 0.01)
        fprintf(fpOut, ",%.4f,%.4f", grid[ii].x, grid[ii].y);
    }
  }
  fprintf(fpOut, "\n");

  // Clean up
  FREE(grid_id);
  FREE(grid_order);
  FREE(p);

  return;
}

int main(int argc, char **argv)
{
  dbf_header_t *header;
  extern int currArg; // Pre-initialized to 1
  int n=0;
  char motion[512], deformation[512], connectivity[512], cell[512];
  char shape_type[25], line[1024], **col, *motion2;

  // Parse command line
  if ((argc-currArg)<1) {
    printf("Insufficient arguments.\n"); 
    usage();
  }
  strcpy(motion, argv[currArg]);
  strcpy(deformation, argv[currArg+1]);
  strcpy(connectivity, argv[currArg+2]);
  strcpy(cell, argv[currArg+3]);

  asfSplashScreen(argc, argv);

  // Reading connectivity file
  asfPrintStatus("Reading connectivity file ...\n");
  FILE *fp = FOPEN(connectivity, "r");
  int ii, kk, nCon = 0;
  fgets(line, 1024, fp);
  while (fgets(line, 1024, fp))
    nCon++;
  FCLOSE(fp);
  rgps_con_t *con = (rgps_con_t *) MALLOC(sizeof(rgps_con_t)*nCon);
  fp = FOPEN(connectivity, "r");
  fgets(line, 1024, fp);
  for (ii=0; ii<nCon; ii++) {
    fgets(line, 1024, fp);
    chomp(line);
    split_into_array(line, ',', &n, &col);
    con[ii].cell_id = atoi(col[0]);
    con[ii].grid_id = atoi(col[1]);
    con[ii].order = atoi(col[2]);
    FREE(col);
  }
  FCLOSE(fp);
  
  // Set up motion connectivity file
  motion2 = appendExt(motion, "cell.csv");
  FILE *fpLP = FOPEN(motion2, "w");
   
  // Reading ice motion file
  asfPrintStatus("Reading ice motion file ...\n");
  read_header_config("RGPS_LP_GRID", &header, &n, shape_type);
  fp = FOPEN(motion, "r");
  int nGrids = 0;
  fgets(line, 1024, fp);
  chomp(line);
  fprintf(fpLP, "%s,CELL_ID,ORDER\n", line);
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
      else if (strcmp_case(header[kk].shape, "X_MAP") == 0)
        grid[ii].x = atof(col[kk]);
      else if (strcmp_case(header[kk].shape, "Y_MAP") == 0)
        grid[ii].y = atof(col[kk]);
    }
    for (kk=0; kk<nCon; kk++) {
      if (grid[ii].gpid == con[kk].grid_id)
        fprintf(fpLP, "%s,%d,%d\n", line, con[kk].cell_id, con[kk].order);
    }
    FREE(col);
  }
  FCLOSE(fp);
  FCLOSE(fpLP);

  // Reading ice deformation file
  asfPrintStatus("Converting ice deformation file ...\n");
  read_header_config("RGPS_DP_CELL", &header, &n, shape_type);
  fp = FOPEN(deformation, "r");
  FILE *fpOut = FOPEN(cell, "w");
  fprintf(fpOut, "CELL_ID,STREAM,BIRTH_YEAR,BIRTH_TIME,N_OBS,OBS_YEAR,OBS_TIME,"
    "X_MAP,Y_MAP,LAT,LON,X_DISP,Y_DISP,C_AREA,D_AREA,DTP,DUDX,DUDY,DVDX,DVDY,"
    "IMAGE_ID\n");
  fgets(line, 1024, fp); // header line
  while (fgets(line, 1024, fp)) {
    chomp(line);
    rgps_grid2cell(line, grid, nGrids, con, nCon, header, fpOut);
  }
  FCLOSE(fp);
  FCLOSE(fpOut);
  FREE(grid);
  FREE(con);
  
  return(0);
}
