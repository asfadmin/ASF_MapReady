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
   "   rgps2vector <type> <inFile> <outFile>\n");
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   type      RGPS_LP_GRID - Lagrangian ice motion grid file\n"
   "             RGPS_DP_GRID - Lagrangian ice deformation grid file\n"
   "             RGPS_DP_CELL - Lagrangian ice deformation cell file\n"
   "             RGPS_EP_GRID - Eulerian ice motion grid file\n"
   "   inFile    File name of CSV file\n"
   "   outFile   Basename of the shapefile\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program converts a RGPS MEaSUREs product in CSV format into\n"
   "   ArcGIS shape file.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

// Convert RGPS cell data to shape
void rgps_cell2shape(char *line, int nCols, DBFHandle dbase, SHPHandle shape, 
  int n, dbf_header_t *dbf)
{
  char **col;
  int ii, nColumns=0;

  // Extract information from line
  split_into_array(line, ',', &nColumns, &col);
  for (ii=0; ii<nCols; ii++) {
    if (dbf[ii].format == CSV_STRING)
      dbf[ii].sValue = STRDUP(col[ii]);
    else if (dbf[ii].format == CSV_DOUBLE)
      dbf[ii].fValue = atof(col[ii]);
    else if (dbf[ii].format == CSV_INTEGER)
      dbf[ii].nValue = atoi(col[ii]);
  }
  int nCoords = (nColumns - nCols)/2;
  double *x = (double *) MALLOC(sizeof(double)*nCoords);
  double *y = (double *) MALLOC(sizeof(double)*nCoords);
  for (ii=0; ii<nCoords; ii++) {
    x[ii] = atof(col[nCols+ii*2]);
    y[ii] = atof(col[nCols+ii*2+1]);
  }
  free_char_array(&col, nColumns);

  // Write information into database file
  for (ii=0; ii<nCols; ii++) {
    if (dbf[ii].format == CSV_STRING)
      DBFWriteStringAttribute(dbase, n, ii, dbf[ii].sValue);
    else if (dbf[ii].format == CSV_DOUBLE)
      DBFWriteDoubleAttribute(dbase, n, ii, dbf[ii].fValue);
    else if (dbf[ii].format == CSV_INTEGER)
      DBFWriteIntegerAttribute(dbase, n, ii, dbf[ii].nValue);
  }
  
  // Write shape object
  SHPObject *shapeObject = NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, nCoords, x, y, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  // Clean up
  FREE(x);
  FREE(y);

  return;
}

// Convert RGPS grid data to shape
void rgps_grid2shape(char *line, DBFHandle dbase, SHPHandle shape, int n,
  dbf_header_t *dbf)
{
  double x=0.0, y=0.0;
  int ii, nCols;
  char **col;

  // Extract information from line
  split_into_array(line, ',', &nCols, &col);
  for (ii=0; ii<nCols; ii++) {
    if (dbf[ii].format == CSV_STRING)
      dbf[ii].sValue = STRDUP(col[ii]);
    else if (dbf[ii].format == CSV_DOUBLE)
      dbf[ii].fValue = atof(col[ii]);
    else if (dbf[ii].format == CSV_INTEGER)
      dbf[ii].nValue = atoi(col[ii]);
  }
  free_char_array(&col, nCols);


  // Write information into database file
  for (ii=0; ii<nCols; ii++) {
    if (dbf[ii].format == CSV_STRING)
      DBFWriteStringAttribute(dbase, n, ii, dbf[ii].sValue);
    else if (dbf[ii].format == CSV_DOUBLE)
      DBFWriteDoubleAttribute(dbase, n, ii, dbf[ii].fValue);
    else if (dbf[ii].format == CSV_INTEGER)
      DBFWriteIntegerAttribute(dbase, n, ii, dbf[ii].nValue);
    if (strcmp_case(dbf[ii].shape, "X_MAP") == 0)
      x = dbf[ii].fValue;
    else if (strcmp_case(dbf[ii].shape, "Y_MAP") == 0)
      y = dbf[ii].fValue;
  }
  
  // Write shape object
  SHPObject *shapeObject = NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POINT, 1, &x, &y, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  return;
}

// Convert RGPS grid data to shape
void rgps_line2shape(char *line, int nCols, DBFHandle dbase, SHPHandle shape, 
  int n, dbf_header_t *dbf)
{
  int ii, kk, nColumns=0, nXMap=0, nYMap=0;
  char **col, str[10];

  // Extract information from line
  split_into_array(line, ',', &nColumns, &col);
  for (ii=0; ii<nCols; ii++) {
    if (dbf[ii].format == CSV_STRING)
      dbf[ii].sValue = STRDUP(col[ii]);
    else if (dbf[ii].format == CSV_DOUBLE)
      dbf[ii].fValue = atof(col[ii]);
    else if (dbf[ii].format == CSV_INTEGER)
      dbf[ii].nValue = atoi(col[ii]);
    if (strncmp_case(dbf[ii].shape, "X_MAP", 5) == 0)
      nXMap++;
    if (strncmp_case(dbf[ii].shape, "Y_MAP", 5) == 0)
      nYMap++;
  }
  if (nXMap != nYMap)
    asfPrintError("Could not read map coordinates (x = %d, y = %d) properly!\n",
      nXMap, nYMap);
  int nCoords = nXMap;
  double *x = (double *) MALLOC(sizeof(double)*nCoords);
  double *y = (double *) MALLOC(sizeof(double)*nCoords);
  free_char_array(&col, nColumns);

  // Write information into database file
  for (ii=0; ii<nCols; ii++) {
    if (dbf[ii].format == CSV_STRING)
      DBFWriteStringAttribute(dbase, n, ii, dbf[ii].sValue);
    else if (dbf[ii].format == CSV_DOUBLE)
      DBFWriteDoubleAttribute(dbase, n, ii, dbf[ii].fValue);
    else if (dbf[ii].format == CSV_INTEGER)
      DBFWriteIntegerAttribute(dbase, n, ii, dbf[ii].nValue);
    for (kk=0; kk<nCoords; kk++) {
      sprintf(str, "X_MAP%d", kk+1);
      if (strcmp_case(dbf[ii].shape, str) == 0)
        x[kk] = dbf[ii].fValue;
      sprintf(str, "Y_MAP%d", kk+1);
      if (strcmp_case(dbf[ii].shape, str) == 0)
        y[kk] = dbf[ii].fValue;
    }
  }
  
  // Write shape object
  SHPObject *shapeObject = NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_ARC, nCoords, x, y, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  // Clean up
  FREE(x);
  FREE(y);

  return;
}

int main(int argc, char **argv)
{
  FILE *fp;
  DBFHandle dbase = NULL;
  SHPHandle shape = NULL;
  dbf_header_t *header;
  extern int currArg; // Pre-initialized to 1
  int n=0, nCols;
  char inFile[512], outFile[512], type[15], shape_type[25], line[1024];

  // Parse command line
  if ((argc-currArg)<1) {
    printf("Insufficient arguments.\n"); 
    usage();
  }
  strcpy(type, argv[currArg]);
  strcpy(inFile, argv[currArg+1]);
  strcpy(outFile, argv[currArg+2]);

  asfSplashScreen(argc, argv);

  if (strcmp_case(type, "RGPS_LP_GRID") == 0) {
    // Generating ice motion grid shapefile
    asfPrintStatus("Generating ice motion grid shapefile ...\n");
    read_header_config("RGPS_LP_GRID", &header, &n, shape_type);
    shapefile_init(inFile, outFile, "RGPS_LP_GRID", NULL);
    open_shape(outFile, &dbase, &shape);
    FILE *fp = FOPEN(inFile, "r");
    fgets(line, 1024, fp); // header line
    n = 0;
    while (fgets(line, 1024, fp)) {
      chomp(line);
      rgps_grid2shape(line, dbase, shape, n, header);
      n++;
    }
    FCLOSE(fp);  
    close_shape(dbase, shape);
  }
  else if (strcmp_case(type, "RGPS_DP_GRID") == 0) {
    // Generating ice deformation grid shapefile
    asfPrintStatus("Generating ice deformation grid shapefile ...\n");
    read_header_config("RGPS_DP_GRID", &header, &n, shape_type);
    shapefile_init(inFile, outFile, "RGPS_DP_GRID", NULL);
    open_shape(outFile, &dbase, &shape);
    fp = FOPEN(inFile, "r");
    fgets(line, 1024, fp); // header line
    n = 0;
    while (fgets(line, 1024, fp)) {
      chomp(line);
      rgps_grid2shape(line, dbase, shape, n, header);
      n++;
    }
    FCLOSE(fp);  
    close_shape(dbase, shape);
  }
  else if (strcmp_case(type, "RGPS_DP_CELL") == 0) {
    // Generating ice deformation cell shapefile
    asfPrintStatus("Generating ice deformation cell shapefile ...\n");
    read_header_config("RGPS_DP_CELL", &header, &nCols, shape_type);
    shapefile_init(inFile, outFile, "RGPS_DP_CELL", NULL);
    open_shape(outFile, &dbase, &shape);
    fp = FOPEN(inFile, "r");
    fgets(line, 1024, fp); // header line
    n = 0;
    while (fgets(line, 1024, fp)) {
      chomp(line);
      rgps_cell2shape(line, nCols, dbase, shape, n, header);
      n++;
    }
    FCLOSE(fp);
    close_shape(dbase, shape);
  }
  else if (strcmp_case(type, "RGPS_EP_GRID") == 0) {
    // Generating ice deformation cell shapefile
    asfPrintStatus("Generating Eulerian grid shapefile ...\n");
    read_header_config("RGPS_EP_GRID", &header, &nCols, shape_type);
    shapefile_init(inFile, outFile, "RGPS_EP_GRID", NULL);
    open_shape(outFile, &dbase, &shape);
    fp = FOPEN(inFile, "r");
    fgets(line, 1024, fp); // header line
    n = 0;
    while (fgets(line, 1024, fp)) {
      chomp(line);
      rgps_line2shape(line, nCols, dbase, shape, n, header);
      n++;
    }
    FCLOSE(fp);
    close_shape(dbase, shape);
  }
  else
    asfPrintError("Unknown type (%s)!\n", type);
  
  // Write polar stereographic projection file
  project_parameters_t pps;
  projection_type_t proj_type;
  datum_type_t datum;
  spheroid_type_t spheroid;
  read_proj_file("polar_stereographic_north_ssmi.proj", 
    &pps, &proj_type, &datum, &spheroid);
  pps.ps.false_easting = 0.0;
  pps.ps.false_northing = 0.0;
  meta_projection *proj = meta_projection_init();
  proj->type = proj_type;
  proj->datum = HUGHES_DATUM;
  proj->spheroid = HUGHES_SPHEROID;
  proj->param = pps;
  strcpy(proj->units, "meters");
  proj->hem = 'N';
  spheroid_axes_lengths(spheroid, &proj->re_major, &proj->re_minor);
  meta_parameters *meta = raw_init();
  meta->projection = proj;
  write_asf2esri_proj(meta, NULL, outFile);
  meta_free(meta);

  return(0);
}
