#include "shapefil.h"
#include "asf_vector.h"
#include "asf.h"
#include <assert.h>
#include <ctype.h>

// Initialize shape file
static void shape_custom_init(char *inFile, const char *format, char *header,
                  int vertices)
{
  char *dbaseFile;
  dbf_header_t *dbf;
  DBFHandle dbase;
  SHPHandle shape;
  int ii, kk, nCols;

  // Read configuration file
  read_header_config(format, &dbf, &nCols);

  // Open database for initialization
  dbaseFile = (char *) MALLOC(sizeof(char)*(strlen(inFile)+5));
  sprintf(dbaseFile, "%s.dbf", inFile);
  dbase = DBFCreate(dbaseFile);
  if (!dbase)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);

  // Add fields to database
  int nColumns = get_number_columns(header);
  char *column = (char *) MALLOC(sizeof(char)*255);
  for (kk=0; kk<nColumns; kk++) {
    column = get_column(header, kk);
    for (ii=0; ii<nCols; ii++) {
      if (strncmp_case(column, dbf[ii].header, strlen(dbf[ii].header)-1) == 0
      && dbf[ii].visible) {
    if (dbf[ii].format == DBF_STRING) {
      if (DBFAddField(dbase, dbf[ii].header, FTString, 255, 0) == -1)
        asfPrintError("Could not add %s field to database file\n",
              dbf[ii].header);
    }
    else if (dbf[ii].format == DBF_INTEGER) {
      if (DBFAddField(dbase, dbf[ii].header, FTInteger, 10, 0) == -1)
        asfPrintError("Could not add %s field to database file\n",
              dbf[ii].header);
    }
    else if (dbf[ii].format == DBF_DOUBLE) {
      if (DBFAddField(dbase, dbf[ii].header, FTDouble, 20, 10) == -1)
        asfPrintError("Could not add %s field to database file\n",
              dbf[ii].header);
    }
      }
    }
  }

  // Close the database for initialization
  DBFClose(dbase);

  // Open shapefile for initialization
  if (vertices == 1)
    shape = SHPCreate(inFile, SHPT_POINT);
  else
    shape = SHPCreate(inFile, SHPT_POLYGON);
  if (!shape)
    asfPrintError("Could not create shapefile '%s'\n", inFile);

  // Close shapefile for initialization
  SHPClose(shape);

  FREE(dbaseFile);

  return;
}

// Convert custom to shapefile
int custom2shape(char *inFile, const char *format, char *outFile, int listFlag)
{
  DBFHandle dbase;
  SHPHandle shape;
  char *line = (char *) MALLOC(sizeof(char)*4096);
  char *header = (char *) MALLOC(sizeof(char)*4096);
  dbf_header_t *dbf;
  int ii, kk, nVertices, nCols;
  char *p, test_format[25];

  // Read configuration file
  if (!read_header_config(format, &dbf, &nCols))
    asfPrintError("Don't currently know anything about the requested format "
          "(%s).\nHowever it can be added to 'header.lst' file in "
          "the share directory\n(%s)\n", uc(format),
          get_asf_share_dir());

  // Read input file and check the format
  // The first line needs to contain a format. Otherwise we suggest the use
  // of the generic csv format (with less control over the data formats).
  // The second line contains the column header information.
  FILE *fp = FOPEN(inFile, "r");
  fgets(line, 4096, fp);
  if (strstr(line, "# Format:")) {
    p = strchr(line, ':');
    if (p) {
      p++;
      while (isspace(*p))
    p++;
      sscanf(p, "%s", test_format);
      if (strcmp_case(test_format, format) != 0)
    asfPrintError("Format in the file (%s) does not match reqested "
              "format (%s),\n", test_format, format);
    }
  }
  else
    asfPrintError("The file (%s) does not contain any format information.\n"
          "In its current form it can only be ingested as generic "
          "csv file.\n", inFile);

  // Read the header information and determine how many vertices we have
  fgets(header, 4096, fp);
  int nColumns = get_number_columns(header);;
  int nLat=0, nLon=0;
  char *param = (char *) MALLOC(sizeof(char)*255);
  for (kk=0; kk<nColumns; kk++) {
    param = get_column(header, kk);
    if (strncmp_case(param, "LAT", 3) == 0)
      nLat++;
    else if (strncmp_case(param, "LON", 3) == 0)
      nLon++;
  }
  if (nLat != nLon)
    asfPrintError("Number of lat (%d) and lon (%d) columns not matching.\n",
          nLat, nLon);
  nVertices = nLat;
  double *lat = (double *) MALLOC(sizeof(double)*nVertices+1);
  double *lon = (double *) MALLOC(sizeof(double)*nVertices+1);

  // Now we have established that the format information seems to be in order
  // and we have the column header information.
  // Initialize the shape file
  shape_custom_init(outFile, format, header, nVertices);

  // Open shape file for some action
  open_shape(outFile, &dbase, &shape);

  // Read values and write them to shape file
  char *sValue = (char *) MALLOC(sizeof(char)*255);
  int nValue, n=0;
  double fValue;

  while (fgets(line, 4096, fp)) {
    nLat = nLon = 0;
    int field = 0;
    for (kk=0; kk<nColumns; kk++) {
      sValue = get_column(line, kk);
      param = get_column(header, kk);
      for (ii=0; ii<nCols; ii++) {
    if (strncmp_case(dbf[ii].header, param, strlen(dbf[ii].header)-1) == 0
             && dbf[ii].visible) {

      // Check for geolocation columns
      if (strncmp_case(dbf[ii].header, "LAT", 3) == 0) {
        sscanf(sValue, "%lf", &lat[nLat]);
        nLat++;
      }
      else if (strncmp_case(dbf[ii].header, "LON", 3) == 0) {
        sscanf(sValue, "%lf", &lon[nLon]);
        nLon++;
      }

      // Write values into the database
      if (dbf[ii].format == DBF_STRING)
        DBFWriteStringAttribute(dbase, n, field, sValue);
      else if (dbf[ii].format == DBF_INTEGER) {
        sscanf(sValue, "%i", &nValue);
        DBFWriteIntegerAttribute(dbase, n, field, nValue);
      }
      else if (dbf[ii].format == DBF_DOUBLE) {
        sscanf(sValue, "%lf", &fValue);
        DBFWriteDoubleAttribute(dbase, n, field, fValue);
      }
      field++;
    }
      }
    }
    lat[nVertices] = lat[0];
    lon[nVertices] = lon[0];

    // Write shape object
    SHPObject *shapeObject=NULL;
    if (nVertices == 1)
      shapeObject =
    SHPCreateSimpleObject(SHPT_POINT, 1, &lon[0], &lat[0], NULL);
    else
      shapeObject =
    SHPCreateSimpleObject(SHPT_POLYGON, nVertices+1, lon, lat, NULL);
    SHPWriteObject(shape, -1, shapeObject);
    SHPDestroyObject(shapeObject);
    n++;
  }
  FCLOSE(fp);

  // Close shapefile
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  // Clean up
  FREE(header);
  FREE(line);
  FREE(param);
  FREE(sValue);

  return 1;
}

// Convert custom to KML file
int custom2kml(char *inFile, const char *format, char *outFile, int listFlag)
{
  char *line = (char *) MALLOC(sizeof(char)*4096);
  char *header = (char *) MALLOC(sizeof(char)*4096);
  dbf_header_t *dbf;
  int ii, kk, nVertices, nCols;
  char *p, test_format[25];

  // Read configuration file
  if (!read_header_config(format, &dbf, &nCols))
    asfPrintError("Don't currently know anything about the requested format "
          "(%s).\nHowever it can be added to 'header.lst' file in "
          "the share directory\n(%s)\n", uc(format),
          get_asf_share_dir());

  // Read input file and check the format
  // The first line needs to contain a format. Otherwise we suggest the use
  // of the generic csv format (with less control over the data formats).
  // The second line contains the column header information.
  FILE *fp = FOPEN(inFile, "r");
  fgets(line, 4096, fp);
  if (strstr(line, "# Format:")) {
    p = strchr(line, ':');
    if (p) {
      p++;
      while (isspace(*p))
    p++;
      sscanf(p, "%s", test_format);
      if (strcmp_case(test_format, format) != 0)
    asfPrintError("Format in the file (%s) does not match reqested "
              "format (%s),\n", test_format, format);
    }
  }
  else
    asfPrintError("The file (%s) does not contain any format information.\n"
          "In its current form it can only be ingested as generic "
          "csv file.\n", inFile);

  // Read the header information and determine how many vertices we have
  fgets(header, 4096, fp);
  int nColumns = get_number_columns(header);;
  int nLat=0, nLon=0;
  char *param = (char *) MALLOC(sizeof(char)*255);
  for (kk=0; kk<nColumns; kk++) {
    param = get_column(header, kk);
    if (strncmp_case(param, "LAT", 3) == 0)
      nLat++;
    else if (strncmp_case(param, "LON", 3) == 0)
      nLon++;
  }
  if (nLat != nLon)
    asfPrintError("Number of lat (%d) and lon (%d) columns not matching.\n",
          nLat, nLon);
  nVertices = nLat;
  double *lat = (double *) MALLOC(sizeof(double)*nVertices+1);
  double *lon = (double *) MALLOC(sizeof(double)*nVertices+1);

  // Now we have established that the format information seems to be in order
  // and we have the column header information.
  // Initialize the KML file
  //kml_custom_init(outFile, format, header, nVertices);

  // Read values and write them to shape file
  char *sValue = (char *) MALLOC(sizeof(char)*255);
  //int nValue;
  int n=0;
  //double fValue;

  while (fgets(line, 4096, fp)) {
    nLat = nLon = 0;
    int field = 0;
    for (kk=0; kk<nColumns; kk++) {
      sValue = get_column(line, kk);
      param = get_column(header, kk);
      for (ii=0; ii<nCols; ii++) {
    if (strncmp_case(dbf[ii].header, param, strlen(dbf[ii].header)-1) == 0
             && dbf[ii].visible) {

      // Check for geolocation columns
      if (strncmp_case(dbf[ii].header, "LAT", 3) == 0) {
        sscanf(sValue, "%lf", &lat[nLat]);
        nLat++;
      }
      else if (strncmp_case(dbf[ii].header, "LON", 3) == 0) {
        sscanf(sValue, "%lf", &lon[nLon]);
        nLon++;
      }

      field++;
    }
      }
    }
    lat[nVertices] = lat[0];
    lon[nVertices] = lon[0];

    n++;
  }
  FCLOSE(fp);

  // Close KML file

  // Clean up
  FREE(header);
  FREE(line);
  FREE(param);
  FREE(sValue);

  return 1;
}
