#include "asf.h"
#include "shapefil.h"
#include "asf_vector.h"

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define ASF_EXPORT_FLOAT_MICRON 0.000000001
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, ASF_EXPORT_FLOAT_MICRON))

void shape_init(char *inFile, format_type_t format)
{
  char *dbaseFile;
  DBFHandle dbase;
  SHPHandle shape;

  // Open database for initialization
  dbaseFile = (char *) MALLOC(sizeof(char)*(strlen(inFile)+5));
  sprintf(dbaseFile, "%s.dbf", inFile);
  dbase = DBFCreate(dbaseFile);
  if (!dbase)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);

  // Add fields to database
  switch (format)
    {
    case META:
      if (DBFAddField(dbase, "Sensor", FTString, 15, 0) == -1)
    asfPrintError("Could not add sensor field to database file\n");
      if (DBFAddField(dbase, "Sensor name", FTString, 15, 0) == -1)
    asfPrintError("Could not add sensor field to database file\n");
      if (DBFAddField(dbase, "Beam mode", FTString, 4, 0) == -1)
    asfPrintError("Could not add beam mode field to database file\n");
      if (DBFAddField(dbase, "Polarization", FTString, 3, 0) == -1)
    asfPrintError("Could not add polarization field to database file\n");
      if (DBFAddField(dbase, "Orbit", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add orbit field to database file\n");
      if (DBFAddField(dbase, "Frame", FTInteger, 4, 0) == -1)
    asfPrintError("Could not add frame field to database file\n");
      if (DBFAddField(dbase, "Date", FTString, 20, 0) == -1)
    asfPrintError("Could not add acquisition date field to database file\n");
      if (DBFAddField(dbase, "Direction", FTString, 15, 0) == -1)
    asfPrintError("Could not add orbit direction field to database file\n");
      if (DBFAddField(dbase, "Center Lat", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add center latitude field to database file\n");
      if (DBFAddField(dbase, "Center Lon", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add center longitude field to database file\n");
      if (DBFAddField(dbase, "Lat1", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add latitude field to database file\n");
      if (DBFAddField(dbase, "Lon1", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add longitude field to database file\n");
      if (DBFAddField(dbase, "Lat2", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add latitude field to database file\n");
      if (DBFAddField(dbase, "Lon2", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add longitude field to database file\n");
      if (DBFAddField(dbase, "Lat3", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add latitude field to database file\n");
      if (DBFAddField(dbase, "Lon3", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add longitude field to database file\n");
      if (DBFAddField(dbase, "Lat4", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add latitude field to database file\n");
      if (DBFAddField(dbase, "Lon4", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add longitude field to database file\n");
      break;
    case POINT:
      if (DBFAddField(dbase, "ID", FTString, 255, 0) == -1)
    asfPrintError("Could not add ID field to database file\n");
      if (DBFAddField(dbase, "Lat", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add latitude field to database file\n");
      if (DBFAddField(dbase, "Lon", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add longitude field to database file\n");
      break;
    case POLYGON:
      if (DBFAddField(dbase, "ID", FTString, 255, 0) == -1)
    asfPrintError("Could not add ID field to database file\n");
      if (DBFAddField(dbase, "Vertices", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add 'Vertices' field to database file\n");
      break;
    case RGPS:
      if (DBFAddField(dbase, "Cell_ID", FTInteger, 6, 0) == -1)
    asfPrintError("Could not add 'Cell_ID' field to database file\n");
      if (DBFAddField(dbase, "Vertices", FTInteger, 2, 0) == -1)
    asfPrintError("Could not add 'Vertices' field to database file\n");
      if (DBFAddField(dbase, "Date", FTString, 25, 0) == -1)
    asfPrintError("Could not add 'Date' field to database file\n");
      if (DBFAddField(dbase, "SrcImage", FTString, 25, 0) == -1)
    asfPrintError("Could not add 'SrcImage' field to database file\n");
      if (DBFAddField(dbase, "TrgImage", FTString, 25, 0) == -1)
        asfPrintError("Could not add 'TrgImage' field to database file\n");
      if (DBFAddField(dbase, "Stream", FTString, 3, 0) == -1)
    asfPrintError("Could not add 'Stream' field to database file\n");
      if (DBFAddField(dbase, "Area", FTDouble, 12, 3) == -1)
    asfPrintError("Could not add 'Area' field to database file\n");
      if (DBFAddField(dbase, "MY_ice", FTDouble, 12, 3) == -1)
    asfPrintError("Could not add 'MY_ice' field to database file\n");
      if (DBFAddField(dbase, "OpenWater", FTDouble, 12, 3) == -1)
    asfPrintError("Could not add 'OpenWater' field to database file\n");
      if (DBFAddField(dbase, "IncidAngle", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add 'IncidAngle' field to database file\n");
      if (DBFAddField(dbase, "Cell_x", FTDouble, 12, 3) == -1)
    asfPrintError("Could not add 'Cell_x' field to database file\n");
      if (DBFAddField(dbase, "Cell_y", FTDouble, 12, 3) == -1)
    asfPrintError("Could not add 'Cell_y' field to database file\n");
      if (DBFAddField(dbase, "dudx", FTDouble, 12, 6) == -1)
    asfPrintError("Could not add 'dudx' field to database file\n");
      if (DBFAddField(dbase, "dudy", FTDouble, 12, 6) == -1)
    asfPrintError("Could not add 'dudy' field to database file\n");
      if (DBFAddField(dbase, "dvdx", FTDouble, 12, 6) == -1)
    asfPrintError("Could not add 'dvdx' field to database file\n");
      if (DBFAddField(dbase, "dvdy", FTDouble, 12, 6) == -1)
    asfPrintError("Could not add 'dvdy' field to database file\n");
      if (DBFAddField(dbase, "dtp", FTDouble, 12, 6) == -1)
    asfPrintError("Could not add 'dtp' field to database file\n");
      if (DBFAddField(dbase, "Temp", FTDouble, 12, 3) == -1)
    asfPrintError("Could not add 'Temp' field to database file\n");
      if (DBFAddField(dbase, "u_wind", FTDouble, 12, 6) == -1)
        asfPrintError("Could not add 'u_wind' field to database file\n");
      if (DBFAddField(dbase, "v_wind", FTDouble, 12, 6) == -1)
        asfPrintError("Could not add 'v_wind' field to database file\n");
      break;
    case RGPS_GRID:
      if (DBFAddField(dbase, "Grid_ID", FTInteger, 6, 0) == -1)
        asfPrintError("Could not add 'Grid_ID' field to database file\n");
      if (DBFAddField(dbase, "Date", FTString, 12, 0) == -1)
        asfPrintError("Could not add 'Date' field to database file\n");
      if (DBFAddField(dbase, "Day", FTDouble, 9, 4) == -1)
        asfPrintError("Could not add 'Grid_x' field to database file\n");
      if (DBFAddField(dbase, "Grid_x", FTDouble, 12, 3) == -1)
        asfPrintError("Could not add 'Grid_x' field to database file\n");
      if (DBFAddField(dbase, "Grid_y", FTDouble, 12, 3) == -1)
        asfPrintError("Could not add 'Grid_y' field to database file\n");
      if (DBFAddField(dbase, "SrcImage", FTString, 25, 0) == -1)
        asfPrintError("Could not add 'SrcImage' field to database file\n");
      if (DBFAddField(dbase, "TrgImage", FTString, 25, 0) == -1)
        asfPrintError("Could not add 'TrgImage' field to database file\n");
      if (DBFAddField(dbase, "Stream", FTString, 3, 0) == -1)
        asfPrintError("Could not add 'Stream' field to database file\n");
      if (DBFAddField(dbase, "Quality", FTInteger, 3, 0) == -1)
        asfPrintError("Could not add 'Quality' field to database file\n");
      break;
    case RGPS_WEATHER:
      if (DBFAddField(dbase, "Date", FTString, 12, 0) == -1)
    asfPrintError("Could not add 'Date' field to database file\n");
      if (DBFAddField(dbase, "Lat", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add 'Lat' field to database file\n");
      if (DBFAddField(dbase, "Lon", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add 'Lon' field to database file\n");
      if (DBFAddField(dbase, "Direction", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add 'Direction' field to database file\n");
      if (DBFAddField(dbase, "Speed", FTDouble, 5, 1) == -1)
    asfPrintError("Could not add 'Speed' field to database file\n");
      if (DBFAddField(dbase, "Temp", FTDouble, 5, 1) == -1)
    asfPrintError("Could not add 'Temp' field to database file\n");
      if (DBFAddField(dbase, "Pressure", FTDouble, 6, 1) == -1)
    asfPrintError("Could not add 'Pressure' field to database file\n");
      break;
    case MULTIMATCH:
      if (DBFAddField(dbase, "Ref_x", FTDouble, 9, 2) == -1)
    asfPrintError("Could not add 'Ref_x' field to database file\n");
      if (DBFAddField(dbase, "Ref_y", FTDouble, 9, 2) == -1)
    asfPrintError("Could not add 'Ref_y' field to database file\n");
      if (DBFAddField(dbase, "Ref_z", FTDouble, 9, 2) == -1)
        asfPrintError("Could not add 'Ref_z' field to database file\n");
      if (DBFAddField(dbase, "Search_x", FTDouble, 9, 2) == -1)
        asfPrintError("Could not add 'Ref_x' field to database file\n");
      if (DBFAddField(dbase, "Search_y", FTDouble, 9, 2) == -1)
        asfPrintError("Could not add 'Ref_y' field to database file\n");
      if (DBFAddField(dbase, "Search_z", FTDouble, 9, 2) == -1)
        asfPrintError("Could not add 'Ref_z' field to database file\n");
      if (DBFAddField(dbase, "dx", FTDouble, 7, 2) == -1)
    asfPrintError("Could not add 'dx' field to database file\n");
      if (DBFAddField(dbase, "dy", FTDouble, 7, 2) == -1)
    asfPrintError("Could not add 'dy' field to database file\n");
      if (DBFAddField(dbase, "dh", FTDouble, 7, 3) == -1)
    asfPrintError("Could not add 'dh' field to database file\n");
      if (DBFAddField(dbase, "Direction", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add 'Direction' field to database file\n");
      if (DBFAddField(dbase, "Speed", FTDouble, 6, 1) == -1)
    asfPrintError("Could not add 'Speed' field to database file\n");
      break;
    case TEXT:
    case URSA:
    case KMLFILE:
    case SHAPEFILE:
    case GEOTIFF_META:
      break;
    }

  // Close the database for initialization
  DBFClose(dbase);

  // Open shapefile for initialization
  if (format == POINT || format == RGPS_GRID || format == RGPS_WEATHER ||
      format == MULTIMATCH)
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

void open_shape(char *inFile, DBFHandle *dbase, SHPHandle *shape)
{
  char *dbaseFile;

  // Open database for adding values
  dbaseFile = (char *) MALLOC(sizeof(char)*(strlen(inFile)+5));
  sprintf(dbaseFile, "%s.dbf", inFile);
  *dbase = DBFOpen(dbaseFile, "r+b");
  if (*dbase == NULL)
    asfPrintError("Could not open database file '%s'\n", dbaseFile);

  // Open shapefile for adding values
  *shape = SHPOpen(inFile, "r+b");
  if (*shape == NULL)
    asfPrintError("Could not open shapefile '%s\n", inFile);

  FREE(dbaseFile);

  return;
}

void close_shape(DBFHandle dbase, SHPHandle shape)
{
  // Close database
  DBFClose(dbase);

  // Close shapefile
  SHPClose(shape);

  return;
}

void convert2shape(char *filename, format_type_t format,
           DBFHandle dbase, SHPHandle shape, int n)
{
    switch (format)
    {
        case META:
            meta2shape(filename, dbase, shape, n);
            break;
        case POINT:
            point2shape(filename, dbase, shape);
            break;
        case POLYGON:
            polygon2shape(filename, dbase, shape, n);
            break;
        case GEOTIFF_META:
            geotiff2shape(filename, dbase, shape, n);
            break;
        case MULTIMATCH:
        case RGPS:
        case RGPS_GRID:
        case RGPS_WEATHER:
        case TEXT:
        case URSA:
        case KMLFILE:
        case SHAPEFILE:
            break;
    }

    return;
}

void convert_from_shape(char *inFile, format_type_t format, FILE *fp)
{
  if (format == TEXT)
    shape2text(inFile, fp);
  else if (format == KMLFILE) {
    kml_header(fp);
    shape2kml(inFile, fp, inFile);
    kml_footer(fp);
  }

  return;
}

int write_shape(char *inFile, char *outFile, format_type_t format, int list)
{
  FILE *fp;
  DBFHandle dbase;
  SHPHandle shape;
  char *line;
  int n=0;

  // Initialize the shapefile
  shape_init(outFile, format);

  // Open shapefile
  open_shape(outFile, &dbase, &shape);

  // Convert to shape
  if (list) {
    line = (char *) MALLOC(sizeof(char)*1024);
    fp = FOPEN(inFile, "r");
    while (fgets(line, 1024, fp)) {
      line[strlen(line)-1] = '\0';
      convert2shape(line, format, dbase, shape, n);
      n++;
    }
    FCLOSE(fp);
  }
  else
    convert2shape(inFile, format, dbase, shape, 0);

  if (format == META || format == POINT || format == POLYGON ||
      format == MULTIMATCH)
    write_esri_proj_file(outFile);

  // Close business
  close_shape(dbase, shape);

  return 0;
}

int read_shape(char *inFile, char *outFile, format_type_t format, int list)
{
  FILE *fpIn, *fpOut;
  char *line;

  if (format == TEXT || format == URSA)
    append_ext_if_needed(outFile, ".txt", ".txt");
  else if (format == KMLFILE)
    append_ext_if_needed(outFile, ".kml", ".kml");
  fpOut = FOPEN(outFile, "w");

  // Convert from shape
  if (list) {
    line = (char *) MALLOC(sizeof(char)*1024);
    fpIn = FOPEN(inFile, "r");
    while (fgets(line, 1024, fpIn)) {
      line[strlen(line)-1] = '\0';
      convert_from_shape(line, format, fpOut);
    }
    FCLOSE(fpIn);
  }
  else
    convert_from_shape(inFile, format, fpOut);

  FCLOSE(fpOut);

  return 0;
}

void write_esri_proj_file(char *inFile)
{
  FILE *fp;
  char esri_prj_file_name[255];

  create_name (esri_prj_file_name, inFile, ".prj");

  fp = FOPEN(esri_prj_file_name, "w");
  fprintf(fp,
      "GEOGCS[\"GCS_WGS_1984\","
      "DATUM[\"D_WGS_1984\","
      "SPHEROID[\"WGS_1984\",6378137,298.257223563]],"
      "PRIMEM[\"Greenwich\",0],"
      "UNIT[\"Degree\",0.017453292519943295]]");
  FCLOSE(fp);
}
