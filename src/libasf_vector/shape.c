#include "shapefil.h"
#include "asf_vector.h"
#include "asf.h"

// Initialize internal format such as RGPS and MULTIMATCH
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
  if (format == RGPS) {
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
  }
  else if (format == RGPS_GRID) {
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
  }
  else if (format == RGPS_WEATHER) {
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
  }
  else if (format == MULTIMATCH) {
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
  dbaseFile = appendExt(inFile, ".dbf");
  *dbase = DBFOpen(dbaseFile, "r+b");
  if (*dbase == NULL)
    asfPrintError("Could not open database file '%s'\n", dbaseFile);

  // Create a copy of the name to use for opening
  char tmpInFile[1024];
  strcpy(tmpInFile, inFile);
  char *ext = findExt(inFile);
  if (!ext) {
      // KLUDGE ALERT!  SHPOpen() below replaces the file extension in inFile
      // by searching from the end of the filename in reverse for a '.'
      // character, then appends .shx and .shp to the two filenames that it
      // produces ...Unfortunately, this results in truncated ALOS basenames
      // in the output files AND we don't own the shape library.

      // So, since SHPOpen() always wants to strip off an extension, we
      // add an extension for it to strip in cases where one isn't already
      // there.  This will solve the ALOS naming problem.
      sprintf(tmpInFile, "%s.dummy", inFile);
  }

  // Open shapefile for adding values
  *shape = SHPOpen(tmpInFile, "r+b");
  if (*shape == NULL)
    asfPrintError("Could not open shapefile '%s'\n", inFile);

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

// Convert shape to point file
int shape2point(char *inFile, char *outFile, int listFlag)
{
  FILE *fp = NULL;
  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject;
  int ii, kk, nEntities, nVertices, pointType;

  // Open shapefile
  open_shape(inFile, &dbase, &shape);

  // Extract the vital information out of the shapefile
  SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);
  switch (pointType) {
  case SHPT_POLYGON:
    asfPrintError("Shape file contains point information!\n"
                  "Use output format 'polygon'\n");
    break;
  case SHPT_POINT:
    break;
  case SHPT_ARC:
    asfPrintError("Shape file data type 'Arc' not supported\n");
    break;
  case SHPT_MULTIPOINT:
    asfPrintError("Shape file data type 'Multipoint' not supported\n");
    break;
  default:
    asfPrintError("Unexpected or unrecognized shape file data format\n");
    break;
  }

  // Write file
  fp = FOPEN(outFile, "w");
  fprintf(fp, "# Format: POINT (generated by convert2vector "
      "(version %s))\n", SVN_REV);
  fprintf(fp, "#\n");
  fprintf(fp, "# ID,Latitude,Longitude\n");

  for (ii=0; ii<nEntities; ii++) {

    // Read object for the number of vertices
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices;

    for (kk=0; kk<nVertices; kk++) {
      fprintf(fp, "%d,%.4f,%.4f\n",
              ii+1, shapeObject->padfY[kk], shapeObject->padfX[kk]);
    }

    SHPDestroyObject(shapeObject);
  }
  FCLOSE(fp);

  // Close shapefile
  close_shape(dbase, shape);

  return 1;
}

// Convert shape to polygon file
int shape2polygon(char *inFile, char *outfile, int listFlag)
{
  FILE *fp = NULL;
  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject;
  char *outFile, *basename;
  int ii, kk, nEntities, nVertices, pointType;

  // Open shapefile
  open_shape(inFile, &dbase, &shape);

  // Extract the vital information out of the shapefile
  SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);
  switch (pointType) {
  case SHPT_POLYGON:
    break;
  case SHPT_POINT:
    asfPrintError("Shape file contains point information!\n"
                  "Use output format 'point'\n");
    break;
  case SHPT_ARC:
    asfPrintError("Shape file data type 'Arc' not supported\n");
    break;
  case SHPT_MULTIPOINT:
    asfPrintError("Shape file data type 'Multipoint' not supported\n");
    break;
  default:
    asfPrintError("Unexpected or unrecognized shape file data format\n");
    break;
  }

  // Write the text file
  outFile = (char *) MALLOC(sizeof(char)*512);
  strcpy(outFile, outfile);
  for (ii=0; ii<nEntities; ii++) {
    // Open file and write header
    if (nEntities > 1) {
      char *ext = findExt(outfile);
      basename = get_basename(outfile);
      sprintf(outFile, "%s_%03d%s", basename, ii, ext);
    }

    // Read object for the number of vertices
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices;

    // Write file
    fp = FOPEN(outFile, "w");
    fprintf(fp, "# Format: POLYGON (generated by convert2vector "
            "(version %s))\n", SVN_REV);
    fprintf(fp, "#\n");
    fprintf(fp, "# ID,Latitude,Longitude\n");

    for (kk=0; kk<nVertices-1; kk++) {
      fprintf(fp, "%d,%.4f,%.4f\n",
              kk+1, shapeObject->padfY[kk], shapeObject->padfX[kk]);
    }

    FCLOSE(fp);
    SHPDestroyObject(shapeObject);
  }

  // Close shapefile
  close_shape(dbase, shape);

  return 1;
}

static void write_dbase_field_to_csv(DBFHandle dbase, int record,
                                     int field, char *line)
{
  DBFFieldType dbaseType;
  char fieldName[25], str[50];
  int nWidth, nDecimals, nValue;
  double fValue;
  const char *sValue;

  dbaseType = DBFGetFieldInfo(dbase, field, fieldName,
                              &nWidth, &nDecimals);
  switch (dbaseType)
    {
    case FTString:
      sValue = DBFReadStringAttribute(dbase, record, field);
      sprintf(str, "\"%s\",", sValue);
      break;
    case FTInteger:
      nValue = DBFReadIntegerAttribute(dbase, record, field);
      sprintf(str, "%d,", nValue);
      break;
    case FTDouble:
      fValue = DBFReadDoubleAttribute(dbase, record, field);
      sprintf(str, "%s,", lf(fValue));
      break;
    case FTLogical:
    case FTInvalid:
      break;
    }
  strcat(line, str);

}

static void write_name_field_to_csv(DBFHandle dbase, int record, char *header)
{
  DBFFieldType dbaseType;
  char fieldName[25], str[50];
  int nWidth, nDecimals;

  dbaseType = DBFGetFieldInfo(dbase, record, fieldName, &nWidth, &nDecimals);
  sprintf(str, "%s,", fieldName);
  strcat(header, str);
}

// Convert shape to generic csv file
int shape2csv(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject;
  char *line = (char *) MALLOC(sizeof(char)*4096);
  char *header = (char *) MALLOC(sizeof(char)*4096);
  int ii, kk, ll, nEntities, nVertices, nParts, *part;
  int nFields, pointType;

  // Open shapefile
  open_shape(inFile, &dbase, &shape);

  // Extract the vital information out of the shapefile
  SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);
  switch (pointType)
    {
    case SHPT_POLYGON:
    case SHPT_POINT:
      break;
    case SHPT_ARC:
      asfPrintError
        ("Conversion does not support shape type 'Arc'\n");
      break;
    case SHPT_MULTIPOINT:
      asfPrintError
        ("Conversion does not support shape type 'Multipoint'\n");
      break;
    }

  // Determine the number of fields in the database file
  nFields = DBFGetFieldCount(dbase);

  // Open csv file
  fp = FOPEN(outFile, "w");
  strcpy(header, "");
  for (ii=0; ii<nFields; ii++)
    write_name_field_to_csv(dbase, ii, header);
  header[strlen(header)-1] = '\0';
  fprintf(fp, "%s\n", header);

  for (ii=0; ii<nEntities; ii++) {

    // Read lat/lon from shape object
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices;
    nParts = shapeObject->nParts;
    part = (int *) MALLOC(sizeof(int)*(nParts+1));
    for (kk=0; kk<nParts; kk++)
      part[kk] = shapeObject->panPartStart[kk];
    part[nParts] = nVertices;
    SHPDestroyObject(shapeObject);
    if (nParts == 0)
      nParts++;

    strcpy(line, "");
    for (ll=0; ll<nParts; ll++) {
      for (kk=0; kk<nFields; kk++)
        write_dbase_field_to_csv(dbase, ii, kk, line);
    }
    line[strlen(line)-1] = '\0';
    fprintf(fp, "%s\n", line);
  }

  // Close shapefile
  close_shape(dbase, shape);

  // Clean up
  FREE(line);
  FREE(header);
  FCLOSE(fp);

  return 1;
}

static void write_dbase_field_to_kml(DBFHandle dbase, int record,
                     int field, FILE *fp)
{
  DBFFieldType dbaseType;
  char fieldName[25], str[50];
  int nWidth, nDecimals, nValue;
  double fValue;
  const char *sValue;

  dbaseType = DBFGetFieldInfo(dbase, field, fieldName,
                  &nWidth, &nDecimals);
  switch (dbaseType)
    {
    case FTString:
      sValue = DBFReadStringAttribute(dbase, record, field);
      fprintf(fp, "<strong>%s</strong>: %s <br>\n",
          fieldName, sValue);
      break;
    case FTInteger:
      nValue = DBFReadIntegerAttribute(dbase, record, field);
      fprintf(fp, "<strong>%s</strong>: %d <br>\n",
          fieldName, nValue);
      break;
    case FTDouble:
      fValue = DBFReadDoubleAttribute(dbase, record, field);
      sprintf(str, "<strong>%%s</strong>: %%%d.%dlf <br>\n",
          nWidth, nDecimals);
      fprintf(fp, str, fieldName, fValue);
      break;
    case FTLogical:
    case FTInvalid:
      break;
    }
}

static void write_name_field_to_kml(DBFHandle dbase, int record, FILE *fp)
{
  DBFFieldType dbaseType;
  char fieldName[25], str[50];
  int nWidth, nDecimals, nValue;
  double fValue;
  const char *sValue;

  dbaseType = DBFGetFieldInfo(dbase, 0, fieldName, &nWidth, &nDecimals);
  switch (dbaseType)
    {
    case FTString:
      sValue = DBFReadStringAttribute(dbase, record, 0);
      fprintf(fp, "<name>%s</name>\n", sValue);
      break;
    case FTInteger:
      nValue = DBFReadIntegerAttribute(dbase, record, 0);
      fprintf(fp, "<name>%d</name>\n", nValue);
      break;
    case FTDouble:
      fValue = DBFReadDoubleAttribute(dbase, record, 0);
      sprintf(str, "<name>%%%d.%dlf</name>\n", nWidth, nDecimals);
      fprintf(fp, str, fValue);
      break;
    case FTLogical:
    case FTInvalid:
      break;
    }
}

// Convert shape to kml file
int shape2kml(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject;
  int ii, kk, ll, nEntities, nVertices, nParts, *part;
  int nFields, pointType;
  double *lat, *lon, *min, *max, clat, clon;

  // Open shapefile
  open_shape(inFile, &dbase, &shape);

  // Open kmlfile
  fp = FOPEN(outFile, "w");
  kml_header(fp);

  // Extract the vital information out of the shapefile
  min = (double *) MALLOC(sizeof(double)*4);
  max = (double *) MALLOC(sizeof(double)*4);
  SHPGetInfo(shape, &nEntities, &pointType, min, max);
  switch (pointType)
    {
    case SHPT_POLYGON:
    case SHPT_POINT:
      break;
    case SHPT_ARC:
      asfPrintError
    ("Conversion does not support shape type 'Arc'\n");
      break;
    case SHPT_MULTIPOINT:
      asfPrintError
    ("Conversion does not support shape type 'Multipoint'\n");
      break;
    }
  // Ball park center for <LookAt> position  - does not have to be accurate
  clat = min[1] + (max[1]-min[1])/2;
  clon = min[0] + (max[0]-min[0])/2;

  for (ii=0; ii<nEntities; ii++) {

    // Read lat/lon from shape object
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices;
    lat = (double *) MALLOC(sizeof(double)*(nVertices+1));
    lon = (double *) MALLOC(sizeof(double)*(nVertices+1));
    for (kk=0; kk<nVertices; kk++) {
      lat[kk] = shapeObject->padfY[kk];
      lon[kk] = shapeObject->padfX[kk];
    }
    lat[nVertices] = lat[0];
    lon[nVertices] = lon[0];
    nParts = shapeObject->nParts;
    part = (int *) MALLOC(sizeof(int)*(nParts+1));
    for (kk=0; kk<nParts; kk++)
      part[kk] = shapeObject->panPartStart[kk];
    part[nParts] = nVertices;
    SHPDestroyObject(shapeObject);
    if (nParts == 0)
      nParts++;

    // Extract the attributes out of the database file
    nFields = DBFGetFieldCount(dbase);

    for (ll=0; ll<nParts; ll++) {

      // Write information in kml file
      fprintf(fp, "<Placemark>\n");
      fprintf(fp, "<description><![CDATA[\n");
      fprintf(fp, "<!-- Format: SHAPE (generated by convert2vector "
          "(version %s)) -->\n", SVN_REV);
      for (kk=1; kk<nFields; kk++)
    write_dbase_field_to_kml(dbase, ii, kk, fp);
      fprintf(fp, "]]></description>\n");
      write_name_field_to_kml(dbase, ii, fp);
      fprintf(fp, "<LookAt>\n");
      fprintf(fp, "<longitude>%9.4f</longitude>\n", clon);
      fprintf(fp, "<latitude>%9.4f</latitude>\n", clat);
      fprintf(fp, "<range>400000</range>\n");
      fprintf(fp, "</LookAt>\n");
      if (pointType == SHPT_POLYGON) {
    write_kml_style_keys(fp);
    fprintf(fp, "<Polygon>\n");
    fprintf(fp, "<outerBoundaryIs>\n");
    fprintf(fp, "<LinearRing>\n");
    fprintf(fp, "<coordinates>\n");
    for (kk=part[ll]; kk<part[ll+1]; kk++)
      fprintf(fp, "%.12f,%.12f,4000\n", lon[kk], lat[kk]);
    fprintf(fp, "</coordinates>\n");
    fprintf(fp, "</LinearRing>\n");
    fprintf(fp, "</outerBoundaryIs>\n");
    fprintf(fp, "</Polygon>\n");
      }
      else if (pointType == SHPT_POINT) {
    fprintf(fp, "<Point>\n");
    fprintf(fp, "<coordinates>%.12lf,%.12lf,4000</coordinates>",
        lon[0], lat[0]);
    fprintf(fp, "</Point>\n");
      }
      fprintf(fp, "</Placemark>\n");
    }
  }

  // Close shapefile
  close_shape(dbase, shape);

  // Clean up
  kml_footer(fp);
  FCLOSE(fp);

  return 1;
}
