#include "asf_baseline.h"

// Convert baselines to kml
void baseline2kml(struct base_pair *pairs, int nPairs, FILE *fp)
{
  int ii, kk, vertices=4;
  double *lat, *lon;
  julian_date jd;
  ymd_date ymd;
  char *mon[13]={"", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

  for (ii=0; ii<nPairs; ii++) {

    // Read coordinates of the vertices
    lat = (double *) MALLOC(sizeof(double)*(vertices+1));
    lon = (double *) MALLOC(sizeof(double)*(vertices+1));
    lat[0] = pairs[ii].ns_lat;
    lon[0] = pairs[ii].ns_lon;
    lat[1] = pairs[ii].fs_lat;
    lon[1] = pairs[ii].fs_lon;
    lat[2] = pairs[ii].fe_lat;
    lon[2] = pairs[ii].fe_lon;
    lat[3] = pairs[ii].ne_lat;
    lon[3] = pairs[ii].ne_lon;
    lat[vertices] = lat[0];
    lon[vertices] = lon[0];
    
    // Write information in kml file
    fprintf(fp, "<Placemark>\n");
    fprintf(fp, "  <description><![CDATA[\n");
    fprintf(fp, "<strong>Sensor</strong>: %s<br>\n", pairs[ii].sensor);
    fprintf(fp, "<strong>Mode</strong>: %s<br>\n", pairs[ii].mode);
    fprintf(fp, "<strong>Frame</strong>: %d<br>\n", pairs[ii].frame);
    fprintf(fp, "<strong>Orbit direction</strong>: %s<br>\n", 
	    pairs[ii].orbit_dir);
    fprintf(fp, "<strong>Master</strong>: %d<br>\n", pairs[ii].master);
    fprintf(fp, "<strong>Master sequence</strong>: %d<br>\n", pairs[ii].m_seq);
    sscanf(pairs[ii].m_time, "%4d-%3dT", &jd.year, &jd.jd);
    date_jd2ymd(&jd, &ymd);
    fprintf(fp, "<strong>Master acquisition</strong>: %d-%s-%d<br>\n",
	    ymd.day, mon[ymd.month], ymd.year);
    fprintf(fp, "<strong>Slave</strong>: %d<br>\n", pairs[ii].slave);
    fprintf(fp, "<strong>Slave sequence</strong>: %d<br>\n", pairs[ii].s_seq);
    sscanf(pairs[ii].s_time, "%4d-%3dT", &jd.year, &jd.jd);
    date_jd2ymd(&jd, &ymd);
    fprintf(fp, "<strong>Slave acquisition</strong>: %d-%s-%d<br>\n", 
	    ymd.day, mon[ymd.month], ymd.year);
    fprintf(fp, "<strong>Parallel baseline</strong>: %d<br>\n", 
	    pairs[ii].b_par);
    fprintf(fp, "<strong>Perpendicular baseline</strong>: %d<br>\n", 
	    pairs[ii].b_perp);
    fprintf(fp, "<strong>Temporal baseline</strong>: %d<br>\n", 
	    pairs[ii].b_temp);
    for (kk=0; kk<vertices; kk++) {
      fprintf(fp, "<strong>%d</strong> - ", kk+1);
      fprintf(fp, "<strong>Lat</strong>: %9.4f, ", lat[kk]);
      fprintf(fp, "<strong>Lon</strong>: %9.4f<br>\n", lon[kk]);
    }
    fprintf(fp, "]]></description>\n");
    fprintf(fp, "<name>%s %s %d baselines</name>\n", 
	    pairs[ii].sensor, pairs[ii].mode, pairs[ii].master);
    fprintf(fp, "<LookAt>\n");
    fprintf(fp, "<longitude>%.4f</longitude>\n", pairs[ii].c_lon);
    fprintf(fp, "<latitude>%.4f</latitude>\n", pairs[ii].c_lat);
    fprintf(fp, "<range>400000</range>\n");
    fprintf(fp, "<heading>0</heading>\n");
    fprintf(fp, "</LookAt>\n");
    fprintf(fp, "<Style>\n");
    fprintf(fp, "<LineStyle><color>00000ff</color>"
	    "<width>3</width></LineStyle>\n");
    fprintf(fp, "</Style>\n");
    fprintf(fp, "<Polygon>\n");
    fprintf(fp, "<outerBoundaryIs>\n");
    fprintf(fp, "<LinearRing>\n");
    fprintf(fp, "<coordinates>\n");
    for (kk=0; kk<=vertices; kk++)
      fprintf(fp, "%.12f,%.12f,4000\n", lon[kk], lat[kk]);
    fprintf(fp, "</coordinates>\n");
    fprintf(fp, "</LinearRing>\n");
    fprintf(fp, "</outerBoundaryIs>\n");
    fprintf(fp, "</Polygon>\n");
    fprintf(fp, "</Placemark>\n");
  }

  return;
}

void init_baseline_shape(char *inFile)
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
  if (DBFAddField(dbase, "Sensor", FTString, 15, 0) == -1)
    asfPrintError("Could not add sensor field to database file\n");
  if (DBFAddField(dbase, "Beam mode", FTString, 5, 0) == -1)
    asfPrintError("Could not add beam mode to database file\n");
  if (DBFAddField(dbase, "Frame", FTInteger, 4, 0) == -1)
    asfPrintError("Could not add frame field to database file\n");
  if (DBFAddField(dbase, "Direction", FTString, 15, 0) == -1)
    asfPrintError("Could not add orbit direction field to database file\n");
  if (DBFAddField(dbase, "Master", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add master field to database file\n");
  if (DBFAddField(dbase, "Master sequence", FTInteger, 10, 0) == -1)
    asfPrintError("Could not add master sequence field to database file\n");
  if (DBFAddField(dbase, "Master date", FTString, 20, 0) == -1)
    asfPrintError("Could not add master acquisition date field"
		  " to database file\n");
  if (DBFAddField(dbase, "Slave", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add orbit slave field to database file\n");
  if (DBFAddField(dbase, "Slave sequence", FTInteger, 10, 0) == -1)
    asfPrintError("Could not add master sequence field to database file\n");
  if (DBFAddField(dbase, "Slave date", FTString, 20, 0) == -1)
    asfPrintError("Could not add slave acquisition date field"
		  " to database file\n");
  if (DBFAddField(dbase, "Parallel baseline", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add parallel baseline field to database file\n");
  if (DBFAddField(dbase, "Perpendicular baseline", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add perpendicular baseline field"
		  " to database file\n");
  if (DBFAddField(dbase, "Temporal baseline", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add temporal baseline field to database file\n");
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

  // Close the database for initialization
  DBFClose(dbase);

  // Open shapefile for initialization
  shape = SHPCreate(inFile, SHPT_POLYGON);
  if (!shape)
    asfPrintError("Could not create shapefile '%s'\n", inFile);

  // Close shapefile for initialization
  SHPClose(shape);

  FREE(dbaseFile);

  return;
}

void open_baseline_shape(char *inFile, DBFHandle *dbase, SHPHandle *shape)
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

// Convert baseline to shape file
void baseline2shape(struct base_pair *pairs, int nPairs, char *shapeFile)
{
  DBFHandle dbase;
  SHPHandle shape;
  int ii, vertices=4;
  char date[15];
  double *lat, *lon;
  julian_date jd;
  ymd_date ymd;
  char *mon[13]={"", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

  // Initialize shape files
  init_baseline_shape(shapeFile);

  // Open shapefile
  open_baseline_shape(shapeFile, &dbase, &shape);

  for (ii=0; ii<nPairs; ii++) {

    // Read coordinates of the vertices
    lat = (double *) MALLOC(sizeof(double)*(vertices+1));
    lon = (double *) MALLOC(sizeof(double)*(vertices+1));
    lat[0] = pairs[ii].ns_lat;
    lon[0] = pairs[ii].ns_lon;
    lat[1] = pairs[ii].fs_lat;
    lon[1] = pairs[ii].fs_lon;
    lat[2] = pairs[ii].fe_lat;
    lon[2] = pairs[ii].fe_lon;
    lat[3] = pairs[ii].ne_lat;
    lon[3] = pairs[ii].ne_lon;
    lat[vertices] = lat[0];
    lon[vertices] = lon[0];
    
    // Write information into database file
    DBFWriteStringAttribute(dbase, ii, 0, pairs[ii].sensor);
    DBFWriteStringAttribute(dbase, ii, 1, pairs[ii].mode);
    DBFWriteIntegerAttribute(dbase, ii, 2, pairs[ii].frame);
    DBFWriteStringAttribute(dbase, ii, 3, pairs[ii].orbit_dir);
    DBFWriteIntegerAttribute(dbase, ii, 4, pairs[ii].master);
    DBFWriteIntegerAttribute(dbase, ii, 5, pairs[ii].m_seq);
    sscanf(pairs[ii].m_time, "%4d-%3dT", &jd.year, &jd.jd);
    date_jd2ymd(&jd, &ymd);
    sprintf(date, "%d-%s-%d", ymd.day, mon[ymd.month], ymd.year);
    DBFWriteStringAttribute(dbase, ii, 6, date);
    DBFWriteIntegerAttribute(dbase, ii, 7, pairs[ii].slave);
    DBFWriteIntegerAttribute(dbase, ii, 8, pairs[ii].s_seq);
    sscanf(pairs[ii].m_time, "%4d-%3dT", &jd.year, &jd.jd);
    date_jd2ymd(&jd, &ymd);
    sprintf(date, "%d-%s-%d", ymd.day, mon[ymd.month], ymd.year);
    DBFWriteStringAttribute(dbase, ii, 9, date);
    DBFWriteIntegerAttribute(dbase, ii, 10, pairs[ii].b_par);
    DBFWriteIntegerAttribute(dbase, ii, 11, pairs[ii].b_perp);
    DBFWriteIntegerAttribute(dbase, ii, 12, pairs[ii].b_temp);
    DBFWriteDoubleAttribute(dbase, ii, 13, pairs[ii].c_lat);
    DBFWriteDoubleAttribute(dbase, ii, 14, pairs[ii].c_lon);
    DBFWriteDoubleAttribute(dbase, ii, 15, pairs[ii].ns_lat);
    DBFWriteDoubleAttribute(dbase, ii, 16, pairs[ii].ns_lon);
    DBFWriteDoubleAttribute(dbase, ii, 17, pairs[ii].fs_lat);
    DBFWriteDoubleAttribute(dbase, ii, 18, pairs[ii].fs_lon);
    DBFWriteDoubleAttribute(dbase, ii, 19, pairs[ii].ne_lat);
    DBFWriteDoubleAttribute(dbase, ii, 20, pairs[ii].ne_lon);
    DBFWriteDoubleAttribute(dbase, ii, 21, pairs[ii].fe_lat);
    DBFWriteDoubleAttribute(dbase, ii, 22, pairs[ii].fe_lon);
    
    // Write shape object
    SHPObject *shapeObject=NULL;
    shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, vertices+1,
					lon, lat, NULL);
    if (shapeObject == NULL)
      asfPrintError("Could not create shape object (%d)\n", ii);
    SHPWriteObject(shape, -1, shapeObject);
    SHPDestroyObject(shapeObject);
    
    FREE(lat);
    FREE(lon);
  }

  // Close database
  DBFClose(dbase);

  // Close shapefile
  SHPClose(shape);
}
