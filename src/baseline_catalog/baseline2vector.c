#include "asf_baseline.h"

// Convert baselines to kml
void baseline2kml(int ii, struct base_pair *pairs, FILE *fp)
{
  int kk, vertices=4;
  double *lat, *lon;
  julian_date jd;
  ymd_date ymd;
  char *mon[13]={"", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

  lat = (double *) MALLOC(sizeof(double)*(vertices+1));
  lon = (double *) MALLOC(sizeof(double)*(vertices+1));
  lat[0] = pairs->ns_lat;
  lon[0] = pairs->ns_lon;
  lat[1] = pairs->fs_lat;
  lon[1] = pairs->fs_lon;
  lat[2] = pairs->fe_lat;
  lon[2] = pairs->fe_lon;
  lat[3] = pairs->ne_lat;
  lon[3] = pairs->ne_lon;
  lat[vertices] = lat[0];
  lon[vertices] = lon[0];
  
  // Write information in kml file
  fprintf(fp, "<Placemark>\n");
  fprintf(fp, "<description><![CDATA[<table><tr><td width=400>\n");
  if (strcmp(pairs->m_sensor, pairs->s_sensor) == 0)
    fprintf(fp, "<strong>Sensor</strong>: %s<br>\n", pairs->m_sensor);
  else {
    fprintf(fp, "<strong>Master sensor</strong>: %s<br>\n", pairs->m_sensor);
    fprintf(fp, "<strong>Slave sensor</strong>: %s<br>\n", pairs->s_sensor);
  }
  fprintf(fp, "<strong>Mode</strong>: %s<br>\n", pairs->mode);
  fprintf(fp, "<strong>Frame</strong>: %d<br>\n", pairs->frame);
  if (strcmp_case(pairs->m_sensor, "PSR") == 0)
    fprintf(fp, "<strong>Off-nadir angle</strong>: %.4f\n", pairs->off_nadir);
  fprintf(fp, "<strong>Orbit direction</strong>: %s<br>\n", 
	  pairs->orbit_dir);
  fprintf(fp, "<strong>Master</strong>: %d<br>\n", pairs->master);
  sscanf(pairs->m_time, "%4d-%3dT", &jd.year, &jd.jd);
  date_jd2ymd(&jd, &ymd);
  fprintf(fp, "<strong>Master acquisition</strong>: %d-%s-%d<br>\n",
	  ymd.day, mon[ymd.month], ymd.year);
  fprintf(fp, "<strong>Slave</strong>: %d<br>\n", pairs->slave);
  sscanf(pairs->s_time, "%4d-%3dT", &jd.year, &jd.jd);
  date_jd2ymd(&jd, &ymd);
  fprintf(fp, "<strong>Slave acquisition</strong>: %d-%s-%d<br>\n", 
	  ymd.day, mon[ymd.month], ymd.year);
  fprintf(fp, "<strong>Parallel baseline</strong>: %i<br>\n", 
	  pairs->b_par);
  fprintf(fp, "<strong>Perpendicular baseline</strong>: %i<br>\n", 
	  pairs->b_perp);
  fprintf(fp, "<strong>Temporal baseline</strong>: %i<br>\n", 
	  pairs->b_temp);
  for (kk=0; kk<vertices; kk++) {
    fprintf(fp, "<strong>%d</strong> - ", kk+1);
    fprintf(fp, "<strong>Lat</strong>: %9.4f, ", lat[kk]);
    fprintf(fp, "<strong>Lon</strong>: %9.4f<br>\n", lon[kk]);
  }
  fprintf(fp, "</td></tr></table>]]></description>\n");
  fprintf(fp, "<name>Master orbit: %d - Slave orbit: %d - "
	  "Frame: %d</name>\n", 
	  pairs->master, pairs->slave, pairs->frame);
  fprintf(fp, "<LookAt>\n");
  fprintf(fp, "<longitude>%.4f</longitude>\n", pairs->c_lon);
  fprintf(fp, "<latitude>%.4f</latitude>\n", pairs->c_lat);
  fprintf(fp, "<range>500000</range>\n");
  fprintf(fp, "<heading>0</heading>\n");
  fprintf(fp, "</LookAt>\n");
  write_kml_style_keys(fp);
  fprintf(fp, "<Polygon>\n");
  fprintf(fp, "<outerBoundaryIs>\n");
  fprintf(fp, "<LinearRing>\n");
  fprintf(fp, "<extrude>1</extrude>\n");
  fprintf(fp, "<tessellate>1</tessellate>\n");
  fprintf(fp, "<altitudeMode>absolute</altitudeMode>\n");
  fprintf(fp, "<coordinates>\n");
  for (kk=0; kk<=vertices; kk++)
    fprintf(fp, "%.12f,%.12f,20000\n", lon[kk], lat[kk]);
  fprintf(fp, "</coordinates>\n");
  fprintf(fp, "</LinearRing>\n");
  fprintf(fp, "</outerBoundaryIs>\n");
  fprintf(fp, "</Polygon>\n");
  fprintf(fp, "</Placemark>\n");
  fflush(fp);
  
  return;
}

void init_baseline_shape(char *inFile, int sensor_count)
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
  if (sensor_count == 1) {
    if (DBFAddField(dbase, "Sensor", FTString, 15, 0) == -1)
      asfPrintError("Could not add sensor field to database file\n");
  }
  else {
    if (DBFAddField(dbase, "Master_sensor", FTString, 15, 0) == -1)
      asfPrintError("Could not add sensor field to database file\n");
    if (DBFAddField(dbase, "Slave_sensor", FTString, 15, 0) == -1)
      asfPrintError("Could not add sensor field to database file\n");
  }
  if (DBFAddField(dbase, "Beam_mode", FTString, 5, 0) == -1)
    asfPrintError("Could not add beam mode to database file\n");
  if (DBFAddField(dbase, "Frame", FTInteger, 4, 0) == -1)
    asfPrintError("Could not add frame field to database file\n");
  if (DBFAddField(dbase, "Direction", FTString, 15, 0) == -1)
    asfPrintError("Could not add orbit direction field to database file\n");
  if (DBFAddField(dbase, "Master", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add master field to database file\n");
  if (DBFAddField(dbase, "Master_date", FTString, 20, 0) == -1)
    asfPrintError("Could not add master acquisition date field"
		  " to database file\n");
  if (DBFAddField(dbase, "Slave", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add slave field to database file\n");
  if (DBFAddField(dbase, "Slave_date", FTString, 20, 0) == -1)
    asfPrintError("Could not add slave acquisition date field"
		  " to database file\n");
  if (DBFAddField(dbase, "B_par", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add parallel baseline field to database file\n");
  if (DBFAddField(dbase, "B_perp", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add perpendicular baseline field"
		  " to database file\n");
  if (DBFAddField(dbase, "B_temp", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add temporal baseline field to database file\n");
  if (DBFAddField(dbase, "Center_Lat", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add center latitude field to database file\n");
  if (DBFAddField(dbase, "Center_Lon", FTDouble, 9, 4) == -1)
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

void close_baseline_shape(DBFHandle dbase, SHPHandle shape) 
{
  // Close database
  DBFClose(dbase);
  dbase = NULL;

  // Close shapefile
  SHPClose(shape);
  shape = NULL;
}

// Convert baseline to shape file
void baseline2shape(int ii, struct base_pair *pairs, 
		    DBFHandle dbase, SHPHandle shape)
{
  int vertices=4, off;
  char date[15];
  double *lat, *lon;
  julian_date jd;
  ymd_date ymd;
  char *mon[13]={"", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

  // Read coordinates of the vertices
  lat = (double *) MALLOC(sizeof(double)*(vertices+1));
  lon = (double *) MALLOC(sizeof(double)*(vertices+1));
  lat[0] = pairs->ns_lat;
  lon[0] = pairs->ns_lon;
  lat[1] = pairs->fs_lat;
  lon[1] = pairs->fs_lon;
  lat[2] = pairs->fe_lat;
  lon[2] = pairs->fe_lon;
  lat[3] = pairs->ne_lat;
  lon[3] = pairs->ne_lon;
  lat[vertices] = lat[0];
  lon[vertices] = lon[0];
  
  // Write information into database file
  if (strcmp_case(pairs->m_sensor, pairs->s_sensor) == 0) {
    DBFWriteStringAttribute(dbase, ii, 0, pairs->m_sensor);
    off = 0;
  }
  else {
    DBFWriteStringAttribute(dbase, ii, 0, pairs->m_sensor);
    DBFWriteStringAttribute(dbase, ii, 1, pairs->s_sensor);
    off = 1;
  }
  DBFWriteStringAttribute(dbase, ii, off+1, pairs->mode);
  DBFWriteIntegerAttribute(dbase, ii, off+2, pairs->frame);
  DBFWriteStringAttribute(dbase, ii, off+3, pairs->orbit_dir);
  DBFWriteIntegerAttribute(dbase, ii, off+4, pairs->master);
  sscanf(pairs->m_time, "%4d-%3dT", &jd.year, &jd.jd);
  date_jd2ymd(&jd, &ymd);
  sprintf(date, "%d-%s-%d", ymd.day, mon[ymd.month], ymd.year);
  DBFWriteStringAttribute(dbase, ii, off+5, date);
  DBFWriteIntegerAttribute(dbase, ii, off+6, pairs->slave);
  sscanf(pairs->s_time, "%4d-%3dT", &jd.year, &jd.jd);
  date_jd2ymd(&jd, &ymd);
  sprintf(date, "%d-%s-%d", ymd.day, mon[ymd.month], ymd.year);
  DBFWriteStringAttribute(dbase, ii, off+7, date);
  DBFWriteIntegerAttribute(dbase, ii, off+8, pairs->b_par);
  DBFWriteIntegerAttribute(dbase, ii, off+9, pairs->b_perp);
  DBFWriteIntegerAttribute(dbase, ii, off+10, pairs->b_temp);
  DBFWriteDoubleAttribute(dbase, ii, off+11, pairs->c_lat);
  DBFWriteDoubleAttribute(dbase, ii, off+12, pairs->c_lon);
  DBFWriteDoubleAttribute(dbase, ii, off+13, pairs->ns_lat);
  DBFWriteDoubleAttribute(dbase, ii, off+14, pairs->ns_lon);
  DBFWriteDoubleAttribute(dbase, ii, off+15, pairs->fs_lat);
  DBFWriteDoubleAttribute(dbase, ii, off+16, pairs->fs_lon);
  DBFWriteDoubleAttribute(dbase, ii, off+17, pairs->ne_lat);
  DBFWriteDoubleAttribute(dbase, ii, off+18, pairs->ne_lon);
  DBFWriteDoubleAttribute(dbase, ii, off+19, pairs->fe_lat);
  DBFWriteDoubleAttribute(dbase, ii, off+20, pairs->fe_lon);
  
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
