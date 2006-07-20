#include <stdio.h>
#include <string.h>
#include "asf.h"
#include "asf_meta.h"
#include "dateUtil.h"
#include "shapefil.h"

#define maxPoints 1000

// Determine the corner coordinates from the metadata, passing back values of
// latitude and longitude in pre-allocated arrays
void corner_coords(meta_parameters *meta, double *lat, double *lon) 
{
  meta_get_latLon(meta, 0, 0, 0.0, &lat[0], &lon[0]);
  meta_get_latLon(meta, 0, meta->general->sample_count, 0.0, 
		  &lat[1], &lon[1]);
  meta_get_latLon(meta, meta->general->line_count, meta->general->sample_count,
                  0.0, &lat[2], &lon[2]);
  meta_get_latLon(meta, meta->general->line_count, 0, 0.0, 
		  &lat[3], &lon[3]);
 }

void shape_init(char *shapeFile)
{
  SHPHandle shape;

  // Create shape file
  shape = SHPCreate(shapeFile, SHPT_POLYGON);
  if (shape == NULL)
    asfPrintError("Could not create shapefile '%s'\n");

  // Close shape file for now
  SHPClose(shape);
}

void dbase_init(char *dbaseFile)
{
  DBFHandle dbase;

  // Create the database
  dbase = DBFCreate(dbaseFile);
  if (dbase == NULL)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);
  
  // Add fields to database
  if (DBFAddField(dbase, "Sensor", FTString, 255, 0) == -1)
    asfPrintError("Could not add sensor field to database file\n");
  if (DBFAddField(dbase, "Beam mode", FTString, 4, 0) == -1)
    asfPrintError("Could not add beam mode field to database file\n");
  //  if (DBFAddField(dbase, "Polarization", FTString, 3, 0) == -1)
  //    asfPrintError("Could not add polarization field to database file\n");
  if (DBFAddField(dbase, "Orbit", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add orbit field to database file\n");
  if (DBFAddField(dbase, "Frame", FTInteger, 4, 0) == -1)
    asfPrintError("Could not add frame field to database file\n");
  if (DBFAddField(dbase, "Date", FTString, 25, 0) == -1)
    asfPrintError("Could not add acquisition date field to database file\n");
  if (DBFAddField(dbase, "Direction", FTString, 15, 0) == -1)
    asfPrintError("Could not add orbit direction field to database file\n");
  if (DBFAddField(dbase, "Latitude", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add latitude field to database file\n");
  if (DBFAddField(dbase, "Longitude", FTDouble, 9, 4) == -1)
	asfPrintError("Could not add longitude field to database file\n");

  // Close the database
  DBFClose(dbase);
}

void meta2dbase(meta_parameters *meta, char *dbaseFile, int n)
{
  DBFHandle dbase;
  julian_date jdate;
  ymd_date ymd;  
  char acquisition_date[25];
  char *mon[13]={"", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
		 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

  // Open database for adding values to the attributes
  dbase = DBFOpen(dbaseFile, "r+b");
  if (dbase == NULL)
    asfPrintError("Could not open database file '%s'\n", dbaseFile);

  // Write metadata into nth record of the database
  DBFWriteStringAttribute(dbase, n, 0, meta->general->sensor);
  DBFWriteStringAttribute(dbase, n, 1, meta->general->mode);
  //DBFWriteStringAttribute(dbase, n, 2, meta->sar->polarization);
  DBFWriteIntegerAttribute(dbase, n, 2, meta->general->orbit);
  DBFWriteIntegerAttribute(dbase, n, 3, meta->general->frame);
  jdate.year = meta->state_vectors->year;
  jdate.jd = meta->state_vectors->julDay;
  date_jd2ymd(&jdate, &ymd);
  sprintf(acquisition_date, "%d-%s-%d", ymd.day, mon[ymd.month], ymd.year);
  DBFWriteStringAttribute(dbase, n, 4, acquisition_date);
  if (meta->general->orbit_direction == 'D')
    DBFWriteStringAttribute(dbase, n, 5, "Descending");
  else if (meta->general->orbit_direction == 'A')
    DBFWriteStringAttribute(dbase, n, 5, "Ascending");
  DBFWriteDoubleAttribute(dbase, n, 6, meta->general->center_latitude);
  DBFWriteDoubleAttribute(dbase, n, 7, meta->general->center_longitude);

  // Close database
  DBFClose(dbase);
}

// Write the corner coordinates for a metadata file into a shapefile
void meta2shape(char *metaFile, char *shapeFile)
{
  SHPHandle shape;
  SHPObject *shapeObject=NULL;
  meta_parameters *meta;
  char dbaseFile[255];
  double lat[5], lon[5];

  // Initialize database and shapefile
  sprintf(dbaseFile, "%s.dbf", shapeFile);
  dbase_init(dbaseFile);
  shape_init(shapeFile);

  // Read metadata and determine corner coordinates
  meta = meta_read(metaFile);
  corner_coords(meta, lat, lon);
  lat[4] = lat[0];
  lon[4] = lon[0];	

  // Open shapefile for writing polygon
  shape = SHPOpen(shapeFile, "r+b");
  if (shape == NULL)
    asfPrintError("Could not open shapefile '%s\n");

  // Adding attributes to the database
  meta2dbase(meta, dbaseFile, 0);

  // Create shapefile object
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, 5, lon, lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  // Close shapefile
  SHPClose(shape);
  meta_free(meta);
}

// Write corner coordinates for a list of frames into a shapefile
void meta2shape_list(char *list, char *shapeFile)
{
  FILE *fp;
  SHPHandle shape;
  SHPObject *shapeObject=NULL;
  meta_parameters *meta;
  int n=0;
  char dbaseFile[255], metaFile[1024];
  double lat[5], lon[5];

  // Initialize database and shapefile
  sprintf(dbaseFile, "%s.dbf", shapeFile);
  dbase_init(dbaseFile);
  shape_init(shapeFile);

  // Open list and keep reading file names
  fp = FOPEN(list, "r");
  while (fgets(metaFile, 1024, fp)) {
 
    if (n > 200) // arbitrary number
      asfPrintError("Cannot handle more than 200 frames at a time!\n");

    // Read metadata and determine corner coordinates
    meta = meta_read(metaFile);
    corner_coords(meta, lat, lon);
    lat[4] = lat[0];
    lon[4] = lon[0];	
    
    // Adding attributes to the database
    meta2dbase(meta, dbaseFile, n);
    
    // Add the frame to the shape
    shape = SHPOpen(shapeFile, "r+b");
    if (shape == NULL)
      asfPrintError("Could not open shapefile '%s\n");
    shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, 5, lon, lat, NULL);
    SHPGetInfo(shape, NULL, NULL, NULL, NULL);
    SHPWriteObject(shape, -1, shapeObject);
    SHPDestroyObject(shapeObject);  
    SHPClose(shape);

    meta_free(meta);
    n++;
  }
  FCLOSE(fp);

}
