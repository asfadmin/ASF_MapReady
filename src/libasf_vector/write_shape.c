#include <stdio.h>
#include <string.h>
#include "asf.h"
#include "shapefil.h"

#define maxPoints 1000

// Write a shapefile that contains point information e.g. from corner reflectors
// Assumed format for point file: ID, lat, lon
int write_point_shapefile(char *baseFile, char *pointFile)
{
  int ret;
  char dbaseFile[255];
  
  sprintf(dbaseFile, "%s.dbf", baseFile);
  ret = write_shapefile(dbaseFile, baseFile, pointFile, SHPT_POINT, NULL);

  return ret;
}

// Write a shapefile that contains a polygon, e.g. to define an area of interest
// by masking out any points outside this polygon
// Assumed format for point file: lat, lon
int write_polygon_shapefile(char *baseFile, char *pointFile, char *comment)
{
  int ret;
  char dbaseFile[255];
  
  sprintf(dbaseFile, "%s.dbf", baseFile);
  ret = write_shapefile(dbaseFile, baseFile, pointFile, SHPT_POLYGON, comment);
  
  return ret;
}

// Function that does the actual work
int write_shapefile (char *dbaseFile, char *shapeFile, char *pointFile,
		     int pointType, char *comment)
{
  FILE *fpPoint=NULL;
  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject=NULL;
  int n=0, nRecords, nFields, nPoints=0;
  double lat[maxPoints], lon[maxPoints];
  char line[1024], id[maxPoints][255];

  // Read polygon file with all vertices in lat/lon
  // Determine number of vertices
  fpPoint = FOPEN(pointFile, "r");
  while (fgets(line, 1024, fpPoint))
    nPoints++;
  FCLOSE(fpPoint);
  if (nPoints > maxPoints)
    asfPrintError("Number of points beyond the threshold (%d)\n", maxPoints);

  // Read lat/lon pairs of vertices in
  fpPoint = FOPEN(pointFile, "r");
  while (fgets(line, 1024, fpPoint)) {
    sscanf(line, "%s %lf %lf", id[n], &lat[n], &lon[n]);
    n++;
  }
  if (pointType == SHPT_POLYGON) {
    lat[n] = lat[0];
    lon[n] = lon[0];
  }

  // Take care of the database part before writing shapefile
  // Create the database
  dbase = DBFCreate(dbaseFile);
  if (dbase == NULL)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);
  
  // Add fields to database
  switch (pointType) 
    {
    case SHPT_POLYGON:
      if (DBFAddField(dbase, "Comment", FTString, 255, 0) == -1)
	asfPrintError("Could not add comment field to database file\n");
      break;
    case SHPT_POINT:
      if (DBFAddField(dbase, "ID", FTString, 255, 0) == -1)
	asfPrintError("Could not add ID field to database file\n");
      if (DBFAddField(dbase, "Lat", FTDouble, 9, 4) == -1)
	asfPrintError("Could not add latitude field to database file\n");
      if (DBFAddField(dbase, "Lon", FTDouble, 9, 4) == -1)
	asfPrintError("Could not add longitude field to database file\n");
      break;
    }

  // Close the database for now
  DBFClose(dbase);

  // Open database for adding lat/lon or comment
  dbase = DBFOpen(dbaseFile, "r+b");
  if (dbase == NULL)
    asfPrintError("Could not open database file '%s'\n", dbaseFile);

  switch (pointType) 
    {
    case SHPT_POLYGON:
      DBFWriteStringAttribute(dbase, 0, 0, comment);
      break;
    case SHPT_POINT:
      for (n=0; n<nPoints; n++) {
	DBFWriteStringAttribute(dbase, n, 0, id[n]);
	DBFWriteDoubleAttribute(dbase, n, 1, lat[n]);
	DBFWriteDoubleAttribute(dbase, n, 2, lon[n]);
      }
      break;
    }
	
  // Close database for good
  DBFClose(dbase);


  // Now let's generate the actual shapefile for the polygon
  // Create shape file
  if (pointType == SHPT_POINT) 
    shape = SHPCreate(shapeFile, SHPT_POINT);
  else
    shape = SHPCreate(shapeFile, SHPT_POLYGON);
  if (shape == NULL)
    asfPrintError("Could not create shapefile '%s'\n");

  // Close shape file for now
  SHPClose(shape);

  // Open shapefile for writing polygon
  shape = SHPOpen(shapeFile, "r+b");
  if (shape == NULL)
    asfPrintError("Could not open shapefile '%s\n");

  // Create shapefile object
  switch (pointType) 
    {
    case SHPT_POLYGON:
      shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, nPoints+1, lon, lat, NULL);
      break;
    case SHPT_POINT:
      shapeObject = SHPCreateSimpleObject(SHPT_POINT, nPoints, lon, lat, NULL);
      break;
    }
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);
  SHPClose(shape);

  return(0);
}
