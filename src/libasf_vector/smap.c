#include "asf_vector.h"
#include "shapefil.h"
#include "asf_nan.h"
#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include "dateUtil.h"
#include "hdf5.h"
#include "smap.h"

void shape_smap_init(char *inFile)
{
  char *dbaseFile;
  DBFHandle dbase;
  SHPHandle shape;

  // Open database for initialization
  dbaseFile = appendExt(inFile, ".dbf");
  dbase = DBFCreate(dbaseFile);
  if (!dbase)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);

  // Add fields to database
  if (DBFAddField(dbase, "GRANULE", FTString, 100, 0) == -1)
    asfPrintError("Could not add GRANULE field to database file\n");
  if (DBFAddField(dbase, "START_TIME", FTString, 30, 0) == -1)
    asfPrintError("Could not add START_TIME field to database file\n");
  if (DBFAddField(dbase, "END_TIME", FTString, 30, 0) == -1)
    asfPrintError("Could not add STOP_TIME field to database file\n");
  if (DBFAddField(dbase, "ORBIT_DIR", FTString, 15, 0) == -1)
    asfPrintError("Could not add ORBIT_DIR field to database file\n");

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

int smap2shape(char *inFile, char *outFile)
{
  DBFHandle dbase;
  SHPHandle shape;
  double *lat, *lon;
  int line_count, sample_count, vertex_count;

  // Initalize the database file
  shape_smap_init(outFile);
  open_shape(outFile, &dbase, &shape);

  // Read lat/lon for boundary of SMAP data set
  check_smap_file(inFile, &line_count, &sample_count);
  lat = (double *) MALLOC(sizeof(double)*(line_count+sample_count)*2+1);
  lon = (double *) MALLOC(sizeof(double)*(line_count+sample_count)*2+1);
  read_smap_outline(inFile, &vertex_count, lat, lon);  
  lat[vertex_count] = lat[0];
  lon[vertex_count] = lon[0];

	// Read metadata
  smap_meta *smap = read_smap_meta(inFile);	

  // Write attributes
  DBFWriteStringAttribute(dbase, 0, 0, smap->file_name);
  DBFWriteStringAttribute(dbase, 0, 1, smap->orbit_start_date_time);
  DBFWriteStringAttribute(dbase, 0, 2, smap->orbit_stop_date_time);
  DBFWriteStringAttribute(dbase, 0, 3, smap->orbit_direction);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject =
    SHPCreateSimpleObject(SHPT_POLYGON, vertex_count+1, lon, lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);  
  
  // Clean up
  FREE(lat);
  FREE(lon);
  FREE(smap);
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  return 1;
}
