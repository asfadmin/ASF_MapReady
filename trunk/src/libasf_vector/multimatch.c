#include "asf.h"
#include "shapefil.h"
#include "asf_vector.h"

// Convert multimatch to shape file
int multimatch2shape(char *inFile, char *outFile, int listFlag)
{
  DBFHandle dbase;
  SHPHandle shape;
  char line[1024], n;
  double lat, lon, ref_x, ref_y, ref_z, search_x, search_y, search_z;
  double dx, dy, dz, direction, speed;

  // Read information from line
  sscanf(line, "%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf",
	 &lat, &lon, &ref_x, &ref_y, &ref_z, &search_x, &search_y, &search_z,
	 &dx, &dy, &dz, &direction, &speed);

  // Write information into database file
  DBFWriteDoubleAttribute(dbase, n, 0, ref_x);
  DBFWriteDoubleAttribute(dbase, n, 1, ref_y);
  DBFWriteDoubleAttribute(dbase, n, 2, ref_z);
  DBFWriteDoubleAttribute(dbase, n, 3, search_x);
  DBFWriteDoubleAttribute(dbase, n, 4, search_y);
  DBFWriteDoubleAttribute(dbase, n, 5, search_z);
  DBFWriteDoubleAttribute(dbase, n, 6, dx);
  DBFWriteDoubleAttribute(dbase, n, 7, dy);
  DBFWriteDoubleAttribute(dbase, n, 8, dz);
  DBFWriteDoubleAttribute(dbase, n, 9, direction);
  DBFWriteDoubleAttribute(dbase, n, 10, speed);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POINT, 1, &lon, &lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  return 1;
}
