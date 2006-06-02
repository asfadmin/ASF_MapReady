#include <stdio.h>
#include <string.h>
#include "asf.h"
#include "asf_reporting.h"
#include "shapefil.h"

// Reading a shapefile assuming that the lat/lon information that are used
// for creating the actual shapefile are also stored in the database file.
// In plain text: use the write_shapefile function to create it and you will
// have no problems in reading it
int read_shapefile (char *baseFile, char *pointFile)
{
  FILE *fpDbase=NULL, *fpShape=NULL;
  DBFHandle dbase;
  DBFFieldType dbaseType;
  SHPHandle shape;
  SHPObject *shapeObject;
  int i, k, nRecords, nFields, nPoints, nWidth, nDecimals, nValue, pointType;
  double fValue;
  char dbaseFile[255], shapeFile[255], fieldName[20], str[10];
  const char *sValue;

  // The shapefile business is a little less straight forward
  // Create shapefile name
  sprintf(shapeFile, "%s.shp", baseFile);

  // Open files
  shape = SHPOpen(shapeFile, "rb");

  // Get general information out of the shapefile
  SHPGetInfo(shape, NULL, &pointType, NULL, NULL);

  if (pointType == SHPT_POLYGON) {

    // We assume the simple case of one polygon in the shapefile.
    // Might want to extend this later
    shapeObject = SHPReadObject(shape, 0);

    // Write the lat/lon coordinates out of the object into a point file
    fpShape = FOPEN(pointFile, "w");
    for (i=0; i<shapeObject->nVertices-1; i++)
      fprintf(fpShape, "%d\t%.4lf\t%.4lf\n", i+1, shapeObject->padfY[i], 
	      shapeObject->padfX[i]);
    FCLOSE(fpShape);

  }
  else if (pointType == SHPT_POINT) {

    // Allocate memory
    sValue = (char *) MALLOC(sizeof(char)*255);
    
    // Point shapefiles can be dealt with by reading the database file.
    // Polygon shapefile require extracting the vertices out of the shapefile.
    
    // Here goes the database scenario
    // Create database file name
    sprintf(dbaseFile, "%s.dbf", baseFile);
    
    // Open files
    dbase = DBFOpen(dbaseFile, "rb");
    if (dbase == NULL)
      asfPrintError("Could not open database file '%s'\n");
    fpDbase = FOPEN(pointFile, "w");
    
    // Get database parameters
    nFields = DBFGetFieldCount(dbase);
    nRecords = DBFGetRecordCount(dbase);
    
    // Read information out of the database and dump it into a text file
    for (i=0; i<nRecords; i++) { 
      for (k=0; k<nFields; k++) {
	if (k>0) 
	  fprintf(fpDbase, "\t");
	dbaseType = DBFGetFieldInfo(dbase, k, fieldName, &nWidth, &nDecimals);
	if (dbaseType == FTString) {
	  sValue = DBFReadStringAttribute(dbase, i, k);
	  fprintf(fpDbase, "%s", sValue);
	}
	else if (dbaseType == FTInteger) {
	  nValue = DBFReadIntegerAttribute(dbase, i, k);
	  fprintf(fpDbase, "%d", nValue);
	}
	else if (dbaseType == FTDouble) {
	  fValue = DBFReadDoubleAttribute(dbase, i, k);
	  sprintf(str, "%%%d.%dlf ", nWidth, nDecimals);
	  fprintf(fpDbase, str, fValue);
	}
      }
      fprintf(fpDbase, "\n");
    }
    FCLOSE(fpDbase);

  }    
  
  SHPDestroyObject(shapeObject);
  SHPClose(shape);

  return(0);
}
