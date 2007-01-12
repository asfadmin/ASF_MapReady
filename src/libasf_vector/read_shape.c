#include <stdio.h>
#include <string.h>
#include "asf.h"
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
  int i, k, nRecords, nFields, nWidth, nDecimals, nValue, pointType;
  int nEntities, nVertices, nParts, iPart;
  double fValue;
  char dbaseFile[255], shapeFile[255], fieldName[20], str[10];
  const char *sValue;

  // The shapefile business is a little less straight forward
  // Create shapefile name
  sprintf(shapeFile, "%s.shp", baseFile);

  // Open files
  shape = SHPOpen(shapeFile, "rb");

  // Get general information out of the shapefile
  SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);

  if (pointType == SHPT_POLYGON) {

    //asfPrintStatus("   Number of structures: %d\n", nEntities);
    
    fpShape = FOPEN(pointFile, "w");
    for (k=0; k<nEntities; k++) {
      // Read a shape object
      //asfPrintStatus("\n   Reading structure: %d\n", k+1);
      shapeObject = SHPReadObject(shape, k);
      nParts = shapeObject->nParts;
      //asfPrintStatus("   Number of vertices: %d\n", shapeObject->nVertices);
      //asfPrintStatus("   Number of parts: %d\n", nParts);

      // Write the lat/lon coordinates out of the object into a point file
      for (i=0, iPart=1; i<shapeObject->nVertices; i++) {
	/*
	if (nParts == 0)
	  fprintf(fpShape, "%d\n", shapeObject->nVertices);
	else if (i == 0) {
	  nVertices = shapeObject->panPartStart[iPart] -
	    shapeObject->panPartStart[iPart-1];
	  fprintf(fpShape, "%d\n", nVertices);
	}
	if (iPart < nParts && shapeObject->panPartStart[iPart] == i) {
	  fprintf(fpShape, "end\n");
	  iPart++;
	  nVertices = shapeObject->panPartStart[iPart] -
	    shapeObject->panPartStart[iPart-1];
	  fprintf(fpShape, "%d\n", nVertices);
	}
	*/
	fprintf(fpShape, "%d, %.4lf, %.4lf\n", i+1, shapeObject->padfY[i], 
		shapeObject->padfX[i]);
      }

      SHPDestroyObject(shapeObject);
    }
    //fprintf(fpShape, "end\n");
    FCLOSE(fpShape);
    SHPClose(shape);

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
  
  return(0);
}
