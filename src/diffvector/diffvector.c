#include "ogr_api.h"
#include "ogr_srs_api.h"
#include "asf_vector.h"
#include "asf_geocode.h"
#include "asf.h"
#include "asf_meta.h"

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
   "USAGE:\n"
   "   %s [-log <logFile>] <refFile> <testFile>\n", name);
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   refFile    Name of the reference vector file\n"
   "   testFile   Name of the vector file to compare\n");
  printf("\n"
   "OPTIONAL ARGUMENTS:\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program compares two vector files for attribute and geolocation."
   "\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

int strmatches(const char *key, ...)
{
    va_list ap;
    char *arg = NULL;
    int found = FALSE;

    va_start(ap, key);
    do {
        arg = va_arg(ap, char *);
        if (arg) {
            if (strcmp(key, arg) == 0) {
                found = TRUE;
                break;
            }
        }
    } while (arg);

    return found;
}

char *fieldType2str(OGRFieldType type)
{
  char *str = (char *) MALLOC(sizeof(char)*20);
  if (type == OFTInteger)
    strcpy(str, "Integer");
  else if (type == OFTIntegerList)
    strcpy(str, "IntegerList");
  else if (type == OFTReal)
    strcpy(str, "Real");
  else if (type == OFTRealList)
    strcpy(str, "RealList");
  else if (type == OFTString)
    strcpy(str, "String");
  else if (type == OFTStringList)
    strcpy(str, "StringList");
  else if (type == OFTBinary)
    strcpy(str, "Binary");
  else if (type == OFTDate)
    strcpy(str, "Date");
  else if (type == OFTTime)
    strcpy(str, "Time");
  else if (type == OFTDateTime)
    strcpy(str, "DateTime");
      
  return str;
}

char *fieldValue(OGRFeatureH hFeature, int field, 
  OGRFieldType type, int width, int precision)
{
  char *str = (char *) MALLOC(sizeof(char)*8192);
  if (type == OFTInteger)
    sprintf(str, "%d", OGR_F_GetFieldAsInteger(hFeature, field));
  else if (type == OFTReal)
    sprintf(str, "%*.*f", 
      width, precision, OGR_F_GetFieldAsDouble(hFeature, field));
  else if (type == OFTString)
    sprintf(str, "%s", OGR_F_GetFieldAsString(hFeature, field));
    
  return str;
}

int main(int argc, char **argv)
{
  char refFile[1024], testFile[1024], logFile[1024];
  char pass = TRUE;
  int currArg = 1;
  int NUM_ARGS = 2;
  report_level_t level = REPORT_LEVEL_STATUS;

  if (argc<=1)
    usage(argv[0]);

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-log","--log",NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
      level = REPORT_LEVEL_LOG;
    }
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
      quietflag = TRUE;
    }
    else {
      --currArg;
      break;
    }
  }

  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    usage(argv[0]);
  }

  strcpy(refFile, argv[currArg++]);
  strcpy(testFile, argv[currArg]);
  asfSplashScreen(argc, argv);

  // Open vector files
  OGRRegisterAll();
  OGRDataSourceH hSourceRef = OGROpen(refFile, FALSE, NULL);
  OGRDataSourceH hSourceTest = OGROpen(testFile, FALSE, NULL);  
  if (hSourceRef == NULL)
    asfPrintError("Could not open reference vector file (%s)!\n", refFile);
  if (hSourceTest == NULL)
    asfPrintError("Could not open test vector file (%s)!\n", testFile);
  fLog = FOPEN(logFile, "w");
   
  // Checking layer count
  int refLayerCount = OGR_DS_GetLayerCount(hSourceRef);
  int testLayerCount = OGR_DS_GetLayerCount(hSourceTest);
  if (refLayerCount == testLayerCount)
    asfReport(level, "Identical layer counts: %d -> passed\n", refLayerCount);
  else {
    pass = FALSE;
    asfReport(level, "Different layer counts! -> failed\n");
    asfReport(level, "Reference - file: %s, layer count: %d\n", 
      refFile, refLayerCount);
    asfReport(level, "Test - file: %s, layer count: %d\n", 
      testFile, testLayerCount);
  }

  // Going through layers
  if (pass) {  
    int kk;
    for (kk=0; kk<refLayerCount; kk++) {
  
      // Handles
      OGRLayerH hLayerRef = OGR_DS_GetLayer(hSourceRef, kk);
      OGR_L_ResetReading(hLayerRef);
      OGRLayerH hLayerTest = OGR_DS_GetLayer(hSourceTest, kk);
      OGR_L_ResetReading(hLayerTest);
      OGRFeatureDefnH hFeatureDefRef = OGR_L_GetLayerDefn(hLayerRef);
      OGRFeatureDefnH hFeatureDefTest = OGR_L_GetLayerDefn(hLayerTest);          
    
      // Check geometry type
      char refGeometry[512], testGeometry[512];
      sprintf(refGeometry, "%s", 
        OGRGeometryTypeToName(OGR_FD_GetGeomType(hFeatureDefRef)));
      sprintf(testGeometry, "%s",
        OGRGeometryTypeToName(OGR_FD_GetGeomType(hFeatureDefTest)));
      if (strcmp_case(refGeometry, testGeometry) == 0)
        asfReport(level, "Identical geometry type: %s -> passed\n", refGeometry);
      else {
        pass = FALSE;
        asfReport(level, "Different layer geometry! -> failed\n");
        asfReport(level, "Reference - file: %s, layer geometry: %s\n", 
          refFile, refGeometry);
        asfReport(level, "Test - file: %s, layer geometry: %s\n", 
          testFile, testGeometry);
      }

      // Check extent
      OGREnvelope refEnv, testEnv;
      OGR_L_GetExtent(hLayerRef, &refEnv, TRUE);
      OGR_L_GetExtent(hLayerTest, &testEnv, TRUE);
      char refExtent[100], testExtent[100];
      sprintf(refExtent, "((%.6f,%.6f)(%.6f,%.6f))", 
        refEnv.MinX, refEnv.MinY, refEnv.MaxX, refEnv.MaxY);
      sprintf(testExtent, "((%.6f,%.6f)(%.6f,%.6f))", 
        testEnv.MinX, testEnv.MinY, testEnv.MaxX, testEnv.MaxY);
      if (strcmp_case(refExtent, testExtent) == 0)
        asfReport(level, "Identical extents: %s -> passed\n", refExtent);
      else {
        pass = FALSE;
        asfReport(level, "Different extents! -> failed\n");
        asfReport(level, "Reference - file: %s, extent: %s\n", 
          refFile, refExtent);
        asfReport(level, "Test - file: %s, extent: %s\n", testFile, testExtent);
      }

      // Check feature count
      int refFeatureCount = OGR_L_GetFeatureCount(hLayerRef, TRUE);
      int testFeatureCount = OGR_L_GetFeatureCount(hLayerTest, TRUE);
      if (refFeatureCount == testFeatureCount)
        asfReport(level, "Identical feature count: %d -> passed\n", 
          refFeatureCount);
      else {
        pass = FALSE;
        asfReport(level, "Different feature count! -> failed\n");
        asfReport(level, "Reference - file: %s, feature count: %d\n",
          refFile, refFeatureCount);
        asfReport(level, "Test - file: %s, feature count: %d\n",
          testFile, testFeatureCount);
      }

      // Check spatial reference
      char *refSpatialRef, *testSpatialRef;
      OGRSpatialReferenceH hSpatialRef = OGR_L_GetSpatialRef(hLayerRef);
      OGRSpatialReferenceH hSpatialTest = OGR_L_GetSpatialRef(hLayerTest);
      OSRExportToPrettyWkt(hSpatialRef, &refSpatialRef, FALSE); 
      OSRExportToPrettyWkt(hSpatialTest, &testSpatialRef, FALSE); 
      if (strcmp_case(refSpatialRef, testSpatialRef) == 0) {
        asfReport(level, "Identical spatial reference -> passed\n");
        asfReport(level, "%s\n", refSpatialRef);
      }
      else {
        pass = FALSE;
        asfReport(level, "Different spatial reference! -> failed\n");
        asfReport(level, "Reference - file: %s, spatial reference:\n%s\n", 
          refFile, refSpatialRef);
        asfReport(level, "Test - file: %s, spatial reference:\n%s\n", 
        testFile, testSpatialRef);
      }
      OGRFree(refSpatialRef);
      OGRFree(testSpatialRef);

      // Go through features
      if (pass) {
        int jj;
        for (jj=0; jj<refFeatureCount; jj++) {
        
          // Handles
          OGRFeatureH hFeatureRef = OGR_L_GetFeature(hLayerRef, jj);
          OGRFeatureH hFeatureTest = OGR_L_GetFeature(hLayerTest, jj);

          // Check field count
          int refFieldCount = OGR_FD_GetFieldCount(hFeatureDefRef);
          int testFieldCount = OGR_FD_GetFieldCount(hFeatureDefTest);
          if (refFieldCount == testFieldCount)
            asfReport(level, "Identical field count: %d -> passed\n\n",
              refFieldCount);
          else {
            pass = FALSE;
            asfReport(level, "Different field count! -> failed\n");
            asfReport(level, "Reference - file: %s, field count: %d\n",
              refFile, refFieldCount);
            asfReport(level, "Test - file: %s, field count: %d\n\n",
              testFile, testFieldCount);
          }
      
          // Go through fields
          if (pass) {
            int ii;
            for (ii=0; ii<refFieldCount; ii++ ) {
      
              // Field handles
              OGRFieldDefnH hFieldDefRef = 
                OGR_FD_GetFieldDefn(hFeatureDefRef, ii);
              OGRFieldDefnH hFieldDefTest = 
                OGR_FD_GetFieldDefn(hFeatureDefTest, ii);

              // Check field names
              char refFieldName[50], testFieldName[50];
              sprintf(refFieldName, "%s", OGR_Fld_GetNameRef(hFieldDefRef));
              sprintf(testFieldName, "%s", OGR_Fld_GetNameRef(hFieldDefTest));
              if (strcmp_case(refFieldName, testFieldName) != 0) {
                pass = FALSE;
                asfReport(level, "Different field names! -> failed\n");
                asfReport(level, "Field - %d\n", ii+1);
                asfReport(level, "Reference - file: %s, field name: %s\n",
                  refFile, refFieldName);
                asfReport(level, "Test - file: %s, field name: %s\n",
                  testFile, testFieldName);
              }

              // Check field types
              char refFieldType[50], testFieldType[50];
              OGRFieldType fieldType = OGR_Fld_GetType(hFieldDefRef);
              sprintf(refFieldType, "%s", fieldType2str(fieldType));
              fieldType = OGR_Fld_GetType(hFieldDefTest);
              sprintf(testFieldType, "%s", fieldType2str(fieldType));
              if (strcmp_case(refFieldType, testFieldType) != 0) {
                pass = FALSE;
                asfReport(level, "Different field type! -> failed\n");
                asfReport(level, "Field - %s\n", refFieldName);
                asfReport(level, "Reference - file: %s, field type: %s\n",
                  refFile, refFieldType);
                asfReport(level, "Test - file: %s, field type: %s\n",
                  testFile, testFieldType);
              }
        
              // Check field width
              int refFieldWidth = OGR_Fld_GetWidth(hFieldDefRef);
              int testFieldWidth = OGR_Fld_GetWidth(hFieldDefTest);
              if (refFieldWidth != testFieldWidth) {
                pass = FALSE;
                asfReport(level, "Different field widths! -> failed\n");
                asfReport(level, "Field: %s\n", refFieldName);
                asfReport(level, "Reference - file: %s, field width: %d\n",
                  refFile, refFieldWidth);
                asfReport(level, "Test - file: %s, field width: %d\n",
                  testFile, testFieldWidth);
              }
        
              // Check field precision
              int refFieldPrecision = OGR_Fld_GetPrecision(hFieldDefRef);
              int testFieldPrecision = OGR_Fld_GetPrecision(hFieldDefTest);
              if (refFieldPrecision != testFieldPrecision) {
                pass = FALSE;
                asfReport(level, "Different field precisions! -> failed\n");
                asfReport(level, "Field - %s\n", refFieldName);
                asfReport(level, "Reference - file: %s, field precision: %d\n",
                  refFile, refFieldPrecision);
                asfReport(level, "Test - file: %s, field precision: %d\n",
                  testFile, testFieldPrecision);
              }
              
              // Check field value
              char refValue[8192], testValue [8192];
              sprintf(refValue, "%s", fieldValue(hFeatureRef, ii, fieldType,
                refFieldWidth, refFieldPrecision));
              sprintf(testValue, "%s", fieldValue(hFeatureTest, ii, fieldType,
                testFieldWidth, testFieldPrecision));
              if (strcmp_case(refValue, testValue) != 0) {
                pass = FALSE;
                asfReport(level, "Different field value! -> failed\n");
                asfReport(level, "Field - %s\n", refValue);
                asfReport(level, "Reference - file: %s, field value: %s\n",
                  refFile, refValue);
                asfReport(level, "Test - file: %s, field value: %s\n",
                  testFile, testValue);
              }
              if (pass)
                asfReport(level, "%s: %s [%s (%d,%d)] -> passed\n", 
                  refFieldName, refValue, fieldType2str(fieldType), 
                  refFieldWidth, refFieldPrecision);
            }

            // Check geometry
            OGRGeometryH hGeomContRef = OGR_F_GetGeometryRef(hFeatureRef);
            OGRGeometryH hGeomContTest = OGR_F_GetGeometryRef(hFeatureTest);
            int refContCount = OGR_G_GetGeometryCount(hGeomContRef);
            int testContCount = OGR_G_GetGeometryCount(hGeomContTest);
            if (refContCount == testContCount) {
              asfReport(level, "\nIdentical geometry info -> passed\n");
              asfReport(level, "Geometry contains %d elements\n", refContCount);
              int hh;
              for (hh=0; hh<refContCount; hh++) {
                OGRGeometryH hGeometryRef = 
                  OGR_G_GetGeometryRef(hGeomContRef, hh);
                OGRGeometryH hGeometryTest = 
                  OGR_G_GetGeometryRef(hGeomContTest, hh);
                int refCount = OGR_G_GetPointCount(hGeometryRef);
                int testCount = OGR_G_GetPointCount(hGeometryTest);
                if (refCount == testCount) {
                  asfReport(level, "\nIdentical vertex count -> passed\n");
                  asfReport(level, "Geometry[%d] contains %d vertices\n",
                    hh+1, refCount);
                  int gg;
                  double refLat, refLon, refHeight;
                  double testLat, testLon, testHeight;
                  char refVertexStr[50], testVertexStr[50];
                  for (gg=0; gg<refCount; gg++) {
                    OGR_G_GetPoint(hGeometryRef, gg, 
                      &refLat, &refLon, &refHeight);
                    sprintf(refVertexStr, "Lat: %.5f, Lon: %.5f\n",
                      refLat, refLon);
                    OGR_G_GetPoint(hGeometryTest, gg, 
                      &testLat, &testLon, &testHeight);
                    sprintf(testVertexStr, "Lat: %.5f, Lon: %.5f\n",
                      testLat, testLon);
                    if (strcmp_case(refVertexStr, testVertexStr) != 0) {
                      pass = FALSE;
                      asfReport(level, "\nDifferent coordinates! -> failed\n");
                      asfReport(level, "Reference - %s\n", refVertexStr);
                      asfReport(level, "Test - %s\n", testVertexStr);
                    }
                  }
                  if (pass)
                    asfReport(level, "\nIdentical coordinates -> passed\n");          
                }
                else {
                  pass = FALSE;
                  asfReport(level, "Different vertex count! -> failed\n");
                  asfReport(level, "Reference - Geometry contains %d vertices\n", 
                    refCount);
                  asfReport(level, "Test - Geometry contains %d vertices\n", 
                    testCount);
                }
              }
            }
            else {
              pass = FALSE;
              asfReport(level, "Different geometries! -> failed\n");
              asfReport(level, "Reference - Geometry contains %d elements\n", 
                refContCount);
              asfReport(level, "Test - Geometry contains %d elements\n", 
                testContCount);
            }
          }
        }
      }
    }
    if (pass)
      asfReport(level, "\nPASS: passed all tests outlined above\n");
    else
      asfReport(level, "\nFAIL: failed one or more tests as reported above\n");
    FCLOSE(fLog);
    OGR_DS_Destroy(hSourceRef);
  }
  return(0);
}
