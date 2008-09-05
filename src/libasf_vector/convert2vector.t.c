#include "asf_vector.h"

static void setup_test_dir(char *tmp_dir)
{
  // Create temporary test directory
  strcpy(tmp_dir, "test_c2v-");
  strcat(tmp_dir, time_stamp_dir());
  create_clean_dir(tmp_dir);
}

int test_c2v(char *inFile, const char *inFormat_str,
	     char *outFile, const char *outFormat_str)
{
  int ret = 0;
  int listFlag = 0;
  format_type_t inFormat = str2format(inFormat_str);
  format_type_t outFormat = str2format(outFormat_str);
  char *tmpFile = (char *) MALLOC(sizeof(char)*255);
  char *tmpDir = (char *) MALLOC(sizeof(char)*512);

  // Convert the formats forth and back, otherwise back out
  if (inFormat == POINT && outFormat == KMLFILE) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/point.kml", tmpDir);
    ret = point2kml(inFile, tmpFile, listFlag);
    if (ret)
      ret = kml2point(tmpFile, outFile, listFlag);
  }
  else if (inFormat == POINT && outFormat == SHAPEFILE) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/point", tmpDir);
    ret = point2shape(inFile, tmpFile, listFlag);
    if (ret)
      ret = shape2point(tmpFile, outFile, listFlag);
  }
  else if (inFormat == POLYGON && outFormat == KMLFILE) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/polygon.kml", tmpDir);
    ret = polygon2kml(inFile, tmpFile, listFlag);
    if (ret)
      ret = kml2polygon(tmpFile, outFile, listFlag);
  }
  else if (inFormat == POLYGON && outFormat == SHAPEFILE) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/polygon", tmpDir);
    ret = polygon2shape(inFile, tmpFile, listFlag);
    if (ret)
      ret = shape2polygon(tmpFile, outFile, listFlag);
  }
  else if (inFormat == CSV && outFormat == KMLFILE) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/csv.kml", tmpDir);
    ret = csv2kml(inFile, tmpFile, listFlag);
    if (ret)
      ret = kml2csv(tmpFile, outFile, listFlag);
  }
  else if (inFormat == CSV && outFormat == SHAPEFILE) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/csv", tmpDir);
    ret = csv2shape(inFile, tmpFile, listFlag);
    if (ret)
      ret = shape2csv(tmpFile, outFile, listFlag);
  }
  else if (inFormat == AUIG && outFormat == KMLFILE) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/auig.kml", tmpDir);
    ret = auig2kml(inFile, tmpFile, listFlag);
    if (ret)
      ret = kml2auig(tmpFile, outFile, listFlag);
  }
  else if (inFormat == KMLFILE && outFormat == POINT) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/point.csv", tmpDir);
    ret = kml2point(inFile, tmpFile, listFlag);
    if (ret)
      ret = point2kml(tmpFile, outFile, listFlag);
  }
  else if (inFormat == KMLFILE && outFormat == POLYGON) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/polygon.csv", tmpDir);
    ret = kml2polygon(inFile, tmpFile, listFlag);
    if (ret)
      ret = polygon2kml(tmpFile, outFile, listFlag);
  }
  else if (inFormat == KMLFILE && outFormat == CSV) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/generic.csv", tmpDir);
    ret = kml2csv(inFile, tmpFile, listFlag);
    if (ret)
      ret = csv2kml(tmpFile, outFile, listFlag);
  }
  else if (inFormat == KMLFILE && outFormat == AUIG) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/auig.csv", tmpDir);
    ret = kml2auig(inFile, tmpFile, listFlag);
    if (ret)
      ret = auig2kml(tmpFile, outFile, listFlag);
  }
  else if (inFormat == SHAPEFILE && outFormat == POINT) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/point.csv", tmpDir);
    ret = shape2point(inFile, tmpFile, listFlag);
    if (ret)
      ret = point2shape(tmpFile, outFile, listFlag);
  }
  else if (inFormat == SHAPEFILE && outFormat == POLYGON) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/polygon.csv", tmpDir);
    ret = shape2polygon(inFile, tmpFile, listFlag);
    if (ret)
      ret = polygon2shape(tmpFile, outFile, listFlag);
  }
  else if (inFormat == SHAPEFILE && outFormat == CSV) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/generic.csv", tmpDir);
    ret = shape2csv(inFile, tmpFile, listFlag);
    if (ret)
      ret = csv2shape(tmpFile, outFile, listFlag);
  }
  else
    asfPrintError("Conversion irreversible. Can't perform test\n");

  // Clean up
  remove_dir(tmpDir);
  FREE(tmpFile);
  FREE(tmpDir);

  return ret;
}
