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
  if (inFormat == AUIG && outFormat == KMLFILE) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/auig.kml", tmpDir);
    ret = auig2kml(inFile, tmpFile, listFlag);
    if (ret)
      ret = kml2auig(tmpFile, outFile, listFlag);
  }
  else if (inFormat == KMLFILE && outFormat == AUIG) {
    setup_test_dir(tmpDir);
    sprintf(tmpFile, "%s/auig.csv", tmpDir);
    ret = kml2auig(inFile, tmpFile, listFlag);
    if (ret)
      ret = auig2kml(tmpFile, outFile, listFlag);
  }
  else
    asfPrintError("Conversion irreversible. Can't perform test\n");

  // Clean up
  remove_dir(tmpDir);
  FREE(tmpFile);
  FREE(tmpDir);

  return ret;
}
