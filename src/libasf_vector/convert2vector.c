#include "asf_vector.h"

int convert2vector(c2v_config *cfg)
{
  char inFormat[25], outFormat[25], inFile[512], outFile_in[512];
  strcpy(inFormat, cfg->input_format);
  strcpy(outFormat, cfg->output_format);
  strcpy(inFile, cfg->input_file);
  strcpy(outFile_in, cfg->output_file);
  int listFlag = cfg->list, ret = FALSE;

  asfPrintStatus("Converting from %s to %s:\n", inFormat, outFormat);
  asfPrintStatus("  %s -> %s.\n", inFile, outFile_in); 
  char *outFile = STRDUP(outFile_in);
  if (strcmp_case(outFormat, "SHAPE") == 0) {
    // The shapefile conversion does not like to have the extension added
    char *ext = findExt(outFile);
    if (strcmp_case(ext, ".shp")==0) *ext = '\0';
  }

  if (strcmp_case(inFormat, "LATLON") == 0 &&
    strcmp_case(outFormat, "SHAPE") == 0)
    ret = latlon2shape(inFile, outFile);
  if (strcmp_case(inFormat, "POINT") == 0 &&
    strcmp_case(outFormat, "KML") == 0)
    ret = point2kml(inFile, outFile);
  else if (strcmp_case(outFormat, "SHAPE") == 0)
    ret = convert2shape(inFile, outFile, inFormat, listFlag);
  else if (strcmp_case(outFormat, "KML") == 0)
    ret = convert2kml(inFile, outFile, inFormat, listFlag, cfg);

  free(outFile);

  char *configFile = (char *) MALLOC(sizeof(char)*1024);
  configFile = appendExt(outFile, ".cfg");
  cfg->short_config = TRUE;
  write_c2v_config(configFile, cfg);
  FREE(configFile);

  if (ret==1)
    asfPrintStatus("Successful completion!\n");
  return ret;
}
