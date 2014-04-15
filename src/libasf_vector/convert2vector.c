#include "asf_vector.h"

format_type_t str2format(const char *str)
{
  format_type_t format;

  if (strcmp_case(str, "META") == 0)
    format = META;
  else if (strcmp_case(str, "LEADER") == 0 || strcmp_case(str, "CEOS") == 0)
    format = LEADER;
  else if (strcmp_case(str, "STF") == 0)
    format = STF_META;
  else if (strcmp_case(str, "POINT") == 0)
    format = POINT;
  else if (strcmp_case(str, "POLYGON") == 0)
    format = POLYGON;
  else if (strcmp_case(str, "CSV") == 0)
    format = CSV;
  else if (strcmp_case(str, "AUIG") == 0)
    format = AUIG;
  else if (strcmp_case(str, "MULTIMATCH") == 0)
    format = MULTIMATCH;
  else if (strcmp_case(str, "GEOTIFF") == 0)
    format = GEOTIFF_META;
  else if (strcmp_case(str, "KML") == 0)
    format = KMLFILE;
  else if (strcmp_case(str, "SHAPE") == 0)
    format = SHAPEFILE;
  else if (strcmp_case(str, "URSA") == 0)
    format = URSA;
  else if (strcmp_case(str, "DATAPOOL") == 0)
    format = DATAPOOL;
  else if (strcmp_case(str, "HAP") == 0)
    format = HAP;
  else if (strcmp_case(str, "TERRASAR") == 0)
    format = TERRASAR_META;
  else if (strcmp_case(str, "FOOT_PRINT") == 0)
    format = FOOT_PRINT;
  else if (strcmp_case(str, "GRANULE") == 0)
    format = GRANULE;
  else if (strcmp_case(str, "SMAP") == 0)
    format = SMAP_BOUNDARY;
  else if (strcmp_case(str, "LATLON") == 0)
    format = LATLON;
  else
    format = CUSTOM_FORMAT;

  return format;
}

char *format2str(format_type_t format)
{
  char *str = (char *) MALLOC(sizeof(char)*25);

  if (format == META)
    strcpy(str, "META");
  else if (format == LEADER)
    strcpy(str, "CEOS");
  else if (format == STF_META)
    strcpy(str, "STF");
  else if (format == POINT)
    strcpy(str, "POINT");
  else if (format == POLYGON)
    strcpy(str, "POLYGON");
  else if (format == CSV)
    strcpy(str, "CSV");
  else if (format == AUIG)
    strcpy(str, "AUIG");
  else if (format == MULTIMATCH)
    strcpy(str, "MULTIMATCH");
  else if (format == GEOTIFF_META)
    strcpy(str, "GEOTIFF");
  else if (format == KMLFILE)
    strcpy(str, "KML");
  else if (format == SHAPEFILE)
    strcpy(str, "SHAPE");
  else if (format == URSA)
    strcpy(str, "URSA");
  else if (format == DATAPOOL)
    strcpy(str, "DATAPOOL");
  else if (format == HAP)
    strcpy(str, "HAP");
  else if (format == TERRASAR_META)
    strcpy(str, "TERRASAR");
  else if (format == FOOT_PRINT)
    strcpy(str, "FOOT_PRINT");
  else if (format == GRANULE)
    strcpy(str, "GRANULE");
  else if (format == SMAP_BOUNDARY)
    strcpy(str, "SMAP");
  else if (format == LATLON)
    strcpy(str, "LATLON");

  return str;
}

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

  /*
  if (inFormat == META && outFormat == KMLFILE)
    ret = meta2kml(inFile, outFile, META, cfg);
  else if (inFormat == LEADER && outFormat == KMLFILE)
    ret = meta2kml(inFile, outFile, LEADER, cfg);
  else if (inFormat == STF_META && outFormat == KMLFILE)
    ret = meta2kml(inFile, outFile, STF_META, cfg);
  else if ((inFormat == META || inFormat == LEADER) && outFormat == SHAPEFILE)
    ret = convert2shape(inFile, outFile, "META", listFlag);
  else if (inFormat == TERRASAR_META && outFormat == KMLFILE)
    ret = terrasar2kml(inFile, outFile, listFlag);
  else if (inFormat == TERRASAR_META && outFormat == SHAPEFILE)
    ret = terrasar2shape(inFile, outFile, listFlag);
  else if (inFormat == POINT && outFormat == KMLFILE)
    ret = point2kml(inFile, outFile, listFlag);
  else if (inFormat == POINT && outFormat == SHAPEFILE)
    ret = point2shape(inFile, outFile, listFlag);
  else if (inFormat == POLYGON && outFormat == KMLFILE)
    ret = polygon2kml(inFile, outFile, listFlag);
  else if (inFormat == POLYGON && outFormat == SHAPEFILE)
    ret = polygon2shape(inFile, outFile, listFlag);
  else if (inFormat == CSV && outFormat == KMLFILE)
    ret = csv2kml(inFile, outFile, listFlag);
  else if (inFormat == CSV && outFormat == SHAPEFILE)
    ret = csv2shape(inFile, outFile, listFlag);
  else if (inFormat == AUIG && outFormat == KMLFILE)
    ret = auig2kml(inFile, outFile, listFlag);
  else if (inFormat == AUIG && outFormat == SHAPEFILE)
    ret = auig2shape(inFile, outFile, listFlag);
  else if (inFormat == GEOTIFF_META && outFormat == KMLFILE)
    ret = geotiff2kml(inFile, outFile, listFlag);
  else if (inFormat == GEOTIFF_META && outFormat == SHAPEFILE)
    ret = geotiff2shape(inFile, outFile, listFlag);
  else if (inFormat == KMLFILE && outFormat == AUIG)
    ret = kml2auig(inFile, outFile, listFlag);
  else if (inFormat == KMLFILE && outFormat == SHAPEFILE)
    ret = kml2shape(inFile, outFile, listFlag);
  else if (inFormat == KMLFILE && outFormat == URSA)
    ret = kml2ursa(inFile, outFile, listFlag);
  else if (inFormat == SHAPEFILE && outFormat == KMLFILE)
    ret = shape2kml(inFile, outFile, listFlag);
  else if (inFormat == URSA && outFormat == KMLFILE)
    ret = ursa2kml(inFile, outFile, listFlag, stackFlag);
  else if (inFormat == URSA && outFormat == SHAPEFILE)
    ret = ursa2shape(inFile, outFile, listFlag, stackFlag);
  else if (inFormat == DATAPOOL && outFormat == KMLFILE)
    ret = datapool2kml(inFile, outFile, listFlag, stackFlag);
  else if (inFormat == DATAPOOL && outFormat == SHAPEFILE)
    ret = datapool2shape(inFile, outFile, listFlag, stackFlag);
  else if (inFormat == SMAP_BOUNDARY && outFormat == SHAPEFILE)
    ret = smap2shape(inFile, outFile);
  else if (inFormat == GRANULE && outFormat == SHAPEFILE)
    ret = granule2shape(inFile, outFile);
  else if (inFormat == LATLON && outFormat == SHAPEFILE)
    ret = latlon2shape(inFile, outFile);
  // custom conversion defined by parameter set in 'header.lst'
  else if (inFormat == CUSTOM_FORMAT && outFormat == SHAPEFILE)
    ret = custom2shape(inFile, inFormat_str, outFile, listFlag);
  else if (inFormat == CUSTOM_FORMAT && outFormat == KMLFILE)
    ret = custom2kml(inFile, inFormat_str, outFile, listFlag);
  else
    asfPrintError("Conversion not supported.\n");
  */
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

  if (ret==1)
    asfPrintStatus("Successful completion!\n");
  return ret;
}
