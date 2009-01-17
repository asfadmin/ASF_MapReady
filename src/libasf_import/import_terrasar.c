#include "terrasar.h"
#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "xml_util.h"
#include <ctype.h>

terrasar_meta *terrasar_meta_init(void)
{
  terrasar_meta *terrasar;
  terrasar = (terrasar_meta *) CALLOC(1, sizeof(terrasar_meta));
 
  strcpy(terrasar->imageData, MAGIC_UNSET_STRING);
  strcpy(terrasar->imageDataType, MAGIC_UNSET_STRING);
  strcpy(terrasar->imageDataFormat, MAGIC_UNSET_STRING);
  terrasar->numberOfLayers = MAGIC_UNSET_INT;
  terrasar->imageDataDepth = MAGIC_UNSET_INT;
  terrasar->numberOfRows = MAGIC_UNSET_INT;
  terrasar->numberOfColumns = MAGIC_UNSET_INT;

  return terrasar;
}

terrasar_meta *read_terrasar_meta2(const char *dataFile)
{
  terrasar_meta *terrasar = terrasar_meta_init();

  xmlDoc *doc = xmlReadFile(dataFile, NULL, 0);
  if (!doc)
    asfPrintError("Could not parse file %s\n", dataFile);

  strcpy(terrasar->imageData, xml_get_string(doc, "imageData:path"));
  strcat(terrasar->imageData, "/");
  strcat(terrasar->imageData, xml_get_string(doc, "imageData:filename"));

  strcpy(terrasar->imageDataType,
         xml_get_string(doc, "imageDataInfo:imageDataType"));
  strcpy(terrasar->imageDataFormat,
         xml_get_string(doc, "imageDataInfo:imageDataFormat"));

  terrasar->numberOfLayers = xml_get_int(doc, "imageDataInfo:numberOfLayers");
  terrasar->imageDataDepth = xml_get_int(doc, "imageDataInfo:imageDataDepth");
  terrasar->numberOfRows = xml_get_int(doc, "imageDataInfo:numberOfRows");
  terrasar->numberOfColumns = xml_get_int(doc, "imageDataInfo:numberOfColumns");

  xmlFreeDoc(doc);
  xmlCleanupParser();

  return terrasar;
}

terrasar_meta *read_terrasar_meta(const char *dataFile)
{
  terrasar_meta *terrasar = NULL;
  FILE *fp;
  int check = FALSE, ii;
  char line[1024], *valStart, *valEnd;

  terrasar = terrasar_meta_init();
  fp = FOPEN(dataFile, "r");

  // quick hack for reading a few parameters to display the image
  while (fgets(line, 1024, fp)) {
    chomp(line);
    if (strstr(line, "<imageData layerIndex=\"1\">"))
      check = TRUE;
    if (strstr(line, "<path>") && check) {
      valStart = strstr(line, "<path>");
      for (ii=0; ii<6; ii++)
	valStart++;
      valEnd = strstr(line, "</path>");
      valEnd[0] = '\0';
      sprintf(terrasar->imageData, "%s", valStart);
      strcat(terrasar->imageData, "/");
    }
    if (strstr(line, "<filename>") && check) {
      valStart = strstr(line, "<filename>");
      for (ii=0; ii<10; ii++)
	valStart++;
      valEnd = strstr(line, "</filename>");
      valEnd[0] = '\0';
      strcat(terrasar->imageData, valStart);
    }
    if (strstr(line, "</imageData>"))
      check = FALSE;
    if (strstr(line, "<imageDataInfo>"))
      check = TRUE;
    if (strstr(line, "<imageDataType>") && check) {
      valStart = strstr(line, "<imageDataType>");
      for (ii=0; ii<15; ii++)
	valStart++;
      valEnd = strstr(line, "</imageDataType>");
      valEnd[0] = '\0';
      sprintf(terrasar->imageDataType, "%s", valStart);      
    }
    if (strstr(line, "<imageDataFormat>") && check) {
      valStart = strstr(line, "<imageDataFormat>");
      for (ii=0; ii<17; ii++)
	valStart++;
      valEnd = strstr(line, "</imageDataFormat>");
      valEnd[0] = '\0';
      sprintf(terrasar->imageDataFormat, "%s", valStart);      
    }
    if (strstr(line, "<numberOfLayers>") && check) {
      valStart = strstr(line, "<numberOfLayers>");
      sscanf(valStart, "<numberOfLayers>%d</numberOfLayers>", 
	     &terrasar->numberOfLayers);
    }
    if (strstr(line, "<numberOfRows>") && check) {
      valStart = strstr(line, "<numberOfRows>");
      sscanf(valStart, "<numberOfRows>%d</numberOfRows>", 
	     &terrasar->numberOfRows);
    }
    if (strstr(line, "<numberOfColumns>") && check) {
      valStart = strstr(line, "<numberOfColumns>");
      sscanf(valStart, "<numberOfColumns>%d</numberOfColumns>", 
	     &terrasar->numberOfColumns);
    }
    if (strstr(line, "<groundRangeResolution>") && check) {
      valStart = strstr(line, "<groundRangeResolution>");
      sscanf(valStart, "<groundRangeResolution>%lf</groundRangeResolution>", 
	     &terrasar->groundRangeResolution);
    }
    if (strstr(line, "<azimuthResolution>") && check) {
      valStart = strstr(line, "<azimuthResolution>");
      sscanf(valStart, "<azimuthResolution>%lf</azimuthResolution>", 
	     &terrasar->azimuthResolution);
    }
    if (strstr(line, "</imageDataInfo>"))
      check = FALSE;
  }

  FCLOSE(fp);
  
  return terrasar;
}

void import_terrasar(const char *inBaseName, radiometry_t radiometry,
		     const char *outBaseName)
{
  FILE *fpIn, *fpOut;
  terrasar_meta *terrasar;
  meta_parameters *meta;
  char dataName[1024];
  unsigned char intValue[4];
  float *amp = NULL, *phase = NULL, re, im;
  short int shortReal, shortImaginary;
  int ii, kk;
  int asfv, aslv, rsfv, rslv;

  // Check radiometry
  if (radiometry != r_AMP &&
      radiometry != r_SIGMA && radiometry != r_SIGMA_DB) {
    asfPrintWarning("Radiometry other than AMPLITUDE and SIGMA is not "
		    "supported for ALOS mosaics.\nDefaulting back to "
		    "AMPLITUDE.\n");
    radiometry = r_AMP;
  }
  
  if (!fileExists(inBaseName))
    appendExt(inBaseName, ".xml");

  terrasar = read_terrasar_meta(inBaseName);
  asfPrintStatus("   ImageDataType: %s, imageDataFormat: %s\n\n",
		 terrasar->imageDataType, terrasar->imageDataFormat);
  meta = terrasar2meta(terrasar);
  sprintf(dataName, "%s", terrasar->imageData);
  if (strcmp_case(terrasar->imageDataType, "COMPLEX") == 0 &&
      strcmp_case(terrasar->imageDataFormat, "COSAR") == 0) {
    asfPrintStatus("Writing a two-banded image ...\n\n");
    strcpy(meta->general->bands, "AMP,PHASE");
    meta->general->band_count = 2;
  }
  else
    asfPrintStatus("Data type (%s) and data format (%s) currently not "
		   "supported!\n", 
		   terrasar->imageDataType, terrasar->imageDataFormat);

  fpIn = FOPEN(dataName, "rb");
  fpOut = FOPEN(outBaseName, "wb");

  FREAD(&intValue, 1, 4, fpIn);
  int bytes_in_burst = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);
  int range_sample_relative_index = bigInt32(intValue);;
  FREAD(&intValue, 1, 4, fpIn);
  int range_samples = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);
  int azimuth_samples = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);
  int burst_index = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);
  int rangeline_total_number_bytes = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);
  int total_number_lines = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);

  /*
  asfPrintStatus("Bytes in burst: %d, range sample relative index: %d\n", 
		 bytes_in_burst, range_sample_relative_index);
  asfPrintStatus("Range samples: %d, azimuth samples: %d\n", 
		 range_samples, azimuth_samples);
  asfPrintStatus("Burst index: %d, rangeline_total_number_bytes: %d, "
		 "total_number_lines: %d\n", burst_index, 
		 rangeline_total_number_bytes, total_number_lines);
  */

  // Check for the first and last azimuth and range pixel
  asfv = 1; // azimuth sample first valid
  FSEEK(fpIn, 2*rangeline_total_number_bytes+8, SEEK_SET);
  for (kk=2; kk<range_samples; kk++) {
    FREAD(&intValue, 1, 4, fpIn);
    if (bigInt32(intValue) > asfv)
      asfv = bigInt32(intValue);
  }
    
  aslv = azimuth_samples; // azimuth sample last valid
  FSEEK(fpIn, 3*rangeline_total_number_bytes+8, SEEK_SET);
  for (kk=2; kk<range_samples; kk++) {
    FREAD(&intValue, 1, 4, fpIn);
    if (bigInt32(intValue) < aslv)
      aslv = bigInt32(intValue);
  }

  rsfv = 1; // range sample first valid
  rslv = range_samples; // range sample last valid
  for (ii=4; ii<total_number_lines; ii++) {
    FSEEK(fpIn, 4*rangeline_total_number_bytes, SEEK_SET);
    FREAD(&intValue, 1, 4, fpIn);
    if (bigInt32(intValue) > rsfv)
      rsfv = bigInt32(intValue);
    FREAD(&intValue, 1, 4, fpIn);
    if (bigInt32(intValue) < rslv)
      rslv = bigInt32(intValue);
  }

  /*
  asfPrintStatus("ASFV: %d, ASLV: %d, RSFV: %d, RSLV: %d\n\n", 
		 asfv, aslv, rsfv, rslv);
  */

  meta->general->line_count = aslv - asfv + 1;
  meta->general->sample_count = rslv - rsfv + 1;
  amp = (float *) MALLOC(sizeof(float)*(rslv-rsfv+1));
  phase = (float *) MALLOC(sizeof(float)*(rslv-rsfv+1));

  // Read in the image
  for (ii=asfv+3; ii<aslv+3; ii++) {
    FSEEK(fpIn, ii*rangeline_total_number_bytes+8, SEEK_SET);
    for (kk=rsfv; kk<=rslv; kk++) {
      FREAD(&shortReal, 2, 1, fpIn);
      re = (float) shortReal;
      FREAD(&shortImaginary, 2, 1, fpIn);
      im = (float) shortImaginary;
      amp[kk-rsfv] = sqrt(re*re + im*im);
      phase[kk-rsfv] = atan2(im, re);
    }
    put_band_float_line(fpOut, meta, 0, ii-3, amp);
    put_band_float_line(fpOut, meta, 1, ii-3, phase);
    asfLineMeter(ii, azimuth_samples);
  }

  meta_write(meta, outBaseName);
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  if (amp)
    FREE(amp);
  if (phase)
    FREE(phase);
  FREE(terrasar);
  meta_free(meta);
}

