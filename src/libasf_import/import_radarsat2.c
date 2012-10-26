#include "radarsat2.h"
#include "doppler.h"
#include "asf.h"
#include "asf_nan.h"
#include "asf_import.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_endian.h"
#include "dateUtil.h"
#include "xml_util.h"
#include <ctype.h>
#include "asf_tiff.h"
#include "geotiff_support.h"

void import_radarsat2(const char *inBaseName, radiometry_t radiometry,
		      const char *outBaseName, int ampOnly)
{
  FILE *fp;
  radarsat2_meta *radarsat2;
  meta_parameters *meta;
  char **inDataNames=NULL, inDataName[1024], *inMetaName=NULL;
  char *outDataName=NULL, str[512];
  float *amp = NULL, *phase = NULL, *tmp = NULL, re, im;
  int band, sample;

  // Check radiometry
  if (radiometry != r_AMP) {
    asfPrintWarning("Radiometry other than AMPLITUDE is currently not "
		    "supported.\n");
    radiometry = r_AMP;
  }
  
  if (!fileExists(inBaseName))
    inMetaName = appendExt(inBaseName, ".xml");
  else {
    inMetaName = (char *) MALLOC(sizeof(char)*1024);
    strcpy(inMetaName, inBaseName);
  }
  outDataName = appendExt(outBaseName, ".img");

  radarsat2 = read_radarsat2_meta(inMetaName);
  asfPrintStatus("   DataType: %s, ProductType: %s\n",
		 radarsat2->dataType, radarsat2->productType);
  if (strcmp_case(radarsat2->dataType, "COMPLEX") != 0)
    asfPrintError("Currently only complex data supported!\n");
  meta = radarsat2meta(radarsat2);
  create_cal_params(inMetaName, meta, REPORT_LEVEL_NONE);
  meta_write(meta, outDataName);

  // Let's check the GeoTIFF data.
  // Unfortunately, there is no identifier in the GeoTIFF that would identify
  // the data as Radarsat-2 data.
  //
  // The only thing that we can actually do is to look whether the data in the
  // GeoTIFF file fit the general bill. We can the image dimensions. The data
  // needs to have 2 bands (I and Q) and 16 bit. The citation geokey needs to
  // be the really non-descriptive "Uncorrected Satellite Data".

  TIFF *tiff = NULL;
  GTIF *gtif = NULL;
  data_type_t data_type;
  short sample_format, bits_per_sample, planar_config;
  short num_bands;
  int is_scanline_format, is_palette_color_tiff, wrong=FALSE;
  char *error_message = (char *) MALLOC(sizeof(char)*2048);

  inDataNames = extract_band_names(meta->general->basename, 
				   meta->general->band_count);
  fp = FOPEN(outDataName, "wb");

  int band_count = radarsat2->band_count;
  if (ampOnly) {
    strcpy(meta->general->bands, "AMP");
    meta->general->band_count = 1;
    band_count = 1;
  }
  for (band=0; band<band_count; band++) {

    // path from the xml (metadata) file
    char *path = get_dirname(inBaseName);
    if (strlen(path)>0) {
      strcpy(inDataName, path);
      if (inDataName[strlen(inDataName)-1] != '/')
        strcat(inDataName, "/");
    }
    else
      strcpy(inDataName, "");
    free(path);
    strcat(inDataName, inDataNames[band]);

    tiff = XTIFFOpen(inDataName, "r");
    if (!tiff)
      asfPrintError("Could not open data file (%s)\n", inDataName);
    gtif = GTIFNew(tiff);
    if (!gtif)
      asfPrintError("Could not read GeoTIFF keys from data file (%s)\n",
		    inDataName);

    // Check image dimensions
    uint32 tif_sample_count;
    uint32 tif_line_count;

    TIFFGetField(tiff, TIFFTAG_IMAGELENGTH, &tif_line_count);
    TIFFGetField(tiff, TIFFTAG_IMAGEWIDTH, &tif_sample_count);
    if ((meta->general->sample_count != tif_sample_count) ||
	(meta->general->line_count != tif_line_count))
      asfPrintError(error_message, 
		    "Problem with image dimensions. Was looking for %d lines "
		    "and %d samples.\nFound %ld lines and %ld samples instead!"
		    "\n", 
		    meta->general->line_count, meta->general->sample_count, 
		    tif_line_count, tif_sample_count);

    // Check general TIFF tags
    get_tiff_data_config(tiff, &sample_format, &bits_per_sample, &planar_config,
			 &data_type, &num_bands, &is_scanline_format, 
			 &is_palette_color_tiff, REPORT_LEVEL_WARNING);

    // The specs say the data is supposed to be unsigned but it is not.
    // Let is pass as long as we are talking about integer data here
    strcpy(error_message, "");
    if (sample_format != SAMPLEFORMAT_UINT && 
	sample_format != SAMPLEFORMAT_INT) {
      strcat(error_message, 
	     "Problem with sampling format. Was looking for integer, ");
      if (sample_format == SAMPLEFORMAT_COMPLEXIEEEFP)
	strcat(error_message, "found complex floating point instead!\n");
      else if (sample_format == SAMPLEFORMAT_COMPLEXINT)
	strcat(error_message, "found complex integer instead!\n");
      else if (sample_format == SAMPLEFORMAT_IEEEFP)
	strcat(error_message, "found floating point instead!\n");
      else if (sample_format == SAMPLEFORMAT_VOID)
	strcat(error_message, "found void instead!\n");
      wrong = TRUE;
    }
    if (bits_per_sample != 16) {
      sprintf(str, "Problem with bits per sample. Was looking for 16, found %d "
	      "instead!\n", bits_per_sample);
      strcat(error_message, str);
      wrong = TRUE;
    }
    if (data_type != INTEGER16) {
      strcat(error_message, "Problem with data type. Was looking INTEGER16, ");
      if (data_type == ASF_BYTE)
	strcat(error_message, "found BYTE instead!\n");
      else if (data_type == INTEGER32)
	strcat(error_message, "found INTEGER32 instead!\n");
      else if (data_type == REAL32)
	strcat(error_message, "found REAL32 instead!\n");
      else if (data_type == REAL64)
	strcat(error_message, "found REAL64 instead!\n");
      else if (data_type == COMPLEX_BYTE)
	strcat(error_message, "found COMPLEX_BYTE instead!\n");
      else if (data_type == COMPLEX_INTEGER16)
	strcat(error_message, "found COMPLEX_INTEGER16 instead!\n");
      else if (data_type == COMPLEX_INTEGER32)
	strcat(error_message, "found COMPLEX_INTEGER32 instead!\n");
      else if (data_type == COMPLEX_REAL32)
	strcat(error_message, "found COMPLEX_REAL32 instead!\n");
      else if (data_type == COMPLEX_REAL64)
	strcat(error_message, "found COMPLEX_REAL64 instead!\n");
      wrong = TRUE;
    }
    if (num_bands != 2) {
      sprintf(str, "Problem with number of bands. Was looking for 2, "
	      "found %d instead!\n", num_bands);
      strcat(error_message, str);
      wrong = TRUE;
    }
    if (wrong)
      asfPrintError(error_message);

    // Check GTCitationGeoKey
    char *citation = NULL;
    int citation_length, typeSize;
    tagtype_t citation_type;

    citation_length = GTIFKeyInfo(gtif, GTCitationGeoKey, &typeSize,
				  &citation_type);
    if (citation_length > 0) {
      citation = (char *) MALLOC(citation_length * typeSize);
      GTIFKeyGet(gtif, GTCitationGeoKey, citation, 0, citation_length);
      if (citation && 
	  strcmp_case(citation, "UNCORRECTED SATELLITE DATA") != 0) {
	asfPrintError("Problem with GTCitationGeoKey. Was looking for "
		      "'Uncorrected Satellite Data',\nfound '%s' instead!\n", 
		      citation);
      }
    }
    else
      asfPrintError("Problem with GTCitationGeoKey. Was looking for "
		    "'Uncorrected Satellite Data',\ndid not find any key!\n");

    tiff_type_t tiffInfo;
    get_tiff_type(tiff, &tiffInfo);
    if (tiffInfo.format != SCANLINE_TIFF &&
	tiffInfo.format != STRIP_TIFF    &&
	tiffInfo.format != TILED_TIFF)
      asfPrintError("Can't read the GeoTIFF file (%s). Unrecognized TIFF "
		    "type!\n", inDataNames[band]);

    // If we made it here, we are reasonably sure that we have the file that
    // we are looking for.
    asfPrintStatus("\n   Importing %s ...\n", inDataNames[band]);

    uint32 scanlineSize = TIFFScanlineSize(tiff);
    tdata_t *tiff_real_buf = _TIFFmalloc(scanlineSize);
    tdata_t *tiff_imag_buf = _TIFFmalloc(scanlineSize);
    if (!tiff_real_buf || !tiff_imag_buf)
      asfPrintError("Can't allocate buffer for reading TIFF lines!\n");

    amp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
    phase = (float *) MALLOC(sizeof(float)*meta->general->sample_count);

    // Check whether we need to flip the image in any fashion
    int flip_vertical = FALSE;
    if (strcmp_case(radarsat2->lineTimeOrdering, "DECREASING") == 0) {
      asfPrintStatus("   Data will be flipped vertically while ingesting!\n");
      flip_vertical = TRUE;
    }
    int flip_horizontal = FALSE;
    if (strcmp_case(radarsat2->pixelTimeOrdering, "DECREASING") == 0) {
      asfPrintStatus("   Data will be flipped horizontally while ingesting!\n");
      flip_horizontal = TRUE;
    }
    if (flip_horizontal)
      tmp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);

    // FIXME: still need to implement flipping vertically
    // Read file line by line
    uint32 row;
    int sample_count = meta->general->sample_count;
    int line_count = meta->general->line_count;
    for (row=0; row<(uint32)meta->general->line_count; row++) {
      asfLineMeter(row, meta->general->line_count);
      if (flip_vertical) {
	switch (tiffInfo.format) 
	  {
	  case SCANLINE_TIFF:
	    TIFFReadScanline(tiff, tiff_real_buf, line_count-row-1, 0);
	    TIFFReadScanline(tiff, tiff_imag_buf, line_count-row-1, 1);
	    break;
	  case STRIP_TIFF:
	    ReadScanline_from_TIFF_Strip(tiff, tiff_real_buf, 
					 line_count-row-1, 0);
	    ReadScanline_from_TIFF_Strip(tiff, tiff_imag_buf, 
					 line_count-row-1, 1);
	    break;
	  case TILED_TIFF:
	    ReadScanline_from_TIFF_TileRow(tiff, tiff_real_buf, 
					   line_count-row-1, 0);
	    ReadScanline_from_TIFF_TileRow(tiff, tiff_imag_buf, 
					   line_count-row-1, 1);
	    break;
	  default:
	    asfPrintError("Can't read this TIFF format!\n");
	    break;
	  }
      }
      else {
	switch (tiffInfo.format) 
	  {
	  case SCANLINE_TIFF:
	    TIFFReadScanline(tiff, tiff_real_buf, row, 0);
	    TIFFReadScanline(tiff, tiff_imag_buf, row, 1);
	    break;
	  case STRIP_TIFF:
	    ReadScanline_from_TIFF_Strip(tiff, tiff_real_buf, row, 0);
	    ReadScanline_from_TIFF_Strip(tiff, tiff_imag_buf, row, 1);
	    break;
	  case TILED_TIFF:
	    ReadScanline_from_TIFF_TileRow(tiff, tiff_real_buf, row, 0);
	    ReadScanline_from_TIFF_TileRow(tiff, tiff_imag_buf, row, 1);
	    break;
	  default:
	    asfPrintError("Can't read this TIFF format!\n");
	    break;
	  }
      }
      for (sample=0; sample<sample_count; sample++) {
	switch (sample_format)
	  {
	  case SAMPLEFORMAT_UINT:
	    re = (float)(((uint16*)tiff_real_buf)[sample]);
	    im = (float)(((uint16*)tiff_imag_buf)[sample]);
	    break;
	  case SAMPLEFORMAT_INT:
	    re = (float)(((int16*)tiff_real_buf)[sample]);
	    im = (float)(((int16*)tiff_imag_buf)[sample]);
	    break;
	  }
	amp[sample] = sqrt(re*re + im*im);
	phase[sample] = atan2(im, re);
      }
      if (flip_horizontal) {
	for (sample=0; sample<sample_count; sample++)
	  tmp[sample] = amp[sample];
	for (sample=0; sample<sample_count; sample++)
	  amp[sample] = tmp[sample_count-sample-1];
	for (sample=0; sample<sample_count; sample++)
	  tmp[sample] = phase[sample];
	for (sample=0; sample<sample_count; sample++)
	  phase[sample] = tmp[sample_count-sample-1];
      }
	  
      put_band_float_line(fp, meta, band*2, (int)row, amp);
      if (!ampOnly)
	put_band_float_line(fp, meta, band*2+1, (int)row, phase);
    }
      
    FREE(amp);
    FREE(phase);
    if (tmp)
      FREE(tmp);
    _TIFFfree(tiff_real_buf);
    _TIFFfree(tiff_imag_buf);
    GTIFFree(gtif);
    XTIFFClose(tiff);
  }

  // update the name field with directory name
  char *path = get_dirname(inBaseName);
  if (strlen(path)<=0)
    path = g_get_current_dir();
  char *p = path, *q = path;
  while (q) {
    if ((q = strchr(p, DIR_SEPARATOR)) != NULL)
      p = q+1;
  }
  sprintf(meta->general->basename, "%s", p);
  FREE(path);
  meta_write(meta, outDataName);

  meta_free(meta);
  FREE(radarsat2);
  FCLOSE(fp);
}

