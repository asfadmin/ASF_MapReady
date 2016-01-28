#include "sentinel.h"
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

void import_sentinel(const char *inBaseName, const char *outBaseName)
{
  char inDataName[1024], *outDataName=NULL;
  char mission[25], beamMode[10], productType[10];
  float *amp = NULL, *phase = NULL, *tmp = NULL, re, im;
  int ii, file_count, band, sample;

  /*
  // Check radiometry
  if (radiometry != r_AMP) {
    asfPrintWarning("Radiometry other than AMPLITUDE is currently not "
		    "supported.\n");
    radiometry = r_AMP;
  }
  */
  
  check_sentinel_meta(inBaseName, mission, beamMode, productType);
  asfPrintStatus("   Mission: %s, beam mode: %s, product type: %s\n",
    mission, beamMode, productType);
  if (strcmp_case(productType, "RAW") == 0)
    asfPrintError("Product type 'RAW' currently not supported!\n");  
  else if (strcmp_case(productType, "SLC") == 0 ||
    strcmp_case(productType, "GRD") == 0) {

    // Work out how many files we need to take care of
    if (strcmp_case(productType, "GRD") == 0)
      file_count = 1;
    else { // must be SLC data
      if (strcmp_case(beamMode, "IW") == 0)
        file_count = 3;
      else if (strcmp_case(beamMode, "EW") == 0)
        file_count = 5;
      else // must be strip mode
        file_count = 1;
    }

    // Go through the files and import them
    for (ii=0; ii<file_count; ii++) {
    
      if (file_count == 1)
        outDataName = appendExt(outBaseName, ".img");
      else {
        outDataName = (char *) MALLOC(sizeof(char)*1024);
        sprintf(outDataName, "%s%d.img", outBaseName, ii+1);
      }
      sentinel_meta *sentinel = read_sentinel_meta(inBaseName, ii+1);
      meta_parameters *meta = sentinel2meta(sentinel);
      meta_write(meta, outDataName);

      // Let's check the GeoTIFF data.
      TIFF *tiff = NULL;
      GTIF *gtif = NULL;
      data_type_t data_type;
      short sample_format, bits_per_sample, planar_config;
      short num_bands;
      int is_scanline_format, is_palette_color_tiff, detected=TRUE;
  
      FILE *fp = FOPEN(outDataName, "wb");
      int band_count = meta->general->band_count;
      if (strcmp_case(sentinel->productType, "SLC") == 0)
        band_count /= 2;
      for (band=0; band<band_count; band++) {
  
        strcpy(inDataName, sentinel->data[band]);
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
          asfPrintError( "Problem with image dimensions. Was looking for %d lines "
                "and %d samples.\nFound %ld lines and %ld samples instead!\n", 
                meta->general->line_count, meta->general->sample_count, 
                tif_line_count, tif_sample_count);
  
        // Check general TIFF tags
        get_tiff_data_config(tiff, &sample_format, &bits_per_sample, &planar_config,
                 &data_type, &num_bands, &is_scanline_format, 
                 &is_palette_color_tiff, REPORT_LEVEL_WARNING);
  
        if (sample_format == SAMPLEFORMAT_UINT && bits_per_sample == 16)
          asfPrintStatus("   Found Unsigned 16 bit data\n");
        else if (sample_format == SAMPLEFORMAT_COMPLEXINT && bits_per_sample == 32) {
          asfPrintStatus("   Found Complex 16 bit data\n");
          detected = FALSE;
        }
  
        // Check the image description
        const char *imageDescription;
        TIFFGetField(tiff, TIFFTAG_IMAGEDESCRIPTION, &imageDescription);
        if (strncmp_case(imageDescription, "SENTINEL-1", 10) != 0)
          asfPrintError("GeoTIFF file (%s) does not contain Sentinel data\n", inDataName);
      
        tiff_type_t tiffInfo;
        get_tiff_type(tiff, &tiffInfo);
        if (tiffInfo.format != SCANLINE_TIFF &&
        tiffInfo.format != STRIP_TIFF    &&
        tiffInfo.format != TILED_TIFF)
          asfPrintError("Can't read the GeoTIFF file (%s). Unrecognized TIFF "
                "type!\n", sentinel->data[band]);
  
        // If we made it here, we are reasonably sure that we have the file that
        // we are looking for.
        asfPrintStatus("\n   Importing %s ...\n", sentinel->data[band]);
  
        uint32 scanlineSize = TIFFScanlineSize(tiff);
        tdata_t *tiff_buf = _TIFFmalloc(scanlineSize);
        if (!tiff_buf)
          asfPrintError("Can't allocate buffer for reading TIFF lines!\n");
  
        amp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
        phase = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  
        // Read file line by line
        uint32 row;
        uint16 *intValue;
        for (row=0; row<(uint32)meta->general->line_count; row++) {
          asfLineMeter(row, meta->general->line_count);
        switch (tiffInfo.format) 
          {
          case SCANLINE_TIFF:
            TIFFReadScanline(tiff, tiff_buf, row, 0);
            break;
          case STRIP_TIFF:
            ReadScanline_from_TIFF_Strip(tiff, tiff_buf, row, 0);
            break;
          case TILED_TIFF:
            ReadScanline_from_TIFF_TileRow(tiff, tiff_buf, row, 0);
            break;
          default:
            asfPrintError("Can't read this TIFF format!\n");
            break;
          }
          for (sample=0; sample<meta->general->sample_count; sample++) {
            switch (sample_format)
              {
              case SAMPLEFORMAT_UINT:
                re = (float)(((uint16*)tiff_buf)[sample]);
                break;
              case SAMPLEFORMAT_COMPLEXINT:
                intValue = &((uint16*)tiff_buf)[sample];
                re = (float) intValue[0];
                im = (float) intValue[1];
                break;
              }
            if (detected)
              amp[sample] = re;
            else {
              amp[sample] = sqrt(re*re + im*im);
              phase[sample] = atan2(im, re);
            }
          }
          if (detected)
            put_band_float_line(fp, meta, band, (int)row, amp);
          else {
            put_band_float_line(fp, meta, band*2, (int)row, amp);
            put_band_float_line(fp, meta, band*2+1, (int)row, phase);
          }
        }
        
        FREE(amp);
        FREE(phase);
        if (tmp)
          FREE(tmp);
        _TIFFfree(tiff_buf);
        GTIFFree(gtif);
        XTIFFClose(tiff);
      }
      FCLOSE(fp);
      meta_free(meta);
      FREE(sentinel->stVec);
      FREE(sentinel->gcp);
      for (band=0; band<255; band++)
        FREE(sentinel->data[band]);
      FREE(sentinel->data);
      FREE(sentinel);
    }
  }
  else if (strcmp_case(productType, "OCN") == 0) {
    if (strcmp_case(beamMode, "IW") == 0 ||
      strcmp_case(beamMode, "EW") == 0) {
      sentinel_meta *sentinel = read_sentinel_meta(inBaseName, 1);
      meta_parameters *meta = sentinel2meta(sentinel);
      meta_write(meta, outDataName);
      meta_free(meta);
      import_netcdf_xml(sentinel->data[0], "output.xml");
      FREE(sentinel->stVec);
      FREE(sentinel->gcp);
      for (band=0; band<255; band++)
        FREE(sentinel->data[band]);
      FREE(sentinel->data);
      FREE(sentinel);
    }
    else if (strcmp_case(beamMode, "WV") == 0)
      asfPrintError("Sentinel OCN WV data currently not supported!\n");
  }
  FREE(outDataName);
}

