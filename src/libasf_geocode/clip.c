#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_geocode.h"

#define VERSION 1.0
#define MINI(a,b) (((a)<(b))?(a):(b))
#define MAXI(a,b) (((a)>(b))?(a):(b))

static int proj_params_match(meta_parameters *m1, meta_parameters *m2)
{
    // these cases actualy should have already been handled
    if (!m1->projection || !m2->projection)
        return FALSE;

    if (m1->projection->type != m2->projection->type)
        return FALSE;

    project_parameters_t pp1 = m1->projection->param;
    project_parameters_t pp2 = m2->projection->param;

    switch (m1->projection->type)
    {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
        return pp1.utm.zone == pp2.utm.zone;

    case POLAR_STEREOGRAPHIC:
        return
            pp1.ps.slat == pp2.ps.slat &&
            pp1.ps.slon == pp2.ps.slon &&
            pp1.ps.is_north_pole == pp2.ps.is_north_pole;
        break;

    case ALBERS_EQUAL_AREA:
        return
            pp1.albers.std_parallel1 == pp2.albers.std_parallel1 &&
            pp1.albers.std_parallel2 == pp2.albers.std_parallel2 &&
            pp1.albers.center_meridian == pp2.albers.center_meridian &&
            pp1.albers.orig_latitude == pp2.albers.orig_latitude;
        break;

    case LAMBERT_CONFORMAL_CONIC:
        return
            pp1.lamcc.plat1 == pp2.lamcc.plat1 &&
            pp1.lamcc.plat2 == pp2.lamcc.plat2 &&
            pp1.lamcc.lon0 == pp2.lamcc.lon0 &&
            pp1.lamcc.lat0 == pp2.lamcc.lat0;
        break;

    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
        return
            pp1.lamaz.center_lat == pp2.lamaz.center_lat &&
            pp1.lamaz.center_lon == pp2.lamaz.center_lon;
        break;

    default:
        return FALSE;
    }
}


int clip(char *inFile, char *maskFile, char *outFile)
{
  meta_parameters *metaIn, *metaMask;
  metaIn = meta_read(inFile);
  metaMask = meta_read(maskFile);

  // Check whether mask file looks legitimate. The only indication that we
  // have is that it should be BYTE and have one band.
  if (metaMask->general->data_type != BYTE)
    asfPrintStatus("Mask image does not have data type 'BYTE'!\n");
  if (metaMask->general->band_count != 1)
    asfPrintStatus("Mask image should have only one band!\n");

  // Check whether input and mask file have the same projection parameters
  if (metaIn->projection && metaMask->projection) {

    // Create temporary processing directory (move outside this loop once
    // we have non-projected case covered)
    char *tmpDir = (char *) MALLOC(sizeof(char)*(strlen(outFile)+25));
    sprintf(tmpDir, "%s-", outFile);
    strcat(tmpDir, time_stamp_dir());
    create_clean_dir(tmpDir);
    char *mask = (char *) MALLOC(sizeof(char)*(strlen(tmpDir)+20));
    sprintf(mask, "%s/mask", tmpDir);
    
    // Check whether mask needs to be re-projected
    if (metaIn->projection->type != metaMask->projection->type) {
      asfPrintWarning("Mask needs to be re-projected!\n");
      project_parameters_t *pp = 
	(project_parameters_t *) MALLOC(sizeof(project_parameters_t));
      *pp = metaIn->projection->param;
      /*
      asf_geocode(pp, metaIn->projection->type, FALSE, 
		  RESAMPLE_NEAREST_NEIGHBOR, metaIn->projection->height, 
		  metaIn->projection->datum, metaIn->projection->perX, 
		  NULL, inFile, mask, metaIn->general->no_data, FALSE);
      */
      FREE(pp);
    }
    else {
      // Check whether mask needs to be resampled
      if ((metaIn->projection->perX != metaMask->projection->perX ||
	   metaIn->projection->perY != metaMask->projection->perY) &&
	  proj_params_match(metaIn, metaMask)) {
	asfPrintWarning("Mask needs to be resampled!\n");
	resample(inFile, mask, metaIn->projection->perX, 
		 fabs(metaIn->projection->perY));
      }
      else if (!proj_params_match(metaIn, metaMask)) {
	asfPrintWarning("Mask needs to be re-projected!\n");
	project_parameters_t *pp = 
	  (project_parameters_t *) MALLOC(sizeof(project_parameters_t));
	*pp = metaIn->projection->param;
	/*
	asf_geocode(pp, metaIn->projection->type, FALSE, 
		    RESAMPLE_NEAREST_NEIGHBOR, metaIn->projection->height, 
		    metaIn->projection->datum, metaIn->projection->perX, 
		    NULL, inFile, mask, metaIn->general->no_data, FALSE);
	*/
	FREE(pp);
      }
      else 
	copyImgAndMeta(maskFile, mask);
    }
    meta_free(metaMask);

    // Now we should have matching projections in both input files
    // Let's figure out the overlapping part of the two input files
    metaMask = meta_read(mask);
    int nl = metaMask->general->line_count;
    int ns = metaMask->general->sample_count;
    
    int startLine = (int)
      ((metaMask->projection->startY - metaIn->projection->startY + 0.5) /
       metaMask->projection->perY) - metaMask->general->start_line;
    int startSample = (int)
      ((metaMask->projection->startX - metaIn->projection->startX + 0.5) /
       metaMask->projection->perX) - metaMask->general->start_sample;
    int endLine = startLine + metaMask->general->line_count;
    int endSample = startSample + metaMask->general->sample_count;
    double coverage = 
      (endLine-startLine) * (endSample-startSample) * 100.0 / (ns*nl);
    printf("startLine: %i, startSample: %i\n", startLine, startSample);
    printf("endLine: %i, endSample: %i\n", endLine, endSample);
    printf("Converage: %.1lf %%\n", coverage);

    // Fail when there is no overlap
    if (startLine > metaIn->general->line_count || endLine < 0 ||
	startSample > metaIn->general->sample_count || endSample < 0) {
      asfPrintStatus("Mask image does not cover the input image!\n");
      return (1);
    }

    // Setup files and memory
    char *inImg = appendExt(inFile, ".img");
    char *maskImg = appendExt(maskFile, ".img");
    char *outImg = appendExt(outFile, ".img");
    float *inBuf = 
      (float *) MALLOC(sizeof(float)*metaIn->general->sample_count);
    unsigned char *maskBuf = (unsigned char *) MALLOC(sizeof(char)*ns);
    float *outBuf = (float *) MALLOC(sizeof(float)*ns);
    char **band_name = extract_band_names(metaIn->general->bands,
					  metaIn->general->band_count);
    FILE *fpIn = FOPEN(inImg, "rb");
    FILE *fpMask = FOPEN(maskImg, "rb");
    FILE *fpOut = FOPEN(outImg, "wb");

    // Write metadata for output
    meta_parameters *metaOut = meta_read(maskFile);
    metaOut->general->band_count = metaIn->general->band_count;
    metaOut->general->data_type = metaIn->general->data_type;
    meta_write(metaOut, outFile);

    // Rock and roll
    int ii, jj, kk;
    for (ii=0; ii<nl; ii++) {
      get_byte_line(fpMask, metaMask, ii, maskBuf);
      for (kk=0; kk<metaIn->general->band_count; kk++) {
	if ((startLine+ii) >= 0 && ii < endLine)
	  get_band_float_line(fpIn, metaIn, kk, startLine+ii, inBuf);
	else
	  for (jj=0; jj<ns; jj++)
	    inBuf[jj] = 0.0;
	for (jj=0; jj<ns; jj++) {
	  if (maskBuf[jj] == 0 || inBuf[startSample+jj] == 0 ||
	      (startSample+jj) < 0 || jj > endSample)
	    outBuf[jj] = metaIn->general->no_data;
	  else
	    outBuf[jj] = inBuf[startSample+jj]*maskBuf[jj];
	}
	put_band_float_line(fpOut, metaOut, kk, ii, outBuf);
      }
      asfLineMeter(ii, nl);
    }
    FCLOSE(fpIn);
    FCLOSE(fpMask);
    FCLOSE(fpOut);

    // Clean up
    for (ii=0; ii<metaIn->general->band_count; ii++)
      FREE(band_name[ii]);
    FREE(band_name);
    FREE(inBuf);
    FREE(maskBuf);
    FREE(outBuf);
    meta_free(metaIn);
    meta_free(metaMask);
    meta_free(metaOut);
    remove_dir(tmpDir);
    FREE(tmpDir);
  }
  else
    asfPrintError("Non-projected case not covered yet!\n");

  return 0;
}
