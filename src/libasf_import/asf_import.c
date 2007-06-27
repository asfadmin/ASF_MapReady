#include <asf_contact.h>
#include <asf_license.h>
#include "geotiff_flavors.h"
#include "asf_import.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "ceos.h"
#include "decoder.h"
#include "find_geotiff_name.h"
#include "get_ceos_names.h"
#include "get_stf_names.h"
#include "asf_raster.h"
#include <ctype.h>

/*
These next few functions are used to fix scaling errors in the data that
result from the PP using an incorrect swath velocity during some of
the calculations.

Here is a summary of the fixes as described by Orion Lawlor in an e-mail
on 3/14/06:

xpix_ypix prints out these values for an ARDoP image.
> > azimuth pixel size at scene center: 3.9648920 meters   (xpix_ypix)
> > ASF geolocate azimuth velocity: 6660.144 m/s   (xpix_ypix)
> > PP swath velocity: 6626.552 m/s = ...
The velocities should be about the same for the corresponding
L1 image.

The PP L1 pixel spacing is supposed to always be 12.5m.
But because the PP miscalulates the swath velocity, L1 images actually
have a pixel spacing of:
PP spacing * real velocity / PP velocity = real spacing
12.5 * 6660.144/6626.552 = 12.5633662876 m

An ARDOP image's pixel spacing is properly computed by xpix_ypix (and
not in the "yPix" field of the .meta file!) and multilooked, so the
ARDOP image pixel spacing is really:
3.9648920 m/pix * 5-pixel ARDOP multilook = 19.8244600 m/pix

The expected L1-to-multilooked-ARDOP image scale factor is just the
ratio of the two image's pixel spacings:
19.8244600 m/pix / 12.5633662876 m/pix = 1.5779576545
*/

void fix_ypix(const char *outBaseName, double correct_y_pixel_size)
{
    asfPrintStatus("Applying y pixel size correction to the metadata...\n");

    char outMetaName[256];
    sprintf(outMetaName, "%s%s", outBaseName, TOOLS_META_EXT);

    meta_parameters *omd = meta_read(outMetaName);

    asfPrintStatus("Original y pixel size: %g\n",
        omd->general->y_pixel_size);
    asfPrintStatus("            corrected: %g\n",
        correct_y_pixel_size);

    omd->general->y_pixel_size = correct_y_pixel_size;
    meta_write(omd, outMetaName);
    meta_free(omd);
}

float get_default_azimuth_scale(const char *outBaseName)
{
    char outMetaName[256];
    sprintf(outMetaName, "%s%s", outBaseName, TOOLS_META_EXT);

    meta_parameters *omd = meta_read(outMetaName);

    float pp_velocity, corrected_velocity;
    xpyp_getVelocities(omd, &pp_velocity, &corrected_velocity);

    asfPrintStatus("       PP Velocity: %g\n", pp_velocity);
    asfPrintStatus("Corrected Velocity: %g\n", corrected_velocity);

    float qua, az_pixsiz;
    xpyp_getPixSizes(omd, &qua, &az_pixsiz);

    asfPrintStatus("      y pixel size: %g (xpix_ypix)\n", az_pixsiz);
    asfPrintStatus("      y pixel size: %g (metadata)\n",
        omd->general->y_pixel_size);

    float real_spacing =
        omd->general->y_pixel_size * corrected_velocity / pp_velocity;

    float scale = az_pixsiz / real_spacing;

    asfPrintStatus("             Scale: %g\n\n", scale);
    meta_free(omd);

    return scale;
}

float get_default_ypix(const char *outBaseName)
{
    char outMetaName[256];
    sprintf(outMetaName, "%s%s", outBaseName, TOOLS_META_EXT);

    meta_parameters *omd = meta_read(outMetaName);

    float qua, az_pixsiz;
    xpyp_getPixSizes(omd, &qua, &az_pixsiz);
    meta_free(omd);

    return az_pixsiz;
}

/******************************************************************************
* Lets rock 'n roll!
*****************************************************************************/
// Convenience wrapper, without the kludgey "flags" array
int asf_import(radiometry_t radiometry, int db_flag,
               char *format_type, char *band_id, char *image_data_type, char *lutName,
               char *prcPath, double lowerLat, double upperLat,
               double *p_range_scale, double *p_azimuth_scale,
               double *p_correct_y_pixel_size, char *inMetaNameOption,
               char *inBaseName, char *outBaseName)
{
    char **inBandName, inDataName[512]="", inMetaName[512]="";
    char unscaledBaseName[256]="", bandExt[10]="", tmp[10]="";
    int do_resample;
    int do_metadata_fix;
    double range_scale = -1, azimuth_scale = -1, correct_y_pixel_size = 0;
    int ii, kk, nBands;

    asfPrintStatus("Importing: %s\n", inBaseName);

    // Allocate memory
    inBandName = (char **) MALLOC(MAX_BANDS*sizeof(char *));
    for (ii=0; ii<MAX_BANDS; ii++)
      inBandName[ii] = (char *) MALLOC(512*sizeof(char));

    // Determine some flags
    do_resample = p_range_scale != NULL && p_azimuth_scale != NULL;
    do_metadata_fix = p_correct_y_pixel_size != NULL;

    if (p_range_scale)
        range_scale = *p_range_scale;
    if (p_azimuth_scale)
        azimuth_scale = *p_azimuth_scale;
    if (p_correct_y_pixel_size)
        correct_y_pixel_size = *p_correct_y_pixel_size;

    strcpy(unscaledBaseName, outBaseName);
    if (do_resample) {
        strcat(unscaledBaseName, "_unscaled");
    }

    /* Ingest all sorts of flavors of CEOS data */
    if (strncmp(format_type, "CEOS", 4) == 0) {
        asfPrintStatus("   Data format: %s\n\n", format_type);
        if (inMetaNameOption == NULL)
            require_ceos_pair(inBaseName, inBandName, inMetaName, &nBands);
        else {
            /* Allow the base name to be different for data & meta */
            require_ceos_data(inBaseName, inBandName, &nBands);
            require_ceos_metadata(inMetaNameOption, inMetaName);
        }
        for (ii=0; ii<nBands; ii++) {
          // Determine the band extension (band ID)
          strcpy(bandExt, "");

          // p will point to the beginning of the actual file (past the path)
          char *p = strrchr(inBandName[ii], '/');
          if (!p)
              p = inBandName[ii];
          else
              ++p;

          if (strncmp(p, "IMG-HH", 6)==0)
            strcpy(bandExt, "HH");
          if (strncmp(p, "IMG-HV", 6)==0)
            strcpy(bandExt, "HV");
          if (strncmp(p, "IMG-VH", 6)==0)
            strcpy(bandExt, "VH");
          if (strncmp(p, "IMG-VV", 6)==0)
            strcpy(bandExt, "VV");
          for (kk=1; kk<10; kk++) {
            sprintf(tmp, "IMG-%02d", kk);
            if (strncmp(p, tmp, 6)==0)
              sprintf(bandExt, "0%d", kk);
          }
          // Check to see if the user forced a band extension
          // (band_id) upon us... override the above if so
          int import_single_band = 0;
          if (band_id && strlen(band_id)) {
            // Force ii index to point at correct filename,
            // dying if it can't be found.
            char file_prefix[256];
            int found=0;

            // Be forgiving on numeric-only band IDs and let
            // the user enter digits with or without leading
            // zeros, e.g. 01 or 1 ...they are equivalent
            int iBandNo = atoi(band_id);
            if (iBandNo > 0 && iBandNo <= MAX_BANDS) {
              // band_id appears to be a valid numeric band number
              int alpha = 0;
              char *s = band_id;
              while (*s != '\0') {
                if (isalpha((int)*s)) alpha = 1;
                s++;
              }
              if (!alpha)
                sprintf(band_id, "%02d", iBandNo);
            }
            sprintf(file_prefix, "IMG-%s-", band_id);
            ii = 0;
            do {
              if (strncmp(inBandName[ii], file_prefix, strlen(file_prefix)) == 0)
                found = 1;
              ii++;
              if (ii > nBands)
                asfPrintError("Expected ALOS-type, CEOS-formatted, multi-band data and\n"
                    "selected band (\"%s\") file was not found \"%s\"\n", band_id,
                    strcat(file_prefix, inBaseName));
            } while (!found);
            if (found) {
              ii--;
              import_single_band = 1;
              strcpy(bandExt, band_id);
            }
          }

          // Time to rock...
          asfPrintStatus("   File: %s\n", inBandName[ii]);
          import_ceos(inBandName[ii], bandExt,
                      import_single_band ? 1 : ii+1, nBands, inBaseName, lutName,
                      unscaledBaseName, radiometry, db_flag, import_single_band);
          if (import_single_band) nBands = 1;
          asfPrintStatus("\n");
        }
    }
    /* Ingest ENVI format data */
    else if (strncmp(format_type, "ENVI", 4) == 0) {
        asfPrintStatus("   Data format: %s\n", format_type);
        import_envi(inDataName, inMetaName, unscaledBaseName);
    }
    /* Ingest ESRI format data */
    else if (strncmp(format_type, "ESRI", 4) == 0) {
        asfPrintStatus("   Data format: %s\n", format_type);
        import_esri(inDataName, inMetaName, unscaledBaseName);
    }
    /* Ingest Vexcel Sky Telemetry Format (STF) data */
    else if (strncmp(format_type, "STF", 3) == 0) {
        asfPrintStatus("   Data format: %s\n", format_type);
        if (inMetaNameOption == NULL)
            require_stf_pair(inBaseName, inDataName, inMetaName);
        else {
            /* Allow the base name to be different for data & meta */
            require_stf_data(inBaseName, inDataName);
            require_stf_metadata(inMetaNameOption, inMetaName);
        }
        int lat_constrained = upperLat != -99 && lowerLat != -99;
        import_stf(inDataName, inMetaName, unscaledBaseName, radiometry,
                   lat_constrained, lowerLat, upperLat, prcPath);
    }
    else if ( strncmp (format_type, "GEOTIFF", 7) == 0 ) {
      asfPrintStatus("   Data format: %s\n", format_type);
      if (band_id != NULL &&
          strlen(band_id) > 0 &&
          strncmp(uc(band_id), "ALL", 3) != 0)
      {
        asfPrintWarning("The -band option is not supported for data files containing\n"
                        "multiple bands within a single file (such as GeoTIFF or TIFF)\n"
                        "rather than in individual band files (such as ALOS etc).\n"
                        "\nThe import will continue, but all available bands will be\n"
                        "imported into a single ASF-format file.  You may select any\n"
                        "individual band for export however.\n");
      }
      char *ext = findExt(inBaseName);
      if (ext != NULL) {
        *ext = '\0';
      }
      GString *inGeotiffName = find_geotiff_name (inBaseName);
      if ( inGeotiffName == NULL ) {
        asfPrintError ("Couldn't find a GeoTIFF file (i.e. a file with "
                       "extension '.tif', '.tiff',\n'.TIF', or '.TIFF') "
                       "corresponding to specified inBaseName:\n"
                       "%s\n", inBaseName);
      }
      geotiff_importer importer = detect_geotiff_flavor (inGeotiffName->str);
      if ( importer != NULL ) {
        if (importer == import_generic_geotiff    &&
            strlen(image_data_type)               &&
            strncmp(image_data_type, "???", 3) != 0
           )
        {
          importer (inGeotiffName->str, outBaseName, image_data_type);
        }
        else {
          importer (inGeotiffName->str, outBaseName);
        }
      }
    }
    else if (strncmp(format_type, "BIL", 3) == 0) {
      asfPrintStatus("   Data format: %s\n", format_type);
      import_bil(inBaseName, outBaseName);
    }
    else if (strncmp(format_type, "GRIDFLOAT", 3) == 0) {
      asfPrintStatus("   Data format: %s\n", format_type);
      import_gridfloat(inBaseName, outBaseName);
    }
    /* Don't recognize this data format; report & quit */
    else {
        asfPrintError("Unrecognized data format: '%s'\n",format_type);
    }

    /* resample, if necessary */
    if (do_resample)
    {
        if (range_scale < 0)
            range_scale = DEFAULT_RANGE_SCALE;

        if (azimuth_scale < 0)
            azimuth_scale = get_default_azimuth_scale(unscaledBaseName);

        asfPrintStatus("Resampling with scale factors: "
                       "%lf range, %lf azimuth.\n",
            range_scale, azimuth_scale);

        resample_nometa(unscaledBaseName, outBaseName,
                        range_scale, azimuth_scale);

        asfPrintStatus("Done.\n");
    }

    /* metadata pixel size fix, if necessary */
    if (do_metadata_fix)
    {
        if (correct_y_pixel_size < 0)
            correct_y_pixel_size = get_default_ypix(outBaseName);

        fix_ypix(outBaseName, correct_y_pixel_size);
    }

    /* If the user asked for sprocket layers, create sprocket data layers
    now that we've got asf tools format data */
    //if (flags[f_SPROCKET] != FLAG_NOT_SET) {
    //    create_sprocket_layers(outBaseName, inMetaName);
    //}

    for (ii=0; ii<MAX_BANDS; ii++)
      FREE(inBandName[ii]);
    FREE(inBandName);

    asfPrintStatus("Import complete.\n");
    return 0;
}
