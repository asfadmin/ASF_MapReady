#include "asf.h"
#include "asf_import.h"
#include "envi.h"

#define ENVI_UTM     3
#define ENVI_LAMCC   4
#define ENVI_ALBERS  9
#define ENVI_LAMAZ  11
#define ENVI_PS     31


/******************************************************************************
 * Import the ENVI format into our ASF Tools file format */
void import_envi(char *inDataName, char *inMetaName, char *outBaseName)
{
  char line[256]="", key[25]="", value[25]="", bla[25];
  char outDataName[256], outMetaName[256];
  char *map_info_ptr=NULL, map_info[256]="";
  char *proj_info_ptr, proj_info[256]="";
  int projection_key;
  double fTmp1, fTmp2;
  FILE *fp;
  meta_parameters *meta=NULL;
  envi_header *envi=NULL;

  asfPrintWarning("ENVI Import not completely tested!\n");

  /* Handle output file name */
  strcpy(outDataName,outBaseName);
  strcat(outDataName,TOOLS_IMAGE_EXT);
  strcpy(outMetaName,outBaseName);
  strcat(outMetaName,TOOLS_IMAGE_EXT);

  /* Allocate memory for ESRI header structure */
  envi = (envi_header *)MALLOC(sizeof(envi_header));

  /* Read .hdr and fill meta structures */
  fp = FOPEN(inMetaName, "r");
  while (NULL != fgets(line, 255, fp)) {
    sscanf(line, "%s = %s", key, value);
    if (strncmp(key, "samples", 6)==0) envi->samples = atoi(value);
    else if (strncmp(key, "lines", 5)==0) envi->lines = atoi(value);
    else if (strncmp(key, "bands", 5)==0) envi->bands = atoi(value);
    else if (strncmp(key, "header", 6)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "offset", 6)==0)
        envi->header_offset = atoi(value);
    }
    /*** ignore file type for the moment ***/
    else if (strncmp(key, "data", 4)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "type", 4)==0)
        envi->data_type = atoi(value);
    }
    else if (strncmp(key, "interleave", 10)==0)
      sprintf(envi->interleave, "%s", value);
    else if (strncmp(key, "sensor", 6)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "type", 4)==0)
        sprintf(envi->sensor_type, "%s", value);
    }
    else if (strncmp(key, "byte", 4)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "order", 5)==0)
        envi->byte_order = atoi(value);
    }
    else if (strncmp(key, "map", 3)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "info", 4)==0) {
        map_info_ptr = strstr(line, ",");
        sprintf(map_info, "%s", map_info_ptr);
      }
    }
    else if (strncmp(key, "projection", 10)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "info", 4)==0) {
        proj_info_ptr = strstr(line, ",");
        sprintf(proj_info, "%s", proj_info_ptr);
        sscanf(value, "{%i,", &projection_key);
      }
    }
    else if (strncmp(key, "wavelength", 10)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "units", 5)==0)
        sprintf(envi->wavelength_units, "%s", value);
    }
    /*** ignore wavelength for the moment ***/
    /*** ignore data ignore for the moment ***/
    /*** ignore default stretch for the moment ***/
  }
  FCLOSE(fp);

  switch(projection_key)
    {
    case ENVI_UTM:
      sprintf(envi->projection, "UTM");
      sscanf(map_info, ", %i %i %lf %lf %lf %lf %i %s",
             &envi->ref_pixel_x, &envi->ref_pixel_y,
             &envi->pixel_easting, &envi->pixel_northing,
             &envi->proj_dist_x, &envi->proj_dist_y,
             &envi->projection_zone, envi->hemisphere);
      sscanf(proj_info, ", %lf, %lf, %lf, %lf, %s}",
             &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
             &envi->center_lon, bla);
      break;
    case ENVI_LAMCC:
      sprintf(envi->projection, "Lambert Conformal Conic");
      sscanf(map_info, ", %i, %i, %lf, %lf, %lf, %lf, %s}",
             &envi->ref_pixel_x, &envi->ref_pixel_y,
             &envi->pixel_easting, &envi->pixel_northing,
             &envi->proj_dist_x, &envi->proj_dist_y,
             envi->hemisphere);
      sscanf(proj_info, ", %lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf, %s}",
             &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
             &envi->center_lon, &fTmp1, &fTmp2, &envi->standard_parallel1,
             &envi->standard_parallel2, bla);
      break;
    case ENVI_ALBERS:
      sprintf(envi->projection, "Albers Conical Equal Area");
      sscanf(map_info, ", %i, %i, %lf, %lf, %lf, %lf, %s}",
             &envi->ref_pixel_x, &envi->ref_pixel_y,
             &envi->pixel_easting, &envi->pixel_northing,
             &envi->proj_dist_x, &envi->proj_dist_y,
             envi->hemisphere);
      sscanf(proj_info, ", %lf, %lf, %lf, %lf, %lf %lf %lf %lf%s}",
             &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
             &envi->center_lon, &fTmp1, &fTmp2, &envi->standard_parallel1,
             &envi->standard_parallel2, bla);
      break;
    case ENVI_LAMAZ:
      sprintf(envi->projection, "Lambert Azimuthal Equal Area");
      sscanf(map_info, ", %i, %i, %lf, %lf, %lf, %lf, %s}",
             &envi->ref_pixel_x, &envi->ref_pixel_y,
             &envi->pixel_easting, &envi->pixel_northing,
             &envi->proj_dist_x, &envi->proj_dist_y,
             envi->hemisphere);
      sscanf(proj_info, ", %lf, %lf, %lf, %lf, %s}",
             &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
             &envi->center_lon, bla);
      break;
    case ENVI_PS:
      sprintf(envi->projection, "Polar Stereographic");
      sscanf(map_info, ", %d, %d, %lf, %lf, %lf, %lf, %s}",
             &envi->ref_pixel_x, &envi->ref_pixel_y,
             &envi->pixel_easting, &envi->pixel_northing,
             &envi->proj_dist_x, &envi->proj_dist_y,
             envi->hemisphere);
      sscanf(proj_info, ", %lf, %lf, %lf, %lf, %s}",
             &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
             &envi->center_lon, bla);
      break;
    default:
      asfPrintError("Unsupported map projection\n");
      break;
    }

  /* Fill metadata structure with valid data */
  meta = envi2meta(envi);

  /* Write metadata file */
  meta_write(meta,outMetaName);

  /* Write data file - currently no header, so just copying generic binary */
  fileCopy(inDataName, outDataName);

  /* Clean and report */
  meta_free(meta);
  asfPrintStatus("   Converted ENVI file (%s) to ASF internal file (%s)\n\n",
                 inDataName, outDataName);
}
