#include <assert.h>

#include "asf_meta.h"
#include "envi.h"
#include "asf_nan.h"
#include "time.h"

envi_header* meta2envi(meta_parameters *meta)
{
  envi_header *envi;

  /* Allocate memory for ENVI header */
  envi = (envi_header *)MALLOC(sizeof(envi_header));

  /* Initialize the values */
  envi->samples = MAGIC_UNSET_INT;
  envi->lines = MAGIC_UNSET_INT;
  envi->bands = 1;
  envi->header_offset = 0; /* Currently only generic binary files supported */
  sprintf(envi->file_type, "ENVI standard"); /* check !!! */
  envi->data_type = 1; /* 8-bit byte as default */
  sprintf(envi->interleave, "bsq"); /* Taken as the default value for now,
                                       since we don't have multiband imagery */
  sprintf(envi->sensor_type, "Unknown");
  envi->byte_order = 1; /* big endian data */
  envi->ref_pixel_x = 0;
  envi->ref_pixel_y = 0;
  envi->pixel_easting = MAGIC_UNSET_DOUBLE;
  envi->pixel_northing = MAGIC_UNSET_DOUBLE;
  envi->proj_dist_x = MAGIC_UNSET_DOUBLE;
  envi->proj_dist_y = MAGIC_UNSET_DOUBLE;
  sprintf(envi->hemisphere, "%s", MAGIC_UNSET_STRING);
  sprintf(envi->projection, "%s", MAGIC_UNSET_STRING);
  envi->standard_parallel1 = MAGIC_UNSET_DOUBLE;
  envi->standard_parallel2 = MAGIC_UNSET_DOUBLE;
  envi->center_lat = MAGIC_UNSET_DOUBLE;
  envi->center_lon = MAGIC_UNSET_DOUBLE;
  sprintf(envi->wavelength_units, "Unknown");
  envi->wavelength = MAGIC_UNSET_DOUBLE;
  envi->data_ignore = MAGIC_UNSET_DOUBLE;
  envi->pixel_size_x = MAGIC_UNSET_DOUBLE;
  envi->pixel_size_y = MAGIC_UNSET_DOUBLE;
  sprintf(envi->default_stretch, "%s", MAGIC_UNSET_STRING);

  /* Fill the values in from the metadata */
  envi->samples = meta->general->sample_count;
  envi->lines = meta->general->line_count;
  envi->bands = meta->general->band_count;
  switch (meta->general->data_type) 
    {
    case BYTE: envi->data_type = 1; break;
    case INTEGER16: envi->data_type = 2; break;
    case INTEGER32: envi->data_type = 3; break;
    case REAL32: envi->data_type = 4; break;
    case REAL64: envi->data_type = 5; break;
    case COMPLEX_REAL32: envi->data_type = 6; break;
    default: 
      sprintf(errbuf,"\n   ERROR: Unsupported data type\n\n");
      printErr(errbuf);
      break;
    }
  if (strcmp(meta->general->sensor, "RSAT-1")==0)
    sprintf(envi->sensor_type, "RADARSAT");
  else if (strncmp(meta->general->sensor, "ERS", 3)==0)
    sprintf(envi->sensor_type, "%s", meta->general->sensor);
  else if (strcmp(meta->general->sensor, "JERS-1")==0)
    sprintf(envi->sensor_type, "%s", meta->general->sensor);
  // All the data we generate now is big_endian by default
  envi->byte_order = 1;
  if (meta->projection && meta->projection->type != LAT_LONG_PSEUDO_PROJECTION)
  {
    switch (meta->projection->type)
      {
      case UNIVERSAL_TRANSVERSE_MERCATOR: 
	sprintf(envi->projection, "UTM"); 
	envi->projection_zone = meta->projection->param.utm.zone;
	envi->center_lat = meta->projection->param.utm.lat0;
	envi->center_lon = meta->projection->param.utm.lon0;
	break;
      case POLAR_STEREOGRAPHIC:
	sprintf(envi->projection, "Polar Stereographic"); 
	envi->center_lat = meta->projection->param.ps.slat;
	envi->center_lon = meta->projection->param.ps.slon;
	break;
      case ALBERS_EQUAL_AREA:
	sprintf(envi->projection, "Albers Conical Equal Area"); 
	envi->standard_parallel1 = meta->projection->param.albers.std_parallel1;
	envi->standard_parallel2 = meta->projection->param.albers.std_parallel2;
	envi->center_lat = meta->projection->param.albers.orig_latitude;
	envi->center_lon = meta->projection->param.albers.center_meridian;
	break;
      case LAMBERT_CONFORMAL_CONIC: 
	sprintf(envi->projection, "Lambert Conformal Conic"); 
	envi->standard_parallel1 = meta->projection->param.lamcc.plat1;
	envi->standard_parallel2 = meta->projection->param.lamcc.plat2;
	envi->center_lat = meta->projection->param.lamcc.lat0;
	envi->center_lon = meta->projection->param.lamcc.lon0;
	break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA: 
	sprintf(envi->projection, "Lambert Azimuthal Equal Area");
	envi->center_lat = meta->projection->param.lamaz.center_lat;
	envi->center_lon = meta->projection->param.lamaz.center_lon;
	break;
      case STATE_PLANE: 
      case SCANSAR_PROJECTION: 
	break;
      case LAT_LONG_PSEUDO_PROJECTION:
      case UNKNOWN_PROJECTION:
	// I haven't tested this at all.
	assert (0);
	break;
      }
    envi->ref_pixel_y = 1;
    envi->ref_pixel_x = 1;
    envi->pixel_easting = meta->projection->startX;
    envi->pixel_northing = meta->projection->startY;
    envi->proj_dist_x = meta->projection->perX;
    envi->proj_dist_y = fabs(meta->projection->perY);
    if (strcmp(envi->projection,"UTM") != 0)
      envi->projection_zone = 0;
    if (meta->projection->hem == 'N') 
      sprintf(envi->hemisphere, "North");
    else if (meta->projection->hem == 'S')
      sprintf(envi->hemisphere, "South");
    envi->semimajor_axis = meta->projection->re_major;
    envi->semiminor_axis = meta->projection->re_minor;
  }
  if (meta->sar) {
    envi->wavelength = meta->sar->wavelength;
    sprintf(envi->wavelength_units, "meters");
  }
  envi->pixel_size_x = meta->general->x_pixel_size;
  envi->pixel_size_y = meta->general->y_pixel_size;

  return envi;
}

meta_parameters* envi2meta(envi_header *envi)
{
  meta_parameters *meta;

  /* Allocate memory for metadata structure */
  meta = raw_init();

  /* Fill metadata with valid ENVI header data */
  meta->general->line_count = envi->lines;
  meta->general->sample_count = envi->samples;
  meta->general->band_count = envi->bands;

  switch (envi->data_type)
    {
    case 1: meta->general->data_type = BYTE; break;
    case 2: meta->general->data_type = INTEGER16; break;
    case 3: meta->general->data_type = INTEGER32; break;
    case 4: meta->general->data_type = REAL32; break;
    case 5: meta->general->data_type = REAL64; break;
    case 6: meta->general->data_type = COMPLEX_REAL32; break;
    default:
      sprintf(errbuf,"\n   ERROR: Unsupported data type\n\n");
      printErr(errbuf);
      break;
    }

  if (strncmp(envi->sensor_type, "RADARSAT", 8)==0)
    sprintf(meta->general->sensor, "RSAT-1");
  else 
    sprintf(meta->general->sensor, "%s", envi->sensor_type);
  if (envi->byte_order == 0) sprintf(meta->general->system, "lil_ieee");
  else if (envi->byte_order == 1) sprintf(meta->general->system, "big_ieee");

  if (!meta->projection) meta->projection = meta_projection_init();

  if (strncmp(envi->projection, "UTM", 3)==0) {
    meta->projection->type = UNIVERSAL_TRANSVERSE_MERCATOR;
    meta->projection->param.utm.zone = envi->projection_zone;
    meta->projection->param.utm.lat0 =envi->center_lat;
    meta->projection->param.utm.lon0 = envi->center_lon;
    meta->projection->param.utm.false_easting = envi->ref_pixel_x;
    meta->projection->param.utm.false_northing = envi->ref_pixel_y;
  }
  else if (strncmp(envi->projection, "Polar Stereographic", 18)==0) {
    meta->projection->type = POLAR_STEREOGRAPHIC;
    meta->projection->param.ps.slat = envi->center_lat;
    meta->projection->param.ps.slon = envi->center_lon;
    meta->projection->param.ps.false_easting = envi->ref_pixel_x;
    meta->projection->param.ps.false_northing = envi->ref_pixel_y;
  }
  else if (strncmp(envi->projection, "Albers Conical Equal Area", 25)==0) {
    meta->projection->param.albers.std_parallel1 = envi->standard_parallel1;
    meta->projection->param.albers.std_parallel2 = envi->standard_parallel2;
    meta->projection->param.albers.center_meridian = envi->center_lat;
    meta->projection->param.albers.orig_latitude = envi->center_lon;
    meta->projection->param.albers.false_easting = envi->ref_pixel_x;
    meta->projection->param.albers.false_northing = envi->ref_pixel_y;
  }
  else if (strncmp(envi->projection, "Lambert Conformal Conic", 23)==0) {
    meta->projection->param.lamcc.plat1 = envi->standard_parallel1;
    meta->projection->param.lamcc.plat2 = envi->standard_parallel2;
    meta->projection->param.lamcc.lat0 = envi->center_lat;
    meta->projection->param.lamcc.lon0 = envi->center_lon;
    meta->projection->param.lamcc.false_easting = envi->ref_pixel_x;
    meta->projection->param.lamcc.false_northing = envi->ref_pixel_y;
  } 
  else if (strncmp(envi->projection, "Lambert Azimuthal Equal Area", 28)==0) {
    meta->projection->param.lamaz.center_lat = envi->center_lat;
    meta->projection->param.lamaz.center_lon = envi->center_lon;
    meta->projection->param.lamaz.false_easting = envi->ref_pixel_x;
    meta->projection->param.lamaz.false_northing = envi->ref_pixel_y;
  }
  else {
    sprintf(errbuf,"\n   ERROR: Unsupported projection type\n\n");
    printErr(errbuf);
  }

  meta->projection->startX = envi->pixel_easting;
  meta->projection->startY = envi->pixel_northing;
  meta->projection->perX = envi->proj_dist_x;
  meta->projection->perY = envi->proj_dist_y;
  if (strncmp(envi->hemisphere, "North", 5)==0)
    meta->projection->hem = 'N';
  else if (strncmp(envi->hemisphere, "South", 5)==0)
    meta->projection->hem = 'S';
  meta->projection->re_major = envi->semimajor_axis;
  meta->projection->re_minor = envi->semiminor_axis;
  sprintf(meta->projection->units, "meters");
  if (meta->sar)
    meta->sar->wavelength = envi->wavelength;
  meta->general->x_pixel_size = envi->pixel_size_x;
  meta->general->y_pixel_size = envi->pixel_size_y;

  return meta;
}

// Write ENVI header file
void write_envi_header(const char *inFile, meta_parameters *meta, 
		       envi_header *envi)
{
  FILE *fp;
  time_t t;
  char t_stamp[15];

  t = time(NULL);
  strftime(t_stamp, 12, "%d-%b-%Y", localtime(&t));

  fp = FOPEN(inFile, "w");
  fprintf(fp, "ENVI\n");
  fprintf(fp, "description = {\n"
	      "  Created by meta2envi (%s)}\n", t_stamp);
  fprintf(fp, "samples = %i\n", envi->samples);
  fprintf(fp, "lines = %i\n", envi->lines);
  fprintf(fp, "bands = %i\n", envi->bands);
  fprintf(fp, "header offset = %i\n", envi->header_offset);
  fprintf(fp, "file type = %s\n", envi->file_type);
  fprintf(fp, "data type = %i\n", envi->data_type);
  fprintf(fp, "interleave = %s\n", envi->interleave);
  fprintf(fp, "sensor type = %s\n", envi->sensor_type);
  fprintf(fp, "byte order = %i\n", envi->byte_order);
  if (meta->projection) {
    switch (meta->projection->type)
      {
      case UNIVERSAL_TRANSVERSE_MERCATOR:
	fprintf(fp, 
		"map info = {%s, %i, %i, %.3f, %.3f, %.3f, %.3f, %i, %s}\n", 
		envi->projection, envi->ref_pixel_x, envi->ref_pixel_y, 
		envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
		envi->proj_dist_y, envi->projection_zone, envi->hemisphere);
	fprintf(fp, 
		"projection info = {3, %.3f, %.3f, %.4f, %.4f, " 
                "0.0, 0.0, 0.99996, %s}\n", 
		envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
		envi->center_lon, envi->projection);
	break;
      case POLAR_STEREOGRAPHIC:
	fprintf(fp, 
		"map info = {%s, %i, %i, %.3f, %.3f, %.3f, %.3f, %s}\n", 
		envi->projection, envi->ref_pixel_x, envi->ref_pixel_y, 
		envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
		envi->proj_dist_y, envi->hemisphere);
	fprintf(fp, 
		"projection info = {31, %.3f, %.3f, %.4f, %.4f, " 
                "0.0, 0.0, %s}\n", 
		envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
		envi->center_lon, envi->projection);
	break;
      case ALBERS_EQUAL_AREA:
	fprintf(fp, 
		"map info = {%s, %i, %i  , %.3f, %.3f, %.3f, %.3f, %s}\n", 
		envi->projection, envi->ref_pixel_x, envi->ref_pixel_y, 
		envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
		envi->proj_dist_y, envi->hemisphere);
	fprintf(fp, 
		"projection info = {9, %.3f, %.3f, %.4f, %.4f, " 
                "0.0, 0.0, %.4f, %.4f, %s}\n", 
		envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
		envi->center_lon, envi->standard_parallel1, 
		envi->standard_parallel2, envi->projection);
	break;
      case LAMBERT_CONFORMAL_CONIC:
	fprintf(fp, 
		"map info = {%s, %i, %i, %.3f, %.3f, %.3f, %.3f, %s}\n", 
		envi->projection, envi->ref_pixel_x, envi->ref_pixel_y, 
		envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
		envi->proj_dist_y, envi->hemisphere);
	fprintf(fp, 
		"projection info = {4, %.3f, %.3f, %.4f, %.4f, " 
                "0.0, 0.0, %.4f, %.4f, %s}\n", 
		envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
		envi->center_lon, envi->standard_parallel1,
		envi->standard_parallel2, envi->projection);
	break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	fprintf(fp, 
		"map info = {%s, %i, %i, %.3f, %.3f, %.3f, %.3f, %s}\n", 
		envi->projection, envi->ref_pixel_x, envi->ref_pixel_y, 
		envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
		envi->proj_dist_y, envi->hemisphere);
	fprintf(fp, 
		"projection info = {11, %.3f, %.3f, %.4f, %.4f, " 
                "0.0, 0.0, %s}\n", 
		envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
		envi->center_lon, envi->projection);
	break;
      case STATE_PLANE:
      case SCANSAR_PROJECTION: 
      case LAT_LONG_PSEUDO_PROJECTION:
      case UNKNOWN_PROJECTION:
	break;
      }
  }
  fprintf(fp, "wavelength units = %s\n", envi->wavelength_units);
  /*** wavelength, data ignore and default stretch currently not used ***/
  FCLOSE(fp);

  return;
}
