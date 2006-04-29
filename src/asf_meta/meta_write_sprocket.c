/******************************************************************************
FUNCTION NAME:
   meta2sprocket

FUNCTION DESCRIPTION:
   Write out a metadata file in SProCKET compatible format using native meta
   format

FUNCTION HISTORY:
    VERS: DATE:     AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
     -    05/2004   P. Denny     Initial implementation

******************************************************************************/

#include "asf.h"
#include "ceos.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "asf_reporting.h"
#include "metadata.h"


void meta_write_sprocket(const char *sprocketName, meta_parameters *meta,
                         struct dataset_sum_rec *dssr)
{
  // This method is untested and unthought-about for this type of
  // image (actually the type was added after the method, at which
  // time this assertion was also added).
  asfRequire (meta->projection == NULL
	      || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION,
	      "Method %s doesn't work on LAT_LONG_PSEUDO_PROJECTION images",
	      __func__);

  FILE *fp = FOPEN(sprocketName,"wb");
  int numLines = meta->general->line_count;
  int numSamples = meta->general->sample_count;
  long centerLine = numLines/2;
  long centerSample = numSamples/2;
  double earth_radius
         = meta_get_earth_radius(meta, centerLine, centerSample) / 1000.0;
  double sat_height
         = meta_get_sat_height(meta, centerLine, centerSample)/1000.0
           - earth_radius;
  double lat, lon;
  char temp[256];

  fprintf(fp,"%s = \"%s\"\n",CONVERTER_VERSION, "ASF Tools");
  fprintf(fp,"%s = \"%s\"\n",PROCESSING_INFO, meta->general->processor);
  fprintf(fp,"%s = \"%s\"\n",PROCESSING_DATE, MAGIC_UNSET_STRING);
  fprintf(fp,"%s = \"%s\"\n",PLATFORM, meta->general->sensor);
  fprintf(fp,"%s = \"%s\"\n",BEAM_MODE, meta->general->mode);
  fprintf(fp,"%s = \"%lf\"\n",FREQUENCY, meta->sar->wavelength);
  switch (meta->general->sensor[0]) {
    case 'E': strcpy(temp,"VV"); break;
    case 'R': strcpy(temp,"HH"); break;
    case 'J': strcpy(temp,"HH"); break;
    default: strcpy(temp, MAGIC_UNSET_STRING);
  }
  fprintf(fp,"%s = \"%s\"\n",POLARIZATION, temp);
  fprintf(fp,"%s = \"%lf\"\n",TRACK_ANGLE,
          (dssr) ? dssr->plat_head_scene : MAGIC_UNSET_DOUBLE);
  fprintf(fp,"%s = \"%lf\"\n",CLOCK_ANGLE,
          (dssr) ? dssr->clock_ang : MAGIC_UNSET_DOUBLE);
  switch (meta->sar->image_type) {
    case 'S': strcpy(temp,"SLANT"); break;
    case 'G': strcpy(temp,"GROUND"); break;
    case 'P': strcpy(temp,"MAP PROJECTION"); break;
    default: strcpy(temp, MAGIC_UNSET_STRING);
  }
  fprintf(fp,"%s = \"%s\"\n",PROJECTION, temp);
  fprintf(fp,"%s = \"%d\"\n",NUMBER_OF_PIXELS, meta->general->sample_count);
  fprintf(fp,"%s = \"%d\"\n",NUMBER_OF_LINES, meta->general->line_count);
  fprintf(fp,"%s = \"%lf\"\n",RNG_PIXEL_SPACING, meta->general->x_pixel_size);
  fprintf(fp,"%s = \"%lf\"\n",AZ_PIXEL_SPACING, meta->general->y_pixel_size);
  fprintf(fp,"%s = \"%s\"\n",CENTER_GMT,
          (dssr) ? dssr->inp_sctim : MAGIC_UNSET_STRING);
  fprintf(fp,"%s = \"%lf\"\n",SLANT_RANGE_TO_FIRST_PIXEL,
          meta->sar->slant_range_first_pixel/1000.0);
  fprintf(fp,"%s = \"%lf\"\n",EARTH_RADIUS_AT_IMAGE_CENTER, earth_radius);
  fprintf(fp,"%s = \"%lf\"\n",EARTH_RADIUS_AT_IMAGE_NARIR, earth_radius);
  fprintf(fp,"%s = \"%lf\"\n",PLATFORM_ALITITUDE, sat_height);
  if (   meta->general->data_type>=BYTE
      && meta->general->data_type<=REAL64)
    strcpy(temp, STANDARD_FORMAT);
  else if (   meta->general->data_type>=COMPLEX_BYTE
           && meta->general->data_type<=COMPLEX_REAL64)
    strcpy(temp, COMPLEX_FORMAT);
  else
    strcpy(temp, MAGIC_UNSET_STRING);
  fprintf(fp,"%s = \"%s\"\n",IMAGE_FORMAT, temp);

  meta_get_latLon(meta, 0.0, numSamples, 0.0, &lat, &lon);
  fprintf(fp,"%s = \"%lf\"\n",TOP_RIGHT_CORNER_LAT, lat);
  fprintf(fp,"%s = \"%lf\"\n",TOP_RIGHT_CORNER_LONG, lon);
  meta_get_latLon(meta, 0.0, 0.0, 0.0, &lat, &lon);
  fprintf(fp,"%s = \"%lf\"\n",TOP_LEFT_CORNER_LAT, lat);
  fprintf(fp,"%s = \"%lf\"\n",TOP_LEFT_CORNER_LONG, lon);
  meta_get_latLon(meta, numLines, numSamples, 0.0, &lat, &lon);
  fprintf(fp,"%s = \"%lf\"\n",BOTTOM_RIGHT_CORNER_LAT, lat);
  fprintf(fp,"%s = \"%lf\"\n",BOTTOM_RIGHT_CORNER_LONG, lon);
  meta_get_latLon(meta, numLines, 0.0, 0.0, &lat, &lon);
  fprintf(fp,"%s = \"%lf\"\n",BOTTOM_LEFT_CORNER_LAT, lat);
  fprintf(fp,"%s = \"%lf\"\n",BOTTOM_LEFT_CORNER_LONG, lon);

  fprintf(fp,"%s = \"%lf\"\n",ELLIPS_MAJ_AXIS, meta->general->re_major/1000.0);
  fprintf(fp,"%s = \"%lf\"\n",ELLIPS_MIN_AXIS, meta->general->re_minor/1000.0);
  fprintf(fp,"%s = \"%d\"\n",REVOLUTION, meta->general->orbit);
  switch (meta->general->orbit_direction) {
    case 'A': strcpy(temp,"ASCENDING"); break;
    case 'D': strcpy(temp,"DESCENDING"); break;
    default: strcpy(temp, MAGIC_UNSET_STRING);
  }
  fprintf(fp,"%s = \"%s\"\n",FLIGHT_DIRECTION, temp);
  fprintf(fp,"%s = \"%lf\"\n",PRF, meta->sar->prf);
  fprintf(fp,"%s = \"%lf\"\n",DOPPLER_POLY_A0,
          meta->sar->range_doppler_coefficients[0]);
  fprintf(fp,"%s = \"%lf\"\n",DOPPLER_POLY_A1,
          meta->sar->range_doppler_coefficients[1]);
  fprintf(fp,"%s = \"%lf\" \n",DOPPLER_POLY_A2,
          meta->sar->range_doppler_coefficients[2]);

  /* Vexcel "Unique" metadata keys */
  fprintf(fp,"%s = \"%s\"\n",VEXCEL_BETA_OR_SIGMA, MAGIC_UNSET_STRING);
  fprintf(fp,"%s = \"%s\"\n",VEXCEL_ANTENNA_PATTERN, MAGIC_UNSET_STRING);

  FCLOSE(fp);
}
