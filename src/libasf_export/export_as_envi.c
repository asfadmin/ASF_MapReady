#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>

#include <cla.h>
#include <envi.h>
#include <esri.h>
#include <geokeys.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_statistics.h>
#include <jpeglib.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_export.h>

void
export_as_envi (const char *metadata_file_name,
                const char *image_data_file_name,
                const char *output_file_name)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  char envi_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  envi_header *envi;
  FILE *fp;
  time_t time;
  char t_stamp[15];
  char envi_data_file_name[2 * MAX_IMAGE_NAME_LENGTH];

  /* Complex data generally can't be output into meaningful images, so
     we refuse to deal with it.  */
  if (   md->general->data_type == BYTE
      || md->general->data_type == INTEGER16
      || md->general->data_type == INTEGER32
      || md->general->data_type == REAL32
      || md->general->data_type == REAL64)
  {
    asfPrintError("Input data cannot be complex.\n");
  }

  create_name (envi_file_name, output_file_name, ".hdr");
  envi = meta2envi (md);

  /* Write ENVI header file */
  fp = FOPEN(envi_file_name, "w");
  strftime(t_stamp, 12, "%d-%b-%Y", localtime(&time));
  fprintf(fp, "ENVI\n");
  fprintf(fp, "description = {\n"
              "  Converted to ENVI format on (%s)}\n", t_stamp);
  fprintf(fp, "samples = %i\n", envi->samples);
  fprintf(fp, "lines = %i\n", envi->lines);
  fprintf(fp, "bands = %i\n", envi->bands);
  fprintf(fp, "header offset = %i\n", envi->header_offset);
  fprintf(fp, "file type = %s\n", envi->file_type);
  fprintf(fp, "data type = %i\n", envi->data_type);
  fprintf(fp, "interleave = %s\n", envi->interleave);
  fprintf(fp, "sensor type = %s\n", envi->sensor_type);
  fprintf(fp, "byte order = %i\n", envi->byte_order);
  if (md->projection) {
    switch (md->projection->type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      fprintf(fp,
              "map info = {%s, %i, %i, %.4f, %.4f, %.4f, %.4f, %i, %s}\n",
              envi->projection, envi->ref_pixel_x, envi->ref_pixel_y,
              envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
              envi->proj_dist_y, envi->projection_zone, envi->hemisphere);
      fprintf(fp,
              "projection info = {3, %.4f, %.4f, %.4f, %.4f, "
              "0.0, 0.0, 0.99996, %s}\n",
              envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
              envi->center_lon, envi->projection);
      break;
    case POLAR_STEREOGRAPHIC:
      fprintf(fp,
              "map info = {%s, %i, %i, %.4f, %.4f, %.4f, %.4f, %s}\n",
              envi->projection, envi->ref_pixel_x, envi->ref_pixel_y,
              envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
              envi->proj_dist_y, envi->hemisphere);
      fprintf(fp,
              "projection info = {31, %.4f, %.4f, %.4f, %.4f, "
              "0.0, 0.0, %s}\n",
              envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
              envi->center_lon, envi->projection);
      break;
    case ALBERS_EQUAL_AREA:
      fprintf(fp,
              "map info = {%s, %i, %i  , %.4f, %.4f, %.4f, %.4f, %s}\n",
              envi->projection, envi->ref_pixel_x, envi->ref_pixel_y,
              envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
              envi->proj_dist_y, envi->hemisphere);
      fprintf(fp,
              "projection info = {9, %.4f, %.4f, %.4f, %.4f, "
                "0.0, 0.0, %.4f, %.4f, %s}\n",
              envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
              envi->center_lon, envi->standard_parallel1,
              envi->standard_parallel2, envi->projection);
      break;
    case LAMBERT_CONFORMAL_CONIC:
      fprintf(fp,
              "map info = {%s, %i, %i, %.4f, %.4f, %.4f, %.4f, %s}\n",
              envi->projection, envi->ref_pixel_x, envi->ref_pixel_y,
              envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
              envi->proj_dist_y, envi->hemisphere);
      fprintf(fp,
              "projection info = {4, %.4f, %.4f, %.4f, %.4f, "
              "0.0, 0.0, %.4f, %.4f, %s}\n",
              envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
              envi->center_lon, envi->standard_parallel1,
              envi->standard_parallel2, envi->projection);
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      fprintf(fp,
              "map info = {%s, %i, %i, %.4f, %.4f, %.4f, %.4f, %s}\n",
              envi->projection, envi->ref_pixel_x, envi->ref_pixel_y,
              envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
              envi->proj_dist_y, envi->hemisphere);
      fprintf(fp,
              "projection info = {11, %.4f, %.4f, %.4f, %.4f, "
              "0.0, 0.0, %s}\n",
              envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
              envi->center_lon, envi->projection);
      break;
    case LAT_LONG_PSEUDO_PROJECTION:
      asfPrintError ("Exporting pseudoprojected images in ENVI format is not "
		     "implemented.");
      break;
    case STATE_PLANE:
      asfPrintError ("Exporting state plane images in ENVI format is not "
		     "implemented.");
      break;
    case SCANSAR_PROJECTION:
      asfPrintError ("Exporting scansar projected images probably doesn't "
		     "make sense and isn't supported.");
      break;
    }
  }
  fprintf(fp, "wavelength units = %s\n", envi->wavelength_units);
  /*** wavelength, data ignore and default stretch currently not used ***/
  FCLOSE(fp);

  /* Clean and report */
  free (envi);
  meta_free (md);

  strcpy (envi_data_file_name, output_file_name);
  strcat (envi_data_file_name, ".bil");
  fileCopy(image_data_file_name, envi_data_file_name);
}
