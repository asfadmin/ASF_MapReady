#include <asf_meta.h>
#include <ceos_io.h>
#include <float_image.h>
#include <math.h>

void
make_input_image_thumbnail (char *input_metadata, char *input_data,
			    size_t max_thumbnail_dimension, 
			    char *output_jpeg)
{
  meta_parameters *imd = meta_read (input_metadata); // Input metadata.
  CEOS_FILE *id = fopenCeos (input_data); // Input data file.

  // Vertical and horizontal scale factors required to meet the
  // max_thumbnail_dimension part of the interface contract.
  int vsf = ceil (imd->general->line_count / max_thumbnail_dimension);
  int hsf = ceil (imd->general->sample_count / max_thumbnail_dimension);
  // Overall scale factor to use is the greater of vsf and hsf.
  int sf = (hsf > vsf ? hsf : vsf);

  // Thumbnail image sizes.
  size_t tsx = imd->general->sample_count / sf;
  size_t tsy = imd->general->line_count / sf;

  // Thumbnail image.
  FloatImage *ti = float_image_new (tsx, tsy);

  // Form the thumbnail image by grabbing individual pixels.  FIXME:
  // Might be better to do some averaging or interpolating.
  size_t ii;
  int *line = g_new (int, imd->general->sample_count);
  for ( ii = 0 ; ii < tsy ; ii++ ) {
    readCeosLine(line, ii * sf, id);
    size_t jj;
    for ( jj = 0 ; jj < tsx ; jj++ ) {
      float_image_set_pixel (ti, jj, ii, line[jj * sf]);
    }
  }
  g_free (line);

  // Export as jpeg image as promised.  We don't want to reduce the
  // image resolution anymore, so we use the largest dimension
  // currently in the image as the max dimension for the image to
  // generate.
  float_image_export_as_jpeg (ti, output_jpeg, (ti->size_x > ti->size_y ? 
						ti->size_x : ti->size_y));

  float_image_free (ti);
}
