// Quick hack to make a subimage of main testing image (so we have
// something that will load more quickly).

#include <stdlib.h>

#include <glib.h>

#include <asf_meta.h>
#include <float_image.h>

#define TEST_IMAGE_BASE_NAME "E116306133G1S009"
#define SUBIMAGE_START_X 3700
#define SUBIMAGE_START_Y 900
#define SUBIMAGE_WIDTH 2800
#define SUBIMAGE_HEIGHT 1900

int
main (void)
{
  // Names of input files.
  GString *input_meta_name = g_string_new (TEST_IMAGE_BASE_NAME);
  g_string_append (input_meta_name, ".meta");
  GString *input_data_name = g_string_new (TEST_IMAGE_BASE_NAME);
  g_string_append (input_data_name, ".img");

  // Input metadata structure.
  meta_parameters *imd = meta_read (input_meta_name->str);
  size_t isx = imd->general->sample_count, isy = imd->general->line_count;

  FloatImage *id 
    = float_image_new_from_file (isx, isy, input_data_name->str, 0,
				 FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  FloatImage *subimage 
    = float_image_new_subimage (id, SUBIMAGE_START_X, SUBIMAGE_START_Y,
				SUBIMAGE_WIDTH, SUBIMAGE_HEIGHT);

  // Names for new subimage output files..
  GString *subimage_base_name = g_string_new (TEST_IMAGE_BASE_NAME);
  g_string_append (subimage_base_name, "_subimage");
  GString *subimage_meta_name = g_string_new (subimage_base_name->str);
  g_string_append (subimage_meta_name, ".meta");
  GString *subimage_data_name = g_string_new (subimage_base_name->str);
  g_string_append (subimage_data_name, ".img");

  meta_parameters *omd = meta_read (input_meta_name->str);
  // Neither projection parameters nor radar geometry parameters are
  // kept up to date.  The projection parameters at least could be so
  // we have this assertion.
  g_assert (omd->projection == NULL);
  omd->general->sample_count = SUBIMAGE_WIDTH;
  omd->general->line_count = SUBIMAGE_HEIGHT;

  // Write out the new files.
  meta_write (omd, subimage_meta_name->str);
  int return_code = float_image_store (subimage, subimage_data_name->str,
				       FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  g_assert (return_code == 0);

  // Make a quick jpeg to verify we have what we want.
  double bogus_mask = -1e100;	// We don't really use the mask.
  float_image_export_as_jpeg (subimage, "subimage.jpg", 
			      (subimage->size_x > subimage->size_y ?
			       subimage->size_x : subimage->size_y),
			      bogus_mask);

  exit (EXIT_SUCCESS);
}
