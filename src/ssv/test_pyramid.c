// Test driver for the pyramid class.

#include <math.h>
#include <stdlib.h>

#include <glib.h>
#include <gsl/gsl_math.h>

#include <asf_meta.h>
#include <float_image.h>

#include "meta_read_wrapper.h"
#include "pyramid.h"

// Base name of .img/.meta pair we will use for testing.
//#define TEST_IMG_BASENAME "E116306133G1S009"
#define TEST_IMG_BASENAME "E116306133G1S009_subimage"

// Directory to use as a scratch directory for pyramid layers.
#define SCRATCH_DIR "/tmp/"

// When we make JPEG images we give this mask value to the routine
// which does it.  This make should have any effect for the image we
// are using.
#define TEST_IMG_BOGUS_MASK_VALUE -999999.9

int
main (void)
{
  g_print ("This test may take a few moments...\n");

  GString *meta_name = g_string_new (TEST_IMG_BASENAME);
  g_string_append (meta_name, ".meta");

  GString *data_name = g_string_new (TEST_IMG_BASENAME);
  g_string_append (data_name, ".img");

  meta_parameters *md = meta_read_wrapper (meta_name->str);

  g_string_free (meta_name, TRUE);

  ssize_t tisx = md->general->sample_count;
  ssize_t tisy = md->general->line_count;

  FloatImage *tiafi
    = float_image_new_from_file (tisx, tisy, data_name->str, 0,
				 FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  g_string_free (data_name, TRUE);

  float_image_export_as_jpeg (tiafi, "test_pyramid_base_layer_direct.jpg",
			      GSL_MAX (tisx, tisy), TEST_IMG_BOGUS_MASK_VALUE);

  meta_free (md);

  Pyramid *p = pyramid_new (TEST_IMG_BASENAME, SCRATCH_DIR);

  // "Testing" currently consists of creating jpeg images of each
  // layer and checking to see if they look decent.

  size_t ii;
  for ( ii = 0 ; ii < p->layers->len ; ii++ ) {

    pyramid_layer *cl = g_ptr_array_index (p->layers, ii);

    float *region;
    size_t rstart_x, rstart_y, rw, rh;
    gboolean unowned_memory;

    pyramid_get_region (p, 0, 0, tisx, tisy, pow (2, ii), &region, &rstart_x,
			&rstart_y, &rw, &rh, &unowned_memory);
    g_assert (rstart_x == 0 & rstart_y == 0);
    g_assert (rstart_x + rw * pow (2.0, ii) >= tisx);
    g_assert (rstart_y + rh * pow (2.0, ii) >= tisy);

    FloatImage *layer_image = float_image_new_from_memory (rw, rh, region);

    if ( unowned_memory ) {
      g_free (region);
    }

    GString *layer_file_name = g_string_new ("");
    g_string_append_printf (layer_file_name, "layer_%d.jpg", ii); 

    ssize_t max_dimension = rw > rh ? rw : rh;

    int return_code
      = float_image_export_as_jpeg (layer_image, layer_file_name->str,
				    max_dimension, TEST_IMG_BOGUS_MASK_VALUE);
    g_assert (return_code == 0);

    float_image_unref (layer_image);

    g_string_free (layer_file_name, TRUE);
  }

  g_print ("JPEG images of pyramid layers formed, now look at them and\n"
	   "'touch test_pyramid_stamp' if they are ok.\n");

  exit (EXIT_SUCCESS);
}
