// Test program to see how the float_image class works out.

#include <assert.h>
#include <stdlib.h>

#include <glib.h>

#include <asf_meta.h>
#include "float_image.h"

int
main (int argc, char **argv)
{
  assert (argc == 3);
  argc = argc;			// Reassure compiler.

  // Image to be brightened.
  GString *input_image_basename = g_string_new (argv[1]);

  // Basename to use for brightened version of image.
  GString *output_image_basename = g_string_new (argv[2]);

  // Form names for the input image data and metadata.
  GString *input_metadata_file = g_string_new (input_image_basename->str);
  g_string_append (input_metadata_file, ".meta");
  GString *input_data_file = g_string_new (input_image_basename->str);
  g_string_append (input_data_file, ".img");

  // Form names for the output image data and metadata.
  GString *output_metadata_file = g_string_new (output_image_basename->str);
  g_string_append (output_metadata_file, ".meta");
  GString *output_data_file = g_string_new (output_image_basename->str);
  g_string_append (output_data_file, ".img");

  // Read the metadata for the input image.
  meta_parameters *imd = meta_read (input_image_basename->str);

  // Shorthand for the dimension of the input image.
  size_t ixs = imd->general->sample_count;
  size_t iys = imd->general->line_count;

  // Set up FloatImage abstraction for the input image.
  FloatImage *id 
    = float_image_new_from_file (ixs, iys, input_data_file->str, 0, 
				 FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  // Set up FloatImage abstraction for the output image.
  FloatImage *od = float_image_new (ixs, iys);

  GString *date_command_string = g_string_new ("date --rfc-822");
  int return_code = system (date_command_string->str);

  // Do the brightening.
  size_t ii;
  // For each line. 
  for ( ii = 0 ; ii < iys ; ii++ ) {
    size_t jj;
    // For each pixel of line.
    for ( jj = 0 ; jj < ixs ; jj++ ) {
      // Brighten pixel by factor of two and store in output image.
      float pixel_value = float_image_get_pixel (id, jj, ii);
      float_image_set_pixel (od, jj, ii, pixel_value * 2);
    }
  }

  return_code = system (date_command_string->str);

  // Done with the input image.
  float_image_free (id);

  // Store the output image data.
  return_code = float_image_store (od, output_data_file->str,
				   FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  g_assert (return_code == 0);

  // Done with the output image.
  float_image_free (od);

  // Copy the input metadata to the output metadata.
  GString *system_command = g_string_new ("cp ");
  g_string_append_printf (system_command, "%s %s", input_metadata_file->str,
			  output_metadata_file->str);
  return_code = system (system_command->str);
  g_assert (return_code == 0);

  exit (EXIT_SUCCESS);
}
