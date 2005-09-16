// Test program for comparison with brighten_float_image.

#include <assert.h>
#include <stdlib.h>

#include <glib.h>
#include <gsl/gsl_blas.h>

#include <asf_meta.h>

int
main (int argc, char **argv)
{
  assert (argc == 3);

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

  // Set up gsl matrix for the input image.
  gsl_matrix_float *id = gsl_matrix_float_alloc (iys, ixs);

  // Set up gsl matrix for the output image.
  gsl_matrix_float *od = gsl_matrix_float_alloc (iys, ixs);

  // Do the brightening.
  size_t ii;
  // For each line. 
  for ( ii = 0 ; ii < iys ; ii++ ) {
    size_t jj;
    // For each pixel of line.
    for ( jj = 0 ; jj < ixs ; jj++ ) {
      // Brighten pixel by factor of two and store in output image.
      float pixel_value = gsl_matrix_float_get (id, ii, jj);
      gsl_matrix_float_set (od, ii, jj, pixel_value * 2);
    }
  }

  // Store the output image data.
  FILE *od_stream = fopen (output_data_file->str, "w");
  g_assert (od_stream != NULL);
  int return_code = gsl_matrix_float_fwrite (od_stream, od);
  g_assert (return_code == GSL_SUCCESS);
  return_code = fclose (od_stream);
  g_assert (return_code == 0);

  // Copy the input metadata to the output metadata.
  GString *system_command = g_string_new ("cp ");
  g_string_append_printf (system_command, "%s %s", input_metadata_file->str,
			  output_metadata_file->str);
  return_code = system (system_command->str);
  g_assert (return_code == 0);

  exit (EXIT_SUCCESS);
}
