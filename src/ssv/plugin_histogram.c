// This is a plugin program that serves mainly as an example of how
// analysis programs for ssv should be written, and gives people
// something to cut and paste from.  It computes and prints histogram
// information about the tiles it analyzes.

#include <stdlib.h>

#include <glib.h>
#include <gsl/gsl_histogram.h>

#include <float_image.h>

// As specified in the analysis interface provided by ssv, the first
// argument (after argv[0], the invocation name) is the number of
// tiles being passed into the analysis plugin.  The second and
// succeeding arguments are also as as described in the output
// produced by 'ssv --help'.
#define TILE_COUNT_ARGUMENT_POSITION 1

// This type describes the information that we get passed for each
// tile.
typedef struct {
  GString *base_name;
  size_t width, height;
  GString *file_name;
  FloatImage *tile;
} tile_spec_type;

int
main (int argc, char **argv)
{
  char *prog_name = argv[0];
  prog_name = prog_name;   // You might use this for reporting errors.

  // How many image tiles are we getting passed?
  int tile_count = atoi (argv[TILE_COUNT_ARGUMENT_POSITION]);

  // Print a string showing how we were invoked.
  GString *invocation = g_string_new ("");
  int ii;
  for ( ii = 0 ; ii < argc ; ii++ ) {
    g_string_append_printf (invocation, "%s ", argv[ii]);
  }
  g_print ("\nRunning analysis program '%s'...\n", invocation->str);
  g_string_free (invocation, TRUE);
	   

  // Read the argument into an array of tile_spec_type structures.
  GPtrArray *tile_specs = g_ptr_array_new ();
  int current_arg = TILE_COUNT_ARGUMENT_POSITION + 1;
  for ( ii = 0 ; ii < tile_count ; ii++ ) {
    tile_spec_type *cs = g_new (tile_spec_type, 1); // Current spec.
    cs->base_name = g_string_new (argv[current_arg++]);
    cs->width = atoi (argv[current_arg++]);
    cs->height = atoi (argv[current_arg++]);
    cs->file_name = g_string_new (argv[current_arg++]);
    cs->tile = float_image_new_from_file (cs->width, cs->height,
					  cs->file_name->str, 0,
					  FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
    g_ptr_array_add (tile_specs, cs);
  }
	  
  g_print ("\nGot %d total structures to process.\n", tile_count);

  // Compute and print histograms and other information for each tile.
  for ( ii = 0 ; ii < tile_specs->len ; ii++ ) {
    // Current tile spec.
    tile_spec_type *cs = g_ptr_array_index (tile_specs, ii);
    g_print ("\nTile from image %s\n", cs->base_name->str);
    g_print ("Width: %llu\n", (long long unsigned int) cs->width);
    g_print ("Height: %llu\n", (long long unsigned int) cs->height);
    if ( cs->width == 0 || cs->height == 0 ) {
      g_print ("Tile of zero area, nothing to analyze.\n");
    }
    else {
      g_print ("Histogram:\n");
      const size_t bin_count = 20;
      gsl_histogram *hg = gsl_histogram_alloc (bin_count);
      // The test data I use is from an originally byte-valued image; if
      // you have a different sort of image a histogram covering only
      // this data region may be pretty dull.
      gsl_histogram_set_ranges_uniform (hg, 0.0, 255.0);
      guint jj, kk;
      for ( jj = 0 ; jj < cs->width ; jj++ ) {
	for ( kk = 0 ; kk < cs->height ; kk++ ) {
	  float pv = float_image_get_pixel (cs->tile, jj, kk);
	  gsl_histogram_increment (hg, pv);
	}
      }
      g_print ("  Range Start     Range End    Occurences\n");
      g_print ("-----------------------------------------\n");
      gsl_histogram_fprintf (stdout, hg, "%13.2f", "%13.0f");
      gsl_histogram_free (hg);
    }
    g_print ("\n");
  }

  // Free the tile specifications array.
  for ( ii = 0 ; ii < tile_specs->len ; ii++ ) {
    tile_spec_type *cs = g_ptr_array_index (tile_specs, ii);
    float_image_free (cs->tile);
    g_string_free (cs->file_name, TRUE);
    g_string_free (cs->base_name, TRUE);
    g_free (cs);
  }
  g_ptr_array_free (tile_specs, TRUE);

  exit (EXIT_SUCCESS);
}
