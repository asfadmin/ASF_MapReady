// Implementation of interface described in ssv_command_line.h.

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ssv_command_line.h"
#include "utilities.h"

static gboolean
parse_pixmap (const gchar *option_name, const gchar *value,
	      SSVCommandLine *self, GError **error)
{
  gfloat range_start, range_end, range_value;

  // The format string we want the pixmap option argument text to
  // match.
  const gchar *sscanf_format = "[%f,%f],%f";
  // CAUTION: sync syntax with the documentation for --help (below).
  g_assert (strcmp (sscanf_format, "[%f,%f],%f") == 0);

  int match_count = sscanf (value, sscanf_format, &range_start,
			    &range_end, &range_value);
  if ( match_count != 3 ) {
    g_set_error (error, G_OPTION_ERROR, G_OPTION_ERROR_FAILED,
		 "Failed to parse option argument '%s': sscanf with format "
		 "argument '%s' didn't return a match count of 3",
		 value, sscanf_format);
    return FALSE;
  }

  // If either end of a pixel remap range is NAN, both must be.
  if ( isnan (range_start) || isnan (range_end) ) {
    if ( ! (isnan (range_start) && isnan (range_end)) ) {
      g_set_error (error, G_OPTION_ERROR, G_OPTION_ERROR_FAILED,
		   "Bad pix map specification '%s': if either range end is "
		   "\"nan\", both must be.", value);
      return FALSE;
    }
  }

  // FIIXME: need some error propagation in this function, or to store
  // somewhere else and plunk into pixmap later.
  pix_maps_spec_add_map (self->pixmaps, range_start, range_end, range_value);

  return TRUE;
}

static gboolean
parse_offset (const gchar *option_name, const gchar *value,
	      SSVCommandLine *self, GError **error)
{
  gint x_offset, y_offset;
  
  // The format string we want the offset option argument text to
  // match.
  const gchar *sscanf_format = "%d,%d";
  // CAUTION: sync syntax with the scanner.
  g_assert (strcmp (sscanf_format, "%d,%d") == 0);

  int match_count = sscanf (value, sscanf_format, &x_offset, &y_offset);

  if ( match_count != 2 ) {
    g_set_error (error, G_OPTION_ERROR, G_OPTION_ERROR_FAILED,
		 "Failed to parse option argument '%s': sscanf with format "
		 "argument '%s', didn't return a match count of 2",
		 value, sscanf_format);
    return FALSE;
  }

  g_array_append_val (self->x_offsets, x_offset);
  g_array_append_val (self->y_offsets, y_offset);

  return TRUE;
}

SSVCommandLine *
ssv_command_line_new (gint *argc, gchar ***argv)
{
  SSVCommandLine *self = g_new (SSVCommandLine, 1);

  // Initialize the contents of the structure we will parse into with
  // default or empty values.  IMPROVEME: this notion of the default
  // cache directory is manually synced with some code in main().
  self->cache_dir = g_string_new (getenv ("HOME"));
  g_string_append (self->cache_dir, "/.ssv_cache");
  const gint default_max_cache_size = 400;
  self->max_cache_size = default_max_cache_size;
  self->pixmaps = pix_maps_spec_new ();
  // If sigmas is negative, we will try to linearly map the whole range.
  const gdouble default_sigmas = 2.0;
  self->sigmas = default_sigmas;
  self->images = g_ptr_array_new ();
  self->x_offsets = g_array_new (FALSE, FALSE, sizeof (gint));
  self->y_offsets = g_array_new (FALSE, FALSE, sizeof (gint));
  gchar *analysis_program = NULL;  // We'll copy into GString in self later.
  self->async_analysis = FALSE;
  const gint default_analysis_tile_size = 51;
  self->analysis_tile_size = default_analysis_tile_size;

  const gint option_type_count = 8;
  // List of uptions (+1 for the 'option' which handles the arguments,
  // +1 for the one with NULL for its long_name which terminated the
  // list).
  GOptionEntry *entries = g_new (GOptionEntry, option_type_count + 1 + 1);

  // Place for the parser to store the cache directory option argument
  // value.  It looks like the option parser automagicly allocates
  // this.
  gchar *cache_dir = NULL;

  // Place for the parser to store the image file names.
  gchar **image_names = NULL;

  // Entry strings, including documentation that gets printed by
  // --help (make sure it still looks good after changes).

  // IMPROVEME: there are little bits in these documentation strings
  // that at the moment need to be manually kept in sync with the
  // defaults and such.

  entries[0].long_name = "cache-dir";
  entries[0].short_name = 'c';
  entries[0].flags = 0;
  entries[0].arg = G_OPTION_ARG_FILENAME;
  entries[0].arg_data  = &cache_dir;
  entries[0].description
    = "\n     Directory to use for pyramid cache (defualt: ~/.ssv_cache)\n";
  entries[0].arg_description = "DIRECTORY";

  entries[1].long_name = "max-cache-size";
  entries[1].short_name = 'm';
  entries[1].flags = 0;
  entries[1].arg = G_OPTION_ARG_INT;
  entries[1].arg_data = &(self->max_cache_size);
  entries[1].description
    = (
"\n     Maximum cache size, in megabytes.  Old (unused) entries start\n"
"     getting deleted when the cache gets this big.  Default is 400.\n");
  entries[1].arg_description = "SIZE";
    
  entries[2].long_name = "pixmap";
  entries[2].short_name = 'p';
  entries[2].flags = 0;
  entries[2].arg = G_OPTION_ARG_CALLBACK;
  entries[2].arg_data = parse_pixmap;
  entries[2].description
    = (
"\n     Pixel remap specification of the form \"[a,b],c\".  Pixels in the \n"
"     range [a,b] (values of \"nan\" or \"inf\" are allowed) are not \n"
"     considerd when computing the image statistics used to map the image \n"
"     into the representable range of pixel values, and are always \n"
"     represented visually as if they have value c.  Note that pixel maps\n"
"     have no effect at all on pixel information, tile analysis, etc.: they\n"
"     only affect how the image data is rendered\n");
  entries[2].arg_description = "SPEC";

  entries[3].long_name = "sigmas";
  entries[3].short_name = 's';
  entries[3].flags = 0;
  entries[3].arg = G_OPTION_ARG_DOUBLE;
  entries[3].arg_data = &(self->sigmas);
  entries[3].description
    = (
"\n     When displaying data, map values within SIGMAS standard deviations\n"
"     of the mean linearly into the displayable range of values, and clamp\n"
"     values outside this region to the endpoints of the displayable range.\n"
"     Note that sufficiently huge or special pixel values will still require\n"
"     the use of pixel maps (see --pixmap option description).  The default\n"
"     value is 2.0.  If SIGMAS is negative, the whole range of pixel values\n"
"     present in the data will be mapped linearly into the displayable\n"
"     range.\n");
  entries[3].arg_description = "SIGMAS";


  entries[4].long_name = "offset";
  entries[4].short_name = 'o';
  entries[4].flags = 0;
  entries[4].arg = G_OPTION_ARG_CALLBACK;
  entries[4].arg_data = parse_offset;
  entries[4].description
    = (
"\n     Image offset specification of the form \"a,b\" where a is \n"
"     counted in whole image pixels rightward and b downward from the top \n"
"     left corner of the first image argument.  The first occurence of this \n"
"     option specifies the offset of the top left corner of the second \n"
"     image, the second occurence the offset of the third image, etc. \n"
"     If any occurences of this option appear in an invocation, the correct \n"
"     number (i.e. one less than the number of images) must be supplied.\n");
  entries[4].arg_description = "SPEC";
  
  entries[5].long_name = "analysis-program";
  entries[5].short_name = 'a';
  entries[5].flags = 0;
  entries[5].arg = G_OPTION_ARG_STRING;
  entries[5].arg_data = &analysis_program;
  entries[5].description
    = (
"\n    Program to run when 'a' key is pressed.  The program is invoked \n"
"    with the following command line arguments:\n"
"\n"
"        tile_count            total number of image tiles to be passed\n"
"        base_name             image file base name\n"
"        tile_width            width of tile in pixels\n"
"        tile_height           height of tile in pixels\n"
"        tile_file_name        name of file containing tile data\n"
"\n"
"    where arguments 2 through 5 in the above list are repeated tile_count\n"
"    times.  The tile_width and tile_height arguments are generally equal to\n"
"    the value specified with the --analysis-tile-size option, but may be\n"
"    smaller (or even zero) if the cursor was near or off the edge of the\n"
"    image when analysis was requested.  The tile_file_name is the name of a\n"
"    file containing the tile data in big endian form.  It's probably\n"
"    easiest to use the float_image_new_from_file method with these\n"
"    arguments.  The base_name argument might be useful if metadata needs to\n"
"    be used or the analysis region needs to be grown algorithmicly.\n");
  entries[5].arg_description = "PROGRAM";

  entries[6].long_name = "async-analysis";
  entries[6].short_name = 0;
  entries[6].flags = 0;
  entries[6].arg = G_OPTION_ARG_NONE;
  entries[6].arg_data = &(self->async_analysis);
  entries[6].description
    = ( 
"\n    Run the analysis program asynchronously (i.e. in background,\n"
"    without waiting for it to complete.\n");
  entries[6].arg_description = "";   // Doesn't take an argument.

  entries[7].long_name = "analysis-tile-size";
  entries[7].short_name = 0;
  entries[7].flags = 0;
  entries[7].arg = G_OPTION_ARG_INT;
  entries[7].arg_data = &(self->analysis_tile_size);
  entries[7].description
    = (
"\n    Dimensions of (square) tiles to analyze (must be odd).  The default\n"
"    size is 51.\n");
  entries[7].arg_description = "TILE_SIZE";

  entries[8].long_name = G_OPTION_REMAINING;
  entries[8].short_name = 0;
  entries[8].flags = 0;
  entries[8].arg = G_OPTION_ARG_FILENAME_ARRAY;
  entries[8].arg_data = &image_names;
  entries[8].description = "YOU_HOPEFULLY_DONT_SEE_ME";
  entries[8].arg_description = "IMAGE_BASE_NAME [IMAGE_BASE_NAME...]";

  entries[9].long_name = NULL;

  GOptionGroup *ssv_group
    = g_option_group_new ("ssv", "ssv Options",
			  "Display help for ssv options.", self, NULL);
  
  g_option_group_add_entries (ssv_group, entries);

  GOptionContext *option_context = g_option_context_new ("");

  g_option_context_set_main_group (option_context, ssv_group);

  // Run the parser.
  GError *err = NULL;
  gboolean parse_results
    = g_option_context_parse (option_context, argc, argv, &err);
  if ( !parse_results ) {
    g_printerr ("%s: option parsing failed: %s\n", g_get_prgname (),
		err->message);
    exit (EXIT_FAILURE);
  }
  else {
    g_assert (err == NULL);
  }

  g_option_context_free (option_context);

  // Vet and copy the cache directory name (if provided) into a new
  // GString instance in self.
  if ( cache_dir != NULL ) {
    GError *err = NULL;
    my_is_writable_directory (cache_dir, &err);
    if ( err != NULL ) {
      g_printerr ("%s: bad -c or --cache-dir option argument '%s': %s\n",
		  g_get_prgname (), cache_dir, err->message);
      exit (EXIT_FAILURE);
    }
    g_string_free (self->cache_dir, TRUE);
    self->cache_dir = g_string_new (cache_dir);
  }

  g_free (cache_dir);

  // Vet and copy the analysis related arguments into their homes in
  // self.
  if ( analysis_program != NULL ) {
    self->analysis_program = g_string_new (analysis_program);
  }
  else {
    self->analysis_program = NULL;
  }
  if ( self->analysis_tile_size % 2 != 1 ) {
    g_printerr ("%s: bad --analysis_tile_size option argument '%d': not odd\n",
		g_get_prgname (), self->analysis_tile_size);
    exit (EXIT_FAILURE);
  }

  // Vet and copy the image name strings into GString instances in the
  // array in self.
  guint ii;
  for ( ii = 0 ; image_names != NULL && image_names[ii] != NULL ; ii++ ) {
    GString *cs = g_string_new (image_names[ii]);

    GError *err = NULL;

    GString *data_name = my_g_string_new_printf ("%s.img", cs->str);
    my_is_readable_file (data_name->str, &err);
    if ( err != NULL ) {
      g_printerr ("%s: file '%s' looks unreadable: %s\n",
		  g_get_prgname (), data_name->str, err->message);
      exit (EXIT_FAILURE);
    }
    my_g_string_free (data_name);

    GString *meta_name = my_g_string_new_printf ("%s.meta", cs->str);
    my_is_readable_file (meta_name->str, &err);
    if ( err != NULL ) {
      g_printerr ("%s: file '%s' looks unreadable: %s\n",
		  g_get_prgname (), meta_name->str, err->message);
      exit (EXIT_FAILURE);
    }
    my_g_string_free (meta_name);

    g_ptr_array_add (self->images, cs);
  }

  g_strfreev (image_names);

  // We require at least one image argument.
  if ( self->images->len < 1 ) {
    g_printerr ("%s: at least one image base name argument must be "
		"provided (try %s --help).\n", g_get_prgname (),
		g_get_prgname ());
    exit (EXIT_FAILURE);
  }

  // We require the corrent number of offset arguments.
  if ( self->x_offsets->len > 0 ) {
    if ( self->x_offsets->len != self->images->len - 1 ) {
      g_printerr ("%s: if any offset options are provided, the number "
		  "provided must be exactly one less than the number of image "
		  "arguments provided\n", g_get_prgname ());
      exit (EXIT_FAILURE);
    }
  }

  // IMPROVEME: Well this restriction has the potential to be very
  // annoying: if more than one image is specified to be loaded,
  // offset options must be specified for each image beyond the first.
  // The sensible thing would be to have default offset of (0, 0) or
  // course.  But it opend a small can or worms if we want and offset
  // for only the third image for example, and I just don't have the
  // time to go through and make everything agree.
  g_assert (self->x_offsets->len == self->images->len - 1);
  g_assert (self->x_offsets->len == self->y_offsets->len);

  self->reference_count = 1;

  return self;
}

// Increment reference count, returns self as a convenience.
SSVCommandLine *
ssv_command_line_ref (SSVCommandLine *self)
{
  self->reference_count++;

  return self;
}

// Decrement reference count, free self if reference count falls to
// zero.
void
ssv_command_line_unref (SSVCommandLine *self)
{
  self->reference_count--;

  if ( self->reference_count == 0 ) {
    g_string_free (self->cache_dir, TRUE);
    pix_maps_spec_unref (self->pixmaps);
    my_g_ptr_array_really_free (self->images, (FreeFunc) my_g_string_free);
    g_array_free (self->x_offsets, TRUE);
    g_array_free (self->y_offsets, TRUE);

    g_free (self);
  }
}
