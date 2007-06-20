// Interface to the command line arguments of ssv.  This class handles
// the fetching and interpretation of arguments, and also outputs help
// text an error message as appropriate.

#ifndef SSV_COMMAND_LINE_H
#define SSV_COMMAND_LINE_H

#include <glib.h>

#include "pix_maps_spec.h"

// All elements are read only.
typedef struct {
  // The cache directory to use.
  GString *cache_dir;
  // Maximum cache size in megabytes.
  gint max_cache_size;
  // Pixel maps (empty if no pixmaps options supplied);
  PixMapsSpec *pixmaps;
  // Standard deviations on each side of the mean to map linearly into
  // displayable range (values outside this are clamped).  See option
  // descriptions.
  gdouble sigmas;
  // Array of image base name arguments in GString form.  Must contain
  // at least one image base name.
  GPtrArray *images;
  // Arrays of gint image offsets with respect to first image.  There
  // must be images->len - 1 or these if there are any of them.
  GArray *x_offsets;
  GArray *y_offsets;

  // Analysis program related options.
  GString *analysis_program;	// Or NULL if no program specified.
  gboolean async_analysis;
  gint analysis_tile_size;
  gint reference_count;
} SSVCommandLine;

// Parse the arguments into the structure, generating help text and/or
// error messages as appropriate (for command line syntax, semantic
// errors aren't detected here).  Return the results as a new
// instance.
SSVCommandLine *
ssv_command_line_new (gint *argc, gchar ***argv);

// Increment reference count, returns self as a convenience.
SSVCommandLine *
ssv_command_line_ref (SSVCommandLine *self);

// Decrement reference count, free self if reference count falls to
// zero.
void
ssv_command_line_unref (SSVCommandLine *self);

#endif // SSV_COMMAND_LINE_H
