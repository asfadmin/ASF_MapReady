#include <stddef.h>

#include <gdk-pixbuf/gdk-pixbuf.h>

/* Make a jpeg thumbnail image from input Committee for Earth
   Observing Systems metadata and data file (arguments input_metadata
   and input_data respectively) or maximum dimension
   max_thumbnail_dimension, storing the result in file
   output_jpeg.  */
void
make_input_image_thumbnail (const char *input_metadata, const char *input_data,
			    size_t max_thumbnail_dimension, 
			    const char *output_jpeg);

/* Like make_input_image_thumbnail, but returns a new GdkPixbuf object
   instead of creating an output JPEG image, and uses passes a
   reasonable minimum size to make_input_image_thumbnail instead of
   really tiny values, then lets the GdkPixbuf class handle remaining
   scaling to get the max_thumbnail_dimension requested.  */
GdkPixbuf *
make_input_image_thumbnail_pixbuf (const char *input_metadata, 
				   const char *input_data,
				   size_t max_thumbnail_dimension);
