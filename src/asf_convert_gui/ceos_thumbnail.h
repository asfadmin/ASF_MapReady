#include <stddef.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

/* Returns a new GdkPixbuf object, and uses a reasonable minimum size
   to make_input_image_thumbnail instead of really tiny values, then
   lets the GdkPixbuf class handle remaining scaling to get the
   max_thumbnail_dimension requested.  */
GdkPixbuf *
make_input_image_thumbnail_pixbuf (const char *input_metadata, 
				   const char *input_data,
				   size_t max_thumbnail_dimension);
