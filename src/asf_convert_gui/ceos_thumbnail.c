#include <unistd.h>
#include <asf_meta.h>
#include <ceos_io.h>
#include <float_image.h>
#include <math.h>

#include "ceos_thumbnail.h"

gboolean
make_input_image_thumbnail (const char *input_metadata, const char *input_data,
			    size_t max_thumbnail_dimension, 
			    const char *output_jpeg)
{
  meta_parameters *imd = meta_create (input_metadata); // Input metadata.
  /* Make a copy of one of the arguments so the compilers doesn't
     complain about us ignoring the const qualifier when we pass it fo
     fopenCeos().  */

  if (imd->general->data_type != BYTE &&
      imd->general->data_type != INTEGER16 &&
      imd->general->data_type != INTEGER32 &&
      imd->general->data_type != REAL32 &&
      imd->general->data_type != REAL64)
  {
      /* don't know how to make a thumbnail for this type ... */
      return FALSE;
  }

  gchar *tmp = g_strdup (input_data);
  g_assert (tmp != NULL);
  CEOS_FILE *id = fopenCeos (tmp); // Input data file.
  g_free (tmp);

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
      // Current sampled value.  We will average a couple pixels together.
      int csv;		
      if ( jj * sf < imd->general->line_count - 1 ) {
	csv = (line[jj * sf] + line[jj * sf + 1]) / 2;
      }
      else {
	csv = (line[jj * sf] + line[jj * sf - 1]) / 2;
      }
      float_image_set_pixel (ti, jj, ii, csv);
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

  return TRUE;
}

GdkPixbuf *
make_input_image_thumbnail_pixbuf (const char *input_metadata,
				   const char *input_data,
				   size_t max_thumbnail_dimension)
{
  GError *err = NULL;
  
  gchar *tfn;			/* Temporary file name.  */
  gint tfd = g_file_open_tmp ("thumbnail_jpg-XXXXXX", &tfn, &err);
  if ( err != NULL ) {
    g_error ("Couldn't open temporary thumbnail image: %s\n", err->message);
  }
  g_assert (err == NULL);
  int return_code = close (tfd);

#ifndef win32
  /* for some reason, this assert fails on windows... errno reveals
     a "Bad file descriptor" error when doing the above close.  Removing
     for now, this should be investigated.  Not sure what problems, if
     any, are caused by ignoring this error. */
  g_assert (return_code == 0);  
#endif

  /* This is about the size needed for the thumbnail code in
     make_input_image_thumbnail to behave decently.  We will let the
     code in gdk_pixbuf_new_from_file_at_size do the remaining
     resizing if any, since it probably does a better job.  */
  const size_t decent_sampled_thumbnail_size = 256;
  gboolean ok;

  if ( max_thumbnail_dimension < decent_sampled_thumbnail_size ) {
    ok = make_input_image_thumbnail (input_metadata, input_data, 
				     decent_sampled_thumbnail_size, tfn);    
  }
  else {
    ok = make_input_image_thumbnail (input_metadata, input_data, 
				     max_thumbnail_dimension, tfn);
  }

  GdkPixbuf *result = NULL;

  if (ok)
  {
     result = gdk_pixbuf_new_from_file_at_size (tfn, max_thumbnail_dimension,
						max_thumbnail_dimension, &err);

     return_code = unlink (tfn);
     g_assert (return_code == 0);
     
     if ( err != NULL ) {
      g_error ("Couldn't load thumbnail popup image '%s': %s", tfn, 
	       err->message);
     }
  }

  g_free (tfn);

  return result;
}
