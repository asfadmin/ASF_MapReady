// Implementation of interface described in pyramid.h.

#include <ctype.h>
#include <math.h>

#include <glib.h>
#include <glib/gstdio.h>

#include <asf_meta.h>

#include "meta_read_wrapper.h"
#include "pyramid.h"
#include "float_blob.h"
#include "utilities.h"

#ifdef G_LOG_DOMAIN
#  undef G_LOG_DOMAIN
#endif
#define G_LOG_DOMAIN "Pyramid"

pyramid_layer *
form_base_layer (const char *base)
{
  pyramid_layer *result = g_new (pyramid_layer, 1);

  GString *meta = my_g_string_new_printf ("%s.meta", base);
  GString *data = my_g_string_new_printf ("%s.img", base);

  meta_parameters *mp = meta_read_wrapper (meta->str);

  // Dimensions of base.
  result->size_x = mp->general->sample_count;
  result->size_y = mp->general->line_count;

  // Convenience aliases.
  size_t sx = result->size_x;
  size_t sy = result->size_y;

  FloatBlob *base_blob = float_blob_new_using (sx, sy, data->str,
					       G_BYTE_ORDER != G_BIG_ENDIAN);

  size_t layer_size = sx * sy;   // Size of current layer in pixels.

  if ( layer_size * sizeof (float) <= PYRAMID_BIGGEST_LAYER_IN_MEMORY ) {
    result->is_float_blob = FALSE;
    result->data = g_new (float, layer_size);
    float_blob_get_region (base_blob, 0, 0, sx, sy, result->data);
  }
  else {
    result->is_float_blob = TRUE;
    result->data = base_blob;
  }

  return result;
}

// Form pyramid layers above base by creating FloatBlob or memory area
// instances using PixMapsSpec pixmaps in directory path.  If
// base_name is NULL, unique file names for any individual FloatBlob
// instances used are automagicly generated, otherwise names of the
// form "base_name_layer_number" are used (where the first
// layer_number is "1").
static GPtrArray *
form_upper_layers (pyramid_layer *base, PixMapsSpec *pixmaps,
		   GString *path, const char *base_name)
{
  GPtrArray *result = g_ptr_array_new ();

  // When the smaller dimesion of the topmost layer of the pyramid is
  // this size or smaller we consider the pyramid fully formed.
  const size_t min_dimension = 1;

  // Layer currently being worked on.
  size_t layer = 0;

  // Dimensions of current layer.
  size_t cw = base->size_x, ch = base->size_y;

  while ( cw > min_dimension && ch > min_dimension ) {
    
    // Previous layer (with respect to layer this iteration will add).
    pyramid_layer *pl;
    if ( layer == 0 ) {
      pl = base;
    }
    else {
      pl = g_ptr_array_index (result, layer - 1);
    }

    // If the previous layer has one or two odd dimensions, we will
    // need to generate the rightmost and/or bottommost edges of the
    // new layer without having four input pixels to use.
    gboolean extra_column = FALSE, extra_row = FALSE;
    if ( cw % 2 == 1 ) {
      extra_column = TRUE;
    }
    if ( ch % 2 == 1 ) {
      extra_row = TRUE;
    }

    cw = ceil (cw / 2.0);
    ch = ceil (ch / 2.0);

    layer++;

    // Curent layer.
    pyramid_layer *cl = g_new (pyramid_layer, 1);

    cl->size_x = cw;
    cl->size_y = ch;
    
    size_t layer_size = cw * ch;   // Size of current layer in pixels.

    if ( layer_size * sizeof (float) <= PYRAMID_BIGGEST_LAYER_IN_MEMORY ) {
      cl->is_float_blob = FALSE;
      cl->data = g_new (float, layer_size);
    }
    else {
      cl->is_float_blob = TRUE;
      if ( base_name == NULL ) {
	cl->data = float_blob_new (cw, ch, path->str, base_name);
      }
      else {
	GString *name = my_g_string_new_printf ("%s_%llu", base_name,
						(long long unsigned) layer);
	cl->data = float_blob_new (cw, ch, path->str, name->str);
	my_g_string_free (name);
      }
    }

    size_t ii, jj;		// Index variables.

    float *cr = g_new (float, cw); // Current row being formed.

    if ( pl->is_float_blob ) {

      float *ra = g_new (float, pl->size_x); // Row above.
      float *rb = g_new (float, pl->size_x); // Row below.

      // First we do all the rows of the new layer which are truly
      // averages of four other pixels.  We'll come back and fill in
      // the extra row that we have to deal with if the previous layer
      // had an odd number of rows later.
      for ( jj = 0 ; jj < ch - (extra_row ? 1 : 0) ; jj++ ) {

	float_blob_get_region (pl->data, 0, jj * 2, pl->size_x, 1, ra);
	float_blob_get_region (pl->data, 0, jj * 2 + 1, pl->size_x, 1, rb);

	// If this is the first layer above the base layer, we need to
	// apply the maps to the inputs as advertised.
	if ( layer == 1 ) {
	  guint kk;
	  for ( kk = 0 ; kk < pl->size_x ; kk++ ) {
	    ra[kk] = pix_maps_spec_map (pixmaps, ra[kk]);
	    rb[kk] = pix_maps_spec_map (pixmaps, rb[kk]);
	  }
	}

	if ( cl->is_float_blob ) {

	  // We will deal with the extra column, if any, later (see
	  // above comment).
	  for ( ii = 0 ; ii < cw - (extra_column ? 1 : 0) ; ii++ ) {
	    cr[ii] = ra[ii * 2] + ra[ii * 2 + 1] + rb[ii * 2] + rb[ii * 2 + 1];
	    cr[ii] /= 4.0;
	  }

	  // Deal with the extra column (except possibly for the
	  // bottom rightmost pixel, which is dealt with when the
	  // bottom row is done if there is an extra row).
	  if ( extra_column ) {
	    size_t cind = cw - 1; // Column index.
	    cr[cind] = (ra[cind * 2] + rb[cind * 2]) / 2.0;
	  }

	  float_blob_set_region (cl->data, 0, jj, cw, 1, cr);
	}

	else {
	  // Current row pointer.
	  float *crp = ((float *) cl->data) + jj * cw;

	  for ( ii = 0 ; ii < cw - (extra_column ? 1 : 0) ; ii++ ) {
	    crp[ii]
	      = ((ra[ii * 2] + ra[ii * 2 + 1] + rb[ii * 2] + rb[ii * 2 + 1])
		 / 4.0);
	  }
	  
	  // Deal with potential extra column (see comment above).
	  if ( extra_column ) {
	    size_t cind = cw - 1; // Column index.
	    crp[cind] = (ra[cind * 2] + rb[cind * 2]) / 2.0;
	  }
	}
      }

      g_free (ra);
      g_free (rb);
    }

    else { // Previous layer is not a FloatBlob

      for ( jj = 0 ; jj < ch - (extra_row ? 1 : 0) ; jj++ ) {

	// Interpreted previous layer data pointer.
	float *pld = ((float *) pl->data);

	for ( ii = 0 ; ii < cw - (extra_column ? 1 : 0) ; ii++ ) {
	  // Pixel values from which we will form the next pixel.
	  float ul, ur, ll, lr;
	  ul = pld[jj * 2 * pl->size_x + ii * 2];
	  ur = pld[jj * 2 * pl->size_x + ii * 2 + 1];
	  ll = pld[(jj * 2 + 1) * pl->size_x + ii * 2];
	  lr = pld[(jj * 2 + 1) * pl->size_x + ii * 2 + 1];
	  // If this is the first layer above the base layer, we need
	  // to apply the maps to the inputs as advertised.
	  if ( layer == 1 ) {
	    guint kk;
	    for ( kk = 0 ; kk < pl->size_x ; kk++ ) {
	      ul = pix_maps_spec_map (pixmaps, ul);
	      ur = pix_maps_spec_map (pixmaps, ul);
	      ll = pix_maps_spec_map (pixmaps, ul);
	      lr = pix_maps_spec_map (pixmaps, ul);
	    }
	  }
	  
 	  ((float *) cl->data)[jj * cw + ii] = (ul + ur + ll + lr) / 4.0;
	}

	if ( extra_column ) {
	  size_t cind = cw - 1;	// Column index.
	  float up, lp;		// Upper pixel, lower pixel.
	  up = pld[jj * 2 * pl->size_x + cind * 2];
	  lp = pld[(jj * 2 + 1) * pl->size_x + cind * 2];
	  // Here (and other such places) is where propagation should
	  // be implemented: if we are "infecting" upper layers, we
	  // need to do it every layer, not just the first, and the
	  // calculation of the new value needs to be conditionalized
	  // as well.
	  if ( layer == 1 ) {
	    up = pix_maps_spec_map (pixmaps, up);
	    lp = pix_maps_spec_map (pixmaps, lp);
	  }
	  ((float *) cl->data)[jj * cw + cind] = (up + lp) / 2.0;
	}
      }
    }

    // Now take care of the extra row, if any.
    if ( extra_row ) {
      float *mr = g_new (float, pl->size_x); // Model row in layer below.
      if ( pl->is_float_blob ) {
	float_blob_get_region ((FloatBlob *) pl->data, 0, (ch - 1) * 2,
			       pl->size_x, 1, mr);
      }
      else {
	for ( ii = 0 ; ii < pl->size_x ; ii++ ) {
	  mr[ii] = ((float *) pl->data)[(ch - 1) * 2 + ii];
	}
      }

      if ( layer == 1 ) {
	for ( ii = 0 ; ii < pl->size_x ; ii++ ) {
	  mr[ii] = pix_maps_spec_map (pixmaps, mr[ii]);
	}
      }

      for ( ii = 0 ; ii < cl->size_x - (extra_column ? 1 : 0) ; ii++ ) {
	cr[ii] = (mr[ii * 2] + mr[ii * 2 + 1]) / 2.0;
      }

      if ( extra_column ) {
	cr[cl->size_x - 1] = mr[pl->size_x - 1];
      }

      if ( cl->is_float_blob ) {
	float_blob_set_region ((FloatBlob *) cl->data, 0, ch - 1, cw, 1, cr);
      }
      else {
	for ( ii = 0 ; ii < cl->size_x ; ii++ ) {
	  ((float *) cl->data)[ii] = cr[ii];
	}
      }

      g_free (mr);
    }

    // Old functional (but weird in that it cloned a row above instead
    // of using the bottom row of the model) extra_row code.
    /*
    if ( extra_row ) {
      if ( cl->is_float_blob ) {
	float_blob_get_region ((FloatBlob *) cl->data, 0, ch - 2, cw, 1, cr);
	float_blob_set_region ((FloatBlob *) cl->data, 0, ch - 1, cw, 1, cr);
      }
      else {
	float *cld = ((float *) cl->data); // Interpreted current layer data.
	for ( ii = 0 ; ii < cw ; ii++ ) {
	  cld[cw * (ch - 1) + ii] = cld[cw * (ch - 2) + ii];
	}
      }
    }
    */

    g_free (cr);

    // Whew!
    
    g_ptr_array_add (result, cl);
  }

  return result;
}

Pyramid *
pyramid_new (const char *base_name, PixMapsSpec *pixmaps, 
	     const char *scratch_dir)
{
  // This path has worked fine in the past and seen decent testing,
  // and I haven't done anything that seems likely to beak it, but I
  // haven't run it in a while either, so this break is here.
  g_assert_not_reached ();

  Pyramid *self = g_new (Pyramid, 1);

  self->base_name = g_string_new (base_name);

  self->layers = g_ptr_array_new ();

  pyramid_layer *base_layer = form_base_layer (base_name);

  g_ptr_array_add (self->layers, base_layer);

  GString *path = g_string_new (scratch_dir);

  GPtrArray *upper_layers
    = form_upper_layers (base_layer, pixmaps, path, NULL);

  my_g_ptr_array_add_entries (self->layers, upper_layers);

  g_ptr_array_free (upper_layers, TRUE);

  self->pixmaps = pix_maps_spec_ref (pixmaps);

  self->cache = NULL;

  self->scratch_dir = path;	// Take ownership of path GString.

  self->reference_count = 1;

  return self;
}

// Get a new string containing the md5 sum of "base_name.meta",
// followed by "_", followed by the md5 sum of "base_name.img".
static GString *
get_md5_sums (const char *base_name)
{
  // Create a temporary file to hold the output of the system command.
  // IMPROVEME: there should be some way to read our own output other
  // than this goofy method of writing it to a temporary file, after
  // all perl does it.  Its not obvious to me how to do it though.
  GString *tmp_file_name = make_unique_tmp_file_name ("/tmp", "pyramid_");

  GString *meta_name = my_g_string_new_printf ("%s.meta", base_name);
  GString *img_name = my_g_string_new_printf ("%s.img", base_name);
    
  // Write md5 sum information for metadata file into temporary file.
  GString *system_command
    = my_g_string_new_printf ("md5sum %s >%s", meta_name->str,
			      tmp_file_name->str);
  int exit_code = system (system_command->str);
  g_assert (exit_code == 0);

  // Append md5 sum information for data file to temporary file.
  g_string_printf (system_command, "md5sum %s >>%s", img_name->str,
		   tmp_file_name->str);
  exit_code = system (system_command->str);
  g_assert (exit_code == 0);

  my_g_string_free (system_command);

  // Now we read the text back out of the temporary file and pick out
  // just the strings.
  GError *tmp_error = NULL;
  gchar *contents = NULL;
  g_file_get_contents (tmp_file_name->str, &contents, NULL, &tmp_error);
  if ( tmp_error != NULL ) {
    g_error ("g_file_get_contents on %s failed: %s\n", tmp_file_name->str,
	     tmp_error->message);
  }
  int return_code = g_unlink (tmp_file_name->str);
  g_assert (return_code == 0);
  my_g_string_free (tmp_file_name);
  // Everything up to the first space is the metadata md5 sum.
  GString *meta_md5sum = g_string_new ("");
  size_t ii;
  for ( ii = 0 ; !isspace (contents[ii]) ; ii++ ) {
    g_string_append_c (meta_md5sum, contents[ii]);
  }
  // Now skip everything up to the newline.
  for ( ; contents[ii] != '\n' ; ii++ ) {
    ;
  }
  // Skip newline itself.
  ii++;
  // Everything up until the next space is the data md5 sum.
  GString *data_md5sum = g_string_new ("");
  for ( ; ! isspace (contents[ii]) ; ii++ ) {
    g_string_append_c (data_md5sum, contents[ii]);
  }
  
  g_free (contents);

  GString *both_md5sums
    = my_g_string_new_printf ("%s_%s", meta_md5sum->str, data_md5sum->str);

  my_g_string_free (data_md5sum);
  my_g_string_free (meta_md5sum);

  return both_md5sums;
}

// Get a new signature string derived from the md5 sums of the data
// file and its associated metadata file, and the unique serialization
// of pixmaps.
static GString *
get_signature (const char *base_name, PixMapsSpec *pixmaps)
{
  GString *result = get_md5_sums (base_name);
  GString *pix_maps_spec = pix_maps_spec_freeze_as_gstring (pixmaps);
  g_string_append_printf (result, "_%s", pix_maps_spec->str);
  my_g_string_free (pix_maps_spec);
  
  return result;
}

Pyramid *
pyramid_new_using_cache (const char *base_name, PixMapsSpec *pixmaps,
			 PyramidCache *cache)
{
  Pyramid *self = g_new0 (Pyramid, 1);

  self->base_name = g_string_new (base_name);

  self->layers = g_ptr_array_new ();

  pyramid_layer *base_layer = form_base_layer (base_name);
  
  g_ptr_array_add (self->layers, base_layer);

  self->signature = get_signature (base_name, pixmaps);

  GPtrArray *upper_layers = pyramid_cache_try_lease (cache, self->signature);

  if ( upper_layers == NULL ) {
    GString *cache_path = g_string_new (cache->dir->str);
    GString *spaceless_signature = spaces_to_underscores (self->signature);
    upper_layers = form_upper_layers (base_layer, pixmaps, cache_path,
				      spaceless_signature->str);
    my_g_string_free (spaceless_signature);
    my_g_string_free (cache_path);
    pyramid_cache_add_entry (cache, self->signature, upper_layers);
  }

  my_g_ptr_array_add_entries (self->layers, upper_layers);

  my_g_ptr_array_really_free (upper_layers, NULL);

  self->pixmaps = pix_maps_spec_ref (pixmaps);

  self->cache = pyramid_cache_ref (cache);

  self->scratch_dir = NULL;

  self->reference_count = 1;

  return self;
}

void
pyramid_get_region (Pyramid *self, ssize_t start_x, ssize_t start_y,
		    ssize_t w, ssize_t h, ssize_t zoom, 
		    float **region, size_t *rstart_x, size_t *rstart_y,
		    size_t *rw, size_t *rh, gboolean *unowned_memory)
{
  pyramid_layer *layer_0 
    = (pyramid_layer *) g_ptr_array_index (self->layers, 0);

  g_assert (start_x >= 0 && start_x < layer_0->size_x);
  g_assert (start_y >= 0 && start_y < layer_0->size_y);
  g_assert (w > 0 && start_x + w <= layer_0->size_x);
  g_assert (h > 0 && start_y + h <= layer_0->size_y);

  // I believe this is a safe use of floating point comparison,
  // assuming log2 is implemented decently and the interface
  // requirement that zoom be a power of two is honored.
  g_assert (log2 (zoom) - floor (log2 (zoom)) == 0.0);
  int loin = log2 (zoom);	// Layer of interest number.

  pyramid_layer *loi = g_ptr_array_index (self->layers, loin);

  if ( loi->is_float_blob ) {
    // Load the region of interest from the FloatBlob for the loi.
    *rstart_x = (start_x / zoom) * zoom;
    *rstart_y = (start_y / zoom) * zoom;
    *rw = ceil ((double) (start_x + w) / zoom) - start_x / zoom;
    g_assert (*rw * zoom >= w);
    *rh = ceil ((double) (start_y + h) / zoom) - start_y / zoom;
    g_assert (*rh * zoom >= h);
    *unowned_memory = TRUE;
    *region = g_new (float, *rw * *rh);
    float_blob_get_region ((FloatBlob *) loi->data, start_x / zoom,
			   start_y / zoom, *rw, *rh, *region);
  }
  else {
    // The loi is already in memory, just return the pointer to the data.
    *rstart_x = 0;
    *rstart_y = 0;
    *rw = loi->size_x;
    *rh = loi->size_y;
    *unowned_memory = FALSE;
    *region = loi->data;
  }

  // If this was the base layer, we have to apply pixmaps.
  if ( loin == 0 && pix_maps_spec_map_count (self->pixmaps) != 0 ) {
    guint ii, jj;
    for ( jj = 0 ; jj < *rh ; jj++ ) {
      for ( ii = 0 ; ii < *rw ; ii++ ) {
	// Current pixel pointer.
	float *cpp = *region + jj * *rw + ii;
	*cpp = pix_maps_spec_map (self->pixmaps, *cpp);
      }
    }
  }
}

void
pyramid_export_as_jpeg_files (Pyramid *self, const char *base)
{
  guint ii;
  for ( ii = 0 ; ii < self->layers->len ; ii++ ) {
    pyramid_layer *cl = g_ptr_array_index (self->layers, ii);

    FloatImage *clafi = float_image_new (cl->size_x, cl->size_y);

    float *cr = g_new (float, cl->size_x);
    guint kk;
    for ( kk = 0 ; kk < cl->size_y ; kk++ ) {
      if ( cl->is_float_blob ) {
	float_blob_get_region ((FloatBlob *) cl->data, 0, kk, cl->size_x,
			       1, cr);
      }
      else {
	guint jj;
	for ( jj = 0 ; jj < cl->size_x ; jj++ ) {
	  cr[jj] = ((float *) cl->data)[kk * cl->size_x + jj];
	}
      }
      guint jj;
      for ( jj = 0 ; jj < cl->size_x ; jj++ ) {
	float_image_set_pixel (clafi, jj, kk, cr[jj]);
      }
    }
    g_free (cr);

    // If we are working on the bottom layer, we have to apply the maps.
    if ( ii == 0 ) {
      for ( kk = 0 ; kk < clafi->size_y ; kk++ ) {
	guint jj;
	for ( jj = 0 ; jj < clafi->size_x ; jj++ ) {
	  float cp = float_image_get_pixel (clafi, jj, kk);
	  float npv = pix_maps_spec_map (self->pixmaps, cp);
	  float_image_set_pixel (clafi, jj, kk, npv);
	}
      }
    }

    GString *layer_jpeg_name
      = my_g_string_new_printf ("%s_%u.jpeg", base, ii);

    size_t larger_dimension
      = (cl->size_x > cl->size_y ? cl->size_x : cl->size_y);

    float_image_export_as_jpeg (clafi, layer_jpeg_name->str, larger_dimension,
				NAN);

    my_g_string_free (layer_jpeg_name);

    float_image_free (clafi);
  }
}

void
pyramid_export_as_float_blobs (Pyramid *self, const char *base)
{
  guint ii;
  for ( ii = 0 ; ii < self->layers->len ; ii++ ) {
    pyramid_layer *cl = g_ptr_array_index (self->layers, ii);

    GString *layer_file_name
      = my_g_string_new_printf ("%s_%u.raw", base, ii);

    FILE *lfp = fopen (layer_file_name->str, "w"); // Layer file pointer.
    g_assert (lfp != NULL);

    my_g_string_free (layer_file_name);

    float *cr = g_new (float, cl->size_x); // Buffer for current row.
    guint kk;
    for ( kk = 0 ; kk < cl->size_y ; kk++ ) {

      if ( cl->is_float_blob ) {
	float_blob_get_region ((FloatBlob *) cl->data, 0, kk, cl->size_x,
			       1, cr);
      }

      else {
	guint jj;
	for ( jj = 0 ; jj < cl->size_x ; jj++ ) {
	  cr[jj] = ((float *) cl->data)[kk * cl->size_x + jj];
	}
      }

      // If we are working on the bottom layer, apply the maps.
      if ( ii == 0 ) {
	guint jj;
	for ( jj = 0 ; jj < cl->size_x ; jj++ ) {
	  cr[jj] = pix_maps_spec_map (self->pixmaps, cr[jj]);
	}
      }

      size_t floats_written = fwrite (cr, sizeof (float), cl->size_x, lfp);
      g_assert (floats_written == cl->size_x);
    }
    g_free (cr);

    int return_code = fclose (lfp);
    g_assert (return_code == 0);
  }
}

Pyramid *
pyramid_ref (Pyramid *self)
{
  self->reference_count++;

  return self;
}

void
pyramid_unref (Pyramid *self)
{
  self->reference_count--;

  if ( self->reference_count == 0 ) {

    // Free or unref the layer data.
    gint ii;
    for ( ii = 0 ; ii < self->layers->len ; ii++ ) {
      pyramid_layer *cl = g_ptr_array_index (self->layers, ii);
      if ( cl->is_float_blob ) {
	// If we are using a cache, then the layers that are FloatBlob
	// instances should either have come from the cache
	// originally, or we have gifted them to the cache after
	// creating them.
	if ( self->cache != NULL ) {
	  g_assert (!((FloatBlob *) cl->data)->is_file_owner);
	}
	float_blob_unref ((FloatBlob *) cl->data);
      }
      else {
	g_free (cl->data);
      }
      g_free (cl);
    }

    g_ptr_array_free (self->layers, TRUE);

    if ( self->cache != NULL ) {
      pyramid_cache_release (self->cache, self->signature);
      pyramid_cache_unref (self->cache);
    }
    else {
      g_assert (self->scratch_dir != NULL);
      g_string_free (self->scratch_dir, TRUE);
    }

    g_string_free (self->base_name, TRUE);

    g_string_free (self->signature, TRUE);

    pix_maps_spec_unref (self->pixmaps);

    g_free (self);
  }
}
