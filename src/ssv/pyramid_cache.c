// Implementation of the interface described in pyramid_cache.h.

#include <ctype.h>
#include <math.h>
#include <string.h>
#include <time.h>

#include <glib/gstdio.h>

#include "float_blob.h"
#include "pyramid.h"
#include "pyramid_cache.h"
#include "utilities.h"

PyramidCache *
pyramid_cache_new (const char *path, gint64 max_size)
{
  PyramidCache *self = g_new (PyramidCache, 1);

  self->dir = g_string_new (path);
  ensure_slash_terminated (self->dir);

  g_assert (max_size > 0);
  self->max_size = max_size;

  GString *index = g_string_new (self->dir->str);
  g_string_append_printf (index, "index");

  self->tf = table_file_new (index->str);

  self->reader_locks
    = g_hash_table_new_full ((GHashFunc) g_string_hash,
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free,
			     NULL);

  self->writer_locks
    = g_hash_table_new_full ((GHashFunc) g_string_hash,
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free,
			     NULL);

  self->reference_count = 1;

  return self;
}

// Comparator which returns less than, equal to, or greater than zero
// if *ap is greater than, equal to, or smaller than *bp in total
// area.
static gint
compare_layers_by_area (pyramid_layer **ap, pyramid_layer **bp)
{
  pyramid_layer *a = *ap, *b = *bp;

  size_t a_area = a->size_x * a->size_y, b_area = b->size_x * b->size_y;

  if ( a_area > b_area ) {
    return -1;
  }
  else if ( a_area < b_area ) {
    return 1;
  }
  else {
    return 0;
  }
}

// Load layers as FloatBlob instances from files "dir/base_1",
// "dir/base_2", etc., where base_1 has dimension size_x by size_y,
// and successive layers have dimensions half those of the layer
// below, rounded up.  The dir argument can have a trailing '/'
// character or not, things will work either way.  The result is
// returned as a new array of pointers to new FloatBlob instances
// using the files in the cache.
static GPtrArray *
load_layers (GString *dir, GString *base, size_t size_x, size_t size_y)
{
  GPtrArray *result = g_ptr_array_new ();

  // IMPROVEME: this is sort of an ass-backward way of doing things.
  // Since we have to take the size of the base layer as an argument
  // anyway, we can determine exactly how many layers we should find,
  // and just open the appropriate file names, rather than looking at
  // all the file names to find the ones we want.  This approach has
  // the small virtue of helping to ensure that our cache isn't full
  // of unexpected junk, however, so I've kept it for now.

  // Open cache directory.
  GError *err = NULL;
  GDir *cd = g_dir_open (dir->str, 0, &err);
  g_assert ((cd == NULL && err != NULL) || (cd != NULL && err == NULL));
  if ( err != NULL ) {
    fprintf (stderr, "failed to open pyramid cache directory %s: %s\n",
	     dir->str, err->message);
    exit (EXIT_FAILURE);
  }

  // Look for layers matching base in the cache directory...
  const char *ce;
  while ( (ce = g_dir_read_name (cd)) != NULL ) {

    if ( strncmp (ce, base->str, base->len) == 0 ) {

      // Read the layer number from the file name.
      int layer_number;
      int scan_count = sscanf (ce + base->len, "_%d", &layer_number);
      g_assert (scan_count == 1);

      // Determine the dimensions of the current layer.
      size_t lsx = size_x, lsy = size_y;
      int ii;
      for ( ii = layer_number ; ii > 1 ; ii-- ) {
	lsx = ceil (lsx / 2.0);
	lsy = ceil (lsy / 2.0);
      }

      // Form a new pyramid layer from the file.
      pyramid_layer *npl = g_new (pyramid_layer, 1);
      npl->size_x = lsx;
      npl->size_y = lsy;
      npl->is_float_blob = TRUE;
      GString *full_path = my_g_string_new_printf ("%s%s", dir->str, ce);
      npl->data = float_blob_new_using (lsx, lsy, full_path->str, FALSE);
      my_g_string_free (full_path);

      // Add new layer to array to be returned (we'll sort it later).
      g_ptr_array_add (result, npl);
    }
  }

  g_dir_close (cd);

  g_ptr_array_sort (result, (GCompareFunc) compare_layers_by_area);

  // We had now better have layers with the correct sizes, and a top
  // layer with one or the other dimensions equal to one.
  pyramid_layer *layer_1 = g_ptr_array_index (result, 0);
  g_assert (layer_1->size_x == size_x && layer_1->size_y == size_y);
  // Previous layer size_x and size_y;
  size_t plsx = layer_1->size_x, plsy = layer_1->size_y;
  guint ii;
  for ( ii = 1 ; ii < result->len ; ii++ ) {
    pyramid_layer *cl = g_ptr_array_index (result, ii);
    g_assert (cl->size_x == ceil (plsx / 2.0)
	      && cl->size_y == ceil (plsy / 2.0));
    plsx = cl->size_x;
    plsy = cl->size_y;
  }
  pyramid_layer *top_layer = g_ptr_array_index (result, result->len - 1);
  g_assert (top_layer->size_x == 1 || top_layer->size_y == 1);

  // Now that we have all the layers loaded, we go through and honor
  // the PYRAMID_BIGGEST_LAYER_IN_MEMORY property of the interface by
  // replacing the upper FloatBlob instances with plain old memory
  // regions.
  for ( ii = 0 ; ii < result->len ; ii++ ) {
    pyramid_layer *cl = g_ptr_array_index (result, ii);
    if ( cl->size_x * cl->size_y * sizeof (float)
	 < PYRAMID_BIGGEST_LAYER_IN_MEMORY ) {
      FloatBlob *cb = cl->data;
      cl->data = g_new (float, cb->size_x * cb->size_y);
      float_blob_get_region (cb, 0, 0, cb->size_x, cb->size_y, cl->data);
      cl->is_float_blob = FALSE;
      float_blob_unref (cb);
    }
  }

  return result;
}

// Find the offset in entry of the value part of the field with name
// field_name in entry consisting of space seperated pairs of the form
// "key=value", or -1 if no field with name field_name is found.
static gssize
find_field_value_offset (GString *entry, const char *field_name)
{
  // Require that the field_name not be outrageously huge.
  const size_t max_field_name_length = 300;
  // Field name length.
  size_t fnl = strnlen (field_name, max_field_name_length + 1);
  g_assert (fnl <= max_field_name_length);

  gboolean found = FALSE; // TRUE iff we have found the entry.
  gssize explored = 0; // Portion of string explored so far.
  for ( ; ; ) {
    size_t equals_sign_position = strcspn (entry->str + explored, "=");
    
    if ( entry->str[explored + equals_sign_position] != '=' ) {
      // Hit end of string without any luck.
      break;
    }
    
    // Did we find the field we want?
    if ( explored + equals_sign_position >= fnl ) {
      if ( strncmp (entry->str + explored + equals_sign_position - fnl,
		    field_name, fnl) == 0 ) {
	found = TRUE;
	return explored + equals_sign_position + 1;
      }
    }

    // We have now explored up through another '=' character.
    explored += equals_sign_position + 1;
  }
  
  return -1;
}

// For entry consisting of space seperated pairs of the form
// "key=value", return as a new GString instance the value associated
// with key "state", or NULL if the key "state" isn't found.

// Use find_field_value_offset to find the value of the "state" field
// in entry, and return a new GString containg this field's value.
static GString *
entry_get_state (GString *entry)
{
  gssize fvo = find_field_value_offset (entry, "state");

  if ( fvo == -1 ) {
    return NULL;
  }
  else {
    const gsize max_value_length = 300, slop = 5;
    // FIIXME: I think filling in a presized GString works but created
    // an essentially broken GString that doesn't necessarily know its
    // own length (at the moment we don't use any of the gstring
    // interface functions so it doesn't matter).
    GString *result = g_string_sized_new (max_value_length + slop);
    GString *format
      = my_g_string_new_printf ("%%%ds", max_value_length - 1);
    int match_count = sscanf (entry->str + fvo, format->str, result->str);
    g_assert (match_count == 1);
    my_g_string_free (format);
    g_assert (strnlen (result->str, max_value_length + 1)
	      <= max_value_length);
    return result;
  }
}

// Use find_field_value_offset to find the "state" field in entry, and
// replace the existing value of this field with new_state.  If we
// don't find the field it is added to entry.
static void
entry_set_state (GString *entry, const char *new_state)
{
  // Find the value part of the state field.
  gssize fvo = find_field_value_offset (entry, "state");

  // If we found the field, replace it.
  if ( fvo != -1 ) {
    gchar *vsp = entry->str + fvo; // Value start pointer.
    gssize vl = 0; // Value length.
    while ( !isspace (vsp[vl]) && vl < strlen (vsp) ) {
      vl++;
    }
    g_string_erase (entry, fvo, vl);
    g_string_insert (entry, fvo, new_state);
  }
  // If we didn't find it, add it.
  else {
    g_string_append_printf (entry, " state=%s", new_state);
  }
}

// Analogous to entry_get_state, but fetches the value of a field
// named field_name whose valuse is a size_t.
static ssize_t
entry_get_size_field (GString *entry, const char *field_name)
{
  gssize fvo = find_field_value_offset (entry, field_name);

  if ( fvo == -1 ) {
    // We could return a negative sentinel value, but we aren't really
    // set up to do so.
    g_assert_not_reached ();
    return -1;
  }
  else {
    int result_as_int;
    int match_count = sscanf (entry->str + fvo, "%d", &result_as_int);
    g_assert (match_count == 1);
    g_assert (result_as_int >= 0);
    return result_as_int;
  }
}

// Analogous to entry_set_state, but sets the value of a field named
// field_name the value of which is a size_t.
static void
entry_set_size_field (GString *entry, const char *field_name, size_t size)
{
  // Find the value part of the size field.
  gssize fvo = find_field_value_offset (entry, field_name);

  // New value string.
  GString *nvs = my_g_string_new_printf ("%llu", (unsigned long long) size);

  // If we found the field, replace it.
  if ( fvo != -1 ) {
    gchar *vsp = entry->str + fvo; // Value start pointer.
    gssize vl = 0; // Value length.
    while ( !isspace (vsp[vl]) && vl < strlen (vsp) ) {
      vl++;
    }
    g_string_erase (entry, fvo, vl);
    g_string_insert (entry, fvo, nvs->str);
  }
  // If we didn't find it, add it.
  else {
    g_string_append_printf (entry, " %s=%s", field_name, nvs->str);
  }

  my_g_string_free (nvs);
}

// Use fine_field_value_offset to find the value of the "date" field
// in entry, and return the date value.  If we don't find the date, we
// return the impossible (in this context) sentinel value 0.
static time_t
entry_get_date (GString *entry)
{
  gssize fvo = find_field_value_offset (entry, "date");

  if ( fvo == -1 ) {
    return 0;
  }
  else {
    const gsize max_value_length = 30;
    unsigned long long int result;
    GString *format
      = my_g_string_new_printf ("%%%dllu", max_value_length - 1);
    int match_count = sscanf (entry->str + fvo, format->str, &result);
    g_assert (match_count == 1);
    my_g_string_free (format);

    return (time_t) result;
  }
}

// Use find_field_value_offset to find the "date" field in entry, and
// replace the existing value of this field with date.  If the field
// isn't already present it is added.
static void
entry_set_date (GString *entry, time_t date)
{
  // Find the value part of the date field.
  gssize fvo = find_field_value_offset (entry, "date");

  // If we found the field, replace it.
  if ( fvo != -1 ) {
    gchar *vsp = entry->str + fvo; // Value start pointer.
    gssize vl = 0; // Value length.
    while ( !isspace (vsp[vl]) && vl < strlen (vsp) ) {
      vl++;
    }
    g_string_erase (entry, fvo, vl);
    g_assert (sizeof (time_t) <= sizeof (unsigned long long int));
    unsigned long long int time_as_llui = date;
    GString *date_as_string = g_string_new ("");
    g_string_append_printf (date_as_string, "%llu", time_as_llui);
    g_string_insert (entry, fvo, date_as_string->str);
  }
  // If we didn't find it, add it.
  else {
    g_string_append_printf (entry, " date=%llu",
			    (unsigned long long int) date);
  }  
}

GPtrArray *
pyramid_cache_try_lease (PyramidCache *self, GString *signature)
{
  GPtrArray *result;

  TableFile *tf = self->tf;	// Convenience alias.

  // True iff we have locked the field one way or another: either a
  // read lock of finished data, or a write lock on an unfinished
  // field so the add_entry method can be called, as described in the
  // interface.
  gboolean locked = FALSE;
  
  while ( !locked ) {

    //    trmsg ("Pre table_reader_lock in %s line %d", __func__, __LINE__);
    table_file_table_reader_lock (tf);
    //    trmsg ("Post table_reader_lock in %s line %d", __func__, __LINE__);

    //    trmsg ("Pre field_reader_lock in %s line %d", __func__, __LINE__);
    gboolean lock_results = table_file_field_reader_lock (tf, signature->str);
    //    trmsg ("Post field_reader_lock in %s line %d", __func__, __LINE__);
    
    if ( lock_results ) {
      GString *fv = table_file_get_field_value (tf, signature->str);
      GString *state = entry_get_state (fv);
      if ( strncmp (state->str, "finished", strlen ("finished")) == 0 ) {
	size_t size_x = entry_get_size_field (fv, "size_x");
	size_t size_y = entry_get_size_field (fv, "size_y");
	GString *spaceless_signature = spaces_to_underscores (signature);
	result = load_layers (self->dir, spaceless_signature, size_x, size_y);
	// IMPROVEME: Ideally we would update the date field here, in
	// order to implement an LRU approach to unused cache entry
	// removal.  But it requires more monkeying around with table
	// locks (so that we can reset the date field value) and just
	// removing the ones added longer ago first is probably a good
	// enough heuristic for our purposes here.
	my_g_string_free (spaceless_signature);
	g_hash_table_insert (self->reader_locks,
			     g_string_new (signature->str), NULL);
	locked = TRUE;
      }
      else {
	// trmsg ("Pre field_reader_unlock in %s line %d", __func__, __LINE__);
	table_file_field_reader_unlock (tf, signature->str);
	// trmsg ("Post field_reader_unlock in %s line %d", __func__, __LINE__);
	// trmsg ("Pre field_writer_lock in %s line %d", __func__, __LINE__);
	gboolean field_lock_result
	  = table_file_field_writer_lock (tf, signature->str);
	// trmsg ("Post field_writer_lock in %s line %d", __func__, __LINE__);
	if ( field_lock_result ) {
	  // We could clean up the half-finished glop somebody left
	  // here, but there isn't much point: left over fields will
	  // get truncated anyway, and it doesn't help the general
	  // cache growth problem.
	  result = NULL;
	  g_hash_table_insert (self->writer_locks,
			       g_string_new (signature->str), NULL);
	  entry_set_state (fv, "empty");
	  entry_set_date (fv, time (NULL));
	  table_file_set_field_value (tf, signature->str, fv->str);
	  locked = TRUE;
	}
      }
      my_g_string_free (state);
      // trmsg ("Pre table_reader_unlock in %s line %d", __func__, __LINE__);
      table_file_table_reader_unlock (tf);
      // trmsg ("Post table_reader_unlock in %s line %d", __func__, __LINE__);
    }

    else {
      // IMPROVEME: it would be nice if we could acquire a writer lock
      // without first relinquishing the reader lock and opening
      // ourselves up to the possibility of another thread slipping in
      // and causing trouble.  I believe this implementation always
      // works correctly (i.e. slip in can happen and things still
      // work), but the impossibility of upgrading a lock makes it
      // much trickier to prove that everything is correct.
      // trmsg ("Pre table_reader_unlock in %s line %d", __func__, __LINE__);
      table_file_table_reader_unlock (tf);
      // trmsg ("Post table_reader_unlock in %s line %d", __func__, __LINE__);
      // trmsg ("Pre table_writer_lock in %s line %d", __func__, __LINE__);
      table_file_table_writer_lock (tf);
      // trmsg ("Post table_writer_lock in %s line %d", __func__, __LINE__);
      gboolean add_field_result
	= table_file_add_field (tf, signature->str, "");
      if ( add_field_result ) {
	result = NULL;
	g_hash_table_insert (self->writer_locks, g_string_new (signature->str),
			     NULL);
	GString *nfv = g_string_new (""); // New field value.
	entry_set_state (nfv, "empty");
	entry_set_date (nfv, time (NULL));
	table_file_set_field_value (tf, signature->str, nfv->str);
	my_g_string_free (nfv);
	locked = TRUE;
      }
      // trmsg ("Pre table_writer_unlock in %s line %d", __func__, __LINE__);
      table_file_table_writer_unlock (tf);
      // trmsg ("Post table_writer_unlock in %s line %d", __func__, __LINE__);
    }
  }

  return result;
}

// Return the total disk usage of all files in dir, in million-byte
// megabytes (rounded down).  This function is just good enough to get
// a rough idea of space usage of a directory.
static gint64
disk_usage (GString *dir)
{
  // trmsg ("Doing disk_usage...\n");
  GError *err = NULL;
  GDir *dirp = g_dir_open (dir->str, 0, &err);
  if ( dirp == NULL ) {
    g_printerr ("%s: failed to open directory '%s': %s", g_get_prgname (),
		dir->str, err->message);
    exit (EXIT_FAILURE);
  }

  gint64 total_bytes = 0;

  gchar *file;
  while ( (file = (gchar *) g_dir_read_name (dirp)) != NULL ) {
    struct stat stat_buf;
    GString *file_path = g_string_new (dir->str);
    g_string_append_printf (file_path, file);
    int return_code = g_lstat (file_path->str, &stat_buf);
    g_assert (return_code == 0);
    total_bytes += stat_buf.st_size;
  }

  g_dir_close (dirp);

  // trmsg ("Done with disk_usage");

  // Return megabytes, near enough for our purposes.
  return total_bytes / 1000000;
}

// Remove entry from cache.  We have to hold writer locks on self->tf
// and the entry field of self->tf before calling this function.
static void
remove_entry (PyramidCache *self, GString *entry)
{
  // Make the entry as deleted.  This way no one will try to use a
  // partially deleted entry, yet we can still find and remove entry
  // files later (in case we get interrupted).
  GString *fv = table_file_get_field_value (self->tf, entry->str);  
  entry_set_state (fv, "empty");
  table_file_set_field_value (self->tf, entry->str, fv->str);
  
  g_string_free (fv, TRUE);

  // Now remove all the layer files we can the correspond to entry.
  GString *spaceless_signature = spaces_to_underscores (entry);
  GString *rm_command = g_string_new ("rm ");
  g_string_append_printf (rm_command, "%s%s_*", self->dir->str,
			  spaceless_signature->str);
  int exit_code = system (rm_command->str);
  g_assert (exit_code == 0);

  g_string_free (rm_command, TRUE);
  g_string_free (spaceless_signature, TRUE);

  // Now that the cache entry is truly gone, we can actually remove
  // the table entry.
  table_file_remove_field (self->tf, entry->str);
}

void
pyramid_cache_add_entry (PyramidCache *self, GString *signature,
			 GPtrArray *layers)
{
  // Figure out how much bigger the new layers will make the cache, in
  // megabytes.
  gsize expected_cache_growth = 0;
  guint ii;
  for ( ii = 0 ; ii < layers->len ; ii++ ) {
    pyramid_layer *cl = g_ptr_array_index (layers, ii);
    // The layers that use FloatBlob instances are required to have
    // already created them in the cache directory, so we avoid
    // counting them again here.
    if ( ! cl->is_float_blob ) {
      expected_cache_growth += sizeof (float) * cl->size_x * cl->size_y;
    }
  }
  expected_cache_growth /= 1000000;

  // If the cache directory looks too big, try to remove old entries
  // until we've got it half free again (we go down to half so we
  // don't end up having to remove something every time and always
  // being slow).  IMPROVEME: its sloppy and potentially very slow to
  // use disk_usage() to recompute the space the cache uses each time.
  if ( disk_usage (self->dir) + expected_cache_growth > self->max_size ) {
    // Ug, we only have trylock for fields, not yet for the entire
    // table lock.  So even the cheap hack strategy outlined below in
    // the non-running section doesn't work.  I'm out of time and
    // inclination to add it, so instead of any actual cache cleaning,
    // we'll just emit a message for the user to read saying that
    // their cache is getting BIG.
    g_print ("HEY, your image cache is getting BIIIIG.  You might want\n"
	     "to go delete the contents of directory '%s'.\n", self->dir->str);
  } else if ( FALSE ) {
    // FIIXME: Well doing a table writer lock here can lead to deadlock
    // with others that are just trying to innocently use the cache.
    // Fixing this might well require implementing lock upgrading
    // (reader-to-writer) or some other hideous backflip, so we just
    // give up and don't clean the cache if the lock fails.  It will
    // hopefully get cleaned out sometime when things arr running
    // single-threaded.
    //    trmsg ("Pre table_writer_trylock in %s,  %d", __func__, __LINE__);
    //    gboolean lock_result = table_file_table_writer_trylock (self->tf);
    //    trmsg ("Post table_writer_trylock in %s,  %d", __func__, __LINE__);
    // We need to define a lock_result so this crud compiles.
    gboolean lock_result = FALSE;
    if ( lock_result ) {
      while ( disk_usage (self->dir) + expected_cache_growth
	      > self->max_size / 2.0 ) {
	GPtrArray *cat = table_file_catalog (self->tf);
	// We will hold a number of field locks for a while, this
	// remembers which ones they are so we can unlock them again.
	GPtrArray *fields_to_unlock = g_ptr_array_new ();
	GString *oldest_entry = NULL;
	// We're going to search for the oldest entry, so we
	// initialize our notion of the oldest entry date to a time
	// comfortably in the future :)
	time_t oldest_entry_date = time (NULL) + 10000;
	for ( ii = 0 ; ii < cat->len ; ii++ ) {
	  GString *ce = g_ptr_array_index (cat, ii);
	  // If we hold a writer lock on the field, we are presumably
	  // in the process of adding, so we don't want to consider it
	  // for removal.
	  if ( my_g_hash_table_entry_exists (self->writer_locks, ce) ) {
	    continue;
	  }
	  // trmsg ("Pre field_writer_trylock in %s,  %d", __func__, __LINE__);
	  gboolean lock_result
	    = table_file_field_writer_trylock (self->tf, ce->str, NULL);
	  // trmsg ("Post field_writer_trylock in %s,  %d", __func__, __LINE__);
	  if ( lock_result ) {
	    g_ptr_array_add (fields_to_unlock, ce);
	    GString *entry = table_file_get_field_value (self->tf, ce->str);
	    time_t entry_date = entry_get_date (entry);
	    g_string_free (entry, TRUE);
	    if ( entry_date < oldest_entry_date ) {
	      oldest_entry = ce;
	      oldest_entry_date = entry_date;
	    }
	  }
	}
	if ( oldest_entry != NULL ) {
	  remove_entry (self, oldest_entry);
	}
	for ( ii = 0 ; ii < fields_to_unlock->len ; ii++ ) {
	  GString *ce = g_ptr_array_index (fields_to_unlock, ii);
	  // trmsg ("Pre field_writer_unlock in %s line %d", __func__, __LINE__);
	  table_file_field_writer_unlock (self->tf, ce->str);
	  // trmsg ("Post field_writer_unlock in %s line %d", __func__, __LINE__);
	}
	g_ptr_array_free (fields_to_unlock, TRUE);
	for ( ii = 0 ; ii < cat->len ; ii++ ) {
	  GString *ce = g_ptr_array_index (cat, ii);
	  g_string_free (ce, TRUE);
	}
	g_ptr_array_free (cat, TRUE);
	// If we go through the entire list and don't find anything to
	// remove, we're done, since apparently there isn't anything
	// to remove.
	if ( oldest_entry == NULL ) { 
	  break;
	}
      }
      // trmsg ("Pre table_writer_unlock in %s line %d", __func__, __LINE__);
      table_file_table_writer_unlock (self->tf);
      // trmsg ("Post table_writer_unlock in %s line %d", __func__, __LINE__);
    }
  }

  g_assert (my_g_hash_table_entry_exists (self->writer_locks, signature));

  GString *entry = table_file_get_field_value (self->tf, signature->str);

  GString *state = entry_get_state (entry);
  g_assert (strcmp (state->str, "empty") == 0);
  my_g_string_free (state);

  entry_set_state (entry, "building");

  // Add the actual layer files here, stealing FloatBlob files or
  // creating new ones as needed.
  for ( ii = 0 ; ii < layers->len ; ii++ ) {
    pyramid_layer *cl = g_ptr_array_index (layers, ii);
    // IMPROVEME: the translation from signatures to layer names is
    // undocumentedly bizarre and should be consolidated somehow.  It
    // involves changing spaces to underscores and appending the layer
    // number.
    GString *tmp
      = my_g_string_new_printf ("%s_%u", signature->str, ii + 1);
    GString *blob_file_name = spaces_to_underscores (tmp);
    g_string_free (tmp, TRUE);

    if ( cl->is_float_blob ) {
      GString *layer_file_name = float_blob_steal_file (cl->data);

      // Make sure the path the FloatBlob was using to store its
      // backing file is the one the cache expects.
      gchar *layer_file_path = g_path_get_dirname (layer_file_name->str);
      GString *layer_file_path_gstring = g_string_new (layer_file_path);
      ensure_slash_terminated (layer_file_path_gstring);
      g_assert (g_string_equal (layer_file_path_gstring, self->dir));

      // Make sure the file the FloatBlob was using as its backing
      // store is the one expected.
      gchar *layer_file_name_without_dir
	= g_path_get_basename (layer_file_name->str);
      g_assert (strcmp (layer_file_name_without_dir, blob_file_name->str)
		== 0);

      // Free left over stuff from the file theft and the above
      // consistency checks.
      g_free (layer_file_name_without_dir);
      my_g_string_free (layer_file_path_gstring);
      g_free (layer_file_path);
      my_g_string_free (layer_file_name);
    }

    else {
      FloatBlob *tmp_blob 
	= float_blob_new (cl->size_x, cl->size_y, self->dir->str,
			  blob_file_name->str);
      float_blob_set_region (tmp_blob, 0, 0, cl->size_x, cl->size_y,
			     cl->data);
      GString *junk = float_blob_steal_file (tmp_blob);
      my_g_string_free (junk);
    }
    my_g_string_free (blob_file_name);
  }

  pyramid_layer *layer_1 = g_ptr_array_index (layers, 0);
  entry_set_size_field (entry, "size_x", layer_1->size_x);
  entry_set_size_field (entry, "size_y", layer_1->size_y);

  entry_set_state (entry, "finished");

  table_file_set_field_value (self->tf, signature->str, entry->str);

  my_g_string_free (entry);

  // We have to briefly read lock the entire table here in order to
  // safely downgrade the field writer lock we hold to a reader lock.
  // trmsg ("Pre table_reader_lock in %s line %d", __func__, __LINE__);
  table_file_table_reader_lock (self->tf);
  // trmsg ("Post table_reader_lock in %s line %d", __func__, __LINE__);
  // trmsg ("Pre field_writer_unlock in %s line %d", __func__, __LINE__);
  table_file_field_writer_unlock (self->tf, signature->str);
  // trmsg ("Post field_writer_unlock in %s line %d", __func__, __LINE__);
  gboolean remove_result = g_hash_table_remove (self->writer_locks, signature);
  g_assert (remove_result);
  // trmsg ("Pre field_reader_lock in %s line %d", __func__, __LINE__);
  table_file_field_reader_lock (self->tf, signature->str);
  // trmsg ("Post field_reader_lock in %s line %d", __func__, __LINE__);
  g_hash_table_insert (self->reader_locks, signature, NULL);
  // trmsg ("Pre table_reader_unlock in %s line %d", __func__, __LINE__);
  table_file_table_reader_unlock (self->tf);
  // trmsg ("Post table_reader_unlock in %s line %d", __func__, __LINE__);
}

void
pyramid_cache_release (PyramidCache *self, GString *signature)
{
  // We should never be in a situation where we think we have both a
  // reader lock and a writer lock on a field.
  g_assert (!(my_g_hash_table_entry_exists (self->reader_locks, signature)
	      && my_g_hash_table_entry_exists (self->writer_locks,
					       signature)));

  if ( my_g_hash_table_entry_exists (self->reader_locks, signature) ) {
    // trmsg ("Pre field_reader_unlock in %s line %d", __func__, __LINE__);
    table_file_field_reader_unlock (self->tf, signature->str);
    // trmsg ("Post field_reader_unlock in %s line %d", __func__, __LINE__);
    g_hash_table_remove (self->reader_locks, signature);
  }
  else if ( my_g_hash_table_entry_exists (self->writer_locks, signature) ) {
    // trmsg ("Pre field_reader_unlock in %s line %d", __func__, __LINE__);
    table_file_field_reader_unlock (self->tf, signature->str);
    // trmsg ("Post field_reader_unlock in %s line %d", __func__, __LINE__);
    g_hash_table_remove (self->reader_locks, signature);    
  }
  else {
    g_error ("%s called for signature %s when no reader or writer lock "
	     "was held by the instance the method was invoked on",
	     __func__, signature->str);
  }
}

PyramidCache *
pyramid_cache_ref (PyramidCache *self)
{
  self->reference_count++;

  return self;
}

void
pyramid_cache_unref (PyramidCache *self)
{
  self->reference_count--;

  if ( self->reference_count == 0 ) {
    g_assert (g_hash_table_size (self->reader_locks) == 0);
    g_assert (g_hash_table_size (self->writer_locks) == 0);
    my_g_string_free (self->dir);
    table_file_unref (self->tf);
    g_hash_table_destroy (self->reader_locks);
    g_hash_table_destroy (self->writer_locks);
    g_free (self);
  }
}

/*  LocalWords:  necesarilly
 */
