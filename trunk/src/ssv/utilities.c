// Implementation of the interface described in utilities.h.

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <semaphore.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib.h>
#include <glib/gstdio.h>

#include "utilities.h"

GQuark
my_g_utilities_error_quark (void)
{
  return g_quark_from_static_string ("my-g-utilities-error-quark");
}

off_t
file_size (int fd)
{
  struct stat stat_buf;

  int return_code = fstat (fd, &stat_buf);
  g_assert (return_code == 0);

  return stat_buf.st_size;
}

unsigned int
get_next_serial_number (void)
{
  static unsigned int current_number = 0;
  unsigned int ret_val;
  static GStaticMutex serial_number_mutex = G_STATIC_MUTEX_INIT;
  
  g_static_mutex_lock (&serial_number_mutex);
  {
    ret_val = current_number;
    current_number++;
    g_assert (current_number < UINT_MAX);
  }
  g_static_mutex_unlock (&serial_number_mutex);
  
  return ret_val;
}

GString *
ensure_slash_terminated (GString *string)
{
  if ( string->len == 0 || (string->str)[string->len - 1] != '/' ) {
    g_string_append_c (string, '/');
  }

  return string;
}

GString *
spaces_to_underscores (GString *string)
{
  GString *result = g_string_new (string->str);

  gsize ii;
  for ( ii = 0 ; ii < result->len ; ii++ ) {
    if ( isspace ((result->str)[ii]) ) {
      (result->str)[ii] = '_';
    }
  }

  return result;
}

GString *
make_unique_tmp_file_name (const char *directory, const char *prefix)
{
  const gint max_hostname_length = 1000;
  char *hostname = g_new (char, max_hostname_length + 1);
  int return_code = gethostname (hostname, max_hostname_length + 1);
  g_assert (return_code == 0);

  GString *pts = pid_thread_string ();

  unsigned int serial_number = get_next_serial_number ();

  GString *result = ensure_slash_terminated (g_string_new (directory));
  g_assert (sizeof (pid_t) <= sizeof (long long int));
  g_string_append_printf (result, "%s%s_%s_%u", prefix, hostname, pts->str,
			  serial_number);

  return result;
}

GString *
my_g_string_new_printf (const char *format, ...)
{
  // Maximum length of new string (not including trailing NUL).
  const size_t max_length = 10000;

  char *racp = g_new (char, max_length + 1); // Result as char pointer.

  va_list ap;
  va_start (ap, format);
  int char_count = vsnprintf (racp, max_length + 1, format, ap);
  va_end (ap);
  g_assert (char_count <= max_length);
  
  GString *result = g_string_new (racp);

  g_free (racp);

  return result;
}

void
my_g_string_free (GString *instance)
{
  g_string_free (instance, TRUE);
}

gboolean
my_g_hash_table_entry_exists (GHashTable *hash_table, gconstpointer key)
{
  // We can't distinguish NULL (or ging or guint with a value of 0)
  // that we set as the value of the entry from NULL returned by
  // g_hash_table_lookup, so we have these don't-care values and use
  // the g_hash_table_lookup_extended method.
  gpointer key_space = NULL, value_space = NULL;
  gboolean result
    = g_hash_table_lookup_extended (hash_table, key, &key_space, &value_space);

  return result;
}

// Add key to keys, ignoring value.  Would be a lambda function if C
// had them.
static void
my_g_hash_table_keys_helper (gpointer key, gpointer value, GPtrArray *keys)
{
  g_ptr_array_add (keys, key);
}

GPtrArray *
my_g_hash_table_keys (GHashTable *hash_table)
{
  GPtrArray *result = g_ptr_array_sized_new (g_hash_table_size (hash_table));

  g_hash_table_foreach (hash_table, (GHFunc) my_g_hash_table_keys_helper,
			result);

  return result;
}

// Ignore key, and add value to values.  Would be a lambda function if
// C had them.
static void
my_g_hash_table_values_helper (gpointer key, gpointer value,
			       GPtrArray *values)
{
  g_ptr_array_add (values, value);
}

GPtrArray *
my_g_hash_table_values (GHashTable *hash_table)
{
  GPtrArray *result = g_ptr_array_sized_new (g_hash_table_size (hash_table));

  g_hash_table_foreach (hash_table, (GHFunc) my_g_hash_table_values_helper,
			result);

  return result;
}

// A type to bind two arrays together so they can be passed to a
// callback that takes a single data pointer argument.
typedef struct {
  GPtrArray *keys;
  GPtrArray *values;
} my_g_hash_table_keys_and_values_helper_type;

// Add key and value to the lists in *helper.  Would be a lambda
// function if C had them.
static
void my_g_hash_table_keys_and_values_helper 
  (gpointer key, gpointer value,
   my_g_hash_table_keys_and_values_helper_type *helper)
{
  g_ptr_array_add (helper->keys, key);
  g_ptr_array_add (helper->values, value);
}

void
my_g_hash_table_keys_and_values (GHashTable *hash_table, GPtrArray **keys,
				 GPtrArray **values)
{
  guint table_size = g_hash_table_size (hash_table);

  *keys = g_ptr_array_sized_new (table_size);
  *values = g_ptr_array_sized_new (table_size);

  my_g_hash_table_keys_and_values_helper_type helper;
  helper.keys = *keys;
  helper.values = *values;

  g_hash_table_foreach (hash_table, 
			(GHFunc) my_g_hash_table_keys_and_values_helper, 
			&helper);
}

void
my_g_ptr_array_add_entries (GPtrArray *self, GPtrArray *other)
{
  guint ii;
  for ( ii = 0 ; ii < other->len ; ii++ ) {
    g_ptr_array_add (self, g_ptr_array_index (other, ii));
  }
}

GPtrArray *
my_g_ptr_array_sum (GPtrArray *a, GPtrArray *b)
{
  GPtrArray *result = g_ptr_array_sized_new (a->len + b->len);

  guint ii;
  for ( ii = 0 ; ii < a->len ; ii++ ) {
    g_ptr_array_add (result, g_ptr_array_index (a, ii));
  }
  for ( ii = 0 ; ii < b->len ; ii++ ) {
    g_ptr_array_add (result, g_ptr_array_index (b, ii));
  }

  return result;
}

gboolean
my_g_ptr_array_equals_as_multiset (GPtrArray *a, GPtrArray *b, GEqualFunc cmp)
{
  gboolean result = TRUE;

  // Make copies of the arrays from which we can remove elements as we
  // match them without affecting the original array.
  GPtrArray *a_copy = g_ptr_array_sized_new (a->len);
  guint ii;
  for ( ii = 0 ; ii < a->len ; ii++ ) {
    g_ptr_array_add (a_copy, g_ptr_array_index (a, ii));
  }
  GPtrArray *b_copy = g_ptr_array_sized_new (b->len);
  for ( ii = 0 ; ii < b->len ; ii++ ) {
    g_ptr_array_add (b_copy, g_ptr_array_index (b, ii));
  }

  // Make sure all the elements of a have matches in b.
  for ( ii = 0 ; result && ii < a_copy->len ; ii++ ) {
    gpointer ce_a = g_ptr_array_index (a_copy, ii);
    guint jj;
    gboolean matched = FALSE;
    for ( jj = 0 ; !matched && jj < b_copy->len ; jj++ ) {
      gpointer ce_b = g_ptr_array_index (b_copy, jj);
      if ( cmp (ce_a, ce_b) ) {
	g_ptr_array_remove_index_fast (a_copy, ii);
	ii--;
	g_ptr_array_remove_index_fast (b_copy, jj);
	jj--;
	matched = TRUE;
      }
    }
    if ( !matched ) {
      result = FALSE;
    }
  }

  // Even if we match all the elements of a, there must not be any
  // leftovers in b.
  if ( b_copy->len > 0 ) {
    g_print ("About to return false due to left over elements.\n");
    result = FALSE;
  }
  else {
    // Sanity check for this implementation.
    g_assert (a_copy->len == 0);
  }

  g_ptr_array_free (b_copy, TRUE);
  g_ptr_array_free (a_copy, TRUE);

  return result;
}

void
my_g_ptr_array_really_free (GPtrArray *array,
			    void (*free_func)(gpointer element))
{
  guint ii;
  if ( free_func != NULL ) {
    for ( ii = 0 ; ii < array->len ; ii++ ) {
      free_func (g_ptr_array_index (array, ii));
    }
  }
  g_ptr_array_free (array, TRUE);
}

gdouble
my_sleep (gdouble time)
{
  g_assert (time >= 0.0);
  struct timespec sleep_time, remaining_time;
  sleep_time.tv_sec = floor (time);
  sleep_time.tv_nsec = (time - sleep_time.tv_sec) * 1000000000;
  g_assert (sleep_time.tv_nsec < 1000000000);
  int return_code = nanosleep (&sleep_time, &remaining_time);
  if ( return_code == -1 ) {
    g_assert (errno == EINTR);
    return remaining_time.tv_sec + ((gdouble) remaining_time.tv_nsec
				    / 1000000000.0);
  }
  else {
    return 0.0;
  }
}

GString *
pid_thread_string (void)
{
  GString *result = g_string_new ("");
  g_assert (sizeof (pid_t) <= sizeof (long long int));
  g_string_append_printf (result, "process_%lld_thread_%p",
			  (long long int) getpid (), g_thread_self ());

  return result;
}

///////////////////////////////////////////////////////////////////////////////
//
// Simple Random Number Functions
//
// See the comments for this section in utilities.h.
//
///////////////////////////////////////////////////////////////////////////////

// A thread safe rand function needs a place to store its state.
static unsigned int rand_r_state;

gdouble
random_fraction (void)
{
  return ((gdouble) rand_r (&rand_r_state)) / RAND_MAX;
}

gdouble
my_random (gdouble max)
{
  return random_fraction () * max;
}

///////////////////////////////////////////////////////////////////////////////

gboolean
my_is_writable_directory (const char *path, GError **err)
{
  struct stat ds;		// Directory stats.
  int return_code = g_stat (path, &ds);

  if ( return_code == -1 ) {
    g_set_error (err, MY_G_UTILITIES_ERROR, MY_G_UTILITIES_ERROR_G_STAT_FAILED,
		 "g_stat failed: %s", strerror (errno));
    return FALSE;
  }

  if ( ! S_ISDIR (ds.st_mode) ) {
    g_set_error (err, MY_G_UTILITIES_ERROR,
		 MY_G_UTILITIES_ERROR_NOT_A_DIRECTORY,
		 "not a directory");
    return FALSE;
  }

  if ( g_access (path, R_OK | W_OK | X_OK) != 0 ) {
    g_set_error (err, MY_G_UTILITIES_ERROR, MY_G_UTILITIES_ERROR_ACCESS_DENIED,
		 "g_access failed: %s", strerror (errno));
    return FALSE;
  }
  
  return TRUE;
}

gboolean
my_is_readable_file (const char *path, GError **err)
{
  struct stat ds;		// File stats.
  int return_code = g_stat (path, &ds);
  
  if ( return_code == -1 ) {
    g_set_error (err, MY_G_UTILITIES_ERROR, MY_G_UTILITIES_ERROR_G_STAT_FAILED,
		 "g_stat failed: %s", strerror (errno));
    return FALSE;
  }
  
  if ( ! S_ISREG (ds.st_mode) ) {
    g_set_error (err, MY_G_UTILITIES_ERROR,
		 MY_G_UTILITIES_ERROR_NOT_A_REGULAR_FILE,
		 "not a regular file");
    return FALSE;
  }

  if ( g_access (path, R_OK) != 0 ) {
    g_set_error (err, MY_G_UTILITIES_ERROR,
		 MY_G_UTILITIES_ERROR_READ_ACCESS_DENIED,
		 "g_access failed: %s", strerror (errno));
    return FALSE;
  }

  return TRUE;
}

static void
print_spaces (int space_count)
{
  g_assert (space_count >= 0);

  guint ii;
  for ( ii = 0 ; ii < space_count ; ii++ ) {
    g_print (" ");
  }
  
}

void
trmsg (const char *format, ...)
{
  // Maximum length of new string not including trailing NUL byte).
  const size_t max_length = 10000;

  char *buf = g_new (char, max_length + 1);

  va_list ap;
  va_start (ap, format);
  int char_count = vsnprintf (buf, max_length + 1, format, ap);
  va_end (ap);
  g_assert (char_count <= max_length);

  GString *message = pid_thread_string ();

  g_string_append (message, ": ");
  g_string_append (message, buf);
  g_free (buf);
  g_string_append_c (message, '\n');
  g_print (message->str);

  g_string_free (message, TRUE);
}

void
dump_array_of_strings (GPtrArray *strings, int space_count)
{
  g_assert (space_count >= 0);

  guint ii;
  for ( ii = 0 ; ii < strings->len ; ii++ ) {
    print_spaces (space_count);
    g_print ("%s\n", ((GString *) g_ptr_array_index (strings, ii))->str);;
  }
}

void
dump_keys_of_string_keyed_hash (GHashTable *table, int space_count)
{
  g_assert (space_count >= 0);

  GPtrArray *strings = my_g_hash_table_keys (table);
  dump_array_of_strings (strings, space_count);
  my_g_ptr_array_really_free (strings, NULL);
}

void
dump_values_of_hash_of_strings (GHashTable *table, int space_count)
{
  g_assert (space_count >= 0);
  
  GPtrArray *strings = my_g_hash_table_values (table);
  dump_array_of_strings (strings, space_count);
  my_g_ptr_array_really_free (strings, NULL);
}

void
dump_gstring_keyed_hash_of_gptr_arrays_of_gstrings (GHashTable *table,
						    int space_count,
						    int internal_space_count)
{
  GPtrArray *keys, *values;
  my_g_hash_table_keys_and_values (table, &keys, &values);

  guint ii;
  for ( ii = 0 ; ii < keys->len ; ii++ ) {
    print_spaces (space_count);
    g_print ("%s:\n", ((GString *) g_ptr_array_index (keys, ii))->str);
    GPtrArray *strings = g_ptr_array_index (values, ii);
    dump_array_of_strings (strings, space_count + internal_space_count);
  }

  my_g_ptr_array_really_free (values, NULL);
  my_g_ptr_array_really_free (keys, NULL);
}

void
random_delay (gdouble max_time)
{
  my_sleep (my_random (max_time));
}

void *
my_read_data_rectangle (const char *file, size_t element_size,
			off64_t data_width, off64_t start_x, off64_t start_y,
			size_t width, size_t height)
{
  g_assert (width <= SSIZE_MAX);

  int fd = open (file, O_LARGEFILE);
  g_assert (fd != -1);

  off64_t start_offset = element_size * (data_width * start_y + start_x);
  off64_t row_skip = element_size * (data_width - width);

  off64_t lseek64_result = lseek64 (fd, start_offset, SEEK_SET);
  g_assert (lseek64_result == start_offset);

  void *result = g_malloc (element_size * width * height);

  size_t ii;
  for ( ii = 0 ; ii < height ; ii++ ) {
    size_t bytes_to_read = width * element_size;
    g_assert (bytes_to_read <= SSIZE_MAX);
    ssize_t read_count = read (fd, result + ii * width * element_size, 
			       bytes_to_read);
    g_assert (read_count == bytes_to_read);
    if ( ii != height - 1 ) {
      lseek64_result = lseek64 (fd, row_skip, SEEK_CUR);
      g_assert (lseek64_result
		== (start_offset
		    + (row_skip + width * element_size) * (ii + 1)));
    }
  }

  return result;
}
