/* See utilities.h.  */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "utilities.h"

#ifndef ASF_SHAREDIR
#  error "ASF_SHAREDIR must be defined by the build system."
#endif 

char *
datafile_path (const char *relative_path)
{
  assert (asf_strnlen (relative_path, FILENAME_MAX + 1) <= FILENAME_MAX); 
  /* The relative path shouldn't start with a slash.  */
  assert (relative_path[0] != '/');

  /* Actual path where we expect to find the file.  */
  gchar *path;		

  /* If this is a production build, we want to look for the file in
     the standard datafile location first.  */
#ifdef FINAL_PRODUCTION_BUILD

  path = g_strdup (ASF_SHAREDIR);
  
  /* Add a '/' to path if it isn't there already.  */
  if ( path[strlen (path) - 1] != '/' ) {
    /* Plus one for the new character, plus one for the trailing null
       which strlen doesn't count.  */
    path = realloc (path, (strlen (path) + 2) * sizeof (char));
    gint tmp = strlen (path);
    path[tmp] = '/';
    path[tmp + 1] = '\0';
  }
    
  /* Form the full path name based on ASF_SHAREDIR.  */
  path = realloc (path, ((strlen (path) + strlen (relative_path) + 1) 
			 * sizeof (char)));
  strcat (path, relative_path);
  
  if ( g_file_test (path, G_FILE_TEST_EXISTS) ) {
    return path;
  } else {
    free (path);
  }

#endif /* FINAL_PRODUCTION_BUILD */

  /* We are not doing a final production build, or the datafile wasn't
     installed yet, so look in the current directory.  If this is our
     first time through this code, print a note explaing where we are
     looking.  */
  static int first_time_through = TRUE;
  if ( first_time_through ) {
    fprintf (stderr, "Note: Looking for data file %s in './' instead of '"
	     ASF_SHAREDIR "' ... ", relative_path);
  }
  path = g_strdup_printf ("./%s", relative_path);
  if ( g_file_test (path, G_FILE_TEST_EXISTS) ) {
    if ( first_time_through ) {
      fprintf (stderr, "found it.\n");
    }
    return path;
  } else {
    /* Look for just the file name in the current directory.  */
    /* Get pointer to character after last directory seperator
       character.  */
    gchar *file_name_part = g_strrstr (path, G_DIR_SEPARATOR_S);
    file_name_part++;
    gchar *file_name = g_strdup (file_name_part);
    free (path);
    if ( first_time_through ) {
      fprintf (stderr, "couldn't find it,  looking for ./%s ... ", file_name);
    }
    if ( g_file_test (file_name, G_FILE_TEST_EXISTS) ) {
      if ( first_time_through) {
	fprintf (stderr, "found it.\n");
      }
      return file_name;
    } else {
      if ( first_time_through ) {
	fprintf (stderr, "failed, giving up.\n");
      }
      assert (FALSE);		/* Shouldn't be here.  */
    }
  }

  first_time_through = FALSE;
}

size_t
asf_strnlen (const char *s, size_t maxlen)
{
  assert (s != NULL);

  size_t ii = 0;		/* Index variable.  */

  for ( ii = 0 ; ii < maxlen ; ii++ ) {
    if ( s[ii] == '\0' ) {
      return ii;
    }
  }
  return maxlen;
}

/* Routine used in the accumulation of all the entries in a hash table
   into a list.  Its probably easiest to understand what this routine
   does by looking at the context in which it is called.  */
static void
accumulate_g_hash_table_values (gpointer key, gpointer value, 
				gpointer user_data)
{
  /* Reassure compiler.  We aren't interested in the the key in this
     case. */
  key = key;		

  GPtrArray *accumulation_array = user_data;

  g_ptr_array_add (accumulation_array, value);
}

GPtrArray *
get_g_hash_table_contents (GHashTable *table)
{
  GPtrArray *ret = g_ptr_array_new ();

  g_hash_table_foreach (table, accumulate_g_hash_table_values, ret);

  return ret;
}

/* Routine used in the accumulation of all the keys and values in a
   hash table into a list.  Its probably easiest to understand what
   this routine does by looking at the context in which it is
   called.  */
static void
accumulate_g_hash_table_keys_and_values (gpointer key, gpointer value, 
					 gpointer user_data)
{
  GPtrArray *accumulation_array = user_data;

  g_ptr_array_add (accumulation_array, key);
  g_ptr_array_add (accumulation_array, value);
}

void
get_g_hash_table_keys_and_values (GHashTable *table, GPtrArray *keys,
				  GPtrArray *values)
{
  GPtrArray *merged_pairs = g_ptr_array_new ();

  g_hash_table_foreach (table, accumulate_g_hash_table_keys_and_values, 
			merged_pairs);
  g_assert (merged_pairs->len % 2 == 0);

  /* Unmerge the keys and values into seperate arrays.  */
  int ii;
  for ( ii = 0 ; (guint) ii < merged_pairs->len ; ii += 2 ) {
    g_ptr_array_add (keys, g_ptr_array_index (merged_pairs, ii));
    g_ptr_array_add (values, g_ptr_array_index (merged_pairs, ii + 1));
  }

  g_ptr_array_free (merged_pairs, FALSE);
}
