/* Various small non-object utility functions.  This sort of
   collection always needs refactoring, so if you want to think up a
   better way of organizing this stuff...  */

#include <glib.h>

/* Return a new string containing the full path to a datafile.  This
   function is compiled differently depending on whether
   FINAL_PRODUCTION_BUILD is defined of not.  If
   FINAL_PRODUCTION_BUILD is defined, this function looks for
   relative_path in ASF_SHAREDIR as supplied by the build environment,
   and then for relative_path in the current directory, and finally
   for the file name part of relative_path in the current directory.
   If FINAL_PRODUCTION_BUILD is not defined, the check in SHAREDIR is
   not performed.  If the file is found in the current directory, a
   notice to that effect is printed.  This function is intended to
   handle a problem with static data files: if their installation
   location is hardcoded, they must be installed before they can be
   tested, which is not what we want, but we also don't want to always
   use the installed version, since testing shouldn't depend on
   already-installed files, and we don't want to always check the
   local directory first, because production programs shouldn't depend
   on the files the user has lying around in their directories.  */
char *
datafile_path (const char *relative_path);

/* The strnlen function is a GNU extension, and for some crazy reason
   GLib doesn't provide it either.  */
size_t
asf_strnlen (const char *s, size_t maxlen);

/* Return a new pointer array containing all the values of the hash
   table entries.  Note that the new array entries point to memory
   shared with the hash table.  This routine is intended to allow
   slightly easier 'foreach' type of operations on the hash table
   entries.  */
GPtrArray *
get_g_hash_table_contents (GHashTable *table);

/* Add all the keys in table to keys and all the values to values.
   Note that the new array entries point to memory shared with the
   hash table.  This routine is intended to allow slightly easier
   'foreach' type of operationf on the hash table entries.  */
void
get_g_hash_table_keys_and_values (GHashTable *table, GPtrArray *keys,
				  GPtrArray *values);
