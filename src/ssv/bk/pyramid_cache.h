// Interface to a cache of ASF image pyramids stored in a cache
// directory.  Only one instance of this class attached to a given
// path may exist in a process at a time, but multiple instances in
// different processes can have open instances attached to the same
// directory.  Entries are shared between users attached to the same
// path via a reader/writer lock system (see method descriptions for
// details).  The base layer (the image itself) is never stored in the
// cache (since it exists elsewhere anyway).

#ifndef PYRAMID_CACHE_H
#define PYRAMID_CACHE_H

#include <glib.h>

#include "table_file.h"

typedef struct {
  // Public read-only members.
  //
  // Name of directory in which index and layer files are to be
  // stored, terminated with a slash.
  GString *dir;

  // Private Members
  //
  gint64 max_size;  // Maximum cache size, in megabytes.
  // Sort of database thingy that coordinates cache access.
  TableFile *tf;
  // GString keyed existance hash of held reader locks in tf.
  GHashTable *reader_locks;
  // GString keyed existance hash of held writer locks in tf.
  GHashTable *writer_locks;
  int reference_count;
} PyramidCache;

// Open a pyramid cache using path as the working directory, and
// treating max_size as the amount of data to allow in the cache
// before we start implicitly removing old entries before adding new
// ones.  A cache is created if it doesn't already exist.  The
// directory argument specified should be for use as a cache
// exclusively.
PyramidCache *
pyramid_cache_new (const char *path, gint64 max_size);

// Attemp to load pyramid layers keyed by signature from the cache.
// If no such entry exists, NULL is returned, an unfinished entry is
// created, and a writer lock on the entry is granted to the caller
// (so the caller can call the add_entry method).  If an entry is
// found, an array of pointers to layers in the cache is returned and
// a reader lock on the cache entry is granted to the caller.  Note
// that the base layer is not stored in the cache, and so is not in
// the returned list.  The client can use the release method when its
// done with the layers.
GPtrArray *
pyramid_cache_try_lease (PyramidCache *self, GString *signature);

// Add a new entry to the cache.  The caller must hold a writer lock
// on the field obtained with the try_lease method.  The caller
// implicitly relinquishes this writer lock and receives a reader lock
// in its place.  The layers argument should consist of all layers
// except the base layer for the signature signature.  Any layers in
// layers which use FloatBlob instances as their "data" members must
// have been created in self->dir with names of the form
// "signature_layer_number" (with the first layer number for a
// signature being "1").
void
pyramid_cache_add_entry (PyramidCache *self, GString *signature,
			 GPtrArray *layers);

// Release any reader or writer locks held by the caller.
void
pyramid_cache_release (PyramidCache *self, GString *signature);

// Increment reference count of self, returning a pointer to self as a
// convenience.
PyramidCache *
pyramid_cache_ref (PyramidCache *self);

// Decrement reference count of self, freeing self in reference count
// falls to zero.
void
pyramid_cache_unref (PyramidCache *self);

#endif // ifndef PYRAMID_CACHE_H
