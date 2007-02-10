// Interface to a square blob of floating point data stored in a file,
// with support for creating from existing open files or with
// temporary files, and for stealing ownership later if necessary in
// the latter case.

#ifndef FLOAT_BLOB_H
#define FLOAT_BLOB_H

#include <glib.h>

// All data members are private.
typedef struct {
  // Public read only fields.
  gsize size_x, size_y;
  gboolean swapped_bytes;
  GString *file;	   // Path of file used as storage.
  gboolean is_file_owner;  // True iff instance owns the underlying file.

  // Private fields (maybe a close friend might play here).
  int fd;
  int reference_count;
} FloatBlob;

// Create a new instance using a new temporary file in scratch_dir.
// If name is not NULL, it is the name of the file in scratch_dir to
// use as the underlying storage area, otherwise a unique temporary
// name is generated automaticly.  If the file to be used already
// exists, it is truncated.  The file is owned by the instance and
// will be automaticly removed when the instance reference count falls
// to zero, unless it is stolen first (with the steal_file method).
// The file is filled with zeros until the set_region method is used
// to change this.
FloatBlob *
float_blob_new (gssize size_x, gssize size_y, const char *scratch_dir,
		const char *name);

// Create a new instance using file, which must be large enough to
// contain size_x by size_y floats.  If swapped_bytes is true, the
// data in file is understood to be in reverse byte order with respect
// to the host byte order, and any get_region or set_region method
// calls will make an implicit translation on behalf of the caller.
// When the instance reference count falls to zero, the file is not
// automaticly removed.  Its still almost certain to create problems
// if the file is twiddled while the instance is using it, however.
FloatBlob *
float_blob_new_using (gssize size_x, gssize size_y, const char *file,
		      gboolean swapped_bytes);

// Return in buffer the w by h float region with upper left corner
// (start_x, start_y), swapping bytes as required so the data returned
// is in native machine format.
void
float_blob_get_region (FloatBlob *self, gssize start_x, gssize start_y,
		       gssize w, gssize h, float *buffer);

// Fill in the w by h float region with upper left corner (start_x,
// start_y) with data from buffer, swapping bytes from buffer as
// required to maintain the convention for the instance specified by
// the swapped_bytes data member.
void
float_blob_set_region (FloatBlob *self, gssize start_x, gssize start_y,
		       gssize w, gssize h, float *buffer);

// Take ownership of the file currently owned by self.  The name of
// the file is returned as a new GString instance for which the client
// is responsible.  The caller assumes resonsibility for removing the
// file as desired, though of course, doing so while the instance is
// still using it will break the instance.
GString *
float_blob_steal_file (FloatBlob *self);

// Export self as a JPEG.  This method uses a FloatImage internally
// and so is fairly inefficient.  It is intended mainly for debugging
// purposes.  The mask argument has the same meaning as in the
// float_image_export_as_jpeg routine.
void
float_blob_export_as_jpeg (FloatBlob *self, const char *file, double mask);

// Increment reference count of self.
FloatBlob *
float_blob_ref (FloatBlob *self);

// Decrementn reference count of self, freeing self if it reference
// count falls to zero.
void
float_blob_unref (FloatBlob *self);

// Symmetry suggests that a "relinquish" method which would grant
// ownership of the file to the instance (probably relinking it into
// the scratch_dir) might be useful, but I have no immediate need.

#endif // FLOAT_BLOB_H
