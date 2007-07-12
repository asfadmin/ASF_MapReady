// Implementation of interface described in float_blob.h.

#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib.h>
#include <glib/gstdio.h>

#include <gsl/gsl_math.h>

#include "float_blob.h"
#include "float_image.h"
#include "utilities.h"

FloatBlob *
float_blob_new (gssize size_x, gssize size_y, const char *scratch_dir,
		const char *name)
{
  g_assert (size_x > 0 && size_y > 0);
  g_assert (g_file_test (scratch_dir, G_FILE_TEST_IS_DIR));
  g_assert (g_access (scratch_dir, R_OK | W_OK | X_OK) == 0);

  FloatBlob *self = g_new (FloatBlob, 1);

  self->size_x = size_x;
  self->size_y = size_y;

  self->swapped_bytes = FALSE;

  if ( name != NULL ) {
    self->file = ensure_slash_terminated (g_string_new (scratch_dir));
    g_string_append (self->file, name);
  }
  else {
    self->file = make_unique_tmp_file_name (scratch_dir, "float_blob_tmp_");
    g_assert (!g_file_test (self->file->str, G_FILE_TEST_EXISTS));
  }
  self->is_file_owner = TRUE;

  self->fd = open (self->file->str, O_RDWR | O_CREAT | O_TRUNC,
		   S_IRUSR | S_IWUSR);
  g_assert (self->fd != -1);

  // Out of carefulness we didn't truncate, but we want to make sure
  // we really got a new file.
  g_assert (file_size (self->fd) == 0);

  // Seek to last byte of file.
  off_t last_byte_offset 
    = (off_t) self->size_x * (off_t) self->size_y * sizeof (float) - 1;
  off_t file_offset = lseek (self->fd, last_byte_offset, SEEK_SET);
  g_assert (file_offset == last_byte_offset);

  // Write a byte at the end of the file.
  size_t bytes_to_write = 1;
  g_assert (sizeof (unsigned char) == 1);
  unsigned char good_old_zero = 0x0;
  ssize_t bytes_written = write (self->fd, &good_old_zero, bytes_to_write);
  g_assert (bytes_written == bytes_to_write);

  // We should now have reserved all the space needed in this file.
  g_assert (file_size (self->fd) == last_byte_offset + 1);

  self->reference_count = 1;

  return self;
}

FloatBlob *
float_blob_new_using (gssize size_x, gssize size_y, const char *file,
		      gboolean swapped_bytes)
{
  g_assert (size_x > 0 && size_y > 0);
  g_assert (g_file_test (file, G_FILE_TEST_IS_REGULAR));
  g_assert (g_access (file, R_OK | W_OK) == 0);

  FloatBlob *self = g_new (FloatBlob, 1);

  self->fd = open (file, O_RDWR);
  g_assert (self->fd != -1);

  g_assert (file_size (self->fd) >= (off_t) size_x * size_y * sizeof (float));

  self->size_x = size_x;
  self->size_y = size_y;
  
  self->swapped_bytes = swapped_bytes;

  self->file = g_string_new (file);
  
  self->is_file_owner = FALSE;

  self->reference_count = 1;

  return self;
}

static void
float_blob_region_transfer (FloatBlob *self, gssize start_x, gssize start_y,
			    gssize w, gssize h, float *buffer,
			    gboolean is_get_transfer)
{
  g_assert (start_x >= 0 && start_x < self->size_x);
  g_assert (start_y >= 0 && start_y < self->size_y);
  g_assert (w > 0 && start_x + w <= self->size_x);
  g_assert (h > 0 && start_y + h <= self->size_y);

  ssize_t (*transfer)(int fd, void *buf, size_t count)
    = is_get_transfer ? read : ((ssize_t (*)(int, void *, size_t)) write);

  size_t jj;
  for ( jj = start_y ; jj < start_y + h ; jj++ ) {
    off_t file_offset 
      = lseek (self->fd,
	       sizeof (float) * ((off_t) jj * self->size_x + start_x),
	       SEEK_SET);
    g_assert (file_offset != (off_t) -1);
    size_t transfer_size = w * sizeof (float);
    ssize_t transfer_result
      = transfer (self->fd, buffer + (jj - start_y) * w, transfer_size);
    if ( transfer_result == -1 ) {
      g_error ("I/O error: %s\n", strerror (errno));
    }
    g_assert (transfer_result == transfer_size);
  }  
}

void
float_blob_get_region (FloatBlob *self, gssize start_x, gssize start_y,
		       gssize w, gssize h, float *buffer)
{
  float_blob_region_transfer (self, start_x, start_y, w, h, buffer, TRUE);

  if ( self->swapped_bytes ) {
    swap_array_bytes_32 (buffer, w * h);
  }
}

void
float_blob_set_region (FloatBlob *self, gssize start_x, gssize start_y,
		       gssize w, gssize h, float *buffer)
{
  if ( self->swapped_bytes ) {
    swap_array_bytes_32 (buffer, w * h);
  }

  float_blob_region_transfer (self, start_x, start_y, w, h, buffer, FALSE);

  if ( self->swapped_bytes ) {
    swap_array_bytes_32 (buffer, w * h);
  }
}

GString *
float_blob_steal_file (FloatBlob *self)
{
  g_assert (self->is_file_owner == TRUE);

  self->is_file_owner = FALSE;

  return g_string_new (self->file->str);

} 

void
float_blob_export_as_jpeg (FloatBlob *self, const char *file, double mask)
{
  // Self as float image.
  FloatImage *safi = float_image_new (self->size_x, self->size_y);
  
  float *row_buffer = g_new (float, self->size_x);

  // We are about to use swap_32 type function, so we have a quick
  // attack of paranoia and check sizeof float...
  g_assert (sizeof (float) == 4);

  // Seek to the beginning of the file.
  off_t resultant_offset = lseek (self->fd, 0, SEEK_SET);
  if ( resultant_offset == (off_t) -1 ) {
    g_error ("lseek failed: %s\n", strerror (errno));
  }
  g_assert (resultant_offset == 0);

  size_t ii, jj;
  for ( jj = 0 ; jj < self->size_y ; jj++ ) {

    // Read one row of data from the file underlying self.
    ssize_t bytes_to_read = self->size_x * sizeof (float);
    ssize_t read_count = read (self->fd, row_buffer, bytes_to_read);
    g_assert (read_count == bytes_to_read);

    // Swap row bytes if necessary.
    if ( self->swapped_bytes ) {
      swap_array_bytes_32 (row_buffer, self->size_x);
    }

    // Write that row to the FloatImage.
    for ( ii = 0 ; ii < self->size_x ; ii++ ) {
      float_image_set_pixel (safi, ii, jj, row_buffer[ii]);
    }
  }

  g_free (row_buffer);

  float_image_export_as_jpeg (safi, file, GSL_MAX (safi->size_x, safi->size_y),
			      mask);

  float_image_unref (safi);
}

FloatBlob *
float_blob_ref (FloatBlob *self)
{
  g_assert (self->reference_count > 0);

  self->reference_count++;

  return self;
}

void
float_blob_unref (FloatBlob *self)
{
  g_assert (self->reference_count > 0);
  
  self->reference_count--;

  if ( self->reference_count == 0 ) {
    int return_code = close (self->fd);
    g_assert (return_code == 0);

    if ( self->is_file_owner ) {
      return_code = unlink (self->file->str);
      if ( return_code != 0 ) {
	g_error ("unlink of float_blob file '%s' in function %s, (file "
		 __FILE__ ", line %d) failed: %s", self->file->str, __func__,
		 __LINE__, strerror (errno));
      }
      g_assert (return_code == 0);
    }

    g_string_free (self->file, TRUE);

    g_free (self);

    self = NULL;
  }
}
