// Implementation of the interface in float_image.h.

#include <errno.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <setjmp.h>

#include <glib.h>
#if GLIB_CHECK_VERSION (2, 6, 0)
#  include <glib/gstdio.h>
#endif
#include <gsl/gsl_spline.h>
#include <gsl/gsl_histogram.h>
#include <gsl/gsl_math.h>

#include "asf.h"
#include "asf_tiff.h"
#include "asf_jpeg.h"
#include "float_image.h"

double gsl_spline_eval_check(gsl_spline *, double, gsl_interp_accel *);

#ifndef linux
#ifndef darwin
#ifndef win32
static double
round (double arg)
{
    return floor (arg + 0.5);
}
#endif // #ifndef win32
#endif // #ifndef darwin
#endif // #ifndef linux

#include "asf_glib.h"

// Default cache size to use is 16 megabytes.
static const size_t default_cache_size = 16 * 1048576;
// This class wide data element keeps track of the number of temporary
// tile files opened by the current process, in order to give them
// unique names.
static unsigned long current_tile_file_number = 0;

#ifndef win32
// We need to ensure that multiple threads trying to create their own
// images concurently don't end up with the same temporary file names.
G_LOCK_DEFINE_STATIC (current_tile_file_number);

// We don't want to let multiple threads twiddle the signal block mask
// concurrently, or we might end up with the wrong set of signals
// blocked.  This lock is used to gaurantee this can't happen (see the
// usage for a better explanation).
G_LOCK_DEFINE_STATIC (signal_block_activity);
#endif

// Return a FILE pointer refering to a new, already unlinked file in a
// location which hopefully has enough free space to serve as a block
// cache.
static FILE *
initialize_tile_cache_file (GString **tile_file_name)
{
  // Create the temporary tile oriented storage file.  This gets
  // filled in in different ways depending on which creation routine
  // we are using.
  g_assert(*tile_file_name == NULL);
  *tile_file_name = g_string_new ("");

  // Here we do a slightly weird thing: if the current directory is
  // writable, we create a temporary file in the current directory.
  // We do this because the temporary file could well be pretty big
  // and /tmp often maps to a small file system.  The idea is that the
  // directory the user is in is more likely to have the extra space
  // required to hold the temporary file.  Of course, if they have
  // been carefully calculating their space requirements, they may be
  // disappointed.  We use a weird name that no sane user would ever
  // use for one of their files, we hope.
#ifndef win32
  G_LOCK (current_tile_file_number);
  g_assert (sizeof (long) >= sizeof (pid_t));
#endif

  g_string_append_printf (*tile_file_name,
                          ".float_image_tile_file_%ld_%lu",
                          (long) getpid (),
                          current_tile_file_number);
  //g_free (current_dir);
  // This hard coded limit on the current number used to uniqueify
  // file names limits us to creating no more than ULONG_MAX instances
  // during a process.
  g_assert (current_tile_file_number < ULONG_MAX);
  current_tile_file_number++;

#ifndef win32
  G_UNLOCK (current_tile_file_number);

  // We block signals while we create and unlink this file, so we
  // don't end up leaving a huge temporary file somewhere.
  // Theoretically, two parallel instantiations of image could end up
  // in a race condition which would result in all signals ending up
  // blocked after both were done with this section, so we consider
  // this section critical and protect it with a lock.
  G_LOCK (signal_block_activity);
  sigset_t all_signals, old_set;
  int return_code = sigfillset (&all_signals);
  g_assert (return_code == 0);
  return_code = sigprocmask (SIG_SETMASK, &all_signals, &old_set);
#endif

  // now open new tile file in the tmp dir
  FILE *tile_file = fopen_tmp_file ((*tile_file_name)->str, "w+b");
  if ( tile_file == NULL ) {
    if ( errno != EACCES ) {
      g_warning ("couldn't create file in tmp directory (%s), and it wasn't"
                 "just a permissions problem", get_asf_tmp_dir());
    }
    else {
      // Couldn't open in current directory, so try using tmpfile,
      // which opens the file in the standardish place for the system.
      // See the comment above about why opening in /tmp or the like
      // is potentially bad.
      tile_file = tmpfile ();
      g_assert (tile_file != NULL);
    }
  }
  else {
    // the open-then-delete trick does not seem to work on MinGW
#ifndef win32
    return_code = unlink_tmp_file ((*tile_file_name)->str);
    g_assert (return_code == 0);
#endif
  }
  g_assert (tile_file != NULL);

#ifndef win32
  return_code = sigprocmask (SIG_SETMASK, &old_set, NULL);
  G_UNLOCK (signal_block_activity);
#endif

  return tile_file;
}

// This routine does the work common to several of the differenct
// creation routines.  Basicly, it does everything but fill in the
// contents of the disk tile store.
static FloatImage *
initialize_float_image_structure (ssize_t size_x, ssize_t size_y)
{
  // Allocate instance memory.
  FloatImage *self = g_new0 (FloatImage, 1);

  // Validate and remember image size.
  g_assert (size_x > 0 && size_y > 0);
  self->size_x = size_x;
  self->size_y = size_y;

  // Greater of size_x and size_y.
  size_t largest_dimension = (size_x > size_y ? size_x : size_y);

  // If we can fit the entire image in a single square tile, then we
  // want just a single big tile and we won't need to bother with the
  // cache file since it won't ever be used, so we do things slightly
  // differently.  FIXME: it would be slightly better to also detect
  // and specially handle the case where we have long narrow images
  // that can fit in a single stip of tiles in the cache.
  if ( largest_dimension * largest_dimension * sizeof (float)
       <= default_cache_size ) {
    self->cache_space = largest_dimension * largest_dimension * sizeof (float);
    self->cache_area = self->cache_space / sizeof (float);
    self->tile_size = largest_dimension;
    self->cache_size_in_tiles = 1;
    self->tile_count_x = 1;
    self->tile_count_y = 1;
    self->tile_count = 1;
    self->tile_area = self->tile_size * self->tile_size;
    self->cache = g_new (float, self->cache_area);
    self->tile_addresses = g_new0 (float *, self->tile_count);
    g_assert (NULL == 0x0);     // Ensure g_new0 effectively sets to NULL.
    // The tile queue shouldn't ever be needed in this case.
    self->tile_queue = NULL;
    // The tile file shouldn't ever be needed, so we set it to NULL to
    // indicate this to a few other methods that use it directly, and
    // to hopefully ensure that it triggers an exception if it is
    // used.
    self->tile_file = NULL;

    // Objects are born with one reference.
    self->reference_count = 1;

    return self;
  }

  // The default cache size compiled into the class.
  self->cache_space = default_cache_size;

  // Memory cache space, in pixels.
  g_assert (self->cache_space % sizeof (float) == 0);
  self->cache_area = self->cache_space / sizeof (float);

  // How small do our tiles have to be on a side to fit two full rows
  // of them in the memory cache?  This is slightly tricky.  In order
  // to provide the services promised in the interface, we need to
  // solve
  //
  //      2 * pow (t, 2) * ceil ((double)largest_dimension / t)
  //           <= self->cache_area
  //
  // for tile size t.  I don't know the closed form solution if there
  // is one, so toss out the ceil() and solve the easier
  //
  //      2 * pow (t, 2) * ((double)largest_dimension / t) <= self->cache_area
  //
  // and then decrement t iteratively until things work.
  self->tile_size = self->cache_area / (2 * largest_dimension);
  while ( (2 * self->tile_size * self->tile_size
           * ceil ((double) largest_dimension / self->tile_size))
          > self->cache_area ) {
    self->tile_size--;
  }

  // If we end up with a tile size of 0, it means we tried to create a
  // truly gigantic image (compared to the size of the tile cache at
  // least.  Really, if we end up with a sufficiently small tile size,
  // there probably isn't much point in going on.  Picking an
  // arbitrary tile size to call too small is tricky, but we do it
  // anyway :).
  const size_t minimum_tile_size = 4;
  g_assert (self->tile_size >= minimum_tile_size);

  // Area of tiles, in pixels.
  self->tile_area = (size_t) (self->tile_size * self->tile_size);

  // Number of tiles which will fit in image cache.
  self->cache_size_in_tiles = self->cache_area / self->tile_area;
  // Can we fit at least as much as we intended in the cache?
  g_assert (self->cache_size_in_tiles
            >= 2 * (size_t) ceil ((double) largest_dimension
                                  / self->tile_size));

  // Number of tiles image has been split into in x and y directions.
  self->tile_count_x = (size_t) ceil ((double) self->size_x / self->tile_size);
  self->tile_count_y = (size_t) ceil ((double) self->size_y / self->tile_size);

  // Total number of image tiles image has been split into.
  self->tile_count = self->tile_count_x * self->tile_count_y;

  // We want to be able to pack a tile number into a pointer later, so
  // we need it to fit into an integer.
  g_assert (self->tile_count < INT_MAX);

  // Did all that math work?
  g_assert (self->tile_size * self->cache_size_in_tiles / 2
            >= largest_dimension);
  g_assert (self->cache_size_in_tiles * self->tile_area <= self->cache_area);

  // Allocate memory for the in-memory cache.
  self->cache = g_new (float, self->cache_area);
  // Do we want to do mlock() here maybe?

  // The addresses in the cache of the starts of each of the tiles.
  // This array contains flattened tile addresses in the same way that
  // image memory normally uses flattened pixel addresses, e.g. the
  // address of tile x = 2, y = 4 is stored at self->tile_addresses[4
  // * self->tile_count_x + 2].  If a tile isn't in the cache, the
  // address is NULL (meaning it will have to be loaded).
  self->tile_addresses = g_new0 (float *, self->tile_count);
  g_assert (NULL == 0x0);       // Ensure g_new0 effectively sets to NULL.

  // Create a queue in order to keep track of which tile was loaded
  // longest ago.
  self->tile_queue = g_queue_new ();

  // Get a new empty tile cache file pointer.
  self->tile_file_name = NULL;
  self->tile_file = initialize_tile_cache_file (&(self->tile_file_name));

  // Objects are born with one reference.
  self->reference_count = 1;

  return self;
}

FloatImage *
float_image_thaw (FILE *file_pointer)
{
  FILE *fp = file_pointer;  // Convenience alias.

  g_assert (file_pointer != NULL);

  FloatImage *self = g_new0 (FloatImage, 1);

  size_t read_count = fread (&(self->size_x), sizeof (size_t), 1, fp);
  g_assert (read_count == 1);

  read_count = fread (&(self->size_y), sizeof (size_t), 1, fp);
  g_assert (read_count == 1);

  read_count = fread (&(self->cache_space), sizeof (size_t), 1, fp);
  g_assert (read_count == 1);

  read_count = fread (&(self->cache_area), sizeof (size_t), 1, fp);
  g_assert (read_count == 1);

  read_count = fread (&(self->tile_size), sizeof (size_t), 1, fp);
  g_assert (read_count == 1);

  read_count = fread (&(self->cache_size_in_tiles), sizeof (size_t), 1, fp);
  g_assert (read_count == 1);

  read_count = fread (&(self->tile_count_x), sizeof (size_t), 1, fp);
  g_assert (read_count == 1);

  read_count = fread (&(self->tile_count_y), sizeof (size_t), 1, fp);
  g_assert (read_count == 1);

  read_count = fread (&(self->tile_count), sizeof (size_t), 1, fp);
  g_assert (read_count == 1);

  read_count = fread (&(self->tile_area), sizeof (size_t), 1, fp);
  g_assert (read_count == 1);

  // The cache isn't serialized -- its a bit of a pain and probably
  // almost never worth it.
  self->cache = g_new (float, self->cache_area);

  self->tile_addresses = g_new0 (float *, self->tile_count);

  // We don't actually keep the tile queue in the serialized instance,
  // but if the serialized pointer to it is NULL, we know we aren't
  // using a tile cache file (i.e. the whole image fits in the memory
  // cache).
  read_count = fread (&(self->tile_queue), sizeof (GQueue *), 1, fp);
  g_assert (read_count == 1);

  // If there was no cache file...
  if ( self->tile_queue == NULL ) {
    // The tile_file structure field should also be NULL.
    self->tile_file = NULL;
    // we restore the file directly into the first and only tile (see
    // the end of the float_image_new method).
    self->tile_addresses[0] = self->cache;
    read_count = fread (self->tile_addresses[0], sizeof (float),
      self->tile_area, fp);
    g_assert (read_count == self->tile_area);
  }
  // otherwise, an empty tile queue needs to be initialized, and the
  // remainder of the serialized version is the tile block cache.
  else {
    self->tile_queue = g_queue_new ();
    self->tile_file_name = NULL;
    self->tile_file = initialize_tile_cache_file (&(self->tile_file_name));
    float *buffer = g_new (float, self->tile_area);
    size_t ii;
    for ( ii = 0 ; ii < self->tile_count ; ii++ ) {
      read_count = fread (buffer, sizeof (float), self->tile_area, fp);
      g_assert (read_count == self->tile_area);
      size_t write_count = fwrite (buffer, sizeof (float), self->tile_area,
           self->tile_file);
      if ( write_count < self->tile_area ) {
  if ( feof (self->tile_file) ) {
    fprintf (stderr,
       "Premature end of file while trying to thaw FloatImage "
       "instance\n");
  }
  else {
    g_assert (ferror (self->tile_file));
    fprintf (stderr,
       "Error writing tile cache file for FloatImage instance "
       "during thaw: %s\n", strerror (errno));
  }
  exit (EXIT_FAILURE);
      }
      g_assert (write_count == self->tile_area);
    }
    g_free (buffer);
  }

  // We didn't call initialize_float_image_structure directly or
  // indirectly for this creation method, so we still have to set the
  // reference count appropriately.
  self->reference_count = 1;

  return self;
}



FloatImage *
float_image_new (ssize_t size_x, ssize_t size_y)
{
  g_assert (size_x > 0 && size_y > 0);

  FloatImage *self = initialize_float_image_structure (size_x, size_y);

  // If we need a tile file for an image of this size, prepare it.
  if ( self->tile_file != NULL ) {
    // The total width or height of all the tiles is probably greater
    // than the width or height of the image itself.
    size_t total_width = self->tile_count_x * self->tile_size;
    size_t total_height = self->tile_count_y * self->tile_size;

    // Fill the file full of zeros.  FIXME: there is almost certainly
    // a faster way to ensure that we have the disk space we need.
    float *zero_line = g_new0 (float, total_width);
    g_assert (0.0 == 0x0);      // Ensure the g_new0 did what we think.

    // We don't have to write in tile order because its all zeros anyway.
    size_t ii;
    for ( ii = 0 ; ii < total_height ; ii++ ) {
      size_t write_count = fwrite (zero_line, sizeof (float), total_width,
                                   self->tile_file);
      // If we wrote less than expected,
      if ( write_count < total_width ) {
        // it must have been a write error (probably no space left),
        g_assert (ferror (self->tile_file));
        // so print an error message,
        fprintf (stderr,
                 "Error writing tile cache file for FloatImage instance: %s\n",
                 strerror (errno));
        // and exit.
        exit (EXIT_FAILURE);
      }
    }

    // Done with the line of zeros.
    g_free (zero_line);
  }

  // Everything fits in the cache (at the moment this means everything
  // fits in the first tile, which is a bit of a FIXME), so just put
  // it there.
  else {
    self->tile_addresses[0] = self->cache;
    size_t ii, jj;
    for ( ii = 0 ; ii < self->tile_size ; ii++ ) {
      for ( jj = 0 ; jj < self->tile_size ; jj++ ) {
        self->tile_addresses[0][ii * self->tile_size + jj] = 0.0;
      }
    }
  }

  return self;
}

FloatImage *
float_image_new_with_value (ssize_t size_x, ssize_t size_y, float value)
{
  g_assert (size_x > 0 && size_y > 0);

  FloatImage *self = initialize_float_image_structure (size_x, size_y);

  // If we need a tile file for an image of this size, prepare it.
  if ( self->tile_file != NULL ) {

    // The total width or height of all the tiles is probably greater
    // than the width or height of the image itself.
    size_t total_width = self->tile_count_x * self->tile_size;
    size_t total_height = self->tile_count_y * self->tile_size;

    // Fill the file full of the given value.
    float *value_line = g_new (float, total_width);
    size_t ii;
    for ( ii = 0 ; ii < total_width ; ii++ ) {
      value_line[ii] = value;
    }
    // We don't have to write in tile order because the values are all
    // the same anyway.
    for ( ii = 0 ; ii < total_height ; ii++ ) {
      size_t write_count = fwrite (value_line, sizeof (float), total_width,
                                   self->tile_file);
      // If we wrote less than expected,
      if ( write_count < total_width ) {
        // it must have been a write error (probably no space left),
        g_assert (ferror (self->tile_file));
        // so print an error message,
        fprintf (stderr,
                 "Error writing tile cache file for FloatImage instance: %s\n",
                 strerror (errno));
        // and exit.
        exit (EXIT_FAILURE);
      }
    }

    // Done with the line of values.
    g_free (value_line);
  }

  // Everything fits in the cache (at the moment this means everything
  // fits in the first tile, which is a bit of a FIXME), so just put
  // it there.
  else {
    self->tile_addresses[0] = self->cache;
    size_t ii, jj;
    for ( ii = 0 ; ii < self->tile_size ; ii++ ) {
      for ( jj = 0 ; jj < self->tile_size ; jj++ ) {
        self->tile_addresses[0][ii * self->tile_size + jj] = value;
      }
    }
  }

  return self;
}

// Swap the byte order of a 32 bit value, hopefully converting from
// big endian to little endian or vice versa.  There is unfortunately
// some question whether or not this always works right for floating
// point values.
static void
swap_bytes_32 (unsigned char *in)
{
  g_assert (sizeof (unsigned char) == 1);
  int tmp = in[0];
  in[0] = in[3];
  in[3] = tmp;
  tmp = in[1];
  in[1] = in[2];
  in[2] = tmp;
}

// Swap the byte order of a 16 bit value, hopefully converting from
// big endian to little endian or vice versa.
//static void
//swap_bytes_16 (unsigned char *in)
//{
//  g_assert (sizeof (unsigned char) == 1);
//  int tmp = in[0];
//  in[0] = in[1];
//  in[1] = tmp;
//}

FloatImage *
float_image_new_from_memory (ssize_t size_x, ssize_t size_y, float *buffer)
{
  g_assert (size_x > 0 && size_y > 0);

  // FIXME: this is an inefficient implementation.

  FloatImage *self = float_image_new (size_x, size_y);

  ssize_t ii, jj;
  for ( ii = 0 ; ii < size_x ; ii++ ) {
    for ( jj = 0 ; jj < size_y ; jj++ ) {
      float_image_set_pixel (self, ii, jj, buffer[jj * size_x + ii]);
    }
  }

  return self;
}

FloatImage *
float_image_copy (FloatImage *model)
{
  g_assert (model->reference_count > 0); // Harden against missed ref=1 in new

  // FIXME: this could obviously be optimized a lot by copying the
  // existed tile file, etc.
  FloatImage *self = float_image_new (model->size_x, model->size_y);

  size_t ii, jj;
  for ( ii = 0 ; ii < self->size_y ; ii++ ) {
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      float_image_set_pixel (self, jj, ii,
           float_image_get_pixel (model, jj, ii));
    }
  }

  return self;
}

// Bilinear interpolation for a point delta_x, delta_y from the lower
// left corner between values ul (upper left), ur (upper right), etc.
// The corner are considered to be corners of a unit square.
static float
bilinear_interpolate (double delta_x, double delta_y, float ul, float ur,
                      float ll, float lr)
{
  float lv = ll + (lr - ll) * delta_x; // Lower value.
  float uv = ul + (ur - ul) * delta_x; // Upper value.

  return lv + (uv - lv) * delta_y;
}

FloatImage *
float_image_new_from_model_scaled (FloatImage *model, ssize_t scale_factor)
{
  g_assert (model->reference_count > 0); // Harden against missed ref=1 in new

  g_assert (model->size_x > 0 && model->size_y > 0);

  g_assert (scale_factor > 0);
  g_assert (scale_factor % 2 == 1);

  FloatImage *self
    = float_image_new (round ((double) model->size_x / scale_factor),
           round ((double) model->size_y / scale_factor));

  // Form an averaging kernel of the required size.
  size_t kernel_size = scale_factor;
  gsl_matrix_float *averaging_kernel
    = gsl_matrix_float_alloc (kernel_size, kernel_size);
  float kernel_value = 1.0 / ((float)kernel_size * kernel_size);
  size_t ii, jj;                // Index values.
  for ( ii = 0 ; ii < averaging_kernel->size1 ; ii++ ) {
    for ( jj = 0 ; jj < averaging_kernel->size2 ; jj++ ) {
      gsl_matrix_float_set (averaging_kernel, ii, jj, kernel_value);
    }
  }

  // "Sample" the model by applying the averaging kernel, putting
  // results into the new image.
  size_t sample_stride = scale_factor;
  for ( ii = 0 ; ii < self->size_y ; ii++ ) {
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      float pv = float_image_apply_kernel (model, jj * sample_stride,
                                           ii * sample_stride,
                                           averaging_kernel);
      float_image_set_pixel (self, jj, ii, pv);
    }
  }

  return self;
}

FloatImage *
float_image_new_subimage (FloatImage *model, ssize_t x, ssize_t y,
        ssize_t size_x, ssize_t size_y)
{
  g_assert (model->reference_count > 0); // Harden against missed ref=1 in new

  // Upper left corner must be in model.
  g_assert (x >= 0 && y >= 0);

  // Size of image to be created must be strictly positive.
  g_assert (size_x >= 1 && size_y >= 1);

  // Given model must be big enough to allow a subimage of the
  // requested size to fit.
  g_assert (model->size_x <= SSIZE_MAX && model->size_y <= SSIZE_MAX);
  g_assert (x + size_x <= (ssize_t) model->size_x);
  g_assert (y + size_y <= (ssize_t) model->size_y);

  FloatImage *self = float_image_new (size_x, size_y);

  // Copy the image pixels from the model.
  ssize_t ii, jj;
  for ( ii = 0 ; ii < (ssize_t) self->size_x ; ii++ ) {
    for ( jj = 0 ; jj < (ssize_t) self->size_y ; jj++ ) {
      float pv = float_image_get_pixel (model, x + ii, y + jj);
      float_image_set_pixel (self, ii, jj, pv);
    }
  }

  return self;
}

// Return true iff file is size or larger.
static gboolean
is_large_enough (const char *file, off_t size)
{
  struct stat stat_buffer;
#if GLIB_CHECK_VERSION(2, 6, 0)
  int return_code = g_stat (file, &stat_buffer);
  if ( return_code != 0 ) {
    g_error ("Couldn't g_stat file %s: %s", file, strerror (errno));
  }
#else
  int return_code = stat (file, &stat_buffer);
  if ( return_code != 0 ) {
    g_error ("Couldn't stat file %s: %s", file, strerror (errno));
  }
#endif

  return stat_buffer.st_size >= size;
}

FloatImage *
float_image_new_from_file (ssize_t size_x, ssize_t size_y, const char *file,
                           off_t offset, float_image_byte_order_t byte_order)
{
  g_assert (size_x > 0 && size_y > 0);

  // Check in advance if the source file looks big enough (we will
  // still need to check return codes as we read() data, of course).
  //g_assert (is_large_enough (file, offset + ((off_t) size_x * size_y
  //               * sizeof (float))));

  // Open the file to read data from.
  FILE *fp = fopen (file, "rb");
  // FIXME: we need some error handling and propagation here.
  g_assert (fp != NULL);

  FloatImage *self = float_image_new_from_file_pointer (size_x, size_y, fp,
                                                        offset, byte_order);

  // Close file we read image from.
  int return_code = fclose (fp);
  g_assert (return_code == 0);

  return self;
}

// Return true iff byte_order is not the native byte order on the
// current platform.
static gboolean
non_native_byte_order (float_image_byte_order_t byte_order)
{
  return ((G_BYTE_ORDER == G_LITTLE_ENDIAN
           && byte_order == FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN)
          || (G_BYTE_ORDER == G_BIG_ENDIAN
              && byte_order == FLOAT_IMAGE_BYTE_ORDER_LITTLE_ENDIAN));
}

FloatImage *
float_image_new_from_file_pointer (ssize_t size_x, ssize_t size_y,
                                   FILE *file_pointer, off_t offset,
                                   float_image_byte_order_t byte_order)
{
  g_assert (size_x > 0 && size_y > 0);

  FloatImage *self = initialize_float_image_structure (size_x, size_y);

  FILE *fp = file_pointer;      // Convenience alias.

  // Seek to the indicated offset in the file.
  int return_code = FSEEK64 (fp, offset, SEEK_CUR);
  g_assert (return_code == 0);

  // If we need a tile file for an image of this size, we will load
  // the data straight into it.
  if ( self->tile_file != NULL ) {

    // We will read the input image data in horizontal stips one tile
    // high.  Note that we probably won't be able to entirely fill the
    // last tiles in each dimension with real data, since the image
    // sizes rarely divide evenly by the numbers of tiles.  So we fill
    // it with zeros instead.  The data off the edges of the image
    // should never be accessed directly anyway.

    // Some data for doing zero fill.  If the tiles are bigger than
    // the image itself, we need to make the available zero fill the
    // size of the tile instead of the size of the file.
    g_assert (self->tile_size <= SSIZE_MAX);
    float *zero_line = g_new0 (float, (size_x > (ssize_t) self->tile_size ?
                                       (size_t) size_x : self->tile_size));
    g_assert (0.0 == 0x0);

    // Buffer capable of holding a full strip.
    float *buffer = g_new (float, self->tile_size * self->size_x);

    // Reorganize data into tiles in tile oriented disk file.
    size_t ii = 0;
    for ( ii = 0 ; ii < self->tile_count_y ; ii++ ) {
      asfPercentMeter((float)ii/(self->tile_count_y-1));

      // The "effective_height" of the strip is the portion of the
      // strip for which data actually exists.  If the effective
      // height is less than self->tile>size, we will have to add some
      // junk to fill up the extra part of the tile (which should
      // never be accessed).
      size_t effective_height;
      if ( ii < self->tile_count_y - 1
           || self->size_y % self->tile_size == 0 ) {
        effective_height = self->tile_size;
      }
      else {
        effective_height = self->size_y % self->tile_size;
      }
      // Total area of the current strip.
      size_t strip_area = effective_height * self->size_x;

      // Read one strip of tiles worth of data from the file.
      size_t read_count = fread (buffer, sizeof (float), strip_area, fp);
      g_assert (read_count == strip_area);

      // Convert from the byte order on disk to the host byte order,
      // if necessary.  Doing this with floats is somewhat
      // questionable apparently: major libraries don't seem to
      // support it with their macros, and the perl documentation says
      // it can't be done in a truly portable way... but it seems to
      // work.
      if ( non_native_byte_order (byte_order) ) {
        // Floats better be four bytes for this to work.
        g_assert (sizeof (float) == 4);
        size_t idx;
        for ( idx = 0 ; idx < strip_area ; idx++ ) {
          swap_bytes_32 ((unsigned char *) &(buffer[idx]));
        }
      }

      // Write data from the strip into the tile store.
      size_t jj;
      for ( jj = 0 ; jj < self->tile_count_x ; jj++ ) {
        // This is roughly analogous to effective_height.
        size_t effective_width;
        if ( jj < self->tile_count_x - 1
             || self->size_x % self->tile_size == 0) {
          effective_width = self->tile_size;
        }
        else {
          effective_width = self->size_x % self->tile_size;
        }
        size_t write_count;     // For return of fwrite() calls.
        size_t kk;
        for ( kk = 0 ; kk < effective_height ; kk++ ) {
          write_count
            = fwrite (buffer + kk * self->size_x + jj * self->tile_size,
                      sizeof (float), effective_width, self->tile_file);
          // If we wrote less than expected,
          if ( write_count < effective_width ) {
            // it must have been a write error (probably no space left),
            g_assert (ferror (self->tile_file));
            // so print an error message,
            fprintf (stderr,
                     "Error writing tile cache file for FloatImage instance: "
                     "%s\n", strerror (errno));
            // and exit.
            exit (EXIT_FAILURE);
          }
          if ( effective_width < self->tile_size ) {
            // Amount we have left to write to fill out the last tile.
            size_t edge_width = self->tile_size - effective_width;
            write_count = fwrite (zero_line, sizeof (float), edge_width,
                                  self->tile_file);
            // If we wrote less than expected,
            if ( write_count < edge_width ) {
              // it must have been a write error (probably no space left),
              g_assert (ferror (self->tile_file));
              // so print an error message,
              fprintf (stderr,
                       "Error writing tile cache file for FloatImage "
                       "instance: %s\n", strerror (errno));
              // and exit.
              exit (EXIT_FAILURE);
            }
          }
        }
        // Finish writing the bottom of the tile for which there is no
        // image data (should only happen if we are on the last strip of
        // tiles).
        for ( ; kk < self->tile_size ; kk++ ) {
          g_assert (ii == self->tile_count_y - 1);
          write_count = fwrite (zero_line, sizeof (float), self->tile_size,
                                self->tile_file);
          // If we wrote less than expected,
          if ( write_count < self->tile_size ) {
            // it must have been a write error (probably no space left),
            g_assert (ferror (self->tile_file));
            // so print an error message,
            fprintf (stderr,
                     "Error writing tile cache file for FloatImage instance: "
                     "%s\n", strerror (errno));
            // and exit.
            exit (EXIT_FAILURE);
          }
        }
      }
    }

    // Did we write the correct total amount of data?
    g_assert (FTELL64 (self->tile_file)
        == (off_t) (self->tile_area * self->tile_count * sizeof (float)));

    // Free temporary buffers.
    g_free (buffer);
    g_free (zero_line);
  }

  // Everything fits in the cache (at the moment this means everything
  // fits in the first tile, which is a bit of a FIXME), so just put
  // it there.
  else {
    self->tile_addresses[0] = self->cache;

    size_t ii;
    for ( ii = 0 ; ii < self->size_y ; ii++ ) {
      // Address where the current row of pixels should end up.
      float *row_address = self->tile_addresses[0] + ii * self->tile_size;

      // Read the data.
      size_t read_count = fread (row_address, sizeof (float), self->size_x,
         fp);
      g_assert (read_count == self->size_x);

      // Convert from the byte order on disk to the host byte order,
      // if necessary.  Doing this with floats is somewhat
      // questionable: major libraries don't seem to support it with
      // their macros, and the perl documentation says it can't be
      // done in a truly portable way... but it seems to work.
      if ( non_native_byte_order (byte_order) ) {
  g_assert (sizeof (float) == 4);
  size_t jj;
  for ( jj = 0 ; jj < self->size_x ; jj++ ) {
    swap_bytes_32 ((unsigned char *) &(row_address[jj]));
  }
      }
    }
  }

  return self;
}

FloatImage *
float_image_new_from_file_scaled (ssize_t size_x, ssize_t size_y,
                                  ssize_t original_size_x,
                                  ssize_t original_size_y,
                                  const char *file, off_t offset,
                                  float_image_byte_order_t byte_order)
{
  g_assert (size_x > 0 && size_y > 0);
  g_assert (original_size_x > 0 && original_size_y > 0);

  // Image can only be scaled down with this routine, not up.
  g_assert (size_x < original_size_x);
  g_assert (size_y < original_size_y);

  // Check in advance if the source file looks big enough (we will
  // still need to check return codes as we read() data, of course).
  g_assert (is_large_enough (file,
           offset + ((off_t) original_size_x
               * original_size_y
               * sizeof (float))));

  // Find the stride that we need to use in each dimension to evenly
  // cover the original image space.
  double stride_x = (size_x == 1 ? 0.0
         : (double) (original_size_x - 1) / (size_x - 1));
  double stride_y = (size_y == 1 ? 0.0
         : (double) (original_size_y - 1) / (size_y - 1));

  // Open the file to read data from.
  FILE *fp = fopen (file, "rb");
  // FIXME: we need some error handling and propagation here.
  g_assert (fp != NULL);

  // We will do a row at a time to save some possibly expensive
  // seeking.  So here we have an entire row worth of upper lefts,
  // upper rights, etc.
  float *uls = g_new (float, size_x);
  float *urs = g_new (float, size_x);
  float *lls = g_new (float, size_x);
  float *lrs = g_new (float, size_x);

  // Results of bilinear interpolation for the current row.
  float *interpolated_values = g_new (float, size_x);

  // We will write the reduced resolution version of the image into a
  // temporary file so we can leverage the new_from_file method and
  // avoid trying to stick the whole reduced resolution image in
  // memory.
  FILE *reduced_image = tmpfile ();

  ssize_t ii, jj;
  for ( ii = 0 ; ii < size_y ; ii++ ) {
    size_t read_count;          // For fread calls.
    int return_code;            // For FSEEK64 calls.
    // Input image y index of row above row of interest.
    ssize_t in_ray = floor (ii * stride_y);
    // Due to the vagaries of floating point arithmetic, we might run
    // past the index of our last pixel by a little bit, so we correct.
    if ( in_ray >= original_size_y - 1 ) {
      // We better not be much over the last index though.
      g_assert (in_ray < original_size_y);
      // The index should be an integer, so floor should fix us up.
      in_ray = floor (in_ray);
      g_assert (in_ray == original_size_y - 1);
    }
    g_assert (in_ray < original_size_y);
    // Input image y index of row below row of interest.  If we would
    // be off the image, we just take the last row a second time, and
    // let the interpolation work things out.
    ssize_t in_rby;
    if ( in_ray == original_size_y - 1 ) {
      in_rby = in_ray;
    }
    else {
      in_rby = in_ray + 1;
    }
    // Fetch the row above.
    for ( jj = 0 ; jj < size_x ; jj++ ) {
      // Input image indicies of current upper left corner pixel.
      ssize_t in_ul_x = floor (jj * stride_x);
      // Watch for floating point inexactness (see comment above).
      if ( G_UNLIKELY (in_ul_x >= original_size_x - 1) ) {
        g_assert (in_ul_x < original_size_x);
        in_ul_x = floor (in_ul_x);
        g_assert (in_ul_x == original_size_x - 1);
      }
      g_assert (in_ul_x < original_size_x);
      size_t in_ul_y = in_ray;
      off_t sample_offset
  = offset + sizeof (float) * ((off_t) in_ul_y * original_size_x
             + in_ul_x);
      return_code = FSEEK64 (fp, sample_offset, SEEK_SET);
      g_assert (return_code == 0);
      read_count = fread (&(uls[jj]), sizeof (float), 1, fp);
      g_assert (read_count == 1);
      // If the upper left pixel was the last pixel in the input image,
      if ( in_ul_x == original_size_x - 1 ) {
        // just treat it as the upper right as well,
        urs[jj] = uls[jj];
      }
      // otherwise read the next pixel as the upper right pixel.
      else {
        read_count = fread (&(urs[jj]), sizeof (float), 1, fp);
        g_assert (read_count == 1);
      }
    }
    // Fetch the row below.
    for ( jj = 0 ; jj < size_x ; jj++ ) {
      // Input image indicies of the lower left corner pixel.
      ssize_t in_ll_x = floor (jj * stride_x);
      // Watch for floating point inexactness (see comment above).
      if ( G_UNLIKELY (in_ll_x >= original_size_y - 1) ) {
        g_assert (in_ll_x < original_size_x);
        in_ll_x = floor (in_ll_x);
        g_assert (in_ll_x == original_size_x - 1);
      }
      g_assert (in_ll_x < original_size_x);
      size_t in_ll_y = in_rby;
      off_t sample_offset
  = offset + sizeof (float) * ((off_t) in_ll_y * original_size_x
             + in_ll_x);
      return_code = FSEEK64 (fp, sample_offset, SEEK_SET);
      g_assert (return_code == 0);
      read_count = fread (&(lls[jj]), sizeof (float), 1, fp);
      g_assert (read_count == 1);
      // If the lower left pixel was the last pixel in the input image,
      if ( in_ll_x == original_size_x - 1 ) {
        // just treat it as the lower right as well,
        lrs[jj] = lls[jj];
      }
      // otherwise read the next pixel as the lower right pixel.
      else {
        read_count = fread (&(lrs[jj]), sizeof (float), 1, fp);
        g_assert (read_count == 1);
      }
    }
    // If things aren't in the native byte order, we must byte swap them.
    if ( non_native_byte_order (byte_order) ) {
      for ( jj = 0 ; jj < size_x ; jj++ ) {
        g_assert (sizeof (float) == 4);
        swap_bytes_32 ((unsigned char *) &(uls[jj]));
        swap_bytes_32 ((unsigned char *) &(urs[jj]));
        swap_bytes_32 ((unsigned char *) &(lls[jj]));
        swap_bytes_32 ((unsigned char *) &(lrs[jj]));
      }
    }
    // Perform the interpolation.
    for ( jj = 0 ; jj < size_x ; jj++ ) {
      double delta_x = stride_x * jj - floor (stride_x * jj);
      double delta_y = -(stride_y * ii - floor (stride_y * ii));
      interpolated_values[jj] = bilinear_interpolate (delta_x, delta_y,
                                                      uls[jj], urs[jj],
                                                      lls[jj], lrs[jj]);
    }
    size_t write_count = fwrite (interpolated_values, sizeof (float), size_x,
                                 reduced_image);
    g_assert (write_count == (size_t) size_x);
  }

  // We are done with the temporary buffers.
  g_free (interpolated_values);
  g_free (lrs);
  g_free (lls);
  g_free (urs);
  g_free (uls);

  // Reposition to the beginning of the temporary file to fit with
  // operation of new_from_file_pointer method.
  int return_code = FSEEK64 (reduced_image, (off_t) 0, SEEK_SET);
  g_assert (return_code == 0);

  // The file we have written should be in host byte order, so we need
  // to determine what that is so we can re-read it correctly.
  float_image_byte_order_t host_byte_order;
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
  host_byte_order = FLOAT_IMAGE_BYTE_ORDER_LITTLE_ENDIAN;
#elif G_BYTE_ORDER == G_BIG_ENDIAN
  host_byte_order = FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN;
#else
#  error
#endif

  // Slurp the scaled file back in as an instance.
  FloatImage *self
    = float_image_new_from_file_pointer (size_x, size_y, reduced_image,
                                         (off_t) 0, host_byte_order);

  // Now that we have an instantiated version of the image we are done
  // with this temporary file.
  return_code = fclose (reduced_image);

  return self;
}

// Returns a new FloatImage, for the image corresponding to the given metadata.
FloatImage *
float_image_new_from_metadata(meta_parameters *meta, const char *file)
{
  //return float_image_new_from_file(meta->general->sample_count,
  //    meta->general->line_count, file, 0,
  //    FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  return float_image_band_new_from_metadata(meta, 0, file);
}

// Returns a new FloatImage, for the image band corresponding to the
// given metadata.
FloatImage *
float_image_band_new_from_metadata(meta_parameters *meta,
           int band, const char *file)
{
    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;

    FILE * fp = FOPEN(file, "rb");
    FloatImage * fi = float_image_new(ns, nl);

    int i,j;
    float *buf = MALLOC(sizeof(float)*ns);
    for (i = 0; i < nl; ++i) {
        get_float_line(fp, meta, i+band*nl, buf);
        for (j = 0; j < ns; ++j)
	  if (meta->general->radiometry >= r_SIGMA_DB &&
	      meta->general->radiometry <= r_GAMMA_DB)
            float_image_set_pixel(fi, j, i, pow(10, buf[j]/10.0));
	  else
            float_image_set_pixel(fi, j, i, buf[j]);
        asfPercentMeter((float)i/(float)(nl-1));
    }

    free(buf);
    fclose(fp);

    return fi;
}

// Copy the contents of tile with flattened offset tile_offset from
// the memory cache to the disk file.  Its probably easiest to
// understand this function by looking at how its used.
static void
cached_tile_to_disk (FloatImage *self, size_t tile_offset)
{
  // If we aren't using a tile file, this operation doesn't make
  // sense.
  g_assert (self->tile_file != NULL);

  // We must have a legitimate tile_offset.
  g_assert (tile_offset < self->tile_count);

  // The tile we are trying to copy from cache to disk must be loaded
  // in the cache for this operation to make sense.
  g_assert (self->tile_addresses[tile_offset] != NULL);

  int return_code
    = FSEEK64 (self->tile_file,
        (off_t) tile_offset * self->tile_area * sizeof (float),
        SEEK_SET);
  g_assert (return_code == 0);
  size_t write_count = fwrite (self->tile_addresses[tile_offset],
             sizeof (float), self->tile_area,
             self->tile_file);
  g_assert (write_count == self->tile_area);
}

// Return true iff tile (x, y) is already loaded into the memory cache.
static gboolean
tile_is_loaded (FloatImage *self, ssize_t x, ssize_t y)
{
  g_assert (    x >= 0 && (size_t) x < self->tile_count_x
             && y >= 0 && (size_t) y < self->tile_count_y );

  size_t tile_offset = self->tile_count_x * y + x;

  return self->tile_addresses[tile_offset] != NULL;
}

// Load (currently unloaded) tile (x, y) from disk cache into memory
// cache, possibly displacing the oldest tile already loaded, updating
// the load order queue, and returning the address of the tile loaded.
static float *
load_tile (FloatImage *self, ssize_t x, ssize_t y)
{
  // Make sure we haven't screwed up somehow and not created a tile
  // file when in fact we should have.
  g_assert (self->tile_file != NULL);

  g_assert (!tile_is_loaded (self, x, y));

  // Address into which tile gets loaded (to be returned).
  float *tile_address;

  // Offset of tile in flattened array.
  size_t tile_offset = self->tile_count_x * y + x;

  // We have to check and see if we have to displace an already loaded
  // tile or not.
  if ( self->tile_queue->length == self->cache_size_in_tiles ) {
    // Displace tile loaded longest ago.
    size_t oldest_tile
      = GPOINTER_TO_INT (g_queue_pop_tail (self->tile_queue));
    cached_tile_to_disk (self, oldest_tile);
    tile_address = self->tile_addresses[oldest_tile];
    self->tile_addresses[oldest_tile] = NULL;
  }
  else {
    // Load tile into first free slot.
    tile_address = self->cache + self->tile_queue->length * self->tile_area;
  }

  // Put the new tile address into the index, and put the index into
  // the load order queue.
  self->tile_addresses[tile_offset] = tile_address;
  // Stash in queue by converting to a pointer (so it must fit in an int).
  g_assert (tile_offset < INT_MAX);
  g_queue_push_head (self->tile_queue,
                     GINT_TO_POINTER ((int) tile_offset));

  // Load the tile data.
  int return_code
    = FSEEK64 (self->tile_file,
              (off_t) tile_offset * self->tile_area * sizeof (float),
              SEEK_SET);
  g_assert (return_code == 0);
  clearerr (self->tile_file);
  size_t read_count = fread (tile_address, sizeof (float), self->tile_area,
                             self->tile_file);
  if ( read_count < self->tile_area ) {
    if ( ferror (self->tile_file) ) {
      perror ("error reading tile cache file");
      g_assert_not_reached ();
    }
    if ( feof (self->tile_file) ) {
      fprintf (stderr,
               "nothing left to read in tile cache file at offset %lld\n",
               FTELL64 (self->tile_file));
      g_assert_not_reached ();
    }
  }
  g_assert (read_count == self->tile_area);

  return tile_address;
}

float
float_image_get_pixel (FloatImage *self, ssize_t x, ssize_t y)
{
  // Are we at a valid image pixel?
  asfRequire (x >= 0 && (size_t) x < self->size_x,
              "%d >= 0 && %d < %d\n"
              "Invalid pixel index in the x dimension\n",
              (int)x, (int)x, (int)(self->size_x));
  asfRequire (y >= 0 && (size_t) y < self->size_y,
              "%d >= 0 && %d < %d\n"
              "Invalid pixel index in the y dimension\n",
              (int)y, (int)y, (int)(self->size_y));

  // Get the pixel coordinates, including tile and pixel-in-tile.
  g_assert (sizeof (long int) >= sizeof (size_t));
  ldiv_t pc_x = ldiv (x, self->tile_size), pc_y = ldiv (y, self->tile_size);

  // Offset of tile x, y, where tiles are viewed as pixels normally are.
  size_t tile_offset = self->tile_count_x * pc_y.quot + pc_x.quot;

  // Address of data for tile containing pixel of interest (may still
  // have to be loaded from disk cache).
  float *tile_address = self->tile_addresses[tile_offset];

  // Load the tile containing the pixel of interest if necessary.
  if ( G_UNLIKELY (tile_address == NULL) ) {
    tile_address = load_tile (self, pc_x.quot, pc_y.quot);
  }

  // Return pixel of interest.
  return tile_address[self->tile_size * pc_y.rem + pc_x.rem];
}

void
float_image_set_pixel (FloatImage *self, ssize_t x, ssize_t y, float value)
{
  // Are we at a valid image pixel?
  g_assert (x >= 0 && (size_t) x <= self->size_x);
  g_assert (y >= 0 && (size_t) y <= self->size_y);

  // Get the pixel coordinates, including tile and pixel-in-tile.
  g_assert (sizeof (long int) >= sizeof (size_t));
  ldiv_t pc_x = ldiv (x, self->tile_size), pc_y = ldiv (y, self->tile_size);

  // Offset of tile x, y, where tiles are viewed as pixels normally are.
  size_t tile_offset = self->tile_count_x * pc_y.quot + pc_x.quot;

  // Address of data for tile containing pixel of interest (may still
  // have to be loaded from disk cache).
  float *tile_address = self->tile_addresses[tile_offset];

  // Load the tile containing the pixel of interest if necessary.
  if ( G_UNLIKELY (tile_address == NULL) ) {
    tile_address = load_tile (self, pc_x.quot, pc_y.quot);
  }

  // Set pixel of interest.
  tile_address[self->tile_size * pc_y.rem + pc_x.rem] = value;
}

void
float_image_get_region (FloatImage *self, ssize_t x, ssize_t y, ssize_t size_x,
                        ssize_t size_y, float *buffer)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  g_assert (size_x >= 0);
  g_assert (x >= 0);
  g_assert ((size_t) x + (size_t) size_x - 1 < self->size_x);
  g_assert (size_y >= 0);
  g_assert (y >= 0);
  g_assert ((size_t) y + (size_t) size_y - 1 < self->size_y);

  ssize_t ii, jj;               // Index variables.
  for ( ii = 0 ; ii < size_y ; ii++ ) {
    for ( jj = 0 ; jj < size_x ; jj++ ) {
      // We are essentially returning a subimage from the big image.
      // These are the indicies in the big image (self) of the current
      // pixel.
      size_t ix = x + jj, iy = y + ii;
      buffer[ii * size_x + jj] = float_image_get_pixel (self, ix, iy);
    }
  }
}

void
float_image_set_region (FloatImage *self, size_t x, size_t y, size_t size_x,
                        size_t size_y, float *buffer)
{
  g_assert_not_reached ();      // Stubbed out for now.
  self = self; x = x; y = y; size_x = size_x, size_y = size_y; buffer = buffer;
}

void
float_image_get_row (FloatImage *self, size_t row, float *buffer)
{
  float_image_get_region (self, 0, row, self->size_x, 1, buffer);
}

float
float_image_get_pixel_with_reflection (FloatImage *self, ssize_t x, ssize_t y)
{
  // Reflect at image edges as advertised.
  if ( x < 0 ) {
    x = -x;
  }
  else if ( (size_t) x >= self->size_x ) {
    x = self->size_x - 2 - (x - self->size_x);
  }
  if ( y < 0 ) {
    y = -y;
  }
  else if ( (size_t) y >= self->size_y ) {
    y = self->size_y - 2 - (y - self->size_y);
  }

  return float_image_get_pixel (self, x, y);
}


void
float_image_statistics (FloatImage *self, float *min, float *max,
                        float *mean, float *standard_deviation, float mask)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  *min = FLT_MAX;
  *max = -FLT_MAX;

  // Buffer for one row of samples.
  float *row_buffer = g_new (float, self->size_x);

  // Its best to keep track of things internally using doubles in
  // order to minimize error buildup.
  double mean_as_double = 0;
  double s = 0;

  size_t sample_count = 0;      // Samples considered so far.
  size_t ii, jj;
  for ( ii = 0 ; ii < self->size_y ; ii++ ) {
    asfPercentMeter((double)ii/(double)(self->size_y));
    float_image_get_row (self, ii, row_buffer);
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      float cs = row_buffer[jj];   // Current sample.
      if ( !isnan (mask) && (gsl_fcmp (cs, mask, 0.00000000001) == 0) )
        continue;
      if ( G_UNLIKELY (cs < *min) ) { *min = cs; }
      if ( G_UNLIKELY (cs > *max) ) { *max = cs; }
      double old_mean = mean_as_double;
      mean_as_double += (cs - mean_as_double) / (sample_count + 1);
      s += (cs - old_mean) * (cs - mean_as_double);
      sample_count++;
    }
  }
  asfPercentMeter(1.0);

  g_free (row_buffer);

  if (*min == FLT_MAX || *max == -FLT_MAX)
      asfPrintError ("Image did not contain any valid data!\n");

  double standard_deviation_as_double = sqrt (s / (sample_count - 1));

  g_assert (fabs (mean_as_double) <= FLT_MAX);
  g_assert (fabs (standard_deviation_as_double) <= FLT_MAX);

  *mean = mean_as_double;
  *standard_deviation = standard_deviation_as_double;
}

int
float_image_band_statistics (FloatImage *self, meta_stats *stats,
                             int line_count, int band_no, float mask)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  stats->min = FLT_MAX;
  stats->max = -FLT_MAX;

  // Buffer for one row of samples.
  float *row_buffer = g_new (float, self->size_x);

  // Its best to keep track of things internally using doubles in
  // order to minimize error buildup.
  double mean_as_double = 0;
  double s = 0;

  size_t sample_count = 0;      // Samples considered so far.
  size_t ii, jj;
  for ( ii = (band_no * line_count); // 0-ordered band number times lines is offset into image
        ii < ((size_t)band_no+1) * line_count && ii < self->size_y;
        ii++ )
  {
    asfPercentMeter( (double)(ii - band_no*line_count)/(double)line_count );
    float_image_get_row (self, ii, row_buffer);
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      float cs = row_buffer[jj];   // Current sample.
      if ( !isnan (mask) && (gsl_fcmp (cs, mask, 0.00000000001) == 0) )
        continue;
      if ( G_UNLIKELY (cs < stats->min) ) { stats->min = cs; }
      if ( G_UNLIKELY (cs > stats->max) ) { stats->max = cs; }
      double old_mean = mean_as_double;
      mean_as_double += (cs - mean_as_double) / (sample_count + 1);
      s += (cs - old_mean) * (cs - mean_as_double);
      sample_count++;
    }
  }
  asfPercentMeter(1.0);

  g_free (row_buffer);

  if (stats->min == FLT_MAX || stats->max == -FLT_MAX)
    return 1;

  double standard_deviation_as_double = sqrt (s / (sample_count - 1));

  if (fabs (mean_as_double) > FLT_MAX ||
      fabs (standard_deviation_as_double) > FLT_MAX)
    return 1;

  stats->mean = mean_as_double;
  stats->std_deviation = standard_deviation_as_double;

  return 0;
}

void
float_image_statistics_with_mask_interval (FloatImage *self, float *min,
             float *max, float *mean,
             float *standard_deviation,
             double interval_start,
             double interval_end)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  *min = FLT_MAX;
  *max = -FLT_MAX;

  // Buffer for one row of samples.
  float *row_buffer = g_new (float, self->size_x);

  // Its best to keep track of things internally using doubles in
  // order to minimize error buildup.
  double mean_as_double = 0;
  double s = 0;

  size_t sample_count = 0;      // Samples considered so far.
  size_t ii, jj;
  for ( ii = 0 ; ii < self->size_y ; ii++ ) {
    float_image_get_row (self, ii, row_buffer);
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      float cs = row_buffer[jj];   // Current sample.
      // If in the mask interval, do not consider this pixel any further.
      if ( cs >= interval_start && cs <= interval_end ) {
  continue;
      }
      if ( G_UNLIKELY (cs < *min) ) { *min = cs; }
      if ( G_UNLIKELY (cs > *max) ) { *max = cs; }
      double old_mean = mean_as_double;
      mean_as_double += (cs - mean_as_double) / (sample_count + 1);
      s += (cs - old_mean) * (cs - mean_as_double);
      sample_count++;
    }
  }

  g_free (row_buffer);

  if (*min == FLT_MAX || *max == -FLT_MAX)
      asfPrintError ("Image did not contain any valid data!\n");

  double standard_deviation_as_double = sqrt (s / (sample_count - 1));

  g_assert (fabs (mean_as_double) <= FLT_MAX);
  g_assert (fabs (standard_deviation_as_double) <= FLT_MAX);

  *mean = mean_as_double;
  *standard_deviation = standard_deviation_as_double;
}

void
float_image_approximate_statistics (FloatImage *self, size_t stride,
                                    float *mean, float *standard_deviation,
                                    float mask)
{
  // Rows and columns of samples that fit in image given stride
  // stride.
  size_t sample_columns = ceil (self->size_x / stride);
  size_t sample_rows = ceil (self->size_y / stride);
  // Total number of samples.
  size_t sample_count = sample_columns * sample_rows;

  // Create an image holding the sample values.
  FloatImage *sample_image = float_image_new (sample_columns, sample_rows);

  // Load the sample values.
  size_t current_sample = 0;
  size_t ii;
  for ( ii = 0 ; ii < sample_columns ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; jj < sample_rows ; jj++ ) {
      double sample = float_image_get_pixel (self, ii * stride, jj * stride);
      float_image_set_pixel (sample_image, ii, jj, sample);
      current_sample++;
    }
  }

  // Ensure that we got the right number of samples in our image.
  g_assert (current_sample == sample_count);

  // Compute the exact statistics of the sampled version of the image.
  // The _statistics method wants to compute min and max, so we let
  // it, even though we don't do anything with them (since they are
  // inaccurate).
  float min, max;
  float_image_statistics (sample_image, &min, &max, mean, standard_deviation,
                          mask);

  float_image_free (sample_image);
}

void
float_image_approximate_statistics_with_mask_interval
  (FloatImage *self, size_t stride, float *mean, float *standard_deviation,
   double interval_start, double interval_end)
{
  // This method is a trivial clone-and-modify of
  // float_image_approximate_statistics, but it is totally untested at
  // the moment.
  g_assert_not_reached ();

  // Rows and columns of samples that fit in image given stride
  // stride.
  size_t sample_columns = ceil (self->size_x / stride);
  size_t sample_rows = ceil (self->size_y / stride);
  // Total number of samples.
  size_t sample_count = sample_columns * sample_rows;

  // Create an image holding the sample values.
  FloatImage *sample_image = float_image_new (sample_columns, sample_rows);

  // Load the sample values.
  size_t current_sample = 0;
  size_t ii;
  for ( ii = 0 ; ii < sample_columns ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; jj < sample_rows ; jj++ ) {
      double sample = float_image_get_pixel (self, ii * stride, jj * stride);
      float_image_set_pixel (sample_image, ii, jj, sample);
      current_sample++;
    }
  }

  // Ensure that we got the right number of samples in our image.
  g_assert (current_sample == sample_count);

  // Compute the exact statistics of the sampled version of the image.
  // The _statistics method wants to compute min and max, so we let
  // it, even though we don't do anything with them (since they are
  // inaccurate).
  float min, max;
  float_image_statistics_with_mask_interval (sample_image, &min, &max,
               mean, standard_deviation,
               interval_start, interval_end);

  float_image_free (sample_image);
}

gsl_histogram *
float_image_gsl_histogram (FloatImage *self, float min, float max,
                           size_t num_bins)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  // Initialize the histogram.
  gsl_histogram *hist = gsl_histogram_alloc (num_bins);
  gsl_histogram_set_ranges_uniform (hist, min, max);

  // Buffer for one row of samples.
  float *row_buffer = g_new (float, self->size_x);

  // Populate the histogram over every sample in the image.
  size_t ii, jj;
  for (ii = 0 ; ii < self->size_y ; ii++ ) {
    float_image_get_row (self, ii, row_buffer);
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      gsl_histogram_increment (hist, row_buffer[jj]);
    }
  }

  g_free (row_buffer);

  return hist;
}


float
float_image_apply_kernel (FloatImage *self, ssize_t x, ssize_t y,
                          gsl_matrix_float *kernel)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  g_assert (x >= 0 && (size_t) x < self->size_x);
  g_assert (y >= 0 && (size_t) y < self->size_y);
  g_assert (kernel->size2 % 2 == 1);
  g_assert (kernel->size2 == kernel->size1);

  size_t ks = kernel->size2;    // Kernel size.

  float sum = 0;                // Result.

  size_t ii;
  for ( ii = 0 ; ii < kernel->size1 ; ii++ ) {
    ssize_t iy = y - ks / 2 + ii; // Current image y pixel index.
    size_t jj;
    for ( jj = 0 ; jj < kernel->size2 ; jj++ ) {
      ssize_t ix = x - ks / 2 + jj; // Current image x pixel index
      sum += (gsl_matrix_float_get (kernel, jj, ii)
              * float_image_get_pixel_with_reflection (self, ix, iy));
    }
  }

  return sum;
}

float
float_image_sample (FloatImage *self, float x, float y,
                    float_image_sample_method_t sample_method)
{
  g_assert (x >= 0.0 && x <= (double) self->size_x - 1.0);
  g_assert (y >= 0.0 && y <= (double) self->size_y - 1.0);

  switch ( sample_method ) {

  case FLOAT_IMAGE_SAMPLE_METHOD_NEAREST_NEIGHBOR:
    return float_image_get_pixel (self, round (x), round (y));
    break;

  case FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR:
    {
      // Indicies of points we are interpolating between (x below, y
      // below, etc., where below is interpreted in the numerical
      // sense, not the image orientation sense.).
      size_t xb = floor (x), yb = floor (y), xa = ceil (x), ya = ceil (y);
      size_t ts = self->tile_size;   // Convenience alias.
      // Offset of xb, yb, etc. relative to tiles they lie in.
      size_t xbto = xb % ts, ybto = yb % ts, xato = xa % ts, yato = ya % ts;
      // Values of points we are interpolating between.
      float ul, ur, ll, lr;

      // If the points were are interpolating between don't span a
      // tile edge, we load them straight from tile memory to save
      // some time.
      if ( G_LIKELY (   xbto != ts - 1 && xato != 0
                     && ybto != ts - 1 && yato != 0) ) {
        // The tile indicies.
        size_t tx = xb / ts, ty = yb / ts;
        // Tile offset in flattened list of tile addresses.
        size_t tile_offset = ty * self->tile_count_x + tx;
        float *tile_address = self->tile_addresses[tile_offset];
        if ( G_UNLIKELY (tile_address == NULL) ) {
          tile_address = load_tile (self, tx, ty);
        }
        ul = tile_address[ybto * self->tile_size + xbto];
        ur = tile_address[ybto * self->tile_size + xato];
        ll = tile_address[yato * self->tile_size + xbto];
        lr = tile_address[yato * self->tile_size + xato];
      }
      else {
        // We are spanning a tile edge, so we just get the pixels
        // using the inefficient but easy get_pixel method.
        ul = float_image_get_pixel (self, floor (x), floor (y));
        ur = float_image_get_pixel (self, ceil (x), floor (y));
        ll = float_image_get_pixel (self, floor (x), ceil (y));
        lr = float_image_get_pixel (self, ceil (x), ceil (y));
      }

      // Upper and lower values interpolated in the x direction.
      float ux = ul + (ur - ul) * (x - floor (x));
      float lx = ll + (lr - ll) * (x - floor (x));

      return ux + (lx - ux) * (y - floor (y));
    }
    break;
  case FLOAT_IMAGE_SAMPLE_METHOD_BICUBIC:
    {
      static gboolean first_time_through = TRUE;
      // Splines in the x direction, and their lookup accelerators.
      static double *x_indicies;
      static double *values;
      static gsl_spline **xss;
      static gsl_interp_accel **xias;
      // Spline between splines in the y direction, and lookup accelerator.
      static double *y_spline_indicies;
      static double *y_spline_values;
      static gsl_spline *ys;
      static gsl_interp_accel *yia;

      // All these splines have size 4.
      const size_t ss = 4;

      size_t ii;                // Index variable.

      if ( first_time_through ) {
        // Allocate memory for the splines in the x direction.
        x_indicies = g_new (double, ss);
        values = g_new (double, ss);
        xss = g_new (gsl_spline *, ss);
        xias = g_new (gsl_interp_accel *, ss);
        for ( ii = 0 ; ii < ss ; ii++ ) {
          xss[ii] = gsl_spline_alloc (gsl_interp_cspline, ss);
          xias[ii] = gsl_interp_accel_alloc ();
        }

        // Allocate memory for the spline in the y direction.
        y_spline_indicies = g_new (double, ss);
        y_spline_values = g_new (double, ss);
        ys = gsl_spline_alloc (gsl_interp_cspline, ss);
        yia = gsl_interp_accel_alloc ();
        first_time_through = FALSE;
      }

      // Get the values for the nearest 16 points.
      size_t jj;                // Index variable.
      for ( ii = 0 ; ii < ss ; ii++ ) {
        for ( jj = 0 ; jj < ss ; jj++ ) {
          x_indicies[jj] = floor (x) - 1 + jj;
          values[jj]
            = float_image_get_pixel_with_reflection (self, x_indicies[jj],
                                                     floor (y) - 1 + ii);
        }
        gsl_spline_init (xss[ii], x_indicies, values, ss);
      }

      // Set up the spline that runs in the y direction.
      for ( ii = 0 ; ii < ss ; ii++ ) {
        y_spline_indicies[ii] = floor (y) - 1 + ii;
        y_spline_values[ii] = gsl_spline_eval_check (xss[ii], x, xias[ii]);
      }
      gsl_spline_init (ys, y_spline_indicies, y_spline_values, ss);

      return (float) gsl_spline_eval_check (ys, y, yia);
    }
    break;
  default:
    g_assert_not_reached ();
    return -42;         // Reassure the compiler.
  }
}

gboolean
float_image_equals (FloatImage *self, FloatImage *other, float epsilon)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  // Compare image sizes.
  if ( self->size_x != other->size_x ) {
    return FALSE;
  }
  if ( self->size_y != other->size_y ) {
    return FALSE;
  }

  size_t sz_x = self->size_x; // Convenience alias.
  size_t sz_y = self->size_y;

  // Compare image pixels.
  size_t ii, jj;
  for ( ii = 0 ; ii < sz_y ; ii++ ) {
    for ( jj = 0 ; jj < sz_x ; jj++ ) {
      if ( G_UNLIKELY (gsl_fcmp (float_image_get_pixel (self, jj, ii),
         float_image_get_pixel (other, jj, ii),
         epsilon) != 0) ) {
  return FALSE;
      }
    }
  }

  return TRUE;
}

// Flip an image about a horizontal line through the center of the image
void
float_image_flip_y(FloatImage *self)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  size_t ii, jj;

  for (jj = 0; jj < self->size_y / 2; ++jj) {
    size_t jj2 = self->size_y - 1 - jj;
    for (ii = 0; ii < self->size_x; ++ii) {
      float a = float_image_get_pixel(self, ii, jj);
      float b = float_image_get_pixel(self, ii, jj2);
      float_image_set_pixel(self, ii, jj, b);
      float_image_set_pixel(self, ii, jj2, a);
    }
  }
}

// Flip an image about a vertical line through the center of the image
void
float_image_flip_x(FloatImage *self)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  size_t ii, jj;

  for (ii = 0; ii < self->size_x / 2; ++ii) {
    size_t ii2 = self->size_x - 1 - ii;
    for (jj = 0; jj < self->size_y; ++jj) {
      float a = float_image_get_pixel(self, ii, jj);
      float b = float_image_get_pixel(self, ii2, jj);
      float_image_set_pixel(self, ii, jj, b);
      float_image_set_pixel(self, ii2, jj, a);
    }
  }
}

// Bring the tile cache file on the disk fully into sync with the
// latest image data stored in the memory cache.
static void
synchronize_tile_file_with_memory_cache (FloatImage *self)
{
  // If we aren't using a tile file, this operation doesn't make
  // sense.
  g_assert (self->tile_file != NULL);

  guint ii;
  for ( ii = 0 ; ii < self->tile_queue->length ; ii++ ) {
    size_t tile_offset = GPOINTER_TO_INT (g_queue_peek_nth (self->tile_queue,
                  ii));
    cached_tile_to_disk (self, tile_offset);
  }
}

void
float_image_freeze (FloatImage *self, FILE *file_pointer)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  FILE *fp = file_pointer;  // Convenience alias.

  g_assert (file_pointer != NULL);

  size_t write_count = fwrite (&(self->size_x), sizeof (size_t), 1, fp);
  g_assert (write_count == 1);

  write_count = fwrite (&(self->size_y), sizeof (size_t), 1, fp);
  g_assert (write_count == 1);

  write_count = fwrite (&(self->cache_space), sizeof (size_t), 1, fp);
  g_assert (write_count == 1);

  write_count = fwrite (&(self->cache_area), sizeof (size_t), 1, fp);
  g_assert (write_count == 1);

  write_count = fwrite (&(self->tile_size), sizeof (size_t), 1, fp);
  g_assert (write_count == 1);

  write_count = fwrite (&(self->cache_size_in_tiles), sizeof (size_t), 1, fp);
  g_assert (write_count == 1);

  write_count = fwrite (&(self->tile_count_x), sizeof (size_t), 1, fp);
  g_assert (write_count == 1);

  write_count = fwrite (&(self->tile_count_y), sizeof (size_t), 1, fp);
  g_assert (write_count == 1);

  write_count = fwrite (&(self->tile_count), sizeof (size_t), 1, fp);
  g_assert (write_count == 1);

  write_count = fwrite (&(self->tile_area), sizeof (size_t), 1, fp);
  g_assert (write_count == 1);

  // We don't bother serializing the cache -- its a pain to keep track
  // of and probably almost never worth it.

  // We write the tile queue pointer away, so that when we later thaw
  // the serialized version, we can tell if a cache file is in use or
  // not (if it isn't tile_queue will be NULL).
  write_count = fwrite (&(self->tile_queue), sizeof (GQueue *), 1, fp);
  g_assert (write_count == 1);

  // If there was no cache file...
  if ( self->tile_queue == NULL ) {
    // We store the contents of the first tile and are done.
    write_count = fwrite (self->tile_addresses[0], sizeof (float),
        self->tile_area, fp);
    if ( write_count < self->tile_area ) {
      if ( ferror (fp) ) {
  fprintf (stderr, "Error writing serialized FloatImage instance during "
     "freeze: %s\n", strerror (errno));
  exit (EXIT_FAILURE);
      }
    }
    g_assert (write_count == self->tile_area);
  }
  // otherwise, the in memory cache needs to be copied into the tile
  // file and the tile file saved in the serialized version of self.
  else {
    synchronize_tile_file_with_memory_cache (self);
    float *buffer = g_new (float, self->tile_area);
    size_t ii;
    off_t tmp = FTELL64 (self->tile_file);
    int return_code = FSEEK64 (self->tile_file, 0, SEEK_SET);
    g_assert (return_code == 0);
    for ( ii = 0 ; ii < self->tile_count ; ii++ ) {
      size_t read_count = fread (buffer, sizeof (float), self->tile_area,
         self->tile_file);
      g_assert (read_count == self->tile_area);
      write_count = fwrite (buffer, sizeof (float), self->tile_area, fp);
      g_assert (write_count == self->tile_area);
    }
    return_code = FSEEK64 (self->tile_file, tmp, SEEK_SET);
    g_assert (return_code == 0);
    g_free (buffer);
  }
}

int
float_image_band_store(FloatImage *self, const char *file,
           meta_parameters *meta, int append_flag)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  // Give status
  if (meta->general->band_count == 1)
    asfPrintStatus("Storing image ...\n");
  else
    asfPrintStatus("Storing band ...\n");

  // Establish byte order
  /*
  float_image_byte_order_t byte_order = 0;
  if (strcmp(meta->general->system, "BIG_IEEE") == 0)
    byte_order = FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN;
  else if (strcmp(meta->general->system, "LIL_IEEE") == 0)
    byte_order = FLOAT_IMAGE_BYTE_ORDER_LITTLE_ENDIAN;
  */

  // Open the file to write to.
  FILE *fp = fopen (file, append_flag ? "ab" : "wb");
  // FIXME: we need some error handling and propagation here.
  g_assert (fp != NULL);

  // We will write the image data in horizontal stips one line at a time.
  float *line_buffer = g_new (float, self->size_x);

  // Sanity check
  if (meta->general->line_count != (int)self->size_y ||
      meta->general->sample_count != (int)self->size_x)
  {
      asfPrintError("Inconsistency between metadata and image!\n"
                    "Metadata says: %dx%d LxS, image has %dx%d\n"
                    "Possibly did not write metadata before storing image.\n",
                    meta->general->line_count, meta->general->sample_count,
                    self->size_y, self->size_x);
  }

  // Reorganize data into tiles in tile oriented disk file.
  int ii;
  for ( ii = 0 ; ii < (int)self->size_y ; ii++ ) {
    float_image_get_row (self, ii, line_buffer);

    // Write the data.
    put_float_line(fp, meta, ii, line_buffer);
  }

  // Done with the line buffer.
  g_free (line_buffer);

  // Close file being written.
  int return_code = fclose (fp);
  g_assert (return_code == 0);

  // Return success code.
  return 0;
}

int
float_image_store (FloatImage *self, const char *file,
                   float_image_byte_order_t byte_order)
{
  meta_parameters *meta;
  meta = meta_read(file);

  //float_image_byte_order_t meta_byte_order = 0;
  //if (strcmp(meta->general->system, "BIG_IEEE") == 0)
  //  meta_byte_order = FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN;
  //else if (strcmp(meta->general->system, "LIL_IEEE") == 0)
  //  meta_byte_order = FLOAT_IMAGE_BYTE_ORDER_LITTLE_ENDIAN;

  //if (meta_byte_order != byte_order)
  //    asfPrintWarning("Passed-in byte order overriden by metadata!\n");

  int ret = float_image_band_store(self, file, meta, 0);
  meta_free(meta);

  return ret;
}

/*
 * JPEG ERROR HANDLING:
 *
 * Override the "error_exit" method so that control is returned to the
 * library's caller when a fatal error occurs, rather than calling exit()
 * as the standard error_exit method does.
 *
 * We use C's setjmp/longjmp facility to return control.  This means that the
 * routine which calls the JPEG library must first execute a setjmp() call to
 * establish the return point.  We want the replacement error_exit to do a
 * longjmp().  But we need to make the setjmp buffer accessible to the
 * error_exit routine.  To do this, we make a private extension of the
 * standard JPEG error handler object.  (If we were using C++, we'd say we
 * were making a subclass of the regular error handler.)
 *
 * Here's the extended error handler struct:
 */
struct my_error_mgr {
  struct jpeg_error_mgr pub;  /* "public" fields */

  jmp_buf setjmp_buffer;  /* for return to caller */
};

typedef struct my_error_mgr * my_error_ptr;

METHODDEF(void)
my_error_exit (j_common_ptr cinfo)
{
  /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
  my_error_ptr myerr = (my_error_ptr) cinfo->err;

  /* Always display the message. */
  /* We could postpone this until after returning, if we chose. */
  (*cinfo->err->output_message) (cinfo);

  /* Return control to the setjmp point */
  longjmp(myerr->setjmp_buffer, 1);
}

int
float_image_export_as_jpeg (FloatImage *self, const char *file,
                            size_t max_dimension, double mask)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  //size_t scale_factor;          // Scale factor to use for output image.
  float fscale_factor;
  size_t scale_factor;
  if ( self->size_x > self->size_y ) {
    //scale_factor = ceil ((double) self->size_x / max_dimension);
    fscale_factor = (float)self->size_x / (float)max_dimension;
  }
  else {
    //scale_factor = ceil ((double) self->size_y / max_dimension);
    fscale_factor = (float)self->size_y / (float)max_dimension;
  }

  // We want the scale factor to be odd, so that we can easily use a
  // standard kernel to average things.
  //if ( scale_factor % 2 == 0 ) {
    //scale_factor++;
  //}

  // Output JPEG x and y dimensions.
  scale_factor = (size_t)(fscale_factor + 0.5);
  if (scale_factor < 1) {
    // Someone tried to _grow_ the image rather than shrink it ...unsupported at this time!
    asfPrintWarning("Maximum dimension that was selected is larger than the maximum\n"
        "dimension in the image.  Only scaling down is supported.  Defaulting to a\n"
        "scale factor of 1.0 (maintain original size.)\n");
  }
  scale_factor = scale_factor < 1 ? 1 : scale_factor;
  size_t osx = (size_t)((float)self->size_x / (float)scale_factor);
  size_t osy = (size_t)((float)self->size_y / (float)scale_factor);
  while ((osx >= MIN_DIMENSION || osy >= MIN_DIMENSION) &&
         (osx % 2 == 0         || osy % 2 == 0)         &&
         scale_factor != 1)
  {
    // Fine tune scale factor until output image dimensions are odd
    // in both directions so an odd-sized filter kernel will fit (odd
    // sized kernels have a center pixel)
    fscale_factor += 0.25;
    scale_factor = (size_t)(fscale_factor + 0.5);
    osx = (size_t)((float)self->size_x / (float)scale_factor);
    osy = (size_t)((float)self->size_y / (float)scale_factor);
  }
  asfRequire(osx >= MIN_DIMENSION || osy >= MIN_DIMENSION, "Output dimensions too small");
  size_t kernel_size = scale_factor;
  kernel_size = kernel_size % 2 ? kernel_size : kernel_size - 1;

  // Number of pixels in output image.
  size_t pixel_count = osx * osy;

  // Pixels of the output image.
  unsigned char *pixels = g_new (unsigned char, pixel_count);

  JSAMPLE test_jsample;         // For verifying properties of JSAMPLE type.
  /* Here are some very funky checks to try to ensure that the JSAMPLE
     really is the type we expect, so we can scale properly.  */
  g_assert (sizeof (unsigned char) == 1);
  g_assert (sizeof (unsigned char) == sizeof (JSAMPLE));
  test_jsample = 0;
  test_jsample--;
  g_assert (test_jsample == UCHAR_MAX);

  // Stuff needed by libjpeg.
  struct jpeg_compress_struct cinfo;
  struct my_error_mgr jerr;
  //cinfo.err = jpeg_std_error (&jerr);
  jpeg_create_compress (&cinfo);

  // Open output file.
  FILE *fp = fopen (file, "wb");
  if ( fp == NULL ) {
    printf("Error opening file %s: %s\n", file, strerror(errno));
    return FALSE;
  }

  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = my_error_exit;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    jpeg_destroy_compress(&cinfo);
    g_free(pixels);
    fclose(fp);
    return 1;
  }
  // Connect jpeg output to the output file to be used.
  jpeg_stdio_dest (&cinfo, fp);

  // Set image parameters that libjpeg needs to know about.
  cinfo.image_width = osx;
  cinfo.image_height = osy;
  cinfo.input_components = 1;   // Grey scale => 1 color component / pixel.
  cinfo.in_color_space = JCS_GRAYSCALE;
  jpeg_set_defaults (&cinfo);   // Use default compression parameters.
  // Reassure libjpeg that we will be writing a complete JPEG file.
  jpeg_start_compress (&cinfo, TRUE);

  // Gather image statistics so we know how to map image values into
  // the output.
  float min, max, mean, standard_deviation;
  float_image_statistics (self, &min, &max, &mean, &standard_deviation, mask);

  // If the statistics don't work, something is beastly wrong and we
  // don't want to deal with it.
#ifndef solaris
#ifndef win32
  // Solaris doesn't have isfinite().
  g_assert (isfinite (min) && isfinite (max) && isfinite (mean)
            && isfinite (standard_deviation));
#endif
#endif

  // If min == max, the pixel values are all the same.  There is no
  // reason to average or scale anything, so we don't.  There is a
  // good chance that the user would like zero to correspond to black,
  // so we do that.  Anything else will be pure white.
  if ( min == max ) {
    // FIXME: this path is broken.  The trouble is that much of the
    // stuff after this if branch shouldn't happen if we do this, but
    // some of it should.  So for now its disabled.
    g_assert_not_reached ();
    unsigned char oval;         // Output value to use.
    if ( min == 0.0 ) {
      oval = 0;
    }
    else {
      oval = UCHAR_MAX;
    }
    size_t ii, jj;
    for ( ii = 0 ; ii < osy ; ii++ ) {
      for ( jj = 0 ; jj < osx ; jj++ ) {
        pixels[ii * osx + jj] = oval;
      }
    }
  }

  // Range of input pixel values which are to be linearly scaled into
  // the output (values outside this range will be clamped).
  double lin_min = mean - 2 * standard_deviation;
  double lin_max = mean + 2 * standard_deviation;

  // As advertised, we will average pixels together.
  //g_assert (scale_factor % 2 != 0);
  //size_t kernel_size = scale_factor;
  gsl_matrix_float *averaging_kernel = NULL;
  size_t ii, jj;
  if (scale_factor > 1) {
    averaging_kernel = gsl_matrix_float_alloc (kernel_size, kernel_size);
    float kernel_value = 1.0 / ((float)kernel_size * kernel_size);
    for ( ii = 0 ; ii < averaging_kernel->size1 ; ii++ ) {
      for ( jj = 0 ; jj < averaging_kernel->size2 ; jj++ ) {
        gsl_matrix_float_set (averaging_kernel, ii, jj, kernel_value);
      }
    }
  }

    // Sample input image, putting scaled results into output image.
  size_t sample_stride = scale_factor;
  for ( ii = 0 ; ii < osy ; ii++ ) {
    for ( jj = 0 ; jj < osx ; jj++ ) {
      // Input image average pixel value.
      float ival;
      if (scale_factor > 1) {
        ival = float_image_apply_kernel (self, jj * sample_stride,
                                         ii * sample_stride,
                                         averaging_kernel);
      }
      else if (scale_factor == 1) {
        ival = float_image_get_pixel(self, jj, ii);
      }
      else {
        asfPrintError("Invalid scale factor.  Scale factor must be 1 or greater.\n");
      }
      unsigned char oval;       // Output value.

      if (!meta_is_valid_double(ival)) {
        oval = 0;
      }
      else if ( ival < lin_min ) {
        oval = 0;
      }
      else if ( ival > lin_max) {
        oval = UCHAR_MAX;
      }
      else {
        int oval_int
            = round (((ival - lin_min) / (lin_max - lin_min)) * UCHAR_MAX);
        // Make sure we haven't screwed up the scaling.
        g_assert (oval_int >= 0 && oval_int <= UCHAR_MAX);
        oval = oval_int;
      }
      pixels[ii * osx + jj] = oval;
    }
  }

  if (averaging_kernel != NULL) gsl_matrix_float_free(averaging_kernel);

  // Write the jpeg, one row at a time.
  const int rows_to_write = 1;
  JSAMPROW *row_pointer = g_new (JSAMPROW, rows_to_write);
  while ( cinfo.next_scanline < cinfo.image_height ) {
    int rows_written;
    row_pointer[0] = &(pixels[cinfo.next_scanline * osx]);
    rows_written = jpeg_write_scanlines (&cinfo, row_pointer, rows_to_write);
    g_assert (rows_written == rows_to_write);
  }
  g_free (row_pointer);

  // Finsh compression and close the jpeg.
  jpeg_finish_compress (&cinfo);
  int return_code = fclose (fp);
  g_assert (return_code == 0);
  jpeg_destroy_compress (&cinfo);

  g_free (pixels);

  return 0;                     // Return success indicator.
}

int
float_image_export_as_jpeg_with_mask_interval (FloatImage *self,
                 const char *file,
                 ssize_t max_dimension,
                 double interval_start,
                 double interval_end)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  size_t scale_factor;          // Scale factor to use for output image.
  if ( self->size_x > self->size_y ) {
    scale_factor = ceil ((double) self->size_x / max_dimension);
  }
  else {
    scale_factor = ceil ((double) self->size_y / max_dimension);
  }

  // We want the scale factor to be odd, so that we can easily use a
  // standard kernel to average things.
  if ( scale_factor % 2 == 0 ) {
    scale_factor++;
  }

  // Output JPEG x and y dimensions.
  size_t osx = self->size_x / scale_factor;
  size_t osy = self->size_y / scale_factor;

  // Number of pixels in output image.
  size_t pixel_count = osx * osy;

  // Pixels of the output image.
  unsigned char *pixels = g_new (unsigned char, pixel_count);

  JSAMPLE test_jsample;         // For verifying properties of JSAMPLE type.
  /* Here are some very funky checks to try to ensure that the JSAMPLE
     really is the type we expect, so we can scale properly.  */
  g_assert (sizeof (unsigned char) == 1);
  g_assert (sizeof (unsigned char) == sizeof (JSAMPLE));
  test_jsample = 0;
  test_jsample--;
  g_assert (test_jsample == UCHAR_MAX);

  // Stuff needed by libjpeg.
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  cinfo.err = jpeg_std_error (&jerr);
  jpeg_create_compress (&cinfo);

  // Open output file.
  FILE *fp = fopen (file, "wb");
  if ( fp == NULL ) { perror ("error opening file"); }
  // FIXME: we need some error handling and propagation here.
  g_assert (fp != NULL);

  // Connect jpeg output to the output file to be used.
  jpeg_stdio_dest (&cinfo, fp);

  // Set image parameters that libjpeg needs to know about.
  cinfo.image_width = osx;
  cinfo.image_height = osy;
  cinfo.input_components = 1;   // Grey scale => 1 color component / pixel.
  cinfo.in_color_space = JCS_GRAYSCALE;
  jpeg_set_defaults (&cinfo);   // Use default compression parameters.
  // Reassure libjpeg that we will be writing a complete JPEG file.
  jpeg_start_compress (&cinfo, TRUE);

  // Gather image statistics so we know how to map image values into
  // the output.
  float min, max, mean, standard_deviation;
  float_image_statistics_with_mask_interval (self, &min, &max, &mean,
               &standard_deviation,
               interval_start,
               interval_end);

  // If the statistics don't work, something is beastly wrong and we
  // don't want to deal with it.
#ifndef solaris
#ifndef win32
  // Solaris doesn't have isfinite().
  g_assert (isfinite (min) && isfinite (max) && isfinite (mean)
            && isfinite (standard_deviation));
#endif
#endif

  // If min == max, the pixel values are all the same.  There is no
  // reason to average or scale anything, so we don't.  There is a
  // good chance that the user would like zero to correspond to black,
  // so we do that.  Anything else will be pure white.
  if ( min == max ) {
    // FIXME: this path seems to be broken somehow -- we end up with
    // jpegs of size zero.
    g_assert_not_reached ();
    unsigned char oval;         // Output value to use.
    if ( min == 0.0 ) {
      oval = 0;
    }
    else {
      oval = UCHAR_MAX;
    }
    size_t ii, jj;
    for ( ii = 0 ; ii < osy ; ii++ ) {
      for ( jj = 0 ; jj < osx ; jj++ ) {
        pixels[ii * osx + jj] = oval;
      }
    }
    return 0;
  }

  // Range of input pixel values which are to be linearly scaled into
  // the output (values outside this range will be clamped).
  double lin_min = mean - 2 * standard_deviation;
  double lin_max = mean + 2 * standard_deviation;

  // As advertised, we will average pixels together.
  g_assert (scale_factor % 2 != 0);
  size_t kernel_size = scale_factor;
  gsl_matrix_float *averaging_kernel
    = gsl_matrix_float_alloc (kernel_size, kernel_size);
  float kernel_value = 1.0 / ((float)kernel_size * kernel_size);
  size_t ii, jj;                // Index values.
  for ( ii = 0 ; ii < averaging_kernel->size1 ; ii++ ) {
    for ( jj = 0 ; jj < averaging_kernel->size2 ; jj++ ) {
      gsl_matrix_float_set (averaging_kernel, ii, jj, kernel_value);
    }
  }

  // Sample input image, putting scaled results into output image.
  size_t sample_stride = scale_factor;
  for ( ii = 0 ; ii < osy ; ii++ ) {
    for ( jj = 0 ; jj < osx ; jj++ ) {
      // Input image average pixel value.
      float ival = float_image_apply_kernel (self, jj * sample_stride,
                                             ii * sample_stride,
                                             averaging_kernel);
      unsigned char oval;       // Output value.
      if ( ival < lin_min ) {
        oval = 0;
      }
      else if ( ival > lin_max) {
        oval = UCHAR_MAX;
      }
      else {
        int oval_int
          = round (((ival - lin_min) / (lin_max - lin_min)) * UCHAR_MAX);
        // Make sure we haven't screwed up the scaling.
        g_assert (oval_int >= 0 && oval_int <= UCHAR_MAX);
        oval = oval_int;
      }
      pixels[ii * osx + jj] = oval;
    }
  }

  // Write the jpeg, one row at a time.
  const int rows_to_write = 1;
  JSAMPROW *row_pointer = g_new (JSAMPROW, rows_to_write);
  while ( cinfo.next_scanline < cinfo.image_height ) {
    int rows_written;
    row_pointer[0] = &(pixels[cinfo.next_scanline * osx]);
    rows_written = jpeg_write_scanlines (&cinfo, row_pointer, rows_to_write);
    g_assert (rows_written == rows_to_write);
  }
  g_free (row_pointer);

  // Finsh compression and close the jpeg.
  jpeg_finish_compress (&cinfo);
  int return_code = fclose (fp);
  g_assert (return_code == 0);
  jpeg_destroy_compress (&cinfo);

  g_free (pixels);

  return 0;                     // Return success indicator.
}

int
float_image_export_as_tiff (FloatImage *self, const char *file,
                            size_t max_dimension, double mask)
{
    g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

    size_t scale_factor;          // Scale factor to use for output image.
    if ( self->size_x > self->size_y ) {
        scale_factor = ceil ((double) self->size_x / max_dimension);
    }
    else {
        scale_factor = ceil ((double) self->size_y / max_dimension);
    }

    // We want the scale factor to be odd, so that we can easily use a
    // standard kernel to average things.
    if ( scale_factor % 2 == 0 ) {
        scale_factor++;
    }

    // Output JPEG x and y dimensions.
    size_t osx = self->size_x / scale_factor;
    size_t osy = self->size_y / scale_factor;

    // Number of pixels in output image.
    size_t pixel_count = osx * osy;

    // Pixels of the output image.
    unsigned char *pixels = g_new (unsigned char, pixel_count);

    // Stuff needed by libtiff
    TIFF *otif = NULL;

    // Open output file.
    otif = TIFFOpen(file, "wb");
    if (otif == NULL ) {
        asfPrintError("Error opening TIFF file %s: %s\n", file, strerror(errno));
        return FALSE;
    }

    // Initialize the TIFF file
    TIFFSetField(otif, TIFFTAG_SUBFILETYPE, 0);
    TIFFSetField(otif, TIFFTAG_IMAGEWIDTH, osx);
    TIFFSetField(otif, TIFFTAG_IMAGELENGTH, osy);
    TIFFSetField(otif, TIFFTAG_BITSPERSAMPLE, 8); // byte greyscale
    TIFFSetField(otif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
    TIFFSetField(otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
    TIFFSetField(otif, TIFFTAG_SAMPLESPERPIXEL, 1);
    TIFFSetField(otif, TIFFTAG_ROWSPERSTRIP, 1);
    TIFFSetField(otif, TIFFTAG_XRESOLUTION, 1.0);
    TIFFSetField(otif, TIFFTAG_YRESOLUTION, 1.0);
    TIFFSetField(otif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_NONE);
    TIFFSetField(otif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
    TIFFSetField(otif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_UINT);

  // Gather image statistics so we know how to map image values into
  // the output.
    float min, max, mean, standard_deviation;
    float_image_statistics (self, &min, &max, &mean, &standard_deviation, mask);

  // If the statistics don't work, something is beastly wrong and we
  // don't want to deal with it.
#ifndef solaris
#ifndef win32
  // Solaris doesn't have isfinite().
  g_assert (isfinite (min) && isfinite (max) && isfinite (mean)
            && isfinite (standard_deviation));
#endif
#endif

  // If min == max, the pixel values are all the same.  There is no
  // reason to average or scale anything, so we don't.  There is a
  // good chance that the user would like zero to correspond to black,
  // so we do that.  Anything else will be pure white.
  if ( min == max ) {
    // FIXME: this path is broken.  The trouble is that much of the
    // stuff after this if branch shouldn't happen if we do this, but
    // some of it should.  So for now its disabled.
               g_assert_not_reached ();
       unsigned char oval;         // Output value to use.
       if ( min == 0.0 ) {
           oval = 0;
       }
       else {
           oval = UCHAR_MAX;
       }
       size_t ii, jj;
       for ( ii = 0 ; ii < osy ; ii++ ) {
           for ( jj = 0 ; jj < osx ; jj++ ) {
               pixels[ii * osx + jj] = oval;
           }
       }
  }

  // Range of input pixel values which are to be linearly scaled into
  // the output (values outside this range will be clamped).
  double lin_min = mean - 2 * standard_deviation;
  double lin_max = mean + 2 * standard_deviation;

  // As advertised, we will average pixels together.
  g_assert (scale_factor % 2 != 0);
  size_t kernel_size = scale_factor;
  gsl_matrix_float *averaging_kernel
          = gsl_matrix_float_alloc (kernel_size, kernel_size);
  float kernel_value = 1.0 / ((float)kernel_size * kernel_size);
  size_t ii, jj;                // Index values.
  for ( ii = 0 ; ii < averaging_kernel->size1 ; ii++ ) {
      for ( jj = 0 ; jj < averaging_kernel->size2 ; jj++ ) {
          gsl_matrix_float_set (averaging_kernel, ii, jj, kernel_value);
      }
  }

  // Sample input image, putting scaled results into output image.
  size_t sample_stride = scale_factor;
  for ( ii = 0 ; ii < osy ; ii++ ) {
      for ( jj = 0 ; jj < osx ; jj++ ) {
      // Input image average pixel value.
          float ival = float_image_apply_kernel (self, jj * sample_stride,
                  ii * sample_stride,
                  averaging_kernel);
          unsigned char oval;       // Output value.

          if (!meta_is_valid_double(ival)) {
              oval = 0;
          }
          else if ( ival < lin_min ) {
              oval = 0;
          }
          else if ( ival > lin_max) {
              oval = UCHAR_MAX;
          }
          else {
              int oval_int
                      = round (((ival - lin_min) / (lin_max - lin_min)) * UCHAR_MAX);
              // Make sure we haven't screwed up the scaling.
              g_assert (oval_int >= 0 && oval_int <= UCHAR_MAX);
              oval = oval_int;
          }
          pixels[ii * osx + jj] = oval;
      }
  }

  gsl_matrix_float_free(averaging_kernel);
  // Write the tiff, one row at a time.
  unsigned char *byte_line=NULL;
  for (ii=0; ii<osy; ii++) {
      byte_line = (unsigned char*)(&pixels[ii*osx]);
      TIFFWriteScanline(otif, byte_line, ii, 0);
  }

  // Finalize the TIFF
  if (otif != NULL) {
      TIFFClose (otif);
  }

  g_free (pixels);

  return 0;                     // Return success indicator.
}

int
float_image_export_as_csv (FloatImage *self, const char * filename)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  size_t ii, jj;

  g_assert (self->size_x < 256);
  g_assert (self->size_y < 256);

  FILE *fout = fopen (filename, "wt");
  if ( fout == NULL ) {
    printf("Attempting to create csv file: %s\n", filename);
    perror ("error opening file");
  }

  for (ii = 0; ii < self->size_x; ++ii) {
    for (jj = 0; jj < self->size_y; ++jj) {
      fprintf(fout, "%5.3f%c", float_image_get_pixel(self, ii, jj),
        jj == self->size_y - 1 ? '\n' : ',');
    }
  }

  fclose(fout);
  return 0;
}

size_t
float_image_get_cache_size (FloatImage *self)
{
  g_assert_not_reached ();      // Stubbed out for now.
  // Compiler reassurance.
  self = self;
  return 0;
}

void
float_image_set_cache_size (FloatImage *self, size_t size)
{
  g_assert_not_reached ();      // Stubbed out for now.
  // Compiler reassurance.
  self = self; size = size;
}

FloatImage *
float_image_ref (FloatImage *self)
{
  g_assert (self->reference_count > 0); // Harden against missed ref=1 in new

  self->reference_count++;

  return self;
}

void
float_image_unref (FloatImage *self)
{
  self->reference_count--;

  if ( self->reference_count == 0 ) {
    float_image_free (self);
  }
}

void
float_image_free (FloatImage *self)
{
  // Close the tile file (which shouldn't have to remove it since its
  // already unlinked), if we were ever using it.
  if ( self->tile_file != NULL ) {
    int return_code = fclose (self->tile_file);
    g_assert (return_code == 0);
  }

  // Deallocate dynamic memory.

  g_free (self->tile_addresses);

  // If we didn't need a tile file, we also won't have a tile queue.
  if ( self->tile_queue != NULL ) {
    g_queue_free (self->tile_queue);
  }

  g_free (self->cache);

  if (self->tile_file_name) {

      // On Windows (mingw), we delete the file now, since it isn't
      // automatically deleted on close.
#ifdef win32
      int return_code = unlink_tmp_file (self->tile_file_name->str);
      g_assert (return_code == 0);
#endif

      g_string_free(self->tile_file_name, TRUE);
  }

  g_free (self);
}
