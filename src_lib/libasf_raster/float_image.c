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

#include <glib.h>
#if GLIB_CHECK_VERSION (2, 6, 0)
#  include <glib/gstdio.h>
#endif
#include <gsl/gsl_spline.h>
#include <gsl/gsl_histogram.h>
#include <gsl/gsl_math.h>

#include <jpeglib.h>
#include "asf_nan.h"
#include "float_image.h"

#ifndef linux
#ifndef win32
static double
round (double arg)
{
  return floor (arg + 0.5);
}
#endif // #ifndef win32
#endif // #ifndef linux

// Default cache size to use is 16 megabytes.
static const size_t default_cache_size = 16 * 1048576;
// This class wide data element keeps track of the number of temporary
// tile files opened by the current process, in order to give them
// unique names.
static unsigned long current_tile_file_number = 0;
// We need to ensure that multiple threads trying to create their own
// images concurently don't end up with the sampe temporary file
// names.
G_LOCK_DEFINE_STATIC (current_tile_file_number);

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

	  // Address where the current row of pixels should end up.
    //	  float *row_address
    //	    = self->tile_addresses[0] + ii * self->tile_size * sizeof (float);


    size_t ii, jj;
    for ( ii = 0 ; ii < self->size_y ; ii++ ) {
      float *row_address
	= self->cache + ii * self->tile_size;
      for ( jj = 0 ; jj < self->size_x ; jj++ ) {
	row_address[jj] = -42.0;
      }
    }


    self->tile_addresses = g_new0 (float *, self->tile_count);
    g_assert (NULL == 0x0);     // Ensure g_new0 effectively sets to NULL.
    // The tile queue shouldn't ever be needed in this case.
    self->tile_queue = NULL;
    // The tile file shouldn't ever be needed, so we set it to NULL to
    // indicate this to a few other methods that use it directly, and
    // to hopefully ensure that it triggers an exception if it is
    // used.
    self->tile_file = NULL;

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
  while ( (2 * pow (self->tile_size, 2.0)
           * ceil ((double) largest_dimension / self->tile_size))
          > self->cache_area ) {
    self->tile_size--;
  }

  // Area of tiles, in pixels.
  self->tile_area = (size_t) pow (self->tile_size, 2.0);

  // Number of tiles which will fit in image cache.
  self->cache_size_in_tiles = self->cache_area / self->tile_area;
  // Can we fit at least as much as we intended in the cache?
  g_assert (self->cache_size_in_tiles
            >= 2 * (size_t) ceil ((double) largest_dimension
                                  / self->tile_size));

  // Number of tiles image has been split into in x and y directions.
  self->tile_count_x = (size_t) ceil ((double) self->size_x / self->tile_size);
  self->tile_count_y = (size_t) ceil ((double) self->size_y / self->tile_size);

  // Total number of tiles image tiles image has been split into.
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

  // Create the temporary tile oriented storage file.  This gets
  // filled in in different ways depending on which creation routine
  // we are using.
  GString *tile_file_name = g_string_new ("");
  gchar *current_dir = g_get_current_dir ();

  // Here we do a slightly weird thing: if the current directory is
  // writable, we create a temporary file in the current directory.
  // We do this because the temporary file could well be pretty big
  // and /tmp often maps to a small file system.  The idea is that the
  // directory the user is in is more likely to have the extra space
  // required to hold the temporary file.  Of course, if they have
  // been carefully calculating their space requirements, they may be
  // disappointed.  We use a weird name that no sane user would ever
  // use for one of their files, we hope.
  G_LOCK (current_tile_file_number);
  g_assert (sizeof (long) >= sizeof (pid_t));
  g_string_append_printf (tile_file_name,
                          "%s/.float_image_tile_file_uNiQuIfY_nAmE_%ld_%lu",
                          current_dir, (long) getpid (),
                          current_tile_file_number);
  g_free (current_dir);
  // This hard coded limit on the current number used to uniqueify
  // file names limits us to creating no more than ULONG_MAX instances
  // during a process.
  g_assert (current_tile_file_number < ULONG_MAX);
  current_tile_file_number++;
  G_UNLOCK (current_tile_file_number);
  // We block signals while we create and unlink this file, so we
  // don't end up leaving a huge temporary file somewhere.
  sigset_t all_signals, old_set;
  int return_code = sigfillset (&all_signals);
  g_assert (return_code == 0);
  return_code = sigprocmask (SIG_SETMASK, &all_signals, &old_set);
  // FIXME?: It might be faster to use file descriptor based I/O
  // everywhere, or at least for the big transfers.  I'm not sure its
  // worth the trouble though.
  self->tile_file = fopen (tile_file_name->str, "w+");
  if ( self->tile_file == NULL ) {
    if ( errno != EACCES ) {
      g_warning ("couldn't create file in current directory, and it wasn't"
                 "just a permissions problem");
    }
    else {
      // Couldn't open in current directory, so try using tmpfile,
      // which opens the file in the standardish place for the system.
      // See the comment above about why opening in /tmp or the like
      // is potentially bad.
      self->tile_file = tmpfile ();
      g_assert (self->tile_file != NULL);
    }
  }
  else {
    return_code = unlink (tile_file_name->str);
    g_assert (return_code == 0);
  }
  g_assert (self->tile_file != NULL);
  return_code = sigprocmask (SIG_SETMASK, &old_set, NULL);
  g_string_free (tile_file_name, TRUE);

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
                 "Error creating tile cache file for float_image instance: "
                 "%s\n", strerror (errno));
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
                 "Error creating tile cache file for float_image instance: "
                 "%s\n", strerror (errno));
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
static void
swap_bytes_16 (unsigned char *in)
{
  g_assert (sizeof (unsigned char) == 1);
  int tmp = in[0];
  in[0] = in[1];
  in[1] = tmp;
}

FloatImage *
float_image_new_from_memory (ssize_t size_x, ssize_t size_y, float *buffer)
{
  g_assert (size_x > 0 && size_y > 0);

  g_assert_not_reached ();      // Stubbed out for now.
  // Compiler reassurance.
  size_x = size_x;
  size_y = size_y;
  buffer = buffer;
  return NULL;
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
  g_assert (model->size_x > 0 && model->size_y > 0);

  g_assert (scale_factor > 0);
  g_assert (scale_factor % 2 == 1);

  FloatImage *self = float_image_new (ceil (model->size_x / scale_factor),
                                      ceil (model->size_y / scale_factor));

  // Form an averaging kernel of the required size.
  size_t kernel_size = scale_factor;
  gsl_matrix_float *averaging_kernel
    = gsl_matrix_float_alloc (kernel_size, kernel_size);
  float kernel_value = 1.0 / pow (kernel_size, 2.0);
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

  g_assert (self->size_x == ceil (model->size_x / scale_factor));
  g_assert (self->size_y == ceil (model->size_y / scale_factor));

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
  g_assert (is_large_enough (file, offset + ((off_t) size_x * size_y
					     * sizeof (float))));

  // Open the file to read data from.
  FILE *fp = fopen (file, "r");
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

// Return true iff file referred to by file_pointer is larger than size.
static gboolean
file_pointed_to_larger_than (FILE *file_pointer, off_t size)
{
  struct stat stat_buffer;
  int return_code = fstat (fileno (file_pointer), &stat_buffer);
  g_assert (return_code == 0);
  return stat_buffer.st_size >= size;
}


FloatImage *
float_image_new_from_file_pointer (ssize_t size_x, ssize_t size_y,
                                   FILE *file_pointer, off_t offset,
                                   float_image_byte_order_t byte_order)
{
  g_assert (size_x > 0 && size_y > 0);

  // Check in advance if the source file looks big enough (we will
  // still need to check return codes as we read() data, of course).
  g_assert (file_pointed_to_larger_than (file_pointer,
					 offset + ((off_t) size_x * size_y
						   * sizeof (float))));

  FloatImage *self = initialize_float_image_structure (size_x, size_y);

  FILE *fp = file_pointer;      // Convenience alias.

  // Seek to the indicated offset in the file.
  int return_code = fseeko (fp, offset, SEEK_CUR);
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
                     "Error creating tile cache file for float_image "
                     "instance: %s\n", strerror (errno));
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
                       "Error creating tile cache file for float_image "
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
                     "Error creating tile cache file for float_image "
                     "instance: %s\n", strerror (errno));
            // and exit.
            exit (EXIT_FAILURE);
          }
        }
      }
    }

    // Did we write the correct total amount of data?
    g_assert (ftello (self->tile_file)
	      == (off_t) self->tile_area * self->tile_count * sizeof (float));

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
  FILE *fp = fopen (file, "r");
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
    int return_code;            // For fseeko calls.
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
      return_code = fseeko (fp, sample_offset, SEEK_SET);
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
      return_code = fseeko (fp, sample_offset, SEEK_SET);
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
  int return_code = fseeko (reduced_image, (off_t) 0, SEEK_SET);
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

FloatImage *
float_image_new_from_file_pointer_with_sample_type
  (ssize_t size_x, ssize_t size_y, FILE *file_pointer, off_t offset,
   float_image_byte_order_t byte_order, float_image_sample_type sample_type)
{
  g_assert (size_x > 0 && size_y > 0);

  // Check in advance if the source file looks big enough (we will
  // still need to check return codes as we read() data, of course).
  switch ( sample_type ) {
  case FLOAT_IMAGE_SAMPLE_TYPE_SIGNED_TWO_BYTE_INTEGER:
    g_assert (file_pointed_to_larger_than (file_pointer,
					   offset + ((off_t) size_x * size_y
						     * sizeof (int16_t))));
    break;
  default:
    g_assert_not_reached ();
  }

  FloatImage *self = initialize_float_image_structure (size_x, size_y);

  FILE *fp = file_pointer;	// Convenience alias.

  // Seek to the indicated offset in the file.
  int return_code = fseeko (fp, offset, SEEK_CUR);
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

    // Buffer capable of holding a full strip of the input sample
    // type.  Initializer reassures compiler without making the thing
    // usable without initialization
    void *input_buffer = NULL;
    switch ( sample_type ) {
    case FLOAT_IMAGE_SAMPLE_TYPE_SIGNED_TWO_BYTE_INTEGER:
      input_buffer = g_new (int16_t, self->tile_size * self->size_x);
      break;
    default:
      g_assert_not_reached ();
    }

    // Buffer capable of holding a full strip of floats.
    float *buffer = g_new (float, self->tile_size * self->size_x);

    // Reorganize data into tiles in tile oriented disk file.
    size_t ii = 0;
    for ( ii = 0 ; ii < self->tile_count_y ; ii++ ) {
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
      size_t read_count = -1;	// Initializer reassures compiler.
      switch ( sample_type ) {
      case FLOAT_IMAGE_SAMPLE_TYPE_SIGNED_TWO_BYTE_INTEGER:
	read_count = fread (input_buffer, sizeof (int16_t), strip_area, fp);
	break;
      default:
	g_assert_not_reached ();
      }
      g_assert (read_count == strip_area);

      // Convert from the byte order on disk to the host byte order,
      // if necessary.
      if ( non_native_byte_order (byte_order) ) {
	switch ( sample_type ) {
	  case FLOAT_IMAGE_SAMPLE_TYPE_SIGNED_TWO_BYTE_INTEGER:
	    g_assert (sizeof (int16_t) == 2);
	    size_t idx;
	    for ( idx = 0 ; idx < strip_area ; idx++ ) {
	      swap_bytes_16
		((unsigned char *) &(((int16_t *) input_buffer)[idx]));
	    }
	    break;
	default:
	  g_assert_not_reached ();
	}
      }

      // Convert from the input sample type to floating point.
      size_t jj;
      for ( jj = 0 ; jj < strip_area ; jj++ ) {
	switch ( sample_type ) {
	case FLOAT_IMAGE_SAMPLE_TYPE_SIGNED_TWO_BYTE_INTEGER:
	  buffer[jj] = ((int16_t *) input_buffer)[jj];
	  break;
	default:
	  g_assert_not_reached ();
	}
      }

      // Write data from the strip into the tile store.
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
	size_t write_count;	// For return of fwrite() calls.
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
		     "Error creating tile cache file for float_image "
		     "instance: %s\n", strerror (errno));
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
		       "Error creating tile cache file for float_image "
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
		     "Error creating tile cache file for float_image "
		     "instance: %s\n", strerror (errno));
	    // and exit.
	    exit (EXIT_FAILURE);
	  }
	}
      }
    }

    // Did we write the correct total amount of data?
    g_assert (ftello (self->tile_file)
	      == (off_t) self->tile_area * self->tile_count * sizeof (float));

    // Free temporary buffers.
    g_free (buffer);
    g_free (input_buffer);
    g_free (zero_line);
  }

  // Everything fits in the cache (at the moment this means everything
  // fits in the first tile, which is a bit of a FIXME), so just put
  // it there.
  else {
    self->tile_addresses[0] = self->cache;
    switch ( sample_type ) {
    case FLOAT_IMAGE_SAMPLE_TYPE_SIGNED_TWO_BYTE_INTEGER:
      {
	// Buffer capable of holding a row of samples in the input
	// sample format.
	int16_t *input_row_16 = g_new (int16_t, self->size_x);

	size_t ii;
	for ( ii = 0 ; ii < self->size_y ; ii++ ) {
	  // Read the data.
	  size_t read_count = fread (input_row_16, sizeof (int16_t),
				     self->size_x, fp);
	  g_assert (read_count == self->size_x);
	  // Convert from the byte order on disk to the host byte
	  // order, if necessary.  Doing this with floats is somewhat
	  // questionable: major libraries don't seem to support it
	  // with their macros, and the perl docs say it can't be done
	  // in a truly portable way... but it seems to work.
	  if ( non_native_byte_order (byte_order) ) {
	    g_assert (sizeof (int16_t) == 2);
	    size_t jj;
	    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
	      swap_bytes_16 ((unsigned char *) &(input_row_16[jj]));
	    }
	  }

	  // Address where the current row of pixels should end up.
	  float *row_address = self->tile_addresses[0] + ii * self->tile_size;

	  // Convert to output float format as advertised.
	  size_t jj;
	  for ( jj = 0 ; jj < self->size_x ; jj++ ) {
	    row_address[jj] = input_row_16[jj];
	  }
	}

	g_free (input_row_16);
      }
      break;
    default:
      g_assert_not_reached ();
    }
  }

  return self;
}

FloatImage *
float_image_new_from_file_with_sample_type
  (ssize_t size_x, ssize_t size_y, const char *file, off_t offset,
   float_image_byte_order_t byte_order, float_image_sample_type sample_type)
{
  g_assert (size_x > 0 && size_y > 0);

  size_t sample_size = 0;	// Initializer reassures compiler.
  switch ( sample_type ) {
  case FLOAT_IMAGE_SAMPLE_TYPE_SIGNED_TWO_BYTE_INTEGER:
    sample_size = sizeof (int16_t);
    break;
  default:
    g_assert_not_reached ();
  }

  // Check in advance if the source file looks big enough (we will
  // still need to check return codes as we read() data, of course).
  g_assert (is_large_enough (file, offset + ((off_t) size_x * size_y
					     * sample_size)));

  // Open the file to read data from.
  FILE *fp = fopen (file, "r");
  // FIXME: we need some error handling and propagation here.
  g_assert (fp != NULL);

  FloatImage *self = float_image_new_from_file_pointer_with_sample_type
    (size_x, size_y, fp, offset, byte_order,
     FLOAT_IMAGE_SAMPLE_TYPE_SIGNED_TWO_BYTE_INTEGER);

  // Close file we read image from.
  int return_code = fclose (fp);
  g_assert (return_code == 0);

  return self;
}

// Flush the contents of tile with flattened offset tile_offset from
// the memory cache to the disk file.  Its probably easiest to
// understand this function by looking at how its used.
static void
cached_tile_to_disk (FloatImage *self, size_t tile_offset)
{
  int return_code
    = fseeko (self->tile_file,
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
    = fseeko (self->tile_file,
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
               ftello (self->tile_file));
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
  g_assert (x >= 0 && (size_t) x < self->size_x);
  g_assert (y >= 0 && (size_t) y < self->size_y);

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
      if ( !ISNAN(mask) && (0==gsl_fcmp(cs, mask, 0.00000000001)) )
        continue;
      if ( G_UNLIKELY (cs < *min) ) { *min = cs; }
      if ( G_UNLIKELY (cs > *max) ) { *max = cs; }
      double old_mean = mean_as_double;
      mean_as_double += (cs - mean_as_double) / (sample_count + 1);
      s += (cs - old_mean) * (cs - mean_as_double);
      sample_count++;
    }
  }

  g_free (row_buffer);

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


gsl_histogram *
float_image_gsl_histogram (FloatImage *self, float min, float max,
                           size_t num_bins)
{
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
        y_spline_values[ii] = gsl_spline_eval (xss[ii], x, xias[ii]);
      }
      gsl_spline_init (ys, y_spline_indicies, y_spline_values, ss);

      return (float) gsl_spline_eval (ys, y, yia);
    }
    break;
  default:
    g_assert_not_reached ();
    return -42;         // Reassure the compiler.
  }
}

int
float_image_store (FloatImage *self, const char *file,
                   float_image_byte_order_t byte_order)
{
  // Open the file to write to.
  FILE *fp = fopen (file, "w");
  // FIXME: we need some error handling and propagation here.
  g_assert (fp != NULL);

  // We will write the image data in horizontal stips one line at a
  // time.
  float *line_buffer = g_new (float, self->size_x);

  // Reorganize data into tiles in tile oriented disk file.
  size_t ii;
  for ( ii = 0 ; ii < self->size_y ; ii++ ) {
    float_image_get_row (self, ii, line_buffer);
    // Convert from the host byte order to the byte order specified
    // for the disk file, if necessary.  Doing this with floats is
    // somewhat questionable apparently: major libraries don't seem to
    // support it with their macros, and the perl documentation says
    // it can't be done in a truly portable way... but it seems to
    // work.
    if ( non_native_byte_order (byte_order) ) {
      // Floats better be four bytes for this to work.
      g_assert (sizeof (float) == 4);
      size_t jj;
      for ( jj = 0 ; jj < self->size_x ; jj++ ) {
        swap_bytes_32 ((unsigned char *) &(line_buffer[jj]));
      }
    }
    // Write the data.
    size_t write_count = fwrite (line_buffer, sizeof (float), self->size_x,
                                 fp);
    // If we wrote less than expected,
    if ( write_count < self->size_x ) {
      // it must have been a write error (probably no space left),
      g_assert (ferror (self->tile_file));
      // so print an error message,
      fprintf (stderr, "Error writing file %s: %s\n", file, strerror (errno));
      // and exit.
      exit (EXIT_FAILURE);
    }

    g_assert (write_count == self->size_x);
  }

  // Done with the line buffer.
  g_free (line_buffer);

  // Close file being written.
  int return_code = fclose (fp);
  g_assert (return_code == 0);

  return 0;
}

int
float_image_export_as_jpeg (FloatImage *self, const char *file,
                            size_t max_dimension)
{
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
  FILE *fp = fopen (file, "w");
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
  float_image_statistics (self, &min, &max, &mean, &standard_deviation, NAN);

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
  float kernel_value = 1.0 / pow (kernel_size, 2.0);
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

  g_free (self);
}
