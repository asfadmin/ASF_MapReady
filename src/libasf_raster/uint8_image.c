// Implementation of the interface in uint8_image.h.

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
#include <assert.h>

#include <glib.h>
#if GLIB_CHECK_VERSION (2, 6, 0)
#  include <glib/gstdio.h>
#endif
#include <gsl/gsl_spline.h>
#include <gsl/gsl_histogram.h>
#include <gsl/gsl_math.h>

#include <jpeglib.h>
#include "uint8_image.h"
#include "asf.h"

#ifndef linux
#ifndef win32
static double
round (double arg)
{
  return floor (arg + 0.5);
}
#endif // #ifndef win32
#endif // #ifndef linux

#include "asf_glib.h"

// Default cache size to use is 16 megabytes.
static const size_t default_cache_size = 16 * 1048576;
// This class wide data element keeps track of the number of temporary
// tile files opened by the current process, in order to give them
// unique names.
static unsigned long current_tile_file_number = 0;
// We need to ensure that multiple threads trying to create their own
// images concurently don't end up with the same temporary file
// names.
G_LOCK_DEFINE_STATIC (current_tile_file_number);

// We don't want to let multiple threads twiddle the signal block mask
// concurrently, or we might end up with the wrong set of signals
// blocked.  This lock is used to guarantee this can't happen (see the
// usage for a better explanation).
G_LOCK_DEFINE_STATIC (signal_block_activity);

// Return a FILE pointer refering to a new, already unlinked file in a
// location which hopefully has enough free space to serve as a block
// cache.
static FILE *
initialize_tile_cache_file (void)
{
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
                          "%s/.uint8_image_tile_file_uNiQuIfY_nAmE_%ld_%lu",
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
  // Theoretically, two parallel instantiations of image could end up
  // in a race condition which would result in all signals ending up
  // blocked after both were done with this section, so we consider
  // this section critical and protect it with a lock.
  G_LOCK (signal_block_activity);
  sigset_t all_signals, old_set;
  int return_code = sigfillset (&all_signals);
  g_assert (return_code == 0);
  return_code = sigprocmask (SIG_SETMASK, &all_signals, &old_set);
  // FIXME?: It might be faster to use file descriptor based I/O
  // everywhere, or at least for the big transfers.  I'm not sure its
  // worth the trouble though.
  FILE *tile_file = fopen (tile_file_name->str, "w+");
  if ( tile_file == NULL ) {
    if ( errno != EACCES ) {
      g_warning ("couldn't create file in current directory, and it wasn't"
                 "just a permissions problem");
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
    return_code = unlink (tile_file_name->str);
    g_assert (return_code == 0);
  }
  g_assert (tile_file != NULL);
  return_code = sigprocmask (SIG_SETMASK, &old_set, NULL);
  G_UNLOCK (signal_block_activity);
  g_string_free (tile_file_name, TRUE);

  return tile_file;
}

// This routine does the work common to several of the differenct
// creation routines.  Basically, it does everything but fill in the
// contents of the disk tile store.
static UInt8Image *
initialize_uint8_image_structure (ssize_t size_x, ssize_t size_y)
{
  // Allocate instance memory.
  UInt8Image *self = g_new0 (UInt8Image, 1);

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
  if ( largest_dimension * largest_dimension * sizeof (uint8_t)
       <= default_cache_size ) {
    self->cache_space = (largest_dimension * largest_dimension
			 * sizeof (uint8_t));
    self->cache_area = self->cache_space / sizeof (uint8_t);
    self->tile_size = largest_dimension;
    self->cache_size_in_tiles = 1;
    self->tile_count_x = 1;
    self->tile_count_y = 1;
    self->tile_count = 1;
    self->tile_area = self->tile_size * self->tile_size;
    self->cache = g_new (uint8_t, self->cache_area);
    self->tile_addresses = g_new0 (uint8_t *, self->tile_count);
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
  g_assert (self->cache_space % sizeof (uint8_t) == 0);
  self->cache_area = self->cache_space / sizeof (uint8_t);

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
  self->cache = g_new (uint8_t, self->cache_area);
  // Do we want to do mlock() here maybe?

  // The addresses in the cache of the starts of each of the tiles.
  // This array contains flattened tile addresses in the same way that
  // image memory normally uses flattened pixel addresses, e.g. the
  // address of tile x = 2, y = 4 is stored at self->tile_addresses[4
  // * self->tile_count_x + 2].  If a tile isn't in the cache, the
  // address is NULL (meaning it will have to be loaded).
  self->tile_addresses = g_new0 (uint8_t *, self->tile_count);
  g_assert (NULL == 0x0);       // Ensure g_new0 effectively sets to NULL.

  // Create a queue in order to keep track of which tile was loaded
  // longest ago.
  self->tile_queue = g_queue_new ();

  // Get a new empty tile cache file pointer.
  self->tile_file = initialize_tile_cache_file ();

  return self;
}

UInt8Image *
uint8_image_thaw (FILE *file_pointer)
{
  FILE *fp = file_pointer;	// Convenience alias.

  g_assert (file_pointer != NULL);

  UInt8Image *self = g_new (UInt8Image, 1);

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
  self->cache = g_new (uint8_t, self->cache_area);

  self->tile_addresses = g_new0 (uint8_t *, self->tile_count);

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
    // the end of the uint8_image_new method).
    self->tile_addresses[0] = self->cache;
    read_count = fread (self->tile_addresses[0], sizeof (uint8_t),
			self->tile_area, fp);
    g_assert (read_count == self->tile_area);
  }
  // otherwise, an empty tile queue needs to be initialized, and the
  // remainder of the serialized version is the tile block cache.
  else {
    self->tile_queue = g_queue_new ();
    self->tile_file = initialize_tile_cache_file ();
    uint8_t *buffer = g_new (uint8_t, self->tile_area);
    size_t ii;
    for ( ii = 0 ; ii < self->tile_count ; ii++ ) {
      read_count = fread (buffer, sizeof (uint8_t), self->tile_area, fp);
      g_assert (read_count == self->tile_area);
      size_t write_count = fwrite (buffer, sizeof (uint8_t), self->tile_area,
				   self->tile_file);
      if ( write_count < self->tile_area ) {
	if ( feof (self->tile_file) ) {
	  fprintf (stderr,
		   "Premature end of file while trying to thaw UInt8Image "
		   "instance\n");
	}
	else {
	  g_assert (ferror (self->tile_file));
	  fprintf (stderr,
		   "Error writing tile cache file for UInt8Image instance "
		   "during thaw: %s\n", strerror (errno));
	}
	exit (EXIT_FAILURE);
      }
      g_assert (write_count == self->tile_area);
    }
    g_free (buffer);
  }

  return self;
}



UInt8Image *
uint8_image_new (ssize_t size_x, ssize_t size_y)
{
  g_assert (size_x > 0 && size_y > 0);

  UInt8Image *self = initialize_uint8_image_structure (size_x, size_y);

  // If we need a tile file for an image of this size, prepare it.
  if ( self->tile_file != NULL ) {

    // The total width or height of all the tiles is probably greater
    // than the width or height of the image itself.
    size_t total_width = self->tile_count_x * self->tile_size;
    size_t total_height = self->tile_count_y * self->tile_size;

    // Fill the file full of zeros.  FIXME: there is almost certainly
    // a faster way to ensure that we have the disk space we need.
    uint8_t *zero_line = g_new0 (uint8_t, total_width);
    g_assert (0 == 0x0);      // Ensure the g_new0 did what we think.

    // We don't have to write in tile order because its all zeros anyway.
    size_t ii;
    for ( ii = 0 ; ii < total_height ; ii++ ) {
      size_t write_count = fwrite (zero_line, sizeof (uint8_t), total_width,
                                   self->tile_file);
      // If we wrote less than expected,
      if ( write_count < total_width ) {
        // it must have been a write error (probably no space left),
        g_assert (ferror (self->tile_file));
        // so print an error message,
        fprintf (stderr,
                 "Error writing tile cache file for UInt8Image instance: "
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
        self->tile_addresses[0][ii * self->tile_size + jj] = 0;
      }
    }
  }

  return self;
}

UInt8Image *
uint_image_new_with_value (ssize_t size_x, ssize_t size_y, uint8_t value)
{
  // Carefully clone-and-modified over from float_image.c, but not
  // tested yet.
  g_assert_not_reached ();

  g_assert (size_x > 0 && size_y > 0);

  UInt8Image *self = initialize_uint8_image_structure (size_x, size_y);

  // If we need a tile file for an image of this size, prepare it.
  if ( self->tile_file != NULL ) {

    // The total width or height of all the tiles is probably greater
    // than the width or height of the image itself.
    size_t total_width = self->tile_count_x * self->tile_size;
    size_t total_height = self->tile_count_y * self->tile_size;

    // Fill the file full of the given value.
    uint8_t *value_line = g_new (uint8_t, total_width);
    size_t ii;
    for ( ii = 0 ; ii < total_width ; ii++ ) {
      value_line[ii] = value;
    }
    // We don't have to write in tile order because the values are all
    // the same anyway.
    for ( ii = 0 ; ii < total_height ; ii++ ) {
      size_t write_count = fwrite (value_line, sizeof (uint8_t), total_width,
                                   self->tile_file);
      // If we wrote less than expected,
      if ( write_count < total_width ) {
        // it must have been a write error (probably no space left),
        g_assert (ferror (self->tile_file));
        // so print an error message,
        fprintf (stderr,
                 "Error writing tile cache file for UInt8Image instance: "
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

UInt8Image *
uint8_image_new_from_memory (ssize_t size_x, ssize_t size_y, uint8_t *buffer)
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

UInt8Image *
uint8_image_copy (UInt8Image *model)
{
  // This method should be totally fine, it is extremely trivial clone
  // and modify from the corresponding method in float_image.c, but
  // since its untested I disable it for the moment.
  g_assert_not_reached ();

  // FIXME: this could obviously be optimized a lot by copying the
  // existed tile file, etc.
  UInt8Image *self = uint8_image_new (model->size_x, model->size_y);

  size_t ii, jj;
  for ( ii = 0 ; ii < self->size_y ; ii++ ) {
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      uint8_image_set_pixel (self, jj, ii,
			     uint8_image_get_pixel (model, jj, ii));
    }
  }

  return self;
}

UInt8Image *
uint8_image_new_from_model_scaled (UInt8Image *model, ssize_t scale_factor)
{
  g_assert (model->size_x > 0 && model->size_y > 0);

  g_assert (scale_factor > 0);
  g_assert (scale_factor % 2 == 1);

  UInt8Image *self
    = uint8_image_new (round ((double) model->size_x / scale_factor),
		       round ((double) model->size_y / scale_factor));

  // This method hasn't yet made the jump from FloatImage to here.
  // The implementation should ultimately follow the one in FloatImage
  // to achieve the interface description given in uint8_image.h.
  g_assert_not_reached ();

  return self;
}

UInt8Image *
uint8_image_new_subimage (UInt8Image *model, ssize_t x, ssize_t y,
			  ssize_t size_x, ssize_t size_y)
{
  // Upper left corner must be in model.
  g_assert (x >= 0 && y >= 0);

  // Size of image to be created must be strictly positive.
  g_assert (size_x >= 1 && size_y >= 1);

  // Given model must be big enough to allow a subimage of the
  // requested size to fit.
  g_assert (model->size_x <= SSIZE_MAX && model->size_y <= SSIZE_MAX);
  g_assert (x + size_x <= (ssize_t) model->size_x);
  g_assert (y + size_y <= (ssize_t) model->size_y);

  UInt8Image *self = uint8_image_new (size_x, size_y);

  // Copy the image pixels from the model.
  ssize_t ii, jj;
  for ( ii = 0 ; ii < (ssize_t) self->size_x ; ii++ ) {
    for ( jj = 0 ; jj < (ssize_t) self->size_y ; jj++ ) {
      uint8_t pv = uint8_image_get_pixel (model, x + ii, y + jj);
      uint8_image_set_pixel (self, ii, jj, pv);
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

UInt8Image *
uint8_image_new_from_file (ssize_t size_x, ssize_t size_y, const char *file,
                           off_t offset)
{
  // Carefully clone-and-modified over from float_image.c, but not
  // tested yet.
  g_assert_not_reached ();

  g_assert (size_x > 0 && size_y > 0);

  // Check in advance if the source file looks big enough (we will
  // still need to check return codes as we read() data, of course).
  g_assert (is_large_enough (file, offset + ((off_t) size_x * size_y
					     * sizeof (uint8_t))));

  // Open the file to read data from.
  FILE *fp = fopen (file, "r");
  // FIXME: we need some error handling and propagation here.
  g_assert (fp != NULL);

  UInt8Image *self = uint8_image_new_from_file_pointer (size_x, size_y, fp,
                                                        offset);

  // Close file we read image from.
  int return_code = fclose (fp);
  g_assert (return_code == 0);

  return self;
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


UInt8Image *
uint8_image_new_from_file_pointer (ssize_t size_x, ssize_t size_y,
                                   FILE *file_pointer, off_t offset)
{
  // Carefully clone-and-modified over from float_image.c, but not
  // tested yet.
  g_assert_not_reached ();

  g_assert (size_x > 0 && size_y > 0);

  // Check in advance if the source file looks big enough (we will
  // still need to check return codes as we read() data, of course).
  g_assert (file_pointed_to_larger_than (file_pointer,
					 offset + ((off_t) size_x * size_y
						   * sizeof (uint8_t))));

  UInt8Image *self = initialize_uint8_image_structure (size_x, size_y);

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
    uint8_t *zero_line = g_new0 (uint8_t, (size_x > (ssize_t) self->tile_size ?
					   (size_t) size_x : self->tile_size));
    g_assert (0 == 0x0);

    // Buffer capable of holding a full strip.
    uint8_t *buffer = g_new (uint8_t, self->tile_size * self->size_x);

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
      size_t read_count = fread (buffer, sizeof (uint8_t), strip_area, fp);
      g_assert (read_count == strip_area);

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
                      sizeof (uint8_t), effective_width, self->tile_file);
          // If we wrote less than expected,
          if ( write_count < effective_width ) {
            // it must have been a write error (probably no space left),
            g_assert (ferror (self->tile_file));
            // so print an error message,
            fprintf (stderr,
                     "Error writing tile cache file for UInt8Image "
                     "instance: %s\n", strerror (errno));
            // and exit.
            exit (EXIT_FAILURE);
          }
          if ( effective_width < self->tile_size ) {
            // Amount we have left to write to fill out the last tile.
            size_t edge_width = self->tile_size - effective_width;
            write_count = fwrite (zero_line, sizeof (uint8_t), edge_width,
                                  self->tile_file);
            // If we wrote less than expected,
            if ( write_count < edge_width ) {
              // it must have been a write error (probably no space left),
              g_assert (ferror (self->tile_file));
              // so print an error message,
              fprintf (stderr,
                       "Error writing tile cache file for UInt8Image "
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
          write_count = fwrite (zero_line, sizeof (uint8_t), self->tile_size,
                                self->tile_file);
          // If we wrote less than expected,
          if ( write_count < self->tile_size ) {
            // it must have been a write error (probably no space left),
            g_assert (ferror (self->tile_file));
            // so print an error message,
            fprintf (stderr,
                     "Error writing tile cache file for UInt8Image "
                     "instance: %s\n", strerror (errno));
            // and exit.
            exit (EXIT_FAILURE);
          }
        }
      }
    }

    // Did we write the correct total amount of data?
    g_assert (ftello (self->tile_file)
	      == ((off_t) self->tile_area * self->tile_count
		  * sizeof (uint8_t)));

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
      uint8_t *row_address = self->tile_addresses[0] + ii * self->tile_size;

      // Read the data.
      size_t read_count = fread (row_address, sizeof (uint8_t), self->size_x,
				 fp);
      g_assert (read_count == self->size_x);
    }
  }

  return self;
}

UInt8Image *
uint8_image_new_from_file_scaled (ssize_t size_x, ssize_t size_y,
                                  ssize_t original_size_x,
                                  ssize_t original_size_y,
				  const char *file, off_t offset)
{
  // This method has been carefully translated from the corresponding
  // method in float_image.c, but it hasn't been tested yet.
  g_assert_not_reached ();

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
				       * sizeof (uint8_t))));

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
  uint8_t *uls = g_new (uint8_t, size_x);
  uint8_t *urs = g_new (uint8_t, size_x);
  uint8_t *lls = g_new (uint8_t, size_x);
  uint8_t *lrs = g_new (uint8_t, size_x);

  // Results of rounded bilinear interpolation for the current row.
  uint8_t *interpolated_values = g_new (uint8_t, size_x);

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
	= offset + sizeof (uint8_t) * ((off_t) in_ul_y * original_size_x
				       + in_ul_x);
      return_code = fseeko (fp, sample_offset, SEEK_SET);
      g_assert (return_code == 0);
      read_count = fread (&(uls[jj]), sizeof (uint8_t), 1, fp);
      g_assert (read_count == 1);
      // If the upper left pixel was the last pixel in the input image,
      if ( in_ul_x == original_size_x - 1 ) {
        // just treat it as the upper right as well,
        urs[jj] = uls[jj];
      }
      // otherwise read the next pixel as the upper right pixel.
      else {
        read_count = fread (&(urs[jj]), sizeof (uint8_t), 1, fp);
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
	= offset + sizeof (uint8_t) * ((off_t) in_ll_y * original_size_x
				       + in_ll_x);
      return_code = fseeko (fp, sample_offset, SEEK_SET);
      g_assert (return_code == 0);
      read_count = fread (&(lls[jj]), sizeof (uint8_t), 1, fp);
      g_assert (read_count == 1);
      // If the lower left pixel was the last pixel in the input image,
      if ( in_ll_x == original_size_x - 1 ) {
        // just treat it as the lower right as well,
        lrs[jj] = lls[jj];
      }
      // otherwise read the next pixel as the lower right pixel.
      else {
        read_count = fread (&(lrs[jj]), sizeof (uint8_t), 1, fp);
        g_assert (read_count == 1);
      }
    }

    // Perform the interpolation.
    for ( jj = 0 ; jj < size_x ; jj++ ) {
      double delta_x = stride_x * jj - floor (stride_x * jj);
      double delta_y = -(stride_y * ii - floor (stride_y * ii));
      float interpolated_value = bilinear_interpolate (delta_x, delta_y,
						       uls[jj], urs[jj],
						       lls[jj], lrs[jj]);
      g_assert (interpolated_value >= 0);
      g_assert (interpolated_value <= UINT8_MAX);
      interpolated_values[jj] = round (interpolated_value);
    }
    size_t write_count = fwrite (interpolated_values, sizeof (uint8_t), size_x,
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

  // Slurp the scaled file back in as an instance.
  UInt8Image *self
    = uint8_image_new_from_file_pointer (size_x, size_y, reduced_image,
					 (off_t) 0);

  // Now that we have an instantiated version of the image we are done
  // with this temporary file.
  return_code = fclose (reduced_image);

  return self;
}

// Returns a new UInt8Image, for the image corresponding to the given metadata.
UInt8Image *
uint8_image_new_from_metadata(meta_parameters *meta, const char *file)
{
  return uint8_image_band_new_from_metadata(meta, 0, file);
}

// Returns a new UInt8Image, for the image band corresponding to the
// given metadata.
UInt8Image *
uint8_image_band_new_from_metadata(meta_parameters *meta,
				   int band, const char *file)
{
    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;

    FILE * fp = FOPEN(file, "rb");
    UInt8Image * bi = uint8_image_new(ns, nl);

    int i,j;
    unsigned char *buf = MALLOC(sizeof(unsigned char)*ns);
    for (i = 0; i < nl; ++i) {
        get_byte_line(fp, meta, i+band*nl, buf);
        for (j = 0; j < ns; ++j)
            uint8_image_set_pixel(bi, j, i, buf[j]);
    }

    free(buf);
    fclose(fp);

    return bi;
}

// Copy the contents of tile with flattened offset tile_offset from
// the memory cache to the disk file.  Its probably easiest to
// understand this function by looking at how its used.
static void
cached_tile_to_disk (UInt8Image *self, size_t tile_offset)
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
    = fseeko (self->tile_file,
	      (off_t) tile_offset * self->tile_area * sizeof (uint8_t),
	      SEEK_SET);
  g_assert (return_code == 0);
  size_t write_count = fwrite (self->tile_addresses[tile_offset],
			       sizeof (uint8_t), self->tile_area,
			       self->tile_file);
  g_assert (write_count == self->tile_area);
}

// Return true iff tile (x, y) is already loaded into the memory cache.
static gboolean
tile_is_loaded (UInt8Image *self, ssize_t x, ssize_t y)
{
  g_assert (    x >= 0 && (size_t) x < self->tile_count_x
             && y >= 0 && (size_t) y < self->tile_count_y );

  size_t tile_offset = self->tile_count_x * y + x;

  return self->tile_addresses[tile_offset] != NULL;
}

// Load (currently unloaded) tile (x, y) from disk cache into memory
// cache, possibly displacing the oldest tile already loaded, updating
// the load order queue, and returning the address of the tile loaded.
static uint8_t *
load_tile (UInt8Image *self, ssize_t x, ssize_t y)
{
  // Make sure we haven't screwed up somehow and not created a tile
  // file when in fact we should have.
  g_assert (self->tile_file != NULL);

  g_assert (!tile_is_loaded (self, x, y));

  // Address into which tile gets loaded (to be returned).
  uint8_t *tile_address;

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
              (off_t) tile_offset * self->tile_area * sizeof (uint8_t),
              SEEK_SET);
  g_assert (return_code == 0);
  clearerr (self->tile_file);
  size_t read_count = fread (tile_address, sizeof (uint8_t), self->tile_area,
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

uint8_t
uint8_image_get_pixel (UInt8Image *self, ssize_t x, ssize_t y)
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
  uint8_t *tile_address = self->tile_addresses[tile_offset];

  // Load the tile containing the pixel of interest if necessary.
  if ( G_UNLIKELY (tile_address == NULL) ) {
    tile_address = load_tile (self, pc_x.quot, pc_y.quot);
  }

  // Return pixel of interest.
  return tile_address[self->tile_size * pc_y.rem + pc_x.rem];
}

void
uint8_image_set_pixel (UInt8Image *self, ssize_t x, ssize_t y, uint8_t value)
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
  uint8_t *tile_address = self->tile_addresses[tile_offset];

  // Load the tile containing the pixel of interest if necessary.
  if ( G_UNLIKELY (tile_address == NULL) ) {
    tile_address = load_tile (self, pc_x.quot, pc_y.quot);
  }

  // Set pixel of interest.
  tile_address[self->tile_size * pc_y.rem + pc_x.rem] = value;
}

void
uint8_image_get_region (UInt8Image *self, ssize_t x, ssize_t y, ssize_t size_x,
                        ssize_t size_y, uint8_t *buffer)
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
      buffer[ii * size_x + jj] = uint8_image_get_pixel (self, ix, iy);
    }
  }
}

void
uint8_image_set_region (UInt8Image *self, size_t x, size_t y, size_t size_x,
                        size_t size_y, uint8_t *buffer)
{
  g_assert_not_reached ();      // Stubbed out for now.
  self = self; x = x; y = y; size_x = size_x, size_y = size_y; buffer = buffer;
}

void
uint8_image_get_row (UInt8Image *self, size_t row, uint8_t *buffer)
{
  uint8_image_get_region (self, 0, row, self->size_x, 1, buffer);
}

uint8_t
uint8_image_get_pixel_with_reflection (UInt8Image *self, ssize_t x, ssize_t y)
{
  // Carefully clone-and-modified over from float_image.c, but not
  // tested yet.
  g_assert_not_reached ();

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

  return uint8_image_get_pixel (self, x, y);
}


void
uint8_image_statistics (UInt8Image *self, uint8_t *min, uint8_t *max,
                        double *mean, double *standard_deviation,
			gboolean use_mask_value, uint8_t mask_value)
{
  // Carefully clone-and-modified over from float_image.c, but not
  // tested yet.
  //g_assert_not_reached ();

  // Minimum and maximum sample values as integers.
  int imin = INT_MAX, imax = INT_MIN;

  // Buffer for one row of samples.
  uint8_t *row_buffer = g_new (uint8_t, self->size_x);

  *mean = 0.0;
  double s = 0.0;

  size_t sample_count = 0;      // Samples considered so far.
  size_t ii, jj;
  // If there is a mask value we are supposed to ignore,
  if ( use_mask_value ) {
    // iterate over all pixels, skipping pixels equal to mask value.
    for ( ii = 0 ; ii < self->size_y ; ii++ ) {
      asfPercentMeter((double)ii/(double)(self->size_y));
      uint8_image_get_row (self, ii, row_buffer);
      for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      	uint8_t cs = row_buffer[jj];   // Current sample.
      	if ( cs == mask_value ) {
      	  continue;
      	}
      	if ( G_UNLIKELY (cs < imin) ) { imin = cs; }
      	if ( G_UNLIKELY (cs > imax) ) { imax = cs; }
      	double old_mean = *mean;
      	*mean += (cs - *mean) / (sample_count + 1);
      	s += (cs - old_mean) * (cs - *mean);
      	sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }
  else {
    // There is no mask value to ignore, so we do the same as the
    // above loop, but without the possible continue statement.
    for ( ii = 0 ; ii < self->size_y ; ii++ ) {
      asfPercentMeter((double)ii/(double)(self->size_y));
      uint8_image_get_row (self, ii, row_buffer);
      for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      	uint8_t cs = row_buffer[jj];   // Current sample.
      	if ( G_UNLIKELY (cs < imin) ) { imin = cs; }
      	if ( G_UNLIKELY (cs > imax) ) { imax = cs; }
      	double old_mean = *mean;
      	*mean += (cs - *mean) / (sample_count + 1);
      	s += (cs - old_mean) * (cs - *mean);
      	sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }

  g_free (row_buffer);

  // Verify the new extrema have been found.
  g_assert (imin != INT_MAX);
  g_assert (imax != INT_MIN);

  // The new extrema had better be in the range supported by uint8_t.
  g_assert (imin >= 0);
  g_assert (imax <= UINT8_MAX);

  *min = imin;
  *max = imax;
  *standard_deviation = sqrt (s / (sample_count - 1));
}

int
uint8_image_band_statistics (UInt8Image *self, meta_stats *stats,
                             int line_count, int band_no,
                             gboolean use_mask_value, uint8_t mask_value)
{
  // Carefully clone-and-modified over from float_image.c, but not
  // tested yet.
  //g_assert_not_reached ();

  // Minimum and maximum sample values as integers.
  int imin = INT_MAX, imax = INT_MIN;

  // Buffer for one row of samples.
  uint8_t *row_buffer = g_new (uint8_t, self->size_x);

  stats->mean = 0.0;
  double s = 0.0;

  size_t sample_count = 0;      // Samples considered so far.
  size_t ii, jj;
  // If there is a mask value we are supposed to ignore,
  if ( use_mask_value ) {
    // iterate over all pixels, skipping pixels equal to mask value.
    for ( ii = (band_no * line_count); // 0-ordered band number times lines is offset into image
          ii < (band_no+1) * line_count && ii < self->size_y;
          ii++ )
    {
      asfPercentMeter( (double)(ii - band_no*line_count)/(double)line_count );
      uint8_image_get_row (self, ii, row_buffer);
      for ( jj = 0 ; jj < self->size_x ; jj++ ) {
        uint8_t cs = row_buffer[jj];   // Current sample.
        if ( cs == mask_value ) {
          continue;
        }
        if ( G_UNLIKELY (cs < imin) ) { imin = cs; }
        if ( G_UNLIKELY (cs > imax) ) { imax = cs; }
        double old_mean = stats->mean;
        stats->mean += (cs - stats->mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - stats->mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }
  else {
    // There is no mask value to ignore, so we do the same as the
    // above loop, but without the possible continue statement.
    for ( ii = (band_no * line_count); // 0-ordered band number times lines is offset into image
          ii < (band_no+1) * line_count && ii < self->size_y;
          ii++ )
    {
      asfPercentMeter( (double)(ii - band_no*line_count)/(double)line_count );
      uint8_image_get_row (self, ii, row_buffer);
      for ( jj = 0 ; jj < self->size_x ; jj++ ) {
        uint8_t cs = row_buffer[jj];   // Current sample.
        if ( G_UNLIKELY (cs < imin) ) { imin = cs; }
        if ( G_UNLIKELY (cs > imax) ) { imax = cs; }
        double old_mean = stats->mean;
        stats->mean += (cs - stats->mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - stats->mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }

  g_free (row_buffer);

  // Verify the new extrema have been found.
  if (imin == INT_MAX || imax == INT_MIN)
    return 1;

  // The new extrema had better be in the range supported by uint8_t.
  if (imin < 0 || imax > UINT8_MAX)
    return 1;

  stats->min = imin;
  stats->max = imax;
  stats->std_deviation = sqrt (s / (sample_count - 1));

  return 0;
}

void
uint8_image_statistics_with_mask_interval (UInt8Image *self, uint8_t *min,
					   uint8_t *max, double *mean,
					   double *standard_deviation,
					   uint8_t interval_start,
					   uint8_t interval_end)
{
  // This method is a trivial clone-and-modify of
  // float_image_statistics, but it is totally untested at the moment.
  g_assert_not_reached ();

  // Minimum and maximum sample values as integers.
  int imin = INT_MAX, imax = INT_MIN;

  // Buffer for one row of samples.
  uint8_t *row_buffer = g_new (uint8_t, self->size_x);

  *mean = 0.0;
  double s = 0.0;

  size_t sample_count = 0;      // Samples considered so far.
  size_t ii, jj;
  for ( ii = 0 ; ii < self->size_y ; ii++ ) {
    uint8_image_get_row (self, ii, row_buffer);
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      uint8_t cs = row_buffer[jj];   // Current sample.
      // If in the mask interval, do not consider this pixel any
      // further.
      if ( cs >= interval_start && cs <= interval_end ) {
	continue;
      }
      if ( G_UNLIKELY (cs < imin) ) { imin = cs; }
      if ( G_UNLIKELY (cs > imax) ) { imax = cs; }
      double old_mean = *mean;
      *mean += (cs - *mean) / (sample_count + 1);
      s += (cs - old_mean) * (cs - *mean);
      sample_count++;
    }
  }

  g_free (row_buffer);

  // Verify the new extrema have been found.
  g_assert (imin != INT_MAX);
  g_assert (imax != INT_MIN);

  // The new extrema had better be in the range supported by uint8_t.
  g_assert (imin >= 0);
  g_assert (imax <= UINT8_MAX);

  *min = imin;
  *max = imax;
  *standard_deviation = sqrt (s / (sample_count - 1));
}

void
uint8_image_approximate_statistics (UInt8Image *self, size_t stride,
                                    double *mean, double *standard_deviation,
				    gboolean use_mask_value,
				    uint8_t mask_value)
{
  // Rows and columns of samples that fit in image given stride
  // stride.
  size_t sample_columns = ceil (self->size_x / stride);
  size_t sample_rows = ceil (self->size_y / stride);
  // Total number of samples.
  size_t sample_count = sample_columns * sample_rows;

  // Create an image holding the sample values.
  UInt8Image *sample_image = uint8_image_new (sample_columns, sample_rows);

  // Load the sample values.
  size_t current_sample = 0;
  size_t ii;
  for ( ii = 0 ; ii < sample_columns ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; jj < sample_rows ; jj++ ) {
      uint8_t sample = uint8_image_get_pixel (self, ii * stride, jj * stride);
      uint8_image_set_pixel (sample_image, ii, jj, sample);
      current_sample++;
    }
  }

  // Ensure that we got the right number of samples in our image.
  g_assert (current_sample == sample_count);

  // Compute the exact statistics of the sampled version of the image.
  // The _statistics method wants to compute min and max, so we let
  // it, even though we don't do anything with them (since they are
  // inaccurate).
  uint8_t min, max;
  uint8_image_statistics (sample_image, &min, &max, mean, standard_deviation,
                          use_mask_value, mask_value);

  uint8_image_free (sample_image);
}

void
uint8_image_approximate_statistics_with_mask_interval
  (UInt8Image *self, size_t stride, double *mean, double *standard_deviation,
   uint8_t interval_start, uint8_t interval_end)
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
  UInt8Image *sample_image = uint8_image_new (sample_columns, sample_rows);

  // Load the sample values.
  size_t current_sample = 0;
  size_t ii;
  for ( ii = 0 ; ii < sample_columns ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; jj < sample_rows ; jj++ ) {
      uint8_t sample = uint8_image_get_pixel (self, ii * stride, jj * stride);
      uint8_image_set_pixel (sample_image, ii, jj, sample);
      current_sample++;
    }
  }

  // Ensure that we got the right number of samples in our image.
  g_assert (current_sample == sample_count);

  // Compute the exact statistics of the sampled version of the image.
  // The _statistics method wants to compute min and max, so we let
  // it, even though we don't do anything with them (since they are
  // inaccurate).
  uint8_t min, max;
  uint8_image_statistics_with_mask_interval (sample_image, &min, &max,
					     mean, standard_deviation,
					     interval_start, interval_end);

  uint8_image_free (sample_image);
}

gsl_histogram *
uint8_image_gsl_histogram (UInt8Image *self, double min, double max,
                           size_t bin_count)
{
  // Carefully clone-and-modified over from float_image.c, but not
  // tested yet.
  g_assert_not_reached ();

  // Initialize the histogram.
  gsl_histogram *histogram = gsl_histogram_alloc (bin_count);
  gsl_histogram_set_ranges_uniform (histogram, min, max);

  // Buffer for one row of samples.
  uint8_t *row_buffer = g_new (uint8_t, self->size_x);

  // Populate the histogram over every sample in the image.
  size_t ii, jj;
  for (ii = 0 ; ii < self->size_y ; ii++ ) {
    uint8_image_get_row (self, ii, row_buffer);
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      gsl_histogram_increment (histogram, row_buffer[jj]);
    }
  }

  g_free (row_buffer);

  return histogram;
}


double
uint8_image_apply_kernel (UInt8Image *self, ssize_t x, ssize_t y,
			  gsl_matrix *kern)
{
  // Carefully clone-and-modified over from float_image.c, but not
  // tested yet.
  g_assert_not_reached ();

  g_assert (x >= 0 && (size_t) x < self->size_x);
  g_assert (y >= 0 && (size_t) y < self->size_y);
  g_assert (kern->size2 % 2 == 1);
  g_assert (kern->size2 == kern->size1);

  size_t ks = kern->size2;    // Kernel size.

  double sum = 0;                // Result.

  size_t ii;
  for ( ii = 0 ; ii < kern->size1 ; ii++ ) {
    ssize_t iy = y - ks / 2 + ii; // Current image y pixel index.
    size_t jj;
    for ( jj = 0 ; jj < kern->size2 ; jj++ ) {
      ssize_t ix = x - ks / 2 + jj; // Current image x pixel index
      sum += (gsl_matrix_get (kern, jj, ii)
              * uint8_image_get_pixel_with_reflection (self, ix, iy));
    }
  }

  return sum;
}

double
uint8_image_sample (UInt8Image *self, double x, double y,
                    uint8_image_sample_method_t sample_method)
{
  g_assert (x >= 0.0 && x <= (double) self->size_x - 1.0);
  g_assert (y >= 0.0 && y <= (double) self->size_y - 1.0);

  switch ( sample_method ) {

  case UINT8_IMAGE_SAMPLE_METHOD_NEAREST_NEIGHBOR:
    return uint8_image_get_pixel (self, round (x), round (y));
    break;

  case UINT8_IMAGE_SAMPLE_METHOD_BILINEAR:
    {
      // Indicies of points we are interpolating between (x below, y
      // below, etc., where below is interpreted in the numerical
      // sense, not the image orientation sense.).
      size_t xb = floor (x), yb = floor (y), xa = ceil (x), ya = ceil (y);
      size_t ts = self->tile_size;   // Convenience alias.
      // Offset of xb, yb, etc. relative to tiles they lie in.
      size_t xbto = xb % ts, ybto = yb % ts, xato = xa % ts, yato = ya % ts;
      // Values of points we are interpolating between.
      uint8_t ul, ur, ll, lr;

      // If the points were are interpolating between don't span a
      // tile edge, we load them straight from tile memory to save
      // some time.
      if ( G_LIKELY (   xbto != ts - 1 && xato != 0
                     && ybto != ts - 1 && yato != 0) ) {
        // The tile indicies.
        size_t tx = xb / ts, ty = yb / ts;
        // Tile offset in flattened list of tile addresses.
        size_t tile_offset = ty * self->tile_count_x + tx;
        uint8_t *tile_address = self->tile_addresses[tile_offset];
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
        ul = uint8_image_get_pixel (self, floor (x), floor (y));
        ur = uint8_image_get_pixel (self, ceil (x), floor (y));
        ll = uint8_image_get_pixel (self, floor (x), ceil (y));
        lr = uint8_image_get_pixel (self, ceil (x), ceil (y));
      }

      // Upper and lower values interpolated in the x direction.
      double ux = ul + (ur - ul) * (x - floor (x));
      double lx = ll + (lr - ll) * (x - floor (x));

      return ux + (lx - ux) * (y - floor (y));
    }
    break;
  case UINT8_IMAGE_SAMPLE_METHOD_BICUBIC:
    {
      // This path is kind of tricky.  It has come from float_image.c
      // and it should probably be verified to work correctly
      // independent of the other paths.
      g_assert_not_reached ();

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
            = uint8_image_get_pixel_with_reflection (self, x_indicies[jj],
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

      return gsl_spline_eval (ys, y, yia);
    }
    break;
  default:
    g_assert_not_reached ();
    return -42;         // Reassure the compiler.
  }
}

gboolean
uint8_image_equals (UInt8Image *self, UInt8Image *other)
{
  // Compare image sizes.
  if ( self->size_x != other->size_x ) {
    return FALSE;
  }
  if ( self->size_y != other->size_y ) {
    return FALSE;
  }

  size_t sz = self->size_x;	// Convenience alias.

  // Compare image pixels.
  size_t ii, jj;
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      if ( G_UNLIKELY (uint8_image_get_pixel (self, jj, ii)
		       != uint8_image_get_pixel (other, jj, ii)) ) {
	return FALSE;
      }
    }
  }

  return TRUE;
}

// Bring the tile cache file on the disk fully into sync with the
// latest image data stored in the memory cache.
static void
synchronize_tile_file_with_memory_cache (UInt8Image *self)
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
uint8_image_freeze (UInt8Image *self, FILE *file_pointer)
{
  FILE *fp = file_pointer;	// Convenience alias.

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
    write_count = fwrite (self->tile_addresses[0], sizeof (uint8_t),
			  self->tile_area, fp);
    if ( write_count < self->tile_area ) {
      if ( ferror (fp) ) {
	fprintf (stderr, "Error writing serialized UInt8Image instance during "
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
    uint8_t *buffer = g_new (uint8_t, self->tile_area);
    size_t ii;
    off_t tmp = ftello (self->tile_file);
    int return_code = fseeko (self->tile_file, 0, SEEK_SET);
    g_assert (return_code == 0);
    for ( ii = 0 ; ii < self->tile_count ; ii++ ) {
      size_t read_count = fread (buffer, sizeof (uint8_t), self->tile_area,
				 self->tile_file);
      g_assert (read_count == self->tile_area);
      write_count = fwrite (buffer, sizeof (uint8_t), self->tile_area, fp);
      g_assert (write_count == self->tile_area);
    }
    return_code = fseeko (self->tile_file, tmp, SEEK_SET);
    g_assert (return_code == 0);
    g_free (buffer);
  }
}

int
uint8_image_band_store(UInt8Image *self, const char *file,
		       meta_parameters *meta, int append_flag)
{
  // Give status
  if (meta->general->band_count == 1)
    asfPrintStatus("Storing image ...\n");
  else
    asfPrintStatus("Storing band ...\n");

  // Open the file to write to.
  FILE *fp = fopen (file, append_flag ? "a" : "w");
  // FIXME: we need some error handling and propagation here.
  g_assert (fp != NULL);

  // We will write the image data in horizontal stips one line at a time.
  uint8_t *line_buffer = g_new (uint8_t, self->size_x);

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
    uint8_image_get_row (self, ii, line_buffer);

    size_t write_count =
      fwrite(line_buffer, sizeof(uint8_t), self->size_x, fp);

    if (write_count < self->size_x) {
      // it must have been a write error (no space left, possibly)
      g_assert(ferror(self->tile_file));
      // so print an error message
      fprintf(stderr, "Error writing file %s: %s\n", file, strerror(errno));
      // and exit
      exit (EXIT_FAILURE);
    }

    g_assert(write_count == self->size_x);
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
uint8_image_store (UInt8Image *self, const char *file)
{
  meta_parameters *meta;
  meta = meta_read(file);

  int ret = uint8_image_band_store(self, file, meta, 0);
  meta_free(meta);

  return ret;
}

int
uint8_image_export_as_jpeg (UInt8Image *self, const char *file,
                            size_t max_dimension, gboolean use_mask_value,
			    uint8_t mask_value)
{
  // Carefully clone-and-modified over from float_image.c, but not
  // tested yet.
  g_assert_not_reached ();

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

  // As advertised, we will average pixels together.
  g_assert (scale_factor % 2 != 0);
  size_t kernel_size = scale_factor;
  gsl_matrix *averaging_kernel
    = gsl_matrix_alloc (kernel_size, kernel_size);
  double kernel_value = 1.0 / pow (kernel_size, 2.0);
  size_t ii, jj;                // Index values.
  for ( ii = 0 ; ii < averaging_kernel->size1 ; ii++ ) {
    for ( jj = 0 ; jj < averaging_kernel->size2 ; jj++ ) {
      gsl_matrix_set (averaging_kernel, ii, jj, kernel_value);
    }
  }

  // Sample input image, putting scaled results into output image.
  size_t sample_stride = scale_factor;
  for ( ii = 0 ; ii < osy ; ii++ ) {
    for ( jj = 0 ; jj < osx ; jj++ ) {
      // Input image average pixel value.
      double ival = uint8_image_apply_kernel (self, jj * sample_stride,
					      ii * sample_stride,
					      averaging_kernel);
      // Set output value.
      int32_t oval = round (ival);
      // In case floating point arithmetic wierdness gets us in
      // trouble, we correct.
      if ( oval < 0 ) {
	oval = 0;
      }
      else if ( oval > UINT8_MAX ) {
	oval = UINT8_MAX;
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
uint8_image_get_cache_size (UInt8Image *self)
{
  g_assert_not_reached ();      // Stubbed out for now.
  // Compiler reassurance.
  self = self;
  return 0;
}

void
uint8_image_set_cache_size (UInt8Image *self, size_t size)
{
  g_assert_not_reached ();      // Stubbed out for now.
  // Compiler reassurance.
  self = self; size = size;
}

void
uint8_image_free (UInt8Image *self)
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
