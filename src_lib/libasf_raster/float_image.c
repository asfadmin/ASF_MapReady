// Implementation of the interface in float_image.h.

#include <errno.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include <glib.h>

#include "float_image.h"

// Default cache size to use is 16 megabytes.
static const size_t default_cache_size = 16 * 1048576;
// This class wide data element keeps track of the number of temporary
// tile files opened by the current process, in order to give them
// unique names.
static unsigned long current_tile_file_number = 0;

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

  // Memory cache space, in bytes.
  self->cache_space = default_cache_size;

  // Memory cache space, in pixels.
  g_assert (self->cache_space % sizeof (float) == 0);
  self->cache_area = self->cache_space / sizeof (float);

  // Greater of size_x and size_y.
  size_t largest_dimension = (size_x > size_y ? size_x : size_y);

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
	   * ceil ((double)largest_dimension / self->tile_size))
	  > self->cache_area ) {
    self->tile_size--;
  }

  // Area of tiles, in pixels.
  self->tile_area = (size_t) pow (self->tile_size, 2.0);

  // Number of tiles which will fit in image cache.
  self->cache_size_in_tiles = self->cache_area / self->tile_area;
  // Can we fit as much as we intended in the cache?
  g_assert (self->cache_size_in_tiles 
	    == 2 * (size_t) ceil ((double)largest_dimension 
				  / self->tile_size));

  // Number of tiles image has been split into in x and y directions.
  self->tile_count_x = (size_t) ceil ((double)self->size_x / self->tile_size);
  self->tile_count_y = (size_t) ceil ((double)self->size_y / self->tile_size);

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

  // The addresses in the cache of the start of each of the tiles.
  // This array contains flattened tile addresses in the same way that
  // image memory normally uses flattened pixel addresses, e.g. the
  // address of tile x = 2, y = 4 is stored at self->tile_addresses[4
  // * self->tile_count_x + 2].  If a tile isn't in the cache, the
  // address is NULL (meaning it will have to be loaded).
  self->tile_addresses = g_new0 (float *, self->tile_count);
  g_assert (NULL == 0x0);	// Ensure g_new0 effectively sets to NULL.

  // Create a queue in order to keep track of which tile was loaded
  // longest ago.
  self->tile_queue = g_queue_new ();

  // Create the temporary tile oriented storage file.  This gets
  // filled in in different ways depending on which creation routine
  // we are using.
  GString *tile_file_name = g_string_new ("");
  gchar *current_dir = g_get_current_dir ();
  g_string_append_printf (tile_file_name, 
			  "%s/.float_image_tile_file_uNiQuIfY_nAmE_%d_%ld",
			  current_dir, getpid (), current_tile_file_number);
  g_free (current_dir);
  // This hard coded limit on the current number used to uniqueify
  // file names limits us to creating no more than ULONG_MAX instances
  // during a process.
  g_assert (current_tile_file_number < ULONG_MAX);
  current_tile_file_number++;
  // We block signals while we create and unlink this file, so we
  // don't end up leaving a huge temporary file somewhere.
  sigset_t all_signals, old_set;
  int return_code = sigfillset (&all_signals);
  g_assert (return_code == 0);
  return_code = sigprocmask (SIG_SETMASK, &all_signals, &old_set);
  self->tile_file = fopen (tile_file_name->str, "w+");
  g_assert (self->tile_file != NULL);
  return_code = unlink (tile_file_name->str);
  g_assert (return_code == 0);
  return_code = sigprocmask (SIG_SETMASK, &old_set, NULL);
  g_string_free (tile_file_name, TRUE);

  return self;
}

FloatImage *
float_image_new (size_t size_x, size_t size_y)
{
  FloatImage *self = initialize_float_image_structure (size_x, size_y);

  // The total width or height of all the tiles is probably greater
  // than the width or height of the image itself.
  size_t total_width = self->tile_count_x * self->tile_size;
  size_t total_height = self->tile_count_y * self->tile_size;

  // Fill the file full of zeros.  FIXME: there is almost certainly a
  // faster way to ensure that we have the disk space we need.
  float *zero_line = g_new0 (float, total_width);
  g_assert (0.0 == 0x0);	// Ensure the g_new0 did what we think.

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
	       "Error creating tile cache file for float_image instance: %s\n",
	       strerror (errno));
      // and exit.
      exit (EXIT_FAILURE);
    }
  }

  // Done with the line of zeros.
  g_free (zero_line);

  return self;
}

FloatImage *
float_image_new_with_value (size_t size_x, size_t size_y, float value)
{
  FloatImage *self = initialize_float_image_structure (size_x, size_y);

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
	       "Error creating tile cache file for float_image instance: %s\n",
	       strerror (errno));
      // and exit.
      exit (EXIT_FAILURE);
    }
  }

  // Done with the line full of values.
  g_free (value_line);

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

FloatImage *
float_image_new_from_file (size_t size_x, size_t size_y, char *file, 
			   off_t offset, float_image_byte_order_t byte_order)
{
  FloatImage *self = initialize_float_image_structure (size_x, size_y);
 
  // Open the file to read data from.
  FILE *fp = fopen (file, "r");
  // FIXME: we need some error handling and propagation here.
  g_assert (fp != NULL);

  // Seek to the indicated offset in the file.
  int return_code = fseeko (fp, offset, SEEK_SET);
  g_assert (return_code == 0);

  // We will read the input image data in horizontal stips one tile
  // high.  Note that we probably won't be able to entirely fill the
  // last tiles in each dimension with real data, since the image
  // sizes rarely divide evenly by the numbers of tiles.  So we fill
  // it with zeros instead.  The data off the edges of the image
  // should never be accessed directly anyway.

  // Some data for doing zero fill.
  float *zero_line = g_new0 (float, size_x);
  g_assert (0.0 == 0x0);

  // Buffer capable of holding a full strip.
  float *buffer = g_new (float, self->tile_size * self->size_x);

  // Reorganize data into tiles in tile oriented disk file.
  size_t ii = 0;
  for ( ii = 0 ; ii < self->tile_count_y ; ii++ ) {
    // The "effective_height" of the strip is the portion of the strip
    // for which data actually exists.  If the effective height is
    // less than self->tile>size, we will have to add some junk to
    // fill up the extra part of the tile (which should never be
    // accessed).
    size_t effective_height;
    if ( ii < self->tile_count_y - 1 || self->size_y % self->tile_size == 0 ) {
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

    // Convert from the byte order on disk to the host byte order, if
    // necessary.  Doing this with floats is somewhat questionable
    // apparently: major libraries don't seem to support it with their
    // macros, and the perl documentation says it can't be done in a
    // truly portable way... but it seems to work.
    if ( (G_BYTE_ORDER == G_LITTLE_ENDIAN 
	  && byte_order == FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN)
	 || (G_BYTE_ORDER == G_BIG_ENDIAN
	     && byte_order == FLOAT_IMAGE_BYTE_ORDER_LITTLE_ENDIAN) ) {
      // Floats better be four bytes for this to work.
      g_assert (sizeof (float) == 4);
      size_t idx;
      for ( idx = 0 ; idx < strip_area ; idx++ ) {
	swap_bytes_32 ((unsigned char *)&(buffer[idx]));
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
		   "Error creating tile cache file for float_image instance: "
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
		   "Error creating tile cache file for float_image instance: "
		   "%s\n", strerror (errno));
	  // and exit.
	  exit (EXIT_FAILURE);
	}
      }
    }
  }

  // Did we write the correct total amount of data?
  off_t end_offset = ftello (self->tile_file);
  end_offset = end_offset;
  g_assert (ftello (self->tile_file) 
	    == self->tile_area * self->tile_count * sizeof (float));

  // Free temporary buffers.
  g_free (buffer);
  g_free (zero_line);
  
  // Close file we read image from.
  return_code = fclose (fp);
  g_assert (return_code == 0);

  return self;
}

FloatImage *
float_image_new_from_memory (size_t size_x, size_t size_y, float *buffer)
{
  g_assert_not_reached ();	// Stubbed out for now.
  // Compiler reassurance.
  size_x = size_x;
  size_y = size_y;
  buffer = buffer;
  return NULL;
}

// Flush the contents of tile with flattened offset tile_offset from
// the memory cache to the disk file.  Its probably easiest to
// understand this function by looking at how its used.
void
cached_tile_to_disk (FloatImage *self, size_t tile_offset)
{
  int return_code 
    = fseeko (self->tile_file, 
	      (off_t)tile_offset * self->tile_area * sizeof (float),
		SEEK_SET);
    g_assert (return_code == 0);
    size_t write_count = fwrite (self->tile_addresses[tile_offset], 
				 sizeof (float), self->tile_area, 
				 self->tile_file);
    g_assert (write_count == self->tile_area);
}

// Do what is necessary to ensure that the tile containing pixel x, y
// is loaded, and return the actual address of the pixel.  FIXME: not
// sure we want to use this.  It avoids a lot of repetition in the
// code but requires an extra address-of/dreference pair per pixel,
// and possibly another function call if the compiler doesn't inline
// it.
static float *
prepare_pixel (FloatImage *self, size_t x, size_t y)
{
  // Get the pixel coordinates, including tile and pixel-in-tile.
  ldiv_t pc_x = ldiv (x, self->tile_size), pc_y = ldiv (y, self->tile_size);
  
  // Offset of tile x, y, where tiles are viewed as pixels normally are.
  size_t tile_offset = self->tile_count_x * pc_y.quot + pc_x.quot;

  // First we check if the tile is in the cache.
  float *tile_address = self->tile_addresses[tile_offset];
  if ( G_LIKELY (tile_address != NULL) ) {
    // If it is, just return the address of the pixel of interest.
    return &(tile_address[self->tile_size * pc_y.rem + pc_x.rem]);
  }
  else {
    // If it isn't, Find the address into which to load the new tile.
    // We have to check and see if we have to displace an already
    // loaded tile or not.
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
    g_queue_push_head (self->tile_queue, 
		       GINT_TO_POINTER ((int)tile_offset));

    // Load the tile data.
    int return_code 
      = fseeko (self->tile_file, 
		(off_t)tile_offset * self->tile_area * sizeof (float),
		SEEK_SET);
    g_assert (return_code == 0);
    size_t read_count = fread (tile_address, sizeof (float), self->tile_area, 
			       self->tile_file);
    g_assert (read_count == self->tile_area);
    
    return &(tile_address[self->tile_size * pc_y.rem + pc_x.rem]);
  }
}

float
float_image_get_pixel (FloatImage *self, ssize_t x, ssize_t y)
{
  // Are we at a valid image pixel?
  g_assert (x >= 0 && (size_t)x <= self->size_x);
  g_assert (y >= 0 && (size_t)y <= self->size_y);

  // Get the pixel coordinates, including tile and pixel-in-tile.
  g_assert (sizeof (long int) >= sizeof (size_t));
  ldiv_t pc_x = ldiv (x, self->tile_size), pc_y = ldiv (y, self->tile_size);
  
  // Offset of tile x, y, where tiles are viewed as pixels normally are.
  size_t tile_offset = self->tile_count_x * pc_y.quot + pc_x.quot;

  // First we check if the tile is in the cache.
  float *tile_address = self->tile_addresses[tile_offset];
  if ( G_LIKELY (tile_address != NULL) ) {
    // If it is, just return the pixel of interest.
    return tile_address[self->tile_size * pc_y.rem + pc_x.rem];
  }
  else {
    // If it isn't, Find the address into which to load the new tile.
    // We have to check and see if we have to displace an already
    // loaded tile or not.
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
    g_queue_push_head (self->tile_queue, 
		       GINT_TO_POINTER ((int)tile_offset));

    // Load the tile data.
    int return_code 
      = fseeko (self->tile_file, 
		(off_t)tile_offset * self->tile_area * sizeof (float),
		SEEK_SET);
    g_assert (return_code == 0);
    size_t read_count = fread (tile_address, sizeof (float), self->tile_area, 
			       self->tile_file);
    g_assert (read_count == self->tile_area);

    return tile_address[self->tile_size * pc_y.rem + pc_x.rem];
  }
}

void
float_image_set_pixel (FloatImage *self, ssize_t x, ssize_t y, float value)
{
  // Are we at a valid image pixel?
  g_assert (x >= 0 && (size_t)x <= self->size_x);
  g_assert (y >= 0 && (size_t)y <= self->size_y);

  // Get the pixel coordinates, including tile and pixel-in-tile.
  g_assert (sizeof (long int) >= sizeof (size_t));
  ldiv_t pc_x = ldiv (x, self->tile_size), pc_y = ldiv (y, self->tile_size);
  
  // Offset of tile x, y, where tiles are viewed as pixels normally are.
  size_t tile_offset = self->tile_count_x * pc_y.quot + pc_x.quot;

  // First we check if the tile is in the cache.
  float *tile_address = self->tile_addresses[tile_offset];
  if ( G_LIKELY (tile_address != NULL) ) {
    // If it is, just set the pixel of interest.
    tile_address[self->tile_size * pc_y.rem + pc_x.rem] = value;
  }
  else {
    // If it isn't, Find the address into which to load the new tile.
    // We have to check and see if we have to displace an already
    // loaded tile or not.
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
    g_queue_push_head (self->tile_queue, 
		       GINT_TO_POINTER ((int)tile_offset));

    // Load the tile data.
    int return_code 
      = fseeko (self->tile_file, 
		(off_t)tile_offset * self->tile_area * sizeof (float),
		SEEK_SET);
    g_assert (return_code == 0);
    size_t read_count = fread (tile_address, sizeof (float), self->tile_area, 
			       self->tile_file);
    g_assert (read_count == self->tile_area);
    
    // Set pixel of interest.
    tile_address[self->tile_size * pc_y.rem + pc_x.rem] = value;
  }
}

void
float_image_get_region (FloatImage *self, size_t x, size_t y, size_t size_x, 
			size_t size_y, float *buffer)
{
  g_assert_not_reached ();	// Stubbed out for now.}
  // Compiler reassurance.
  self = self; x = x; y = y; size_x = size_x, size_y = size_y; buffer = buffer;
  // FIXME: THIS LINE IS MEANINGLESS JUNK COMPILER REASSURANCE THAT
  // prepare_pixel get used somewhere.
  prepare_pixel (self, 0, 0);
}

void
float_image_set_region (FloatImage *self, size_t x, size_t y, size_t size_x, 
			size_t size_y, float *buffer)
{
  g_assert_not_reached ();	// Stubbed out for now.}
  self = self; x = x; y = y; size_x = size_x, size_y = size_y; buffer = buffer;
}

int
float_image_store (FloatImage *self, char *file, 
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
    size_t jj;
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      line_buffer[jj] = float_image_get_pixel (self, jj, ii);
    }
    // Convert from the host byte order to the byte order specified
    // for the disk file, if necessary.  Doing this with floats is
    // somewhat questionable apparently: major libraries don't seem to
    // support it with their macros, and the perl documentation says
    // it can't be done in a truly portable way... but it seems to
    // work.
    if ( (G_BYTE_ORDER == G_LITTLE_ENDIAN 
	  && byte_order == FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN)
	 || (G_BYTE_ORDER == G_BIG_ENDIAN
	     && byte_order == FLOAT_IMAGE_BYTE_ORDER_LITTLE_ENDIAN) ) {
      // Floats better be four bytes for this to work.
      g_assert (sizeof (float) == 4);
      size_t idx;
      for ( idx = 0 ; idx < self->size_x ; idx++ ) {
	swap_bytes_32 ((unsigned char *)&(line_buffer[idx]));
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

size_t
float_image_get_cache_size (FloatImage *self)
{
  g_assert_not_reached ();	// Stubbed out for now.
  // Compiler reassurance.
  self = self;
  return 0;
}

void
float_image_set_cache_size (FloatImage *self, size_t size)
{
  g_assert_not_reached ();	// Stubbed out for now.}
  // Compiler reassurance.
  self = self; size = size;
}

void
float_image_free (FloatImage *self)
{
  // Close and remove the tile file.
  int return_code = fclose (self->tile_file);
  g_assert (return_code == 0);

  // Deallocate dynamic memory.
  g_free (self->tile_addresses);
  g_queue_free (self->tile_queue);
  g_free (self->cache);

  g_free (self);
}
