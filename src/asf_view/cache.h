#ifndef CACHED_IMAGE_H
#define CACHED_IMAGE_H

#ifndef solaris
#  include <stdint.h>
#endif
#include "asf_meta.h"
#include "float_image.h"
#include <stdio.h>
#include <sys/types.h>

#include <glib.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_histogram.h>

//---------------------------------------------------------------------------
// Adding a new supported data type:
//  cache.c:
//    data_size()
//    cached_image_get_pixel()
//    cached_image_get_rgb()
//  stats.c:
//    generate_thumbnail_data()
//  big_image.c:
//    update_pixel_info()
//  read_X.c: (for clients that will handle the data type)
//    open_X_data()
//     
typedef enum {
    UNDEFINED = 0,
    GREYSCALE_FLOAT = 1,
    GREYSCALE_BYTE = 2,
    RGB_BYTE = 3,
    RGB_FLOAT = 4
} ssv_data_type_t;

// Stats structure -- this one is used by both greyscale and RGB
typedef struct {
    double map_min, map_max;
    double avg, stddev;
    double act_min, act_max; // absolute min/max of all values
    int hist[256];           // histogram
    double no_data_value;    // value indicating "no data"
    int have_no_data;        // TRUE if "no_data_value" is present
    double no_data_min;
    double no_data_max;
    int have_no_data_range;
    int truncate;
} ImageStats;

// Stats structure -- this one is used only for RGB images, keeps track
//                    of stats for each channel
typedef struct {
    double map_min, map_max;
    double avg, stddev;
    double act_min, act_max; // absolute min/max of all values
    double no_data_value;
    int have_no_data;
    double no_data_max;
    double no_data_min;
    int have_no_data_range;
    int truncate;
} ImageStatsRGB;

// NOTE: At the moment, the Pixbuf that contains the displayed data
// is plain RGB, not RGB-A, so adding support for transparency would
// also involve changing big_image.c/make_big_image(), and you'd have
// to add a method to the cache interface that returned the alpha
// channel in addition to the rgb values.

//---------------------------------------------------------------------------
// This is the client interface -- the set of function pointers, etc,
// that encapsulate how the image cache gets data from the file.
// See: -read.c/read_file() for how the cache is hooked up to the
//          client depending on what the file type is
//      -read_template.c for an example of how to add another client
typedef int ReadClientFn(int row_start, int n_rows_to_get,
                         void *dest, void *read_client_info,
                         meta_parameters *meta, int data_type);
typedef int ThumbFn(int thumb_size_x, int thumb_size_y, meta_parameters *meta,
                    void *read_client_info, void *dest, int data_type);
typedef void FreeFn(void *read_client_info);

typedef struct {
    ReadClientFn *read_fn;
    ThumbFn *thumb_fn;
    FreeFn *free_fn;
    void *read_client_info;
    ssv_data_type_t data_type;
    int require_full_load;
} ClientInterface;


//---------------------------------------------------------------------------
// Here is the ImageCache stuff.  The global ImageCache that holds the
// loaded image is "data_ci".  This is all private data.
typedef struct {
  int nl, ns;               // Image dimensions.
  ClientInterface *client;  // pointers to data read implementations
  int n_tiles;              // Number of tiles in memory
  int reached_max_tiles;    // Have we loaded as many tiles as we can?
  int rows_per_tile;        // Number of rows in each tile
  int entire_image_fits;    // TRUE if we can load the entire image
  int *rowstarts;           // Row numbers starting each tile
  unsigned char **cache;    // Cached values (floats, unsigned chars ...)
  int *access_counts;       // Updated when a tile is accessed
  int n_access;             // used to find oldest tile
  ssv_data_type_t data_type;// type of data we have
  meta_parameters *meta;    // metadata -- don't own this pointer
  ImageStats *stats;        // not owned by us, not populated by us
  ImageStatsRGB *stats_r;   // not owned by us, not populated by us
  ImageStatsRGB *stats_g;   // not owned by us, not populated by us
  ImageStatsRGB *stats_b;   // not owned by us, not populated by us
} CachedImage;

CachedImage * cached_image_new_from_file(
    const char *file, meta_parameters *meta, ClientInterface *client,
    ImageStats *stats, ImageStatsRGB *stats_r, ImageStatsRGB *stats_g,
    ImageStatsRGB *stats_b);

float cached_image_get_pixel (CachedImage *self, int line, int samp);
void cached_image_get_rgb(CachedImage *self, int line, int samp,
                          unsigned char *r, unsigned char *g,
                          unsigned char *b);
void cached_image_get_rgb_float(CachedImage *self, int line, int samp,
                                float *r, float *g, float *b);

void load_thumbnail_data(CachedImage *self, int thumb_size_x, int thumb_size_y,
                         void *dest);

void cached_image_free (CachedImage *self);

#endif
