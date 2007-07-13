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

// sometimes we don't have this - choose a conservative value
#ifndef SSIZE_MAX
#define SSIZE_MAX 32767
#endif

typedef int ReadClientFn(FILE *fp, int row_start, int n_rows_to_get,
                         void *dest, void *read_client_info,
                         meta_parameters *meta);
typedef int ThumbFn(FILE *fp, int thumb_size_x,
                    int thumb_size_y, meta_parameters *meta,
                    void *read_client_info, void *dest);
typedef void FreeFn(void *read_client_info);

// Adding a new supported data type:
//  cache.c:
//    data_size()
//    cached_image_get_pixel()
//    cached_image_get_rgb()
//  stats.c:
//    generate_thumbnail_data()
//  big_image.c:
//    update_pixel_info()
//  read_X.c: (for X clients that will handle the data type)
//    open_X_data()
//     
typedef enum {
    UNDEFINED = 0,
    GREYSCALE_FLOAT = 1,
    GREYSCALE_BYTE = 2,
    RGB_BYTE = 3
} ssv_data_type_t;

typedef struct {
    ReadClientFn *read_fn;
    ThumbFn *thumb_fn;
    FreeFn *free_fn;
    void *read_client_info;
    ssv_data_type_t data_type;
    int require_full_load;
} ClientInterface;

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
  FILE *fp;                 // file pointer
  int n_access;             // used to find oldest tile
  ssv_data_type_t data_type;// type of data we have
  meta_parameters *meta;    // metadata -- don't own this pointer
} CachedImage;

CachedImage * cached_image_new_from_file(
    const char *file, meta_parameters *meta, ClientInterface *client);

float cached_image_get_pixel (CachedImage *self, int line, int samp);
void cached_image_get_rgb(CachedImage *self, int line, int samp,
                          unsigned char *r, unsigned char *g,
                          unsigned char *b);

void load_thumbnail_data(CachedImage *self, int thumb_size_x, int thumb_size_y,
                         void *dest);

void cached_image_free (CachedImage *self);

#endif
