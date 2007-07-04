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
                         float *dest, void *read_client_info,
                         meta_parameters *meta);
typedef int ThumbFn(FILE *fp, int thumb_size_x,
                    int thumb_size_y, meta_parameters *meta,
                    void *read_client_info, float *dest);

typedef struct {
  int nl, ns;               // Image dimensions.
  ReadClientFn *read_fn;    // Function to read data (the "client")
  ThumbFn *thumb_fn;        // Function to thumbnail data
  void *read_client_info;   // Pointer to the read client's info
  int n_tiles;              // Number of tiles in memory
  int reached_max_tiles;    // Have we loaded as many tiles as we can?
  int rows_per_tile;        // Number of rows in each tile
  int entire_image_fits;    // TRUE if we can load the entire image
  int *rowstarts;           // Row numbers starting each tile
  float **cache;            // Cached values
  int *access_counts;       // Updated when a tile is accessed
  FILE *fp;                 // file pointer
  int n_access;             // used to find oldest tile
  meta_parameters *meta;    // metadata
} CachedImage;

CachedImage * cached_image_new_from_file(
    const char *file, meta_parameters *meta,
    ReadClientFn *read_fn, ThumbFn *thumb_fn,
    void *read_client_info);

float cached_image_get_pixel (CachedImage *self, int line, int samp);

void load_thumbnail_data(CachedImage *self, int thumb_size_x, int thumb_size_y,
                        float *dest);

void cached_image_free (CachedImage *self);

#endif
