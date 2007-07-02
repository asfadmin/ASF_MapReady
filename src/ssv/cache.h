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

typedef struct {
  int nl, ns;               // Image dimensions.
  int band;                 // Band #
  int headerLen;            // Header length in the file (CEOS)
  int recLen;               // Length of each record in the file (CEOS)
  int n_tiles;              // Number of tiles in memory
  int reached_max_tiles;    // Have we loaded as many tiles as we can?
  int rows_per_tile;        // Number of rows in each tile
  int *rowstarts;           // Row numbers starting each tile
  float **cache;            // Cached values
  int *access_counts;       // Updated when a tile is accessed
  FILE *fp;                 // file pointer
  int n_access;             // used to find oldest tile
  int is_asf_internal;      // is file ASF Internal?  If false, CEOS.
  meta_parameters *meta;    // metadata
} CachedImage;

CachedImage * cached_image_new_from_file(
    const char *file, int band, int headerLen, int recLen,
    meta_parameters *meta, int is_asf_internal,
    float_image_byte_order_t byte_order);

float cached_image_get_pixel (CachedImage *self, int line, int samp);

void cached_image_free (CachedImage *self);

#endif
