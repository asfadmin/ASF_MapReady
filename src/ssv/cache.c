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

#include <jpeglib.h>
#include "float_image.h"
#include "asf.h"
#include "asf_endian.h"

#include "ssv.h"

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

// at 64MB tiles, this is 1GB
static const int MAX_TILES = 16;

CachedImage * cached_image_new_from_file(
    const char *file, int band, int headerLen, int recLen,
    meta_parameters *meta, int is_asf_internal,
    float_image_byte_order_t byte_order)
{
    CachedImage *self = MALLOC(sizeof(CachedImage));

    self->nl = meta->general->line_count;
    self->ns = meta->general->sample_count;
    self->band = band;
    self->headerLen = headerLen;
    self->recLen = recLen;
    self->is_asf_internal = is_asf_internal;

    // how many rows per tile?
    // We will use ~64 Meg tiles
    self->rows_per_tile = 64*1024*1024 / (ns*4);

    // test line -- uncomment this for very small tiles
    //self->rows_per_tile = 2*1024*1024 / (ns*4);

    printf("Using %d rows per tile.\n", self->rows_per_tile);

    // at the beginning, we have no tiles
    self->n_tiles = 0;
    self->reached_max_tiles = FALSE;

    self->rowstarts = MALLOC(sizeof(int)*MAX_TILES);
    self->cache = MALLOC(sizeof(float*)*MAX_TILES);
    self->access_counts = MALLOC(sizeof(int)*MAX_TILES);

    int i;
    for (i=0; i<MAX_TILES; ++i) {
        self->rowstarts[i] = -1;
        self->cache[i] = NULL;
        self->access_counts[i] = 0;
    }

    self->n_access = 0;

    self->fp = FOPEN(file, "rb");

    return self;
}

static void print_cache_size(CachedImage *self)
{
    int i;
    int size=0;
    for (i=0; i<self->n_tiles; ++i)
        size += 4*self->rows_per_tile*self->ns;
    printf("Cache size is %.1f megabytes.\n", (float)size/1024./1024.);
}

float cached_image_get_pixel (CachedImage *self, int line, int samp)
{
    int i;
    for (i=0; i<self->n_tiles; ++i) {
        int rs = self->rowstarts[i];
        if (rs >= 0) {
            //printf(" --> %d: %d-%d\n", i, rs, rs+self->rows_per_tile); 
            if (line >= rs && line < rs+self->rows_per_tile) {
                // found the right cache
                assert(self->cache[i]);
                // mark this as the most recently accessed
                self->access_counts[i] = self->n_access++;

                if (self->n_access > 1024*1024*1024)
                    printf("n_accesses is getting a little out of hand!\n");

                // return cached value
                return self->cache[i][(line-rs)*self->ns + samp];
            }
        }
    }

    int spot = 0;
    if (!self->reached_max_tiles) {
        assert(self->cache[self->n_tiles] == NULL);
        float *data = malloc(sizeof(float)*ns*self->rows_per_tile);
        if (!data) {
            // couldn't allocate the next tile -- must dump existing
            printf("reached max # of tiles: %d\n", self->n_tiles);
            print_cache_size(self);
            self->reached_max_tiles = TRUE;
        } else {
            spot = self->n_tiles;
            self->cache[spot] = data;
            ++self->n_tiles;
        }
    }

    if (self->reached_max_tiles) {
        // dump an existing cached tile
        // not found in the cache -- find least used spot
        int least_access_count = self->access_counts[0];
        for (i=0; i<self->n_tiles; ++i) {
            if (self->access_counts[i] < least_access_count) {
                least_access_count = self->access_counts[i];
                spot = i;
            }
        }
    }

    if (!self->reached_max_tiles && self->n_tiles == MAX_TILES) {
        printf("Fully loaded with %d tiles.\n", self->n_tiles);
        print_cache_size(self);
        self->reached_max_tiles = TRUE;
    }

    // load info from file
    assert(spot >= 0 && spot < self->n_tiles);
    assert(self->cache[spot] != NULL);

    // clear out the cache -- we may not fill up the tile, if
    // we are near the end of the file, and we don't want old data
    // to appear
    memset(self->cache[spot], 0, sizeof(float)*ns*self->rows_per_tile);

    // update where this cache entry starts
    int rs = (line / self->rows_per_tile) * self->rows_per_tile;
    self->rowstarts[spot] = rs;

    // mark this tile as the most recently accessed
    self->access_counts[spot] = self->n_access++;

    //printf("Updated entry #%d:\n"
    //    "     row start: %d\n"
    //    "     row end: %d\n"
    //    "     access count: %d\n", spot,
    //    rs, rs+self->rows_per_tile-1, self->access_counts[spot]);

    // ensure we don't read past the end of the file
    int rows_to_get = self->rows_per_tile;
    if (rs + self->rows_per_tile > nl)
        rows_to_get = nl - rs;

    printf("Cache: loading into spot #%d: rows %d-%d\n",
        spot, rs, rs+rows_to_get);
    print_cache_size(self);

    //printf("Reading %d rows\n", rows_to_get);

    // for asf internal, can use get_float_line
    if (self->is_asf_internal) {
        // asf internal is the easy one
        get_float_lines(self->fp, meta, rs + nl*self->band,
            rows_to_get, self->cache[spot]);
    } else {
        // ceos
        if (meta->general->data_type == INTEGER16) {
            unsigned short *shorts = MALLOC(sizeof(unsigned short)*self->ns);
            for (i=rs; i<rs+rows_to_get; ++i) {
                long long offset =
                    (long long)(self->headerLen + i*self->recLen);

                FSEEK64(self->fp, offset, SEEK_SET);
                FREAD(shorts, sizeof(unsigned short), self->ns, self->fp);

                int j;
                for (j=0; j<self->ns; ++j) {
                    big16(shorts[j]);
                    self->cache[spot][i*self->ns + j] = (float)shorts[j];
                }
            }
            free (shorts);
        } else if (meta->general->data_type == BYTE) {
            unsigned char *bytes = MALLOC(sizeof(unsigned char)*self->ns);
            for (i=rs; i<rs+self->rows_per_tile; ++i) {
                long long offset =
                    (long long)(self->headerLen + i*self->recLen);

                FSEEK64(self->fp, offset, SEEK_SET);
                FREAD(bytes, sizeof(unsigned char), self->ns, self->fp);

                int j;
                for (j=0; j<self->ns; ++j)
                    self->cache[spot][i*self->ns + j] = (float)bytes[j];
            }
            free (bytes);
        }
    }

    return self->cache[spot][(line-rs)*self->ns + samp];
}

void cached_image_free (CachedImage *self)
{
    fclose(self->fp);

    int i;
    for (i=0; i<self->n_tiles; ++i) {
        if (self->cache[i])
            free(self->cache[i]);
    }

    free(self->rowstarts);
    free(self->access_counts);
    free(self->cache);

    // we do not own the metadata -- don't free it!
    free(self);
}

