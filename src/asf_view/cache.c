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
#include "float_image.h"
#include "asf_endian.h"
#include "asf_jpeg.h"

#include "asf_view.h"

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

// at 64MB tiles, this is ~1.5GB
static const int MAX_TILES = 24;

// quit blathering?
int quiet = FALSE;

static int data_size(CachedImage *self)
{
    switch (self->data_type) {
        case GREYSCALE_FLOAT:
            return 4;
        case RGB_BYTE:
            return 3;
        case GREYSCALE_BYTE:
            return 1;
        case RGB_FLOAT:
            return 12;
        default:
            assert(FALSE);
            return 0;
    }
}

static void print_cache_size(CachedImage *self)
{
    int i;
    int size=0;
    int ds = data_size(self);
    for (i=0; i<self->n_tiles; ++i)
        size += ds*self->rows_per_tile*self->ns;

    asfPrintStatus("Cache size is %.1f megabytes.\n",
        (float)size/1024./1024.);
}

static unsigned char *get_pixel(CachedImage *self, int line, int samp)
{
    // check if outside the image
    static unsigned char zero = 0;
    if (line<0 || samp<0 || line >= self->nl || samp >= self->ns)
        return &zero;

    // size of each pixel
    int ds = data_size(self);

    int i;
    for (i=0; i<self->n_tiles; ++i) {
        int rs = self->rowstarts[i];
        if (rs >= 0) {
            if (line >= rs && line < rs+self->rows_per_tile) {
                // found the right cache
                assert(self->cache[i]);

                // this probably won't ever happen, but here we go anyway
                if (self->n_access > 1024*1024*1024) {
                    asfPrintStatus("Resetting n_access.\n");
                    for (i=0; i<self->n_tiles; ++i)
                        self->access_counts[i] = 0;
                    self->n_access = 1;
                }

                // mark this as the most recently accessed
                self->access_counts[i] = self->n_access++;

                // return pointer to the cached value
                return &self->cache[i][((line-rs)*self->ns + samp)*ds];
            }
        }
    }

    int spot = 0;
    if (!self->reached_max_tiles) {
        assert(self->cache[self->n_tiles] == NULL);
        unsigned char *data = malloc(ds*self->ns*self->rows_per_tile);
        if (!data) {
            // if this is the first tile -- abort, we are out of memory
            if (self->n_tiles == 0)
                asfPrintError("Failed to allocate cache of %ld bytes.\n"
                              "Out of memory.\n",
                              ds*self->ns*self->rows_per_tile);
            // couldn't allocate the next tile -- must dump existing
            if (!quiet)
                asfPrintStatus("reached max # of tiles: %d\n", self->n_tiles);
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
        if (!quiet)
            asfPrintStatus("Fully loaded with %d tiles.\n", self->n_tiles);
        print_cache_size(self);
        self->reached_max_tiles = TRUE;
    }

    // load info from file
    assert(spot >= 0 && spot < self->n_tiles);
    assert(self->cache[spot] != NULL);

    // clear out the cache -- we may not fill up the tile, if
    // we are near the end of the file, and we don't want old data
    // to appear
    int ns = self->ns;
    memset(self->cache[spot], 0, ds*ns*self->rows_per_tile);

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
    if (rs + self->rows_per_tile > self->nl)
        rows_to_get = self->nl - rs;

    if (!quiet) {
        asfPrintStatus("Cache: loading into spot #%d: rows %d-%d\n",
            spot, rs, rs+rows_to_get);
        //print_cache_size(self);
    }

    self->client->read_fn(rs, rows_to_get, (void*)(self->cache[spot]),
        self->client->read_client_info, self->meta, self->client->data_type);

    assert((line-rs)*self->ns + samp <= self->ns*self->rows_per_tile);
    return &self->cache[spot][((line-rs)*self->ns + samp)*ds];
}

void load_thumbnail_data(CachedImage *self, int thumb_size_x, int thumb_size_y,
                         void *dest_void)
{
    if (self->entire_image_fits || !self->client->thumb_fn) {
        // Either we don't have thumbnailing support from the client,
        // or the image will fit entirely in memory.  In both cases, we
        // can just call get_pixel() on the subset necessary to show
        // the image, which will load the entire image into cache.
        // (... well, unless it is so huge that our tiles fit between
        //      the thumbnail grid ... but that's crazy talk)
        int ds = data_size(self);
        unsigned char *dest = (unsigned char*)dest_void;

        // this will fill the cache with the image data
        int sf = self->meta->general->line_count / thumb_size_y;
        //assert(sf == self->meta->general->sample_count / thumb_size_x);

        // supress the "populating cache" msgs when loading the whole thing
        quiet=TRUE;

        int i,j;
        for (i=0; i<thumb_size_y; ++i) {
            for (j=0; j<thumb_size_x; ++j) {
                // make this independent of the data type
                unsigned char *p = get_pixel(self,i*sf,j*sf);
                memcpy(dest+(i*thumb_size_x+j)*ds, p, ds);
            }
            asfPercentMeter((float)i/(thumb_size_y-1));
        }

        quiet=FALSE;
    } else {
        self->client->thumb_fn(thumb_size_x, thumb_size_y,
            self->meta, self->client->read_client_info, dest_void,
            self->client->data_type);
    }
}

CachedImage * cached_image_new_from_file(
    const char *file, meta_parameters *meta, ClientInterface *client,
    ImageStats *stats, ImageStatsRGB *stats_r, ImageStatsRGB *stats_g,
    ImageStatsRGB *stats_b)
{
    CachedImage *self = MALLOC(sizeof(CachedImage));

    asfPrintStatus("Opening cache: %s\n", file);

    self->data_type = client->data_type;
    assert(self->data_type != UNDEFINED);

    self->client = client;     // take ownership of this
    self->meta = meta;         // do NOT take ownership of this

    self->stats = stats;       // do NOT take ownership of this

    self->stats_r = stats_r;   // do NOT take ownership of this
    self->stats_g = stats_g;   // do NOT take ownership of this
    self->stats_b = stats_b;   // do NOT take ownership of this

    // line line_count may have been fudges, if we are multilooking
    self->nl = meta->general->line_count;
    self->ns = meta->general->sample_count;
    asfPrintStatus("Image is %dx%d LxS\n", self->nl, self->ns);

    if (client->require_full_load) {
        // Use only 1 tile -- load entire image into it
        // (client tells us we should do it this way)
        // will it fit?  Who knows.  Assume it will, MALLOC will fail
        // if it actually does not.
        self->rows_per_tile = self->nl;
    } else {
        // how many rows per tile?
        // We will use ~64 Meg tiles
        self->rows_per_tile = 64*1024*1024 / (self->ns*data_size(self));

        // test line -- uncomment this for very small tiles
        //self->rows_per_tile = 2*1024*1024 / (self->ns*data_size(self));
    }

    asfPrintStatus("Using %d rows per tile.\n", self->rows_per_tile);

    int n_tiles_required = (int)ceil((double)self->nl / self->rows_per_tile);
    self->entire_image_fits = n_tiles_required <= MAX_TILES;
    // self->entire_image_fits = FALSE; // uncomment to test thumb_fn

    // at the beginning, we have no tiles
    self->n_tiles = 0;
    self->reached_max_tiles = FALSE;

    int i;
    self->rowstarts = MALLOC(sizeof(int)*MAX_TILES);
    self->cache = MALLOC(sizeof(float*)*MAX_TILES);
    self->access_counts = MALLOC(sizeof(int)*MAX_TILES);
    for (i=0; i<MAX_TILES; ++i) {
        self->rowstarts[i] = -1;
        self->cache[i] = NULL;
        self->access_counts[i] = 0;
    }

    self->n_access = 0;

    asfPrintStatus("Number of tiles required for the entire image: %d\n",
        n_tiles_required);
    asfPrintStatus("Fits in memory: %s\n",
        self->entire_image_fits ? "Yes" : "No");

    return self;
}

float cached_image_get_pixel (CachedImage *self, int line, int samp)
{
    if (self->data_type == GREYSCALE_FLOAT) {
        return *((float*)get_pixel(self, line, samp));
    }
    else if (self->data_type == GREYSCALE_BYTE) {
        return (float) *(get_pixel(self, line, samp));
    }
    else if (self->data_type == RGB_BYTE || self->data_type == RGB_FLOAT) {
        unsigned char r, g, b;
        cached_image_get_rgb(self, line, samp, &r, &g, &b);
        return ((float)r + (float)g + (float)b)/3.;
    }

    // not reached
    assert(0);
    return 0;
}

void cached_image_get_rgb(CachedImage *self, int line, int samp,
                          unsigned char *r, unsigned char *g,
                          unsigned char *b)
{
    if (self->data_type == GREYSCALE_FLOAT) {
        float f = cached_image_get_pixel(self, line, samp);
        if (have_lut()) {
            // do not scale in the case of a lut
            apply_lut((int)f, r, g, b);
        } else {
            *r = *g = *b =
              (unsigned char)calc_scaled_pixel_value(self->stats, f);
        }
    }
    else if (self->data_type == GREYSCALE_BYTE) {
        if (have_lut()) {
            apply_lut((int)(*(get_pixel(self, line, samp))), r, g, b);
        }
        else {
            float f = (float) *(get_pixel(self, line, samp));
            *r = *g = *b =
                (unsigned char)calc_scaled_pixel_value(self->stats, f);
        }
    }
    else if (self->data_type == RGB_BYTE) {
        unsigned char *uc = get_pixel(self, line, samp);

        *r = (unsigned char)calc_rgb_scaled_pixel_value(self->stats_r,
                                                        (float)uc[0]);
        *g = (unsigned char)calc_rgb_scaled_pixel_value(self->stats_g,
                                                        (float)uc[1]);
        *b = (unsigned char)calc_rgb_scaled_pixel_value(self->stats_b,
                                                        (float)uc[2]);
    }
    else if (self->data_type == RGB_FLOAT) {
        float *f = (float*)get_pixel(self, line, samp);

        *r = (unsigned char)calc_rgb_scaled_pixel_value(self->stats_r,f[0]);
        *g = (unsigned char)calc_rgb_scaled_pixel_value(self->stats_g,f[1]);
        *b = (unsigned char)calc_rgb_scaled_pixel_value(self->stats_b,f[2]);
    }
    else {
        // impossible!
        assert(0);
        *r = *g = *b = 0;
    }
}

void cached_image_get_rgb_float(CachedImage *self, int line, int samp,
                                float *r, float *g, float *b)
{
    if (self->data_type == GREYSCALE_FLOAT) {
        float f = cached_image_get_pixel(self, line, samp);
        *r = *g = *b = calc_scaled_pixel_value(self->stats, f);
    }
    else if (self->data_type == GREYSCALE_BYTE) {
        *r = *g = *b = (float)(*(get_pixel(self, line, samp)));
    }
    else if (self->data_type == RGB_BYTE) {
        unsigned char *uc = get_pixel(self, line, samp);

        *r = (float)(uc[0]);
        *g = (float)(uc[1]);
        *b = (float)(uc[2]);
    }
    else if (self->data_type == RGB_FLOAT) {
        float *f = (float*)get_pixel(self, line, samp);

        *r = f[0];
        *g = f[1];
        *b = f[2];
    }
    else {
        // impossible!
        assert(0);
        *r = *g = *b = 0.;
    }
}

void cached_image_free (CachedImage *self)
{
    int i;
    for (i=0; i<self->n_tiles; ++i) {
        if (self->cache[i])
            free(self->cache[i]);
    }

    if (self->client->free_fn)
      self->client->free_fn(self->client->read_client_info);

    free(self->rowstarts);
    free(self->access_counts);
    free(self->cache);
    free(self->client);

    // we do not own the metadata -- don't free it!

    free(self);
}

