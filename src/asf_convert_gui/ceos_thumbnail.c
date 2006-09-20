#include <unistd.h>
#include <asf_meta.h>
#include <ceos_io.h>
#include <float_image.h>
#include <math.h>

#include <asf_nan.h>
#include "ceos_thumbnail.h"

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    g_free(pixels);
}

GdkPixbuf *
make_input_image_thumbnail_pixbuf (const char *input_metadata, 
                                   const char *input_data,
                                   size_t max_thumbnail_dimension)
{
    /* This can happen if we don't get around to drawing the thumbnail
       until the file has already been processes & cleaned up, don't want
       to crash in that case. */
    if (!fileExists(input_metadata))
        return NULL;

    meta_parameters *imd = meta_create (input_metadata); // Input metadata.
    /* Make a copy of one of the arguments so the compilers doesn't
    complain about us ignoring the const qualifier when we pass it fo
    fopenCeos().  */

    if (imd->general->data_type != BYTE &&
        imd->general->data_type != INTEGER16 &&
        imd->general->data_type != INTEGER32 &&
        imd->general->data_type != REAL32 &&
        imd->general->data_type != REAL64)
    {
        /* don't know how to make a thumbnail for this type ... */
        return NULL;
    }

    gchar *tmp = g_strdup (input_data);
    g_assert (tmp != NULL);
    CEOS_FILE *id = fopenCeos (tmp); // Input data file.
    g_free (tmp);

    if (!id->f_in) {
        // failed for some reason, just quit without thumbnailing...
        // possibly the file is bad, or perhaps it was removed from the
        // file list before we could get around to thumbnailing it.
        meta_free(imd);
        return NULL;
    }

    // use a larger dimension at first, for our crude scaling.  We will
    // use a better scaling method later, from GdbPixbuf
    int larger_dim = 512;

    // Vertical and horizontal scale factors required to meet the
    // max_thumbnail_dimension part of the interface contract.
    int vsf = ceil (imd->general->line_count / larger_dim);
    int hsf = ceil (imd->general->sample_count / larger_dim);
    // Overall scale factor to use is the greater of vsf and hsf.
    int sf = (hsf > vsf ? hsf : vsf);

    // Thumbnail image sizes.
    size_t tsx = imd->general->sample_count / sf;
    size_t tsy = imd->general->line_count / sf;

    // Thumbnail image buffers - 'idata' is the temporary data prior
    // to scaling to 2-sigma, 'data' is the byte buffer used to create the
    // pixbuf, it will need 3 bytes per value, all equal, since the pixbuf
    // wants an RGB value.
    int *idata = g_new(int, tsx*tsy);
    guchar *data = g_new(guchar, 3*tsx*tsy);

    // Form the thumbnail image by grabbing individual pixels.  FIXME:
    // Might be better to do some averaging or interpolating.
    size_t ii;
    int *line = g_new (int, imd->general->sample_count);

    // Keep track of the average pixel value, so later we can do a 2-sigma
    // scaling - makes the thumbnail look a little nicer and more like what
    // they'd get if they did the default jpeg export.
    double avg = 0.0;
    for ( ii = 0 ; ii < tsy ; ii++ ) {
        readCeosLine (line, ii * sf, id);
        size_t jj;
        for ( jj = 0 ; jj < tsx ; jj++ ) {
            // Current sampled value.
            double csv;		

            // We will average a couple pixels together.
            if ( jj * sf < imd->general->line_count - 1 ) {
                csv = (line[jj * sf] + line[jj * sf + 1]) / 2;
            }
            else {
                csv = (line[jj * sf] + line[jj * sf - 1]) / 2;
            }

            idata[ii*tsx + jj] = (int)csv;
            avg += csv;
        }
    }
    g_free (line);
    closeCeos(id);

    // Compute the std devation
    avg /= tsx*tsy;
    double stddev = 0.0;
    for (ii = 0; ii < tsx*tsy; ++ii)
        stddev += ((double)idata[ii] - avg) * ((double)idata[ii] - avg);
    stddev = sqrt(stddev / (tsx*tsy));
    
    // Set the limits of the scaling - 2-sigma on either side of the mean
    double lmin = avg - 2*stddev;
    double lmax = avg + 2*stddev;
    
    // Now actually scale the data, and convert to bytes.
    // Note that we need 3 values, one for each of the RGB channels.
    for (ii = 0; ii < tsx*tsy; ++ii) {
        int val = idata[ii];
        guchar uval;
        if (val < lmin)
            uval = 0;
        else if (val > lmax)
            uval = 255;
        else
            uval = (guchar) round(((val - lmin) / (lmax - lmin)) * 255);
        
        int n = 3*ii;
        data[n] = uval;
        data[n+1] = uval;
        data[n+2] = uval;
    }
    
    g_free(idata);
    
    // Create the pixbuf
    GdkPixbuf *pb =
        gdk_pixbuf_new_from_data(data, GDK_COLORSPACE_RGB, FALSE, 
                                 8, tsy, tsx, tsx*3, destroy_pb_data, NULL);
    
    if (!pb) {
        printf("Failed to create the thumbnail pixbuf: %s\n", input_data);
        meta_free(imd);
        g_free(data);
        return NULL;
    }
    
    // Scale down to the size we actually want, using the built-in Gdk
    // scaling method, much nicer than what we did above
    GdkPixbuf *pb_s =
        gdk_pixbuf_scale_simple(pb, max_thumbnail_dimension, 
                                max_thumbnail_dimension, GDK_INTERP_BILINEAR);
    gdk_pixbuf_unref(pb);
    
    if (!pb_s) {
        printf("Failed to allocate scaled thumbnail pixbuf: %s\n", input_data);
        meta_free(imd);
        return NULL;
    }

    meta_free(imd);
    return pb_s;
}
