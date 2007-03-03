#include <unistd.h>
#include <asf_meta.h>
#include <ceos_io.h>
#include <float_image.h>
#include <math.h>

#include <asf_nan.h>
#include "ceos_thumbnail.h"
#include "asf_convert_gui.h"
#include "get_ceos_names.h"
#include "asf_import.h"
#include "asf_endian.h"

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    g_free(pixels);
}

static meta_parameters * silent_meta_create(const char *filename)
{
    report_level_t prev = g_report_level;

    g_report_level = NOREPORT;
    meta_parameters *ret = meta_create(filename);

    g_report_level = prev;
    return ret;
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

    // Input metadata
    meta_parameters *imd;
    char *data_name, *met;

    int pre = has_prepension(input_metadata);
    if (pre > 0)
    {
        int ii, nBands;
        char **dataName = MALLOC(sizeof(char*)*MAX_BANDS);
        for (ii=0; ii<MAX_BANDS; ++ii)
            dataName[ii] = MALLOC(sizeof(char)*255);
        char filename[255], dirname[255];
        split_dir_and_file(input_metadata, dirname, filename);
        met = MALLOC(sizeof(char)*(strlen(input_metadata)+1));
        sprintf(met, "%s%s", dirname, filename + pre);

        get_ceos_data_name(met, dataName, &nBands);

        imd = silent_meta_create(met);
        data_name = STRDUP(dataName[0]);

        for (ii=0; ii<MAX_BANDS; ++ii)
            FREE(dataName[ii]);
        FREE(dataName);
    }
    else
    {
        imd = silent_meta_create (input_metadata);
        data_name = STRDUP(input_data);
        met = STRDUP(input_metadata);
    }

    if (imd->general->data_type != BYTE &&
        imd->general->data_type != INTEGER16) 
// Turning off support for these guys for now.
//        imd->general->data_type != INTEGER32 &&
//        imd->general->data_type != REAL32 &&
//        imd->general->data_type != REAL64)
    {
        /* don't know how to make a thumbnail for this type ... */
        return NULL;
    }

    FILE *fpIn = fopen(data_name, "rb");
    if (!fpIn)
    {
        // failed for some reason, quit without thumbnailing
        meta_free(imd);
        return NULL;
    }

    struct IOF_VFDR image_fdr;                /* CEOS File Descriptor Record */
    get_ifiledr(met, &image_fdr);
    int leftFill = image_fdr.lbrdrpxl;
    int rightFill = image_fdr.rbrdrpxl;
    int headerBytes = firstRecordLen(data_name) +
        (image_fdr.reclen - (imd->general->sample_count + leftFill + rightFill)
         * image_fdr.bytgroup);

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
    unsigned short *line = g_new (unsigned short, imd->general->sample_count);
    unsigned char *bytes = g_new (unsigned char, imd->general->sample_count);

    // Keep track of the average pixel value, so later we can do a 2-sigma
    // scaling - makes the thumbnail look a little nicer and more like what
    // they'd get if they did the default jpeg export.
    double avg = 0.0;
    for ( ii = 0 ; ii < tsy ; ii++ ) {

        size_t jj;
        long long offset =
            (long long)headerBytes+ii*sf*(long long)image_fdr.reclen;

        FSEEK64(fpIn, offset, SEEK_SET);
        if (imd->general->data_type == INTEGER16)
        {
            FREAD(line, sizeof(unsigned short), imd->general->sample_count,
                  fpIn);

            for (jj = 0; jj < imd->general->sample_count; ++jj)
                big16(line[jj]);
        }
        else if (imd->general->data_type == BYTE)
        {
            FREAD(bytes, sizeof(unsigned char), imd->general->sample_count,
                  fpIn);

            for (jj = 0; jj < imd->general->sample_count; ++jj)
                line[jj] = (unsigned short)bytes[jj];
        }

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
    g_free (bytes);
    fclose(fpIn);

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
                                 8, tsx, tsy, tsx*3, destroy_pb_data, NULL);
    
    if (!pb) {
        printf("Failed to create the thumbnail pixbuf: %s\n", data_name);
        meta_free(imd);
        g_free(data);
        return NULL;
    }

    // Scale down to the size we actually want, using the built-in Gdk
    // scaling method, much nicer than what we did above

    // Must ensure we scale the same in each direction
    double scale_y = tsy / max_thumbnail_dimension;
    double scale_x = tsx / max_thumbnail_dimension;
    double scale = scale_y > scale_x ? scale_y : scale_x;
    int x_dim = tsx / scale;
    int y_dim = tsy / scale;

    GdkPixbuf *pb_s =
        gdk_pixbuf_scale_simple(pb, x_dim, y_dim, GDK_INTERP_BILINEAR);
    gdk_pixbuf_unref(pb);
    
    if (!pb_s) {
        printf("Failed to allocate scaled thumbnail pixbuf: %s\n", data_name);
        meta_free(imd);
        return NULL;
    }

    meta_free(imd);
    FREE(data_name);
    FREE(met);
    return pb_s;
}
