#include <unistd.h>
#include <asf_meta.h>
#include <ceos_io.h>
#include <float_image.h>
#include <math.h>

#include <asf_nan.h>
#include "ceos_thumbnail.h"

void destroy_pixbuf_data(guchar *pixels, gpointer data)
{
	g_free(pixels);
}

GdkPixbuf *
make_input_image_thumbnail_pixbuf (const char *input_metadata, const char *input_data,
								   size_t max_thumbnail_dimension)
{
    /* This can happen if we don't get around to drawing the thumbnail
       until the file has already been processes & cleaned up, don't want
       to crash in that case. */
    if (!fileExists(input_metadata))
        return FALSE;

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
        return FALSE;
    }

    gchar *tmp = g_strdup (input_data);
    g_assert (tmp != NULL);
    CEOS_FILE *id = fopenCeos (tmp); // Input data file.
    g_free (tmp);

    if (!id->f_in) {
        // failed for some reason, just quit without thumbnailing
        meta_free(imd);
        return FALSE;
    }

	// use a larger dimension at first, for our crude scaling.  We will
	// use a better scaling method later, from GdbPixbuf
	int larger_dim = 256;

    // Vertical and horizontal scale factors required to meet the
    // max_thumbnail_dimension part of the interface contract.
    int vsf = ceil (imd->general->line_count / larger_dim);
    int hsf = ceil (imd->general->sample_count / larger_dim);
    // Overall scale factor to use is the greater of vsf and hsf.
    int sf = (hsf > vsf ? hsf : vsf);

    // Thumbnail image sizes.
    size_t tsx = imd->general->sample_count / sf;
    size_t tsy = imd->general->line_count / sf;

    // Thumbnail image.
	int *idata = g_new(int, tsx*tsy);
	guchar *data = g_new(guchar, 3*tsx*tsy);

    // Form the thumbnail image by grabbing individual pixels.  FIXME:
    // Might be better to do some averaging or interpolating.
    size_t ii;
    int *line = g_new (int, imd->general->sample_count);
	double max = -999999, min = 999999;
	double avg=0;
    for ( ii = 0 ; ii < tsy ; ii++ ) {
        readCeosLine (line, ii * sf, id);
        size_t jj;
        for ( jj = 0 ; jj < tsx ; jj++ ) {
            // Current sampled value.  We will average a couple pixels together.
            double csv;		
            if ( jj * sf < imd->general->line_count - 1 ) {
                csv = (line[jj * sf] + line[jj * sf + 1]) / 2;
            }
            else {
                csv = (line[jj * sf] + line[jj * sf - 1]) / 2;
            }

			idata[ii*tsx + jj] = (int)csv;

			if (csv > max) max = csv;
			if (csv < min) min = csv;
			avg += csv;
        }
    }
    g_free (line);
	closeCeos(id);

	avg /= tsx*tsy;

	double stddev = 0;
	for (ii = 0; ii < tsx*tsy; ++ii)
		stddev += ((double)idata[ii] - avg)*((double)idata[ii] - avg);
	stddev = sqrt(stddev / (tsx*tsy));

	double lmin = avg - 2*stddev;
	double lmax = avg + 2*stddev;

	for (ii = 0; ii < tsx*tsy; ++ii) {
		int n = 3*ii;
		int val = idata[ii];
		guchar uval;
		if (val < lmin)
			uval = 0;
		else if (val > lmax)
			uval = 255;
		else
			uval = (guchar) round(((val - lmin) / (lmax - lmin)) * 255);

		data[n] = uval;
		data[n+1] = uval;
		data[n+2] = uval;
	}

	g_free(idata);

	GdkPixbuf *pb = gdk_pixbuf_new_from_data(data, GDK_COLORSPACE_RGB, FALSE, 
		8, tsy, tsx, tsx*3, destroy_pixbuf_data, NULL);

	if (!pb) {
		printf("Failed to allocate thumbnail pixbuf.\n");
        meta_free(imd);
		return FALSE;
	}

	GdkPixbuf *pb_s = gdk_pixbuf_scale_simple(pb, max_thumbnail_dimension, 
		max_thumbnail_dimension, GDK_INTERP_BILINEAR);
	gdk_pixbuf_unref(pb);

	if (!pb_s) {
		printf("Failed to allocate scaled thumbnail pixbuf.\n");
        meta_free(imd);
		return FALSE;
	}

    return pb_s;
}
