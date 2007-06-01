#include "asf.h"
#include "asf_meta.h"
#include <png.h>
#include <gdk/gdk.h>
/*
static int eq(float a, float b) {
    return fabs(a-b)<.000001;
}
*/

/* Appears we don't need this at the moment * /
static int
img2png(meta_parameters *meta, const char *img_file, int max_dimension,
        const char *output_png)
{
    // Vertical and horizontal scale factors required to meet the
    // max_thumbnail_dimension part of the interface contract.
    int vsf = ceil (meta->general->line_count / max_dimension);
    int hsf = ceil (meta->general->sample_count / max_dimension);

    // Overall scale factor to use is the greater of vsf and hsf.
    int sf = (hsf > vsf ? hsf : vsf);

    // Thumbnail image sizes.
    int tsx = meta->general->sample_count / sf;
    int tsy = meta->general->line_count / sf;

    // The actual data
    double mean = 0.0;
    double mask = (double)meta->general->no_data;
    float *pixels = CALLOC(sizeof(float), tsx*tsy);
    unsigned char *bytes = MALLOC(sizeof(unsigned char) * tsx*tsy*2);

    // temporary storage location
    float *buf = MALLOC(sizeof(float)*meta->general->sample_count);

    // open up the image
    FILE *fp = fopenImage(img_file, "wb");

    // read in our subset of the image, no resampling at this point...
    int l,s;
    for (l=0; l<tsy; ++l) {
        get_float_line(fp, meta, l*sf, buf);
        for (s=0; s<tsx; ++s) {
            pixels[s + tsy*l] = buf[s*sf];
            if (!eq(buf[s*sf],mask))
                mean += (double)buf[s*sf];
        }
    }

    FREE(buf);
    FCLOSE(fp);
    mean /= tsx*tsy;
    
    // compute std dev
    float stddev = 0.0;
    int i,n=0;
    for (i=0; i < tsx*tsy; ++i) {
        if (!eq(pixels[i],mask)) {
            stddev += (pixels[i] - mean)*(pixels[i] - mean);
            ++n;
        }
    }
    stddev = sqrt(stddev / (n-1));

    // convert to bytes, scaling two 2-sigma
    float top = mean + 2*stddev;
    float bot = mean - 2*stddev;

    for (i=0; i < tsx*tsy; ++i) {
        float val = pixels[i];

        unsigned char bval;
        if (val < bot)
            bval = 0;
        else if (val > top)
            bval = 255;
        else {
            float f = (pixels[i] - bot) / (top - bot) * 255.0;
            if (f < 0)
                bval = 0;
            if (f > 255)
                bval = 255;
            else
                bval = (unsigned char)f;
        }

        int gray_index = i*2;
        int alpha_index = i*2 + 1;

        bytes[gray_index] = bval;
        bytes[alpha_index] = eq(pixels[i],mask) ? 0 : 255;
    }
        
    // now write that png, baby
    FILE *fout = FOPEN(output_png, "wb");
    png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
        NULL, NULL, NULL);
    if (!png_ptr) {
        asfPrintWarning("Couldn't open png file: %s\n", output_png);
        return FALSE;
    }

    png_infop info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr) {
        png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
        return FALSE;
    }

    if (setjmp(png_jmpbuf(png_ptr))) {
        png_destroy_write_struct(&png_ptr, &info_ptr);
        fclose(fout);
        return FALSE;
    }

    png_init_io(png_ptr, fout);

    png_set_IHDR(png_ptr, info_ptr, tsx, tsy, 8, 
        PNG_COLOR_TYPE_GRAY_ALPHA, PNG_INTERLACE_NONE,
        PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

    png_write_info(png_ptr, info_ptr);

    png_bytep *row_pointers = MALLOC(sizeof(png_bytep)*tsy);
    for (i=0; i<tsy; ++i)
        row_pointers[i] = bytes + i*tsx*2;

    png_write_image(png_ptr, row_pointers);

    png_write_end(png_ptr, NULL);
    png_destroy_write_struct(&png_ptr, &info_ptr);

    FREE(row_pointers);
    fclose(fout);

    return TRUE;
}
*/

int pixbuf2png(GdkPixbuf *pb, const char *output_png)
{
    int i;
    int width = gdk_pixbuf_get_width(pb);
    int height = gdk_pixbuf_get_height(pb);
    int n_channels = gdk_pixbuf_get_n_channels(pb);

    int rowstride = gdk_pixbuf_get_rowstride(pb);
    guchar *pixels = gdk_pixbuf_get_pixels(pb);
    guchar *pixels_out = MALLOC(sizeof(guchar)*width*height*4);

    //printf("pixbuf2png> opening: %s\n", output_png);
    FILE *fout = FOPEN(output_png, "wb");
    png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
        NULL, NULL, NULL);
    if (!png_ptr) {
        asfPrintWarning("Couldn't open png file: %s\n", output_png);
        return FALSE;
    }

    //printf("pixbuf2png> png_create_info_struct\n");
    png_infop info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr) {
        png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
        fclose(fout);
        asfPrintWarning("Couldn't open png info for %s\n", output_png);
        return FALSE;
    }

    //printf("pixbuf2png> setjmp\n");
    if (setjmp(png_jmpbuf(png_ptr))) {
        png_destroy_write_struct(&png_ptr, &info_ptr);
        fclose(fout);
        asfPrintWarning("Error writing the png: %s\n", output_png);
        return FALSE;
    }

    //printf("pixbuf2png> png_init_io\n");
    png_init_io(png_ptr, fout);

    //printf("pixbuf2png> png_set_IHDR\n");
    png_set_IHDR(png_ptr, info_ptr, width, height, 8, 
        PNG_COLOR_TYPE_RGB_ALPHA, PNG_INTERLACE_NONE,
        PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

    //printf("pixbuf2png> png_write_info\n");
    png_write_info(png_ptr, info_ptr);

    // add a transparency byte to each pixel in the pixels_out buffer
    for (i=0; i<height; ++i) {
        int j;
        for (j=0; j<width; ++j) {
            // output: red=k, green=k+1, blue=k+2, alpha=k+3
            int out_k = 4*(j + i*width);
            // input: red=k, green=k+1, blue=k+2
            int in_k = j*n_channels + i*rowstride;

            // make it transparent, if the pixel is black
            // (i.e., all channels are 0)
            int trans = pixels[in_k] == 0 &&
                pixels[in_k+1] == 0 && pixels[in_k+2] == 0;

            pixels_out[out_k] = pixels[in_k];
            pixels_out[out_k+1] = pixels[in_k+1];
            pixels_out[out_k+2] = pixels[in_k+2];
            pixels_out[out_k+3] = trans ? 0 : 255;
        }
    }

    //printf("pixbuf2png> row_pointers\n");
    png_bytep *row_pointers = MALLOC(sizeof(png_bytep)*height);
    for (i=0; i<height; ++i)
        row_pointers[i] = pixels_out + i*width*4;

    //printf("pixbuf2png> png_write_image\n");
    png_write_image(png_ptr, row_pointers);

    //printf("pixbuf2png> png_write_end\n");
    png_write_end(png_ptr, NULL);

    //printf("pixbuf2png> png_destroy_write_struct\n");
    png_destroy_write_struct(&png_ptr, &info_ptr);

    //printf("pixbuf2png> fclose\n");
    fclose(fout);

    //printf("pixbuf2png> freeing row pointers\n");
    FREE(row_pointers);
    FREE(pixels_out);

    return TRUE;
}
