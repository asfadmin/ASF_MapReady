#include <unistd.h>

#ifdef win32
#define BYTE __byte
#include "asf.h"
#undef BYTE
#include <windows.h>
#include <shellapi.h>
#endif

#include "ssv.h"
#include <asf.h>
#include <asf_meta.h>

#define POINT __tmp_point
#include <asf_vector.h>
#undef POINT

#include <png.h>
#include <gdk/gdk.h>

#ifdef win32
static const char PATH_SEPARATOR=':';
#else
static const char PATH_SEPARATOR=';';
#endif

char *find_in_path(char * file)
{
    char *path, *buf, *name, *p;
    int len, pathlen;

    /* first see if file is in current directory */
    if (fileExists(file))
        return STRDUP(file);

    path = (gchar *)g_getenv("PATH");

    len = strlen(file) + 1;
    pathlen = strlen(path);

    /* work area */
    buf = MALLOC( sizeof(char) * (pathlen + len + 2) ); 

    /* put separator + filename at the end of the buffer */
    name = buf + pathlen + 1;
    *name = DIR_SEPARATOR;
    memcpy(name + 1, file, len);

    /* now try each path item, prepended to the filename in the work area */
    p = path;
    do
    {
        char * start;
        char * q = strchr(p + 1, PATH_SEPARATOR);

        /* if separator not found, point to the end */
        if ( !q ) 
            q = path + pathlen;

        start = name - (q - p);

        /* copy path portion to the work area */
        memcpy( start, p, q - p );

        if (fileExists(start))
        {
            char * ret = STRDUP(start);
            free(buf);
            return ret; 
        }

        p = q;
    } 
    while (*p++ != '\0');

    /* not found! */ 
    free(buf);
    return NULL;
}

static int pixbuf2png(GdkPixbuf *pb, const char *output_png)
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

extern GdkPixbuf *pixbuf_small;

int open_google_earth()
{
    char *kml_filename = appendExt(g_filename, ".kml");

    char *basename = get_basename(g_filename);
    char *dirname = get_dirname(g_filename);

    char *arg;
    if (strlen(dirname)==0) {
        char *tmpdir = g_get_current_dir();
        dirname = escapify(tmpdir);
        arg = MALLOC(sizeof(char)*(strlen(dirname)+strlen(kml_filename)+20));
        sprintf(arg, "%s/%s", dirname, kml_filename);
        //free(tmpdir);
    }
    else {
        arg = STRDUP(kml_filename);
    }

    char *png_file = appendExt(arg, ".png");

    printf("png file: %s\n", png_file);
    printf("Temporary kml file: %s\n", arg);

    FILE *kml_file = fopen(arg, "w");
    if (!kml_file)
    {
        asfPrintWarning("Couldn't open kml file!\n");        
        return FALSE;
    }

    kml_header(kml_file);

    if (meta && meta->general &&
        meta_is_valid_double(meta->general->center_latitude) &&
        meta_is_valid_double(meta->general->center_longitude))
    {
        pixbuf2png(pixbuf_small, png_file);
        kml_entry_with_overlay(kml_file, meta, basename, png_file, dirname);
    }
    else
    {
        asfPrintWarning(
            "Failed, metadata doesn't contain valid lat/lon info.\n");
        return FALSE;
    }

    kml_footer(kml_file);
    fclose(kml_file);

    gchar *ge;

#ifdef win32
    char path[1024];
    FindExecutable((LPCTSTR)kml_filename, (LPCTSTR)dirname, (LPTSTR)path);
    ge = escapify(path);
    printf("Path to google earth: %s\n", ge);
#else
    ge = find_in_path("googleearth");
    if (!ge)
    {
       message_box("Couldn't find googleearth! Is it installed?");
       return FALSE;
    }
#endif

    int pid = fork();
    if (pid == 0) {
        asfSystem("\"%s\" \"%s\"", ge, arg);
        //unlink(kml_filename);
        exit(EXIT_SUCCESS);
    }

    free(kml_filename);
    free(basename);
    free(dirname);
    free(arg);

    return TRUE;
}
