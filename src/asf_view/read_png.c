#include "asf_view.h"
#include <png.h>

#ifndef png_jmpbuf
#  define png_jmpbuf (png_ptr)    ((png_ptr)->jmpbuf)
#endif

typedef struct {
    png_structp png_ptr;
    png_infop info_ptr;
    png_uint_32 width, height;
    int rgb_flag;
    FILE *fp;
} ReadPngClientInfo;

int try_png(const char *filename, int try_extensions)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
      return strcmp_case(ext, ".png") == 0;
    } else if (try_extensions) {
      return try_ext(filename, ".png");
    }
    return FALSE;
}

int handle_png_file(const char *filename, char *meta_name, char *data_name,
                    char **err)
{
    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int has_png_ext = has_ext && strcmp_case(ext,".png")==0;

    if (!has_ext) {
        has_png_ext = try_ext(filename, ".png");
    }

    if (has_png_ext)
    {
        char *d=NULL;
        if (has_ext)
            d = STRDUP(filename);
        else if (has_png_ext)
            d = appendExt(filename, ".png");
        assert(d);

        strcpy(meta_name, d);
        strcpy(data_name, d);
        free(d);

        int ret;
        if (!fileExists(data_name)) {
            // I don't think this will actually ever run
            int l = sizeof(char)*strlen(filename)+255;
            *err = MALLOC(l);
            snprintf(*err, l, "Error opening PNG file: %s\n", data_name);
            ret = FALSE;
        }
        else
            ret = TRUE;

        return ret;
    }
    else {
        // in theory this shouldn't happen, if try_ext is working
        assert(!try_ext(filename, ".png"));
        int l = sizeof(char)*strlen(filename)+255;
        *err = MALLOC(l);
        snprintf(*err, l, "Failed to open %s as a PNG File.\n", filename);
        return FALSE;
    }

    // not reached
    assert(FALSE);
    return FALSE;
}

int read_png_client(int row_start, int n_rows_to_get,
                    void *dest_void, void *read_client_info,
                    meta_parameters *meta, int data_type)
{
    // since we set "require_full_load", we should be reading in the
    // entire image
    assert(row_start == 0);
    assert(n_rows_to_get == meta->general->line_count);

    unsigned char *dest = (unsigned char*)dest_void;
    ReadPngClientInfo *info = (ReadPngClientInfo*)read_client_info;

    // these will explode if we ever call this fn a second time on
    // the same data.  Shouldn't happen, because we set "require_full_load"
    assert(info->fp);
    assert(info->png_ptr);
    assert(info->info_ptr);

    png_structp png_ptr = (png_structp)info->png_ptr;
    png_infop info_ptr = (png_infop)info->info_ptr;

    int ns = meta->general->sample_count;
    int nchan = info->rgb_flag ? 3 : 1;
    png_bytep png_buf = MALLOC(ns * sizeof(png_byte) * nchan);
    int ii, jj, b, k=0;

    // iterate over all rows in the png
    for ( ii = 0; ii < n_rows_to_get; ii++ )
    {
      png_read_row(png_ptr, png_buf, NULL);
      for (jj = 0 ; jj < ns; jj++ ) {
        for (b = 0; b<nchan; ++b)
          dest[k+b] = png_buf[nchan*jj+b];

        k += nchan;
      }
    }

    fclose(info->fp);
    info->fp = NULL;

    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    info->png_ptr = NULL;
    info->info_ptr = NULL;

    FREE(png_buf);

    return TRUE;
}

void free_png_client_info(void *read_client_info)
{
    ReadPngClientInfo *info = (ReadPngClientInfo*)read_client_info;
    if (info->fp) fclose(info->fp); // should never be still open
    free(info);
}

// for png, combined the "open_meta" and "open_data" functions -- both
// will be opening the same file.
meta_parameters* open_png(const char *data_name, ClientInterface *client)
{
    ReadPngClientInfo *info = MALLOC(sizeof(ReadPngClientInfo));

    png_structp png_ptr;
    png_infop info_ptr;
    png_uint_32  width, height;
    int bit_depth, color_type, nbands, interlace_type, compression_type,
      filter_type;

    // We set up all the png infrastructure here, but don't actually
    // do anything with it, that all happens in read_png_client.
    // Can't do it all in read_png_client, because we need to set
    // up metadata in here.
    info->fp = FOPEN(data_name, "rb");

    unsigned char sig[8];
    // Important: Leaves file pointer offset into file by 8 bytes for png lib
    fread(sig, 1, 8, info->fp); 
    if (!png_check_sig(sig, 8)) {
      // Bad PNG magic number (signature)
      asfPrintWarning("Invalid PNG file (%s)\n", 
                      "file type header bytes invalid", data_name);
      return NULL;
    }
    png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (!png_ptr) {
      asfPrintWarning("Cannot allocate PNG read struct (out of memory?)\n");
      return NULL;
    }
    info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr) {
      png_destroy_read_struct(&png_ptr, NULL, NULL);
      asfPrintWarning("Cannot allocate PNG info struct (out of memory?)\n");
      return NULL;
    }
    if (setjmp(png_jmpbuf(png_ptr))) {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      asfPrintWarning("PNG library error occurred (invalid PNG file?)\n");
      return NULL;
    }
    png_init_io(png_ptr, info->fp);
    // Because of the sig-reading offset ...must do this for PNG lib
    png_set_sig_bytes(png_ptr, 8); 

    // Read info and IHDR
    png_read_info(png_ptr, info_ptr);
    png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
                 &interlace_type, &compression_type, &filter_type);
    nbands = (int)png_get_channels(png_ptr, info_ptr);

    // Preliminary error checking on returned values
    // (make sure results are valid)
    if (//bit_depth != 1 &&
        //bit_depth != 2 &&
        //bit_depth != 4 &&
        //bit_depth != 16 &&
        bit_depth != 8)
    {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      asfPrintWarning("Invalid PNG file bit depth found (%d).\n"
                      "Must be 8.\n", bit_depth);
      return NULL;
    }
    if (color_type != PNG_COLOR_TYPE_GRAY &&
        //color_type != PNG_COLOR_TYPE_GRAY_ALPHA &&
        //color_type != PNG_COLOR_TYPE_PALETTE &&
        color_type != PNG_COLOR_TYPE_RGB &&
        //color_type != PNG_COLOR_TYPE_RGB_ALPHA &&
        //color_type != PNG_COLOR_MASK_PALETTE &&
        //color_type != PNG_COLOR_MASK_ALPHA &&
        color_type != PNG_COLOR_MASK_COLOR)
    {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      asfPrintWarning("Invalid PNG file color type found.\n");
      return NULL;
    }
    if (filter_type != PNG_FILTER_TYPE_BASE &&
        filter_type != PNG_INTRAPIXEL_DIFFERENCING)
    {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      asfPrintWarning("Invalid PNG file filter type found.\n");
      return NULL;
    }
    if (compression_type != PNG_COMPRESSION_TYPE_BASE) {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      asfPrintWarning("Invalid PNG file compression type found.\n");
      return NULL;
    }
    if (interlace_type != PNG_INTERLACE_NONE &&
        interlace_type != PNG_INTERLACE_ADAM7)
    {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      asfPrintWarning("Invalid PNG file interlace type found.\n");
      return NULL;
    }
    
    info->png_ptr = png_ptr;
    info->info_ptr = info_ptr;

    client->read_client_info = info;
    client->read_fn = read_png_client;
    client->thumb_fn = NULL;
    client->free_fn = free_png_client_info;
    client->require_full_load = TRUE;

    if (color_type == PNG_COLOR_TYPE_GRAY)
    {
      client->data_type = GREYSCALE_BYTE;
      info->rgb_flag = FALSE;
    }
    else if (color_type == PNG_COLOR_TYPE_RGB && 
             color_type == PNG_COLOR_MASK_COLOR)
    {
      client->data_type = RGB_BYTE;
      info->rgb_flag = TRUE;
    }

    meta_parameters *meta = raw_init();
    meta->general->line_count = height;
    meta->general->sample_count = width;
    meta->general->band_count = 1;
    strcpy(meta->general->bands, "");
    meta->general->data_type = BYTE;
    return meta;
}
