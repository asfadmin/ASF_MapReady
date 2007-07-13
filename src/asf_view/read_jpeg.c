#include "asf_view.h"
#include <jpeglib.h>

typedef struct {
    struct jpeg_decompress_struct *cinfo;
    int fp_was_set;
} ReadJpegClientInfo;

int try_jpeg(const char *filename)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
        return strcmp_case(ext, ".jpg") == 0 ||
               strcmp_case(ext, ".jpeg") == 0;
    } else {
        return try_ext(filename, ".jpg") || try_ext(filename, ".jpeg");
    }
}

int handle_jpeg_file(const char *filename, char *meta_name, char *data_name,
                    char **err)
{
    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int has_jpg_ext = has_ext && strcmp_case(ext,".jpg")==0;
    int has_jpeg_ext = has_ext && strcmp_case(ext,".jpeg")==0;

    if (!has_ext) {
        has_jpg_ext = try_ext(filename, ".jpg");
        if (!has_jpg_ext)
            has_jpeg_ext = try_ext(filename, ".jpeg");
    }

    if (has_jpg_ext || has_jpeg_ext)
    {
        char *d=NULL;
        if (has_jpg_ext)
            d = appendExt(filename, ".jpg");
        else if (has_jpeg_ext)
            d = appendExt(filename, ".jpeg");
        assert(d);

        strcpy(meta_name, d);
        strcpy(data_name, d);
        free(d);

        int ret;
        if (!fileExists(data_name)) {
            // I don't think this will actually ever run
            int l = sizeof(char)*strlen(filename)+255;
            *err = MALLOC(l);
            snprintf(*err, l, "Error opening JPEG file: %s\n", data_name);
            ret = FALSE;
        }
        else
            ret = TRUE;

        return ret;
    }
    else {
        // in theory this shouldn't happen, if try_ext is working
        assert(!try_ext(filename, ".jpg"));
        int l = sizeof(char)*strlen(filename)+255;
        *err = MALLOC(l);
        snprintf(*err, l, "Failed to open %s as a JPEG File.\n", filename);
        return FALSE;
    }

    // not reached
    assert(FALSE);
    return FALSE;
}

meta_parameters *read_jpeg_meta(const char *meta_name)
{
    meta_parameters *meta = raw_init();
    
    struct jpeg_decompress_struct cinfo;
    jpeg_create_decompress(&cinfo);

	struct jpeg_error_mgr mgr;
	cinfo.err = jpeg_std_error(&mgr);

    FILE *fp = FOPEN(meta_name, "rb");
    jpeg_stdio_src(&cinfo, fp);

    jpeg_read_header(&cinfo, TRUE);

    meta->general->line_count = cinfo.image_height;
    meta->general->sample_count = cinfo.image_width;

    jpeg_destroy_decompress(&cinfo);
    FCLOSE(fp);

    return meta;
}

int read_jpeg_client(FILE *fp, int row_start, int n_rows_to_get,
                     void *dest_void, void *read_client_info,
                     meta_parameters *meta)
{
    // since we set "require_full_load", we should be reading in the
    // entire image
    assert(row_start == 0);
    assert(n_rows_to_get == meta->general->line_count);

    unsigned char *dest = (unsigned char*)dest_void;
    ReadJpegClientInfo *info = (ReadJpegClientInfo*)read_client_info;
    struct jpeg_decompress_struct *cinfo = info->cinfo;

    if (!info->fp_was_set) {
	    static struct jpeg_error_mgr mgr;
	    cinfo->err = jpeg_std_error(&mgr);

        jpeg_stdio_src(cinfo, fp);
        jpeg_read_header(cinfo, TRUE);
        jpeg_start_decompress(cinfo);

        info->fp_was_set = TRUE;
    } else {
        // we should actually never get here -- because we require a full
        // load, this function should only be called ONCE
        assert(0);
    }

    int ns = meta->general->sample_count;
    JSAMPLE *rgbBuf = MALLOC(sizeof(JSAMPLE)*ns*cinfo->output_components);

    int i,k=0;
    for (i=0; i<n_rows_to_get; ++i) {
        jpeg_read_scanlines(cinfo, &rgbBuf, 1);

        int j,b;
        for (j=0; j<ns; ++j) {
            for (b=0; b<cinfo->output_components; ++b)
                dest[k+b] = rgbBuf[cinfo->output_components*j+b];

            k += cinfo->output_components;
        }
    }

    return TRUE;
}

void free_jpeg_client_info(void *read_client_info)
{
    ReadJpegClientInfo *info = (ReadJpegClientInfo*)read_client_info;
    jpeg_finish_decompress(info->cinfo);
    jpeg_destroy_decompress(info->cinfo);
    free(info);
}

int open_jpeg_data(const char *data_name, const char *meta_name,
                   const char *band, meta_parameters *meta,
                   ClientInterface *client)
{
    ReadJpegClientInfo *info = MALLOC(sizeof(ReadJpegClientInfo));

    struct jpeg_decompress_struct *cinfo =
        MALLOC(sizeof(struct jpeg_decompress_struct));
    jpeg_create_decompress(cinfo);
    cinfo->buffered_image = TRUE;

    info->cinfo = cinfo;
    info->fp_was_set = FALSE;

    client->read_client_info = info;
    client->read_fn = read_jpeg_client;
    client->thumb_fn = NULL;
    client->free_fn = free_jpeg_client_info;

    client->data_type =
        cinfo->output_components==3 ? RGB_BYTE : GREYSCALE_BYTE;

    // since jpeg doesn't support random access ...
    client->require_full_load = TRUE;

    return FALSE;
}
