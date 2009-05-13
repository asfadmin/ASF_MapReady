#include "asf_view.h"
#include "asf.h"
#include "asf_jpeg.h"

typedef struct {
    struct jpeg_decompress_struct *cinfo;
    FILE *fp;
} ReadJpegClientInfo;

int try_jpeg(const char *filename, int try_extensions)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
        return strcmp_case(ext, ".jpg") == 0 ||
               strcmp_case(ext, ".jpeg") == 0;
    } else if (try_extensions) {
        return try_ext(filename, ".jpg") || try_ext(filename, ".jpeg");
    } else {
        return FALSE;
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
	if (has_ext)
            d = STRDUP(filename);
        else if (has_jpg_ext)
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

int read_jpeg_client(int row_start, int n_rows_to_get,
                     void *dest_void, void *read_client_info,
                     meta_parameters *meta, int data_type)
{
    // since we set "require_full_load", we should be reading in the
    // entire image
    assert(row_start == 0);
    assert(n_rows_to_get == meta->general->line_count);

    unsigned char *dest = (unsigned char*)dest_void;
    ReadJpegClientInfo *info = (ReadJpegClientInfo*)read_client_info;
    struct jpeg_decompress_struct *cinfo = info->cinfo;

    // these will explode if we ever call this fn a second time on
    // the same data.  Shouldn't happen, because we set "require_full_load"
    assert(info->fp);
    assert(info->cinfo);

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

    fclose(info->fp);
    info->fp = NULL;

    jpeg_finish_decompress(cinfo);
    jpeg_destroy_decompress(cinfo);
    free(cinfo);
    info->cinfo = NULL;

    FREE(rgbBuf);
    return TRUE;
}

void free_jpeg_client_info(void *read_client_info)
{
    ReadJpegClientInfo *info = (ReadJpegClientInfo*)read_client_info;
    if (info->fp) fclose(info->fp); // should never be still open
    free(info);
}

// for jpeg, combined the "open_meta" and "open_data" functions -- both
// will be opening the same file.
meta_parameters* open_jpeg(const char *data_name, ClientInterface *client)
{
    ReadJpegClientInfo *info = MALLOC(sizeof(ReadJpegClientInfo));

    // We set up all the jpeg infrastructure here, but don't actually
    // do anything with it, that all happens in read_jpeg_client.
    // Can't do it all in read_jpeg_client, because we need to set
    // up metadata in here.
    struct jpeg_decompress_struct *cinfo =
        MALLOC(sizeof(struct jpeg_decompress_struct));
    jpeg_create_decompress(cinfo);
    cinfo->buffered_image = TRUE;

    static struct jpeg_error_mgr mgr;
    cinfo->err = jpeg_std_error(&mgr);

    info->fp = fopen(data_name, "rb");
    if (!info->fp) {
        asfPrintWarning("Failed to open Jpeg file %s: %s\n",
            data_name, strerror(errno));
        return NULL;
    }

    jpeg_stdio_src(cinfo, info->fp);
    jpeg_read_header(cinfo, TRUE);
    jpeg_start_decompress(cinfo);

    info->cinfo = cinfo;

    client->read_client_info = info;
    client->read_fn = read_jpeg_client;
    client->thumb_fn = NULL;
    client->free_fn = free_jpeg_client_info;

    client->data_type =
        cinfo->output_components==3 ? RGB_BYTE : GREYSCALE_BYTE;

    // since jpeg doesn't support random access ...
    client->require_full_load = TRUE;

    meta_parameters *meta = raw_init();
    meta->general->line_count = cinfo->image_height;
    meta->general->sample_count = cinfo->image_width;
    meta->general->data_type = BYTE;
    meta->general->band_count = 1;
    strcpy(meta->general->bands, "");
    meta->general->image_data_type = AMPLITUDE_IMAGE;

    if (strstr(data_name, "land_shallow_topo") != NULL) {
      // kludge up some lat/lon pseudoprojection action here!!
      meta->projection = meta_projection_init();

      meta->projection->type = LAT_LONG_PSEUDO_PROJECTION;
      meta->projection->startX = -180;
      meta->projection->startY = 90;
      meta->projection->perX = 180./(double)(meta->general->line_count);
      meta->projection->perY = -360./(double)(meta->general->sample_count);
      strcpy(meta->projection->units, "degrees");
      meta->projection->datum = WGS84_DATUM;
      meta->projection->spheroid = WGS84_SPHEROID;
      meta->general->start_line = 0;
      meta->general->start_sample = 0;
    }
    else if (strstr(data_name, "terramodis.jpg") != NULL) {
      meta->projection = meta_projection_init();

      meta->projection->type = POLAR_STEREOGRAPHIC;
      meta->projection->hem = 'N';

      meta->projection->param.ps.slat = 70;
      meta->projection->param.ps.slon = -113.5;
      meta->projection->param.ps.is_north_pole = 1;
      meta->projection->param.ps.false_easting = 0;
      meta->projection->param.ps.false_northing = 0;

      // kludged values... found by a linear regression from manually
      // associated points:
      //   sample    line    proj_x      proj_y
      //   ------    ----    ------      ------
      //     2254    3355   1794254    -4571231
      //     3112    1360   4441720     1510018
      //      206    1888  -4384474      -26248
      //     2940    2775   3898138    -2809595
      //      738     783  -2604090     3429022
      // In Excel:
      //  perX =   INDEX(LINEST(C1:C5,A1:A5),1)
      //  perY =   INDEX(LINEST(D1:D5,B1:B5),1)
      //  startX = INDEX(LINEST(C1:C5,A1:A5),2)
      //  startY = INDEX(LINEST(D1:D5,B1:B5),2)
      meta->projection->perX = 3006;
      meta->projection->startX = -4931853;
      meta->projection->perY = -3097;
      meta->projection->startY = 5801627;
      strcpy(meta->projection->units, "meters");

      meta->projection->datum = WGS84_DATUM;
      meta->projection->datum = WGS84_SPHEROID;

      meta->general->start_line = 0;
      meta->general->start_sample = 0;
    }

    return meta;
}
